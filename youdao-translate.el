;;; youdao-translate-api.el --- Use youdao translate api to perform translations

;; Copyright (C) 2012 zxy

;; Copyright (C) 2011 zxy
;; Author: zxy <gcoordinate@gmail.com>
;; Maintainer: zxy <gcoordinate@gmail.com>
;; Created: May 2012
;; Version: 0.1

;; This file is NOT part of Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Installation
;; ============
;;
;; (defvar youdaotranslate-keyfrom "your-keyfrom")
;; (defvar youdaotranslate-key "your-key")
;; (require youdao-translate-api)
;;

(require 'url)
(message (concat "Loading " load-file-name))

;; public var

(defcustom youdaotranslate-service "http://fanyi.youdao.com/openapi.do?"
  "Service to use for translation."
  :group 'youdaotranslate
  :type 'string)

(defcustom youdaotranslate-keyfrom ""
  "keyfrom to use for translation."
  :group 'youdaotranslate
  :type 'string)

(defcustom youdaotranslate-key ""
  "key to use for translation."
  :group 'youdaotranslate
  :type 'string)

(defcustom youdaotranslate-doctype "json"
  "doctype."
  :group 'youdaotranslate
  :type 'string)

(defcustom youdaotranslate-version "1.1"
  "version."
  :group 'youdaotranslate
  :type 'string)

;; private var

(defvar youdaotranslate-history-hash (make-hash-table :test 'equal))

(defvar youdaotranslate-history-text "")

;; defun

(defun youdaotranslate-make-url (text)
  "Generate the url to send to the translation service."
  (concat youdaotranslate-service
          "keyfrom=" youdaotranslate-keyfrom
          "&key=" youdaotranslate-key
          "&type=" "data"
          "&doctype=" youdaotranslate-doctype
          "&version=" youdaotranslate-version
          "&q=" (url-hexify-string text)))

(defun youdaotranslate-url-callback (status)
  "Switch to the buffer returned by `url-retreive'."
  ;; (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (if (search-forward-regexp "^$" nil t)
      ;; (if (search-forward-regexp "\"" nil t)
      (setq header (buffer-substring (point-min) (point))
            data (buffer-substring (1+ (point)) (point-max)))
    ;; unexpected situation, return the whole buffer
    (setq data (buffer-string)))
  ;; (message (encode-coding-string (buffer-string) 'utf-8))
  ;; (setq result (decode-coding-string (buffer-string) 'utf-8))
  (setq result (decode-coding-string data 'utf-8))
  (setq result (substring result 2 (- (length result) 1)))
  (kill-new result)
  ;; (kill-buffer (current-buffer))
  (puthash youdaotranslate-history-text result youdaotranslate-history-hash)
  (message result))

(defun get-translate-result (word)
  (shell-command-to-string
   (concat
    (format "curl 'http://fanyi.youdao.com/openapi.do?keyfrom=&key=&type=data&doctype=xml&version=1.1&q=%s' 2>/dev/null"
            word))))

(defun analytic-translate-result (translateresult)
  (let* ((root (with-temp-buffer (insert translateresult)
                                 (xml-parse-region (point-min) (point-max))))
         (youdao-fanyi (car root))
         (basic (car (xml-get-children youdao-fanyi 'basic)))
         (explains-ex (xml-get-children (car (xml-get-children basic 'explains)) 'ex))
         (phonetic-texts (car (xml-node-children (car (xml-get-children basic 'phonetic)))))
         (explains-texts (concat "音标：" phonetic-texts "\n")))
    (loop for ex in explains-ex
          do (setq explains-texts (concat explains-texts  (car (xml-node-children ex)) "\n")))
    explains-texts))

(defun youdaotranslate-region-or-input ()
  "Translate region or input"
  (interactive)
  ;; if marked
  (if (and mark-active
           (/= (point) (mark)))
      (setq youdaotranslate-history-text (buffer-substring (point) (mark)))
    ;; read text from mini buffer
    (progn
      (if (equal nil (current-word))
          (setq defaultext youdaotranslate-history-text)
        (setq defaultext (current-word)))
      (setq youdaotranslate-history-text (read-string (format "[youdaotranslate] text (default %s): " defaultext)
                                                      nil nil defaultext nil))))
  ;; read other infor
  (setq result (gethash youdaotranslate-history-text youdaotranslate-history-hash))
  (if (and (not (equal "" result))
           (not (equal nil result)))
      (message result)
    (url-retrieve (youdaotranslate-make-url youdaotranslate-history-text) 'youdaotranslate-url-callback)
    ))

(defun youdaotranslate-show-history ()
  "Show translate history"
  (interactive)
  (with-output-to-temp-buffer "*translate-temp*"
    (print "<Temp buffer show translate history. Type 'q' to close.>")
    (print "--------------------------------------------------------")
    (loop for k being the hash-key of youdaotranslate-history-hash do
          (print k)
          (print (gethash k youdaotranslate-history-hash))
          (print "--------------------------------------------------------"))
    (switch-to-buffer "*translate-temp*")
    ))

(provide 'youdao-translate-api)

;;; youdao-translate-api.el ends here
