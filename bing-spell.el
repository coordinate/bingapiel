;;; bing-spell.el --- Use bing spell api to check spell

;; Copyright (C) 2012 zxy
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

(require 'bing-api)
(message (concat "Loading " load-file-name))

;; public var

(defvar bingspell-history-hash (make-hash-table :test 'equal))

(defvar bingspell-history-text "")

;; (defcustom bingtranslate-service "http://api.bing.net/json.aspx?"
;;   "Service to use for translation."
;;   :group 'bingtranslate
;;   :type 'string)

;; (defcustom bingtranslate-appId ""
;;   "Service to use for translation."
;;   :group 'bingtranslate
;;   :type 'string)

;; defun

;; http://api.bing.net/json.aspx?AppId=*&Query=Mispeling%20words%20is%20a%20common%20ocurrence.&Sources=Spell&Version=2.0&Market=en-us&Options=EnableHighlighting&JsonType=callback&JsonCallback=SearchCompleted
(defun bingspell-make-url (text)
  "Generate the url to send to the translation service."
  (bingapi-make-url (list (concat "appId="bingapi-appId)
                          (concat "&Query="text)
                          "&Sources=Spell&Version=2.0&Market=en-us&Options=EnableHighlighting&JsonType=callback&JsonCallback=SearchCompleted")))

(defun bingspell-region-or-input ()
  "spell region or input"
  (interactive)
  ;; if marked
  (if (and mark-active
           (/= (point) (mark)))
      (setq bingspell-history-text (buffer-substring (point) (mark)))
    ;; read text from mini buffer
    (progn
      (if (equal nil (current-word))
          (setq defaultext bingspell-history-text)
        (setq defaultext (current-word)))
      (setq bingspell-history-text (read-string (format "[bing-api] text to check spell (default %s): " defaultext)
                                                    nil nil defaultext nil))))
  (setq result (gethash (concat bingspell-history-text) bingspell-history-hash))
  (if (and (not (equal "" result))
           (not (equal nil result)))
      (message result)
    (url-retrieve (bingspell-make-url bingspell-history-text) 'bingspell-url-callback))
  )

(defun bingspell-url-callback (status)
  "Switch to the buffer returned by `url-retreive'."
  ;; (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (if (search-forward-regexp "Value" nil t)
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
  (kill-buffer (current-buffer))
  (puthash (concat bingspell-history-text) result bingspell-history-hash)
  (message result))

(provide 'bing-spell)

;;; bing-spell.el ends here
