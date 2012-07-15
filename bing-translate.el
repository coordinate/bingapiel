;;; bing-translate.el --- Use bing translate api to perform translations

;; Copyright (C) 2012 zxy
;; Author: zxy <gcoordinate@gmail.com>
;; Maintainer: zxy <gcoordinate@gmail.com>
;; Created: May 2012
;; Version: 1.0

;; This file is NOT part of Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or change it under
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
;; (add-to-list 'load-path (concat plugin-path "bingapiel"))
;;
;; ;; Your bing api client_id.
;; ;; Get from https://datamarket.azure.com/developer/applications/
;; (defvar bingapi-clientid "667f3adb-e22d-4dca-b476-ca536c0a6d8a")
;;
;; ;; Your bing api client_secret.
;; ;; Get from https://datamarket.azure.com/developer/applications/
;; (defvar bingapi-clientsecret "xBTJ5Ee5RSFf++uVjSVKVFcsoswQlDyb8kPp5wSyrV8=")
;;
;; ;; list all language codes may be used
;; (defvar bingtranslate-language-list
;;   '("en"
;;     "zh-CHS"
;;     "ja"))
;;
;; ;; Turn auto detect language code on
;; (defvar bingtranslate-auto-detect t)
;;
;; (require 'bing-translate)
;;
;; ;; key bounding
;; (global-set-key [M-f1] 'bingtranslate-region-or-input)
;;
;; ;; add a pair of language to shortcut keys shortkey
;; ;; Parameters: "pair name" "from language" "to language"
;; (bingtranslate-add-pair "1" "zh-CHS" "ja")
;;

(require 'url)
(require 'bing-api)
(message (concat "Loading " load-file-name))

;; public var

(defcustom bingtranslate-service "http://api.microsofttranslator.com/V2/Http.svc/"
  "Service to use for translation."
  :group 'bingapi
  :type 'string)

(defcustom bingtranslate-auto-detect nil
  "Set auto detect language codes."
  :group 'bingapi
  :type 'boolean)

;; http://www.emreakkas.com/internationalization/microsoft-translator-
;;        api-languages-list-language-codes-and-names
(defcustom bingtranslate-language-list '()
  "list all language to use."
  :group 'bingapi
  :type 'list)

;; private var

(defvar bingtranslate-language-pair-hash (make-hash-table :test 'equal))

(defvar bingtranslate-language-pair-list '())

(defvar bingtranslate-history-hash (make-hash-table :test 'equal))

(defvar bingtranslate-history-text "")

(defvar bingtranslate-history-from "")

(defvar bingtranslate-history-to "")

(defvar bingtranslate-appId nil)

;; defun

(defun bingtranslate-make-url (request args)
  "Generate the url to send to the translation service."
  ;; get access_token
  (unless bingtranslate-appId
    (setq bingtranslate-appId (bingapi-post-and-get-accesstoken)))
  ;; make url with bingtranslate-service
  (setq url
        (concat bingtranslate-service
                ;; and request type
                request
                ;; and appID
                "appID=Bearer+"(url-hexify-string bingtranslate-appId)))
  ;; and arguments
  (if args
      (concat url "&"
              (mapconcat (lambda (arg)
                           (concat (url-hexify-string (car arg)) "="
                                   (decode-coding-string
                                    (url-hexify-string (cadr arg))
                                    'utf-8))) args "&"))
    url))

(defun bingtranslate-getlanguagesfortranslate ()
  "Obtain a list of language codes representing languages that are
supported by the Translation Service."
  ;; get first time
  (let ((buf (url-retrieve-synchronously
              (bingtranslate-make-url
               "GetLanguagesForTranslate?" nil))))
    (if buf
        (with-current-buffer buf
          (let* ((xmldata (decode-coding-string (buffer-string) 'utf-8))
                 (result nil))
            (kill-buffer (current-buffer))
            ;; check access_taken
            (when (equal "expired" (bingapi-check-accesstoken xmldata))
              ;; get secend time
              (setq buf (url-retrieve-synchronously
                         (bingtranslate-make-url
                          "GetLanguagesForTranslate?" nil)))
              (if buf
                  (with-current-buffer buf
                    (setq xmldata (decode-coding-string (buffer-string) 'utf-8))
                    (kill-buffer (current-buffer)))
                (error "[bingtranslate] Could not get language for translate!")))
            ;; get language code
            (when (string-match "<string>\\(.*\\)</string>" xmldata 0)
              (setq result (match-string 1 xmldata))
              (split-string result "</string><string>"))))
      (error "[bingtranslate] Could not get language for translate!"))))

(defun bingtranslate-checklanguageslist ()
  "get language list."
  (condition-case nil
      (when (equal 0 (length bingtranslate-language-list))
        (setq bingtranslate-language-list
              (bingtranslate-getlanguagesfortranslate)))
    (error
     (error "[bingtranslate] Cannot connect to network!"))))

(defun bingtranslate-detect (text)
  "detect language code"
  ;; short text, too long will slow and error
  (while (string-match "^\\s-+" text)
    (setq text (replace-match "" t t text)))
  (when (< 20 (length text))
    (setq text (substring text 0 20)))
  ;; detect first time
  (let ((buf (url-retrieve-synchronously
              (bingtranslate-make-url
               "Detect?"
               (list (list "text" text))))))
    (if buf
        (with-current-buffer buf
          (let* ((xmldata (decode-coding-string (buffer-string) 'utf-8))
                 (result nil))
            (kill-buffer (current-buffer))
            ;; check access_taken
            (when (equal "expired" (bingapi-check-accesstoken xmldata))
              ;; detect secend time
              (setq buf (url-retrieve-synchronously
                         (bingtranslate-make-url
                          "Detect?"
                          (list (list "text" text)))))
              (if buf
                  (with-current-buffer buf
                    (setq xmldata (decode-coding-string (buffer-string) 'utf-8))
                    (kill-buffer (current-buffer)))
                (error "[bingtranslate] Could not detect language!")))
            ;; get language code
            (when (string-match "\">\\(.*\\)</string>" xmldata 0)
              (setq result (match-string 1 xmldata)))
            result))
      (error "[bingtranslate] Could not detect language!"))))

(defun bingtranslate-priority-code (ignorecode codes)
  "get priority language code"
  (setq firstcode (car codes))
  (if (or (equal nil codes)
          (equal 0 (length codes)))
      "en"
    (if (equal 1 (length codes))
        firstcode
      (if (equal ignorecode firstcode)
          (bingtranslate-priority-code ignorecode (cdr codes))
        firstcode))))

(defun bingtranslate-url-callback (status)
  "callback of bingtranslate-region-or-input."
  (let* ((xmldata (decode-coding-string (buffer-string) 'utf-8))
         (result nil))
    (kill-buffer (current-buffer))
    ;; check access_token
    (if (equal "expired" (bingapi-check-accesstoken xmldata))
        ;; translate again
        (bingtranslate-url-retrieve)
      ;; get translate
      (when (string-match "\">\\(.*\\)</string>" xmldata 0)
        (setq result (match-string 1 xmldata)))
      ;;(message xmldata)
      (puthash (concat bingtranslate-history-text " from "
                       bingtranslate-history-from " to "
                       bingtranslate-history-to)
               result bingtranslate-history-hash)
      (kill-new result)
      (message result)
      )))

(defun bingtranslate-url-retrieve ()
  "call url-retrieve to translate."
  (url-retrieve (bingtranslate-make-url
                 "Translate?"
                 (list (list "text" bingtranslate-history-text)
                       (list "from" bingtranslate-history-from)
                       (list "to" bingtranslate-history-to)))
                'bingtranslate-url-callback))

(defun bingtranslate-curl-get ()
  "curl get to translate."
  (let* ((command (format "curl -H \"Authorization: bearer %s\"
\"%sTranslate?text=%s&from=%s&to=%s\""
                          (bingapi-post-and-get-accesstoken)
                          bingtranslate-service
                          bingtranslate-history-text
                          bingtranslate-history-from
                          bingtranslate-history-to))
         (xmldata (decode-coding-string
                    (shell-command-to-string
                     (decode-coding-string command 'utf-8)
                     ) 'utf-8))
         (result nil))
    ;; get translate
    (when (string-match "\">\\(.*\\)</string>" xmldata 0)
      (setq result (match-string 1 xmldata)))
    (puthash (concat bingtranslate-history-text " from "
                     bingtranslate-history-from " to "
                     bingtranslate-history-to)
             result bingtranslate-history-hash)
    (kill-new result)
    (message result)))

(defun bingtranslate-region-or-input ()
  "Translate region or input"
  (interactive)
  (bingtranslate-checklanguageslist)
  ;; if marked
  (if (and mark-active
           (/= (point) (mark)))
      (setq bingtranslate-history-text (buffer-substring (point) (mark)))
    ;; read text from mini buffer
    (progn
      (if (equal nil (current-word))
          (setq defaultext bingtranslate-history-text)
        (setq defaultext (current-word)))
      (setq bingtranslate-history-text
            (read-string
             (format "[bingtranslate] text (default %s): " defaultext)
             nil nil defaultext nil))))
  (if (equal "" bingtranslate-history-text)
      (error "[bingtranslate] Translate text must not be ''."))
  ;; remove trailing enter and whitspace and %s
  (while (string-match "%s" bingtranslate-history-text)
    (setq bingtranslate-history-text (replace-match "" t t bingtranslate-history-text)))
  (while (string-match "^\\s-+" bingtranslate-history-text)
    (setq bingtranslate-history-text (replace-match "" t t bingtranslate-history-text)))
  (while (string-match "\\s-+$" bingtranslate-history-text)
    (setq bingtranslate-history-text (replace-match "" t t bingtranslate-history-text)))
  (while (string-match "\n" bingtranslate-history-text)
    (setq bingtranslate-history-text (replace-match " " t t bingtranslate-history-text)))
  ;; read other infor
  (if bingtranslate-auto-detect
      (setq from-priority (bingtranslate-detect bingtranslate-history-text))
    (setq from-priority (car bingtranslate-language-list)))
  (setq tmptype
        (completing-read
         (format "[bingtranslate] language pair name or from language (default %s): "
                 from-priority)
         (append bingtranslate-language-list bingtranslate-language-pair-list)
         nil t nil nil from-priority))
  ;; get pair
  (setq pair (gethash tmptype bingtranslate-language-pair-hash))
  (if (and (not (equal "" pair))
           (not (equal nil pair)))
      (progn
        (setq bingtranslate-history-from (car pair))
        (setq bingtranslate-history-to (cadr pair)))
    (progn
      (setq bingtranslate-history-from tmptype)
      (setq to-priority (bingtranslate-priority-code
                         bingtranslate-history-from
                         bingtranslate-language-list))
      (setq bingtranslate-history-to
            (completing-read
             (format "[bingtranslate] to language (default %s): "
                     to-priority)
             bingtranslate-language-list
             nil t nil nil to-priority))))
  ;; check if have result
  (setq result (gethash (concat bingtranslate-history-text " from "
                                bingtranslate-history-from " to "
                                bingtranslate-history-to)
                        bingtranslate-history-hash))
  (if (and (not (equal "" result))
           (not (equal nil result)))
      (message result)
    (bingtranslate-url-retrieve)))

(defun bingtranslate-show-history ()
  "Show translate history"
  (interactive)
  (with-output-to-temp-buffer "*translate-temp*"
    (print "<Temp buffer show translate history. Type 'q' to close.>")
    (print "--------------------------------------------------------")
    (loop for k being the hash-key of bingtranslate-history-hash do
          (print k)
          (print (gethash k bingtranslate-history-hash))
          (print "--------------------------------------------------------"))
    (switch-to-buffer "*translate-temp*")))

(defun bingtranslate-add-pair (key from to)
  "Add a pair of language for translate."
  (interactive)
  ;; add to language list
  (setq result (gethash key bingtranslate-language-pair-hash))
  (if (or (member key bingtranslate-language-pair-list)
          (or (member key bingtranslate-language-list)
              (and (not (equal "" result))
                   (not (equal nil result)))))
      (message (format "[bingtranslate] Language pair named %s exist!" key))
    (progn
      (setq bingtranslate-language-pair-list
            (append bingtranslate-language-pair-list (list key)))
      (puthash key (list from to) bingtranslate-language-pair-hash))))

(condition-case nil
    (bingtranslate-checklanguageslist)
  (error
   (message "[bingtranslate] Cannot connect to network!")))

(provide 'bing-translate)

;;; bing-translate.el ends here
