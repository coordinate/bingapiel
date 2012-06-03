;;; bing-api.el --- Use bing api

;; Copyright (C) 2012 zxy
;; Author: zxy <gcoordinate@gmail.com>
;; Maintainer: zxy <gcoordinate@gmail.com>
;; Created: May 2012
;; Version: 1.0

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
;; Do not need to install
;;

(require 'url)
(message (concat "Loading " load-file-name))

;; public var

(defcustom bingapi-oauth-service "https://datamarket.accesscontrol.windows.net/v2/OAuth2-13"
  "Service to use for bing api."
  :group 'bingapi
  :type 'string)

(defcustom bingapi-clientid ""
  "client id."
  :group 'bingapi
  :type 'string)

(defcustom bingapi-clientsecret ""
  "client secret."
  :group 'bingapi
  :type 'string)

(defcustom bingapi-scope "http://api.microsofttranslator.com"
  "client secret."
  :group 'bingapi
  :type 'string)

;; defun

(defun bingapi-make-url (request-parameters-list)
  "Generate the url to send to bing api service."
  (setq bing-service (concat "\""bingapi-service ""))
  (while request-parameters-list
    (setq bing-service (concat bing-service (car request-parameters-list)))
    (setq request-parameters-list (cdr request-parameters-list)))
  (concat bingapi-service"\""))

(defun bingapi-post-and-get-accesstoken ()
  "Post your ID and get access_token."
  (let* ((command (format "curl -d \"grant_type=%s&client_id=%s&client_secret=%s&scope=%s\" %s"
                          "client_credentials"
                          (url-hexify-string bingapi-clientid)
                          (url-hexify-string bingapi-clientsecret)
                          bingapi-scope
                          "https://datamarket.accesscontrol.windows.net/v2/OAuth2-13"))
         (output (shell-command-to-string command))
         (result nil))
    (when (string-match "{\"access_token\":\"\\(.*\\)\",\"token_type\"" output 0)
      (setq result (match-string 1 output)))
    (when (equal nil result)
      (error "[bingapi] Cannot get access_token!"))
    ;;(url-hexify-string result)
    result
    ))

(defun bingapi-check-accesstoken (xmldata)
  "Check if access_token expired."
  (if (string-match "Get a new access token from the Authorization Server" xmldata 0)
      (progn
        (setq bingtranslate-appId (bingapi-post-and-get-accesstoken))
        (message "[bingapi] The incoming token has expired! We got new one.")
        "expired")
    (if (string-match "Invalid URL" xmldata 0)
        (error "[bingapi] Your translate text maybe too long."))))

(provide 'bing-api)

;;; bing-api.el ends here
