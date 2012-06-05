WHAT IS BINGAPIEL FOR
==========

This is an open source elisp library to use bing-api in emacs.
Now bingapiel provides support for **bing translate api**. It
tested in emacs 24.

INSTALL
==========

Clone bingapiel from github.

    git clone git@github.com:coordinate/bingapiel.git

Install curl with *sudo apt-get install curl* on Ubuntu. Download
[curl Win32 - Generic](http://curl.haxx.se/gknw.net/7.26.0/dist-w32/curl-7.26.0-devel-mingw32.zip)
and add *curl.exe* to *PATH* on Windows.

    (setenv "PATH" (concat (concat program-path "/curl-7.26.0-devel-mingw32/bin;") (getenv "PATH")))

Configure bingapiel in your *.emacs* file. The following
*client_id* and *client_secret* are my. You'd better register
your own.

    (add-to-list 'load-path (concat plugin-path "bingapiel"))
    ;; Your bing api client_id.
    (defvar bingapi-clientid "667f3adb-e22d-4dca-b476-ca536c0a6d8a")
    ;; Your bing api client_secret.
    (defvar bingapi-clientsecret "xBTJ5Ee5RSFf++uVjSVKVFcsoswQlDyb8kPp5wSyrV8=")
    ;; list all language codes may be used
    (defvar bingtranslate-language-list '("en" "zh-CHS" "ja"))
    ;; Turn auto detect language code on
    (defvar bingtranslate-auto-detect t)

    (require 'bing-translate)
    ;; key bounding
    (global-set-key [M-f1] 'bingtranslate-region-or-input)
    ;; add a pair of language
    ;; Parameters: "pair name" "from language" "to language"
    (bingtranslate-add-pair "1" "zh-CHS" "ja")

Restart emacs or eval-region.

*M-x bingtranslate-region-or-input* and input text, from languge,
to language. Then translation results show in minibuffer and
shear plate.

*M-x bingtranslate-show-history* will show translate history at a
temp buffer.

REGISTER BING-API
==========

Register [Windows Live ID](https://signup.live.com/signup.aspx).

Subscribe to the Microsoft Translator API on
[Azure Marketplace](http://go.microsoft.com/?linkid=9782667). Basic
subscriptions, up to 2 million characters a month, are free.

Register your application
[Azure DataMarket](https://datamarket.azure.com/developer/applications/).
Click on "Register". In the "Register your application" dialog
box, you can define your own Client ID and Name. Take a note of
the client ID and the client secret value.

MORE INFORMATION
==========

You can find more information at:

-[My github](https://github.com/coordinate/bingapiel)

-[My blog](http://coordinate.sinaapp.com/?p=178)
