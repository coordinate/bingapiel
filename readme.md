WHATE IS TRANSLATE-EMACS-TOOLKIT FOR
==========

This is a open source elisp liberary for emacs to use bing api.
It provides support for **bing translate api**.
It tested on emacs 24.

INSTALL
==========

1. Clone bingapiel from githum.

    git clone

2. Add bingapiel to load-path and configure.

    (add-to-list 'load-path "bingapiel-path")

    ;; Your bing api client_id.
    (defvar bingapi-clientid "bingapiel")

    ;; Your bing api client_secret.
    (defvar bingapi-clientsecret "bq4h4FPS14CBDCBs7tsqiqVD6YVG4bmt3ftkbKBQKmk=")

    ;; Your priority language to translate from.
    (defvar bingtranslate-from-priority "en")

    ;; Your priority language to translate to.
    (defvar bingtranslate-to-priority "zh-CHS")

    (require 'bing-translate)

    ;; key bounding
    (global-set-key [M-f1] 'bingtranslate-region-or-input)

    ;; add a pair of language
    ;; Parameters: "pair name" "from language" "to language"
    (bingtranslate-add-pair "1" "zh-CHS" "en")

3. Restart emacs or eval-region.

4. *M-x bingtranslate-region-or-input* and input
   text, from languge, to language. Then translation result show in minibuffer and shear plate.

5. *M-x bingtranslate-show-history* will show translate history at a temp buffer.

register BING API
==========

1.

MORE INFORMATION
==========

You can find more information at:

-[my github](http://github)

-[my blog](http://coordinate.sinaapp.com)
