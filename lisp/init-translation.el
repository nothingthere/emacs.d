;;; init-translation.el --- 中英翻译配置
;; Author: Claudio <3261958605@qq.com.com>
;; Created: 2017-04-13 14:06:10
;;; Commentary:
;; 使用Bing的API翻译
;; 网上抄的，有空自己写一个：https://github.com/coordinate/bingapiel
;;; Code:

(add-to-list 'load-path "~/.emacs.d/bingapiel")

(defvar bingapi-clientid "667f3adb-e22d-4dca-b476-ca536c0a6d8a")
;; Your bing api client_secret.
(defvar bingapi-clientsecret "xBTJ5Ee5RSFf++uVjSVKVFcsoswQlDyb8kPp5wSyrV8=")
;; list all language codes may be used
(defvar bingtranslate-language-list '("en" "zh-CHS"))
;; Turn auto detect language code on
(defvar bingtranslate-auto-detect t)
(require 'bing-translate)

(bingtranslate-add-pair "英->中" "en" "zh-CHS")
(bingtranslate-add-pair "中->英" "zh-CHS" "en")

(bind-key "C-c b" #'bingtranslate-region-or-input)

(provide 'init-translation)
;;; init-translation.el ends here
