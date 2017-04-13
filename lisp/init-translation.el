;;; init-translation.el --- 中英翻译配置
;; Author: Claudio <3261958605@qq.com.com>
;; Created: 2017-04-13 14:06:10
;;; Commentary:
;; 使用Bing的API翻译
;; 有空希望自己写一个
;;; Code:

(use-package bing-dict
  :bind
  ("C-c b" . bing-dict-brief))

(provide 'init-translation)
;;; init-translation.el ends here
