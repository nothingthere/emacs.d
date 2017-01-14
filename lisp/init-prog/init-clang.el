;;; init-clang.el --- C语言环境配置
;;; Commentary:
;;; Code:

;; macrostep -- 原来此插件还可以支持C语言的宏扩展
(use-package macrostep
  :demand t
  :bind
  (:map c-mode-map
		("C-c e" . macrostep-expand)))

(provide 'init-clang)
;;; init-clang.el ends here
