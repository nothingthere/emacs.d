;;; init-clang.el --- C语言环境配置
;;; Commentary:
;;; Code:
;; (add-hook 'c-mode-hook
;; 		  (lambda ()
;; 			(font-lock-add-keywords
;; 			 nil
;; 			 '(("\\<\\(FIXME\\):" 2
;; 				font-lock-warning-face t)))))

;; macrostep -- 原来此插件还可以支持C语言的宏扩展
(use-package macrostep
  :bind
  (:map c-mode-map
		("C-c e" . macrostep-expand)))

(provide 'init-clang)
;;; init-clang.el ends here