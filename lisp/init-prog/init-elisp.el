;;; init-elisp.el --- emacs 配置
;;; Commentary:
;;; Code:

;; 添加文档提示功能
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; macrostep -- 宏扩展
(use-package macrostep
  :bind (
         :map emacs-lisp-mode-map
			  ("C-c e" . macrostep-expand)
			  :map lisp-interaction-mode-map
			  ("C-c e" . macrostep-expand)
              ;; 在C-语言中也可使用
              :map c-mode-base-map
              ("C-c e" . macrostep-expand)
              ))

(provide 'init-elisp)
;;; init-elisp.el ends here
