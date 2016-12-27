;;; init-elisp.el --- emacs 配置
;;; Commentary:
;;; Code:


;; macrostep -- 宏扩展
(use-package macrostep			
  :ensure t
  :bind (:map emacs-lisp-mode-map
	      ("C-c e" . macrostep-expand)
	      :map lisp-interaction-mode-map
	      ("C-c e" . macrostep-expand)))




(provide 'init-elisp)
;;; init-elisp.el ends here
