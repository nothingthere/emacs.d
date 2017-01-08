;; init-check.el --- 编程语言语法检测
;;; Commentary:
;;; Code:

;; 拼写检查
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; flycheck -- 语法检查
(use-package flycheck
  :config
  (global-flycheck-mode 1)
  ;; flycheck对elisp的错误检测好烦人，出现好多不合适的提示，禁用。
  ;; 还是自己对flycheck的checker配置不合适？？
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp)))
  )
(provide 'init-check)
;;; init-check.el ends here
