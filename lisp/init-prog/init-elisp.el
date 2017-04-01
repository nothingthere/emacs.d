;;; init-elisp.el --- emacs 配置
;; Author:Claudio <m15982038632@gmial.com>
;; Created: 2017
;;; Commentary:
;;; Code:

;; 添加文档提示功能
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; flylisp -- 提醒不配对括号
(use-package flylisp
  :config
  (set-face-attribute 'fl-mismatched-face nil
                      :weight 'bold
                      :underline t
                      :foreground "green")
  (dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
    (add-hook hook 'flylisp-mode)))

;; macrostep -- 宏扩展
(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)
        :map lisp-interaction-mode-map
        ("C-c e" . macrostep-expand)))

(provide 'init-elisp)
;;; init-elisp.el ends here
