;; init-check.el --- 编程语言语法检测
;;; Commentary:
;;; Code:

;; flycheck -- 语法检查
(use-package flycheck

  :config
  (global-flycheck-mode 1)

  ;; elisp
  ;; flycheck对elisp的错误检测好烦人，出现好多不合适的提示，禁用。
  ;; 还是自己对flycheck的checker配置不合适？？
  (setq-default flycheck-disabled-checkers '(emacs-lisp))

  ;; python
  ;; 将检测python的版本更换到3.X
  (my/with-system-enabled
   ("pylint3")
   (setq flycheck-python-pylint-executable
         (or (executable-find "pylint3") "pylint3"))
   ;; 还需添加下面这行才能生效
   (setq flycheck-python-pycompile-executable "python3"))
  )

(provide 'init-check)
;;; init-check.el ends here
