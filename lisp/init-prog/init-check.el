;; init-check.el --- 编程语言语法检测
;; Author:Claudio <m15982038632@gmial.com>
;; Created: 2017
;;; Commentary:
;;; Code:

;; flycheck -- 语法检查
(use-package flycheck
  :config
  (diminish 'flycheck-mode "FlyC")
  (global-flycheck-mode 1)

  ;; elisp
  ;; flycheck对elisp的错误检测好烦人，出现好多不合适的提示，禁用。
  ;; 还是自己对flycheck的checker配置不合适？？
  (setq-default flycheck-disabled-checkers '(emacs-lisp))

  ;; python
  ;; 将检测python的版本更换到3.X
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 直接使用pylint3有时会报错
  ;; 解决办法：使用pylint：sudo apt install pylint
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (claudio/with-sys-enabled ("pylint")
                            (setq flycheck-python-pylint-executable
                                  (or (executable-find "pylint")
                                      flycheck-python-pylint-executable)

                                  ;; 还需添加下面这行才能生效
                                  flycheck-python-pycompile-executable "python3"))

  )

(provide 'init-check)
;;; init-check.el ends here
