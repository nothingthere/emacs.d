;;; init-clang.el --- C语言环境配置
;;; Commentary:
;;; Code:

;; clang-format -- 格式化c语言
(use-package clang-format
  :demand t
  :init
  (my/with-system-enabled ("clang-format" :pkg-name "clang-format"))
  :config
  (defun my/clang-format-enable-on-save()
    "保存前执行clang-format的hook.
模仿py-autopep8-enable-on-save的作法。"
    ;; (interactive)
    (add-hook 'before-save-hook 'clang-format-buffer nil t))

  (add-hook 'c-mode-hook 'my/clang-format-enable-on-save))

(provide 'init-clang)
;;; init-clang.el ends here
