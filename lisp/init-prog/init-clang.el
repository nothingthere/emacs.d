;;; init-clang.el --- C语言环境配置
;;; Commentary:
;;; Code:

;; clang-format -- 格式化c语言
(el-get-bundle nothingthere/clang-format
  ;; 确保本地安装了clang-format程序
  (my/with-system-enabled ("clang-format" :pkg-name "clang-format"))
  (defun my/clang-format-enable-on-save()
    "保存前执行clang-format的hook.
模仿py-autopep8-enable-on-save的作法。"
    ;; (interactive)
    ;; (setq clang-format-style "LLVM")
    (add-hook 'before-save-hook 'clang-format-buffer nil t))

  ;; 由于c++、js都可使用clang-format，所以此处一并添加
  (loop for hook in '(c-mode-hook c++-mode-hook js-mode-hook)
        do (add-hook hook 'my/clang-format-enable-on-save))
  )

;; macrostep -- 宏扩展
(use-package macrostep
  :bind (:map c-mode-map
              ("C-c e" . macrostep-expand)))

(provide 'init-clang)
;;; init-clang.el ends here
