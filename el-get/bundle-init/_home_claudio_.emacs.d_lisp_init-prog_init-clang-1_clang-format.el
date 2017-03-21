(my/with-system-enabled
 ("clang-format" :pkg-name "clang-format"))
(defun my/clang-format-enable-on-save nil "保存前执行clang-format的hook.\n模仿py-autopep8-enable-on-save的作法。"
       (add-hook 'before-save-hook 'clang-format-buffer nil t))
(loop for hook in
      '(c-mode-hook c++-mode-hook js-mode-hook)
      do
      (add-hook hook 'my/clang-format-enable-on-save))
