;;;js配置

;;nodejs
(my-install-all-packages
 '(
   nodejs-repl;;在emacs中使用node执行js代码"
   ))

;;nodejs-repl
(require 'nodejs-repl)
;;使用M-x nodejs-repl执行
;;M-x nodejs-repl-send-buffer执行当前文件中的js代码

;;;js
(my-install-all-packages
 '(
   js2-mode;;jsIDE
   js2-refactor;;
   ))
;;js2-mode
;;(setq js2-external-variable '((t (:foreground "dark gray")))) ？？？？不能生效

;;js2-refactor-mode
;;(add-hook 'js2-mode 'js2-refactor-mode)
;;(js2r-add-keybindings-with-prefix (kbd ""))

;;
(provide 'init-js)
