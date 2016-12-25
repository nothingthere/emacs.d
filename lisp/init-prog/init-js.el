;;; init-js.el --- js和nodejs配置
;;; Commentary:
;;; Code:

;; js2-mode -- ???
(my/use-package
 (:pkg js2-mode)
 (push '("\\.js\\'" . js2-mode) auto-mode-alist) ;.js文件自动使用js2-mode
 )

;; js2-refactor-mode -- ???
(my/use-package
 (:pkg js2-refactor :require-p nil)
 )

;;nodejs-repl -- 在emacs中使用node执行js代码
;;使用M-x nodejs-repl执行
;;M-x nodejs-repl-send-buffer执行当前文件中的js代码
(my/use-package
 (:pkg nodejs-repl)
 )

(provide 'init-js)
;;; init-js.el ends here
