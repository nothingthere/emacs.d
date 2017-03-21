;;; init-js.el --- js和nodejs配置
;;; Commentary:
;;; Code:

;; js2-mode -- ???
(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; :mode ("\\.js\\" . js2-mode)
  :config
  ;; js2-refactor-mode -- ???
  (use-package js2-refactor

    )

  ;;nodejs-repl -- 在emacs中使用node执行js代码
  ;;使用M-x nodejs-repl执行
  ;;M-x nodejs-repl-send-buffer执行当前文件中的js代码
  (use-package nodejs-repl
    )
  )

(provide 'init-js)
;;; init-js.el ends here
