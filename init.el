;;加载各个配置文件 
(add-to-list 'load-path "~/.emacs.d/lisp/init-prog/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(defvar *my-configs*
  '(
    init-util;;辅助函数
    init-package;;插件
    init-better-defaults
    init-edit-help
    init-ui
    init-mode
    init-prog
    init-keybindings
    init-beauty
    init-abbrev
    )
  "所有需加载的配置文件")

(dolist (config *my-configs*)
  (require config))

;;;将customize文件重置位置
;;(setq custom-file "custom.el" user-emacs-directory)
;;;(global-auto-revert-mode 1)
