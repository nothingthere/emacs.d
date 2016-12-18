;;加载各个配置文件
(add-to-list 'load-path "~/.emacs.d/lisp")
(defvar *my-configs*
  '(
    init-util;;辅助函数
    init-package;;插件
    init-mode
    init-ui
    init-better-defaults
    init-org
    init-abbrev
    init-beauty
    init-keybindings
    )
  "所有需加载的配置文件")

(dolist (config *my-configs*)
  (require config))

;;;将customize文件重置位置
;;(setq custom-file "custom.el" user-emacs-directory)
;;;(global-auto-revert-mode 1)
