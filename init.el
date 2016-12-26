;;; init.el --- 加载各个配置文件
;;; Commentary:
;;; Code:

;; 添加文件加载路径
(dolist (path '("~/.emacs.d/lisp/init-prog/"
		"~/.emacs.d/lisp/"))
  (add-to-list 'load-path path))

(defvar *my/init-files* nil "所有被加载的配置文件.")
(defmacro my/require-init-files(&rest files)
  "加载配置文件FILES:file1 file2 ..."
  `(dolist (file ',files)
     (require file)
     (pushnew file *my/init-files*)))

;; 将customize文件重置位置
(setq custom-file "~/.emacs.d/custom.el")

;;加载所有配置文件
(my/require-init-files
 init-util;;辅助函数
 init-package;;插件
 init-ui
 init-restore
 init-better-defaults
 init-edit-help
 init-prog
 init-beauty
 init-misc
 )

;;; init.el ends here
