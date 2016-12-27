;;; init.el --- 加载各个配置文件
;;; Commentary:
;;; Code:

;; 插件安装配置
;; 插件初始化
;;添加MELPA源启用安装包
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; 使用use-package管理插件
(use-package use-package
  :ensure t
  :config
  (eval-when-compile
    (require 'use-package))
  (require 'diminish) ;;使用:diminish关键字
  (require 'bind-key) ;;属于:bind关键字
  :bind ("C-c ." . describe-personal-keybindings)
  )

;; 本地加载配置
;; 添加文件加载路径
(dolist (path '("~/.emacs.d/lisp/init-prog/"
		"~/.emacs.d/lisp/"))
  (add-to-list 'load-path path))

(defvar *my/init-files* nil "所有被加载的配置文件.")
(defmacro my/require-init-files(&rest files)
  "加载配置文件FILES:file1 file2 ..."
  `(dolist (file ',files)
     (require file)
     (push file *my/init-files*)))

;; 将customize文件重置位置
(setq custom-file "~/.emacs.d/custom.el")

;;加载所有配置文件
(my/require-init-files
 init-util;;辅助函数
 init-ui
 init-restore

 init-better-defaults
 init-edit-help
 init-beauty
 init-prog

;;; init-misc

 )

;;; init.el ends here
