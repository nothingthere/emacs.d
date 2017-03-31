;;; init.el --- 加载各个配置文件
;;; Commentary:
;;; Code:

;; 插件安装配置
;; 插件初始化
;;添加MELPA源启用安装包
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; 所有包安装都以来use-package，所以先手动安装此包
(when (not (package-installed-p 'use-package))
  (message "安装use-package...")
  (package-install 'use-package))

;; 使用use-package管理插件
(use-package use-package
  :config (setq
           ;; 启动时在*Message*中显示加载时间
           use-package-verbose nil
           ;; 所有包都需安装后才能使用
           use-package-always-ensure t
           ;; 所有包都延迟加载
           ;; use-package-always-defer t
           use-package-always-demand t)

  (eval-when-compile
    (require 'use-package))

  ;;使用:diminish关键字
  (require 'diminish)
  ;; 使用:bind关键字
  (require 'bind-key)

  :bind
  ("C-c ." . describe-personal-keybindings))

;; 启动quelpa，现在主要是为了安装自己的项目nothingthere/clang-format
(use-package quelpa
  :config (setq
           ;; 不更新自己
           quelpa-self-upgrade-p nil
           ;; 已经安装的包不更新
           quelpa-upgrade-p nil
           ;; 不跟新github上的melpa源
           quelpa-update-melpa-p nil
           ;; 只有quelpa安装不在melpa上的包
           quelpa-checkout-melpa-p nil
           ))

;; 本地加载配置
;; 添加文件加载路径
(dolist (path '(
                "~/.emacs.d/lisp/"
                ;; 编程语言相关配置
                "~/.emacs.d/lisp/init-prog/"
                ;; 模式文件路径
                ;; 副模式
                "~/.emacs.d/lisp/init-modes/"
                "~/.emacs.d/lisp/init-modes/init-minor-modes/"
                "~/.emacs.d/lisp/init-modes/init-minor-modes/claudio-fill/"
                ;; 主模式
                "~/.emacs.d/lisp/init-modes/init-major-modes/"
                ))
  (add-to-list 'load-path path))

(defvar *claudio/init-files* nil
  "所有被加载的配置文件，主要是为了方便查看加载了那些文件.")

(cl-defmacro claudio/require-init-files(&body files)
  "加载配置文件FILES:file1 file2 ..."
  `(dolist (file ',files)
     (require file)
     ;; 第三个参数使文件添加到链表最后
     (add-to-list '*claudio/init-files* file t)))

;; 将customize文件重置位置
(setq custom-file (concat user-emacs-directory "custom.el"))

;;加载所有配置文件
(claudio/require-init-files
 init-util
 init-ui
 init-restore
 init-better-defaults
 init-edit-help
 init-format
 init-prog
 init-broswe
 init-move
 init-chinese
 init-misc
 init-timestamp
 init-modes
 )

;;; init.el ends here
