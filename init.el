;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;包管理
;;添加MELPA源启用安装包
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;加载common lisp库
(require 'cl)

(defvar my-packges '(
		     company;;自动补全
		     zenburn-theme;;主题
		     hungry-delete;;快速删除空白
		     ;; smex;;提升M-x命令的使用效果
		     swiper;;C-s搜索提升3件套
		     ivy
		     counsel
		     smartparens;;自动补全括号
		     js2-mode;;jsIDE
		     nodejs-repl;;在emacs中使用node执行js代码
		     markdown-mode;;支持markdown语法
		     magit
		      )
  "所有自己需安装的包")

(defun my-packges-installed-p()
  "检查my-pacakges中的所有包是否都已安装
如果有未安装的包，返回nil，否则返回t"
  (loop for pkg in my-packges
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

;;安装my-packages中未安装的包
(if (my-packges-installed-p)
    (message "所有包都已安装")
    (progn
      (message "安装未安装的包...")
      (dolist (pkg my-packges)
	(when (not (package-installed-p pkg))
	  (message (format "正在安装%S" pkg))
	  (package-install pkg)))
      (message "包更新完毕")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;对已安装各包的控制
;;company全局打开company补全功能
(global-company-mode t)
(setq-default company-idle-delay 0.01
	      company-minimum-prefix-length 1)

;;主题theme配置
;;Solarized主题有点奇葩，一般加载主题使用的就是(load-theme 'xxx)，为啥会这样？
;;(require 'solarized)
;;(deftheme solarized-dark "The dark variant of the Solarized colour theme")
;;(create-solarized-theme 'dark 'solarized-dark)
;;(provide-theme 'solarized-dark)
;;使用zenburn主题主要是为了兼容非图像界面使用问题
(load-theme 'zenburn t)

;;hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)

;;smex提升M-x命令的使用效果！！！有了swiper插件3件套就没有卵用
;;(require 'smex) ;; Not needed if you use package.el
;;(smex-initialize) ;; Can be omitted. This might cause a (minimal) delay  when Smex is auto-initialized on its first run.

;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;swiper提升搜索性能
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-h C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-x v") 'counsel-describe-variable)
;;(global-set-key (kbd "<f1> l") 'counsel-find-library)
;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
;;(global-set-key (kbd "C-c j") 'counsel-git-grep)
;;(global-set-key (kbd "C-c k") 'counsel-ag)
;;(global-set-key (kbd "C-x l") 'counsel-locate)
;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;;smartparens 自动补全括号和引号等
;;(require 'smartparens-config)
(smartparens-global-mode t)

;;js2-mode
(setq auto-mode-alist
      (append
	'(("\\.js\\'" . js2-mode))
	auto-mode-alist))
;;(setq js2-external-variable '((t (:foreground "dark gray")))) ？？？？不能生效

;;nodejs-repl
(require 'nodejs-repl)
;;使用M-x nodejs-repl执行
;;M-x nodejs-repl-send-buffer执行当前文件中的js代码

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;菜单等工具栏
;;关闭菜单、工具和滚动条
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
;;(global-linum-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;随时打开配置文件<F2>
(global-set-key
 (kbd "<f2>") 
 (lambda ()
   (interactive)
   (find-file "~/.emacs.d/init.el")))

;;改变光标样式
;setq 和 setq-default 的区别：
;对于buffer.local的变量，使用setq只在当前buffer中有效
(setq-default cursor-type 'bar)

;;禁用备份文件
(setq make-backup-files nil)

;; 使org-mode中的语法高亮
(require 'org)
(setq org-src-fontify-natively t)

;;启用保存最近打开文档，下次打开时可快速打开
;加载模块
(require 'recentf)
;启用模块，Elisp中使用1和t效果相同
(recentf-mode 1)
;设置保存文件的最大个数，默认为20
(setq-default recentf-max-saved-items 100)
;设置对应快捷键
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;开启时全屏显示
(setq
 initial-frame-alist '((fullscreen . maximized)))

;;高亮匹配括号
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;;高亮当前行
(global-hl-line-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;日程管理
;;设置org agenda的默认文件夹
(setq org-agenda-files '("~/.agenda"))
(global-set-key (kbd "C-c a") 'org-agenda)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
