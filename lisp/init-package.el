;;;;;;;;;;;;;;;;;;;;;;;;;;;插件初始化
;;添加MELPA源启用安装包
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(defvar *my-packges* '(
		       company;;自动补全
		       zenburn-theme;;主题
					;hungry-delete;;快速删除空白
		       swiper;;C-s搜索提升3件套
		       ivy
		       counsel
		       smartparens;;自动补全括号
		       js2-mode;;jsIDE
		       js2-refactor;;
		       nodejs-repl;;在emacs中使用node执行js代码
		       markdown-mode;;支持markdown语法
		       magit;;emacs的github
		       popwin;;使光标跳转到帮助窗口
		       web-mode;;
		       session;;保存上次会话
		       expand-region;;
		       iedit
		       emmet-mode
		       )
  "所有自己需安装的包")

(defun my-install-all-packages()
  "安装所有指定安装包"
  (interactive)
  (dolist (pkg *my-packges*)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(my-install-all-packages)
;;;;;;;;;;;对各个包的设置
;;company全局打开company补全功能
(global-company-mode t)
(setq-default company-idle-delay 0.01
	      company-minimum-prefix-length 3)

;;recentf启用保存最近打开文档，下次打开时可快速打开
					;加载模块
(require 'recentf)
					;启用模块，Elisp中使用1和t效果相同
(recentf-mode 1)
					;设置保存文件的最大个数，默认为20
(setq-default recentf-max-saved-items 100)

;;hungry-delete
;;(require 'hungry-delete)
;;(global-hungry-delete-mode)

;;swiper提升搜索性能
(ivy-mode 1)

;;smartparens 自动补全括号和引号等
;;(require 'smartparens-config)
(smartparens-global-mode t)
;; elisp和cl模式下输入单引号时只有一个但引号
(sp-local-pair '(emacs-lisp-mode common-lisp-mode) "'" nil :actions nil)
(sp-local-pair '(emacs-lisp-mode common-lisp-mode) "(" nil :actions nil)
;;处在代码中时高亮两边的括号？？还没理解怎么用defadvice
'(defadvice show-paren-function (around fix-show-paren-function)
   "Highlight enclosing parens."
   (cond ((looking-at-p "\\s(") (funcall #'show-paren-function))
	 (t (save-excursion
	      (ignore-errors (backward-up-list))
	      (funcall #'show-paren-function)))))
'(ad-activate #'show-paren-function)

;;js2-mode
;;(setq js2-external-variable '((t (:foreground "dark gray")))) ？？？？不能生效

;;js2-refactor-mode
;;(add-hook 'js2-mode 'js2-refactor-mode)
;;(js2r-add-keybindings-with-prefix (kbd ""))
;;nodejs-repl
(require 'nodejs-repl)
;;使用M-x nodejs-repl执行
;;M-x nodejs-repl-send-buffer执行当前文件中的js代码

;;popwin；使光标跳转到帮助窗口
(require 'popwin)
(popwin-mode 1)

;;web-mode设置
;;设置默认缩进
(add-hook 'web-mode-hook
	  (lambda ()
	    (setq web-mode-markup-indent-offset 2
		  web-mode-css-indent-offset 2
		  web-mode-code-indent-offset 2)))

;;expand-region
(require 'expand-region)

;;iedit
(require 'iedit)

;;保存上次会话
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)

;;emmet
(require 'emmet-mode)
;;;;;;;;;;;
(provide 'init-package)
