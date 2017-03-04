;;; init-edit-help.el ---  编辑体验提升配置
;;; Commentary:
;;; Code:

;; company -- 自动补全插件
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (:map company-active-map
		;; ("M-p" . nil)
		;; ("M-n" . nil)
		("C-p" . company-select-previous)
		("C-n" . company-select-next))
  :config
  ;; 添加英语补全？？？好像没用
  ;; (setq company-backends (append company-backends
  ;; 								 '(company-ispell)))

  (setq-default company-idle-delay 0.001;等待时间"秒"
				company-minimum-prefix-length 1);输入多少个字符时激活

  ;; !!! 在图像界面下才能使用
  ;; company-quickhelp -- 代码提示功能
  (when (display-graphic-p)
	(use-package company-quickhelp
	  :demand 1
	  :config
	  (bind-key "M-h" 'company-quickhelp-manual-begin
				company-active-map
				(featurep 'company))))

  )

;; yasnippet -- snippets片段补全
(use-package yasnippet
  ;; :demand t
  :bind (:map yas-minor-mode-map
			  ("<tab>" . nil) ;禁用yansnippets默认键
			  ("TAB" . nil))

  :config
  ;; (defvar *my/yas-mode-hooks* '(prog-mode-hook org-mode-hook) "使用yasnippets的所有Major mode.")
  ;; (yas-reload-all)
  ;; (add-hook 'prog-mode-hook 'yas-minor-mode)
  (yas-global-mode 1)

  (setq yas-fallback-behavior nil)

  ;; 将yasnippets的内容添加到company的备选中
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (my/with-pkg-enabled					;当company启动后才使用
   company

   (defun company-mode/backend-with-yas (backend)
	 "将yasnipets的补全添加到compny中."
	 (if  (and (listp backend) (member 'company-yasnippet backend))
		 backend
	   (append
		(if (consp backend) backend (list backend))
		'(:with company-yasnippet)
		)))

   (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
  ;; end 将yasnippets备选添加到company中

  )

;;recentf --启用保存最近打开文档，下次打开时可快速打开
(use-package recentf
  :config
  (setq-default recentf-max-saved-items 1000)
  )

;; swiper --- 3件套：swiper ivy counsel
(use-package swiper
  :bind
  ("C-s" . swiper)
  )

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)		;使用recentf和bookmarks中的内容作为备选
  )

(use-package counsel
  :config
  :bind (("C-c g" . counsel-git)
		 ("M-s i" . counsel-imenu)
		 ;; 与此快捷键的默认相比添加了^通配符
		 ("M-x" . counsel-M-x)
		 ("C-h f" . counsel-describe-function)
		 ("C-h v" . counsel-describe-variable)
		 ("C-x C-f" . counsel-find-file)
		 )
  )

;; smartparens -- 自动补全括号
(use-package smartparens
  :demand t
  :config
  ;; (smartparens-global-strict-mode t)
  (smartparens-global-mode t)
  ;; elisp 和 common-lisp 中不自动补全单引号、反引号和括号
  (dolist (X '("'" "`" "("))
    (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) X nil :actions nil))
  )

;; popwin -- 使光标跳转到帮助窗口
(use-package popwin
  :demand t				;启动时就执行
  :config
  (popwin-mode 1)
  )

;; expand-region -- 方便选中文本
(use-package expand-region
  :bind ("C-c =" . er/expand-region)
  )

;; multiple-cursors -- 多行编辑
(use-package multiple-cursors
  :bind
  (("C-c ;" . mc/mark-all-dwim)
   ("C-c d" . mc/mark-next-like-this)
   ("C-c D" . mc/skip-to-next-like-this))
  )

;; helm-ag -- 项目内快速搜索
(use-package helm-ag
  :init
  (my/with-system-enabled ("ag" :pkg-name "helm-ag" :apt-name "silversearch-ag"))
  :bind ("C-x f" . helm-ag-project-root)
  )

;; avy -- 根据字符跳转??还没搞清有啥子用
(use-package avy
  :bind("C-c :" . avy-goto-char-2)
  )

;; acs-pinyin -- ？？？？
(use-package ace-pinyin
  :disabled t
  )

;; which-key -- 当按下快捷键忘记后面键位时，停留后显示提示
(use-package which-key
  :demand t
  :config
  (which-key-mode)
  )

;;;;;;方便编辑的快捷键

(bind-keys
 ("C-c TAB" . hippie-expand)
 ("M-s o" . (lambda()()
			  "提升occur-mode性能，默认备选为选择/光标处单词"
			  (interactive)
			  (push (if(region-active-p)
						(buffer-substring-no-properties (region-beginning) (region-end))
					  (let((sym (thing-at-point 'symbol)))
						(if (stringp sym)
							(regexp-quote sym)
						  sym)))
					regexp-history)
			  (call-interactively 'occur)))
 ("M-;" . (lambda()
			"修改后的注释函数。
1. 如果有选中区域，注释/去注释该区域
2. 如果为空行，仅注释
3n. 如果在代码行末，注释并缩进

对于有些没后缀的文件，如.bashrc也需注释，所以对所有文件都应用。

缺点：不能通过快捷键去注释行末注释"
			(interactive)
			(cond ((my/current-line-empty-p) (comment-dwim nil)) ;如果在空行上。不能理解comment-dwim的参数该怎样设置
				  ((my/at-end-of-line-p) (comment-indent))	;如果在行末，前面有内容
				  (t						;默认注释选中区域和本行
				   (let* ((region (my/get-region-or-get-the-line-as-region))
						  (start (car region))
						  (end (cdr region)))
					 (comment-or-uncomment-region start end))))))

 ;; 将此行与下一行合并
 ;; 原来有合并行的命令 delete-indentation M-^
 ;; ("C-c j" . (lambda()
 ;; 			  "合并下一行"
 ;; 			  (interactive)
 ;; 			  (unless (eobp)
 ;; 				(goto-char (line-end-position))
 ;; 				(just-one-space -1)
 ;; 				(goto-char (line-end-position)))))

 )

(provide 'init-edit-help)
;;; init-edit-help.el ends here
