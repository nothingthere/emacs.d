;;; init-edit-help.el ---  编辑体验提升配置
;;; Commentary:
;;; Code:

;; company -- 自动补全插件
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq-default company-idle-delay 0.01;等待时间"秒"
		company-minimum-prefix-length 1);输入多少个字符时激活
  :bind
  (:map company-active-map
	("M-p" . nil)
	("M-n" . nil)
	("C-p" . company-select-previous)
	("C-n" . company-select-next))
  )

;;recentf --启用保存最近打开文档，下次打开时可快速打开
(use-package recentf
  :config
  (setq-default recentf-max-saved-items 1000)
  )

;; swiper --- 3件套：swiper ivy counsel
(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper)
  )

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  )

(use-package counsel
  :ensure t
  :config
  :bind (("C-c g" . counsel-git)
	 ("M-s i" . counsel-imenu)
	 )
  )

;; smartparens -- 自动补全括号
(use-package smartparens
  :ensure t
  :config
  ;; elisp 和 common-lisp 中不自动补全单引号、反引号和括号
  (dolist (X '("'" "`" "("))
    (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) X nil :actions nil))

  ;; 处在括号间时，自动高亮括号
  '(defadvice show-paren-function(around fix-show-paren-function activate)
     ;;处在代码中时高亮两边的括号？？还没理解怎么用defadvice
     (cond ((looking-at-p "\\s(") ad-do-it)
	   (t (save-excursion
		(ignore-errors (backward-up-list))
		ad-do-it)))
     )
  )

;; popwin -- 使光标跳转到帮助窗口
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  )

;; expand-region -- 方便选中文本
(use-package expand-region
  :ensure t
  :bind
  :bind ("C-c =" . er/expand-region)
  )

;; multiple-cursors -- 多行编辑
(use-package multiple-cursors
  :ensure t
  :bind (("C-c ;" . mc/mark-all-dwim)
	 ("C-c d" . mc/mark-next-like-this)
	 ("C-c D" . mc/skip-to-next-like-this))
  )

;; helm-ag -- 项目内快速搜索
(use-package helm-ag
  :ensure t
  :init
  (my/ensure-system-configed "ag" :pkg-name "helm-ag" :apt-name "silversearcher-ag")
  :bind ("C-x f" . helm-ag-project-root)
  )

;; yasnippet -- snippets片段补全
(use-package yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
	      ("<tab>" . nil) ;禁用yansnippets默认键
	      ("TAB" . nil))
  ;; :defer 30
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (setq yas-fallback-behavior nil)
  (use-package helm-c-yasnippet
    :ensure t
    ;; :defer 40
    :config
    (setq helm-yas-space-match-any-greedy t) ;贪婪匹配
    :bind (:map yas-minor-mode-map
		("C-c y" . helm-yas-complete);helm-yas-complete使用下拉菜单选取snippets
		))
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
 ("<up>" . windmove-up)
 ("<down>" . windmove-down)
 ("<left>" . windmove-left)
 ("<right>" . windmove-right)
 
 )

(provide 'init-edit-help)
;;; init-edit-help.el ends here
