;;; init-edit-help.el ---  编辑体验提升配置
;;; Commentary:
;;; Code:

;; company -- 自动补全插件
(my/use-package
 (:pkg company
       :require-p nil			;自动加载，所以不要require
       :keys
       (("M-p" nil "禁用M-p向上选择" company-active-map)
	("M-n" nil "禁用M-n向下选择" company-active-map)
	("C-p" company-select-previous "向上选择" company-active-map)
	("C-n" company-select-next "向上选择" company-active-map)
 	))
 (global-company-mode t)
 (setq-default company-idle-delay 0.01	;等待时间"秒"
	       company-minimum-prefix-length 3);输入多少个字符时激活
 )

;;recentf --启用保存最近打开文档，下次打开时可快速打开
(my/use-package
 (:req recentf)
 (recentf-mode 1)
 (setq-default recentf-max-saved-items 1000)
 )

;; swiper --- 3件套：swiper ivy counsel
(my/use-package
 (:pkg swiper
       :require-p nil
       :keys (("C-s" swiper "搜索命令")))
 )
(my/use-package
 (:pkg ivy :require-p nil)
 (ivy-mode 1)
 (setq ivy-use-virtual-buffers t)
 )

(my/use-package
 (:pkg counsel
       :require-p nil
       :keys (("C-c g" counsel-git "获取处在git项目中的文件")
	      ("M-s i" counsel-imenu "获取当前文档索引（函数定义...）")
	      ))

 )

;; smartparens -- 自动补全括号
(my/use-package
 (:pkg smartparens :require-p nil)
 (smartparens-global-mode t)
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
(my/use-package
 (:pkg popwin)
 (popwin-mode 1)
 )

;; expand-region -- 方便选中文本
(my/use-package
 (:pkg expand-region
       :keys (("C-c =" er/expand-region "方便选择文本")))
 )

;; multiple-cursors -- 多行编辑
(my/use-package
 (:pkg multiple-cursors
       :keys (("C-c ;" mc/mark-all-dwim "标记所有相同")
	      ("C-c d" mc/mark-next-like-this "标记下一个")
	      ("C-c D" mc/skip-to-next-like-this "跳过此处")))
 )

;; helm-ag -- 项目内快速搜索
(my/use-package
 (:pkg helm-ag
       :require-p nil
       :keys (("C-x f" helm-ag-project-root "在项目内全局查找")))

 (my/ensure-system-configed "ag" :pkg-name "helm-ag" :apt-name "silversearcher-ag")
 )

;; yasnippet -- snippets片段补全
(my/use-package
 (:pkg yasnippet
       :keys (("<tab>" nil "禁用yansnippets默认键" yas-minor-mode-map)
	      ("TAB" nil "禁用yansnippets默认键" yas-minor-mode-map)
	      ))
 (yas-reload-all)

 (add-hook 'prog-mode-hook 'yas-minor-mode)
 ;; 1.组织:http://joaotavora.github.io/yasnippet/snippet-organization.html
 ;; 2.扩张:http://joaotavora.github.io/yasnippet/snippet-organization.html
 ;; 默认补全快捷键为<tab>，对应函数为yas-expand
 (setq yas-fallback-behavior nil)	;nil表示不做任何行为
 ;; 查看当前mode下所有snippets的函数为： yas-insert-snippet
 ;;使用hippie-expand备选
 ;; (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
 ;; 使用不在当前没有的mode的snippets用法：
 ;; (add-hook '当前的-minor-mode-hook
 ;; 	   #'(lambda ()
 ;; 	       (yas-activate-extra-mode '需要的-mode)))
 
 ;; helm-c-yansnippet -- 在下拉菜单选取snippets
 (my/use-package
  (:pkg helm-c-yasnippet
	:keys (("C-c y" helm-yas-complete "helm-yas-complete使用下拉菜单选取snippets"
		yas-minor-mode-map)))
  (setq helm-yas-space-match-any-greedy t) ;贪婪匹配
  )
 ;; end helm-c-yasnippet
 )

(my/set-keys ("C-c TAB" hippie-expand "打开hippie-expand")
	     ("M-s o" (lambda()()
			(interactive)
			(push (if(region-active-p)
				  (buffer-substring-no-properties (region-beginning) (region-end))
				(let((sym (thing-at-point 'symbol)))
				  (if (stringp sym)
				      (regexp-quote sym)
				    sym)))
			      regexp-history)
			(call-interactively 'occur))
	      "提升occur-mode性能，默认备选为选择/光标处单词")
	     ("M-;" my/comment-or-uncomment "注释优化")
	     ("<up>" windmove-up "移到上一个窗口")
	     ("<down>" windmove-down "移到下一个窗口")
	     ("<left>" windmove-left "移到左边窗口")
	     ("<right>" windmove-right "移到右边")
	     )

(provide 'init-edit-help)
;;; init-edit-help.el ends here
