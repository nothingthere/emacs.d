;;; init-edit-help.el ---  编辑体验提升配置
;;; Commentary:
;;; Code:

;; company -- 自动补全插件
(my/use-package
 (:pkg company :require-p nil)		;自动加载，所以不要require
 (global-company-mode t)
 (setq-default company-idle-delay 0.01	;等待时间"秒"
	       company-minimum-prefix-length 1);输入多少个字符时激活
 )

;;recentf --启用保存最近打开文档，下次打开时可快速打开
(my/use-package
 (:req recentf)
 (recentf-mode 1)
 (setq-default recentf-max-saved-items 1000)
 )

;; swiper --- 3件套：swiper ivy counsel
(my/use-package
 (:pkg swiper :require-p nil)
 )
(my/use-package
 (:pkg ivy :require-p nil)
 (ivy-mode 1)
 (setq ivy-use-virtual-buffers t)
 )

(my/use-package
 (:pkg counsel :require-p nil)
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
 (:pkg expand-region)
 )

;; multiple-cursors -- 多行编辑
(my/use-package
 (:pkg multiple-cursors))

;; helm-ag -- 项目内快速搜索
(my/use-package
 (:pkg helm-ag :require-p nil)
 (my/ensure-system-configed "ag" :pkg-name "helm-ag" :apt-name "silversearcher-ag")
 )

;; yasnippet -- snippets片段补全
(my/use-package
 (:pkg yasnippet)
 (yas-reload-all)
 (add-hook 'prog-mode-hook 'yas-minor-mode)
 ;; 1.组织:http://joaotavora.github.io/yasnippet/snippet-organization.html
 ;; 为提高加载速度，不加载yasnipets安装包内的snippets内容
 ;; ！！！不过需手动设置.emacs.d/snippets内容TODO
 ;; (setf yas-snippet-dirs (concat user-emacs-directory "snippents"))
 ;;除需要的mode外，在其他mode对应的文件夹下创建.yas-skip空文件，不加载该文件
 ;; (defvar *my/yas-modes* '() "需要使用snippets的mode")
 ;;编译文件TODO

 ;; 2.扩张:http://joaotavora.github.io/yasnippet/snippet-organization.html
 ;; 默认补全快捷键为<tab>，对应函数为yas-expand
 ;;取消掉没有补全备选时的默认行为，
 (setq yas-fallback-behavior nil)	;nil表示不做任何行为
 ;; 查看当前mode下所有snippets的函数为： yas-insert-snippet
 ;;使用hippie-expand备选
 ;; (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
 ;; 使用不在当前没有的mode的snippets用法：
 ;; (add-hook '当前的-minor-mode-hook
 ;; 	   #'(lambda ()
 ;; 	       (yas-activate-extra-mode '需要的-mode)))
 
 )

;; helm-c-yansnippet -- 在下拉菜单选取snippets
(my/use-package
 (:pkg helm-c-yasnippet)
 (setq helm-yas-space-match-any-greedy t) ;贪婪匹配

 )

(provide 'init-edit-help)
;;; init-edit-help.el ends here
