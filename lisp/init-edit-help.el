;;;编辑体验提升配置
(my-install-all-packages
 '(
   company;;自动补全
   					;hungry-delete;;快速删除空白
   swiper;;C-s搜索提升3件套
   ivy
   counsel
   smartparens;;自动补全括号
   popwin;;使光标跳转到帮助窗口
   expand-region;;
   iedit
   multiple-cursors
   ))

(my-require '
 (
  recentf
  popwin
  expand-region
  iedit
  multiple-cursors
  ))

;;;;;;;;;;;对各个包的设置
;;company全局打开company补全功能
(global-company-mode t)
(setq-default company-idle-delay 0.01
	      company-minimum-prefix-length 3)

;;recentf启用保存最近打开文档，下次打开时可快速打开
					;加载模块

(recentf-mode 1)
					;设置保存文件的最大个数，默认为20
(setq-default recentf-max-saved-items 100)

;;swiper提升搜索性能
(ivy-mode 1)

;;smartparens 自动补全括号和引号等
;;(require 'smartparens-config)
(smartparens-global-mode t)

;;处在代码中时高亮两边的括号？？还没理解怎么用defadvice
'(defadvice show-paren-function (around fix-show-paren-function)
   "Highlight enclosing parens."
   (cond ((looking-at-p "\\s(") (funcall #'show-paren-function))
	 (t (save-excursion
	      (ignore-errors (backward-up-list))
	      (funcall #'show-paren-function)))))
'(ad-activate #'show-paren-function)

;;popwin；使光标跳转到帮助窗口
(popwin-mode 1)

;;;;;
(provide 'init-edit-help)
