;;;html文件编辑配置
;;web-mode配置
(my-install-all-packages
 '(
   web-mode
   emmet-mode
   ))

(my-require
 '(
   web-mode
   emmet-mode
   ))
;;web-mode设置
;;使用说明页面：http://web-mode.org/
;;编辑纯html文件
(add-to-list 'auto-mode-alist
	     '("\\.html?\\'" . web-mode))
;;;功能设置
(setq
 web-mode-enable-css-colorization t ;;高亮内嵌css颜色
 web-mode-enable-auto-pairing t ;;自动补全匹配
 web-mode-enable-auto-closing t;;自动补全标签
 web-mode-enable-current-element-highlight t;;自动高亮匹配标签
 ;; web-mode-enable-current-column-highlight t
 )
;;设置默认缩进
(add-hook 'web-mode-hook
	  (lambda ()
	    (setq web-mode-markup-indent-offset 2
		  web-mode-css-indent-offset 2
		  web-mode-code-indent-offset 2)))

;;;emmet
;;(setq emmet-move-cursor-between-quotes t)
(add-hook 'web-mode-hook 'emmet-mode);使用web-mode时自动加载
(add-hook 'css-mode-hook  'emmet-mode);;编写css时使用缩写
;;默认快捷键
;;C-j emmet-expand-line
;;C-M-left emmet-prev-edit-point
;;C-M-right emmet-next-edit-point

;;;
(provide 'init-html)
