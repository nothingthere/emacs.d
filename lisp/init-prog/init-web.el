;;;web编程语言相关配置的通用配置
;;web-mode配置
(my-install-all-packages
 '(
   web-mode
   emmet-mode
   ))

;;web-mode设置
;;设置默认缩进
(add-hook 'web-mode-hook
	  (lambda ()
	    (setq web-mode-markup-indent-offset 2
		  web-mode-css-indent-offset 2
		  web-mode-code-indent-offset 2)))

;;;emmet
;;;
(provide 'init-web)
