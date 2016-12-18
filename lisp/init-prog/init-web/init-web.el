;;;web编程语言相关配置
(my-require
 '(
   init-js
   ))

;;web-mode配置
(my-install-all-packages
 '(
   web-mode
   ))

;;web-mode设置
;;设置默认缩进
(add-hook 'web-mode-hook
	  (lambda ()
	    (setq web-mode-markup-indent-offset 2
		  web-mode-css-indent-offset 2
		  web-mode-code-indent-offset 2)))

;;;
(provide 'init-web)
