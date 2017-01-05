;;; init-disabled.el --- 禁用功能激活配置
;;; Commentary:
;;; Code:

(cl-defmacro my/enable(&rest functions)
  `(dolist (function ',functions)
	 (put function 'disabled nil)))

(my/enable
 ;; 大小写转换
 upcase-region							;C-x C-u
 downcase-region						;C-x C-l
 ;; narrow
 narrow-to-region						;C-x C-n
 )

(provide 'init-disabled)
;;; init-disabled.el ends here