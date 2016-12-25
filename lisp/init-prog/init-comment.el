;;; init-comment.el --- 注释修饰配置
;;; Commentary:
;;; Code:

(defun my/comment-or-uncomment()
  "修改后的注释函数。
1. 如果有选中区域，注释/去注释该区域
2. 如果为空行，仅注释
3. 如果在代码行末，注释并缩进

缺点：不能通过快捷键去注释行末注释"
  (interactive)
  (cond ((my/current-line-empty-p) (comment-dwim nil)) ;如果在空行上。不能理解comment-dwim的参数该怎样设置
	((my/at-end-of-line-p) (comment-indent))	;如果在行末，前面有内容
	(t						;默认注释选中区域和本行
	 (let* ((region (my/get-region-or-get-the-line-as-region))
		(start (car region))
		(end (cdr region)))
	   (comment-or-uncomment-region start end)))))

(provide 'init-comment)
;;; init-comment.el ends here
