;;; init-beauty.el --- 代码美化配置
;;; Commentary:
;;; Code:

(defun my/beauty/delete-top-blanklines ()
  "删除顶部所有空格."
  (interactive )
  (my/with-save-everything+widen
   (goto-char (point-min))
   (when (my/current-line-empty-p)
	 (kill-line)
	 (my/beauty/delete-top-blanklines)
	 )))

(defun my/beauty/leave-1-empty-line()
  "将buffer中多个相邻的空行只留1个."
  (interactive)
  (my/with-save-everything+widen
   (goto-char (point-min))
   (let ((previous-line-empty-p (my/current-line-empty-p)))					;当前行是否为空
	 (while (not (eobp))
	   (forward-line)
	   (cond ((my/current-line-empty-p)	;如果当前行为空
			  (if previous-line-empty-p	;且上一行也为空
				  (delete-blank-lines)	;则保留一个空行
				(setq previous-line-empty-p t))) ;否则标记上一行为空
			 (t (setq previous-line-empty-p nil)))))))

(defun my/beauty/delete-extra-spaces()
  "删除buffer多余空行，且同时删除行末多余空格。"
  (my/with-save-everything+widen
   ;; 删除行末所有空格
   (setq delete-trailing-lines t)
   (delete-trailing-whitespace)
   ;;删除顶部所有空格
   (my/beauty/delete-top-blanklines)
   ;;删除中间的多余空行
   (my/beauty/leave-1-empty-line)
   ))

(defvar *my/beauty-indent-blacklist*
  '(makefile-gmake-mode snippet-mode)
  "在黑名单中的模式美化时缩进.")

(defun my/beauty/indent-buffer()
  "调整整个buffer的缩进."
  (interactive)
  (unless (find major-mode *my/beauty-indent-blacklist*)
	(save-restriction
	  (widen)
	  (indent-region (point-min) (point-max)))))

(defun my/beautify()
  "美化buffer并保存"
  (interactive)
  (my/beauty/indent-buffer)
  (my/beauty/delete-extra-spaces)
  )

(add-hook 'before-save-hook 'my/beautify)

(provide 'init-beauty)
;;; init-beauty.el ends here
