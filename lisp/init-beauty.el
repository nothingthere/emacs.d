;;; init-beauty.el --- 代码美化配置
;;; Commentary:
;;; Code:

(cl-defmacro my/beauty/delete-top/bottom-blanklines(&key bottom)
  "删除buffer顶部/底部所有空行。默认为删除顶部空行，如果:BOTTOM为no-nil为删除底部空行.

参考：https://www.emacswiki.org/emacs/DeletingWhitespace#toc4"
  `(my/with-save-everything+widen
	,(if bottom `(goto-char (point-max)) `(goto-char (point-min)))
	(delete-blank-lines)
	(let ((blank-lines (abs
						,(if bottom
							 `(skip-chars-backward "\n\t")
						   `(skip-chars-forward "\n\t")))))
	  (when (> blank-lines 0)
		(delete-char ,(if bottom
						  `blank-lines
						`(- blank-lines)))))))

(defun my/beauty/leave-1-empty-line()
  "将buffer中多个相邻的空行只留1个."
  (interactive)
  (my/with-save-everything+widen
   (goto-char (point-min))
   (let ((in-blank-lines-p (my/current-line-empty-p)))
	 (while(not (eobp))
	   (forward-line)
	   (cond ((my/current-line-empty-p)
			  (if in-blank-lines-p
				  (delete-blank-lines)
				(setq in-blank-lines-p t)))
			 (t (setq in-blank-lines-p nil)))))))

(defun my/beauty/delete-extra-spaces()
  "删除buffer多余空行，且同时删除行末多余空格。"
  (my/with-save-everything+widen
   ;; 删除buffer末空行
   ;; (my/beauty/delete-top/bottom-blanklines :bottom t)
   ;; 将变量delete-trailing-lines设置为non-nil后，
   ;; 调用delete-trailing-whitespace命令就可删除buffer末所有空行。
   ;; 删除所有行末空白
   (setq delete-trailing-lines t)

   (delete-trailing-whitespace)

   ;;删除顶部多余空行
   (my/beauty/delete-top/bottom-blanklines)

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
