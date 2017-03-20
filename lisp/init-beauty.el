;;; init-beauty.el --- 代码美化配置
;;; Commentary:
;;; Code:

(defun my/beauty/delete-top-blanklines ()
  "删除顶部所有空格."
  (interactive )
  (my/with-save-everything+widen
   (goto-char (point-min))
   (when (my/current-line-empty-p)
	 (delete-blank-lines)
     (when (my/current-line-empty-p)
       (delete-blank-lines)))))

(defun my/beauty/delete-bottom-blanklines()
  "删除文本末的空行."
  (interactive)
  (my/with-save-everything+widen
   (goto-char (point-max))
   (beginning-of-line)
   (when (my/current-line-empty-p)
     (delete-blank-lines)

     ;; 如果完全无文本，就不进行任何操作
     ;; 当前位置不是buffer最前面
     (unless (bobp)
       (delete-backward-char 1)))))

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

(defconst *MY/BEAUTY-INDENT-BLACKLIST*
  ;; python-mode:使用autopep8自动缩进，使用Emacs修饰可能会产生混淆
  '(makefile-gmake-mode snippet-mode python-mode)
  "在黑名单中的模式美化时不缩进.")

(defun my/beauty/indent-buffer()
  "调整整个buffer的缩进."
  (interactive)
  (unless (find major-mode *MY/BEAUTY-INDENT-BLACKLIST*)
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
