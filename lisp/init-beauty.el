;;; init-beauty.el --- 代码美化配置
;;; Commentary:
;;; Code:
(defun beauty-delete-extra-whitespaces()
  "删除buffer多余空行，且同时删除行末多余空格。"
  (interactive)
  (save-excursion
    ;;删除顶部多余空行
    (progn
      (goto-char (point-min))
      (when (my/current-line-empty-p)
	(delete-blank-lines)
	;;如果还有空行，再删除
	(when (my/current-line-empty-p)
	  (delete-blank-lines))))

    ;;删除中间的多余空行
    (let ((in-blank-lines nil))
      (while (not (eobp))
	(forward-line)
	(cond ((my/current-line-empty-p)
	       (if in-blank-lines;;如果已近有空行只保留一个
		   (delete-blank-lines)
		 (setq in-blank-lines t)));;如果只有当前一个空行，先标记
	      (t (setq in-blank-lines nil);;不是空行则标记为nil
		 (my/line-trim-end)))))));;删除末尾多余空白字符

(defun beautify()
  "美化buffer并保存"
  (interactive)
  ;;获取美化区域
  (save-excursion
    (let ((region (my/get-region)))
      ;;删除多余空行
      (beauty-delete-extra-whitespaces)
      ;;对齐
      (indent-region (car region) (cdr region)))))

;; 希望自在prog-mode下才生效，所以使用defadvice整体改变不佳
;; (defadvice save-buffer(before beautify-buffer-before-save activate)
;;    "保存buffer前先美化。"
;;    (beautify)
;;    )

(my/set-keys
 ("C-x C-s" (lambda()
	      (interactive)
	      (beautify)
	      (save-buffer))
  "保存文件前美化"
  prog-mode-map org-mode-map)
 )

(provide 'init-beauty)
;;; init-beauty.el ends here
