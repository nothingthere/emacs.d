;;; init-org.el --- org配置
;;; Commentary:
;;; Code:
;;org-mode -- YES
(use-package org
  :demand t								;如果不使用demand，只有打开过.org文件后才能使用快捷键
  :config
  ;; 代码块语法高亮
  (setq org-src-fontify-natively t)

  ;; 设置org agenda的默认文件夹
  (setq
   org-directory "~/.emacs.d/org/" ;;org的默认文件夹
   *my/org-agenda-directory* (concat org-directory "agenda/")
   org-agenda-files (list *my/org-agenda-directory*));;org agenda 的文件夹

  ;; 打开自动换行
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; 拼写检查
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; 执行源码
  (setq org-babel-python-command "/usr/bin/python3.5")
  ;; (add-to-list 'org-src-lang-modes '("python" . python))
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))

  ;; 为smartparen新增标记配对
  (my/with-pkg-enabled
   smartparens
   (dolist (match '(("“" . "”")
					("《" . "》")
					("（" . "）")
					("'" . "'")
					;; ("<" . ">")
					;; ("*" . "*")
					;; ("/" . "/")
					))
	 (sp-local-pair 'org-mode (car match) (cdr match))))

  ;; 设置capture
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-capture-templates
		'(("j" "生活安排(Journal)" entry (file+headline (concat *my/org-agenda-directory* "生活安排.org") "生活安排")
		   "* TODO %? %T")
		  ("d" "每日安排(Daily)" entry (file+headline (concat *my/org-agenda-directory* "每日安排.org") "每日安排")
		   "* TODO %? %T")
		  ("s" "学习安排(Study)" entry (file+headline (concat *my/org-agenda-directory* "学习安排.org") "学习安排")
		   "* TODO %? %t")))
  ;; 自动补全
  (add-hook 'org-mode-hook 'yas-minor-mode)

  ;; 使用:bind关键字时，不能使用匿名函数作为执行函数
  (bind-keys
   ("C-c t" . (lambda()
				"打开所有未完成任务."
				(interactive)
				(org-agenda nil "t")))
   ("C-c C-d" .  (lambda()
				   "新建日常任务."
				   (interactive)
				   (org-capture nil "d")))
   )

  ;; 源码编辑保存时美化
  (progn
	(defun my/py-src-beauty-before-save()
	  "保存前使用my/beautify美化，如果major-mode=python-mode，则是py-autopep8美化."
	  (my/beautify)
	  (when (equal major-mode 'python-mode)
		(py-autopep8-buffer)))

	(advice-add 'org-edit-src-save :before #'my/py-src-beauty-before-save)
	)

  ;; org-bullets -- show head with bullets
  (use-package org-bullets
	;; :demand t
	:disabled t
	:config
	(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))
	)

  :bind
  (:map org-mode-map
		;; ("C-c =" . nil)
		("C-c =" . er/expand-region)
		;; ("C-c ;" . nil)
		("C-c ;" . mc/mark-all-dwim))
  )

;; org-pomodoro -- 番茄工作坊
(use-package org-pomodoro
  :disabled t
  :config
  (setq org-pomodoro-format "余时间~%s") ;显示格式
  (message "org-pomodoro on")
  )

;; 自定义函数
(cl-defun my/org-increase-headline(&optional (level 1))
  "将标题增加/减小一级.
遍历每行，使用正则检查改行是否为标题。如果是则改变标题等级
"
  (interactive)
  (cond ((not (equal major-mode 'org-mode))
		 (message "此函数只能用于org-mode下"))
		(t
		 (let ((icon ?*))				;表示标题的符号
		   (my/with-save-everything+widen
			(goto-char (point-min))
			(while (not (eobp))			;逐行遍历
			  (beginning-of-line)

			  (when(looking-at-p "^\*+[[:space:]]+?+")
				;; (message (format "第%d行" (line-number-at-pos))))
				;; 开始替换
				(when (= level 1)		;如果是提升一个等级
				  (delete-char 1)
				  ;; 最多变为一级标题
				  (when (equal (char-after) ?\s)
					(insert-char icon)))

				(when (= level -1)		;如果是减小一个等级
				  (insert-char icon)))

			  (forward-line)
			  ))))))

(provide 'init-org)
;;; init-org.el ends here
