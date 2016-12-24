;;; init-keybingdings.el --- 快捷键绑定
;;; Commentary:
;;; Code:

;; mwe-log-commands -- 记录快捷键
(my/use-package
 (:pkg mwe-log-commands :require-p nil))

;; 快捷键设置
(my/set-keys
 (
  ;;elisp
  ("C-c e" macrostep-expand emacs-lisp-mode-map lisp-interaction-mode-map);;宏展开

  ;;wiper ivy counsel三件套
  ("C-s" swiper);;搜索命令
  ;;("C-r" counsel-expression-history read-expression-map);;???与my/set-keys定义有冲突
  ("C-c C-r" ivy-resume) ;;查找最近打开文件
  ("M-x" counsel-M-x);;执行命令
  ("C-x C-f" counsel-find-file);;寻找当前文件夹下的文件
  ("C-h f" counsel-describe-function);;获取函数文档
  ("C-h v" counsel-describe-variable);;获取变量文档
  ("C-c g" counsel-git);;寻找处于git项目中的文件
  ;;("<f1> l) 'counsel-find-library)
  ;;("<f2> i) 'counsel-info-lookup-symbol)
  ;;("<f2> u) 'counsel-unicode-char)
  ;;("C-c j" 'counsel-git-grep)
  ;;("C-c k" 'counsel-ag)
  ;;("C-x l" 'counsel-locate)
  ;;("C-S-o" 'counsel-rhythmbox)
  
  ;;;通过C-c tab键，打开hippie-expand
  ("C-c TAB" hippie-expand)

  ;;提高occur-mode性能，默认备选为选中/光标所在处单词
  ("M-s o" (lambda()()
	     (interactive)
	     (push (if(region-active-p)
		       (buffer-substring-no-properties (region-beginning) (region-end))
		     (let((sym (thing-at-point 'symbol)))
		       (if (stringp sym)
			   (regexp-quote sym)
			 sym)))
		   regexp-history)
	     (call-interactively 'occur)))

  ;;选中所有函数定义
  ("M-s i" counsel-imenu)

  ;;expand-region：文本选择，继续："=" 返回："-"
  ("C-c =" er/expand-region)

  ;;iedit-mode：同时编辑相同字符串
  ;;默认快捷键为C-;，不过与搜狗输入法有冲突
  ;;("C-c ;" iedit-mode)

  ;;multi-cursor-mode替换iedit-mode
  ("C-c ;" mc/mark-all-dwim);;标记所有
  ("C-c d" mc/mark-next-like-this);;标记下一个
  ("C-c D" mc/skip-to-next-like-this)
  ;;org
  ("C-c T" (lambda()
	     (interactive)
	     (org-agenda nil "t")))

  ("C-c a" (lambda()
	     (interactive)
	     (org-agenda nil "a")))
  
  ;;org 配置
  ;;打开日程

  ;;capture
  ;;添加学习安排
  ("C-c C-s"(lambda()
	      (interactive)
	      (org-capture nil "s")))

  ;;添加日常安排
  ("C-c C-d" (lambda()
	       (interactive)
	       (org-capture nil "d")))

  ;;添加日程安排
  ("C-c C-j" (lambda()
	       (interactive)
	       (org-capture nil "j")))

  ;;helm-ag项目查找
  ("C-x f" helm-ag-project-root)

  ;;在不同窗口之间切换
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("<left>" windmove-left)
  ("<right>" windmove-right)

  ;;mwe观察快捷键
  ("C-c ." (lambda()
	     (interactive)
	     (save-excursion
	       (let ((old-buffer (current-buffer)))
		 (mwe:log-keyboard-commands)
		 (split-window-right)
		 (mwe:open-command-log-buffer)
		 (current-buffer old-buffer)))))
  ;;注释行
  ("M-;" my/comment)

  ;; 保存前美化
  ("C-x C-s" (lambda()
	       (interactive)
	       (beautify)
	       (save-buffer))
   prog-mode-map org-mode-map)			;只有在prog-mode和org-mode下才保存前美化

  ;;;;;;KEYS ENDS HERE
  ))

;;yasnippet
;; ("C-c y" yas-expand prog-mode-
;; (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)
;;修改company选中快捷键
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next))

(provide 'init-keybindings)
;;; init-keybindings ends here
