;;;;快捷键绑定
;;swiper快捷键
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-h C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-x v") 'counsel-describe-variable)
;;(global-set-key (kbd "<f1> l") 'counsel-find-library)
;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
;;(global-set-key (kbd "C-c j") 'counsel-git-grep)
;;(global-set-key (kbd "C-c k") 'counsel-ag)
;;(global-set-key (kbd "C-x l") 'counsel-locate)
;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;;打开recentf中的缓存文件
;;(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;打开日程
(global-set-key (kbd "C-c a") 'org-agenda)

;;随时打开~/.agenda目录<F2>
(global-set-key
 (kbd "<f2>")
 (lambda()
   (interactive)
   (dired "~/.agenda")))

;;;保存文档时同时美化文档
(global-set-key
 (kbd "C-x C-s")
 (lambda()
   (interactive)
   (beautify)
   (save-buffer)))

;;;通过C-c tab键，打开hippie-expand
(global-set-key (kbd "C-c TAB") 'hippie-expand)

;;提高occur-mode性能，默认备选为选中/光标所在处单词
(global-set-key
 (kbd "M-s o")
 (lambda()()
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
(global-set-key (kbd "M-s i") 'counsel-imenu)
;;expand-region：文本选择，继续："=" 返回："-"
(global-set-key (kbd "C-c =") 'er/expand-region)

;;iedit-mode：同时编辑相同字符串
;;默认快捷键为C-;，不过与搜狗输入法有冲突
(global-set-key (kbd "C-c ;") 'iedit-mode)

;;修改company选中快捷键
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-n") #'company-select-next))
;;;;;; 
(provide 'init-keybindings) 
