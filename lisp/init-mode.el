;;;major-mode设置
(setq auto-mode-alist
      (append
       '(
	 ("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode)
	 )
       auto-mode-alist))

;;;
(provide 'init-mode)
