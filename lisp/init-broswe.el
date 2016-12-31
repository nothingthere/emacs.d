;;; init-broswe.el --- 浏览器搜索配置
;;; Commentary:
;; 参考：https://github.com/emacs-china/Spacemacs-rocks#第九天macro-与-use-package
;; 估计@zilongshanren也是你参考的prelude配置
;;; Code:
(defun my/search(query-url prompt)
  "通过PROMT提示输入搜索内容，通过QUERY-URL指定的地址在浏览器中打开。"
  (let((query-string (concat query-url
							 (url-hexify-string
							  (if mark-active
								  (buffer-substring (region-beginning) (region-end))
								(read-string prompt)))))
	   )
	(if (string-match-p "google" query-url) ;如果google搜索，需使用chromium，以便翻墙
		(browse-url-chromium query-string) ;否则使用firefox
	  (browse-url-firefox query-string))
	;; 输出提示消息
	(message "搜索：%s" query-string)))

(defmacro my/install-search-engin(search-engin-name search-engin-url search-engin-prompt)
  "定义一些不同浏览器的搜索函数.
如果要使用"
  `(defun ,(intern (format "@%s" search-engin-name))()
	 ,(format "通过%s搜索" search-engin-name)
	 (interactive)
	 ;; 如果需使用google，要配合lantern一起使用
	 ,(when (string= (downcase search-engin-name) "google")
		`(progn
		   (my/with-system-enabled ("lantern")
								   ;; 在kali上，还需配合chromium一起使用，且需将其设置为默认浏览器
								   (my/with-system-enabled ("chromium")
														   ;; 如果没启用lantern，先启用
														   (when (my/shell-result-empty-p "pgrep -x lantern")
															 (shell-command "lantern&")))))
		)
	 ;; 执行搜索
	 (my/search ,search-engin-url ,search-engin-prompt)))

(my/install-search-engin "bing" "http://cn.bing.com/search?q=" "必应: ")
(my/install-search-engin "github" "https://github.com/search?q=" "GitHub 搜索: ")
(my/install-search-engin "google" "http://www.google.com/search?q=" "谷歌: ")
(my/install-search-engin "melpa" "http://melpa.org/#/" "Melpa：")
(my/install-search-engin "repo" "https://github.com/nothingthere/" "我的项目： ")

(provide 'init-broswe)
;;; init-broswe.el ends here