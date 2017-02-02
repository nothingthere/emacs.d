;;; init-broswe.el --- 浏览器搜索配置
;;; Commentary:
;; 参考：https://github.com/emacs-china/Spacemacs-rocks#第九天macro-与-use-package
;; 估计@zilongshanren也是你参考的prelude配置
;;; Code:

;; 更改eww的默认搜索引擎
(setq eww-search-prefix "http://cn.bing.com/search?q=")

;; 定制自己的搜索方式
(defvar *my/search-engin-need-lantern* '("google" "youtube")
  "需要使用lantern翻墙的网站.")

(defun my/search(query-url prompt)
  "通过PROMT提示输入搜索内容，通过QUERY-URL指定的地址在浏览器中打开。"
  (let((query-string (concat query-url
							 (url-hexify-string
							  (if mark-active
								  (buffer-substring (region-beginning) (region-end))
								(read-string prompt)))))

	   )
	(if (find query-url *my/search-engin-need-lantern*
			  :test (lambda(url engin)	;如果需翻墙搜索，需使用chromium，以便翻墙
					  (string-match-p (downcase engin) (downcase url))))
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
	 ;; 如果需使用翻墙，要配合lantern一起使用
	 ,(when (find search-engin-name *my/search-engin-need-lantern*
				  :test (lambda(name need-lantern)
						  (string= (downcase name) (downcase need-lantern))))
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
;; (find "a" '("a" "b") :test 'string=)
(my/install-search-engin "bing" "http://cn.bing.com/search?q=" "必应: ")
(my/install-search-engin "github" "https://github.com/search?q=" "GitHub 搜索: ")
(my/install-search-engin "google" "http://www.google.com/search?q=" "谷歌: ")
(my/install-search-engin "melpa" "http://melpa.org/#/" "Melpa：")
(my/install-search-engin "repo" "https://github.com/nothingthere/" "我的项目：")
(my/install-search-engin "douban" "https://www.douban.com/search?q=" "豆瓣搜索：")
(my/install-search-engin "youtube" "https://www.youtube.com/results?search_query=" "Youtube搜索：")
(my/install-search-engin "bilibili" "http://search.bilibili.com/all?keyword=" "bilibili：")
(my/install-search-engin "youdao" "http://dict.youdao.com/w/" "有道翻译：")
(my/install-search-engin "wiki" "https://en.wikipedia.org/wiki/" "Wiki：")

(provide 'init-broswe)
;;; init-broswe.el ends here
