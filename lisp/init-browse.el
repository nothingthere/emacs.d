;;; init-browse.el --- 浏览器搜索配置
;; Author:Claudio <3261958605@qq.com>
;; Created: 2017
;;; Commentary:
;; 参考：https://github.com/emacs-china/Spacemacs-rocks#第九天macro-与-use-package
;; 估计@zilongshanren也是参考的prelude配置
;;; Code:

;; 更改eww的默认搜索引擎，即使用eww命令的默认浏览器
(setq eww-search-prefix "http://cn.bing.com/search?q=")

;; 定制自己的搜索方式
(defvar *claudio/browse-search-engine-need-lantern* '("google" "youtube" "emacs" "linux" "python")
  "需要使用lantern翻墙的网站.")

;; 在kali上，lantern还需配合chromium一起使用，且需将其设置为默认浏览器
(claudio/app-may-tobe-installed "lantern" :manual t)
(claudio/app-may-tobe-installed "chromium")

(defun claudio/search(query-url prompt)
  "通过PROMPT提示输入搜索内容，通过QUERY-URL指定的地址在浏览器中打开。"
  (let((query-string (concat query-url (url-hexify-string
                                        (if mark-active
                                            (buffer-substring
                                             (region-beginning)
                                             (region-end))
                                          (read-string prompt))))))
    (if (find query-url *claudio/browse-search-engine-need-lantern*
              :test (lambda(url engine)
                      "如果需翻墙搜索，需使用chromium，以便翻墙"
                      (string-match-p (downcase engine)
                                      (downcase url))))
        (browse-url-chromium query-string)
      ;; 否则使用firefox
	  (browse-url-firefox query-string))
	;; 输出提示消息
	(message "搜索：%s" query-string)))

(defmacro claudio/browse-install-search-engine(search-engine-name search-engine-url search-engine-prompt)
  "定义一些不同浏览器的搜索函数.
SEARCH-ENGINE-NAME为生成函数的函数名，SEARCH-ENGINE-URL为搜索路径，SEARCH-ENGINE-PROMPT为执行命令时的提示信息."
  `(defun ,(intern (format "@%s" search-engine-name))
       ()
     ,(format "通过%s搜索" search-engine-name)
     (interactive)
     ;; 如果需使用翻墙，要配合lantern一起使用
     ,(if (find search-engine-name *claudio/browse-search-engine-need-lantern*
                :test (lambda(name need-lantern)
                        (string= (downcase name)
                                 (downcase need-lantern))))

          ;; 如果没启用lantern，先启用
          `(unless (claudio/util-app-running-p "lantern")
             (shell-command (format "%s&"
                                    (or (executable-find "lantern") "lantern")))))
     ;; 执行搜索
     (claudio/search ,search-engine-url ,search-engine-prompt)))

(claudio/browse-install-search-engine "bing" "http://cn.bing.com/search?q=" "必应: ")
(claudio/browse-install-search-engine "github" "https://github.com/search?q=" "GitHub 搜索: ")
(claudio/browse-install-search-engine "google" "http://www.google.com/search?q=" "谷歌: ")
(claudio/browse-install-search-engine "melpa" "http://melpa.org/#/" "Melpa：")
(claudio/browse-install-search-engine "repo" "https://github.com/nothingthere/" "我的项目：")
(claudio/browse-install-search-engine "douban" "https://www.douban.com/search?q=" "豆瓣搜索：")
(claudio/browse-install-search-engine "youtube" "https://www.youtube.com/results?search_query=" "Youtube搜索：")
(claudio/browse-install-search-engine "bilibili" "http://search.bilibili.com/all?keyword=" "bilibili：")
(claudio/browse-install-search-engine "wiki" "https://en.wikipedia.org/wiki/" "Wiki：")
(claudio/browse-install-search-engine "zhihu" "https://www.zhihu.com/search?type=content&q=" "知乎：")
(claudio/browse-install-search-engine "music" "http://music.163.com/#/search/m/?s=" "163音乐：")
(claudio/browse-install-search-engine "ximalaya" "http://www.ximalaya.com/search/" "喜马拉雅电台: ")
(claudio/browse-install-search-engine "emacs" "http://www.google.com/search?q=emacs+" "emacs: ")
(claudio/browse-install-search-engine "linux" "http://www.google.com/search?q=linux+" "linux: ")
(claudio/browse-install-search-engine "python" "http://www.google.com/search?q=python+" "python: ")

(defun @()
    "直接输入网址，用火狐浏览器打开."
  (interactive)
  (let ((url (read-string "网址： ")))
    (browse-url-firefox url)))

(provide 'init-browse)
;;; init-browse.el ends here
