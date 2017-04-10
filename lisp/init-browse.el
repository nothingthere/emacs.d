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
(defvar *claudio/browse-search-engin-need-lantern* '("google" "youtube" "emacs")
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
    (if (find query-url *claudio/browse-search-engin-need-lantern*
              :test (lambda(url engin)
                      "如果需翻墙搜索，需使用chromium，以便翻墙"
                      (string-match-p (downcase engin)
                                      (downcase url))))
        (browse-url-chromium query-string)
      ;; 否则使用firefox
	  (browse-url-firefox query-string))
	;; 输出提示消息
	(message "搜索：%s" query-string)))

(defmacro claudio/browse-install-search-engin(search-engin-name search-engin-url search-engin-prompt)
  "定义一些不同浏览器的搜索函数."
  `(defun ,(intern (format "@%s" search-engin-name))
       ()
     ,(format "通过%s搜索" search-engin-name)
     (interactive)
     ;; 如果需使用翻墙，要配合lantern一起使用
     ,(if (find search-engin-name *claudio/browse-search-engin-need-lantern*
                :test (lambda(name need-lantern)
                        (string= (downcase name)
                                 (downcase need-lantern))))

          ;; 如果没启用lantern，先启用
          `(unless (claudio/util-app-running-p "lantern")
             (shell-command (format "%s&"
                                    (or (executable-find "lantern") "lantern")))))
     ;; 执行搜索
     (claudio/search ,search-engin-url ,search-engin-prompt)))

(claudio/browse-install-search-engin "bing" "http://cn.bing.com/search?q=" "必应: ")
(claudio/browse-install-search-engin "github" "https://github.com/search?q=" "GitHub 搜索: ")
(claudio/browse-install-search-engin "google" "http://www.google.com/search?q=" "谷歌: ")
(claudio/browse-install-search-engin "emacs" "http://www.google.com/search?q=emacs+" "emacs: ")
(claudio/browse-install-search-engin "melpa" "http://melpa.org/#/" "Melpa：")
(claudio/browse-install-search-engin "repo" "https://github.com/nothingthere/" "我的项目：")
(claudio/browse-install-search-engin "douban" "https://www.douban.com/search?q=" "豆瓣搜索：")
(claudio/browse-install-search-engin "youtube" "https://www.youtube.com/results?search_query=" "Youtube搜索：")
(claudio/browse-install-search-engin "bilibili" "http://search.bilibili.com/all?keyword=" "bilibili：")
(claudio/browse-install-search-engin "youdao" "http://dict.youdao.com/w/" "有道翻译：")
(claudio/browse-install-search-engin "wiki" "https://en.wikipedia.org/wiki/" "Wiki：")
(claudio/browse-install-search-engin "music" "http://music.163.com/#/search/m/?s=" "163音乐：")

(defun @()
    "直接输入网址，用火狐浏览器打开."
  (interactive)
  (let ((url (read-string "网址： ")))
    (browse-url-firefox url)))

(provide 'init-browse)
;;; init-browse.el ends here
