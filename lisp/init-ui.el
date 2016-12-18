;;主题theme配置
(my-install-all-packages '(
			   zenburn-theme))

;;Solarized主题有点奇葩，一般加载主题使用的就是(load-theme 'xxx)，为啥会这样？
;;(require 'solarized)
;;(deftheme solarized-dark "The dark variant of the Solarized colour theme")
;;(create-solarized-theme 'dark 'solarized-dark)
;;(provide-theme 'solarized-dark)
;;使用zenburn主题主要是为了兼容非图像界面使用问题
(load-theme 'zenburn t)

;;;;;;
(provide 'init-ui)
