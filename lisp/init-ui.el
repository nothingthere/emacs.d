;;; init-ui.el --- 主题theme配置
;;; Commentary:
;;; Code:

										; 修改默认UI配置 ;;;;;;;;;;;;;;;;;;;;;;;;

;;关闭菜单、工具和滚动条
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
;;(global-linum-mode t)

;;改变光标样式，显示为单竖线。
;; 只在图像界面中有作用，在命令行中时，默认与命令行的光标保持一致。
(setq-default cursor-type 'bar)

;;开启时全屏显示
(setq
 initial-frame-alist '((fullscreen . maximized)))

;;高亮当前行
(global-hl-line-mode t)

;;高亮匹配括号
(show-paren-mode)

;; tab的宽度
(setq-default tab-width 4)

										; 主题应用 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Solarized主题有点奇葩，一般加载主题使用的就是(load-theme 'xxx)，为啥会这样？
;;(require 'solarized)
;;(deftheme solarized-dark "The dark variant of the Solarized colour theme")
;;(create-solarized-theme 'dark 'solarized-dark)
;;(provide-theme 'solarized-dark)
;;使用zenburn主题主要是为了兼容非图像界面使用问题
;; zenburn-them -- zenbutn主题
(use-package zenburn-theme
  :demand t				;马上需要
  :config
  (load-theme 'zenburn t)
  ;; 设置当前行的颜色。使用zenburn主题后，命令行环境下高亮当前行的解决办法。
  ;; 参考地址：http://stackoverflow.com/questions/2718189/emacshighlight-the-current-line-by-underline-it
  (set-face-attribute hl-line-face nil  :background "#000")
  )

										; 改变样式 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute font-lock-function-name-face nil :bold t :italic t)

(provide 'init-ui)
;;; init-ui.el ends here