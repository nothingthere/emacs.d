;;; init-html.el --- html文件编辑配置
;;; Commentary:
;;; Code:

;;web-mode -- html文件中高亮js和CSS代码，高亮CSS颜色等。
;;web-mode设置
;;使用说明页面：http://web-mode.org/
;;html文件自动使用web-mode
(use-package
  web-mode
  :config
  ;;编辑纯html文件
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; 功能设置
  (setq
   ;; 高亮内嵌css颜色
   web-mode-enable-css-colorization t
   ;; 自动补全匹配
   web-mode-enable-auto-pairing t
   ;; 自动补全标签
   web-mode-enable-auto-closing t
   ;; 自动高亮匹配标签
   web-mode-enable-current-element-highlight t
   ;; web-mode-enable-current-column-highlight t
   )
  ;;设置默认缩进
  (add-hook 'web-mode-hook
            (lambda ()
              (setq
               ;; html缩进
               web-mode-markup-indent-offset 2
               ;; css缩进
               web-mode-css-indent-offset 2
               web-mode-code-indent-offset 2))))

;;emmet -- 哈哈
;;默认快捷键
;;C-j emmet-expand-line
;;C-M-left emmet-prev-edit-point
;;C-M-right emmet-next-edit-point
(use-package
  emmet-mode
  :config
  ;;(setq emmet-move-cursor-between-quotes t)
  (add-hook 'web-mode-hook 'emmet-mode)  ;使用web-mode时自动加载
  (add-hook 'css-mode-hook  'emmet-mode) ;;编写css时使用缩写
  :bind ("C-j" . emmet-expand-line))

(provide 'init-html)
;;; init-html.el ends here
