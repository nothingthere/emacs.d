;;; init-prog.el -- 编程语言综合配置
;;; Commentary:
;;; Code:

(my/require-init-files
 init-elisp
 init-html
 init-js
 init-markdown
 init-org
 init-check
 )

(bind-key "C-x C-s" 'beautify-before-save prog-mode-map)

(provide 'init-prog)
;;; init-prog.el ends here
