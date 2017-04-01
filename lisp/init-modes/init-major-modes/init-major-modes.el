;;; init-major-modes.el --- 所有自定义major modes的入口文件
;; Author:Claudio <m15982038632@gmial.com>
;; Created: 2017
;;; Commentary:
;;; Code:
(message "加载自定义major modes")

(cl-defun claudio/major-mode-font-face
    (font-face keywords &optional (type 'words))
  "通过关键字KEYWORDS生成可赋值font-face-defaults的assoc-list.
TYPE参数用于regexp-opt函数的第二个参数"
  (cons (regexp-opt keywords type) font-face))

(claudio/require-init-files
 claudio-wpdl-mode
 )

(provide 'init-major-modes)
;;; init-major-modes.el ends here
