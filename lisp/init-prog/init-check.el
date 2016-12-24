;; init-check.el --- 编程语言语法检测
;;; Commentary:
;;; Code:
;; flycheck -- 语法检查
(my/use-package
 (:pkg flycheck :require-p nil)
 (global-flycheck-mode 1)
 )

(provide 'init-check)
;;; init-check.el ends here
