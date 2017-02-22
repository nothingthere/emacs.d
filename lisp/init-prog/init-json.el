;;; init-json.el --- json模式初始化
;;; Commentary:
;;; Code:
(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  )

(provide 'init-json)
;;; init-json.el ends here
