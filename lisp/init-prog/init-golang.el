;;; init-golang.el --- go语言开发环境
;;; Commentary:
;;; Code:
(use-package go-mode
  :config
  ;; (remove-hook 'before-save-hook 'beautify-before-save)
  (add-hook 'go-mode-hook 'gofmt-before-save)
  )

(provide 'init-golang)
;;; init-golang.el ends here