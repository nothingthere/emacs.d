;;; init-bash.el --- bash脚本配置文件
;;; Commentary:
;;; Code:

(use-package company-shell
  :config
  (add-hook 'sh-mode-hook
            (lambda()
              (claudio/company-push-local-backend
               '(company-shell company-files :with company-yasnippet))))

  )

(provide 'init-bash)
;;; init-bash.el ends here
