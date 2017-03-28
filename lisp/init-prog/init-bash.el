;;; init-bash.el --- bash脚本配置文件
;;; Commentary:
;;; Code:

(use-package company-shell
  :config
  (my/company-add-backend 'sh-mode-hook
                          (company-shell :with company-yasnippet)
                          )
  )

(provide 'init-bash)
;;; init-bash.el ends here
