;;; init-bash.el --- bash脚本配置文件
;; Author:Claudio <m15982038632@gmial.com>
;; Created: 2017
;;; Commentary:
;;; Code:

(use-package company-shell
  :config
  (add-hook 'sh-mode-hook
            (lambda()
              (claudio/company-push-local-backend
               '(company-shell company-files :with company-yasnippet)))))

(provide 'init-bash)
;;; init-bash.el ends here
