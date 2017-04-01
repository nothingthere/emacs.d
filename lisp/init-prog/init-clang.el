;;; init-clang.el --- C语言环境配置
;; Author:Claudio <m15982038632@gmial.com>
;; Created: 2017
;;; Commentary:
;;; Code:

(add-hook 'c-mode-hook
          (lambda()
            (claudio/company-push-local-backend
             '(company-clang  :with company-yasnippet))))
(provide 'init-clang)
;;; init-clang.el ends here
