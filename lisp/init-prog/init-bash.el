;;; init-bash.el --- bash脚本配置文件
;; Author:Claudio <3261958605@qq.com>
;; Created: 2017
;;; Commentary:
;;; Code:

(use-package company-shell
  :config
  (add-hook 'sh-mode-hook
            (lambda()
              (set (make-local-variable 'company-backends)
                   '((company-shell
                      :with company-yasnippet)

                     (claudio/company-files-without-prefix-backend
                      :with company-yasnippet)

                     (company-files
                      :with company-yasnippet))))))

(provide 'init-bash)
;;; init-bash.el ends here
