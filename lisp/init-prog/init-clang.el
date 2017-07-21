;;; init-clang.el --- C语言环境配置
;; Author:Claudio <3261958605@qq.com>
;; Created: 2017
;;; Commentary:
;;; Code:

;; 补全
(add-hook 'c-mode-hook
          (lambda()
            (set (make-local-variable 'company-backends)
                 '((company-clang :with company-yasnippet)
                   (company-capf :with company-yasnippet)
                   (claudio/company-files-without-prefix-backend :with company-yasnippet)
                   (company-dabbrev :with company-yasnippet)))))

(use-package ggtags
  :init
  (claudio/app-may-tobe-installed "global")
  ;; (claudio/app-may-tobe-installed "Pygments" :use-pip t)

  :config
  (add-hook 'c-mode-common-hook
            (lambda()
              (when (derived-mode-p 'c-mode 'c++-mode )
                (ggtags-mode)))
            )
  )

(provide 'init-clang)
;;; init-clang.el ends here
