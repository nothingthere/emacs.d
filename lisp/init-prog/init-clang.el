;;; init-clang.el --- C语言环境配置
;;; Commentary:
;;; Code:

(my/company-add-backend 'c-mode-hook
                        company-clang
                        :delete t)

(my/company-add-backend 'c-mode-hook
                        (company-clang :with company-yasnippet)
                        )

(provide 'init-clang)
;;; init-clang.el ends here
