;;; init-makefile.el --- makefile-gmake模式配置
;; Author: Claudio <3261958605@qq.com.com>
;; Created: 2017-07-18 12:46:29
;;; Commentary:
;;; Code:

;; 设置自动补全

(add-hook 'makefile-gmake-mode-hook
          (lambda()
            (set (make-local-variable 'company-backends)
                 '((company-cmake
                    :with company-yasnippet)
                   (claudio/company-files-without-prefix-backend
                    :with company-yasnippet)
                   (company-files
                    :with company-yasnippet)
                   (company-dabbrev
                    :with company-yasnippet)
                   ))))

(provide 'init-makefile)
;;; init-makefile.el ends here
