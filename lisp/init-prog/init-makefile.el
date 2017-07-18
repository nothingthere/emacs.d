;;; init-makefile.el --- makefile-gmake模式配置
;; Author: Claudio <3261958605@qq.com.com>
;; Created: 2017-07-18 12:46:29
;;; Commentary:
;;; Code:

;; 设置自动补全

(add-hook 'makefile-gmake-mode-hook
          (lambda()
            (set (make-local-variable 'company-backends)
                 '(company-cmake
                   claudio/company-files-without-prefix-backend
                   company-files
                   company-dabbrev))))

(provide 'init-makefile)
;;; init-makefile.el ends here
