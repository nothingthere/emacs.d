;;; init-python.el --- python开发环境配置
;;; Commentary:
;;; Code:
;; anaconda-mode -- 需先安装setuptools
;; ：https://pypi.python.org/pypi/setuptools

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )

;; company-anaconda -- 使用company补全
(use-package company-anaconda
  :config
  (my/company-add-backend 'python-mode-hook
                          (company-anaconda :with company-yasnippet))

  )

(provide 'init-python)
;;; init-python.el ends here
