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
  :demand t
  :config
  (my/with-pkg-enabled
   company
   (add-hook 'company-mode-hook
			 (lambda()
			   (add-to-list 'company-backends '(company-anaconda :with company-yasnippet)))))
  )

;; py-autopep8 -- 代码美化
(use-package py-autopep8
  :demand t
  :init (my/with-system-enabled ("autopep8"
								 :msg "为使用%s，先确保安装pip，再执行sudo pip install %s"
								 ))

  :config
  (require 'py-autopep8)
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )

(provide 'init-python)
;;; init-python.el ends here
