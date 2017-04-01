;;; init-python.el --- python开发环境配置
;; Author:Claudio <m15982038632@gmial.com>
;; Created: 2017
;;; Commentary:
;;; Code:
;; 不让emacs猜测缩进宽度
(setq-default
 ;; 默认缩进长度
 python-indent-offset 4
 ;; 不猜测缩进
 python-indent-guess-indent-offset nil
 )

;; anaconda-mode -- 需先安装setuptools
;; ：https://pypi.python.org/pypi/setuptools
(use-package
  anaconda-mode
  :config
  ;; company-anaconda -- 使用company补全，依赖于anaconda-mode的服务端
  (use-package
    company-anaconda
    :config (add-hook 'python-mode-hook
                      (lambda()
                        (anaconda-mode)
                        (anaconda-eldoc-mode)
                        (claudio/company-push-local-backend
                         '(company-anaconda :with company-yasnippet))))))

(provide 'init-python)
;;; init-python.el ends here
