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
 ;; 解释器位置
 python-shell-interpreter (or
                           (executable-find "python3.5")
                           python-shell-interpreter)

 )

;; anaconda-mode -- 需先安装setuptools
;;https://pypi.python.org/pypi/setuptools
;; 如果要使用python3.5则需安装python3-jedi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 报错
;; Traceback (most recent call last):
;; File "<string>", line 4, in <module>
;; File "/home/cryptomaniac/.emacs.d/anaconda-mode/0.1.7/anaconda_mode-0.1.7-py2.7.egg/anaconda_mode.py", line 17, in <module>
;; from jedi import Script, NotFoundError
;; ImportError: No module named jedi
;; 的解决方法为：
;; sudo apt install python3-jedi
;; 或者使用pip3安装
;; sudo pip3 install jedi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 报错
;; Exception happened during processing of request from ('127.0.0.1', 41164)
;; Traceback (most recent call last):
;;   File "/usr/local/lib/python3.5/dist-packages/jedi/parser/python/__init__.py", line 39, in load_grammar
;;     return _loaded_grammars[path]
;; KeyError: '/usr/local/lib/python3.5/dist-packages/jedi/parser/python/grammar3.5.txt'
;; 好像是使用pip3安装版本没有grammer*.txt文件，
;; 参考：https://jedi.readthedocs.io/en/latest/docs/installation.html 后
;; 最后手动安装：sudo pip install -e git://github.com/davidhalter/jedi.git#egg=jedi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package anaconda-mode
  :disabled t                           ;jedi有bug，还没修复，暂时不使用
  :init
  (claudio/with-sys-enabled ("jedi" :use-pip t))
  :config
  ;; company-anaconda -- 使用company补全，依赖于anaconda-mode的服务端
  (use-package
    company-anaconda
    :config
    (add-hook 'python-mode-hook
              (lambda()
                (anaconda-mode)
                (anaconda-eldoc-mode)
                (claudio/company-push-local-backend
                 '(company-anaconda :with company-yasnippet))))))

(provide 'init-python)
;;; init-python.el ends here
