;;; init-flyspell.el --- 拼写检查配置
;; Author: Claudio <3261958605@qq.com.com>
;; Created: 2017-04-09 20:57:55
;;; Commentary:
;; 参考自Purcell：https://github.com/nothingthere/emacs.d-1/blob/master/lisp/init-flyspell.el
;;; Code:
(use-package flyspell
  :init
  (claudio/app-may-tobe-installed "aspell")
  :config
  (setq-default ispell-program-name (executable-find "aspell")
                ;; 个人词典位置
                ;; 此文件中的单词不能用于补全
                ispell-personal-dictionary "~/.emacs.d/aspell.en.pws")

  ;; 为所有编程buffer的注释和字符串添加检查功能
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  ;; 为文本编辑模式添加拼写检查功能，当然也包含org模式
  (add-hook 'text-mode-hook 'flyspell-mode)

  )

(provide 'init-flyspell)
;;; init-flyspell.el ends here
