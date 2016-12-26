;;; init-elisp.el --- emacs 配置
;;; Commentary:
;;; Code:
(my/use-package
 (:pkg macrostep
       :require-p nil
       :keys
       (("C-c e" macrostep-expand  "宏扩展"  emacs-lisp-mode-map lisp-interaction-mode-map)
	))
 )

(provide 'init-elisp)
;;; init-elisp.el ends here
