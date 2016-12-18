;; elisp模式下输入单引号时只有一个但引号

(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "(" nil :actions nil)
;;
(provide 'init-elisp)
