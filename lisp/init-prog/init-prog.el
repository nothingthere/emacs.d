;;; init-prog.el -- 编程语言综合配置
;;; Commentary:
;;; Code:

(my/require-init-files
 init-elisp
 init-html
 ;; init-js
 ;; init-markdown
 ;; init-golang
 ;; init-python
 ;; init-clang
 init-org
 init-bash
 init-check
 )

;; multi-term -- 方便打开命令行
(use-package multi-term
  :config
  (setq multi-term-program "/bin/bash")
  :bind (("M-!" . multi-term))
  )

;; magit -- github工具
(use-package magit
  :init
  (my/with-system-enabled ("git" :pkg-name "magit"))
  :bind
  (("C-x g" . magit-status))
  )

(provide 'init-prog)
;;; init-prog.el ends here
