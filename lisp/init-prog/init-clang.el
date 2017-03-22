;;; init-clang.el --- C语言环境配置
;;; Commentary:
;;; Code:

;; macrostep -- 宏扩展
(use-package macrostep
  :bind (:map c-mode-map
              ("C-c e" . macrostep-expand)))

(provide 'init-clang)
;;; init-clang.el ends here