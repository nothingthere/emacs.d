;;; init-bash.el --- bash脚本配置文件
;;; Commentary:
;;; Code:

(use-package yasnippet
  :config
  (add-hook 'shell-mode-hook 'yas-minor-mode))

(provide 'init-bash)
;;; init-bash.el ends here
