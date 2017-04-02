;;; init-session.el --- 重新加载上次会话状态...
;; Author:Claudio <m15982038632@gmial.com>
;; Created: 2017
;;; Commentary:
;; 1.buffer
;; 2.split windows
;; 3.命令
;;; Code:
(desktop-save-mode 1)

(setq-default
 desktop-save t
 ;;desktop文件存放文件夹.emacs.d
 desktop-path (list user-emacs-directory)

 ;;自动保存间隔秒数
 desktop-auto-save-timeout 30
 ;; 是否恢复多窗口状态
 desktop-restore-frames t
 desktop-restore-eager 10
 )

;;退出前，保存minibuffer命令
(savehist-mode 1)

;; restart-emacs -- 在emacs中重启emacs
(use-package restart-emacs
  :bind ([f5] . restart-emacs)
  )

(provide 'init-restore)
;;; init-restore ends here
