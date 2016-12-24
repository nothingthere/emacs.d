;;; init-session.el --- 重新加载上次会话状态...
;;; Commentary:
;; 1.buffer
;; 2.split windows
;; 3.命令
;;; Code:
(desktop-save-mode 1)
(setq
 desktop-path (list user-emacs-directory) ;;desktop文件存放文件夹.emacs.d
 desktop-auto-save-timeout (* 5 60) ;;自动保存间隔秒数
 desktop-save t
 desktop-restore-frames t
 )

;;;
(savehist-mode 1) ;;退出前，保存minibuffer命令

(provide 'init-restore)
;;; init-restore ends here
