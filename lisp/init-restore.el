;;; init-session.el --- 重新加载上次会话状态...
;;; Commentary:
;; 1.buffer
;; 2.split windows
;; 3.命令
;;; Code:
(desktop-save-mode 1)

(setq-default
 ;;desktop文件存放文件夹.emacs.d
 desktop-path (list user-emacs-directory)
 ;;自动保存间隔秒数
 desktop-auto-save-timeout (* 5 60)
 ;; 如果开启多窗口，是否恢复多窗口状态
 desktop-save t desktop-restore-frames t)

;;退出前，保存minibuffer命令
(savehist-mode 1)

(provide 'init-restore)
;;; init-restore ends here
