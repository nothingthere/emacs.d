;;; init-session.el --- 重新加载上次会话状态...
;; Author:Claudio <3261958605@qq.com>
;; Created: 2017
;;; Commentary:
;;; Code:

;; 保存buffer信息
(desktop-save-mode)
;;退出前，保存minibuffer命令
(savehist-mode)

(setq-default
 ;; 如果有新的desktop文件生成，指定的执行方式
 desktop-save 'ask-if-exists
 ;;desktop文件存放文件夹.emacs.d
 desktop-path (list user-emacs-directory)

 ;;自动保存间隔秒数
 desktop-auto-save-timeout (* 5 60)

 ;; 减少自动加载buffer数量，提高启动速度
 desktop-restore-eager 4
 )

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-05/msg01261.html
;; 解决重启emacs25.1后不能重新加载上次所在buffer的问题
(when (version= emacs-version "25.1" )
  (setq-default desktop-restore-frames nil))

;; restart-emacs -- 在emacs中重启emacs
(use-package restart-emacs
  :bind ([f5] . restart-emacs)
  )

(provide 'init-restore)
;;; init-restore ends here
