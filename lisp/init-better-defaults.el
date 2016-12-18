;;;改善默认配置
;;关闭菜单、工具和滚动条
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
;;(global-linum-mode t)

;;改变光标样式
					;setq 和 setq-default 的区别：
					;对于buffer.local的变量，使用setq只在当前buffer中有效
(setq-default cursor-type 'bar)

;;禁用备份文件
(setq make-backup-files nil)

;;开启时全屏显示
(setq
 initial-frame-alist '((fullscreen . maximized)))

;;高亮匹配括号
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;;高亮当前行
(global-hl-line-mode t)

;;关闭移动到顶部/底部时的警告音
(setq ring-bell-function 'ignore)

;;将yes-or-no别名为y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;;dire-mode默认设置修改
(setq dired-recursive-copies 'always ;;递归复制目录
      dired-recursive-deletes 'always;;递归删除目录
      )
;;避免使用dire-mode时生成多余文件？？没懂
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired;;当使用时才加载
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;;;;打开当前文件所在文件夹的dired
(require 'dired-x);;默认快捷键C-x C-j
;;;;在方便在同时打开的两个dired中复制文件
(setq dired-dwim-target t)

;;隐藏和删除DOS换行符
(defun hidden-dos-eol()
  "不显示DOS换行符"
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(defun remove-dos-eof()
  "替换DOS的换行符"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

;;保存上次打开状态
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)
;;(load "desktop")
;;(desktop-load-default)
;;(desktop-read) 
;;
(provide 'init-better-defaults)
