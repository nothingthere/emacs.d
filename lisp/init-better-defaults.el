;;; init-better-defaults.el --- 改善默认配置
;;; Commentary:
;;; Code:
;; 退出emacs前确认
(setq confirm-kill-emacs #'yes-or-no-p)

;;禁用备份文件
(setq make-backup-files nil)

;;关闭移动到顶部/底部时的警告音
;; (setq ring-bell-function 'ignore)
;; (setq visible-bell t)

;; 剪切只读文本时不发出警报声
(setq kill-read-only-ok t)

;; 来回翻页时保留光标位置
(setq scroll-preserve-screen-position t)

;; 不延迟register显示时间
;; (setq register-preview-delay nil)

;;将yes-or-no别名为y-or-n
(fset #'yes-or-no-p #'y-or-n-p)

;;dire-mode默认设置修改
(setq
 ;;递归复制目录
 dired-recursive-copies 'always
 ;;递归删除目录
 dired-recursive-deletes 'always)

;; 设置默认行宽
(setq-default fill-column 70)

;;避免使用dire-mode时生成多余文件？？没懂
(put #'dired-find-alternate-file 'disabled nil)

;;当使用时才加载dired
(with-eval-after-load
    'dired
  (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))

;;;;打开当前文件所在文件夹的dired
;;默认快捷键C-x C-j
(require 'dired-x)
;;;;在方便在同时打开的两个dired中复制文件
(setq dired-dwim-target t)

;; 获取帮助搜索时显示更多结果
(setq apropos-do-all t)

;; 开启禁用特性
(defmacro claudio/enable (&rest functions)
  "解除禁用功能,FUNCTIONS为禁用函数名."
  `(dolist (function ',functions)
     (put function 'disabled nil)))

(dolist (fn '(upcase-region
              downcase-region
              narrow-to-region
              scroll-left
              scroll-right))
  (put fn 'disabled nil))

(provide 'init-better-defaults)
;;; init-better-defaults.el ends here
