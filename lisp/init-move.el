;;; init-move.el --- buffer和窗口移动的配置
;;; Commentary:
;;; Code:

(cl-defmacro my/advice-to-split-window(split-fn)
  "分屏后在新窗口显示other-buffer的内容.
 SPLIT-FN：split-window-right和split-window-below等."
  `(advice-add ',split-fn :after
               (lambda (&rest r)
                 (let ((target-window (next-window)))
                   (set-window-buffer target-window (other-buffer))
                   (select-window target-window)))))

;; 左右和上下分屏后光标移动到新窗口
(my/advice-to-split-window split-window-right)
(my/advice-to-split-window split-window-below)

;; 设置在不同窗口间移动的快捷键
(bind-keys
 ("<up>" . windmove-up)
 ("<down>" . windmove-down)
 ("<left>" . windmove-left)
 ("<right>" . windmove-right)
 )

(provide 'init-move)
;;; init-move.el ends here
