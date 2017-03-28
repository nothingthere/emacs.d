;;; init-move.el --- buffer和窗口移动的配置
;;; Commentary:
;;; Code:

;; 调教窗口切换后，光标处在新窗口

(cl-macrolet ((my/advice-to-split-window
               (split-fn)
               ;; 分屏后在新窗口显示other-buffer的内容.
               ;; SPLIT-FN：split-window-right和split-window-below等."
               `(advice-add ,split-fn :after
                            (lambda (&rest r)
                              (let ((target-window (next-window)))
                                (set-window-buffer target-window (other-buffer))
                                (select-window target-window))))))

  ;; 左右和上下分屏后光标移动到新窗口
  (dolist (split-fn '(split-window-right split-window-below))
    (my/advice-to-split-window split-fn))

  ;; 设置在不同窗口间移动的快捷键
  (bind-keys
   ("<up>" . windmove-up)
   ("<down>" . windmove-down)
   ("<left>" . windmove-left)
   ("<right>" . windmove-right)
   )
  )

;; 对上下/左右翻页的反悔
(defun my/unscroll-maybe-remember(&rest r)
  "记录滚动前的信息."
  (unless (find last-command
                '(scroll-up-command
                  scroll-down-command
                  scroll-left
                  scroll-right)
                :test 'function-equal)
    ;; 记录当前buffer中是否可反悔滚动操作
    ;; 后面的my/unscroll函数以此来确定是否执行
    (setq-local *my/unscrollable* t)

    ;; point使用marker类型保存，保证编辑内容后也能回到原来文本处
    (unless (boundp '*my/unscroll-point*)
      (setq-local *my/unscroll-point* (make-marker)))
    (unless (boundp '*my/unscroll-window-start*)
      (setq-local *my/unscroll-window-start* (make-marker)))

    (set-marker *my/unscroll-point* (point))
    (set-marker *my/unscroll-window-start* (window-start))
    (setq-local *my/unscrol-hscroll* (window-hscroll))))

(cl-macrolet((my/advice-to-scroll
              (scroll-fn)
              ;; 生成scroll-*函数的advice-add版本
              `(advice-add ,scroll-fn :before 'my/unscroll-maybe-remember)))
  (dolist (scroll-fn '(scroll-up-command scroll-down-command scroll-left scroll-right))
    (my/advice-to-scroll scroll-fn)))

(defun my/unscroll()
  "返回没滚动前位置."
  (interactive)
  (cond ((bound-and-true-p *my/unscrollable*)
         (goto-char *my/unscroll-point*)
         (set-window-start nil *my/unscroll-window-start*)
         (set-window-hscroll nil *my/unscrol-hscroll*)
         ;; 重置，并删除marker这个内存消耗量大的变量
         (setq-local *my/unscrollable* nil)
         (set-marker *my/unscroll-point* nil)
         (set-marker *my/unscroll-window-start* nil)
         (setq-local *my/unscrol-hscroll* nil))

        (t (message "没有滚动操作记录，不能反悔。"))))

(provide 'init-move)
;;; init-move.el ends here
