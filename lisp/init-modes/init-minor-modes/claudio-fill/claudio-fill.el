;;; claudio-fill.el --- 自动行折叠插件
;; Author:Claudio <3261958605@qq.com>
;; Created: 2017
;;; Commentary:
;; 参考自《Writing GNU Emacs Extensions》例题
;; 感觉完全不适用，比如使用delete-indentation命令后，再反悔会出现各种混乱.
;;; Code:
(message "加载自定义claudio-fill插件...")

(define-minor-mode claudio-fill-mode
  "输入文本时自动对其段落."
  :init-value nil
  :lighter " Cf"
  (make-local-variable 'after-change-functions)
  (if claudio-fill-mode
      (add-hook 'after-change-functions #'claudio-fill-refill nil t)
    (remove-hook 'after-change-functions #'claudio-fill-refill t))
  )

;;;###autoload
(defun claudio-fill-refill(start _end len)
  "对齐当前段落.
3个参数主要是为了兼容after-change-function所需的参数.

START为文本改变前的光标位置.

_END为文本改变后的位置，插入时，START为插入前的光标位置，_END为插入后的光标位置；
删除文本时，START和_END的值相同

LEN为原来文本被改变的长度，插入时值为0，删除时值为被删文本长度."
  (interactive)
  (let ((left (if (zerop len)
                  ;; 如果为插入，左边界为光标所在行行首
                  ;; start
                  (claudio/simple-save-excursion
                   (goto-char start)
                   (beginning-of-line)
                   (point))
                ;; 如果为删除，则为上一行行首和段落首靠后处
                (claudio/simple-save-excursion
                 (max (progn
                        (goto-char start)
                        (beginning-of-line 0)
                        (point))
                      (progn
                        (goto-char start)
                        (backward-paragraph 1)
                        (point)))))))

    (unless (or
             ;; 如果插入时还没超出fill-column变量指定的范围，不对齐
             (and
              (zerop len)
              (claudio-fill--same-line-p start _end)
              (claudio-fill--short-line-p _end))
             ;; 如果前一个字符为空白字符都不执行
             ;; 包括前面时空行，前面是空格或tab
             ;; "?\ "表示所有空白字符
             (char-equal (char-syntax (char-before)) ?\ )
             ;; 解决在文本中手动换行（回车键）时报错：
             ;; Assertion failed: (eq 10 (char-before))
             (function-equal this-command #'newline))

      ;; (message "claudio-fill-refill Beginning...")
      (save-excursion
        (fill-region left _end nil nil t)
        ;; (message "claudio-fill-refill Done!")
        ))
    ))

(defun claudio-fill--same-line-p(start end)
  "START和END是否在同一行."
  ;; (message "same-line-p")
  (claudio/simple-save-excursion
   (goto-char start)
   (end-of-line)
   (<= end (point))))

(defun claudio-fill--short-line-p(pos)
  "光标位置POS所在列是否在fill-column变量范围内."
  (claudio/simple-save-excursion
   (goto-char pos)
   (<= (current-column) fill-column)))

(provide 'claudio-fill)
;;; claudio-fill.el ends here
