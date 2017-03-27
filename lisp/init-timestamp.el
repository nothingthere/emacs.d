;;; init-timestamp.el --- 时间戳插入配置
;;; Commentary:
;;; Code:
(defvar *my/timestamp-insert-time-format* "%H:%M:%S"
  "* \\[my/timestamp-insert-time]的打印格式 (c.f. format-time-string')."
  )
(defvar *my/timestamp-insert-date-format* "%F"
  "* \\[my/timestamp-insert-date]的打印格式 (c.f. format-time-string')."
  )

(defvar *my/timestamp-insert-timestamp-format*
  (concat *my/timestamp-insert-date-format*
          " "
          *my/timestamp-insert-time-format*
          )
  "* \\[my/timestamp-insert-timestamp]的打印格式 (c.f. format-time-string')."
  )

(defvar *my/timestamp-prefix* "@[["
  "*时间戳的前缀.")
(defvar *my/timestamp-suffix* "]]"
  "*时间戳后缀.")

(defun my/timestamp-instert-time(&optional type)
  "根据变量*my/timestamp-insert-time-format*插入当前时间."
  (interactive "*P")
  (cl-flet* ((insert-time()
                         (insert (format-time-string
                                  *my/timestamp-insert-time-format*
                                  (current-time))))
             (insert-date()
                         (insert (format-time-string
                                  *my/timestamp-insert-date-format*
                                  (current-time)))))
    (cl-case type
      ('date (insert-date))
      ('both (insert-date)
             (insert " ")
             (insert-time))
      (t (insert-time)))))

(defun my/timestamp-instert-timestamp()
  "使用my/timestamp-insert-time函数插入时间戳."
  (interactive "*")
  (insert *my/timestamp-prefix*)
  (insert (format-time-string *my/timestamp-insert-timestamp-format*
                              (current-time)))
  (insert *my/timestamp-suffix*)
  )

(defun my/timestamp-update-timestamp()
  "更新buffer中的时间戳."
  (interactive "*")
  (my/with-save-position+widen
   (save-match-data
     (goto-char (point-min))
     (let ((regexp (concat (regexp-quote *my/timestamp-prefix*)
                           "\\(.*\\)"
                           (regexp-quote *my/timestamp-suffix*))))
       (while (re-search-forward regexp nil t)
         (replace-match (format-time-string
                         *my/timestamp-insert-timestamp-format*
                         (current-time))
                        t t nil 1))))))

(provide 'init-timestamp)
;;; init-timestamp.el ends here
