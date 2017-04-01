;;; init-timestamp.el --- 时间戳插入配置
;; Author:Claudio <m15982038632@gmial.com>
;; Created: 2017
;;; Commentary:
;;; Code:
(defvar *claudio/timestamp-insert-time-format* "%H:%M:%S"
  "* \\[claudio/timestamp-insert-time]的打印格式 (c.f. format-time-string').")

(defvar *claudio/timestamp-insert-date-format* "%F"
  "* \\[claudio/timestamp-insert-date]的打印格式 (c.f. format-time-string').")

(defvar *claudio/timestamp-insert-timestamp-format*
  (concat *claudio/timestamp-insert-date-format* " " *claudio/timestamp-insert-time-format*)
  "* \\[claudio/timestamp-insert-timestamp]的打印格式 (c.f. format-time-string').")

(defvar *claudio/timestamp-prefix* "@[["
  "*时间戳的前缀.")

(defvar *claudio/timestamp-suffix* "]]"
  "*时间戳后缀.")

(defun claudio/timestamp-instert-time
    (&optional
     type)
  "根据变量*CLAUDIO/TIMESTAMP-INSERT-TIME-FORMAT*插入当前时间.
TYPE：为'date时插入日期，'both时插入日期和时间，其他情况则插入时间."
  (interactive "*P")
  (cl-flet* ((insert-time()
                         (insert (format-time-string *claudio/timestamp-insert-time-format*
                                                     (current-time))))
             (insert-date()
                         (insert (format-time-string *claudio/timestamp-insert-date-format*
                                                     (current-time)))))
    (cl-case
        type
      ('date (insert-date))
      ('both (insert-date)
             (insert " ")
             (insert-time))
      (t (insert-time)))))

(defun claudio/timestamp-instert-timestamp()
  "使用claudio/timestamp-insert-time函数插入时间戳."
  (interactive "*")
  (insert *claudio/timestamp-prefix*)
  (insert (format-time-string *claudio/timestamp-insert-timestamp-format* (current-time)))
  (insert *claudio/timestamp-suffix*))

(defun claudio/timestamp-update-timestamp()
  "更新buffer中的时间戳."
  (interactive "*")
  (claudio/with-save-position+widen
   (save-match-data
     (goto-char (point-min))
     (let ((regexp (concat (regexp-quote *claudio/timestamp-prefix*)
                           "\\(.*\\)"
                           (regexp-quote
                            *claudio/timestamp-suffix*))))
       (while (re-search-forward regexp nil t)
         (replace-match (format-time-string *claudio/timestamp-insert-timestamp-format*
                                            (current-time)) t t nil 1))))))

(provide 'init-timestamp)
;;; init-timestamp.el ends here
