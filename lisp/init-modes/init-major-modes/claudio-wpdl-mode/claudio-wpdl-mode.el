;;; claudio-wpdl-mode.el --- 学习如何编写主模式的实验模式
;; Author: Claudio <3261958605@qq.com>
;; Created: 2017-04-01 15:52:32
;;; Commentary:
;; 参考地址：https://www.emacswiki.org/emacs/ModeTutorial
;;; Code:

(defvar claudio-wpdl-mode-hook nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wpd\\'" . claudio-wpdl-mode))

(defun claudio-wpdl-mode()
  (interactive)
  (kill-all-local-variables)
  (use-local-map claudio-wpdl-mode-map)
  (set-syntax-table claudio-wpdl-syntax-table)

  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  (set (make-local-variable 'font-lock-defaults)
       (list claudio-wpdl-font-lock-keywords))

  (set (make-local-variable 'indent-line-function)
       'claudio-wpdl-indent-line)

  (setq major-mode 'claudio-wpdl-mode)
  (setq mode-name "cWPDL")
  (run-hooks 'claudio-wpdl-mode-hook)
  )

;; 快捷键
(defvar claudio-wpdl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" #'newline-and-indent)
    map)
  "定义claudio-wpdl-mode的Keymap."
  )

(defvar claudio-wpdl-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    ;; (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax Table.")

;; 定制函数
(defun claudio-wpdl-indent-line ()
  "在claudio-wpdl-mode中的缩进.
1.如果在整个buffer的最前面，则不缩进.
2.如果该行有END_字符串，则相对上一个非空行减小一级缩进.
3.如果上一行是整个buffer中第一次出现END_的行，则与该行缩进相同.
4.如果上一行是第一次出现PARTICIPANT等关键字的行，则增加一级缩进.
5.如果上面情况都不满足，则不缩进."
  (interactive)

  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t)
          (cur-indent nil)
          (keywords-need-to-indent
           '("PARTICIPANT" "MODEL"
             "WORKFLOW" "ACTIVITY"
             "TRANSITION" "APPLICATION"
             "DATA" "TOOL_LIST"
             )))
      (if (looking-at-p "^[ \t]*END_")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (when (< cur-indent 0) (setq cur-indent 0)))
        (save-excursion
          ;; 直到找到有缩进的行
          (while not-indented
            (forward-line -1)
            (cond ((looking-at-p  "^[ \t]*END_")
                   (setq cur-indent (current-indentation)
                         not-indented nil))
                  ((looking-at-p
                    (concat "^[ \\t]*" (regexp-opt keywords-need-to-indent t)))
                   (setq cur-indent (+ (current-indentation) tab-width)
                         not-indented nil))
                  ((bobp) (setq not-indented nil))))))

      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;; 高亮显示
(defconst claudio-wpdl-font-lock-keywords-1
  (list (claudio/major-mode-font-face font-lock-builtin-face
                                      '("PARTICIPANT" "END_PARTICIPANT"
                                        "MODEL" "END_MODEL"
                                        "WORKFLOW" "END_WORKFLOW"
                                        "ACTIVITY" "END_ACTIVITY"
                                        "TRANSITION" "END_TRANSITION"
                                        "APPLICATION" "END_APPLICATION"
                                        "DATA" "END_DATA"
                                        "TOOL_LIST" "END_TOOL_LIST"))
        '("\\('\\w*'\"\\)" . font-lock-variable-name-face))
  "高亮1.")

(defconst claudio-wpdl-font-lock-keywords-2
  (append claudio-wpdl-font-lock-keywords-1
          (list
           (claudio/major-mode-font-face font-lock-keyword-face
                                         '("WPDL_VERSION" "VENDOR"
                                           "CREATED" "NAME" "DESCRIPTION"
                                           "AUTHOR" "STATUS"
                                           "EXTENDED_ATTRIBUTE"
                                           "TYPE" "TOOLNAME" "IN_PARAMETERS"
                                           "OUT_PARAMETERS" "DEFAULT_VALUE"
                                           "IMPLEMENTATION" "PERFORMER"
                                           "SPLIT" "CONDITION" "ROUTE"
                                           "JOIN" "OTHERWISE" "TO" "FROM"))
           (claudio/major-mode-font-face font-lock-constant-face
                                         '("TRUE" "FALSE"))))
  "高亮2.")

(defconst claudio-wpdl-font-lock-keywords-3
  (append claudio-wpdl-font-lock-keywords-2
          (list
           (claudio/major-mode-font-face font-lock-constant-face
                                         '("ROLE" "ORGANISATIONAL_UNIT" "STRING" "REFERENCE" "AND"
                                           "XOR" "WORKFLOW" "SYNCHR" "NO" "APPLICATIONS" "BOOLEAN"
                                           "INTEGER" "HUMAN" "UNDER_REVISION" "OR"))))
  "高亮3.")

'(defconst claudio-wpdl-font-lock-keywords-4
   (append claudio-wpdl-font-lock-keywords-3
           '("//.*" . font-lock-comment-face))
   "注释高亮.")

(defvar claudio-wpdl-font-lock-keywords claudio-wpdl-font-lock-keywords-3
  "默认高亮关键字.")

(provide 'claudio-wpdl-mode)
;;; claudio-wpdl-mode.el ends here
