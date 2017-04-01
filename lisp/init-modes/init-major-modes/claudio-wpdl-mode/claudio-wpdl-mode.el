;;; claudio-wpdl-mode.el --- 学习如何编写主模式的实验模式
;; Author: Claudio <m15982038632@gmail.com>
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

  (set (make-local-variable 'font-lock-defaults)
       ;; '(test-font-lock-keywords))
       (list claudio-wpdl-font-lock-keywords))

  (setq major-mode 'claudio-wpdl-mode)
  (setq mode-name "cWPDL")
  (run-hooks 'claudio-wpdl-mode-hook)
  )

;; 快捷键
(defvar claudio-wpdl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" #'newline-and-indent)
    (define-key map "\M-;" #'claudio-wpdl-comment)
    map)
  "定义claudio-wpdl-mode的Keymap."
  )

;; 快捷键对应函数

(defun claudio-wpdl-comment()
  "插入注释."
  (interactive)
  (let ((comment-str "//"))
    (insert comment-str " ")))

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
                                           "AUTHOR" "STATUS" "EXTENDED_ATTRIBUTE"
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

(defconst claudio-wpdl-font-lock-keywords-4
  (append claudio-wpdl-font-lock-keywords-3
          '("//.*" . font-lock-comment-face))
  "注释高亮.")

(defvar claudio-wpdl-font-lock-keywords claudio-wpdl-font-lock-keywords-4
  "默认高亮关键字.")

(provide 'claudio-wpdl-mode)
;;; claudio-wpdl-mode.el ends here
