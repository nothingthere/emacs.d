;;; init-org.el --- org配置
;; Author:Claudio <3261958605@qq.com>
;; Created: 2017
;;; Commentary:
;;; Code:
;;org-mode -- YES

(use-package org
  :config
  (add-hook 'org-mode-hook
            (lambda()
              ;; 打开自动换行
              (auto-fill-mode)

              ;; 单词补全
              (claudio/company-push-local-backend
               '(company-ispell :with company-yasnippet))
              )
            ;; 如果不append到最后，会使org-babel-hide-all-hashes
            ;; 有时不能正常使用，显示完整的hash值
            )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;smartparens配置

  ;; 使用:bind关键字时，不能使用匿名函数作为执行函数
  (bind-keys ("C-c t" . (lambda()
                          "打开所有未完成任务."
                          (interactive)
                          (org-agenda nil "t")))
             ("C-c C-d" . (lambda()
                            "新建日常任务."
                            (interactive)
                            (org-capture nil "d"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Capture配置
  ;; 设置org agenda的默认文件夹
  (setq
   ;; org的默认文件夹
   org-directory "~/.emacs.d/org/"
   *claudio/org-agenda-directory* (concat org-directory "agenda/")
   org-agenda-files (list *claudio/org-agenda-directory*))

  ;;org agenda 的文件夹
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-capture-templates '(("j" "生活安排(Journal)" entry (file+headline
                                                                (concat
                                                                 *claudio/org-agenda-directory*
                                                                 "生活安排.org")
                                                                "生活安排")
                                 "* TODO %? %T")
                                ("d" "每日安排(Daily)" entry (file+headline
                                                              (concat
                                                               *claudio/org-agenda-directory*
                                                               "每日安排.org")
                                                              "每日安排")
                                 "* TODO %? %T")
                                ("s" "学习安排(Study)" entry (file+headline
                                                              (concat
                                                               *claudio/org-agenda-directory*
                                                               "学习安排.org")
                                                              "学习安排")
                                 "* TODO %? %t")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;源码便捷配置
  ;; 不进行语法检查配置
  (add-hook 'org-src-mode-hook
            (lambda()
              (set (make-local-variable 'flycheck-disabled-checkers)
                   (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))
              ))

  ;; 代码块语法高亮
  (setq org-src-fontify-natively t)

  ;; 保留源代码中的空格，只在代码提取和导出时有用。对Python很有用。
  ;; 值为非nil时导出和提取时保留源码中空格。
  ;; 但在编辑源码时，会很难看，所以这里暂时设置为nil。
  (setq org-src-preserve-indentation nil)

  ;; 返回org文件区时是否询问
  ;; (setq org-src-ask-before-returning-to-edit-buffer t)

  ;; 全局有效的设置
  (setq org-babel-default-header-args '(
                                        ;; 当为none时，每个代码块执行使用单独进程，执行完毕后关闭进程。
                                        ;; 当为其他任意值时，所有session名相同的代码块公用一个进程。
                                        ;; 当不为任何值时，所有源程序语言相同的代码块公用一个进程。
                                        ;; 这里公用一个进程是指，可公用声明的函数和变量名。
                                        ;; 我希望同一种语言共享一个session，这里直接设置为none，
                                        ;; 使用claudio/org-insert-src-block生成不跟任何进程名的模板。
                                        (:session . "none")

                                        ;; 控制输出那些值的参数：
                                        ;; value：最后一个表达式的值
                                        ;; output：在命令行中执行时的所有打印，含报错信息
                                        ;; 控制处理方式的参数：
                                        ;; replace：替代原来的输出结果，对应值有
                                        ;; append和prepend
                                        ;; silent：不插入，在minibuffer中显示。
                                        ;;控制输出样式的参数：
                                        ;; raw：原样输出，如果像org表格，则按表格对其
                                        ;; org：包含在org代码块中
                                        ;; html：包含在HTML代码块中
                                        ;; latex：包含在latex代码块中
                                        ;; code：包含在与源码相同语言的代码块中
                                        ;; pp：pretty print，只支持elisp、python和ruby
                                        ;; drawer：？
                                        ;; 控制输出方式的参数：
                                        ;; table/vector：解释为org表格
                                        ;; scalar/verbatim：不解释为表格
                                        ;; list：解释为org列表
                                        (:results . "output replace pp table")

                                        ;; 导出时的选择：
                                        ;; code： 只导出代码
                                        ;; result：只导出执行结果
                                        ;; both：前两者都导出
                                        ;; none：都不导出
                                        (:exports . "both")

                                        ;;是否缓存执行结果
                                        ;; no：每次执行时都计算
                                        ;; yes：如果执行时代码无更改，不计算，使用上次的结果
                                        ;; 如果不缓存，重新打开buffer再执行代码，输出结果有时会混乱
                                        (:cache . "yes")

                                        (:noweb . "no")
                                        (:hlines . "no")

                                        ;; 提取源码时的参数
                                        ;; no：不提取
                                        ;; tangle：提取
                                        ;; 其他值：解释为相对于当前文件名的路径
                                        (:tangle . "no")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;代码导出配置
  ;; 导出时不evaluate
  (setq org-export-babel-evaluate nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;代码执行配置
  ;; 源码evaluated时不确认
  (setq org-confirm-babel-evaluate nil)
  ;; 修改执行结果中显示的关键字
  ;; 修改后造成不能显示和隐藏，还不能缩略显示HASH
  ;; (setq org-babel-results-keyword "结果")
  ;; 指定可执行的语言
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (python . t)
                                                           (sh . t)
                                                           (js . t)))

  ;; 指定执行Python代码的配置
  (setq org-babel-python-command (or (executable-find "python3")
                                     (org-babel-python-command)))

  ;; 如果为t老是询问，好烦
  (setq org-src-tab-acts-natively nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;快捷编辑源代码，参考自：http://wenshanren.org/?p=327
  (defun claudio/org-insert-src-block (src-code-type)
    "快速编辑`SRC-CODE-TYPE’源代码."
    (interactive
     (let ((src-code-types '("emacs-lisp" "python" "js" "sh" "c" "awk" "common-lisp")))
       (list (ivy-read "源代码名称：" src-code-types
                       :initial-input "python"
                       :sort t))))
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s :session\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;执行所有源码
  (defun claudio/org-evaluate-all-block-code()
    "执行buffer中所有源码."
    (interactive)
    (claudio/util-with-save-position+widen
     (goto-char (point-min))
     (while (not (eobp))
       (beginning-of-line)
       (when (looking-at-p
              ;; 第一次用elisp的正则，好痛苦。
              ;; 最后使用了 string-match-p 函数来测试才写正确了
              "^[[:space:]]*#\\+BEGIN_SRC[[:space:]]\\(python\\|emacs-lisp\\)")
         (org-babel-execute-maybe))
       (forward-line 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; 快捷键绑定
  :bind (:map org-mode-map
              ("C-c =" . er/expand-region)
              ("C-c ;" . mc/mark-all-dwim)
              ("C-c s i" . claudio/org-insert-src-block))

  ;; END
  )

;; org-bullets -- show head with bullets
(use-package org-bullets
  :disabled t
  :config
  (add-hook 'org-mode-hook (lambda()
                             (org-bullets-mode 1))))

;; org-pomodoro -- 番茄工作坊
(use-package org-pomodoro
  :disabled t
  :config
  ;; 显示格式
  (setq org-pomodoro-format "余时间~%s")
  (message "org-pomodoro on"))

;; 自定义函数
(cl-defun claudio/org-increase-headline
    (&optional (level 1))
  "将标题增加/减小一级.
遍历每行，使用正则检查改行是否为标题。如果是则改变标题等级
"
  (interactive
   (let ((choices '("1" "-1")))
     (list (string-to-int (ivy-read "增加（1），减少（-1）： "
                                    choices
                                    :initial-input "1")))))
  (cond ((not (equal major-mode 'org-mode))
         (message "此函数只能用于org-mode下"))
        (t
         (let ((icon ?*))				;表示标题的符号
           (claudio/util-with-save-position+widen
            (goto-char (point-min))
            (while (not (eobp)) ;逐行遍历
              (beginning-of-line)
              (when(looking-at-p
                    "^\*+[[:space:]]+?+")
                ;; (message (format "第%d行" (line-number-at-pos))))
                ;; 开始替换
                ;; 如果是提升一个等级
                (when (= level 1)
                  (delete-char 1)
                  ;; 最多变为一级标题
                  (when (equal (char-after)
                               ?\s)
                    (insert-char icon)))
                ;; 如果是减小一个等级
                (when (= level -1)
                  (insert-char icon)))
              (forward-line)))
           ;; 最后在整体调整缩进
           (indent-region (point-min)
                          (point-max))))))

(provide 'init-org)
;;; init-org.el ends here
