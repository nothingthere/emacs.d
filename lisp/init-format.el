;;; init-format.el --- 文件格式化配置
;; Author:Claudio <3261958605@qq.com>
;; Created: 2017
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;基础格式化配置函数
(defun claudio/format-delete-top-blanklines()
  "删除顶部所有空格."
  (claudio/util-simple-save-excursion
   (goto-char (point-min))
   (when (claudio/util-current-line-empty-p)
     (delete-blank-lines)
     (when (claudio/util-current-line-empty-p)
       (delete-blank-lines)))))

(defun claudio/format-leave-1-empty-line(start end)
  "将buffer中从START到END多个相邻的空行只留1个."
  (claudio/util-simple-save-excursion
   (save-restriction
     (widen)
     (goto-char start)
     (let ((previous-line-empty-p (claudio/util-current-line-empty-p)))
       ;; 当前行是否为空
       (while (and
               (not (eobp))
               (< (point) end))
         (forward-line)
         (cond ((claudio/util-current-line-empty-p)     ;如果当前行为空
                (if previous-line-empty-p          ;且上一行也为空
                    (delete-blank-lines)           ;则保留一个空行
                  (setq previous-line-empty-p t))) ;否则标记上一行为空
               (t
                (setq previous-line-empty-p nil))))))))

(defun claudio/format-delete-bottom-blanklines(&optional strict)
  "删除文本末的空行，保证最后有空行.如果STRICT参数为non-nil，末尾不留空行.
末尾不留空行的情况，仅适用于org-src源码格式化."
  (claudio/util-simple-save-excursion
   (goto-char (point-max))
   (cond (strict
          (beginning-of-line)
          (when (claudio/util-current-line-empty-p)
            (delete-blank-lines)
            ;; 如果完全无文本，就不进行任何操作
            ;; 当前位置不是buffer最前面
            (unless (bobp)
              (delete-backward-char 1))))
         (t
          (end-of-line)
          ;; 添加新行，保证至少有一个空行
          (newline)
          (delete-blank-lines)))))

(defun claudio/format-indent-region(start end)
  "调整buffer中START到END的的缩进."
  (save-restriction
    (widen)
    (let ((indent-blacklist '(makefile-gmake-mode snippet-mode python-mode)))
      (unless (find major-mode indent-blacklist)
        (indent-region start end)))))

(defun claudio/format:get-format-region-for-big-buffer(lines-for-big-buffer)
  "获取当前行前面LINES-FOR-BIG-BUFFER行处的位置作为start，当前行后面一行处作为end，返回(start . end)."
  (claudio/util-simple-save-excursion
   (save-restriction
     (widen)
     (let ((start
            (progn (forward-line (- lines-for-big-buffer))
                   (point)))
           (end
            ;; +1是为了缩进整个区域时包含当前行
            ;; 再加上lines-for-big-buffer是为了在org模式下缩进光标后的内容
            (progn (forward-line (+ (1+ lines-for-big-buffer) lines-for-big-buffer))
                   (point))))
       (cons start end)))))

(defun claudio/format-basic()
  "删除顶部空行，删除底部空行，文本中相邻空行只保留一个，删除行末空白字符，且缩进。
当buffer太大时，只格式化前面N行到当前行
虽然提升了性能，可能会造成不完全格式化，如
1. 没保存就跳转到距离当前行很远的地方编辑，再保存
2. 输入>LINES-FOR-BIG-BUFFER行还没保存
解决办法：随时保存
LINES-FOR-BIG-BUFFER的确定方法：
笔记本全屏显示为40行，所以设置为40
即使是大屏，也很少书写40行后还未保存
"
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (big-buffer-size 10000)
        (lines-for-big-buffer 40))

    ;; 当buffer太大时，重新设置格式化区域
    (when (> (buffer-size) big-buffer-size)
      (let ((region (claudio/format:get-format-region-for-big-buffer lines-for-big-buffer)))
        (setq start (car region)
              end (cdr region))))

    (claudio/format-delete-top-blanklines)
    (claudio/format-leave-1-empty-line start end)
    (claudio/format-delete-bottom-blanklines)
    (delete-trailing-whitespace start end)
    (claudio/format-indent-region start end)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;各种语言的独立配置
;; elisp和common lisp
(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
  (claudio/util-add-local-before-save-hook hook #'claudio/format-basic))

;; python
(use-package py-autopep8
  :init
  ;; 本来可以使用python-autpep8，但是会出现卡顿，所以使用pip版本
  (claudio/app-may-tobe-installed "autopep8" :use-pip t)
  :config (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

;; golang
(use-package go-mode
  :config
  (claudio/util-add-local-before-save-hook 'go-mode-hook
                                           #'gofmt-before-save))

;; clang家族语言：c c++ js
;; 确保本地安装了clang-format程序
(claudio/app-may-tobe-installed "clang-format")
(quelpa '(clang-format :repo "nothingthere/clang-format" :fetcher github)
        :update nil)
;; 保存前执行clang-format，参考py-autopep8的作法
(dolist (hook '(c-mode-hook c++-mode-hook))
  (claudio/util-add-local-before-save-hook hook #'clang-format-buffer))

;; sh
(claudio/util-add-local-before-save-hook 'sh-mode-hook #'claudio/format-basic)

;; org
(claudio/util-add-local-before-save-hook 'org-mode-hook #'claudio/format-basic)

;; 源码
(advice-add 'org-edit-src-exit
            :before
            (lambda (&rest _r)
              "格式化源码."
              (cl-case
                  major-mode
                ((emacs-lisp-mode lisp-mode org-mode sh-mode)
                 (claudio/format-basic))
                (python-mode
                 (let ((old-py-autopep8-options py-autopep8-options))
                   ;; 由于执行代码时相当于时在解释器中逐行输入，
                   ;; 函数定义和类定义中不能有空行
                   (setq py-autopep8-options '("--ignore=E301"))
                   (py-autopep8-buffer)
                   (setq py-autopep8-options old-py-autopep8-options)))
                ((c-mode c++-mode js-mode)
                 (clang-format-buffer)))
              ;; 全部去首尾空行
              (claudio/format-delete-top-blanklines)
              (claudio/format-delete-bottom-blanklines t)))

;;1. 刚开始使用 (advice-add 'org-edit-src-exit :before #'claudio/org-src-beautify)
;;调用org-edit-src-exit时还行，不过调用org-edit-src-save时总是报参数错误

;; 2.通过下面add-hook的方法，可解决问题，不过代价就是进入和退出源码编辑时都会调用claudio/org-src-beautify函数。
;; 原因：为org-src-mode-hook时进入和退出 "后" 时都会使用的钩子。
;; 不过：还可接受，因为进入编辑时，py-autopep8会生成临时文件（查看*Message*可知），如果没改变内容，也不会重复美化。
;; 但是：(claudio/format-basic)和(claudio/format-delete-bottom-blanklines)会重复执行，影响效率

;; 3.最后还是用(add-hook 'org-src-mode-hook  'claudio/org-src-beautify)添加钩子的形式
;; 原因为org-src-mode-hook为进入和退出源码编辑 “后” 时的钩子。

;; 最终研究得出：
;; 调用org-edit-src-exit时，为了在原buffer中保存，会自动调用一次org-edit-src-save，
;; 所以使用方法1时会保存。
;; 那么为啥调用org-edit-src-save时会保存呢：
;; 保存信息指出是参数个数不正确。参考：https://www.gnu.org/software/emacs/manual/html_node/elisp/Advice-combinators.html
;; 得知，before形式的函数相当于为：(lambda(&rest r) (apply fn r) (apply oldFn r))
;; 这里使用了apply函数，所以声明claudio/org-src-beautify函数时也应有参数&rest ...
;; 所以在声明修饰函数时添加此参数即可
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-format)
;;; init-format.el ends here
