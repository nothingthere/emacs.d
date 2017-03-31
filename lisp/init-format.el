;;; init-format.el --- 文件格式化配置
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;基础格式化配置函数
(defun claudio/format-delete-top-blanklines()
  "删除顶部所有空格."
  (goto-char (point-min))
  (when (claudio/current-line-empty-p)
    (delete-blank-lines)
    (when (claudio/current-line-empty-p)
      (delete-blank-lines))))

(defun claudio/format-delete-bottom-blanklines()
  "删除文本末的空行."
  (goto-char (point-max))
  (beginning-of-line)
  (when (claudio/current-line-empty-p)
    (delete-blank-lines)
    ;; 如果完全无文本，就不进行任何操作
    ;; 当前位置不是buffer最前面
    (unless (bobp)
      (delete-backward-char 1))))

(defun claudio/format-leave-1-empty-line()
  "将buffer中多个相邻的空行只留1个."
  (goto-char (point-min))
  (let ((previous-line-empty-p (claudio/current-line-empty-p)))
    ;; 当前行是否为空
    (while (not (eobp))
      (forward-line)
      (cond ((claudio/current-line-empty-p)     ;如果当前行为空
             (if previous-line-empty-p          ;且上一行也为空
                 (delete-blank-lines)           ;则保留一个空行
               (setq previous-line-empty-p t))) ;否则标记上一行为空
            (t
             (setq previous-line-empty-p nil))))))

(defun claudio/format-indent-buffer()
  "调整整个buffer的缩进."
  (let ((indent-blacklist '(makefile-gmake-mode snippet-mode python-mode)))
    (unless (find major-mode indent-blacklist)
      (indent-region (point-min)
                     (point-max)))))

(defun claudio/format-basic()
  "删除顶部空行，（暂时不）删除底部空行，文本中最多2个空行，删除行末空白字符，且缩进。"
  (interactive)
  (claudio/with-save-position+widen (claudio/format-delete-top-blanklines)
                                    ;; (claudio/format-delete-bottom-blanklines)
                                    (claudio/format-leave-1-empty-line)
                                    (delete-trailing-whitespace (point-min)
                                                                (point-max))
                                    (claudio/format-indent-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;各种语言的独立配置
;; elisp和common lisp
(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
  (claudio/add-local-before-save-hook hook #'claudio/format-basic))

;; python
(use-package py-autopep8
  :init (claudio/with-system-enabled ("autopep8"
                                      :msg "为使用%s，先确保安装pip，再执行sudo pip install %s"))
  :config (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

;; clang家族语言：c c++ js
;; 确保本地安装了clang-format程序
(claudio/with-system-enabled ("clang-format" :pkg-name "clang-format")
                             (quelpa '(clang-format :repo "nothingthere/clang-format" :fetcher github)
                                     :update nil)

                             ;; 保存前执行clang-format，参考py-autopep8的作法
                             (dolist (hook '(c-mode-hook c++-mode-hook))
                               (claudio/add-local-before-save-hook hook 'clang-format-buffer)))

;; sh
(claudio/add-local-before-save-hook 'sh-mode-hook #'claudio/format-basic)

;; org
(claudio/add-local-before-save-hook 'org-mode-hook #'claudio/format-basic)

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
              (claudio/format-delete-bottom-blanklines)))

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
