;;; init-util.el -- 辅助函数
;; Author:Claudio <m15982038632@gmial.com>
;; Created: 2017
;;; Commentary:
;;; Code:
;;加载common lisp库
;; 如果只是简单使用(require 'cl)，会出现"cl package requred at run time"
;; 解决办法：
;; 1. http://stackoverflow.com/questions/5019724/in-emacs-what-does-this-error-mean-warning-cl-package-required-at-runtime
;; 2. https://web.archive.org/web/20141203143357/http://dto.github.io/notebook/require-cl.html#sec-8
;; 加载cl库，方便使用common-lisp
(with-no-warnings
  (require 'cl)
  )

;;辅助函数
(defun claudio/string-empty-p(str)
  "字符串STR是否不含任何字符串.
本来有内置string-empty-p函数，不过需(require 'subr-x)
目前只是用这一个函数，代价挺大."
  (zerop (length str)))

(cl-defun claudio/list2string(lst &optional (sep " "))
  "将链表LST转换为字符串，用SEP隔开.
不支持嵌套链表."
  (concatenate
   'string
   (car lst)
   (apply #'concatenate 'string
          (mapcar (lambda(str)
                    (format "%s%s" sep str))
                  (cdr lst)))))

(defun claudio/system-running-p(pname)
  "判断系统上是否有满足PNAME的进程开启.
参考自：https://gist.github.com/tkhoa2711/f3d9c1cb04597f03ee76
"
  (dolist (pid (list-system-processes))
    (let ((attrs (process-attributes pid)))
      (when (string= pname (cdr (assoc 'comm attrs)))
        (return t)))))

;; (claudio/system-running-p "emacs24")

;;;;;;;;;;;;;;;;;buffer操作函数
(cl-defmacro claudio/with-save-position+widen (&body body)
  "'save-excursion + 'save-restriction + 'widen再执行 &BODY."
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(defun claudio/get-region()
  "获取区域。如果选中文本，返回选中区域，否则为整个buffer。
返回：'(start . end)"
  (interactive)
  (if (region-active-p)
      (cons (region-beginning)
            (region-end))
    (cons (point-min)
          (point-max))))

(defun claudio/get-region-or-get-the-line-as-region()
  "如果有文本选择将文本选择作为返回区域；如果无，将该行作为文本区域。
返回值为：'(start . end)"
  (interactive)
  (cond ((region-active-p)
         (cons (region-beginning)
               (region-end)))
        (t (cons (line-beginning-position)
                 (line-end-position)))))

(defun claudio/current-line-empty-p ()
  "判断当前行是否为空行.
网上抄的这个函数，还没学习elisp的正则表达式
网址:http://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[[:space:]]*$")))

(defun claudio/at-end-of-line-p()
  "光标所在行有内容，且光标在最后。"
  (and (not (claudio/current-line-empty-p))
       (or (eobp)                        ;; 或者是文本末尾
           (char-equal (char-after) ?\n) ;; 后面是换行符（只对linux环境文件有效）
           )))

(defun claudio/str-trim-end (str)
  "删除字符串STR末尾的空白字符.
来源：https://www.emacswiki.org/emacs/ElispCookbook#toc6"
  (replace-regexp-in-string (rx (* (any " \t")) eos) "" str))
;; (defun claudio/line-trim-end()
;;   "删除当前行末尾的空白字符，保留换行符。
;; 经验：光标处在行末时，后面一个字符才是空白字符"
;;   (interactive)
;;   ;;获取行首位置
;;   (save-excursion
;;     (end-of-line);;移动到行末
;;     (while (looking-back "[ \t]")
;;       (delete-char -1))))

;; 链表及原子操作函数
(defmacro claudio/sort-symbols(symbols)
  "将所有的symbol按字符顺序排序"
  `(sort ,symbols
         (lambda(x y)
           (let ((str-x (symbol-name x))
                 (str-y (symbol-name y)))
             (not (string-lessp str-x str-y))))))

(defun claudio/reload-buffer()
  "重新从磁盘读取文件."
  (interactive)
  (let ((file (buffer-file-name)))
    (kill-buffer)
    (find-file file)))

(cl-defmacro claudio/add-local-before-save-hook (mode-hook &body body)
  "在为MODE-HOOK的before-save-hook添加函数."
  `(add-hook ,mode-hook
             (lambda()
               (add-hook 'before-save-hook ,@body nil t))))

(cl-defmacro claudio/simple-save-excursion(&body body)
  "简化版的save-excursion,只记录当前光标.
参考《Writing GNU Emacs Extensions》的作法，只保留光标位置.
原来函数会记录当前buffer，marker和光标。造成性能低下."
  (let ((original-point-symbol (cl-gensym)))
    `(let ((,original-point-symbol (point-marker)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,original-point-symbol)
         (set-marker ,original-point-symbol nil))))
  )

(cl-defmacro claudio/with-app-enabled (pkg &body body)
  "是否安装有插件APP，如果安装则执行&BODY,否则报错."
  (if (package-installed-p pkg)
      `(progn
         ,@body)
    `(error
      (format "需先安装%S插件" ',pkg))))

;; (claudio/simple-save-excursion
;;  (mark-whole-buffer)
;;  (goto-char (point-min))
;;  (message "%d" (point))
;; )

(provide 'init-util)
;;; init-util.el ends here
