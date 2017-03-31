;;; init-util.el -- 辅助函数
;;; Commentary:
;;; Code:
;;加载common lisp库
;; 如果只是简单使用(require 'cl)，会出现"cl package requred at run time"
;; 解决办法：
;; 1. http://stackoverflow.com/questions/5019724/in-emacs-what-does-this-error-mean-warning-cl-package-required-at-runtime
;; 2. https://web.archive.org/web/20141203143357/http://dto.github.io/notebook/require-cl.html#sec-8
;; 加载cl库，方便使用common-lisp
(with-no-warnings
  (require 'cl))

;;辅助函数
;;;;;;;;;;;;;;;;;;包/模块安装函数

(cl-defmacro claudio/with-system-enabled ((app &key
                                               (pkg-name "XXX")
                                               (apt-name app)
                                               (msg "为使用插件%s，请先在系统上执行安装:sudo apt install %s")) &body body)
  "确保系统安装了APP，如果APP有其他名字，需提供APT-NAME.
cl-defun使用方法：https://www.gnu.org/software/emacs/manual/html_node/cl/Argument-Lists.html。"
  (if (not (executable-find (format "%s" app))) ;; (claudio/shell-result-empty-p (format "which %s" app))
	  `(error
        (format ,msg ,pkg-name ,apt-name))
    `(progn
       ,@body)))

(cl-defmacro claudio/with-pkg-enabled (pkg &body body)
  "是否安装有插件PKG，如果安装则执行&BODY,否则报错."
  (if (package-installed-p pkg)
      `(progn
         ,@body)
    `(error
      (format "需先安装%S插件" ',pkg))))

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

(defun claudio/system-running-p(pname)
  "判断系统上是否有满足PNAME的进程开启.
参考自：https://gist.github.com/tkhoa2711/f3d9c1cb04597f03ee76
"
  (dolist (pid (list-system-processes))
    (let ((attrs (process-attributes pid)))
      (when (string= pname (cdr (assoc 'comm attrs)))
        (return t)))))
;; (claudio/system-running-p "lantern")

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

;; (claudio/simple-save-excursion
;;  (mark-whole-buffer)
;;  (goto-char (point-min))
;;  (message (format "%d" (point)))
;;  )

(provide 'init-util)
;;; init-util.el ends here
