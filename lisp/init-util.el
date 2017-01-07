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
;;;;;;;;;;;;;;;;;;默认配置辅助函数

;; (cl-defmacro my/set-default-variable(option &optional (config t))
;;   "将默认变量OPTION为CONFIG，默认设置值为t。
;; 如将scroll-error-top-bottom为t，可使用(my/enable scroll-error-top-bottom)。
;; 好像没啥子卵用。"
;;   `(setq ,option ',config))

;;;;;;;;;;;;;;;;;;包/模块安装函数

(defun my/shell-result-empty-p(cmd-string)
  "使用shell-command-to-string执行CMD-STRING后的结果是否为空字符串."
  (zerop
   (length (shell-command-to-string cmd-string))))

(cl-defmacro my/with-system-enabled((app &key (pkg-name "XXX") (apt-name app)) &body body)
  "确保系统安装了APP，如果APP有其他名字，需提供APT-NAME.
cl-defun使用方法：https://www.gnu.org/software/emacs/manual/html_node/cl/Argument-Lists.html。"
  (if (my/shell-result-empty-p (format "which %s" app))
	  `(error (format
			   "为使用插件%s，请先在系统上执行安装:sudo apt install %s"
			   ,pkg-name ,apt-name))
	`(progn ,@body)))

(cl-defmacro my/with-pkg-enabled(pkg &body body)
  (if (package-installed-p pkg)
	  `(progn
		 ,@body)
	`(error (format "需先安装%S插件" ',pkg))))

;;;;;;;;;;;;;;;;;buffer操作函数
(cl-defmacro my/with-save-everything+widen(&body body)
  "save-excursion + save-restriction + widen再执行BODY."
  `(save-excursion
	 (save-restriction
	   (widen)
	   ,@body))
  )

(defun my/get-region()
  "获取区域。如果选中文本，返回选中区域，否则为整个buffer。
返回：'(start . end)"
  (interactive)
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (cons (point-min) (point-max))))

(defun my/get-region-or-get-the-line-as-region()
  "如果有文本选择将文本选择作为返回区域；如果无，将该行作为文本区域。
返回值为：'(start . end)"
  (interactive)
  (cond ((region-active-p);;将选中区
		 (cons (region-beginning) (region-end)))
		(t (cons (line-beginning-position) (line-end-position)))))

(defun my/current-line-empty-p ()
  "判断当前行是否为空行.
网上抄的这个函数，还没学习elisp的正则表达式
网址:http://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun my/at-end-of-line-p()
  "光标所在行有内容，光标在最后。"
  (and (not (my/current-line-empty-p))
       (char-equal (char-after) ?\n)))

(defun my/str-trim-end (str)
  "删除字符串STR末尾的空白字符.
来源：https://www.emacswiki.org/emacs/ElispCookbook#toc6"
  (replace-regexp-in-string (rx (* (any " \t")) eos)
							""
							str))
(defun my/line-trim-end()
  "删除当前行末尾的空白字符，保留换行符。
经验：光标处在行末时，后面一个字符才是空白字符"
  (interactive)
  ;;获取行首位置
  (save-excursion
    (end-of-line);;移动到行末
    (while (looking-back "[ \t]")
      (delete-char -1))))

;; 链表及原子操作函数
(defmacro my/sort-symbols(symbols)
  "将所有的symbol按字符顺序排序"
  `(sort ,symbols
		 (lambda(x y)
		   (let ((str-x (symbol-name x))
				 (str-y (symbol-name y)))
			 (not (string-lessp str-x str-y))))))

(provide 'init-util)
;;; init-util.el ends here
