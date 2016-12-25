;;; init-util.el -- 辅助函数
;;; Commentary:
;;; Code:
;;加载common lisp库
;; 如果只是简单使用(require 'cl)，会出现"cl package requred at run time"
;; 解决办法：
;; 1. http://stackoverflow.com/questions/5019724/in-emacs-what-does-this-error-mean-warning-cl-package-required-at-runtime
;; 2. https://web.archive.org/web/20141203143357/http://dto.github.io/notebook/require-cl.html#sec-8
(with-no-warnings
  (require 'cl))

;;辅助函数

;;;;;;;;;;;;;;;;;;包/模块安装函数
(defvar *my/requires* nil "所有的require.")
(defvar *my/packages* nil "所有已经安装的包.")
(cl-defmacro my/use-package((&key pkg (req pkg) (require-p t)) &body body)
  "如果提供PKG参数，确保安装插件PKG。如果REQUIRED-P为非nil，(require REQ), REQ默认与PKG同名.
使用方法：
1. 如果需安装包且需require，直接使用(my/use-package :pkg PKG)
REQ默认与PKG同名，如果不同，使用(my/use-package :pkg PKG :req ANOTHER-NANE)
2. 如果只需安装，不require，使用(my/use-package :pkg PKG :require-p nil)
3. 如果只需require某种内置feature，使用(my/use-package :req REQ)
!!!不清楚是否有变量捕获，是否应该使用gensym。"
  ;;确保安装pkg
  `(progn
     ,(when pkg
	`(progn (when (not (package-installed-p ',pkg))
		  (package-install ',pkg))
		(pushnew ',pkg *my/packages*)))	;放在最后，以免包名错误还是写入
     ;;是否require
     ,(when (and req require-p)
	`(progn
	   (require ',req)
	   (pushnew ',req *my/requires*)))	;放在最后，以免feature不存在还是写入
     ;;执行函数体
     ,@body))

(defmacro my/set-keys(pairs)
  "绑定快捷键：PAIRS->(快捷键字符串 函数 [mode-map...])
当PAIRS中的pair链表元素长度为2时绑定全局快捷键
当pair长度大于2时，将快捷键绑定在后面所有的???-mode-map上

对不同mode-map下绑定的快捷键，通过eval-after-load函数
在相应模式加载后再生效。！！这点暂时不考虑，需mode-map名
对应有

"
  `(flet ((get-mode-symbol(mode-map-symbol)
			  ;;通过mode-map名获取mode名，如'emacs-lisp-mode-map -> emacs-lisp-mode
			  (let* ((str (symbol-name mode-map-symbol))
				 (str-len (length str))
				 (suffix-len (length "-map")))
			    (make-symbol (substring str 0 (- str-len suffix-len))))))

     (dolist (pair ',pairs)
       (let ((len (length pair)))
	 (cond ((= len 2) ;;全局快捷键
		(global-set-key (kbd (car pair))
				(car (cdr pair))))
	       ((> len 2) ;;不同mode下的快捷键
		(dolist (mode-map (nthcdr 2 pair))
		  (eval-after-load
		      (let ((mode (get-mode-symbol mode-map)))
			(if (fboundp mode)
			    mode
			  'fundamental-mode))
		    (define-key
		      (symbol-value mode-map);;这里用该有改进！！！
		      (kbd (first pair))
		      (second pair)))))
	       (t (message "快捷键设置错误！")))))))

(cl-defun my/ensure-system-configed(app &key (pkg-name "XXX") (apt-name app))
  "确保系统安装了APP，如果APP有其他名字，需提供APT-NAME.
cl-defun使用方法：https://www.gnu.org/software/emacs/manual/html_node/cl/Argument-Lists.html。"
  (when (zerop (length (shell-command-to-string (format "which %s" app))))
    (error (format
	    "为使用插件%s，请先在系统上执行安装:sudo apt install %s"
	    pkg-name apt-name))))

;;;;;;;;;;;;;;;;;buffer操作函数
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
