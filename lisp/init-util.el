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
(defvar *my/keybindings* nil "所有自行定制的快捷键绑定.")

(cl-defmacro my/use-package((&key pkg keys (req pkg) (require-p t)) &body body)
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
     ,@body
     ,(when keys				;快捷键设置。需放在最后，只有当mode调用后才能成功设置！！！
	`(my/set-keys ,@keys))
     ))

(defvar *my/keybindings* nil
  "所有自定义的快捷键.格式为：快捷键 函数 描述字符串 所在配置文件.")
(defmacro my/set-keys(&rest pairs)
  "绑定快捷键：PAIRS->(快捷键字符串 函数  快捷键描述字符串 [mode-map...])
(my/set-keys2 (\"qwd\" bind-fn 描述 mode-map...))
其中快捷键描述字符串必须出现，需为PAIR的最后一项，方便查看所有自定义快捷键

当PAIRS中的pair链表元素长度为3时（快捷键字符串 函数 描述字符串）绑定全局快捷键
当PAIR长度大于3时，将快捷键绑定在后面所有的???-mode-map上

对不同mode-map下绑定的快捷键，通过eval-after-load函数
在相应模式加载后再生效。！！这点暂时不考虑，需mode-map名
对应有！！！

缺点：参数错误检查只限于小于3的情况
"
  `(dolist (pair ',pairs)
     (let ((len (length pair))
	   (key (first pair))
	   (desc (third pair))
	   (bind-fn (second pair))
	   (maps (nthcdr 3 pair)))
       (if (< len 3)
 (error "Error:my/set-keys的参数形式为：(快捷键字符串 函数  快捷键描述字符串 [mode-map...])")
(progn
  (pushnew (append pair (list (or load-file-name buffer-file-name))) ;添加函数调用时的文件名，及行号
	   *my/keybindings* :test #'equal)
  (if (= len 3)
      (global-set-key (kbd key) bind-fn)
    (dolist (map maps)
      (when (boundp map)
	(define-key (symbol-value map) (kbd key) bind-fn)
	))))))))

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
