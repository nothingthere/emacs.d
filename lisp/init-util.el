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

(defun claudio/string-empty-p(str)
  "字符串STR是否不含任何字符串.
本来有内置string-empty-p函数，不过需(require 'subr-x)
目前只是用这一个函数，代价挺大."
  (zerop (length str)))

;;辅助函数
;;;;;;;;;;;;;;;;;;自动安装系统程序配置
(defun claudio/sys-app-install-p(app)
  "系统是否安装APP.
使用execute-find函数只能找到可执行程度。有时不能确定程序是否安装，如python3-jedi."
  (or (executable-find app)
      (let ((command (format "dpkg --list | awk '{print $2}' | grep ^%s$" app)))
        (not (claudio/string-empty-p (shell-command-to-string command))))))

;; (claudio/sys-app-install-p "silversearcher-ag")

;; 不清楚为了要使用  (let ((default-directory "/sudo::/")
;; 参考自：https://lists.gnu.org/archive/html/emacs-orgmode/2013-02/msg00354.html
;; 和：http://emacs.stackexchange.com/questions/29555/how-to-run-sudo-commands-using-shell-command
(defun claudio/sys-install(app)
  "属于sudo命令安装APP.
sudo apt install APP"
  (message "系统正在执行sudo apt install %s命令，可能会造成卡顿." app)
  (let ((default-directory "/sudo::/")
        (command (format "sudo apt install %s" app)))
    (async-shell-command command))
  (message "系统成功安装%s程序" app))

;; (claudio/sys-install "silversearcher-ag")

(defvar *claudio/ensure-all-sys-apps-installed-p* t
  "是否安装函数clauduio/with-system-enabled函数指定的系统程序.
如果是非Debian操作系统，可将此值设为nil，然后手动安装.")

(defvar *claudio/sys-apps-tobe-installed* nil
  "需要在系统上安装的程序.")

(add-hook 'after-init-hook
          (lambda()
            "安装所有需在系统上安装的程序."
            (when *claudio/sys-apps-tobe-installed*
              (let ((app-str (apply #'concatenate 'string
                                    (mapcar (lambda(str)
                                              (concatenate 'string " " str))
                                            *claudio/sys-apps-tobe-installed*))))
                (claudio/sys-install app-str))))
          ;; 最后执行
          t)

(cl-defmacro claudio/with-sys-enabled((app &optional manual) &body body)
  "确保系统上安装程序APP.
如果manual为non-nil，表示需手动安装的程序，如果没安装，只是提醒。如lantern.
如果变量*claudio/ensure-all-sys-app-installed-p*为non-nil，则直接安装.
如果为nil，则只是警告。"
  (unless (claudio/sys-app-install-p app)
    (cond (manual (message "需在系统上手动安装%s，才能确保功能完全." app))
          (*claudio/ensure-all-sys-apps-installed-p*
           (add-to-list '*claudio/sys-apps-tobe-installed*  app))
          (t (warn "系统上没安装%s，使用过程中可能会报错！" app))))
  `(progn ,@body))

(cl-defmacro claudio/with-app-enabled (app &body body)
  "是否安装有插件APP，如果安装则执行&BODY,否则报错."
  (if (package-installed-p app)
      `(progn
         ,@body)
    `(error
      (format "需先安装%S插件" ',app))))

(defun claudio/system-running-p(pname)
  "判断系统上是否有满足PNAME的进程开启.
参考自：https://gist.github.com/tkhoa2711/f3d9c1cb04597f03ee76
"
  (dolist (pid (list-system-processes))
    (let ((attrs (process-attributes pid)))
      (when (string= pname (cdr (assoc 'comm attrs)))
        (return t)))))
;; (claudio/system-running-p "lantern")

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

;; (claudio/simple-save-excursion
;;  (mark-whole-buffer)
;;  (goto-char (point-min))
;;  (message "%d" (point))
;; )

(provide 'init-util)
;;; init-util.el ends here
