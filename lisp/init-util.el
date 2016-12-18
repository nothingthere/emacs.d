;;;;
;;不能有任何依赖
;;;;

;;加载common lisp库
(require 'cl)

;;辅助函数

;;;;;;;;;;;;;;;;;;包/模块安装函数
(defun my-require(requires)
  "将所有需require的集中require。require：'(...)"
  (dolist(module requires)
    (require module)))

(defun my-install-all-packages(pkgs)
  "安装所有指定安装包。pkgs：：'...)"
  (interactive)
  (dolist (pkg pkgs)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;;;;;;;;;;;;;;;;buffer操作函数
(defun my-get-region()
  "获取区域。如果选中文本，返回选中区域，否则为整个buffer。
返回：'(start . end)"
  (interactive)
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (cons (point-min) (point-max))))

(defun my-current-line-empty-p ()
  "判断当前行是否为空行
网上抄的这个函数，还没学习elisp的正则表达式
网址:http://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun my-str-trim-end (str)
  "删除字符串str末尾的空白字符
来源：https://www.emacswiki.org/emacs/ElispCookbook#toc6"
  (replace-regexp-in-string (rx (* (any " \t")) eos)
			    ""
			    str))
(defun my-line-trim-end()
  "删除当前行末尾的空白字符，保留换行符。
经验：光标处在行末时，后面一个字符才是空白字符"
  (interactive)
  ;;获取行首位置
  (save-excursion
    (end-of-line);;移动到行末
    (while (looking-back "[ \t]")
      (delete-backward-char 1))))

;;
(provide 'init-util)
