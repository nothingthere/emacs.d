;;; init-prog.el -- 编程语言综合配置
;; Author:Claudio <3261958605@qq.com>
;; Created: 2017
;;; Commentary:
;;; Code:

(claudio/require-init-files
 init-elisp
 init-html
 init-js
 init-json
 init-markdown
 init-golang
 init-python
 init-clang
 init-org
 init-bash
 init-nasm
 )

;; multi-term -- 方便打开命令行
(use-package multi-term
  :config
  (setq multi-term-program "/bin/bash")

  ;; 将新建命令行窗口放在垂直方向
  (advice-add 'multi-term :after
              (lambda(&rest _r)
                (split-window-below)
                (windmove-up)))

  :bind (("M-!" . multi-term)))

;; (defun claudio/prog-reload-script()
;;   "重新加载当前文件。并保留光标位置。改变权限为775
;; 主要作用为重新加载bash文件，使用shell-script-mode生效"
;;   (interactive)
;;   (let ((file buffer-file-name)			;buff对应的文件
;; 		(pos (point))					;当前位置
;; 		(perm 755))                     ;权限
;;     (save-buffer)						;先保持文件
;; 	(kill-buffer (current-buffer))
;;     (find-file file)
;;     (goto-char pos)
;;     (shell-command (format "chmod %d %s" perm file))))

;; 保存后，如果为可执行脚本文件，修改其文件属性，使可执行
(add-hook 'prog-mode-hook
          (lambda()
            "使脚本可执行."
            (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p
                      nil t)))

(add-hook 'prog-mode-hook 'subword-mode)

(defun claudio/prog-run-current-file()
  "执行当前脚本。"
  (interactive)
  (let* ((suffix-map '(("php" . "php")
                       ("pl" . "perl")
                       ("go" . "go run")
                       ("py" . "python3.5")
                       ("sh" . "bash")))
         (file-name (buffer-file-name))
         file-suffix prog-name cmd-str)

    ;; 不存在文件
    (unless file-name
      (when (y-or-n-p "保存当前buffer? ")
        (setq file-name (buffer-file-name))))

    ;; 通过文件名构建命令
    (when file-name
      (setq file-suffix (file-name-extension file-name)
            prog-name (cdr (assoc file-suffix suffix-map))
            cmd-str (concat prog-name " '" file-name "'")))

    (message cmd-str)
    ;; 根据不同条件执行
    (cond ((not file-name) (message "文件不存在！"))
          ((string-equal file-suffix "el") ; special case for emacs lisp
           (load (file-name-sans-extension file-name)))
          (prog-name
           (when (buffer-modified-p) (save-buffer))
           (async-shell-command cmd-str (format "*%s.%s脚本运行结果*"  (file-name-base file-name) file-suffix)))
          (t (message "不支持 .%s 文件运行！" file-suffix)))))

(bind-keys ("C-c R" . claudio/prog-run-current-file))

(provide 'init-prog)
;;; init-prog.el ends here
