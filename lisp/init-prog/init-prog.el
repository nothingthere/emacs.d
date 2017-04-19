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

(provide 'init-prog)
;;; init-prog.el ends here
