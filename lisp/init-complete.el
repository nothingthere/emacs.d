;;; init-complete.el --- 自动补全配置
;; Author: Claudio <3261958605@qq.com.com>
;; Created: 2017-05-06 10:42:58
;;; Commentary:
;;; Code:
;; company -- 自动补全插件
(use-package company
  :demand t
  :bind
  (:map company-active-map
        ("M-p" . nil)
        ("M-n" . nil)
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next))

  :config
  ;; (diminish 'company-mode "CMP")
  (add-hook 'after-init-hook 'global-company-mode)
  (setq-default
   ;; 等待时间"秒"
   company-idle-delay 0.1
   ;; 输入多少个字符时激活
   company-minimum-prefix-length 1
   company-dabbrev-ignore-case t
   )

  ;; !!! 在图像界面下才能使用
  ;; company-quickhelp -- 代码提示功能
  (use-package company-quickhelp
    :if (display-graphic-p)
    :config
    (bind-key "M-h" 'company-quickhelp-manual-begin company-active-map
              (featurep 'company)))

  ;; yasnippet -- snippets片段补全
  (use-package yasnippet
    :demand t
    :bind
    (:map yas-minor-mode-map
          ;; 禁用yansnippets默认键
          ("<tab>" . nil)
          ("TAB" . nil))
    :config
    (yas-global-mode 1)
    (setq yas-fallback-behavior nil)

    ;; 实在不想折腾company完美补全，差不都照抄了purcell的配置
    ;; https://github.com/nothingthere/emacs.d-1/blob/master/lisp/init-company.el

    (setq-default company-backends
                  '((company-capf :with company-yasnippet)
                    company-dabbrev-code
                    company-dabbrev
                    )
                  ;; 只在相同major-mode下搜索
                  company-dabbrev-other-buffers t)

    (defun claudio/company-push-local-backend(backend)
      "将BACKEND添加到buffer-local的company-backends中."
      (set (make-local-variable 'company-backends)
           (append (list backend) company-backends)))

    (defun claudio/yas-insert-init-snippet()
      "当新建文件时，如果没有任何内容，且可找到\"init:文件初始化\"的snippet，自动插入。"
      (let* ((base "init:文件初始化")
             snippet)

        (when base
          (setq snippet (yas-lookup-snippet base major-mode t)))

        (when (and
               (equal (buffer-size) 0)
               snippet)
          (yas-expand-snippet snippet)
          (message "自动添加文件初始结构完成。"))))

    (add-hook 'find-file-hook #'claudio/yas-insert-init-snippet t)

    )

  ;; end yasnippet
  ;; end company
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 在不输入./时，也能自动补全当前文件夹中的文件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company-files)                ;使用company-files内部的私有函数
(defun claudio/company-files-without-prefix-backend (command &optional arg &rest ignored)
  "自动补全当前文件夹中的文件，不输入./也可自动补全 COMMAND ARG IGNORED用法不详.
如果编写自己的backend教程：
http://sixty-north.com/blog/writing-the-simplest-emacs-company-mode-backend"
  (interactive (list 'interactive))
  ;; 获取当前文件夹中所有文件
  (let* ((dir-prefix "./")
         (candiates-files (company-files--complete dir-prefix)))
    (cl-case command
      (interactive (company-begin-backend #'claudio/company-files-without-prefix-backend))
      ;; 对当前输入内容过滤，如果当前文件夹中有文件名以当前输入内容开始

      ;; 则返回：当前输入内容，作为需candidates作为补全的判断
      ;; 不能返回"./当前内容"，参考company--insert-candidate发现：
      ;; 这样做为删除一些字符（具体原因还没明白）

      ;; 否则返回nil，以便将机会让给别的backend，返回值作为arg可在candidates中使用
      (prefix
       (let ((current-symbol (company-grab-symbol)))
         ;; (message "grab-symbol： %s" current-symbol)
         (if (or (zerop (length (remove-if-not
                                 (lambda(file-name)
                                   (string-prefix-p (concat dir-prefix current-symbol) file-name))
                                 candiates-files)))
                 (null current-symbol))
             nil
           current-symbol)))
      ;;获取备选文件名，去除所有备选中前面的：./
      (candidates
       ;; (message "company-prefix：%s" company-prefix)
       (mapcar (lambda(file)
                 (substring file 2))
               (remove-if-not
                (lambda(file-name)
                  (string-prefix-p (concat dir-prefix arg) file-name))
                candiates-files)))
      (sorted t)
      (no-cache t)
      )))

(provide 'init-complete)
;;; init-complete.el ends here
