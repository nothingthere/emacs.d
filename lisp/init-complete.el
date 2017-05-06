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
   company-minimum-prefix-length 1)

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
                  '((company-capf  :with company-yasnippet)
                    company-dabbrev-code
                    company-dabbrev
                    )
                  ;; 只在相同major-mode下搜索
                  company-dabbrev-other-buffers t)

    (defun claudio/company-push-local-backend(backend)
      "将BACKEND添加到buffer-local的company-backends中."
      (set (make-local-variable 'company-backends)
           (append (list backend) company-backends))))

  ;; end yasnippet
  ;; end company
  )

(provide 'init-complete)
;;; init-complete.el ends here
