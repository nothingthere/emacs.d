;;; init-edit-help.el ---  编辑体验提升配置
;; Author:Claudio <3261958605@qq.com>
;; Created: 2017
;;; Commentary:
;;; Code:

;;recentf --启用保存最近打开文档，下次打开时可快速打开
(use-package recentf
  :config
  (setq-default recentf-max-saved-items 1000))

;; swiper --- 3件套：swiper ivy counsel
(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package ivy
  :config
  (ivy-mode 1)
  ;; 使用recentf和bookmarks中的内容作为备选
  (setq ivy-use-virtual-buffers t)
  )

(use-package counsel
  :config
  :bind
  (("C-c g" . counsel-git)
   ("M-s i" . counsel-imenu)
   ;; 与此快捷键的默认相比添加了^通配符
   ("M-x" . counsel-M-x)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-x C-f" . counsel-find-file)))

;; smartparens -- 自动补全括号
(use-package smartparens
  :demand t
  :config
  ;; (smartparens-global-strict-mode t)
  (smartparens-global-mode t)

  ;; elisp 和 common-lisp 中不自动补全单引号、反引号和括号
  (dolist (X '("'" "`" "("))
    (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) X nil
                   :actions nil))

  ;; org文件中新增配对
  (dolist (match '(("“" . "”")
                   ("《" . "》")
                   ("（" . "）")
                   ("'" . "'")
                   ;; ("<" . ">")
                   ;; ("*" . "*")
                   ;; ("/" . "/")
                   ))
    (sp-local-pair '(org-mode python-mode)
                   (car match)
                   (cdr match)))
  )

;; popwin -- 使光标跳转到帮助窗口
(use-package popwin
  :demand t
  :config
  (popwin-mode 1))

;; expand-region -- 方便选中文本
(use-package expand-region
  :bind ("C-c =" . er/expand-region))

;; multiple-cursors -- 多行编辑
(use-package multiple-cursors
  :disabled t
  :bind
  (("C-c ;" . mc/mark-all-dwim)
   ("C-c d" . mc/mark-next-like-this)
   ("C-c D" . mc/skip-to-next-like-this)))

;; iedit -- 多行编辑
(use-package iedit
  :bind
  (("C-c ;" . iedit-mode)))

;; helm-ag -- 项目内快速搜索
;; 需先在系统中安装silversearch-ag
;; ubuntu安装方法为：sudo apt install silversearch-ag
;; 有点奇怪：安装后使用明为"ag"

(use-package helm-ag
  :init
  (claudio/app-may-tobe-installed "silversearcher-ag")
  :bind
  ("C-x f" . helm-ag-project-root))

;; avy -- 根据字符跳转??还没搞清有啥子用
(use-package avy
  :disabled t
  :bind
  ("C-c :" . avy-goto-char-2))

;; which-key -- 当按下快捷键忘记后面键位时，停留后显示提示
(use-package which-key
  :config
  (which-key-mode))

;; origami -- 文本折叠
(use-package origami
  :init
  ;; 两个依赖包5
  (use-package s)
  (use-package dash)
  :config
  (defun claudio/origami-elisp-parser (create)
    "修改自插件源码，只是添加了关键字'use-package bind-keys cl-def'作为块头"
    (origami-lisp-parser create
                         "(\\(def\\|cl-def\\|use-package\\|bind-keys\\)\\w*\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*(.*?)\\)?"))

  ;; 激活自定义parser函数
  (setq origami-parser-alist
        (cons '(emacs-lisp-mode . claudio/origami-elisp-parser)
              (assq-delete-all 'emacs-lisp-mode origami-parser-alist)))

  ;; 全局生效，官方文档说不支持的语言自动使用缩进确定折叠
  (global-origami-mode)

  :bind (("C-c f" . origami-recursively-toggle-node)
         ("C-c F" . origami-toggle-all-nodes))
  )

;; string-inflection -- 下划线、横线、Camel转换
(use-package string-inflection
  :bind (("C-c C" . string-inflection-camelcase)
         ("C-c L" . string-inflection-lower-camelcase)))

;; comment-dwim-2 -- 注释
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; 注释
(defun claudio/edit-help-comment()
  "修改后的注释函数。
1. 如果有选中区域，注释/去注释该区域
2. 如果为空行，仅注释
3n. 如果在代码行末，注释并缩进

对于有些没后缀的文件，如.bashrc也需注释，所以对所有文件都应用。

缺点：不能通过快捷键去注释行末注释"
  (interactive)
  ;; 如果在空行上。不能理解comment-dwim的参数该怎样设置
  (cond ((claudio/util-current-line-empty-p)
         (comment-dwim nil))
        ((claudio/util-at-end-of-line-p)
         (comment-indent))
        ;; 如果在行末，前面有内容
        ;; 默认注释选中区域和本行
        (t
         (let* ((region (claudio/util-get-region-or-get-the-line-as-region))
                (start (car region))
                (end (cdr region)))
           (comment-or-uncomment-region start end)))))

;;;;;;方便编辑的快捷键
(bind-keys ("C-c TAB" . hippie-expand)
           ("M-s o" .
            (lambda()
              ()
              "提升occur-mode性能，默认备选为选择/光标处单词"
              (interactive)
              (push
               (if(region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning)
                    (region-end))
                 (let((sym (thing-at-point 'symbol)))
                   (if (stringp sym)
                       (regexp-quote sym)
                     sym)))
               regexp-history)
              (call-interactively 'occur)))
           )

(provide 'init-edit-help)
;;; init-edit-help.el ends here
