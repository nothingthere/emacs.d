;;;for melap
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)


;;;for quicklisp sbcl
(require 'slim-mode)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;;;;;for ac-slime
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;;;;;;;;

;;start auto complete
;;(require 'auto-complete-mode)
(add-to-list 'ac-dictionary-directories 
	     "~/.emacs.d/elpa/auto-complete-20150618.1949/dict")
(require 'auto-complete-config)
(ac-config-default)
;;;
(defvar ac-slime-modes
  '(lisp-mode))

(defun ac-slime-candidates ()
  "Complete candidates of the symbol at point."
  (if (memq major-mode ac-slime-modes)
      (let* ((end (point))
	          (beg (slime-symbol-start-pos))
		       (prefix (buffer-substring-no-properties beg end))
		            (result (slime-simple-completions prefix)))
	(destructuring-bind (completions partial) result
	  completions))))

(defvar ac-source-slime
  '((candidates . ac-slime-candidates)
    (requires-num . 3)))

(add-hook 'lisp-mode-hook (lambda ()
			    (slime-mode t)
			    (push 'ac-source-slime ac-sources)
			    (auto-complete-mode)))



;;;for php
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


(defun clean-php-mode ()
  (interactive)
  (php-mode)
  (setq c-basic-offset 2) ; 2 tabs indenting
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 'c-lineup-arglist-operators))
(c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
(c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values



(add-to-list 'auto-mode-alist '("/drupal.*\.\(php\|module\|inc\|test\|install\)$" . php-mode))
(add-to-list 'auto-mode-alist '("/drupal.*\.info" . conf-windows-mode))


;; run php lint when press f8 key
;; php lint
(defun phplint-thisfile ()
  (interactive)
  (compile (format "php -l %s" (buffer-file-name))))
(add-hook 'php-mode-hook
	  '(lambda ()
	     (local-set-key [f8] 'phplint-thisfile)))
;; end of php lint



;;--------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;javascript
;;;;;;ac-js2
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;;js-comint for nodejs
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))
;;flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)
