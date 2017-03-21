;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "clang-format/clang-format" "clang-format/clang-format.el"
;;;;;;  (22736 48541 250033 422000))
;;; Generated autoloads from clang-format/clang-format.el

(autoload 'clang-format-region "clang-format/clang-format" "\
Use clang-format to format the code between START and END according to STYLE.
If called interactively uses the region or the current statement if there
is no active region.  If no style is given uses `clang-format-style'.

\(fn START END &optional STYLE)" t nil)

(autoload 'clang-format-buffer "clang-format/clang-format" "\
Use clang-format to format the current buffer according to STYLE.

\(fn &optional STYLE)" t nil)

(defalias 'clang-format 'clang-format-region)

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
