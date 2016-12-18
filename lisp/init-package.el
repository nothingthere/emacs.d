;;;;;;;;;;;;;;;;;;;;;;;;;;;插件初始化
;;添加MELPA源启用安装包
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(defvar *my-packges* '(


		       magit;;emacs的github
		       session;;保存上次会话
		       emmet-mode
		       )
  "所有自己需安装的包")

;;保存上次会话
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)

;;emmet
(require 'emmet-mode)
;;;;;;;;;;;
(provide 'init-package)
