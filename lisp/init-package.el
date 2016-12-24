;;;;;;;;;;;;;;;;;;;;;;;;;;;插件初始化
;;添加MELPA源启用安装包
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;;;;;;;;;
(provide 'init-package)
