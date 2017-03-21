((clang-forma status "required" recipe nil)
 (clang-format status "installed" recipe
               (:name clang-format :type github :pkgname "nothingthere/clang-format" :after
                      (progn
                        (el-get-bundle-load-init "/home/claudio/.emacs.d/el-get/bundle-init/_home_claudio_.emacs.d_lisp_init-prog_init-clang-1_clang-format.el"))))
 (el-get status "required"))
