;; -*- coding: utf-8 -*-

(defun my/prog/init/add-hook (func)
  (my/add-mode-hook "my/prog/init" func))

(defun my/prog/init/run-hook ()
  (my/run-mode-hook "my/prog/init"))

(my/add-mode-hook "prog" #'my/prog/init/run-hook)


(use-package which-func
  :after (prog-mode)
  :if (pkg/package/enabled-p 'which-func)
  :init
  (setq which-func-unknown "∅")
  :config
  (setq which-func-modes '(lisp-mode
                           emacs-lisp-mode
                           lisp-interaction-mode))
  (which-function-mode 1))

(use-package prog-mode
  :defer t
  :preface
  (defun pkg/prog-mode/start ()
    (setq indent-tabs-mode nil)
    (linum-mode -1)
    (prettify-symbols-mode 1)
    (add-to-list 'prettify-symbols-alist '("lambda" . ?λ)))
  :init
  (my/prog/init/add-hook #'pkg/prog-mode/start)
  :config
  (setq prettify-symbols-unprettify-at-point t))

(use-package flymake
  :defer t
  :preface
  (defun pkg/flymake/start ()
    (flymake-mode 1))
  :if (pkg/package/enabled-p 'flymake)
  :init
  (my/prog/init/add-hook #'pkg/flymake/start))

(use-package flycheck
  :diminish flycheck-mode
  :defer t
  :preface
  (defun pkg/flycheck/start ()
    (flycheck-mode 1))
  (defun pkg/flycheck/enable-checker-i ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'flycheck-disable-checker)))
  (defun pkg/flycheck/checker-enabled-p (chk)
    (and (memq chk flycheck-checkers)          ;; global variable
         (not (memq chk flycheck-disabled-checkers)))) ;; buffer-local variable
  (defun pkg/flycheck/disable-checker (chk)
    (add-to-list 'flycheck-disabled-checkers chk))
  :if (pkg/package/enabled-p 'flycheck)
  :init
  (my/add-mode-hooks '(("elisp" pkg/flycheck/elisp-mode-hook)
                       ("c" pkg/flycheck/c&c++-mode-hook)
                       ("c++" pkg/flycheck/c&c++-mode-hook)
                       ("python" pkg/flycheck/python-mode-hook)
                       ("haskell" pkg/flycheck/haskell-mode-hook)))
  (my/prog/init/add-hook #'pkg/flycheck/start)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
        flycheck-checker-error-threshold 500
        flycheck-idle-change-delay 2.5
        flycheck-indication-mode 'left-fringe)
  (flycheck-error-list-set-filter 'error)
  ;; (flycheck-list-errors)可以列出当前buffer中的所有error，优化显示窗口
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.33)))
  (defun pkg/flycheck/elisp-mode-hook ()
    (when (pkg/flycheck/checker-enabled-p 'emacs-lisp)
      (pkg/flycheck/disable-checker 'emacs-lisp-checkdoc)
      (setq flycheck-emacs-lisp-load-path 'inherit)))
  (defun pkg/flycheck/c&c++-mode-hook ()
    (when (pkg/flycheck/checker-enabled-p 'c/c++-gcc)
      ;; (setq flycheck-gcc-language-standard "c++11") ;; 交由cpputils-cmake插件设置
      ))
  (defun pkg/flycheck/python-mode-hook ()
    (when (or (pkg/flycheck/checker-enabled-p 'python-pyflakes)
              (pkg/flycheck/checker-enabled-p 'python-pycheckers))
      (pkg/flycheck/disable-checker 'python-pylint)
      (pkg/flycheck/disable-checker 'python-flake8))
    (when (pkg/flycheck/checker-enabled-p 'python-flake8)
      (add-to-list 'flycheck-flake8-error-level-alist '("^E305$" . info) t)))
  (defun pkg/flycheck/haskell-mode-hook ()
    (when (and (pkg/flycheck/checker-enabled-p 'haskell-hlint)
               (my/locate-exec "hlint"))
      ;; 'flycheck-haskell-stack-ghc-executable
      ;; 'flycheck-haskell-ghc-executable
      ;; 'flycheck-haskell-hlint-executable
      (pkg/flycheck/disable-checker 'haskell-stack-ghc)
      (pkg/flycheck/disable-checker 'haskell-ghc)))
  (use-package helm-flycheck
    :after (helm)
    :commands (helm-flycheck)
    :if (pkg/package/enabled-p '(helm helm-flycheck))))

(use-package ggtags
  :diminish ggtags-mode
  :defer t
  :preface
  (defun pkg/ggtags/start ()
    (ggtags-mode 1))
  :if (and (pkg/package/enabled-p 'ggtags)
           (my/locate-exec "gtags"))
  :init
  (dolist (mode '("c" "c++"))
    (my/add-mode-hook mode #'pkg/ggtags/start))
  :config
  (setq ggtags-use-idutils t
        ggtags-oversize-limit (* 100 1024 1024)
        ggtags-mode-line-project-name nil
        ggtags-sort-by-nearness t
        ggtags-mode-prefix-key (kbd "C-c g")))

(use-package helm-gtags
  :after (helm)
  :defer t
  :preface
  (defun pkg/helm-gtags/start ()
    (helm-gtags-mode 1))
  :if (and (pkg/package/enabled-p '(helm helm-gtags))
           (my/locate-exec "gtags"))
  :init
  (dolist (mode '("c" "c++"))
    (my/add-mode-hook mode #'pkg/helm-gtags/start))
  :config
  (setq helm-gtags-path-style 'root ;; 'relative, 'absolute
        helm-gtags-ignore-case t
        helm-gtags-read-only t
        helm-gtags-use-input-at-cursor t
        helm-gtags-highlight-candidate t
        helm-gtags-maximum-candidates 1000
        helm-gtags-display-style nil ;; 'detail
        helm-gtags-fuzzy-match nil
        helm-gtags-direct-helm-completing nil
        helm-gtags-auto-update t
        helm-gtags-update-interval-second 60
        helm-gtags-pulse-at-cursor t
        helm-gtags-cache-select-result t
        helm-gtags-cache-max-result-size (* 100 1024 1024)
        helm-gtags-preselect nil
        helm-gtags-prefix-key (kbd "C-c g")
        ;; 启用以下配置项会使得某些常用快捷键不再绑定于上述前缀中
        ;; 例如将(helm-gtags-dwim)绑定于"M-."
        helm-gtags-suggested-key-mapping nil))

(use-package asn1-mode
  :defer t
  :if (pkg/package/enabled-p 'asn1-mode))

(use-package reformatter
  :if (pkg/package/enabled-p 'reformatter)
  :config
  (when-let ((exec (my/locate-exec "uncrustify" "/usr/local/bin/"))
             (cfg (my/get-user-emacs-file "my.config/uncrustify.c.cfg")))
    (defconst pkg-reformatter-c-program exec)
    (defconst pkg-reformatter-c-args `("-l" "C" "-c" ,cfg "--no-backup"))
    (reformatter-define pkg-reformatter-c
      :program pkg-reformatter-c-program
      :args pkg-reformatter-c-args)
    (my/add-mode-hook "c" #'pkg-reformatter-c-on-save-mode))
  (when-let ((exec (my/locate-exec "autopep8")) ;; "yapf"
             (cfg (my/get-user-emacs-file "my.config/pycodestyle.cfg")))
    (defconst pkg-reformatter-python-program exec)
    (defconst pkg-reformatter-python-args `(,(concat "--global-config=" cfg)
                                            ,(concat "--max-line-length="
                                                     (number-to-string fill-column))
                                            "-"))
    (reformatter-define pkg-reformatter-python
      :program pkg-reformatter-python-program
      :args pkg-reformatter-python-args)
    (my/add-mode-hook "python" #'pkg-reformatter-python-on-save-mode))
  (when-let ((exec (my/locate-exec "stylish-haskell")) ;; hindent
             (cfg (my/get-user-emacs-file "my.config/stylish-haskell.yaml")))
    (defconst pkg-reformatter-haskell-program exec)
    (defconst pkg-reformatter-haskell-args `("-c" ,cfg "-"))
    (reformatter-define pkg-reformatter-haskell
      :program pkg-reformatter-haskell-program
      :args pkg-reformatter-haskell-args)
    (my/add-mode-hook "haskell" #'pkg-reformatter-haskell-on-save-mode)))

(defun my/reformat-current-file ()
  (interactive)
  (defmacro my/reformat-wrapper (func)
    `(let ((state buffer-read-only))
       (when state (read-only-mode -1))
       (delete-trailing-whitespace)
       (when (< (- (point-max) (point-min)) (* 1024 1024))
         ,func)
       ;; 每次保存buffer时都将删除现有的改动高亮，替换成以下两个hook无法生效，原因未知
       ;; write-content-functions或write-file-functions
       (highlight-changes-remove-highlight (point-min) (point-max))
       (when state (read-only-mode 1))))
  (my/reformat-wrapper
   (let* ((file (my/locate-file buffer-file-name))
          (mode (when file (assoc-default file auto-mode-alist
                                          'string-match))))
     (cond
      ((and (provided-mode-derived-p mode 'lisp-mode 'emacs-lisp-mode)
            (derived-mode-p 'lisp-mode 'emacs-lisp-mode))
       (indent-region (point-min) (point-max)))))))

(add-hook 'before-save-hook 'my/reformat-current-file t)


(provide 'my/prog/init)
