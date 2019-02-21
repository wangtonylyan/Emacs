;; -*- coding: utf-8 -*-

(defun my/prog/add-start-hook (func)
  (my/add-mode-hook "my/prog" func))

(defun my/prog/run-start-hook ()
  (my/run-mode-hook "my/prog"))

(defun my/prog/init ()
  (pkg/prog-mode/init)
  (pkg/flymake/init)
  (pkg/flycheck/init)
  (pkg/ggtags/init)
  (pkg/helm-gtags/init)
  (pkg/asn1-mode/init)
  (my/add-mode-hook "prog" #'my/prog/run-start-hook))

(my/add-mode-hook "init" #'my/prog/init)


(defun pkg/prog-mode/init ()
  (use-package prog-mode
    :defer t
    :preface
    (defun pkg/prog-mode/start ()
      (linum-mode -1)
      (prettify-symbols-mode 1))
    :init
    (setq prettify-symbols-unprettify-at-point t)
    (my/prog/add-start-hook #'pkg/prog-mode/start)
    :config
    ;; 在mode-line显示当前光标所在的函数名
    (which-function-mode 1)
    ;; show "lambda" as "λ"
    (add-to-list 'prettify-symbols-alist '("lambda" . 955))))

(defun pkg/flymake/init ()
  (use-package flymake
    :defer t
    :preface
    (defun pkg/flymake/start ()
      (flymake-mode 1))
    :if (my/package-enabled-p 'flymake)
    :init
    (my/prog/add-start-hook #'pkg/flymake/start)))

(defun pkg/flycheck/init ()
  (use-package flycheck
    :diminish flycheck-mode
    :defer t
    :preface
    (defun pkg/flycheck/start ()
      (flycheck-mode 1))
    (defun pkg/flycheck/enable-checker ()
      (interactive)
      (let ((current-prefix-arg t))
        (call-interactively #'flycheck-disable-checker)))
    (defun pkg/flycheck/checker-enabled-p (chk)
      (and (memq chk flycheck-checkers) ;; global variable
           (not (memq chk flycheck-disabled-checkers)))) ;; buffer-local variable
    :if (my/package-enabled-p 'flycheck)
    :init
    (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
          flycheck-checker-error-threshold 500
          flycheck-idle-change-delay 2.5
          flycheck-indication-mode 'left-fringe)
    (my/prog/add-start-hook #'pkg/flycheck/start)
    :config
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
        (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
        (setq flycheck-emacs-lisp-load-path 'inherit)))
    (defun pkg/flycheck/c&c++-mode-hook ()
      (when (pkg/flycheck/checker-enabled-p 'c/c++-gcc)
        ;; (setq flycheck-gcc-language-standard "c++11") ;; 由cpputils-cmake插件设置
        ))
    (use-package flycheck-pyflakes
      :after (python-mode)
      :if (and (my/package-enabled-p 'flycheck-pyflakes)
               (my/locate-exec "pyflakes")))
    (defun pkg/flycheck/python-mode-hook ()
      (when (pkg/flycheck/checker-enabled-p 'flycheck-pyflakes)
        (add-to-list 'flycheck-disabled-checkers 'python-flake8)
        (add-to-list 'flycheck-disabled-checkers 'python-pylint))
      (when (pkg/flycheck/checker-enabled-p 'python-flake8)
        (add-to-list 'flycheck-flake8-error-level-alist '("^E305$" . info) t)))
    (use-package flycheck-haskell
      :after (haskell-mode)
      :if (my/package-enabled-p 'flycheck-haskell)
      :init
      (my/add-mode-hook "haskell" #'flycheck-haskell-setup))
    (defun pkg/flycheck/haskell-mode-hook ()
      (when (and (pkg/flycheck/checker-enabled-p 'haskell-hlint)
                 (my/locate-exec "hlint"))
        ;; 'flycheck-haskell-stack-ghc-executable
        ;; 'flycheck-haskell-ghc-executable
        ;; 'flycheck-haskell-hlint-executable
        (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
        (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)))
    (my/add-modes-hook '(("elisp" pkg/flycheck/elisp-mode-hook)
                         ("c" pkg/flycheck/c&c++-mode-hook)
                         ("c++" pkg/flycheck/c&c++-mode-hook)
                         ("python" pkg/flycheck/python-mode-hook)
                         ("haskell" pkg/flycheck/haskell-mode-hook)))
    (use-package helm-flycheck
      :after (helm)
      :commands (helm-flycheck)
      :if (and (my/package-enabled-p 'helm)
               (my/package-enabled-p 'helm-flycheck)))))

(defun pkg/ggtags/init ()
  (use-package ggtags
    :diminish ggtags-mode
    :defer t
    :preface
    (defun pkg/ggtags/start ()
      (ggtags-mode 1))
    :if (and (my/package-enabled-p 'ggtags)
             (my/locate-exec "gtags"))
    :init
    (setq ggtags-use-idutils t
          ggtags-oversize-limit (* 100 1024 1024)
          ggtags-mode-line-project-name nil
          ggtags-sort-by-nearness t
          ggtags-mode-prefix-key (kbd "C-c g"))
    (dolist (mode '("c" "c++"))
      (my/add-mode-hook mode #'pkg/ggtags/start))))

(defun pkg/helm-gtags/init ()
  (use-package helm-gtags
    :after (helm)
    :defer t
    :preface
    (defun pkg/helm-gtags/start ()
      (helm-gtags-mode 1))
    :if (and (my/package-enabled-p 'helm)
             (my/package-enabled-p 'helm-gtags)
             (my/locate-exec "gtags"))
    :init
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
          helm-gtags-suggested-key-mapping nil)
    (dolist (mode '("c" "c++"))
      (my/add-mode-hook mode #'pkg/helm-gtags/start))))

(defun pkg/asn1-mode/init ()
  (use-package asn1-mode
    :defer t
    :if (my/package-enabled-p 'asn1-mode)))


(provide 'my/prog)
