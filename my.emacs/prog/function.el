;; -*- coding: utf-8 -*-

(use-package sml-mode
  :defer t
  :if (pkg/package/enabled-p 'sml-mode)
  :config
  (setq sml-indent-level 2
        sml-program-name (or (my/locate-exec "sml")
                             (my/locate-exec "sml" "/usr/share/smlnj/bin"))
        sml-config-file (my/get-user-emacs-file "my.config/smlconfig.sml")
        sml-max-name-components 3))


(use-package haskell-mode
  :defer t
  :preface
  (defun pkg/haskell-mode/start ()
    (setq haskell-indentation-electric-flag t)
    (subword-mode 1)
    (when (my/minor-mode-on-p highlight-thing-mode)
      ;; (highlight-uses-mode)
      (highlight-thing-mode -1)))
  :if (pkg/package/enabled-p 'haskell-mode)
  :config
  (setq haskell-process-type 'stack-ghci ;; rely entirely on Stack
        haskell-process-path-ghci "ghci"
        haskell-process-path-cabal "cabal"
        haskell-process-path-stack "stack"
        haskell-process-suggest-remove-import-lines t
        haskell-process-suggest-hoogle-imports t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t ;; *haskell-process-log* buffer
        haskell-stylish-on-save nil ;; use 'reformatter instead
        haskell-mode-stylish-haskell-path (my/locate-exec "stylish-haskell")
        haskell-tags-on-save t
        haskell-hasktags-path (my/locate-exec "hasktags")
        haskell-interactive-types-for-show-ambiguous nil
        ;; (haskell-compile)执行的是以下三个命令之一，前两者都用于Cabal所管理的项目
        ;; 目前暂没有发现更多的配置选项，以支持定制Stack环境内的GHC
        ;; 因此采用以下临时性方案，建议多使用(haskell-process-*)相关命令
        ;; haskell-compile-cabal-build-command
        ;; haskell-compile-cabal-build-alt-command
        haskell-compile-command "stack ghc %s")

  (my/add-mode-hook "haskell" #'haskell-indentation-mode) ;; (haskell-indent-mode)
  (my/add-mode-hook "haskell" #'interactive-haskell-mode)
  (my/add-mode-hook "haskell" #'haskell-doc-mode)
  (my/add-mode-hook "haskell" #'highlight-uses-mode)
  (my/add-mode-hook "haskell" #'pkg/haskell-mode/start)

  (bind-keys :map haskell-mode-map
             ("M-." . haskell-mode-jump-to-def-or-tag) ;; (haskell-mode-tag-find)
             ("C-c C-c" . haskell-compile)
             ;; ("" . haskell-interactive-switch)
             ("C-c C-l" . haskell-process-load-file) ;; (haskell-process-load-or-reload)
             ("C-c C-r" . haskell-process-restart)   ;; (haskell-process-clear)
             ("" . haskell-process-do-type)
             ("" . haskell-process-do-info)
             ("" . haskell-navigate-imports)
             ("" . haskell-mode-format-imports)
             ("" . haskell-sort-imports)
             ("" . haskell-align-imports)
             ("" . haskell-process-cabal)
             ("" . haskell-process-cabal-build)
             :map haskell-cabal-mode-map
             :map highlight-uses-mode-map))

(use-package flycheck-haskell
  :after (haskell-mode flycheck)
  :defer t
  :preface
  (defun pkg/flycheck-haskell/start ()
    (flycheck-haskell-setup))
  :if (pkg/package/enabled-p '(haskell-mode flycheck-haskell))
  :init
  (my/add-mode-hook "haskell" #'pkg/flycheck-haskell/start))


(provide 'my/prog/function)
