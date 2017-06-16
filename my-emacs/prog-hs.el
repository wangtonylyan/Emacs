;; -*- coding: utf-8 -*-

(require 'my-prog)

(defvar my-prog-hs-mode-start-hook '())

;; =============================================================================
;; haskell-mode
;; GHC :: Glasgow Haskell Compiler
;; GHCi :: Glasgow Haskell Compiler interactive
;; Cabal :: 类似于Ubuntu apt-get、python pip、Ruby gem

(defun my-plugin-haskell-mode-init ()
  (use-package haskell-mode
    :if (my-func-package-enabled-p 'haskell-mode)
    :init
    (setq haskell-process-path-ghci "ghci"
          haskell-process-path-cabal "cabal"
          ;; haskell-process-path-stack "stack"
          haskell-stylish-on-save t ;; $ cabal install stylish-haskell
          haskell-mode-stylish-haskell-path "~/.cabal/bin/stylish-haskell"
          haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-log t)
    :config
    (setq haskell-indentation-electric-flag t)
    (setq-default haskell-indentation-electric-flag haskell-indentation-electric-flag)
    ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent) ;; obsoleted
    (add-hook 'haskell-mode-hook 'haskell-indentation-mode) ;; (turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
    ;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
    ))

(defun my-plugin-haskell-mode-start ()
  )

;; =============================================================================
;; =============================================================================
(defun my-prog-hs-mode-init ()
  (my-plugin-haskell-mode-init)
  (add-hook 'haskell-mode-hook 'my-prog-hs-mode-start t))

(defun my-prog-hs-mode-start ()
  (run-hooks 'my-prog-hs-mode-start-hook))

(eval-after-load 'prog-mode '(my-prog-hs-mode-init))

(provide 'my-prog-hs)
