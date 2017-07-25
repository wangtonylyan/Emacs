;; -*- coding: utf-8 -*-

(require 'my-prog)

(defvar my-prog-hs-mode-start-hook '())

;; 常用的三大插件：haskell-mode, ghc-mode, structured-haskell-mode

;; =============================================================================
;; haskell-mode
;; 必要的Haskell库：alex, happy
;; 可选的Haskell库：hindent, stylish-haskell, hlint, hasktags, hoogle
;; alex与happy就分别相当于C的lex与yacc
(defun my-plugin-haskell-mode-init ()
  (use-package haskell-mode
    :if (my-func-package-enabled-p 'haskell-mode)
    :init
    (setq haskell-process-type 'auto
          haskell-process-path-ghci "ghci"
          haskell-process-path-cabal "cabal"
          haskell-process-path-stack "stack"
          haskell-process-suggest-remove-import-lines t
          haskell-process-suggest-hoogle-imports (when (my-func-executable-find "hoogle") t)
          haskell-process-auto-import-loaded-modules t
          haskell-process-log t ;; *haskell-process-log* buffer
          haskell-stylish-on-save t
          haskell-mode-stylish-haskell-path "stylish-haskell"
          haskell-tags-on-save t
          haskell-hasktags-path "hasktags"
          haskell-interactive-types-for-show-ambiguous nil
          ;; haskell-compile-command ""
          haskell-compile-cabal-build-command "stack build")
    (add-hook 'my-prog-hs-mode-start-hook 'my-plugin-haskell-mode-start t)
    :config
    (bind-keys :map haskell-mode-map
               ("M-." . haskell-mode-jump-to-def-or-tag) ;; (haskell-mode-tag-find)
               ("C-c C-c" . haskell-compile)
               ;; ("" . haskell-interactive-switch)
               ("C-c C-l" . haskell-process-load-file) ;; (haskell-process-load-or-reload)
               ("" . haskell-process-restart) ;; (haskell-process-clear)
               ("" . haskell-process-do-type)
               ("" . haskell-process-do-info)
               ("" . haskell-navigate-imports)
               ("" . haskell-mode-format-imports)
               ("" . haskell-sort-imports)
               ("" . haskell-align-imports)
               ("" . haskell-process-cabal)
               ("" . haskell-process-cabal-build)
               :map haskell-cabal-mode-map)
    ;; 缩进功能使用hindent插件替代
    ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent) ;; obsoleted
    ;; (add-hook 'haskell-mode-hook 'haskell-indentation-mode) ;; (turn-on-haskell-indentation)
    (use-package hindent
      :if (my-func-package-enabled-p 'hindent)
      :config
      (add-hook 'haskell-mode-hook 'hindent-mode))
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
    ;; (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
    ))

(defun my-plugin-haskell-mode-start ()
  (setq haskell-indentation-electric-flag t))

;; =============================================================================
;; =============================================================================
(defun my-prog-hs-mode-init ()
  (my-plugin-haskell-mode-init)
  (add-hook 'haskell-mode-hook 'my-prog-hs-mode-start t))

(defun my-prog-hs-mode-start ()
  (run-hooks 'my-prog-hs-mode-start-hook))

(eval-after-load 'prog-mode '(my-prog-hs-mode-init))

(provide 'my-prog-hs)
