;; -*- coding: utf-8 -*-

(require 'my-prog)

(defvar my-prog-hs-mode-start-hook '())

;; =============================================================================
;; haskell-mode
(defun my-plugin-haskell-mode-init ()
  (use-package haskell-mode
    :if (my-func-package-enabled-p 'haskell)
    :init
    (setq haskell-stylish-on-save t)
    :config
    (when (require 'haskell-mode-autoloads nil t)
      ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
      )))

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

(provide 'my-prog-haskell)
