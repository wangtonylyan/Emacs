(require 'my-prog)

;===========================================================================
;===========================================================================
(defun my-plugin-haskell-mode-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "haskell-mode"))
  (when (require 'haskell-mode)
    (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
    (add-to-list 'auto-mode-alist '("\\.lhs\\'" . literate-haskell-mode))
;    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    ))
(defun my-plugin-haskell-mode-start ()
  )

;===========================================================================
;===========================================================================
(defun my-haskell-mode-init ()
  (my-plugin-haskell-mode-init)
  )
(defun my-haskell-mode-start ()
  (my-plugin-haskell-mode-start)
  )

(eval-after-load 'prog-mode
  '(progn
     (my-haskell-mode-init)
     (add-hook 'prog-mode-hook 'my-haskell-mode-start)
     ))

(provide 'my-prog-haskell)
