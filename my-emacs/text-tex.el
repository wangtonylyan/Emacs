(require 'my-init)

(defvar my-text-tex-mode-start-hook '())
(defvar my-text-tex-mode-start-mode 'tex-mode)

;; =============================================================================
(defun my-plugin-auctex-init ()
  ;; 该插件在安装后就会随(package-initialize)的执行而被自动加载
  ;; 而无需且不能以以下方式被重新加载：(load "auctex.el" t)
  (use-package auctex
    :if (my-func-package-enabled-p 'auctex)
    :commands (reftex-mode TeX-fold-mode)
    :init
    (add-hook 'my-text-tex-mode-start-hook 'my-plugin-auctex-start t)
    :config
    (when (eq system-type 'windows-nt)
      (when (my-func-executable-find "MiKTeX/miktex/bin/x64"
                                     "miktex-texworks.exe")
        (require 'tex-mik nil t)))
    (setq TeX-auto-save t
          TeX-parse-self t)
    ;; (setq-default TeX-master nil)
    (setq my-text-tex-mode-start-mode 'TeX-mode)))

(defun my-plugin-auctex-start ()
  (reftex-mode t)
  (TeX-fold-mode t))

;; =============================================================================
(defun my-text-tex-mode-init ()
  (my-plugin-auctex-init)
  (let ((hook (if (eq my-text-tex-mode-start-mode 'TeX-mode)
                  'TeX-mode-hook 'tex-mode-hook)))
    (add-hook hook 'my-text-tex-mode-start t)))

(defun my-text-tex-mode-start ()
  (font-lock-mode 1)
  (linum-mode 1)
  (run-hooks 'my-text-tex-mode-start-hook))

(eval-after-load 'tex '(my-text-tex-mode-init))

(provide 'my-text-tex)
