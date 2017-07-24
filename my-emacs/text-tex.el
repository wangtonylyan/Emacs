;; -*- coding: utf-8 -*-

(require 'my-init)

(defvar my-text-tex-mode-start-hook '())
(defvar my-text-tex-mode-start-mode 'tex-mode)

;; =============================================================================
;; TeXlive ::
;; RefTeX :: Emacs自带，用于管理资源的引用
;; AUCTeX :: 综合性的插件集，包括了如下组件
;; preview-latex :: 预览
(defun my-plugin-auctex-init ()
  ;; 该插件在安装后就会随(package-initialize)的执行而被自动加载
  ;; 而无需且不能以以下方式被重新加载：(load "auctex.el")
  (when (my-func-package-enabled-p 'auctex) ;; (use-package)
    ;; :init
    (add-hook 'my-text-tex-mode-start-hook 'my-plugin-auctex-start t)
    ;; :config
    (when (eq system-type 'windows-nt)
      (when (my-func-executable-find "miktex-texworks.exe"
                                     "MiKTeX/miktex/bin/x64")
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
