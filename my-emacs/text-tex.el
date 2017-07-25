;; -*- coding: utf-8 -*-

(require 'my-init)

(defvar my-text-tex-mode-start-hook '())
(defvar my-text-tex-mode-start-mode 'tex-mode)

;; =============================================================================
;; TeXlive ::
;; RefTeX :: Emacs自带，用于管理资源的引用
;; AUCTeX :: 综合性的插件集，包括了如下组件
;; preview-latex :: 预览
;; pdflatex :: tex文件可转换成pdf格式输出
;; dvips-ps2pdf :: tex编译后变成dvi，再转换成pdf格式
(defun my-plugin-auctex-init ()
  ;; 该插件在安装后就会随(package-initialize)的执行而被自动加载
  ;; 而无需且不能以以下方式被重新加载：(load "auctex.el")
  (use-package tex
    :if (my-func-package-enabled-p 'auctex)
    :init
    (add-hook 'my-text-tex-mode-start-hook 'my-plugin-auctex-start t)
    :config
    (setq my-text-tex-mode-start-mode 'TeX-mode)
    (when (eq system-type 'windows-nt)
      (when (my-func-executable-find "miktex-texworks.exe"
                                     "MiKTeX/miktex/bin/x64")
        (require 'tex-mik nil t)))
    (setq TeX-engine 'xetex
          TeX-master nil
          TeX-auto-save t
          TeX-parse-self t
          TeX-PDF-from-DVI nil
          ;; edit
          TeX-electric-math (cons "$" "$")
          TeX-electric-sub-and-superscript t
          TeX-arg-right-insert-p t
          LaTeX-electric-left-right-brace t
          LaTeX-math-abbrev-prefix "`"
          ;; indent
          TeX-brace-indent-level 2
          LaTeX-indent-level 2
          LaTeX-item-indent -2)
    (setq-default TeX-engine TeX-engine
                  TeX-master TeX-master
                  TeX-PDF-from-DVI TeX-PDF-from-DVI)
    (use-package latex
      :defer t
      :config
      (bind-keys :map LaTeX-mode-map
                 ;; output
                 ("C-c C-c" . TeX-command-master)
                 ("C-c C-a" . TeX-command-run-all)
                 ("C-c C-t C-p" . TeX-PDF-mode)
                 ("C-c C-t C-i" . TeX-interactive-mode)
                 ;; edit
                 ("C-c C-f" . TeX-font) ;; C-b, C-i, C-e, C-s
                 ("C-c C-s" . LaTeX-section)
                 ("C-c C-e" . LaTeX-environment)
                 ("C-c ]" . LaTeX-close-environment)
                 ("C-M-a" . LaTeX-find-matching-begin)
                 ("C-M-e" . LaTeX-find-matching-end))
      (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill t)
      (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode t))))

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
