;; -*- coding: utf-8 -*-

(use-package whitespace
  :if (my/package-enabled-p 'whitespace)
  :init
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column 80))

(use-package fill-column-indicator
  :if (my/package-enabled-p 'fill-column-indicator)
  :init
  (setq fci-rule-use-dashes nil
        fci-rule-column 100)
  :config
  (define-globalized-minor-mode global-fci-mode fci-mode
    ;; 避免在special buffers、dired、shell等特殊模式下启用
    (lambda () (when buffer-file-name (fci-mode 1))))
  (global-fci-mode 1))

;; 嵌套的括号通过大小而不仅是颜色来进行区分
(use-package rainbow-delimiters
  :if (my/package-enabled-p 'rainbow-delimiters)
  :config
  (my/add-mode-hook "prog" 'rainbow-delimiters-mode))

;; 修改默认字体颜色，从而将文字与符号区分开来
(use-package rainbow-identifiers
  :if (my/package-enabled-p 'rainbow-identifiers)
  :init
  (setq rainbow-identifiers-face-count 1)
  :config
  (my/add-mode-hook "prog" 'rainbow-identifiers-mode))

(use-package highlight-thing
  :diminish highlight-thing-mode
  :if (my/package-enabled-p 'highlight-thing)
  :init
  (setq highlight-thing-what-thing 'symbol
        highlight-thing-exclude-thing-under-point t
        highlight-thing-delay-seconds 0.5
        highlight-thing-limit-to-defun nil
        highlight-thing-case-sensitive-p t)
  :config
  ;; (global-hl-line-mode -1)
  (my/add-mode-hook "text" 'hl-line-mode)
  (my/add-mode-hook "prog" 'highlight-thing-mode))

(use-package avy
  :if (my/package-enabled-p 'avy)
  :bind (("C-:" . avy-goto-char-timer) ;; (avy-goto-char)
         ("C-'" . avy-pop-mark))
  :config
  ;; (avy-setup-default)
  (setq avy-timeout-seconds 1.0))

(use-package ace-jump-mode
  :if (my/package-enabled-p 'ace-jump-mode)
  :bind (("C-:" . ace-jump-mode-pop-mark)
         ("C-'" . ace-jump-char-mode))
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package ace-pinyin
  :preface
  (defvar pkg/ace-pinyin/enabled-p
    (or (my/package-enabled-p 'avy)
        (my/package-enabled-p 'ace-jump-mode)))
  :if (and pkg/ace-pinyin/enabled-p
           (my/package-enabled-p 'ace-pinyin))
  :config
  (when (eq pkg/ace-pinyin/enabled-p 'ace-jump-mode)
    (setq ace-pinyin-use-avy nil))
  (ace-pinyin-global-mode 1))

(use-package undo-tree
  :diminish undo-tree-mode
  :if (my/package-enabled-p 'undo-tree)
  :init
  (setq undo-tree-visualizer-diff nil
        undo-tree-visualizer-relative-timestamps nil)
  :config
  (bind-keys :map undo-tree-visualizer-mode-map
             ("<return>" . undo-tree-visualizer-quit)
             ("C-p" . undo-tree-visualize-undo-to-x)
             ("C-n" . undo-tree-visualize-redo-to-x))
  (unbind-key "C-_" undo-tree-map)
  (global-undo-tree-mode 1))

(use-package smart-hungry-delete
  :ensure t
  :if (my/package-enabled-p 'smart-hungry-delete)
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :config
  (smart-hungry-delete-add-default-hooks))

(use-package paredit
  :diminish paredit-mode
  :if (my/package-enabled-p 'paredit)
  :config
  (mapc (lambda (mode)
          (my/add-mode-hook mode 'enable-paredit-mode))
        '("org" "lisp" "elisp" "ilisp" "slime" "scheme")))

(use-package flyspell
  :if (and (my/package-enabled-p 'flyspell)
           (my/locate-exec "aspell"))
  :config
  (setq ispell-program-name (my/locate-exec "aspell") ;; 设置后台支持程序
        ;; ispell-dictionary "english" ;; default dictionary
        ;; ispell-personal-dictionary ""
        flyspell-issue-message-flag nil)
  (my/add-mode-hook "text" flyspell-mode)
  ;; (my/add-mode-hook "prog" flyspell-prog-mode)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN" . "^#+END") t))
