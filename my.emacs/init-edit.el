;; -*- coding: utf-8 -*-

(use-package beacon
  :diminish (beacon-mode)
  :hook (after-init . pkg/beacon/start)
  :preface
  (defun pkg/beacon/start ()
    (beacon-mode 1))
  :if (my/package-enabled-p 'beacon))

(use-package nlinum-hl
  :hook (after-init . pkg/nlinum-hl/start)
  :preface
  (defun pkg/nlinum-hl/start ()
    (run-with-idle-timer 5 t #'nlinum-hl-flush-window)
    (run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)
    (add-hook 'focus-in-hook #'nlinum-hl-flush-all-windows)
    (add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)
    (advice-add 'select-window :before #'nlinum-hl-do-flush)
    (advice-add 'select-window :after #'nlinum-hl-do-flush))
  :if (my/package-enabled-p 'nlinum-hl))

(use-package yascroll
  :hook (after-init . pkg/yascroll/start)
  :preface
  (defun pkg/yascroll/start ()
    (global-yascroll-bar-mode 1))
  :if (my/package-enabled-p 'yascroll)
  :init
  (setq yascroll:delay-to-hide nil)
  :config
  (add-to-list 'yascroll:disabled-modes 'neotree-mode))

(use-package sublimity
  :hook (after-init . pkg/sublimity/start)
  :preface
  (defun pkg/sublimity/start ()
    (sublimity-mode 1))
  :if (my/package-enabled-p 'sublimity)
  :init
  (setq sublimity-map-size 17
        sublimity-map-max-fraction 0.2
        sublimity-map-text-scale -7)
  :config
  ;; (require 'sublimity-scroll)
  ;; (require 'sublimity-attractive)
  (require 'sublimity-map nil t)
  (sublimity-map-set-delay 5))

(use-package minimap
  :if (my/package-enabled-p 'minimap)
  :init
  (setq minimap-always-recenter nil ;; 设置为nil才有效?
        minimap-recenter-type 'middle
        minimap-buffer-name-prefix "MINI" ;; 不能为空，否则无法启动minimap窗口
        minimap-hide-fringes t
        minimap-hide-scroll-bar t
        minimap-update-delay 1.0
        minimap-window-location 'left
        minimap-display-semantic-overlays nil
        minimap-enlarge-certain-faces nil))

(use-package fill-column-indicator
  :hook (after-init . pkg/fill-column-indicator/start)
  :preface
  (defun pkg/fill-column-indicator/start ()
    (define-globalized-minor-mode global-fci-mode fci-mode
      ;; 避免在special buffers、dired、shell等特殊模式下启用
      (lambda () (when buffer-file-name (fci-mode 1))))
    (global-fci-mode 1))
  :if (my/package-enabled-p 'fill-column-indicator)
  :init
  (setq fci-rule-use-dashes nil
        fci-rule-column 100))

(use-package whitespace
  :if (my/package-enabled-p 'whitespace)
  :init
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column 80))

(use-package rainbow-delimiters
  :hook (after-init . pkg/rainbow-delimiters/start)
  :preface
  (defun pkg/rainbow-delimiters/start ()
    (my/add-mode-hook "prog" #'rainbow-delimiters-mode))
  :if (my/package-enabled-p 'rainbow-delimiters))

;; 修改默认字体颜色，从而将文字与符号区分开来
(use-package rainbow-identifiers
  :hook (after-init . pkg/rainbow-identifiers/start)
  :preface
  (defun pkg/rainbow-identifiers/start ()
    (my/add-mode-hook "prog" #'rainbow-identifiers-mode))
  :if (my/package-enabled-p 'rainbow-identifiers)
  :init
  (setq rainbow-identifiers-face-count 1))

(use-package highlight-thing
  :diminish (highlight-thing-mode)
  :hook (after-init . pkg/highlight-thing/start)
  :preface
  (defun pkg/highlight-thing/start ()
    (my/add-mode-hook "text" #'hl-line-mode) ;; (global-hl-line-mode -1)
    (my/add-mode-hook "prog" #'highlight-thing-mode))
  :if (my/package-enabled-p 'highlight-thing)
  :init
  (setq highlight-thing-what-thing 'symbol
        highlight-thing-exclude-thing-under-point t
        highlight-thing-delay-seconds 0.5
        highlight-thing-limit-to-defun nil
        highlight-thing-case-sensitive-p t))


(use-package flyspell
  :diminish (flyspell-mode)
  :hook (after-init . pkg/flyspell/start)
  :preface
  (defun pkg/flyspell/start ()
    (my/add-mode-hook "text" #'flyspell-mode)
    (my/add-mode-hook "prog" #'flyspell-prog-mode))
  :if (and (my/package-enabled-p 'flyspell)
           (my/locate-exec "aspell"))
  :init
  (setq ispell-program-name (my/locate-exec "aspell") ;; 设置后台支持程序
        ;; ispell-dictionary "english" ;; default dictionary
        ;; ispell-personal-dictionary ""
        flyspell-issue-message-flag nil)
  :config
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN" . "^#+END") t))

(use-package flyspell-correct
  :after (flyspell)
  :if (and (my/package-enabled-p 'flyspell)
           (my/package-enabled-p 'flyspell-correct))
  :config
  (use-package flyspell-correct-helm
    :after (helm)
    :if (and (my/package-enabled-p 'helm)
             (my/package-enabled-p 'flyspell-correct-helm))
    :init
    (setq flyspell-correct-interface #'flyspell-correct-helm))
  (use-package flyspell-correct-ivy
    :after (ivy)
    :if (and (my/package-enabled-p 'ivy)
             (my/package-enabled-p 'flyspell-correct-ivy))
    :init
    (setq flyspell-correct-interface #'flyspell-correct-ivy)))

(use-package avy
  :bind (("C-:" . avy-goto-char-timer) ;; (avy-goto-char)
         ("C-;" . avy-pop-mark))
  :if (my/package-enabled-p 'avy)
  :config
  ;; (avy-setup-default)
  (setq avy-timeout-seconds 1.0))

(use-package ace-jump-mode
  :bind (("C-:" . ace-jump-char-mode)
         ("C-;" . ace-jump-mode-pop-mark))
  :if (my/package-enabled-p 'ace-jump-mode)
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package ace-pinyin
  :hook (after-init . pkg/ace-pinyin/start)
  :preface
  (defun pkg/ace-pinyin/start ()
    (ace-pinyin-global-mode 1))
  (defconst pkg/ace-pinyin/backend
    (or (my/package-enabled-p 'avy)
        (my/package-enabled-p 'ace-jump-mode)))
  :if (and pkg/ace-pinyin/backend
           (my/package-enabled-p 'ace-pinyin))
  :config
  (setq ace-pinyin-use-avy (eq pkg/ace-pinyin/backend 'avy)))

(use-package undo-tree
  :diminish (undo-tree-mode)
  :hook (after-init . pkg/undo-tree/start)
  :preface
  (defun pkg/undo-tree/start ()
    (global-undo-tree-mode 1))
  :if (my/package-enabled-p 'undo-tree)
  :init
  (setq undo-tree-visualizer-diff nil
        undo-tree-visualizer-relative-timestamps nil))

(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :if (my/package-enabled-p 'smart-hungry-delete)
  :config
  (smart-hungry-delete-add-default-hooks))

(use-package paredit
  :diminish (paredit-mode)
  :hook (after-init . pkg/paredit/start)
  :preface
  (defun pkg/paredit/start ()
    (mapc (lambda (mode)
            (my/add-mode-hook mode #'enable-paredit-mode))
          '("org" "lisp" "elisp" "ilisp" "slime" "scheme")))
  :if (my/package-enabled-p 'paredit))

(use-package lispy
  :diminish (lispy-mode)
  :hook (after-init . pkg/lispy/start)
  :preface
  (defun pkg/lispy/start ()
    (mapc (lambda (mode)
            (my/add-mode-hook mode (lambda () (lispy-mode 1))))
          '("org" "lisp" "elisp" "ilisp" "slime" "scheme")))
  (defconst pkg/lispy/show-backend
    (or (my/package-enabled-p 'helm)
        (my/package-enabled-p 'ivy)
        'default))
  :if (my/package-enabled-p 'lispy)
  :init
  (setq lispy-no-permanent-semantic t
        lispy-close-quotes-at-end-p t
        lispy-completion-method pkg/lispy/show-backend
        lispy-occur-backend pkg/lispy/show-backend)
  :config
  (use-package semantic ;; FIXME: workaround for a semantic bug
    :defer t
    :config
    (advice-add 'semantic-idle-scheduler-function :around #'ignore)))

(use-package parinfer
  :hook (after-init . pkg/parinfer/start)
  :preface
  (defun pkg/parinfer/start ()
    (mapc (lambda (mode)
            (my/add-mode-hook mode #'parinfer-mode))
          '("lisp" "elisp" "ilisp" "scheme")))
  :if (my/package-enabled-p 'parinfer)
  :init
  (setq parinfer-extensions '(defaults
                               pretty-parens
                               smart-yank
                               smart-tab
                               paredit)
        parinfer-auto-switch-indent-mode nil
        parinfer-auto-switch-indent-mode-when-closing nil
        parinfer-delay-invoke-idle 1.0))


(provide 'my/init-edit)