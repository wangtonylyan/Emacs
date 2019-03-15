;; -*- coding: utf-8 -*-

(use-package beacon
  :diminish (beacon-mode)
  :if (pkg/package/enabled-p 'beacon)
  :config
  (beacon-mode 1))

(use-package centered-cursor-mode
  :if (pkg/package/enabled-p 'centered-cursor-mode)
  :config
  (global-centered-cursor-mode 1))

(use-package nlinum-hl
  :if (pkg/package/enabled-p 'nlinum-hl)
  :init
  (run-with-idle-timer 5 t #'nlinum-hl-flush-window)
  (run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)
  (add-hook 'focus-in-hook #'nlinum-hl-flush-all-windows)
  (add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)
  (advice-add 'select-window :before #'nlinum-hl-do-flush)
  (advice-add 'select-window :after #'nlinum-hl-do-flush))

(use-package yascroll
  :if (pkg/package/enabled-p 'yascroll)
  :config
  (setq yascroll:delay-to-hide nil)
  (add-to-list 'yascroll:disabled-modes 'neotree-mode)
  (global-yascroll-bar-mode 1))

(use-package sublimity
  :if (pkg/package/enabled-p 'sublimity)
  :config
  (setq sublimity-map-size 17
        sublimity-map-max-fraction 0.2
        sublimity-map-text-scale -7)
  (sublimity-mode 1)
  ;; (require 'sublimity-scroll)
  ;; (require 'sublimity-attractive)
  (use-package sublimity-map
    :config
    (sublimity-map-set-delay 5)))

(use-package minimap
  :if (pkg/package/enabled-p 'minimap)
  :config
  (setq minimap-always-recenter nil ;; 设置为nil才有效?
        minimap-recenter-type 'middle
        minimap-buffer-name-prefix "MINI" ;; 不能为空，否则无法启动minimap窗口
        minimap-hide-fringes t
        minimap-hide-scroll-bar t
        minimap-update-delay 1.0
        minimap-window-location 'left
        minimap-display-semantic-overlays nil
        minimap-enlarge-certain-faces nil))

(use-package whitespace
  :if (pkg/package/enabled-p 'whitespace)
  :config
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column nil)
  (global-whitespace-mode 1))

(use-package fill-column-indicator
  :defer t
  :if (pkg/package/enabled-p 'fill-column-indicator)
  :init
  (my/add-mode-hook "prog" #'fci-mode)
  :config
  (setq fci-rule-column nil
        fci-rule-use-dashes nil
        fci-rule-color (face-background 'hl-line)
        fci-rule-width 3
        fci-handle-truncate-lines truncate-lines
        fci-handle-line-move-visual (unless line-move-visual))
  (define-globalized-minor-mode global-fci-mode fci-mode
    ;; 避免在special buffers、dired、shell等特殊模式下启用
    (lambda () (when buffer-file-name (fci-mode 1)))))

(use-package visual-fill-column
  :defer t
  :preface
  (defun pkg/visual-fill-column/start ()
    (visual-fill-column-mode 1)
    (setq visual-fill-column-width 125))
  :if (pkg/package/enabled-p 'visual-fill-column)
  :init
  (my/add-mode-hook "text" #'pkg/visual-fill-column/start))

(use-package rainbow-delimiters
  :defer t
  :if (pkg/package/enabled-p 'rainbow-delimiters)
  :init
  (my/add-mode-hook "prog" #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :defer t
  :if (pkg/package/enabled-p 'rainbow-identifiers)
  :init
  (my/add-mode-hook "prog" #'rainbow-identifiers-mode)
  :config
  (setq rainbow-identifiers-face-count 1))

(use-package color-identifiers
  :defer t
  :if (pkg/package/enabled-p 'color-identifiers)
  :init
  (my/add-mode-hook "prog" #'color-identifiers-mode))

(use-package highlight-context-line
  :defer t
  :if (pkg/package/enabled-p 'highlight-context-line)
  :init
  (highlight-context-line-mode 1))

(use-package highlight-numbers
  :defer t
  :if (pkg/package/enabled-p 'highlight-numbers)
  :init
  (my/add-mode-hook "prog" #'highlight-numbers-mode))

(use-package highlight-parentheses
  :diminish (highlight-parentheses-mode)
  :defer t
  :if (pkg/package/enabled-p 'highlight-parentheses)
  :init
  (my/add-mode-hook "prog" #'highlight-parentheses-mode)
  :config
  (setq hl-paren-highlight-adjacent nil
        hl-paren-colors '("green")))

(use-package highlight-indentation
  :defer t
  :if (pkg/package/enabled-p 'highlight-indentation)
  :init
  (my/add-mode-hook "python" #'highlight-indentation-mode))

(use-package highlight-indent-guides
  :defer t
  :if (pkg/package/enabled-p 'highlight-indent-guides)
  :init
  (my/add-mode-hook "python" #'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package highlight-defined
  :defer t
  :if (pkg/package/enabled-p 'highlight-defined)
  :init
  (my/add-mode-hook "elisp" #'highlight-defined-mode)
  :config
  (setq highlight-defined-face-use-itself t))

(use-package highlight-thing
  :diminish (highlight-thing-mode)
  :defer t
  :if (pkg/package/enabled-p 'highlight-thing)
  :init
  (my/add-mode-hook "prog" #'highlight-thing-mode)
  :config
  (setq highlight-thing-what-thing 'symbol
        highlight-thing-exclude-thing-under-point nil
        highlight-thing-delay-seconds 1.0
        highlight-thing-limit-to-defun t
        highlight-thing-case-sensitive-p t
        highlight-thing-prefer-active-region t
        highlight-thing-all-visible-buffers-p t)
  (add-hook 'activate-mark-hook
            (lambda () (when (derived-mode-p 'prog-mode)
                    (highlight-thing-mode -1))))
  (add-hook 'deactivate-mark-hook
            (lambda () (when (derived-mode-p 'prog-mode)
                    (highlight-thing-mode 1)))))

(use-package dimmer
  :if (pkg/package/enabled-p 'dimmer)
  :config
  (dimmer-mode 1))

(use-package zoom
  :diminish (zoom-mode)
  :commands (zoom)
  :preface
  (defun pkg/zoom/start () ;; dummy
    (zoom-mode 1))
  :if (pkg/package/enabled-p 'zoom)
  :config
  (setq zoom-minibuffer-preserve-layout nil))


(provide 'my/init/visual)
