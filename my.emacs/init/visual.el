;; -*- coding: utf-8 -*-

(use-package beacon
  :diminish (beacon-mode)
  :if (pkg/package/enabled-p 'beacon)
  :config
  (beacon-mode 1))

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
  :init
  (setq yascroll:delay-to-hide nil)
  :config
  (add-to-list 'yascroll:disabled-modes 'neotree-mode)
  (global-yascroll-bar-mode 1))

(use-package sublimity
  :if (pkg/package/enabled-p 'sublimity)
  :init
  (setq sublimity-map-size 17
        sublimity-map-max-fraction 0.2
        sublimity-map-text-scale -7)
  :config
  (sublimity-mode 1)
  ;; (require 'sublimity-scroll)
  ;; (require 'sublimity-attractive)
  (use-package sublimity-map
    :config
    (sublimity-map-set-delay 5)))

(use-package minimap
  :if (pkg/package/enabled-p 'minimap)
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

(use-package whitespace
  :if (pkg/package/enabled-p 'whitespace)
  :init
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column 100)
  :config
  (global-whitespace-mode 1))

(use-package fill-column-indicator
  :if (pkg/package/enabled-p 'fill-column-indicator)
  :init
  (setq fci-rule-use-dashes nil
        fci-rule-column 100)
  :config
  (define-globalized-minor-mode global-fci-mode fci-mode
    ;; 避免在special buffers、dired、shell等特殊模式下启用
    (lambda () (when buffer-file-name (fci-mode 1))))
  (global-fci-mode 1))

(use-package rainbow-delimiters
  :defer t
  :if (pkg/package/enabled-p 'rainbow-delimiters)
  :init
  (my/add-mode-hook "prog" #'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :defer t
  :if (pkg/package/enabled-p 'rainbow-identifiers)
  :init
  (setq rainbow-identifiers-face-count 1)
  (my/add-mode-hook "prog" #'rainbow-identifiers-mode))

(use-package color-identifiers
  :defer t
  :if (pkg/package/enabled-p 'color-identifiers)
  :init
  (my/add-mode-hook "prog" #'color-identifiers-mode))

(use-package highlight-thing
  :diminish (highlight-thing-mode)
  :defer t
  :if (pkg/package/enabled-p 'highlight-thing)
  :init
  (setq highlight-thing-what-thing 'symbol
        highlight-thing-exclude-thing-under-point nil
        highlight-thing-delay-seconds 1.0
        highlight-thing-limit-to-defun t
        highlight-thing-case-sensitive-p t
        highlight-thing-prefer-active-region t
        highlight-thing-all-visible-buffers-p t)
  (my/add-mode-hook "prog" #'highlight-thing-mode)
  :config
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
  (defun pkg/zoom/setup ()
    (zoom-mode 1))
  :if (pkg/package/enabled-p 'zoom)
  :init
  (setq zoom-minibuffer-preserve-layout nil))


(provide 'my/init/visual)
