;; -*- coding: utf-8 -*-

(defun my/init-vis/init ()
  (pkg/beacon/init)
  (pkg/nlinum-hl/init)
  (pkg/yascroll/init)
  (pkg/sublimity/init)
  (pkg/minimap/init)
  (pkg/whitespace/init)
  (pkg/fill-column-indicator/init)
  (pkg/rainbow-delimiters/init)
  (pkg/rainbow-identifiers/init)
  (pkg/highlight-thing/init)
  (pkg/dimmer/init)
  (pkg/zoom/init))

(my/add-mode-hook "init" #'my/init-vis/init)


(defun pkg/beacon/init ()
  (use-package beacon
    :diminish (beacon-mode)
    :if (my/package-enabled-p 'beacon)
    :config
    (beacon-mode 1)))

(defun pkg/nlinum-hl/init ()
  (use-package nlinum-hl
    :if (my/package-enabled-p 'nlinum-hl)
    :init
    (run-with-idle-timer 5 t #'nlinum-hl-flush-window)
    (run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)
    (add-hook 'focus-in-hook #'nlinum-hl-flush-all-windows)
    (add-hook 'focus-out-hook #'nlinum-hl-flush-all-windows)
    (advice-add 'select-window :before #'nlinum-hl-do-flush)
    (advice-add 'select-window :after #'nlinum-hl-do-flush)))

(defun pkg/yascroll/init ()
  (use-package yascroll
    :if (my/package-enabled-p 'yascroll)
    :init
    (setq yascroll:delay-to-hide nil)
    :config
    (add-to-list 'yascroll:disabled-modes 'neotree-mode)
    (global-yascroll-bar-mode 1)))

(defun pkg/sublimity/init ()
  (use-package sublimity
    :if (my/package-enabled-p 'sublimity)
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
      (sublimity-map-set-delay 5))))

(defun pkg/minimap/init ()
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
          minimap-enlarge-certain-faces nil)))

(defun pkg/whitespace/init ()
  (use-package whitespace
    :if (my/package-enabled-p 'whitespace)
    :init
    (setq whitespace-style '(face lines-tail)
          whitespace-line-column 100)
    :config
    (global-whitespace-mode 1)))

(defun pkg/fill-column-indicator/init ()
  (use-package fill-column-indicator
    :if (my/package-enabled-p 'fill-column-indicator)
    :init
    (setq fci-rule-use-dashes nil
          fci-rule-column 100)
    :config
    (define-globalized-minor-mode global-fci-mode fci-mode
      ;; 避免在special buffers、dired、shell等特殊模式下启用
      (lambda () (when buffer-file-name (fci-mode 1))))
    (global-fci-mode 1)))

(defun pkg/rainbow-delimiters/init ()
  (use-package rainbow-delimiters
    :defer t
    :if (my/package-enabled-p 'rainbow-delimiters)
    :init
    (my/add-mode-hook "prog" #'rainbow-delimiters-mode)))

(defun pkg/rainbow-identifiers/init ()
  (use-package rainbow-identifiers
    :defer t
    :if (my/package-enabled-p 'rainbow-identifiers)
    :init
    (setq rainbow-identifiers-face-count 1)
    (my/add-mode-hook "prog" #'rainbow-identifiers-mode)))

(defun pkg/highlight-thing/init ()
  (use-package highlight-thing
    :diminish (highlight-thing-mode)
    :defer t
    :if (my/package-enabled-p 'highlight-thing)
    :init
    (setq highlight-thing-what-thing 'symbol
          highlight-thing-exclude-thing-under-point t
          highlight-thing-delay-seconds 0.5
          highlight-thing-limit-to-defun nil
          highlight-thing-case-sensitive-p t)
    (my/add-mode-hook "text" #'hl-line-mode) ;; (global-hl-line-mode -1)
    (my/add-mode-hook "prog" #'highlight-thing-mode)))

(defun pkg/dimmer/init ()
  (use-package dimmer
    :if (my/package-enabled-p 'dimmer)
    :config
    (dimmer-mode 1)))

(defun pkg/zoom/init ()
  (use-package zoom
    :diminish (zoom-mode)
    :commands (zoom)
    :preface
    (defun pkg/zoom/setup ()
      (zoom-mode 1))
    :if (my/package-enabled-p 'zoom)
    :init
    (setq zoom-minibuffer-preserve-layout nil)))


(provide 'init-vis)
