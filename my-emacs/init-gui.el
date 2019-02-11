;; -*- coding: utf-8 -*-

(when (fboundp 'x-send-client-message)
  ((lambda ()
     ;; 全屏
     ;; (interactive)
     ;; (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
     ;; 或
     ;; (set-frame-parameter nil 'fullscreen 'fullboth)
     ;; 窗口最大化需要分别经过水平和垂直两个方向的最大化
     (interactive)
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
     (interactive)
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))))


(use-package dashboard
  :if (my/package-enabled-p 'dashboard)
  :init
  (setq dashboard-banner-logo-title "Welcome to Emacs"
        dashboard-startup-banner 'official
        dashboard-items '((recents  . 10)
                          (bookmarks . 10)
                          (projects . 10)))
  :config
  (dashboard-setup-startup-hook))

(use-package nyan-mode
  :if (my/package-enabled-p 'nyan-mode)
  :init
  (setq nyan-animate-nyancat nil
        nyan-wavy-trail nil)
  :config
  (nyan-mode 1))

(use-package spaceline
  :if (or (my/package-enabled-p 'spaceline)
          (my/package-enabled-p 'spaceline-all-the-icons))
  :init
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
  :config
  (use-package powerline
    :init
    (setq powerline-default-separator 'arrow
          powerline-default-separator-dir '(left . right)))
  (use-package spaceline-config
    :if (my/package-enabled-p 'spaceline)
    :config
    (spaceline-emacs-theme)
    (when (my/package-enabled-p 'helm)
      (with-eval-after-load 'helm
        (spaceline-helm-mode))))
  (use-package spaceline-all-the-icons
    :if (my/package-enabled-p 'spaceline-all-the-icons)
    :config
    (spaceline-all-the-icons-theme)
    ;; (spaceline-all-the-icons--setup-package-updates)
    (use-package spaceline-all-the-icons
      :if (my/package-enabled-p 'neotree)
      :config
      (with-eval-after-load 'neotree
        (spaceline-all-the-icons--setup-neotree)))))


(let* ((dir (my/set-user-emacs-file "theme/" t))
       (dir (my/locate 'exist dir nil t))
       (path (my/get-file-path dir)))
  (when path
    (add-to-list 'custom-theme-load-path path)))

(use-package atom-one-dark-theme
  :if (my/package-enabled-p 'atom-one-dark-theme)
  :config
  (load-theme 'atom-one-dark t))

(use-package doom-themes
  :if (my/package-enabled-p 'doom-themes)
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil
        doom-spacegrey-brighter-modeline t
        doom-spacegrey-brighter-comments t
        doom-spacegrey-comment-bg nil)
  (load-theme 'doom-spacegrey t) ;; 'doom-one, 'doom-nova, 'doom-spacegrey
  (when visible-bell
    (doom-themes-visual-bell-config))
  (use-package doom-themes-treemacs
    :after (treemacs)
    :if (my/package-enabled-p 'treemacs)
    :init
    (setq doom-treemacs-enable-variable-pitch t
          doom-treemacs-line-spacing 1
          doom-treemacs-use-generic-icons nil)
    :config
    (with-eval-after-load 'treemacs
      (doom-themes-treemacs-config)))
  (use-package doom-themes
    :after (neotree)
    :if (my/package-enabled-p 'neotree)
    :config
    (with-eval-after-load 'neotree
      (doom-themes-neotree-config))))

(use-package github-theme
  :if (my/package-enabled-p 'github-theme)
  :init
  (setq github-override-colors-alist '(("github-white" . "#FBF9E1")
                                       ("github-comment" . "#009E73")
                                       ("github-text" . "#000000")))
  :config
  (load-theme 'github t))

(use-package solarized-theme
  :if (my/package-enabled-p 'solarized-theme)
  :init
  (setq solarized-distinct-fringe-background nil
        solarized-distinct-doc-face t
        solarized-high-contrast-mode-line t
        solarized-use-more-italic t
        solarized-emphasize-indicators t)
  :config
  ;; (load-theme 'solarized-dark t)
  (load-theme 'solarized-light t))

(use-package zenburn-theme
  :if (my/package-enabled-p 'zenburn-theme)
  :init
  ;; (setq zenburn-override-colors-alist '(("zenburn-fg" . "#EDEDDD")))
  :config
  (load-theme 'zenburn t))


(use-package tabbar
  :if (my/package-enabled-p 'tabbar)
  :config
  (tabbar-mode 1)
  (define-key tabbar-mode-map tabbar-prefix-key nil)
  (bind-keys :map tabbar-mode-map
             ("C-S-i" . tabbar-forward-tab)
             ("C-S-u" . tabbar-backward-tab)
             ("C-S-o" . tabbar-forward-group)
             ("C-S-y" . tabbar-backward-group)))

(use-package treemacs
  :commands (treemacs-select-window
             treemacs-projectile)
  :preface
  (defun pkg/treemacs/select-window ()
    (interactive)
    (treemacs-select-window)
    (text-scale-increase 0)
    (text-scale-decrease 0.5))
  :if (my/package-enabled-p 'treemacs)
  :init
  (setq treemacs-python-executable (or (my/locate-exec "python3")
                                       (my/locate-exec "python"))
        treemacs-persist-file (my/set-user-emacs-file ".cache/treemacs-persist")
        treemacs-display-in-side-window t
        treemacs-is-never-other-window nil
        treemacs-no-delete-other-windows t
        treemacs-position 'left
        treemacs-width 40
        treemacs-show-cursor t
        treemacs-indentation 2
        treemacs-indentation-string " "
        treemacs-sorting 'alphabetic-desc
        treemacs-show-hidden-files nil
        treemacs-no-png-images nil
        treemacs-space-between-root-nodes nil
        treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-follow-after-init t
        treemacs-project-follow-cleanup t
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow nil
        treemacs-follow-recenter-distance 0.2
        treemacs-file-follow-delay 5
        treemacs-file-event-delay 5000
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-tag-follow-delay 1.5
        treemacs-tag-follow-cleanup t
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-deferred-git-apply-delay 0.5
        treemacs-git-command-pipe ""
        treemacs-max-git-entries 5000)
  :config
  (treemacs-resize-icons 10)
  (treemacs-follow-mode -1)
  (treemacs-tag-follow-mode -1)
  (treemacs-filewatch-mode 1)
  (when (not treemacs-show-cursor)
    ;; 该子模式似乎并不完善，不建议启用
    (treemacs-fringe-indicator-mode 1))
  (bind-keys :map treemacs-mode-map
             ([mouse-1] . treemacs-single-click-expand-action))
  (use-package treemacs-icons-dired
    :ensure t
    :config
    (treemacs-icons-dired-mode))
  (use-package treemacs-projectile
    :after (projectile)
    :commands (treemacs-projectile)
    :if (my/package-enabled-p 'projectile))
  (when (and (my/locate-exec "git")
             (my/locate-exec "python3"))
    (treemacs-git-mode 'deferred)))

(use-package neotree
  :ensure all-the-icons
  :commands (neotree-toggle)
  :preface
  (defun pkg/neotree/toggle ()
    (interactive)
    (let ((root (when (and (fboundp 'projectile-project-p)
                           (fboundp 'projectile-project-root)
                           (projectile-project-p))
                  (projectile-project-root)))
          (file (buffer-file-name)))
      (neotree-toggle)
      (if (and root file (neo-global--window-exists-p))
          (progn
            (neotree-dir root)
            (neotree-find file))
        (user-error "*neotree* could not find projectile project"))))
  :if (my/package-enabled-p 'neotree)
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'nerd) ;; all-the-icons
        neo-smart-open t
        neo-show-hidden-files nil
        neo-show-updir-line t
        neo-window-width 35)
  :config
  (unbind-key "s" neotree-mode-map)
  (unbind-key "S" neotree-mode-map)
  (unbind-key "D" neotree-mode-map)
  (unbind-key "H" neotree-mode-map)
  (unbind-key "U" neotree-mode-map)
  (bind-keys :map neotree-mode-map
             ("n" . neotree-next-line)
             ("p" . neotree-previous-line)
             ("M-n" . neotree-select-next-sibling-node)
             ("M-p" . neotree-select-previous-sibling-node)
             ("u" . neotree-select-up-node)
             ("a" . neotree-hidden-file-toggle))
  (when (my/package-enabled-p 'projectile)
    (add-hook 'pkg/projectile/switch-hook #'neotree-projectile-action t)))

(use-package windmove
  :ensure t
  :commands (windmove-left
             windmove-right
             windmove-up
             windmove-down)
  :config
  (windmove-default-keybindings))

(use-package winner
  :ensure t
  :config
  (winner-mode 1))

(use-package buffer-move
  :commands (buf-move-left
             buf-move-right
             buf-move-up
             buf-move-down)
  :if (my/package-enabled-p 'buffer-move))

(use-package dimmer
  :if (my/package-enabled-p 'dimmer)
  :config
  (dimmer-mode 1))

(use-package zoom
  :diminish zoom-mode
  :commands (zoom)
  :if (my/package-enabled-p 'zoom)
  :init
  (setq zoom-minibuffer-preserve-layout nil)
  :config
  ;; (zoom-mode 1)
  )

(provide 'my/init-gui)
