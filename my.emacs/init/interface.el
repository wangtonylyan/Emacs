;; -*- coding: utf-8 -*-

(let* ((dir (my/set-user-emacs-file "theme/" t))
       (dir (my/locate 'exist dir nil t))
       (path (my/get-file-path dir)))
  (when path
    (add-to-list 'custom-theme-load-path path)))

(use-package atom-one-dark-theme
  :if (pkg/package/enabled-p 'atom-one-dark-theme)
  :init
  (load-theme 'atom-one-dark t))

(use-package doom-themes
  :if (pkg/package/enabled-p 'doom-themes)
  :init
  ;; 'doom-one, 'doom-nova, 'doom-spacegrey
  (load-theme 'doom-spacegrey t)
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil
        doom-spacegrey-brighter-modeline t
        doom-spacegrey-brighter-comments t
        doom-spacegrey-comment-bg nil)
  (when visible-bell
    (doom-themes-visual-bell-config))
  (use-package doom-themes-treemacs
    :after (treemacs)
    :if (pkg/package/enabled-p 'treemacs)
    :config
    (setq doom-treemacs-enable-variable-pitch t
          doom-treemacs-line-spacing 1
          doom-treemacs-use-generic-icons nil)
    (doom-themes-treemacs-config))
  (use-package doom-themes
    :after (neotree)
    :if (pkg/package/enabled-p 'neotree)
    :config
    (doom-themes-neotree-config))
  (use-package doom-themes
    :after (org)
    :if (pkg/package/enabled-p 'org)
    :config
    (doom-themes-org-config)))

(use-package github-theme
  :if (pkg/package/enabled-p 'github-theme)
  :init
  (load-theme 'github t)
  :config
  (setq github-override-colors-alist '(("github-white" . "#FBF9E1")
                                       ("github-comment" . "#009E73")
                                       ("github-text" . "#000000"))))

(use-package solarized-theme
  :if (pkg/package/enabled-p 'solarized-theme)
  :init
  ;; (load-theme 'solarized-dark t)
  (load-theme 'solarized-light t)
  :config
  (setq solarized-distinct-fringe-background nil
        solarized-distinct-doc-face t
        solarized-high-contrast-mode-line t
        solarized-use-more-italic t
        solarized-emphasize-indicators t))

(use-package zenburn-theme
  :if (pkg/package/enabled-p 'zenburn-theme)
  :init
  (load-theme 'zenburn t)
  :config
  ;; (setq zenburn-override-colors-alist '(("zenburn-fg" . "#EDEDDD")))
  )

(use-package dashboard
  :diminish (page-break-lines-mode)
  :if (pkg/package/enabled-p 'dashboard)
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs"
        dashboard-startup-banner 'official
        dashboard-items '((recents . 10)
                          (bookmarks . 10)
                          (projects . 10)))
  (dashboard-setup-startup-hook))

(use-package nyan-mode
  :if (pkg/package/enabled-p 'nyan-mode)
  :config
  (setq nyan-animate-nyancat nil
        nyan-wavy-trail nil)
  (nyan-mode 1))

(use-package powerline
  :defer t
  :config
  (setq powerline-default-separator 'arrow
        powerline-default-separator-dir '(left . right)))

(use-package spaceline
  :defer t
  :config
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified))

(use-package spaceline-config
  :if (pkg/package/enabled-p 'spaceline)
  :config
  (spaceline-emacs-theme)
  (use-package spaceline-config
    :after (helm)
    :if (pkg/package/enabled-p 'helm)
    :config
    (spaceline-helm-mode)))

(use-package spaceline-all-the-icons
  :if (pkg/package/enabled-p 'spaceline-all-the-icons)
  :config
  (spaceline-all-the-icons-theme)
  (use-package spaceline-all-the-icons
    :after (package)
    :config
    (spaceline-all-the-icons--setup-package-updates))
  (use-package spaceline-all-the-icons
    :after (neotree)
    :if (pkg/package/enabled-p 'neotree)
    :config
    (spaceline-all-the-icons--setup-neotree)))

(use-package doom-modeline
  :if (pkg/package/enabled-p 'doom-modeline)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-python-executable my/bin/python-interpreter
        doom-modeline-height 1 ;; lowest
        doom-modeline-bar-width 1 ;; lowest
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes t
        doom-modeline-enable-word-count t
        doom-modeline-persp-name t
        doom-modeline-lsp (when (pkg/package/enabled-p 'lsp-mode) t)
        doom-modeline-github nil
        doom-modeline-github-interval (* 60 60)
        doom-modeline-version t
        doom-modeline-mu4e nil)
  (doom-modeline-mode 1)
  ;; FIXME: modeline in 'helm window is redefined by spaceline,
  ;; but not by doom-modeline, here is just a workaround
  (advice-add #'helm-display-mode-line :override #'ignore))

(use-package tabbar
  :if (pkg/package/enabled-p 'tabbar)
  :config
  (tabbar-mode 1))

(use-package treemacs
  :commands (treemacs-select-window
             treemacs-projectile)
  :preface
  (defun pkg/treemacs/select-window ()
    (interactive)
    (treemacs-select-window)
    (text-scale-increase 0)
    (text-scale-decrease 0.3))
  :if (pkg/package/enabled-p 'treemacs)
  :config
  (setq treemacs-python-executable my/bin/python-interpreter
        treemacs-persist-file (my/set-user-emacs-file ".treemacs/persist")
        treemacs-display-in-side-window t
        treemacs-is-never-other-window t
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
  (treemacs-resize-icons 10)
  (treemacs-follow-mode -1)
  (treemacs-tag-follow-mode -1)
  (treemacs-filewatch-mode 1)
  (when (not treemacs-show-cursor)
    ;; 该子模式似乎并不完善，不建议启用
    (treemacs-fringe-indicator-mode 1))
  (when (and treemacs-python-executable my/bin/git-command)
    (treemacs-git-mode 'deferred))
  ;; the following package DOES NOT define its own mode-map
  ;; neither does it remap or provide any key-binding configurations
  ;; instead, it directly sets the 'treemacs-mode-map
  ;; which results in the conflict with current init-keys.el
  ;; in which 'treemacs-mode-map is reset right after 'treemacs is loaded
  ;; FIXME: load this packages in this 'treemacs :config part
  ;; additionally, use :demand as an indication for this kind of workaround
  (use-package treemacs-projectile
    :demand t
    :if (pkg/package/enabled-p '(projectile treemacs-projectile))))

(use-package treemacs-icons-dired
  :defer t
  :preface
  (defun pkg/treemacs-icons-dired/setup ()
    (treemacs-icons-dired-mode))
  :if (pkg/package/enabled-p '(treemacs treemacs-icons-dired))
  :init
  (my/add-mode-hook "DIRED" #'pkg/treemacs-icons-dired/setup))

(use-package neotree
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
  :if (pkg/package/enabled-p 'neotree)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'nerd) ;; all-the-icons
        neo-smart-open t
        neo-show-hidden-files nil
        neo-show-updir-line t
        neo-window-width 35)
  (use-package neotree
    :after (projectile)
    :if (pkg/package/enabled-p 'projectile)
    :config
    (my/add-pkg-hook "projectile/switch" #'neotree-projectile-action)))


(provide 'my/init/interface)
