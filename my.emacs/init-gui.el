;; -*- coding: utf-8 -*-

(defun my/init-gui/init ()
  (pkg/theme/init)
  (pkg/dashboard/init)
  (pkg/nyan-mode/init)
  (pkg/powerline/init)
  (pkg/spaceline/init)
  (pkg/spaceline-config/init)
  (pkg/spaceline-all-the-icons/init)
  (pkg/doom-modeline/init)
  (pkg/tabbar/init)
  (pkg/treemacs/init)
  (pkg/treemacs-icons-dired/init)
  (pkg/neotree/init)
  (toggle-frame-maximized))

(my/add-mode-hook "init" #'my/init-gui/init)


(defun pkg/theme/init ()
  (let* ((dir (my/set-user-emacs-file "theme/" t))
         (dir (my/locate 'exist dir nil t))
         (path (my/get-file-path dir)))
    (when path
      (add-to-list 'custom-theme-load-path path)))
  (use-package atom-one-dark-theme
    :if (my/package-enabled-p 'atom-one-dark-theme)
    :init
    (load-theme 'atom-one-dark t))
  (use-package doom-themes
    :if (my/package-enabled-p 'doom-themes)
    :init
    (setq doom-themes-enable-bold nil
          doom-themes-enable-italic nil
          doom-spacegrey-brighter-modeline t
          doom-spacegrey-brighter-comments t
          doom-spacegrey-comment-bg nil)
    ;; 'doom-one, 'doom-nova, 'doom-spacegrey
    (load-theme 'doom-spacegrey t)
    :config
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
      (doom-themes-treemacs-config))
    (use-package doom-themes
      :after (neotree)
      :if (my/package-enabled-p 'neotree)
      :config
      (doom-themes-neotree-config)))
  (use-package github-theme
    :if (my/package-enabled-p 'github-theme)
    :init
    (setq github-override-colors-alist '(("github-white" . "#FBF9E1")
                                         ("github-comment" . "#009E73")
                                         ("github-text" . "#000000")))
    (load-theme 'github t))
  (use-package solarized-theme
    :if (my/package-enabled-p 'solarized-theme)
    :init
    (setq solarized-distinct-fringe-background nil
          solarized-distinct-doc-face t
          solarized-high-contrast-mode-line t
          solarized-use-more-italic t
          solarized-emphasize-indicators t)
    ;; (load-theme 'solarized-dark t)
    (load-theme 'solarized-light t))
  (use-package zenburn-theme
    :if (my/package-enabled-p 'zenburn-theme)
    :init
    ;; (setq zenburn-override-colors-alist '(("zenburn-fg" . "#EDEDDD")))
    (load-theme 'zenburn t)))

(defun pkg/dashboard/init ()
  (use-package dashboard
    :diminish (page-break-lines-mode)
    :if (my/package-enabled-p 'dashboard)
    :init
    (setq dashboard-banner-logo-title "Welcome to Emacs"
          dashboard-startup-banner 'official
          dashboard-items '((recents . 10)
                            (bookmarks . 10)
                            (projects . 10)))
    :config
    (dashboard-setup-startup-hook)))

(defun pkg/nyan-mode/init ()
  (use-package nyan-mode
    :if (my/package-enabled-p 'nyan-mode)
    :init
    (setq nyan-animate-nyancat nil
          nyan-wavy-trail nil)
    :config
    (nyan-mode 1)))

(defun pkg/powerline/init ()
  (use-package powerline
    :defer t
    :init
    (setq powerline-default-separator 'arrow
          powerline-default-separator-dir '(left . right))))

(defun pkg/spaceline/init ()
  (use-package spaceline
    :defer t
    :init
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)))

(defun pkg/spaceline-config/init ()
  (use-package spaceline-config
    :if (my/package-enabled-p 'spaceline)
    :config
    (spaceline-emacs-theme)
    (use-package spaceline-config
      :after (helm)
      :if (my/package-enabled-p 'helm)
      :config
      (spaceline-helm-mode))))

(defun pkg/spaceline-all-the-icons/init ()
  (use-package spaceline-all-the-icons
    :if (my/package-enabled-p 'spaceline-all-the-icons)
    :config
    (spaceline-all-the-icons-theme)
    (use-package spaceline-all-the-icons
      :after (package)
      :config
      (spaceline-all-the-icons--setup-package-updates))
    (use-package spaceline-all-the-icons
      :after (neotree)
      :if (my/package-enabled-p 'neotree)
      :config
      (spaceline-all-the-icons--setup-neotree))))

(defun pkg/doom-modeline/init ()
  (use-package doom-modeline
    :if (my/package-enabled-p 'doom-modeline)
    :init
    (setq doom-modeline-buffer-file-name-style 'relative-to-project
          doom-modeline-python-executable (or (my/locate-exec "python3")
                                              (my/locate-exec "python"))
          doom-modeline-height 1 ;; lowest
          doom-modeline-bar-width 1 ;; lowest
          doom-modeline-icon t
          doom-modeline-major-mode-icon t
          doom-modeline-major-mode-color-icon t
          doom-modeline-minor-modes t
          doom-modeline-enable-word-count t
          doom-modeline-persp-name t
          doom-modeline-lsp (when (my/package-enabled-p 'lsp-mode) t)
          doom-modeline-github nil
          doom-modeline-github-interval (* 60 60)
          doom-modeline-version t
          doom-modeline-mu4e nil)
    :config
    (doom-modeline-mode 1)
    ;; FIXME: modeline in 'helm window is redefined by spaceline,
    ;; but not by doom-modeline, here is just a workaround
    (advice-add #'helm-display-mode-line :override #'ignore)))

(defun pkg/tabbar/init ()
  (use-package tabbar
    :if (my/package-enabled-p 'tabbar)
    :config
    (tabbar-mode 1)))

(defun pkg/treemacs/init ()
  (use-package treemacs
    :commands (treemacs-select-window
               treemacs-projectile)
    :preface
    (defun pkg/treemacs/select-window ()
      (interactive)
      (treemacs-select-window)
      (text-scale-increase 0)
      (text-scale-decrease 0.3))
    :if (my/package-enabled-p 'treemacs)
    :init
    (setq treemacs-python-executable (or (my/locate-exec "python3")
                                         (my/locate-exec "python"))
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
    :config
    (treemacs-resize-icons 10)
    (treemacs-follow-mode -1)
    (treemacs-tag-follow-mode -1)
    (treemacs-filewatch-mode 1)
    (when (not treemacs-show-cursor)
      ;; 该子模式似乎并不完善，不建议启用
      (treemacs-fringe-indicator-mode 1))
    (when (and (my/locate-exec "git")
               (my/locate-exec "python3"))
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
      :if (and (my/package-enabled-p 'projectile)
               (my/package-enabled-p 'treemacs-projectile)))))

(defun pkg/treemacs-icons-dired/init ()
  (use-package treemacs-icons-dired
    :defer t
    :preface
    (defun pkg/treemacs-icons-dired/setup ()
      (treemacs-icons-dired-mode))
    :if (and (my/package-enabled-p 'treemacs)
             (my/package-enabled-p 'treemacs-icons-dired))
    :init
    (my/add-mode-hook "DIRED" #'pkg/treemacs-icons-dired/setup)))

(defun pkg/neotree/init ()
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
    :if (my/package-enabled-p 'neotree)
    :init
    (setq neo-theme (if (display-graphic-p) 'icons 'nerd) ;; all-the-icons
          neo-smart-open t
          neo-show-hidden-files nil
          neo-show-updir-line t
          neo-window-width 35)
    :config
    (use-package neotree
      :after (projectile)
      :if (my/package-enabled-p 'projectile)
      :config
      (pkg/projectile/add-switch-action #'neotree-projectile-action))))


(provide 'my/init-gui)
