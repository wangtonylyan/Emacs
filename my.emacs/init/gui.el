;; -*- coding: utf-8 -*-

(defun my/theme/load (list)
  (when (consp list)
    (let ((first (car list)))
      (if (and (my/pairp first)
               (my/theme/style-p (car first)))
          (load-theme (cdr first) t)
        (my/theme/load (cdr list))))))

(defun my/theme/style-p (style)
  (eq style 'light)
  (eq style 'dark))

(defun my/theme/enabled-p (theme)
  (eq theme 'apropospriate-theme)
  (eq theme 'zenburn-theme)
  (eq theme 'one-themes)
  (eq theme 'poet-theme) ;; provide nice org themes
  (eq theme 'solarized-theme)
  (eq theme 'spacemacs-theme)
  (eq theme 'doom-themes))

(let* ((dir (my/set-user-emacs-file "theme/" t))
       (dir (my/locate 'exist dir nil t))
       (path (my/get-file-path dir)))
  (when path
    (add-to-list 'custom-theme-load-path path)))

(use-package apropospriate-theme
  :if (and (pkg/package/enabled-p 'apropospriate-theme)
           (my/theme/enabled-p 'apropospriate-theme))
  :init
  (setq apropospriate-org-level-resizing nil)
  (my/theme/load '((light . apropospriate-light)
                   (dark . apropospriate-dark))))

(use-package doom-themes
  :if (and (pkg/package/enabled-p 'doom-themes)
           (my/theme/enabled-p 'doom-themes))
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-solarized-light-brighter-modeline nil
        doom-solarized-light-brighter-comments nil
        doom-solarized-light-comment-bg nil
        doom-spacegrey-brighter-modeline nil
        doom-spacegrey-brighter-comments nil
        doom-spacegrey-comment-bg nil
        doom-one-brighter-modeline nil
        doom-one-brighter-comments nil
        doom-one-comment-bg nil
        doom-vibrant-brighter-modeline nil
        doom-vibrant-brighter-comments nil
        doom-vibrant-comment-bg nil)
  (my/theme/load '((light . doom-solarized-light)
                   (dark . doom-spacegrey)
                   (dark . doom-one)
                   (dark . doom-vibrant)))
  :config
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
    :disabled ;; doesn't work
    :after (neotree)
    :if (pkg/package/enabled-p 'neotree)
    :config
    (doom-themes-neotree-config))
  (use-package doom-themes
    :after (org)
    :if (pkg/package/enabled-p 'org)
    :config
    (doom-themes-org-config)))

(use-package one-themes
  :if (and (pkg/package/enabled-p 'one-themes)
           (my/theme/enabled-p 'one-themes))
  :init
  (my/theme/load '((light . one-light)
                   (dark . one-dark))))

(use-package custom ;; do not directly require 'poet-theme
  :if (and (pkg/package/enabled-p 'poet-theme)
           (my/theme/enabled-p 'poet-theme))
  :init
  (my/theme/load '((light . poet)
                   (light . poet-monochrome)
                   (dark . poet-dark)
                   (dark . poet-dark-monochrome))))

(use-package spacemacs-common
  :if (and (pkg/package/enabled-p 'spacemacs-theme)
           (my/theme/enabled-p 'spacemacs-theme))
  :init
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italic t
        spacemacs-theme-keyword-italic nil
        spacemacs-theme-underline-parens nil
        spacemacs-theme-org-bold t
        spacemacs-theme-org-height nil
        spacemacs-theme-org-highlight t
        spacemacs-theme-org-priority-bold t
        spacemacs-theme-org-agenda-height nil)
  (when (my/theme/style-p 'light)
    (setq spacemacs-theme-custom-colors '((base . "#251330") ;; "#655370"
                                          (base-dim . "#605462") ;; "#a094a2"
                                          (const . "#8700af")    ;; terminal
                                          (func . "#5c2153"))))  ;; "#6c3163"
  (my/theme/load '((light . spacemacs-light)
                   (dark . spacemacs-dark))))

(use-package solarized-theme
  :if (and (pkg/package/enabled-p 'solarized-theme)
           (my/theme/enabled-p 'solarized-theme))
  :init
  (setq solarized-distinct-fringe-background nil
        solarized-distinct-doc-face t
        solarized-high-contrast-mode-line t
        solarized-use-more-italic t
        solarized-emphasize-indicators t)
  (my/theme/load '((light . solarized-light)
                   (dark . solarized-dark))))

(use-package zenburn-theme
  :if (and (pkg/package/enabled-p 'zenburn-theme)
           (my/theme/enabled-p 'zenburn-theme))
  :init
  ;; (setq zenburn-override-colors-alist '(("zenburn-fg" . "#EDEDDD")))
  (load-theme 'zenburn t))

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

(use-package parrot
  :if (pkg/package/enabled-p 'parrot)
  :config
  (setq parrot-animate-parrot t
        parrot-num-rotations 5)
  (dolist (rotation '())
    (add-to-list 'parrot-rotate-dict rotation))
  (add-hook 'parrot-click-hook #'flyspell-buffer)
  (parrot-mode 1)
  (when (pkg/package/enabled-p 'nyan-mode)
    (parrot-set-parrot-type 'nyan)))

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
  (spaceline-emacs-theme) ;; (spaceline-spacemacs-theme)
  (spaceline-info-mode 1)
  (use-package spaceline-config
    :after (helm)
    :if (pkg/package/enabled-p 'helm)
    :config
    (spaceline-helm-mode 1)))

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
  :init
  (setq doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-python-executable my/bin/python-interpreter
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes t
        doom-modeline-enable-word-count t
        doom-modeline-persp-name t
        doom-modeline-lsp (when (pkg/package/enabled-p 'lsp-mode) t)
        doom-modeline-github nil
        doom-modeline-github-interval (* 60 60)
        doom-modeline-env-version t
        doom-modeline-mu4e nil)
  :config
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
  :commands (neotree-toggle
             neo-global--window-exists-p
             neo-global--select-window)
  :preface
  (defun pkg/neotree/select-window ()
    (interactive)
    (if (neo-global--window-exists-p)
        (neo-global--select-window)
      (pkg/neotree/toggle)))
  (defun pkg/neotree/toggle ()
    (interactive)
    (let ((root (when (and (fboundp 'projectile-project-p)
                           (fboundp 'projectile-project-root)
                           (projectile-project-p))
                  (projectile-project-root)))
          (file (buffer-file-name)))
      (unless (neo-global--window-exists-p)
        (neotree-toggle))
      (when (and root file (neo-global--window-exists-p))
        (neotree-dir (file-truename root))
        (neotree-find (file-truename file)))))
  :if (pkg/package/enabled-p 'neotree)
  :config
  (setq neo-theme 'icons ;; 'all-the-icons
        neo-mode-line-type 'neotree
        neo-mode-line-custom-format nil
        neo-window-position 'left
        neo-window-width 35
        neo-window-fixed-size t
        neo-reset-size-on-open nil
        neo-toggle-window-keep-p nil
        neo-smart-open t
        neo-hide-cursor t
        neo-auto-indent-point t
        neo-show-updir-line nil
        neo-show-hidden-files nil
        neo-show-slash-for-folder t
        neo-create-file-auto-open nil
        neo-banner-message nil
        neo-autorefresh nil
        neo-force-change-root nil
        neo-click-changes-root nil
        neo-keymap-style 'default)
  (use-package neotree
    :after (projectile)
    :if (pkg/package/enabled-p 'projectile)
    :config
    (my/add-pkg-hook "projectile/switch" #'neotree-projectile-action)))

;; TODO: eyebrowse, perspective, framegroups
(use-package window-purpose
  :commands (purpose-save-window-layout
             purpose-load-window-layout)
  :if (pkg/package/enabled-p 'window-purpose)
  :config
  (setq purpose-layout-dirs (my/set-user-emacs-file ".purpose"))
  (purpose-mode 1)
  (use-package window-purpose-x))


(provide 'my/init/gui)
