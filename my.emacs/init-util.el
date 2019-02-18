;; -*- coding: utf-8 -*-

(use-package shell ;; inferior shell
  :commands (shell))

(use-package term ;; terminal emulator
  :commands (term ansi-term)
  :preface
  (defun pkg/term/toggle-mode ()
    (interactive)
    (cond ((term-in-char-mode) (term-line-mode))
          ((term-in-line-mode) (term-char-mode))))
  :init
  (setq explicit-shell-file-name shell-file-name)
  :config
  (bind-keys :map term-raw-map
             ("C-q" . pkg/term/toggle-mode)
             :map term-mode-map
             ("C-q" . pkg/term/toggle-mode)))

(use-package eshell ;; emacs shell
  :commands (eshell)
  :bind (("C-c e" . pkg/eshell/startup))
  :preface
  (defun pkg/eshell/startup ()
    (interactive)
    (if (eq major-mode 'eshell-mode)
        (progn
          (insert "exit")
          (eshell-send-input)
          (delete-window))
      (progn
        (let* ((dir (if (buffer-file-name)
                        (my/get-file-directory (buffer-file-name))
                      default-directory))
               (name (car (last (split-string dir "/" t)))))
          (split-window-vertically (- (/ (window-total-height) 3)))
          (other-window 1)
          (eshell "new")
          (rename-buffer (concat "*eshell: " name "*"))))))
  :init
  (setq eshell-directory-name (my/set-user-emacs-file ".eshell/")))

(use-package dired
  :commands (dired)
  :preface
  (defun pkg/dired/count-marked ()
    (let* ((list (dired-get-marked-files nil nil nil t))
           (len (length list)))
      (cond
       ((> len 2) len)
       ((= len 2) (if (eq (car list) t) 1 2))
       (t 0))))
  :init
  (setq dired-recursive-deletes 'top
        dired-copy-preserve-time t))

(use-package dired-hacks-utils
  :if (my/package-enabled-p 'dired-hacks-utils))

(use-package icomplete
  :defer t
  :if (not (my/package-enabled-p 'icomplete))
  :config
  (icomplete-mode -1))

(use-package ido
  :hook (after-init . pkg/ido/start)
  :preface
  (defun pkg/ido/sart ()
    (ido-mode 1)
    ;; 仅使ido支持find-file和switch-to-buffer
    (ido-everywhere -1))
  :if (my/package-enabled-p 'ido)
  :init
  (setq ido-enable-prefix t
        ido-enable-flex-matching t
        ido-use-filename-at-point t
        ido-enter-matching-directory nil))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; 该插件会自动替换原M-x快捷键所绑定的命令，若想保留则可重新绑定之
         ("C-x M-x" . execute-extended-command))
  :if (my/package-enabled-p 'smex)
  :config
  (smex-initialize))

(use-package helm
  :diminish (helm-mode)
  :commands (helm-command-prefix
             helm-M-x
             helm-show-kill-ring
             helm-find-files
             helm-recentf
             helm-mini
             helm-buffers-list
             helm-occur)
  :if (my/package-enabled-p 'helm)
  :init
  (require 'helm-config)
  (setq helm-split-window-inside-p t
        helm-full-frame nil
        helm-use-frame-when-more-than-two-windows nil
        helm-display-header-line nil
        helm-echo-input-in-header-line nil
        helm-autoresize-max-height 30
        helm-autoresize-min-height 0
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-mode-fuzzy-match nil ;; globally disabled
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        ;; helm-apropos-fuzzy-match t
        ;; helm-etags-fuzzy-match t
        helm-move-to-line-cycle-in-source t
        helm-follow-input-idle-delay 0.5
        helm-follow-mode-persistent nil
        ;; helm-source-names-using-follow '("Occur")
        helm-buffer-skip-remote-checking t
        ;; 配置该参数可以指定不同的后台支持，包括imenu、ido、smex等
        ;; helm-completing-read-handlers-alist
        )
  :config
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package ivy
  :if (my/package-enabled-p 'ivy))

(use-package bm ;; TODO
  :commands (bm-next
             bm-previous
             bm-toggle)
  :if (my/package-enabled-p 'bm)
  :config
  (setq bm-marker 'bm-marker-left
        bm-cycle-all-buffers t
        temporary-bookmark-p t
        bm-buffer-persistence t
        bm-restore-repository-on-load t
        bm-repository-file (my/set-user-emacs-file "bm-repository/"))
  (setq-default bm-buffer-persistence bm-buffer-persistence)
  (add-hook' after-init-hook #'bm-repository-load t)
  (add-hook 'find-file-hooks #'bm-buffer-restore t)
  (add-hook 'kill-buffer-hook #'bm-buffer-save t)
  (add-hook 'kill-emacs-hook (lambda ()
                               (progn
                                 (bm-buffer-save-all)
                                 (bm-repository-save))) t)
  (add-hook 'after-save-hook #'bm-buffer-save t)
  (add-hook 'after-revert-hook #'bm-buffer-restore t)
  (use-package helm-bm
    :after (helm)
    :bind (("C-c b b" . helm-bm))
    :if (and (my/package-enabled-p 'helm)
             (my/package-enabled-p 'helm-bm)))
  (unbind-key "C-x r"))

(use-package ediff
  :after (winner)
  :commands (ediff-current-file
             ediff-files
             ediff-files3
             ediff-buffers
             ediff-buffers3)
  :if (my/package-enabled-p 'ediff)
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-make-buffers-readonly-at-startup nil
        ;; ediff-diff-options "" ;; "-w" = "##", "-i" = "#c"
        ;; ediff-forward-word-function 'forward-char ;; "@", "*"
        ediff-highlight-all-diffs nil)
  (defun pkg/ediff/setup-keymap ()
    ;; 可参考(ediff-setup-keymap)，或激活ediff后输入"?"
    ;; (bind-keys :map ediff-mode-map)
    )
  (add-hook 'ediff-keymap-setup-hook #'pkg/ediff/setup-keymap t)
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo t))

(use-package vdiff
  :commands (vdiff-current-file
             vdiff-files
             vdiff-files3
             vdiff-buffers
             vdiff-buffers3)
  :if (my/package-enabled-p 'vdiff)
  :init
  (setq vdiff-diff-algorithm (if (my/locate-exec "git") 'git-diff 'diff-u)
        vdiff-diff3-command '("diff3")
        vdiff-lock-scrolling t
        vdiff-default-refinement-syntax-code "w"
        vdiff-auto-refine nil
        vdiff-subtraction-style 'full
        vdiff-subtraction-fill-char ?\s
        vdiff-disable-folding nil
        vdiff-may-close-fold-on-point nil
        vdiff-min-fold-size 4
        vdiff-fold-padding 6
        vdiff-fold-string-function 'vdiff-fold-string-default))


(use-package projectile
  :diminish (projectile-mode)
  :commands (projectile-project-root)
  :preface
  (defvar pkg/projectile/switch-hook)
  (defun pkg/projectile/add-switch-action (func)
    (add-hook 'pkg/projectile/switch-hook func t))
  (defun pkg/projectile/switch-action ()
    (run-hooks 'pkg/projectile/switch-hook))
  :if (my/package-enabled-p 'projectile)
  :init
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-project-search-path pvt/project/root-directories
        projectile-switch-project-action 'pkg/projectile/switch-action
        projectile-cache-file (my/set-user-emacs-file
                               ".projectile/cache")
        projectile-known-projects-file (my/set-user-emacs-file
                                        ".projectile/bookmarks"))
  :config
  ;; (add-to-list 'projectile-other-file-alist '("html" "js"))
  ;; (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (projectile-mode 1))

(use-package helm-projectile
  :after (helm projectile)
  :if (and (my/package-enabled-p 'helm)
           (my/package-enabled-p 'projectile)
           (my/package-enabled-p 'helm-projectile))
  :init
  (setq projectile-completion-system 'helm
        helm-projectile-fuzzy-match t)
  :config
  (pkg/projectile/add-switch-action #'helm-projectile)
  (helm-projectile-on))

(use-package magit
  :commands (magit-status)
  :if (and (my/package-enabled-p 'magit)
           (my/locate-exec "git"))
  :init
  (setq magit-auto-revert-mode t
        magit-auto-revert-immediately t
        magit-auto-revert-tracked-only t
        magit-ediff-dwim-show-on-hunks t
        ;; magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        ;; 执行(magit-list-repositories)，可以打印出以下列表所指示的路径下搜索到的git项目
        magit-repository-directories
        (my/map (lambda (dir) (cons dir 1)) ;; directory depth = 1
                pvt/project/root-directories)))

(use-package vdiff-magit
  :after (magit)
  :if (and (my/package-enabled-p 'vdiff)
           (my/package-enabled-p 'magit)
           (my/package-enabled-p 'vdiff-magit))
  :config
  (bind-keys :map magit-mode-map
             ("e" . vdiff-magit-dwim)
             ("E" . vdiff-magit-popup))
  (setcdr (assoc ?e (plist-get magit-dispatch-popup :actions))
          '("vdiff dwim" 'vdiff-magit-dwim))
  (setcdr (assoc ?E (plist-get magit-dispatch-popup :actions))
          '("vdiff popup" 'vdiff-magit-popup)))


(provide 'my/init-util)