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
  :config
  (setq explicit-shell-file-name shell-file-name)
  (bind-keys :map term-raw-map
             ("C-q" . pkg/term/toggle-mode)
             :map term-mode-map
             ("C-q" . pkg/term/toggle-mode)))

(use-package eshell ;; emacs shell
  :commands (eshell)
  :config
  (setq eshell-directory-name (my/set-user-emacs-file ".eshell/")))

(use-package dired
  :preface
  (defun pkg/dired/count-marked ()
    (let* ((list (dired-get-marked-files nil nil nil t))
           (len (length list)))
      (cond
       ((> len 2) len)
       ((= len 2) (if (eq (car list) t) 1 2))
       (t 0))))
  :config
  (setq dired-recursive-deletes 'top
        dired-copy-preserve-time t))

(use-package icomplete
  :if (pkg/package/enabled-p 'icomplete)
  :config
  (icomplete-mode 1))

(use-package ido
  :if (pkg/package/enabled-p 'ido)
  :config
  (setq ido-enable-prefix t
        ido-enable-flex-matching t
        ido-use-filename-at-point t
        ido-enter-matching-directory nil)
  (ido-mode 1)
  ;; 仅使ido支持find-file和switch-to-buffer
  (ido-everywhere -1))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; 该插件会自动替换原M-x快捷键所绑定的命令，若想保留则可重新绑定之
         ("C-x M-x" . execute-extended-command))
  :if (pkg/package/enabled-p 'smex)
  :config
  (smex-initialize))

(use-package helm
  :diminish (helm-mode)
  :defer t
  :preface
  (defun pkg/helm/spacemacs/hide-minibuffer ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  :if (pkg/package/enabled-p 'helm)
  :init
  (require 'helm-config)
  :config
  (setq helm-split-window-inside-p t
        helm-always-two-windows nil
        helm-full-frame nil
        helm-use-frame-when-more-than-two-windows nil
        helm-autoresize-max-height 30
        helm-autoresize-min-height 10
        helm-display-header-line nil
        helm-echo-input-in-header-line t
        helm-prevent-escaping-from-minibuffer t
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-mode-fuzzy-match nil ;; globally disabled
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match nil
        helm-imenu-fuzzy-match t
        helm-etags-fuzzy-match nil
        helm-apropos-fuzzy-match t
        helm-move-to-line-cycle-in-source t
        helm-follow-input-idle-delay 0.5
        helm-follow-mode-persistent nil
        ;; helm-source-names-using-follow '("Occur")
        helm-buffer-skip-remote-checking t
        ;; 配置该参数可以指定不同的后台支持，包括imenu、ido、smex等
        ;; helm-completing-read-handlers-alist
        helm-imenu-execute-action-at-once-if-one nil
        helm-bookmark-show-location t
        helm-org-format-outline-path t)
  (add-hook 'helm-minibuffer-set-up-hook #'pkg/helm/spacemacs/hide-minibuffer)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package ivy
  :if (pkg/package/enabled-p 'ivy))

(use-package ediff
  :after (winner)
  :commands (ediff-current-file
             ediff-files
             ediff-files3
             ediff-buffers
             ediff-buffers3)
  :preface
  (defun pkg/ediff/setup-keymap ()
    ;; 可参考(ediff-setup-keymap)，或激活ediff后输入"?"
    ;; (bind-keys :map ediff-mode-map)
    )
  :if (pkg/package/enabled-p 'ediff)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-make-buffers-readonly-at-startup nil
        ;; ediff-diff-options "" ;; "-w" = "##", "-i" = "#c"
        ;; ediff-forward-word-function 'forward-char ;; "@", "*"
        ediff-highlight-all-diffs nil)
  (add-hook 'ediff-keymap-setup-hook #'pkg/ediff/setup-keymap t)
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo t))

(use-package vdiff
  :commands (vdiff-current-file
             vdiff-files
             vdiff-files3
             vdiff-buffers
             vdiff-buffers3)
  :if (pkg/package/enabled-p 'vdiff)
  :config
  (setq vdiff-diff-algorithm (if my/bin/git-command 'git-diff 'diff-u)
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
  :if (pkg/package/enabled-p 'projectile)
  :config
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-project-search-path my/project/root-directories
        projectile-cache-file (my/set-user-emacs-file ".projectile/cache")
        projectile-known-projects-file (my/set-user-emacs-file
                                        ".projectile/bookmarks"))
  (setq projectile-switch-project-action
        (lambda ()
          (my/run-pkg-hook "projectile/switch")
          (when projectile-switch-project-action
            (funcall projectile-switch-project-action))))
  ;; (add-to-list 'projectile-other-file-alist '("html" "js"))
  (projectile-mode 1))

(use-package helm-projectile
  :after (helm projectile)
  :if (pkg/package/enabled-p '(helm projectile helm-projectile))
  :config
  (setq projectile-completion-system 'helm
        helm-projectile-fuzzy-match t)
  (my/add-pkg-hook "projectile/switch" #'helm-projectile)
  (helm-projectile-on))

(use-package magit
  :commands (magit-status)
  :if (and (pkg/package/enabled-p 'magit) my/bin/git-command)
  :config
  (setq magit-auto-revert-mode t
        magit-auto-revert-immediately t
        magit-auto-revert-tracked-only t
        ;; magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        ;; 执行(magit-list-repositories)，可以打印出以下列表所指示的路径下搜索到的git项目
        magit-repository-directories (my/map (lambda (dir) (cons dir 1))
                                             my/project/root-directories)
        magit-ediff-dwim-show-on-hunks t
        magit-diff-section-arguments '("--ignore-space-change")))

(use-package vdiff-magit
  :after (magit)
  :if (pkg/package/enabled-p '(vdiff magit vdiff-magit))
  :config
  (bind-keys :map magit-mode-map
             ("e" . vdiff-magit-dwim)
             ("E" . vdiff-magit-popup))
  (setcdr (assoc ?e (plist-get magit-dispatch-popup :actions))
          '("vdiff dwim" #'vdiff-magit-dwim))
  (setcdr (assoc ?E (plist-get magit-dispatch-popup :actions))
          '("vdiff popup" #'vdiff-magit-popup)))


(provide 'my/init/utility)
