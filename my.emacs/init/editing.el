;; -*- coding: utf-8 -*-

(use-package ace-window
  :defer t
  :if (pkg/package/enabled-p 'ace-window)
  :init
  (setq aw-background t
        aw-char-position 'top-left
        aw-ignore-current t
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package buffer-move
  :commands (buf-move-left
             buf-move-right
             buf-move-up
             buf-move-down)
  :if (pkg/package/enabled-p 'buffer-move))

(use-package avy
  :defer t
  :if (pkg/package/enabled-p 'avy)
  :init
  (setq avy-timeout-seconds 0.5)
  :config
  ;; (avy-setup-default)
  )

(use-package ace-jump-mode
  :defer t
  :if (pkg/package/enabled-p 'ace-jump-mode)
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package ace-pinyin
  :after `(,pkg/ace-pinyin/backend)
  :preface
  (defconst pkg/ace-pinyin/backend
    (or (pkg/package/enabled-p 'avy)
        (pkg/package/enabled-p 'ace-jump-mode)))
  :if (and pkg/ace-pinyin/backend
           (pkg/package/enabled-p 'ace-pinyin))
  :init
  (setq ace-pinyin-use-avy (eq pkg/ace-pinyin/backend 'avy))
  :config
  (ace-pinyin-global-mode 1))

(use-package ace-link
  :after (:any help-mode info woman eww compile custom)
  :if (pkg/package/enabled-p 'ace-link)
  :config
  (ace-link-setup-default ":"))

(use-package undo-tree
  :diminish (undo-tree-mode)
  :if (pkg/package/enabled-p 'undo-tree)
  :init
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode 1))

(use-package smart-hungry-delete
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :if (pkg/package/enabled-p 'smart-hungry-delete)
  :config
  (smart-hungry-delete-add-default-hooks))

(use-package paredit
  :diminish (paredit-mode)
  :defer t
  :preface
  (defun pkg/paredit/start ()
    (enable-paredit-mode))
  :if (pkg/package/enabled-p 'paredit)
  :init
  (dolist (mode '("org" "lisp" "elisp" "ilisp" "slime" "scheme"))
    (my/add-mode-hook mode #'pkg/paredit/start)))

(use-package lispy
  :diminish (lispy-mode)
  :defer t
  :preface
  (defun pkg/lispy/start ()
    (lispy-mode 1))
  (defconst pkg/lispy/show-backend
    (or (pkg/package/enabled-p 'helm)
        (pkg/package/enabled-p 'ivy)
        'default))
  :if (pkg/package/enabled-p 'lispy)
  :init
  (setq lispy-no-permanent-semantic t
        lispy-close-quotes-at-end-p t
        lispy-completion-method pkg/lispy/show-backend
        lispy-occur-backend pkg/lispy/show-backend)
  (dolist (mode '("org" "lisp" "elisp" "ilisp" "slime" "scheme"))
    (my/add-mode-hook mode #'pkg/lispy/start))
  :config
  (use-package semantic ;; FIXME: workaround for a semantic bug
    :defer t
    :config
    (advice-add 'semantic-idle-scheduler-function :around #'ignore)))

(use-package parinfer
  :defer t
  :preface
  (defun pkg/parinfer/start ()
    (parinfer-mode 1))
  :if (pkg/package/enabled-p 'parinfer)
  :init
  (setq parinfer-extensions '(defaults
                               pretty-parens
                               smart-yank
                               smart-tab
                               paredit)
        parinfer-auto-switch-indent-mode nil
        parinfer-auto-switch-indent-mode-when-closing nil
        parinfer-delay-invoke-idle 1.0)
  (dolist (mode '("lisp" "elisp" "ilisp" "scheme"))
    (my/add-mode-hook mode #'pkg/parinfer/start)))

(use-package expand-region
  :preface
  (defun pkg/expand-region/text-mode ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append er/try-expand-list
                                     '(mark-paragraph mark-page))))
  :if (pkg/package/enabled-p 'expand-region)
  :init
  (setq expand-region-smart-cursor t)
  :config
  (er/enable-mode-expansions 'text-mode 'pkg/expand-region/text-mode))

(use-package multiple-cursors
  ;; 'region-bindings-mode is not recommended when 'lispy is enabled
  :if (pkg/package/enabled-p 'multiple-cursors)
  :init
  (setq mc/list-file (my/set-user-emacs-file ".multiple-cursors.el")
        mc/always-repeat-command t
        mc/always-run-for-all t
        mc/cycle-looping-behaviour 'stop
        mc/edit-lines-empty-lines nil
        mc/max-cursors 100))

(use-package flyspell
  :diminish (flyspell-mode)
  :defer t
  :if (and (pkg/package/enabled-p 'flyspell)
           (my/locate-exec "aspell"))
  :init
  (setq ispell-program-name (my/locate-exec "aspell") ;; 设置后台支持程序
        ;; ispell-dictionary "english" ;; default dictionary
        ;; ispell-personal-dictionary ""
        flyspell-issue-message-flag nil)
  (my/add-mode-hook "text" #'flyspell-mode)
  (my/add-mode-hook "prog" #'flyspell-prog-mode)
  :config
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN" . "^#+END") t))

(use-package flyspell-correct
  :after (flyspell)
  :if (and (pkg/package/enabled-p 'flyspell)
           (pkg/package/enabled-p 'flyspell-correct))
  :config
  (use-package flyspell-correct-helm
    :after (helm)
    :if (and (pkg/package/enabled-p 'helm)
             (pkg/package/enabled-p 'flyspell-correct-helm))
    :init
    (setq flyspell-correct-interface #'flyspell-correct-helm))
  (use-package flyspell-correct-ivy
    :after (ivy)
    :if (and (pkg/package/enabled-p 'ivy)
             (pkg/package/enabled-p 'flyspell-correct-ivy))
    :init
    (setq flyspell-correct-interface #'flyspell-correct-ivy)))

(use-package hydra
  :defer t
  :preface
  (defconst pkg/hydra/timeout-sec 30)
  (defun pkg/hydra/quit ()
    (interactive)
    (message "Hydra Quit"))
  :if (pkg/package/enabled-p 'hydra)
  :init
  ;; 目前发现启用此项会导致，Hydra子窗口过小，无法完整地呈现提示文字
  ;; 此外，启用全局的zoom mode似乎也可以避免该问题
  (setq hydra-lv nil))


(provide 'my/init/editing)
