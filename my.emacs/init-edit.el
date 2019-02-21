;; -*- coding: utf-8 -*-

(defun my/init-edit/init ()
  (pkg/windmove/init)
  (pkg/buffer-move/init)
  (pkg/avy/init)
  (pkg/ace-jump-mode/init)
  (pkg/ace-pinyin/init)
  (pkg/undo-tree/init)
  (pkg/smart-hungry-delete/init)
  (pkg/paredit/init)
  (pkg/lispy/init)
  (pkg/parinfer/init)
  (pkg/flyspell/init)
  (pkg/flyspell-correct/init)
  (pkg/hydra/init))

(my/add-mode-hook "init" #'my/init-edit/init)


(defun pkg/windmove/init ()
  (use-package windmove
    :commands (windmove-left
               windmove-right
               windmove-up
               windmove-down)
    :if (my/package-enabled-p 'windmove)
    :config
    (windmove-default-keybindings)))

(defun pkg/buffer-move/init ()
  (use-package buffer-move
    :commands (buf-move-left
               buf-move-right
               buf-move-up
               buf-move-down)
    :if (my/package-enabled-p 'buffer-move)))

(defun pkg/avy/init ()
  (use-package avy
    :bind (("C-:" . avy-goto-char-timer) ;; (avy-goto-char)
           ("C-;" . avy-pop-mark))
    :if (my/package-enabled-p 'avy)
    :init
    (setq avy-timeout-seconds 0.5)
    :config
    ;; (avy-setup-default)
    ))

(defun pkg/ace-jump-mode/init ()
  (use-package ace-jump-mode
    :bind (("C-:" . ace-jump-char-mode)
           ("C-;" . ace-jump-mode-pop-mark))
    :if (my/package-enabled-p 'ace-jump-mode)
    :config
    (ace-jump-mode-enable-mark-sync)))

(defun pkg/ace-pinyin/init ()
  (use-package ace-pinyin
    :after `(,pkg/ace-pinyin/backend)
    :preface
    (defconst pkg/ace-pinyin/backend
      (or (my/package-enabled-p 'avy)
          (my/package-enabled-p 'ace-jump-mode)))
    :if (and pkg/ace-pinyin/backend
             (my/package-enabled-p 'ace-pinyin))
    :init
    (setq ace-pinyin-use-avy (eq pkg/ace-pinyin/backend 'avy))
    :config
    (ace-pinyin-global-mode 1)))

(defun pkg/undo-tree/init ()
  (use-package undo-tree
    :diminish (undo-tree-mode)
    :if (my/package-enabled-p 'undo-tree)
    :init
    (setq undo-tree-visualizer-diff nil
          undo-tree-visualizer-relative-timestamps nil)
    :config
    (global-undo-tree-mode 1)))

(defun pkg/smart-hungry-delete/init ()
  (use-package smart-hungry-delete
    :bind (("<backspace>" . smart-hungry-delete-backward-char)
           ("C-d" . smart-hungry-delete-forward-char))
    :if (my/package-enabled-p 'smart-hungry-delete)
    :config
    (smart-hungry-delete-add-default-hooks)))

(defun pkg/paredit/init ()
  (use-package paredit
    :diminish (paredit-mode)
    :defer t
    :preface
    (defun pkg/paredit/start ()
      (enable-paredit-mode))
    :if (my/package-enabled-p 'paredit)
    :init
    (dolist (mode '("org" "lisp" "elisp" "ilisp" "slime" "scheme"))
      (my/add-mode-hook mode #'pkg/paredit/start))))

(defun pkg/lispy/init ()
  (use-package lispy
    :diminish (lispy-mode)
    :defer t
    :preface
    (defun pkg/lispy/start ()
      (lispy-mode 1))
    (defconst pkg/lispy/show-backend
      (or (my/package-enabled-p 'helm)
          (my/package-enabled-p 'ivy)
          'default))
    :if (my/package-enabled-p 'lispy)
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
      (advice-add 'semantic-idle-scheduler-function :around #'ignore))))

(defun pkg/parinfer/init ()
  (use-package parinfer
    :defer t
    :preface
    (defun pkg/parinfer/start ()
      (parinfer-mode 1))
    :if (my/package-enabled-p 'parinfer)
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
      (my/add-mode-hook mode #'pkg/parinfer/start))))

(defun pkg/flyspell/init ()
  (use-package flyspell
    :diminish (flyspell-mode)
    :defer t
    :if (and (my/package-enabled-p 'flyspell)
             (my/locate-exec "aspell"))
    :init
    (setq ispell-program-name (my/locate-exec "aspell") ;; 设置后台支持程序
          ;; ispell-dictionary "english" ;; default dictionary
          ;; ispell-personal-dictionary ""
          flyspell-issue-message-flag nil)
    (my/add-mode-hook "text" #'flyspell-mode)
    (my/add-mode-hook "prog" #'flyspell-prog-mode)
    :config
    (add-to-list 'ispell-skip-region-alist '("^#+BEGIN" . "^#+END") t)))

(defun pkg/flyspell-correct/init ()
  (use-package flyspell-correct
    :after (flyspell)
    :if (and (my/package-enabled-p 'flyspell)
             (my/package-enabled-p 'flyspell-correct))
    :config
    (use-package flyspell-correct-helm
      :after (helm)
      :if (and (my/package-enabled-p 'helm)
               (my/package-enabled-p 'flyspell-correct-helm))
      :init
      (setq flyspell-correct-interface #'flyspell-correct-helm))
    (use-package flyspell-correct-ivy
      :after (ivy)
      :if (and (my/package-enabled-p 'ivy)
               (my/package-enabled-p 'flyspell-correct-ivy))
      :init
      (setq flyspell-correct-interface #'flyspell-correct-ivy))))

(defun pkg/hydra/init ()
  (use-package hydra
    :defer t
    :preface
    (defconst pkg/hydra/timeout-sec 30)
    (defun pkg/hydra/quit ()
      (interactive)
      (message "Hydra Quit"))
    :if (my/package-enabled-p 'hydra)
    :init
    ;; 目前发现启用此项会导致，Hydra子窗口过小，无法完整地呈现提示文字
    ;; 此外，启用全局的zoom mode似乎也可以避免该问题
    (setq hydra-lv nil)))


(provide 'my/init-edit)
