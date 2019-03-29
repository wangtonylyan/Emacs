;; -*- coding: utf-8 -*-

(use-package ace-window
  :defer t
  :if (pkg/package/enabled-p 'ace-window)
  :config
  (setq aw-background t
        aw-char-position 'top-left
        aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-ignore-current t
        aw-ignore-on t)
  (dolist (buffer '(magit-status-mode
                    neotree-mode))
    (add-to-list 'aw-ignored-buffers buffer)))

(use-package buffer-move
  :commands (buf-move-left
             buf-move-right
             buf-move-up
             buf-move-down)
  :if (pkg/package/enabled-p 'buffer-move))

(use-package avy
  :defer t
  :if (pkg/package/enabled-p 'avy)
  :config
  (setq avy-timeout-seconds 0.5)
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
  :config
  (setq ace-pinyin-use-avy (eq pkg/ace-pinyin/backend 'avy))
  (ace-pinyin-global-mode 1))

(use-package ace-link
  :after (:any help-mode info woman eww compile custom)
  :if (pkg/package/enabled-p 'ace-link)
  :config
  (ace-link-setup-default ":"))

(use-package undo-tree
  :diminish (undo-tree-mode)
  :if (pkg/package/enabled-p 'undo-tree)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-auto-save-history nil)
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
  (dolist (mode '("lisp" "elisp" "ilisp" "slime" "scheme"))
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
  (dolist (mode '("lisp" "elisp" "ilisp" "slime" "scheme"))
    (my/add-mode-hook mode #'pkg/lispy/start))
  :config
  (setq lispy-no-permanent-semantic t
        lispy-close-quotes-at-end-p t
        lispy-completion-method pkg/lispy/show-backend
        lispy-occur-backend pkg/lispy/show-backend)
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
  (dolist (mode '("lisp" "elisp" "ilisp" "scheme"))
    (my/add-mode-hook mode #'pkg/parinfer/start))
  :config
  (setq parinfer-extensions '(defaults
                               pretty-parens
                               smart-yank
                               smart-tab
                               paredit)
        parinfer-auto-switch-indent-mode nil
        parinfer-auto-switch-indent-mode-when-closing nil
        parinfer-delay-invoke-idle 1.0))

(use-package expand-region
  :preface
  (defun pkg/expand-region/text-mode ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append er/try-expand-list
                                     '(mark-paragraph mark-page))))
  :if (pkg/package/enabled-p 'expand-region)
  :config
  (setq expand-region-smart-cursor t)
  (er/enable-mode-expansions 'text-mode 'pkg/expand-region/text-mode))

(use-package multiple-cursors
  ;; 'region-bindings-mode is not recommended when 'lispy is enabled
  :if (pkg/package/enabled-p 'multiple-cursors)
  :config
  (setq mc/list-file (my/set-user-emacs-file ".multiple-cursors.el")
        mc/always-repeat-command t
        mc/always-run-for-all t
        mc/cycle-looping-behaviour 'stop
        mc/edit-lines-empty-lines nil
        mc/max-cursors 100))

(use-package ace-mc
  :defer t
  :if (pkg/package/enabled-p 'ace-mc))

(use-package flyspell
  :diminish (flyspell-mode)
  :defer t
  :if (and (pkg/package/enabled-p 'flyspell)
           (my/locate-exec "aspell"))
  :init
  (my/add-mode-hook "text" #'flyspell-mode)
  (my/add-mode-hook "prog" #'flyspell-prog-mode)
  :config
  (setq ispell-program-name (my/locate-exec "aspell") ;; 设置后台支持程序
        ;; ispell-dictionary "english" ;; default dictionary
        ;; ispell-personal-dictionary ""
        flyspell-issue-message-flag nil)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN" . "^#+END") t))

(use-package flyspell-correct
  :after (flyspell)
  :if (pkg/package/enabled-p '(flyspell flyspell-correct))
  :config
  (use-package flyspell-correct-helm
    :after (helm)
    :if (pkg/package/enabled-p '(helm flyspell-correct-helm))
    :config
    (setq flyspell-correct-interface #'flyspell-correct-helm))
  (use-package flyspell-correct-ivy
    :after (ivy)
    :if (pkg/package/enabled-p '(ivy flyspell-correct-ivy))
    :config
    (setq flyspell-correct-interface #'flyspell-correct-ivy)))

(use-package hydra
  :defer t
  :preface
  (defconst pkg/hydra/timeout-sec 30)
  (defun pkg/hydra/quit ()
    (interactive)
    (message "Hydra Quit"))
  (defun pkg/hydra/alternate-key (key alist)
    (let ((value (alist-get key alist)))
      (or (my/mapcar (lambda (x)
                       (when (and (my/pairp x)
                                  (pkg/package/enabled-p (car x)))
                         (cdr x)))
                     (if (atom (car value)) (list value) value))
          key)))
  :if (pkg/package/enabled-p 'hydra)
  :config
  ;; 目前发现启用此项会导致，Hydra子窗口过小，无法完整地呈现提示文字
  ;; 此外，启用全局的zoom mode似乎也可以避免该问题
  (setq hydra-lv nil))

(use-package which-key
  :diminish (which-key-mode)
  :if (pkg/package/enabled-p 'which-key)
  :config
  (setq which-key-idle-delay 0.5
        which-key-separator " "
        which-key-max-description-length 30
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-allow-multiple-replacements t)
  (dolist (replace '((("<up>") . ("↑")) (("<down>") . ("↓"))
                     ((nil . "RET") . (nil . "⏎"))
                     ((nil . "^pkg/hydra/") . (nil . "η"))
                     ((nil . "^pkg/hydra/group") . (nil . "η"))
                     ((nil . "\\`\\?\\?\\'") . (nil . "λ"))))
    (add-to-list 'which-key-replacement-alist replace))
  (which-key-mode 1))

(use-package pyim ;; 暂时感觉体验不好，还是倾向于使用搜狗
  :defer t
  :if (pkg/package/enabled-p 'pyim)
  :config
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'quanpin
        pyim-translate-trigger-char "v"
        pyim-page-tooltip (or (pkg/package/enabled-p 'posframe) 'popup)
        pyim-page-length 5
        pyim-auto-select nil
        pyim-dcache-directory (my/set-user-emacs-file "my.pyim/"))
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-bind)
                pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; (pyim-isearch-mode 1)
  (use-package pyim-basedict
    :if (pkg/package/enabled-p 'pyim-basedict)
    :config
    (pyim-basedict-enable)))


(provide 'my/init/edit)
