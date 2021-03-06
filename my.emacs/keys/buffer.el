;; -*- coding: utf-8 -*-

(bind-keys ("C-M-v" . nil) ;; (scroll-other-window)
           ("C-x <left>" . nil) ;; (previous-buffer)
           ("C-x <right>" . nil) ;; (next-buffer)
           ("C-x o" . nil) ("M-o" . other-window)
           ("M-r" . nil) ("M-l" . move-to-window-line-top-bottom))

(when (pkg/package/enabled-p 'zoom)
  (bind-keys ("C-+" . zoom)))

(when (pkg/package/enabled-p 'ace-window)
  ;; "C-u M-o", "C-u C-u M-o"
  (bind-keys ("M-o" . ace-window)))

(defun pkg/hydra/group/tabbar/alternate (func)
  (let ((alist '((tabbar-forward-tab . (awesome-tab . awesome-tab-forward-tab))
                 (tabbar-backward-tab . (awesome-tab . awesome-tab-backward-tab))
                 (tabbar-forward-group . (awesome-tab . awesome-tab-forward-group))
                 (tabbar-backward-group . (awesome-tab . awesome-tab-backward-group)))))
    (funcall (pkg/hydra/alternate-key func alist))))

(bind-keys ("<C-left>" . (lambda () (interactive)
                           (pkg/hydra/group/tabbar/alternate #'tabbar-backward-tab)))
           ("<C-right>" . (lambda () (interactive)
                            (pkg/hydra/group/tabbar/alternate #'tabbar-forward-tab)))
           ("<C-up>" . (lambda () (interactive)
                         (pkg/hydra/group/tabbar/alternate #'tabbar-backward-group)))
           ("<C-down>" . (lambda () (interactive)
                           (pkg/hydra/group/tabbar/alternate #'tabbar-forward-group))))

(defhydra pkg/hydra/group/buffer
  (:timeout pkg/hydra/timeout-sec)
  ("b"       (pkg/hydra/group/tabbar/alternate
              #'tabbar-backward-tab)     "left      " :column "tab bar     ")
  ("f"       (pkg/hydra/group/tabbar/alternate
              #'tabbar-forward-tab)      "right     "                       )
  ("<"       awesome-tab-select-beg-tab  "first     "                       )
  (">"       awesome-tab-select-end-tab  "last      "                       )
  ("n"       (pkg/hydra/group/tabbar/alternate
              #'tabbar-forward-group)    "next group"                       )
  ("p"       (pkg/hydra/group/tabbar/alternate
              #'tabbar-backward-group)   "prev group"                       )
  ("C-h"     buf-move-left               "left      " :column "move        ")
  ("C-l"     buf-move-right              "right     "                       )
  ("C-k"     buf-move-up                 "up        "                       )
  ("C-j"     buf-move-down               "down      "                       )
  ("M-p"     scroll-other-window-down    "up        " :column "scroll other")
  ("M-n"     scroll-other-window         "down      "                       )
  ("u"       winner-undo                 "undo      " :column "layout      ")
  ("r"       winner-redo                 "redo      "                       )
  ("<left>"  enlarge-window-horizontally "↔ ++      " :column "size        ")
  ("<right>" shrink-window-horizontally  "↔ --      "                       )
  ("<up>"    enlarge-window              "↕ ++      "                       )
  ("<down>"  shrink-window               "↕ --      "                       )
  ("+"       text-scale-increase         "++        " :column "text scale  ")
  ("_"       text-scale-decrease         "--        "                       )
  ("q" pkg/hydra/quit nil :exit t))

(use-package tabbar
  :defer t
  :config
  (define-key tabbar-mode-map tabbar-prefix-key nil))


(bind-keys ("C-M-n" . nil) ;; (forward-list)
           ("C-M-p" . nil) ;; (backward-list)
           ("C-M-a" . nil) ;; (beginning-of-defun)
           ("C-M-e" . nil) ;; (end-of-defun)
           ("C-SPC" . nil) ;; (set-mark-command)
           ("C-x SPC" . nil) ;; (rectangle-mark-mode)
           ("M-f" . forward-word)
           ("M-b" . backward-word)
           ("M-e" . forward-sentence)
           ("M-a" . backward-sentence)
           ("M-{" . nil) ("C-M-e" . forward-paragraph)
           ("M-}" . nil) ("C-M-a" . backward-paragraph)
           ("C-M-f" . forward-sexp)
           ("C-M-b" . backward-sexp)
           ("C-M-l" . up-list)
           ("C-M-d" . nil) ("C-M-j" . down-list)
           ("C-M-u" . nil) ("C-M-h" . backward-up-list)
           ("C-SPC" . pkg/hydra/group/cursor/mark/body))

(cond
 ((pkg/package/enabled-p 'avy)
  (bind-keys ("C-:" . avy-goto-char-timer) ;; (avy-goto-char)
             ("C-;" . avy-pop-mark)))
 ((pkg/package/enabled-p 'ace-jump-mode)
  (bind-keys ("C-:" . ace-jump-char-mode)
             ("C-;" . ace-jump-mode-pop-mark))))

(cond
 ((pkg/package/enabled-p 'move-text)
  (bind-keys ("M-n" . move-text-down)
             ("M-p" . move-text-up)))
 ((pkg/package/enabled-p 'move-dup)
  (bind-keys ("M-n" . md-move-lines-down)
             ("M-p" . md-move-lines-up)
             ("M-S-n" . md-duplicate-up)
             ("M-S-p" . md-duplicate-down))))

(when (pkg/package/enabled-p 'multiple-cursors)
  (bind-keys ("C-S-n" . mc/mark-next-like-this)
             ("C-S-p" . mc/mark-previous-like-this)))

(defhydra pkg/hydra/group/cursor
  (:timeout pkg/hydra/timeout-sec :exit t)
  ("C-    f,b"    nil "char          " :column "forward/backward")
  ("   M- f,b"    nil "word          "                           )
  ("C-    e,a"    nil "line          " :column "beginning/end   ")
  ("   M- e,a"    nil "sentence      "                           )
  ("C- M- e,a"    nil "paragraph     "                           )
  ("C- M- f,b"    nil "sexp          " :column "s-expression    ")
  ("C- M- j,k"    nil "tree down     "                           )
  ("C- M- l,h"    nil "tree up       "                           ))

(use-package paredit
  :defer t
  :config
  (bind-keys :map paredit-mode-map
             ("C-M-f" . paredit-forward)
             ("C-M-b" . paredit-backward)
             ("C-M-n" . nil) ("C-M-l" . paredit-forward-up)
             ("C-M-p" . nil) ("C-M-k" . paredit-backward-down)
             ("C-M-d" . nil) ("C-M-j" . paredit-forward-down)
             ("C-M-u" . nil) ("C-M-h" . paredit-backward-up)))

(use-package lispy
  :defer t
  :config
  (setq lispy-mode-map-base
        (let ((map (make-sparse-keymap)))
          (bind-keys :map map
                     ("C-a" . lispy-move-beginning-of-line)
                     ("C-e" . lispy-move-end-of-line)
                     ("C-k" . lispy-kill)
                     ("M-d" . lispy-kill-word)
                     ("<C-backspace>" . lispy-backward-kill-word)
                     ("(" . lispy-parens))
          map))
  (setq lispy-mode-map-special
        (let ((map (make-sparse-keymap)))
          ;; navigation
          (lispy-define-key map "f" 'lispy-forward)
          (lispy-define-key map "b" 'lispy-backward)
          (lispy-define-key map "h" 'lispy-left)
          (lispy-define-key map "l" 'lispy-right)
          (lispy-define-key map "j" 'lispy-flow)
          (lispy-define-key map "k" 'lispy-flow)
          (lispy-define-key map ":" 'lispy-ace-paren)
          (lispy-define-key map "s" 'lispy-goto-local)
          (lispy-define-key map "S" 'lispy-goto-symbol)
          (lispy-define-key map "." 'lispy-follow t)
          (lispy-define-key map "," 'pop-tag-mark)
          ;; edit
          (lispy-define-key map "n" 'lispy-move-down)
          (lispy-define-key map "p" 'lispy-move-up)
          (lispy-define-key map "w" 'lispy-new-copy)
          (lispy-define-key map "W" 'lispy-kill-at-point)
          (lispy-define-key map "y" 'lispy-clone)
          (lispy-define-key map "u" 'lispy-undo)
          (lispy-define-key map "i" 'lispy-tab)
          (lispy-define-key map ">" 'lispy-slurp)
          (lispy-define-key map "<" 'lispy-barf)
          (lispy-define-key map "/" 'lispy-splice)
          (lispy-define-key map "+" 'lispy-join)
          (lispy-define-key map "D" 'lispy-ace-symbol-replace)
          ;; (lispy-define-key map "" 'lispy-oneline)
          ;; (lispy-define-key map "" 'lispy-alt-multiline)
          ;; misc
          (lispy-define-key map "m" 'lispy-mark-list)
          (lispy-define-key map "M" 'lispy-ace-symbol
            :override '(cond ((looking-at lispy-outline)
                              (lispy-meta-return))))
          (lispy-define-key map "o" 'lispy-occur)
          (lispy-define-key map "a" 'lispy-widen)
          (lispy-define-key map "A" 'lispy-narrow)
          (define-key map (kbd "SPC") 'lispy-space)
          map))
  (setq lispy-mode-map-lispy
        (let ((map (copy-keymap lispy-mode-map-base)))
          (define-key map (kbd "]") 'lispy-forward)
          (define-key map (kbd "[") 'lispy-backward)
          (define-key map (kbd "{") 'lispy-braces)
          (define-key map (kbd "}") 'lispy-brackets)
          (define-key map (kbd ")") 'lispy-right-nostring)
          (define-key map (kbd "\"") 'lispy-doublequote) ;; (lispy-quotes)
          (define-key map (kbd "C-d") 'lispy-forward-delete)
          (define-key map (kbd "<C-return>") 'lispy-open-line)
          map))
  (lispy-set-key-theme '(special lispy c-digits))
  ;; TODO
  ;; (lispy-define-key lispy-mode-map "?"
  ;;                     (defhydra pkg/hydra/group/lispy-help
  ;;                       (lispy-mode-map "" :timeout pkg/hydra/timeout-sec :exit t)
  ;;                       ("q" nil nil :exit)))
  )

(defhydra pkg/hydra/group/cursor/mark
  (:timeout pkg/hydra/timeout-sec :exit t)
  ("C-SPC"       set-mark-command                 "continuous  " :column "mark   "          )
  ("C-S-SPC"     rectangle-mark-mode              "rectangle   "                            )
  ("a"           mark-whole-buffer                "whole buffer"                            )
  ("SPC"         er/expand-region                 "expand      " :column "syntax " :exit nil)
  ("<backspace>" er/contract-region               "contract    "                   :exit nil)
  (":"           ace-mc-add-multiple-cursors      "jump others " :column "cursors"          )
  (";"           ace-mc-add-single-cursor         "jump another"                            )
  ("n"           parrot-rotate-next-word-at-point "next"         :column "rotate " :exit nil)
  ("p"           parrot-rotate-prev-word-at-point "prev"                           :exit nil)
  ("q" pkg/hydra/quit nil :exit t))

(use-package multiple-cursors
  :defer t
  :config
  (bind-keys :map mc/keymap
             ("<return>" . nil)
             ("C-v" . nil) ("M-n" . mc/cycle-forward)
             ("M-v" . nil) ("M-p" . mc/cycle-backward)
             ("C-'" . nil) ("C-q" . mc-hide-unmatched-lines-mode)
             :map rectangle-mark-mode-map
             ("<return>" . mc/edit-lines)))


(provide 'my/keys/buffer)
