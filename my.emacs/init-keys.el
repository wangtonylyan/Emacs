;; -*- coding: utf-8 -*-

(defun my/init-keys/init ()
  (my/init-keys/built-in)
  (my/init-keys/package)
  (my/init-keys/tabbar)
  (my/init-keys/helm)
  (my/init-keys/undo-tree)
  (my/init-keys/flyspell)
  (my/init-keys/hydra/group)
  (my/init-keys/hydra/group/window)
  (my/init-keys/hydra/group/cursor)
  (my/init-keys/paredit)
  (my/init-keys/lispy)
  (my/init-keys/dired)
  (my/init-keys/treemacs)
  (my/init-keys/neotree)
  (my/init-keys/highlight)
  (my/init-keys/bookmark)
  (my/init-keys/hydra/group/diff)
  (my/init-keys/ediff)
  (my/init-keys/vdiff)
  (my/init-keys/org)
  (my/init-keys/projectile)
  (my/init-keys/helm-projectile)
  (my/init-keys/flymake)
  (my/init-keys/flycheck)
  (my/init-keys/ggtags)
  (my/init-keys/helm-gtags)
  (my/init-keys/company)
  (my/init-keys/cedet))

(my/add-mode-hook "init" #'my/init-keys/init)


;; 在foo-mode-map中绑定("x y" . foo)
;; (defhydra foo (foo-mode-map "x") ("y" . foo))
;; 在global-map中新增快捷键前缀"x"
;; (bind-key "x" #'foo/body global-map)

;; 与输入法切换键冲突
;; (global-set-key (kbd "C-S-SPC") 'set-mark-command)
;; (global-unset-key (kbd "C-SPC"))

(defun my/init-keys/built-in ()
  ;; 以下部分是重复绑定，目的是便于查阅
  (bind-keys
   ("C-x f"          . nil) ;; (set-fill-column)
   ("C-x C-l"        . nil) ;; (downcase-region)
   ("C-x C-u"        . nil) ;; (upcase-region)
   ("C-M-v"          . nil) ;; (scroll-other-window)
   ("M-s h"          . nil)
   ("C-x o"          . nil) ;; (other-window)
   ("C-x <left>"     . nil) ;; (previous-buffer)
   ("C-x <right>"    . nil) ;; (next-buffer)
   ("M-{"            . nil) ;; (forward-paragraph)
   ("M-}"            . nil) ;; (backward-paragraph)
   ("C-M-n"          . nil) ;; (forward-list)
   ("C-M-p"          . nil) ;; (backward-list)
   ("C-M-d"          . nil) ;; (down-list)
   ("C-M-u"          . nil) ;; (backward-up-list)
   ("C-M-a"          . nil) ;; (beginning-of-defun)
   ("C-M-e"          . nil) ;; (end-of-defun)
   ("C-M-k"          . nil) ;; (kill-sexp)
   ("C-x d"          . nil) ;; (dired)
   ("C-s"            . nil) ;; (isearch-forward)
   ("C-r"            . nil) ;; (isearch-backward)
   ("M-/"            . nil) ;; (dabbrev-expand)
   ("C-_"            . nil) ;; (undo), (undo-only)
   ("C-/"            . nil) ;; (undo), (undo-only)
   ("M-c"            . nil) ;; (capitalize-word)
   ("M-m"            . nil) ;; (back-to-indentation)
   ("M-j"            . nil) ;; (indent-new-comment-line)
   ("C-M-x"          . nil) ;; (eval-defun)
   ("C-x e"          . nil) ;; ((kmacro-end-and-call-macro)
   ("C-h h"          . nil) ;; (view-hello-file)
   ("C-h f"          . nil) ;; (describe-function)
   ("C-h v"          . nil) ;; (describe-variable)
   ("C-S-h"          . windmove-left           )
   ("C-S-l"          . windmove-right          )
   ("C-S-k"          . windmove-up             )
   ("C-S-j"          . windmove-down           )
   ("<S-left>"       . windmove-left           )
   ("<S-right>"      . windmove-right          )
   ("<S-up>"         . windmove-up             )
   ("<S-down>"       . windmove-down           )
   ("M-f"            . forward-word            )
   ("M-b"            . backward-word           )
   ("M-e"            . forward-sentence        )
   ("M-a"            . backward-sentence       )
   ("C-M-e"          . forward-paragraph       )
   ("C-M-a"          . backward-paragraph      )
   ("C-M-f"          . forward-sexp            )
   ("C-M-b"          . backward-sexp           )
   ("C-M-l"          . up-list                 )
   ("C-M-j"          . down-list               )
   ("C-M-h"          . backward-up-list        )
   ("C-+"            . zoom                    )
   ("<C-wheel-up>"   . text-scale-increase     )
   ("<C-wheel-down>" . text-scale-decrease     )
   ("<C-up>"         . text-scale-increase     )
   ("<C-down>"       . text-scale-decrease     )
   ("C-x C--"        . downcase-region         )
   ("C-x C-="        . upcase-region           )
   ("C-S-a"          . mark-whole-buffer       )
   ("C-q"            . read-only-mode          )
   ("M-!"            . shell-command           )
   ("M-."            . xref-find-definitions   )
   ("M-,"            . xref-pop-marker-stack   )
   ;; ("C-:")        ;; avy, ace-jump-mode
   ;; ("C-;")        ;; avy, ace-jump-mode
   ("M-j"            . pkg/hydra/group/body    ))
  (use-package kmacro
    :defer t
    :config
    (bind-keys ("C-x C-k" . ;; (kmacro-keymap)
                (lambda () (interactive) ;; need no confirm
                  (kill-buffer))))))

(defun my/init-keys/package ()
  (use-package package
    :defer t
    :config
    (bind-keys :map package-menu-mode-map
               ("r" . nil)
               ("g" . package-menu-refresh)
               ("G" . package-refresh-contents))))

(defun my/init-keys/tabbar ()
  (use-package tabbar
    :defer t
    :config
    (define-key tabbar-mode-map tabbar-prefix-key nil)
    (bind-keys :map tabbar-mode-map
               ("C-S-f" . tabbar-forward-tab)
               ("C-S-b" . tabbar-backward-tab)
               ("C-S-n" . tabbar-forward-group)
               ("C-S-p" . tabbar-backward-group))))

(defun my/init-keys/helm ()
  (defhydra pkg/hydra/group/helm
    (global-map "" :timeout pkg/hydra/timeout-sec :exit t)
    ("M-x"     helm-M-x               "M-x              " :column "emacs   ")
    (""        helm-colors            "colors           "                   )
    (""        helm-calcul-expression "calcul-expression"                   )
    ("M-y"     helm-show-kill-ring    "show-kill-ring   " :column "ring    ")
    ("C-h SPC" helm-all-mark-rings    "all-mark-rings   "                   )
    ("C-x C-f" helm-find-files        "find-files       " :column "file    ")
    ("C-x C-r" helm-recentf           "recentf          "                   )
    ("C-x b"   helm-mini              "mini             " :column "buffer  ")
    ("C-x C-b" helm-buffers-list      "buffers-list     "                   )
    ("C-o"     helm-occur             "occur            " :column "symbol  ")
    ("C-S-o"   helm-regexp            "regexp           "                   )
    ("C-s"     helm-semantic-or-imenu "semantic-or-imenu"                   )
    ("C-h f"   helm-apropos           "apropos          "                   )
    ("C-h v"   helm-apropos           "apropos          "                   )
    ("C-h h"   helm-man-woman         "man-woman        " :column "external")
    (""        helm-locate            "locate           "                   )
    (""        helm-find              "find             "                   ))
  (use-package helm
    :defer t
    :config
    (bind-keys (helm-command-prefix-key . nil)
               :map minibuffer-local-map
               ("C-c C-l" . helm-minibuffer-history)
               :map helm-map
               ("C-z" . nil)
               ("C-j" . helm-execute-persistent-action)
               ("<tab>" . helm-execute-persistent-action)
               ("C-i" . helm-select-action)
               ("M-x" . helm-select-action)
               ("C-c C-f" . helm-follow-mode)
               ("C-o" . helm-next-source))
    (use-package helm-files
      :defer t
      :config
      (bind-keys :map helm-find-files-map
                 ("C-l" . helm-find-files-up-one-level)
                 ("C-r" . helm-find-files-down-last-level)
                 ("C-s" . helm-ff-run-grep)))
    (use-package helm-buffers
      :defer t
      :config
      (bind-keys :map helm-buffer-map
                 ("M-a" . helm-mark-all)))))

(defun my/init-keys/undo-tree ()
  (use-package undo-tree
    :defer t
    :config
    (bind-keys :map undo-tree-map
               ("C-/" . nil) ;; (undo-tree-undo)
               ("C-_" . nil) ;; (undo-tree-undo)
               ("C-?" . nil) ;; (undo-tree-redo)
               ("M-_" . nil) ;; (undo-tree-redo)
               :map undo-tree-visualizer-mode-map
               ("<return>" . undo-tree-visualizer-quit)
               ("C-p" . undo-tree-visualize-undo-to-x)
               ("C-n" . undo-tree-visualize-redo-to-x))))

(defun my/init-keys/flyspell ()
  (use-package flyspell
    :defer t
    :config
    (bind-keys :map flyspell-mode-map
               ("C-," . nil) ;; (flyspell-goto-next-error)
               ("C-;" . nil) ;; (flyspell-auto-correct-previous-word)
               ("C-." . nil) ;; (flyspell-auto-correct-word)
               ("C-M-i" . nil) ;; (flyspell-auto-correct-word)
               ("C-c $" . nil) ;; (flyspell-correct-word-before-point)
               ("C-\"" . flyspell-goto-next-error)
               ("C-'" . flyspell-auto-correct-word))
    (use-package flyspell-correct
      :defer t
      :config
      (bind-keys :map flyspell-mode-map
                 ("C-'" . flyspell-correct-wrapper)))))

(defun my/init-keys/hydra/group ()
  (defhydra pkg/hydra/group
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("h" pkg/hydra/group/helm/body
     "helm" :column "")
    ("b" pkg/hydra/group/window/body
     "window, windmove, winner, buffer-move, zoom")
    ("c" pkg/hydra/group/cursor/body
     "cursor, paredit")
    ("d" (lambda () (interactive)
           (cond
            ((my/package-enabled-p 'treemacs) (pkg/hydra/group/treemacs/body))
            ((my/package-enabled-p 'neotree) (pkg/hydra/group/neotree/body))
            (t (user-error "*pkg/hydra/group* no package enabled for \"t\""))))
     "dired, treemacs, neotree")
    ("i" pkg/hydra/group/highlight/body
     "highlight, highlight-thing")
    ("m" pkg/hydra/group/bookmark/body
     "bookmark, bm, helm-bm")
    ("=" pkg/hydra/group/diff/body
     "ediff, vdiff")
    ("o" pkg/hydra/group/org/body
     "org")
    ("p" pkg/hydra/group/projectile/body
     "projectile, helm-projectile")
    ("!" (lambda () (interactive)
           (cond
            ((my/package-enabled-p 'flymake) (pkg/hydra/group/flymake/body))
            ((my/package-enabled-p 'flycheck) (pkg/hydra/group/flycheck/body))
            (t (user-error "*pkg/hydra/group* no package enabled for \"!\""))))
     "flymake, flycheck")
    ("." (lambda () (interactive)
           (cond
            ((my/package-enabled-p 'ggtags) (pkg/hydra/group/ggtags/body))
            ((my/package-enabled-p 'helm-gtags) (pkg/hydra/group/helm-gtags/body))
            (t (user-error "*pkg/hydra/group* no package enabled for \"g\""))))
     "ggtags, helm-gtags")
    ("q" pkg/hydra/quit nil :exit t)))

(defun my/init-keys/hydra/group/window ()
  (defhydra pkg/hydra/group/window
    (:timeout pkg/hydra/timeout-sec)
    ("+" enlarge-window              "++ <>      " :column "buffer size ")
    ("=" enlarge-window-horizontally "++ ^v      "                       )
    ("_" shrink-window               "-- <>      "                       )
    ("-" shrink-window-horizontally  "-- ^v      "                       )
    ("p" scroll-other-window-down    "scroll up  " :column "scroll other")
    ("n" scroll-other-window         "scroll down"                       )
    ("u" winner-undo                 "undo       " :column "winner      ")
    ("r" winner-redo                 "redo       "                       )
    ("h" buf-move-left               "left       " :column "move buffer ")
    ("l" buf-move-right              "right      "                       )
    ("k" buf-move-up                 "up         "                       )
    ("j" buf-move-down               "down       "                       )
    ("q" pkg/hydra/quit nil :exit t)))

(defun my/init-keys/hydra/group/cursor ()
  (defhydra pkg/hydra/group/cursor
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("C-    f,b" nil "char     " :column "forward/backward")
    ("   M- f,b" nil "word     "                           )
    ("C-    e,a" nil "line     " :column "beginning/end   ")
    ("   M- e,a" nil "sentence "                           )
    ("C- M- e,a" nil "paragraph"                           )
    ("C- M- f,b" nil "sexp     " :column "s-expression    ")
    ("C- M- j,k" nil "tree down"                           )
    ("C- M- l,h" nil "tree up  "                           )))

(defun my/init-keys/paredit ()
  (use-package paredit
    :defer t
    :config
    (bind-keys :map paredit-mode-map
               ("C-M-n" . nil) ;; (paredit-forward-up)
               ("C-M-p" . nil) ;; (paredit-backward-down)
               ("C-M-d" . nil) ;; (paredit-forward-down)
               ("C-M-u" . nil) ;; (paredit-backward-up)
               ("C-M-f" . paredit-forward)
               ("C-M-b" . paredit-backward)
               ("C-M-l" . paredit-forward-up)
               ("C-M-k" . paredit-backward-down)
               ("C-M-j" . paredit-forward-down)
               ("C-M-h" . paredit-backward-up))))

(defun my/init-keys/lispy ()
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
            (lispy-define-key map ":" 'lispy-ace-char)
            (lispy-define-key map ";" 'lispy-ace-paren)
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
    ))

(defun my/init-keys/dired ()
  (use-package dired
    :defer t
    :config
    (bind-keys :map dired-mode-map
               ("A" . nil) ("C" . nil) ("R" . nil)
               ("* c" . nil) ("* s" . nil)
               ("?" . pkg/hydra/group/dired-help/body))
    (defhydra pkg/hydra/group/dired-help
      (dired-mode-map "" :timeout pkg/hydra/timeout-sec :exit t)
      "
Number of marked: %(pkg/dired/count-marked)

"
      ("s"   dired-sort-toggle-or-edit    "sort             " :column "show    ")
      ("g"   revert-buffer                "revert           "                   )
      ("G"   dired-do-redisplay           "refresh          "                   )
      ("k"   dired-do-kill-lines          "hide line        "                   )
      ("a"   dired-hide-details-mode      "hide details     "                   )
      ("n"   dired-next-line              "next line        " :column "move    ")
      ("p"   dired-previous-line          "prev line"                           )
      ("C-n" dired-next-dirline           "next dir         "                   )
      ("C-p" dired-prev-dirline           "prev dir         "                   )
      ("j"   dired-goto-file              "jump             "                   )
      ("l"   dired-up-directory           "parent           "                   )
      ("m"   dired-mark                   "mark             " :column "mark    ")
      ("M"   dired-mark-files-regexp      "mark regexp      "                   )
      ("u"   dired-unmark                 "unmark           "                   )
      ("U"   dired-unmark-all-marks       "unmark all       "                   )
      ("t"   dired-toggle-marks           "toggle (un)marked"                   )
      ("M-n" dired-next-marked-file       "next marked      "                   )
      ("M-p" dired-prev-marked-file       "prev marked      "                   )
      ("* a,/,*,@" nil                    "mark all/dir/exe/link"               )
      ("* a" dired-mark-subdir-files      nil                                   )
      ("* /" dired-mark-directories       nil                                   )
      ("* *" dired-mark-executables       nil                                   )
      ("* @" dired-mark-symlinks          nil                                   )
      ("v"   dired-view-file              "peek             " :column "view    ")
      ("V"   dired-display-file           "display          "                   )
      ("f"   dired-find-file              "open             "                   )
      ("F"   dired-find-file-other-window "open in other    "                   )
      ("y"   dired-do-copy                "copy             " :column "action  ")
      ("Y"   dired-do-rename              "move             "                   )
      ("="   dired-create-directory       "create dir       "                   )
      ("d"   dired-flag-file-deletion     "flag delete      " :column "delete  ")
      ("#"   dired-flag-auto-save-files   "flag auto-saved  "                   )
      ("~"   dired-flag-backup-files      "flag backup      "                   )
      ("x"   dired-do-flagged-delete      "delete flagged   "                   )
      ("X"   dired-clean-directory        "cleanup          "                   )
      ("D"   dired-do-delete              "delete marked    "                   )
      (": m" dired-do-chmod               "chmod            " :column "property")
      (": o" dired-do-chown               "chown            "                   )
      (": g" dired-do-chgrp               "chgrp            "                   )
      (": t" dired-do-touch               "touch            "                   )
      ("!"   dired-do-shell-command       "shell            " :column "external")
      ("&"   dired-do-async-shell-command "shell &          "                   ))))

(defun my/init-keys/treemacs ()
  (defhydra pkg/hydra/group/treemacs
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("d"   dired                         "enable       " :column "dired  ")
    ("j"   pkg/treemacs/select-window    "select       " :column "window ")
    ("1"   treemacs-delete-other-windows "delete others"                  )
    ("C-f" treemacs-follow-mode          "mode         " :column "follow ")
    ("f"   treemacs-find-file            "file         "                  )
    ("t"   treemacs-find-tag             "tag          "                  )
    ("m"   treemacs-bookmark             "bookmark     "                  )
    ("p"   treemacs-projectile           "import       " :column "project")
    ("q" pkg/hydra/quit nil :exit t))
  (use-package treemacs
    :defer t
    :config
    (setq treemacs-mode-map (make-sparse-keymap))
    (bind-keys :map treemacs-mode-map
               ([mouse-1] . treemacs-single-click-expand-action)
               ("?"       . pkg/hydra/group/treemacs-help/body))
    (defhydra pkg/hydra/group/treemacs-help
      (treemacs-mode-map "" :timeout pkg/hydra/timeout-sec :exit t)
      ("s"         treemacs-resort                     "sort           " :column "show    ")
      ("g"         treemacs-refresh                    "refresh        "                   )
      ("a"         treemacs-toggle-show-dotfiles       "dot files      "                   )
      ("<C-tab>"   treemacs-collapse-other-projects    "collapse others"                   )
      ("<backtab>" treemacs-collapse-all-projects      "collapse all   "                   )
      ("n"         treemacs-next-line                  "next line      " :column "move    ")
      ("p"         treemacs-previous-line              "prev line      "                   )
      ("C-n"       treemacs-next-neighbour             "next dir       "                   )
      ("C-p"       treemacs-previous-neighbour         "prev dir       "                   )
      ("l"         treemacs-goto-parent-node           "parent         "                   )
      ("L"         treemacs-collapse-parent-node       "collapse parent"                   )
      ("v"         treemacs-TAB-action                 "peek           " :column "view    ")
      ("<tab>"     treemacs-TAB-action                 nil                                 )
      ("V"         treemacs-peek                       "display        "                   )
      ("f"         treemacs-RET-action                 "open           "                   )
      ("C-M-n"     treemacs-next-line-other-window     "peek next line "                   )
      ("C-M-p"     treemacs-previous-line-other-window "peek prev line "                   )
      ("y"         treemacs-copy-file                  "copy           " :column "action  ")
      ("Y"         treemacs-rename                     "move           "                   )
      ("D"         treemacs-delete                     "delete         "                   )
      ("+"         treemacs-create-file                "create file    "                   )
      ("="         treemacs-create-dir                 "create dir     "                   )
      ("m"         treemacs-add-bookmark               "bookmark       "                   )
      ("M-n"       treemacs-next-project               "move next      " :column "project ")
      ("M-p"       treemacs-previous-project           "move prev      "                   )
      ("<C-return>"treemacs-root-up                    "root up        "                   )
      ("<return>"  treemacs-root-down                  "root down      "                   )
      (": f"       treemacs-follow-mode                "follow mode    " :column "treemacs")
      (": g"       treemacs-git-mode                   "git mode       "                   )
      (": w"       treemacs-set-width                  "set width      "                   )
      (": m"       treemacs-toggle-fixed-width         "fixed width    "                   )
      ("q"         treemacs-quit                       "quit           "                   )
      ("Q"         treemacs-kill-buffer                "terminate      "                   ))))

(defun my/init-keys/neotree ()
  (defhydra pkg/hydra/group/neotree
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("d" dired              "enable" :column "dired ")
    ("j" pkg/neotree/toggle "select" :column "window")
    ("q" pkg/hydra/quit nil :exit t))
  (use-package neotree
    :defer t
    :config
    (bind-keys :map neotree-mode-map
               ("?" . pkg/hydra/group/neotree-help/body))
    (defhydra pkg/hydra/group/neotree-help
      (neotree-mode-map "" :timeout pkg/hydra/timeout-sec :exit t)
      ("n"   neotree-next-line                    "next line")
      ("p"   neotree-previous-line                "prev line")
      ("C-n" neotree-select-next-sibling-node     "next dir ")
      ("C-p" neotree-select-previous-sibling-node "prev dir ")
      ("l"   neotree-select-up-node               "parent   ")
      ("a"   neotree-hidden-file-toggle           "hidden   "))))

(defun my/init-keys/highlight ()
  (defhydra pkg/hydra/group/highlight
    (:timeout pkg/hydra/timeout-sec)
    ("i" highlight-symbol-at-point       "at point   " :column "highlight  ")
    ("s" highlight-phrase                "word       "                      )
    ("r" highlight-regexp                "regexp     "                      )
    ("l" highlight-lines-matching-regexp "regexp line"                      )
    ("u" unhighlight-regexp              "regexp     " :column "unhighlight")
    ("q" pkg/hydra/quit nil :exit t)))

(defun my/init-keys/bookmark ()
  (defhydra pkg/hydra/group/bookmark
    (:timeout pkg/hydra/timeout-sec)
    ("m" bookmark-set        "set     " :column "bookmark")
    ("d" bookmark-delete     "unset   "                   )
    ("r" bookmark-rename     "rename  "                   )
    ("l" bookmark-bmenu-list "list    "                   )
    ("p" bm-previous         "previous" :column "browse  ")
    ("n" bm-next             "next    "                   )
    ("t" bm-toggle           "toggle  "                   )
    ("q" pkg/hydra/quit nil :exit t)))

(defun my/init-keys/hydra/group/diff ()
  (defhydra pkg/hydra/group/diff
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("e" pkg/hydra/group/ediff/body "choose" :column "ediff")
    ("v" pkg/hydra/group/vdiff/body "choose" :column "vdiff")
    ("q" pkg/hydra/quit nil :exit t)))

(defun my/init-keys/ediff ()
  (defhydra pkg/hydra/group/ediff
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("f" ediff-files        "2 files  " :column "file  ")
    ("F" ediff-files3       "3 files  "                 )
    ("b" ediff-buffers      "2 buffers" :column "buffer")
    ("B" ediff-buffers3     "3 buffers"                 )
    ("d" ediff-current-file "current  "                 )
    ("q" pkg/hydra/quit nil :exit t)))

(defun my/init-keys/vdiff ()
  (defhydra pkg/hydra/group/vdiff
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("f" vdiff-files        "2 files  " :column "file  ")
    ("F" vdiff-files3       "3 files  "                 )
    ("b" vdiff-buffers      "2 buffers" :column "buffer")
    ("B" vdiff-buffers3     "3 buffers"                 )
    ("d" vdiff-current-file "current  "                 )
    ("q" pkg/hydra/quit nil :exit t))
  (use-package vdiff
    :defer t
    :config
    (bind-keys :map vdiff-mode-map
               ("C-c d" . vdiff-mode-prefix-map)
               ("C-c d h" . vdiff-hydra/body))))

(defun my/init-keys/org ()
  (defhydra pkg/hydra/group/org
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("c" org-capture "capture" :column "org mode")
    ("a" org-agenda  "agenda "                   )
    ("q" pkg/hydra/quit nil :exit t)))

(defun my/init-keys/projectile ()
  ;; (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (defhydra pkg/hydra/group/projectile
    (:timeout pkg/hydra/timeout-sec :exit t)
    "
PROJECT: %(projectile-project-root)

"
    ("h"   helm-projectile                            "helm     " :column "project  ")
    ("p"   projectile-switch-project                  "open     "                    )
    ("P"   projectile-switch-open-project             "switch   "                    )
    ("v"   projectile-vc                              "version  "                    )
    ("x"   projectile-remove-known-project            "remove   "                    )
    ("X"   projectile-cleanup-known-projects          "cleanup  "                    )
    ("b"   projectile-switch-to-buffer                "switch   " :column "buffer   ")
    ("k"   projectile-kill-buffers                    "kill     "                    )
    ("f"   projectile-find-file                       "this proj" :column "find file")
    ("F"   projectile-find-file-in-known-projects     "all proj "                    )
    ("j"   projectile-find-file-in-directory          "the dir  "                    )
    ("r"   projectile-recentf                         "recent   "                    )
    ("t"   projectile-find-other-file                 "same name"                    )
    ("d"   projectile-find-dir                        "find     " :column "directory")
    ("D"   projectile-dired                           "dired    "                    )
    ("o"   projectile-grep                            "grep     " :column "symbol   ")
    ("O"   projectile-multi-occur                     "occur    "                    )
    ("w"   projectile-replace                         "replace  "                    )
    ("!"   projectile-run-shell-command-in-root       "shell    " :column "external ")
    ("&"   projectile-run-async-shell-command-in-root "shell &  "                    )
    ("a"   projectile-ag                              "ag       "                    )
    ("c"   projectile-ack                             "ack      "                    )
    ;; TODO
    ;; V projectile-browse-dirty-projects
    ;; c projectile-compile-project
    ;; I projectile-ibuffer
    ;; S projectile-save-project-buffers
    ;; j projectile-find-tag
    ;; R projectile-regenerate-tags
    ;; i projectile-invalidate-cache
    ;; z projectile-cache-current-file
    ("q" pkg/hydra/quit nil :exit t)))

(defun my/init-keys/helm-projectile ()
  (use-package helm-projectile
    :defer t
    :config
    (mapc
     (lambda (cmd)
       (let* ((cmd (symbol-name cmd))
              (group (symbol-name 'pkg/hydra/group/projectile))
              (keymap (concat group "/keymap"))
              (hydra (concat group "/" cmd "-and-exit"))
              (helm (concat "helm-" cmd)))
         (define-key (symbol-value (intern keymap))
           `[remap ,(intern hydra)] (intern helm))))
     '(projectile-switch-project
       projectile-switch-to-buffer
       projectile-find-file
       projectile-recentf
       projectile-find-dir
       projectile-grep
       projectile-ag
       projectile-ack
       projectile-browse-dirty-projects))))

(defun my/init-keys/flymake ()
  (use-package flymake
    :defer t
    :config
    (defhydra pkg/hydra/group/flymake
      (:timeout pkg/hydra/timeout-sec :exit t)
      ("q" pkg/hydra/quit nil :exit t))))

(defun my/init-keys/flycheck ()
  (use-package flycheck
    :defer t
    :config
    (unbind-key flycheck-keymap-prefix flycheck-mode-map)
    (defhydra pkg/hydra/group/flycheck
      (:timeout pkg/hydra/timeout-sec :exit t)
      ("n" flycheck-next-error             "next     " :column "error  " :exit nil)
      ("p" flycheck-previous-error         "prev     "                   :exit nil)
      ("l" helm-flycheck                   "helm list"                            )
      ("L" flycheck-list-errors            "list     "                            )
      ;; 'flycheck-error-list-mode-map in (flycheck-list-errors)
      ;; "<return>" :: go to the current error in the source buffer
      ;; "e" :: explain the error
      ;; "f" :: filter the error list by level
      ;; "F" :: remove the filter
      ;; "S" :: sort the error list by the column at point
      ;; "g" :: check the source buffer and update the error list
      ;; "q" :: quit the error list and hide its window
      ("h" flycheck-display-error-at-point "display  " :column "detail "          )
      ("H" flycheck-explain-error-at-point "explain  "                            )
      ("y" flycheck-copy-errors-as-kill    "copy     "                            )
      ("?" flycheck-describe-checker       "describe " :column "checker"          )
      ("e" pkg/flycheck/enable-checker     "enable   "                            )
      ("d" flycheck-disable-checker        "disable  "                            )
      ("j" flycheck-select-checker         "select   "                            )
      ("v" flycheck-verify-setup           "info     " :column "buffer "          )
      ("g" flycheck-buffer                 "refresh  "                            )
      ("G" flycheck-compile                "compile  "                            )
      ("k" flycheck-clear                  "clear    "                            )
      ("q" pkg/hydra/quit nil :exit t))))

(defun my/init-keys/ggtags ()
  (use-package ggtags
    :defer t
    :config
    (unbind-key ggtags-mode-prefix-key ggtags-mode-map)
    (bind-keys :map ggtags-mode-map
               ("M-." . ggtags-find-tag-dwim)
               ("M-n" . ggtags-next-mark)
               ("M-p" . ggtags-prev-mark)
               ("M-/" . ggtags-view-tag-history))
    (setq ggtags-navigation-mode-map (make-sparse-keymap))
    (bind-keys :map ggtags-navigation-mode-map
               ("M-n" . next-error)
               ("M-p" . previous-error)
               ("C-M-n" . ggtags-navigation-next-file)
               ("C-M-p" . ggtags-navigation-previous-file)
               ("M-<" . first-error)
               ("M->" . ggtags-navigation-last-error)
               ("M-s" . ggtags-navigation-isearch-forward)
               ;; 搜索结果中的文件路径名可能会变成缩写，可利用此命令缩放
               ("M-a" . ggtags-navigation-visible-mode)
               ("<return>" . ggtags-navigation-mode-done)
               ("M-," . ggtags-navigation-mode-abort)
               ("M-u" . ggtags-navigation-start-file))
    (defhydra pkg/hydra/group/ggtags
      (:timeout pkg/hydra/timeout-sec :exit t)
      ("."   ggtags-find-tag-dwim            "M-.        " :column "jump    ")
      (","   xref-pop-marker-stack           "M-,        "                   )
      ("n"   ggtags-next-mark                "M-n        "                   )
      ("p"   ggtags-prev-mark                "M-p        "                   )
      ("/"   ggtags-view-tag-history         "M-/        "                   )
      ("o d" ggtags-find-definition          "definition " :column "search  ")
      ("o r" ggtags-find-reference           "reference  "                   )
      ("o o" ggtags-grep                     "grep       "                   )
      ("o O" ggtags-find-tag-regexp          "pattern    "                   )
      ("o s" ggtags-find-other-symbol        "symbol     "                   )
      ("o f" ggtags-find-file                "file       "                   )
      ("s"   ggtags-save-to-register         "register   " :column "history ")
      ("r"   jump-to-register                "restore    "                   )
      ("l"   ggtags-view-search-history      "show       "                   )
      ("g"   ggtags-update-tags              "refresh    " :column "database")
      ("G"   ggtags-create-tags              "setup      "                   )
      ("X"   ggtags-delete-tags              "cleanup    "                   )
      ("d"   ggtags-visit-project-root       "dired @root" :column "advance ")
      ("1"   ggtags-kill-file-buffers        "kill others"                   )
      ("v h" ggtags-browse-file-as-hypertext "view html  "                   )
      ("w"   ggtags-query-replace            "replace    "                   )
      ("C-q" ggtags-toggle-project-read-only "readonly   "                   )
      ("q" pkg/hydra/quit nil :exit t))))

;; todo :: 在以下交互函数执行时，输入"C-u"，还可限定搜索的目录路径
(defun my/init-keys/helm-gtags ()
  (use-package helm-gtags
    :defer t
    :config
    (unbind-key helm-gtags-prefix-key helm-gtags-mode-map)
    (bind-keys :map helm-gtags-mode-map
               ("M-." . helm-gtags-dwim)
               ("M-," . helm-gtags-pop-stack)
               ("M-n" . helm-gtags-next-history)
               ("M-p" . helm-gtags-previous-history)
               ("M-/" . helm-gtags-show-stack))
    (defhydra pkg/hydra/group/helm-gtags
      (:timeout pkg/hydra/timeout-sec :exit t)
      ("."   helm-gtags-dwim                  "M-.        " :column "jump    ")
      (","   helm-gtags-pop-stack             "M-,        "                   )
      ("n"   helm-gtags-next-history          "M-n        "                   )
      ("p"   helm-gtags-previous-history      "M-p        "                   )
      ("/"   helm-gtags-show-stack            "M-/        "                   )
      ("o d" helm-gtags-find-tag              "definition " :column "search  ")
      ("o r" helm-gtags-find-rtag             "reference  "                   )
      ("o o" helm-gtags-find-pattern          "pattern    "                   )
      ("o s" helm-gtags-find-symbol           "symbol     "                   )
      ("o f" helm-gtags-find-files            "file       "                   )
      ("i"   helm-gtags-tags-in-this-function "this func  " :column "select  ")
      ("f"   helm-gtags-parse-file            "this file  "                   )
      ("l"   helm-gtags-select                "this proj  "                   )
      ("d"   helm-gtags-select-path           "path       "                   )
      ("g"   helm-gtags-update-tags           "refresh    " :column "database")
      ("G"   helm-gtags-create-tags           "setup      "                   )
      ("x"   helm-gtags-clear-all-stacks      "clear stack"                   )
      ("X"   helm-gtags-clear-all-cache       "clear cache"                   )
      ("q" pkg/hydra/quit nil :exit t))))

(defun my/init-keys/company ()
  (use-package company
    :defer t
    :config
    (bind-keys :map company-active-map
               ("C-w" . nil) ;; (company-show-location)
               ("<f1>" . nil) ;; (company-show-doc-buffer)
               ("C-M-s" . nil) ;; (company-filter-candidates)
               :map company-search-map
               ("C-o" . nil) ;; (company-search-toggle-filtering)
               )
    (defun pkg/company/move-next ()
      (interactive)
      (company-complete-common-or-cycle 1))
    (defun pkg/company/move-prev ()
      (interactive)
      (company-complete-common-or-cycle -1))
    (defhydra pkg/hydra/group/company-active
      (company-active-map "" :timeout pkg/hydra/timeout-sec :exit t)
      ("C-n"      company-select-next        "next       " :column "move    ")
      ("C-p"      company-select-previous    "prev       "                   )
      ("M-n"      pkg/company/move-next      "next/common"                   )
      ("M-p"      pkg/company/move-prev      "prev/common"                   )
      ("<tab>"    company-complete-common    "common     " :column "complete")
      ("<return>" company-complete-selection "select     "                   )
      ("C-h"      company-show-doc-buffer    "docstring  " :column "help    ")
      ("C-S-h"    company-show-location      "code       "                   )
      ("C-?"      company-diag               "company    "                   )
      ("C-s"      company-search-candidates  "search     " :column "search  ")
      ("C-f"      company-filter-candidates  "filter     "                   )
      ("C-g"      company-abort              nil                             ))
    (defhydra pkg/hydra/group/company-search
      (company-search-map "" :timeout pkg/hydra/timeout-sec :exit t)
      ("C-n" company-select-next             "next       " :column "move  ")
      ("C-p" company-select-previous         "prev       "                 )
      ("M-n" pkg/company/move-next           "next/common"                 )
      ("M-p" pkg/company/move-prev           "prev/common"                 )
      ("C-s" company-search-repeat-forward   "forward    " :column "search")
      ("C-r" company-search-repeat-backward  "backward   "                 )
      ("C-t" company-search-toggle-filtering "toggle     "                 )
      ("C-g" company-search-abort            nil                           ))))


;; todo ","
(defun my/init-keys/cedet ()
  (use-package cedet
    :defer t
    :config
    (defhydra pkg/hydra/group/cedet
      (:timeout pkg/hydra/timeout-sec)
      ("g" semantic-symref-symbol                "find symbol    " :column "reference")
      ("G" semantic-symref                       "find function  "                    )
      ("i" semantic-decoration-include-visit     "include file   "                    )
      ("t" semantic-analyze-proto-impl-toggle    "prototype      "                    )
      ("l" semantic-analyze-possible-completions                                      )
      ("-" pkg/cedet/fold-block)
      ("=" pkg/cedet/unfold-block)
      ("_" semantic-tag-folding-fold-all)
      ("+" semantic-tag-folding-show-all)
      (""  semantic-ia-complete-tip)
      (""  semantic-ia-complete-symbol)
      (""  semantic-ia-complete-symbol-menu)
      ("," semantic-ia-fast-jump)
      ("." semantic-ia-show-summary)
      ("/" semantic-ia-show-doc)
      ("b" semantic-mrub-switch-tags))))


(provide 'my/init-keys)
