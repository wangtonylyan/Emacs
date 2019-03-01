;; -*- coding: utf-8 -*-

(bind-keys ("C-x d" . nil) ;; (dired)
           )

(defun pkg/hydra/group/dired/body ()
  (interactive)
  (cond
   ((pkg/package/enabled-p 'treemacs) (pkg/hydra/group/treemacs/body))
   ((pkg/package/enabled-p 'neotree) (pkg/hydra/group/neotree/body))
   (t (user-error "*pkg/hydra/group/dired/body* no package is enabled"))))

(defhydra pkg/hydra/group/treemacs
  (:timeout pkg/hydra/timeout-sec :exit t)
  ("d"   dired                         "enable       " :column "dired  ")
  ("j"   pkg/treemacs/select-window    "select       " :column "window ")
  ("1"   treemacs-delete-other-windows "delete others"                  )
  ("C-f" treemacs-follow-mode          "mode         " :column "follow ")
  ("f"   treemacs-find-file            "file         "                  )
  ("t"   treemacs-find-tag             "tag          "                  )
  ("m"   treemacs-bookmark             "bookmark     "                  )
  ("p"   treemacs-projectile           "import       " :column "project"))

(defhydra pkg/hydra/group/neotree
  (:timeout pkg/hydra/timeout-sec :exit t)
  ("d" dired              "enable" :column "dired ")
  ("j" pkg/neotree/toggle "select" :column "window"))

(use-package dired
  :defer t
  :config
  (bind-keys :map dired-mode-map
             ("A" . nil)
             ("C" . nil)
             ("R" . nil)
             ("* c" . nil)
             ("* s" . nil)
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
    ("&"   dired-do-async-shell-command "shell &          "                   )))

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
    ("Q"         treemacs-kill-buffer                "terminate      "                   )))

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
    ("a"   neotree-hidden-file-toggle           "hidden   ")))


(provide 'my/keys/directory)
