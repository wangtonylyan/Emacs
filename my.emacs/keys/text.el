;; -*- coding: utf-8 -*-

(defhydra pkg/hydra/group/org
  (:timeout pkg/hydra/timeout-sec :exit t)
  ("a" org-agenda     "agenda     " :column "")
  ("c" org-capture    "capture    ")
  ("l" org-store-link "store link ")
  ("b" org-switchb    "switch file")
  ("t" pkg/hydra/group/org/timestamp/body "time"))

;; =======================================================================================
(defun wocaiio ()
  (defhydra todo ()

    ;; "<return>"用来insert各种line
    ;; "M-"用来move headline或subtree
    ;; "S-"用来修改tag的

    ("<return>" org-return "" :column "insert new")
    ("<M-return>" org-meta-return "a new line")
    ("<C-return>" org-insert-heading-respect-content "headline")
    ("<S-return>" org-table-copy-down "headline")
    ("<M-S-return>" org-insert-todo-heading "")
    ("<C-S-return>" org-insert-todo-heading-respect-content "")

    ("<M-up>" org-metaup "")
    ("<M-down>" org-metadown "")
    ("<M-S-up>" org-shiftmetaup "")
    ("<M-S-down>" org-shiftmetadown "")

    ("<M-left>" org-metaleft "")
    ("<M-right>" org-metaright "")
    ("<M-S-left>" org-shiftmetaleft "")
    ("<M-S-right>" org-shiftmetaright "")

    ;; 没有"C-M-<up>

    ;; ===========================================================================
    ("C-c C-p" org-previous-visible-heading)
    ("C-c C-n" org-next-visible-heading)


    ("n" outline-next-visible-heading)
    ("p" outline-previous-visible-heading)
    ("f" org-forward-same-level)
    ("b" org-backward-same-level)
    ("l" outline-up-heading)
    ("j" org-goto)

    ("M-b" org-do-promote)
    ("M-f" org-do-demote)
    ("M-S-b" org-promote-subtree)
    ("M-S-f" org-demote-subtree)
    ("C-M-S-b" org-move-subtree-up)
    ("C-M-S-f" org-move-subtree-down))

  (org-defkey org-mode-map (kbd "C-i") #'org-cycle)
  (org-defkey org-mode-map (kbd "<tab>") #'org-cycle)
  (org-defkey org-mode-map (kbd "C-<tab>") #'org-force-cycle-archived)


  (org-defkey org-mode-map (kbd "<S-iso-leftab>") #'org-shifttab)
  (org-defkey org-mode-map (kbd "S-<tab>") #'org-shifttab)
  (define-key org-mode-map (kbd "<backtab>") #'org-shifttab)

;;;; RET key with modifiers

;;;; Cursor keys with modifiers


  (org-defkey org-mode-map (kbd "C-M-S-<right>") #'org-increase-number-at-point)
  (org-defkey org-mode-map (kbd "C-M-S-<left>") #'org-decrease-number-at-point)


  (org-defkey org-mode-map (kbd "S-<up>") #'org-shiftup)
  (org-defkey org-mode-map (kbd "S-<down>") #'org-shiftdown)
  (org-defkey org-mode-map (kbd "S-<left>") #'org-shiftleft)
  (org-defkey org-mode-map (kbd "S-<right>") #'org-shiftright)

  (org-defkey org-mode-map (kbd "C-S-<right>") #'org-shiftcontrolright)
  (org-defkey org-mode-map (kbd "C-S-<left>") #'org-shiftcontrolleft)
  (org-defkey org-mode-map (kbd "C-S-<up>") #'org-shiftcontrolup)
  (org-defkey org-mode-map (kbd "C-S-<down>") #'org-shiftcontroldown)

;;;; Babel keys
  (define-key org-mode-map org-babel-key-prefix org-babel-map)
  (pcase-dolist (`(,key . ,def) org-babel-key-bindings)
    (define-key org-babel-map key def))

;;;; Extra keys for TTY access.

  ;;  We only set them when really needed because otherwise the
  ;;  menus don't show the simple keys

  (org-defkey org-mode-map (kbd "C-c C-x") (make-sparse-keymap))

;;;; TAB key with modifiers
  (org-defkey org-mode-map (kbd "C-i") #'org-cycle)
  (org-defkey org-mode-map (kbd "<tab>") #'org-cycle)
  (org-defkey org-mode-map (kbd "C-<tab>") #'org-force-cycle-archived)
  (org-defkey org-mode-map (kbd "M-<tab>") #'pcomplete)
  (org-defkey org-mode-map (kbd "M-TAB") #'pcomplete)
  (org-defkey org-mode-map (kbd "ESC <tab>") #'pcomplete)
  (org-defkey org-mode-map (kbd "ESC TAB") #'pcomplete)

  (org-defkey org-mode-map (kbd "<S-iso-leftab>") #'org-shifttab)
  (org-defkey org-mode-map (kbd "S-<tab>") #'org-shifttab)
  (define-key org-mode-map (kbd "<backtab>") #'org-shifttab)

;;;; Cursor keys with modifiers
  (org-defkey org-mode-map (kbd "M-<left>") #'org-metaleft)
  (org-defkey org-mode-map (kbd "M-<right>") #'org-metaright)
  (org-defkey org-mode-map (kbd "ESC <right>") #'org-metaright)
  (org-defkey org-mode-map (kbd "M-<up>") #'org-metaup)
  (org-defkey org-mode-map (kbd "ESC <up>") #'org-metaup)
  (org-defkey org-mode-map (kbd "M-<down>") #'org-metadown)
  (org-defkey org-mode-map (kbd "ESC <down>") #'org-metadown)

  (org-defkey org-mode-map (kbd "C-M-S-<right>") #'org-increase-number-at-point)
  (org-defkey org-mode-map (kbd "C-M-S-<left>") #'org-decrease-number-at-point)
  (org-defkey org-mode-map (kbd "M-S-<left>") #'org-shiftmetaleft)
  (org-defkey org-mode-map (kbd "ESC S-<left>") #'org-shiftmetaleft)
  (org-defkey org-mode-map (kbd "M-S-<right>") #'org-shiftmetaright)
  (org-defkey org-mode-map (kbd "ESC S-<right>") #'org-shiftmetaright)
  (org-defkey org-mode-map (kbd "M-S-<up>") #'org-shiftmetaup)
  (org-defkey org-mode-map (kbd "ESC S-<up>") #'org-shiftmetaup)
  (org-defkey org-mode-map (kbd "M-S-<down>") #'org-shiftmetadown)
  (org-defkey org-mode-map (kbd "ESC S-<down>") #'org-shiftmetadown)

  (org-defkey org-mode-map (kbd "S-<up>") #'org-shiftup)
  (org-defkey org-mode-map (kbd "S-<down>") #'org-shiftdown)
  (org-defkey org-mode-map (kbd "S-<left>") #'org-shiftleft)
  (org-defkey org-mode-map (kbd "S-<right>") #'org-shiftright)

  (org-defkey org-mode-map (kbd "C-S-<right>") #'org-shiftcontrolright)
  (org-defkey org-mode-map (kbd "C-S-<left>") #'org-shiftcontrolleft)
  (org-defkey org-mode-map (kbd "C-S-<up>") #'org-shiftcontrolup)
  (org-defkey org-mode-map (kbd "C-S-<down>") #'org-shiftcontroldown))


(use-package image-mode
  :defer t
  :config
  (bind-keys :map image-mode-map
             ("SPC" . nil) ("S-SPC" . nil) ("DEL" . nil)
             ("k" . nil) ("a" . nil) ("F" . nil)
             ("C-c C-c" . nil) ("C-c C-x" . nil)
             ("?" . pkg/hydra/group/image-mode-help/body))
  (defhydra pkg/hydra/group/image-mode-help
    (image-mode-map "" :timeout pkg/hydra/timeout-sec :exit t)
    ("n"   image-scroll-up          "up        " :column "scroll")
    ("p"   image-scroll-down        "down      "                 )
    ("f"   image-forward-hscroll    "left      "                 )
    ("b"   image-backward-hscroll   "right     "                 )
    ("C-n" image-next-frame         "next frame" :column "browse")
    ("C-p" image-previous-frame     "prev frame"                 )
    ("M-n" image-next-file          "next file "                 )
    ("M-p" image-previous-file      "prev file "                 )
    ("j p" image-goto-frame         "page      " :column "jump  ")
    ("v v" image-toggle-display     "toggle    " :column "view  ")
    ("v x" image-toggle-hex-display "hex text  "                 )
    ("q"   quit-window              "quit      " :column "buffer")
    ("Q"   image-kill-buffer        "kill      "                 )))

(use-package doc-view
  :defer t
  :config
  (bind-keys :map doc-view-mode-map
             ("SPC" . nil) ("S-SPC" . nil) ("DEL" . nil)
             ("W" . nil) ("H" . nil) ("P" . nil) ("K" . nil)
             ("=" . nil) ("0" . nil) ("s" . nil) ("r" . nil)
             ("C-s" . nil) ("C-r" . nil) ("<find>" . nil) ("C-t" . nil)
             ("C-c C-c" . nil) ("C-c C-t" . nil)
             ("?" . pkg/hydra/group/doc-view-help/body))
  (defhydra pkg/hydra/group/doc-view-help
    (doc-view-mode-map "" :timeout pkg/hydra/timeout-sec :exit t)
    ("n"   doc-view-next-line-or-next-page         "line up  " :column "scroll")
    ("p"   doc-view-previous-line-or-previous-page "line down"                 )
    ("C-n" doc-view-next-page                      "page up  "                 )
    ("C-p" doc-view-previous-page                  "page down"                 )
    ("j p" doc-view-goto-page                      "page     " :column "browse")
    (": w" doc-view-fit-width-to-window            "width    " :column "fit   ")
    (": h" doc-view-fit-height-to-window           "height   "                 )
    (": p" doc-view-fit-page-to-window             "page     "                 )
    ("+"   doc-view-enlarge                        "enlarge  " :column "zoom  ")
    ("-"   doc-view-shrink                         "shrink   "                 )
    ("="   doc-view-scale-reset                    "reset    "                 )
    ("v v" doc-view-toggle-display                 "toggle   " :column "view  ")
    ("v f" doc-view-open-text                      "raw text "                 )
    ("g"   doc-view-revert-buffer                  "refresh  " :column "buffer")
    ("q"   quit-window                             "quit     "                 )
    ("Q"   doc-view-kill-proc                      "kill     "                 )))

(use-package pdf-tools
  :defer t
  :config
  (bind-keys :map pdf-view-mode-map
             ("SPC" . nil) ("S-SPC" . nil) ("DEL" . nil)
             ("W" . nil) ("H" . nil) ("P" . nil)
             ("=" . nil) ("0" . nil) ("s" . nil) ("r" . nil)
             ("m" . nil) ("'" . nil) ("M-g l" . nil)
             ("C-c C-c" . nil) ("C-c C-d" . nil) ("C-c C-i" . nil)
             ("C-c C-r m" . nil) ("C-c C-r p" . nil)
             ("?" . pkg/hydra/group/pdf-tools-help/body))
  (defhydra pkg/hydra/group/pdf-tools-help
    (pdf-view-mode-map "" :timeout pkg/hydra/timeout-sec :exit t)
    ("n"   pdf-view-next-line-or-next-page         "line up  " :column "scroll")
    ("p"   pdf-view-previous-line-or-previous-page "line down"                 )
    ("C-n" pdf-view-next-page-command              "page up  "                 )
    ("C-p" pdf-view-previous-page-command          "page down"                 )
    ("j p" pdf-view-goto-page                      "page     " :column "browse")
    ("j l" pdf-view-goto-label                     "label    "                 )
    ("j m" pdf-view-position-to-register           "register "                 )
    ("j M" pdf-view-jump-to-register               "restore  "                 )
    ("j o" pdf-outline                             "outline  "                 )
    (": w" pdf-view-fit-width-to-window            "width    " :column "fit   ")
    (": h" pdf-view-fit-height-to-window           "height   "                 )
    (": p" pdf-view-fit-page-to-window             "page     "                 )
    ("+"   pdf-view-enlarge                        "enlarge  " :column "zoom  ")
    ("-"   pdf-view-shrink                         "shrink   "                 )
    ("="   pdf-view-scale-reset                    "reset    "                 )
    ("v v" doc-view-mode                           "toggle   " :column "view  ")
    ("v e" pdf-view-extract-region-image           "extract  "                 )
    ("g"   revert-buffer                           "refresh  " :column "buffer")
    ("q"   quit-window                             "quit     "                 )
    ("Q"   kill-this-buffer                        "kill     "                 ))
  (use-package pdf-outline
    :defer t
    :config
    (bind-keys :map pdf-outline-minor-mode-map
               ("o" . nil)))
  (use-package pdf-history
    :defer t
    :config
    (bind-keys :map pdf-history-minor-mode-map
               ("B" . nil) ("N" . nil)))
  (use-package pdf-links
    :defer t
    :config
    (bind-keys :map pdf-links-minor-mode-map
               ("f" . nil) ("F" . nil)))
  (use-package pdf-misc
    :defer t
    :config
    (bind-keys :map pdf-misc-minor-mode-map
               ("I" . nil) ("C-c C-p" . nil))))


(provide 'my/keys/text)
