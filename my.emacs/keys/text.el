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


(provide 'my/keys/text)
