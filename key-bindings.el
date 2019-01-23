;; -*- coding: utf-8 -*-

;; 与输入法切换键冲突
;; (global-set-key (kbd "C-S-SPC") 'set-mark-command)
;; (global-unset-key (kbd "C-SPC"))

(unbind-key "C-x f") ;; (set-fill-column)
(unbind-key "C-x C-l") ;; (downcase-region)
(unbind-key "C-x C-u") ;; (upcase-region)
(unbind-key "C-M-v") ;; (scroll-other-window)
(unbind-key "M-s h")
(unbind-key "C-x o") ;; (other-window)
(unbind-key "C-x <left>") ;; (previous-buffer)
(unbind-key "C-x <right>") ;; (next-buffer)

;; 以下部分是重复绑定，目的是便于查阅
(bind-keys ("C-S-a" . mark-whole-buffer)
           ("<C-wheel-up>" . text-scale-increase)
           ("<C-wheel-down>" . text-scale-decrease)
           ("<C-up>" . text-scale-increase)
           ("<C-down>" . text-scale-decrease)
           ("C-x C--" . downcase-region)
           ("C-x C-=" . upcase-region)
           ("C-q" . read-only-mode)
           ("M-." . xref-find-definitions)
           ("M-," . xref-pop-marker-stack)
           ("M-!" . shell-command)
           ("C-S-h" . windmove-left)
           ("C-S-l" . windmove-right)
           ("C-S-k" . windmove-up)
           ("C-S-j" . windmove-down)
           ("C-+" . zoom)
           )

;; 命令集前缀，以C-c加单个字母为前缀，且全局性key map的前缀互不相同
;; C-c C- :: tabbar
;; C-c h :: helm
;; C-c c :: helm-gtags
;; C-c p :: projectile, helm-projectile
;; C-c g :: magit
;; C-c , :: CEDET/Semantic
;; C-c . :: CEDET/EDE
(bind-keys ("C-c w" . pkg/hydra/group/window/body) ;; window, windmove, winner, buffer-move, zoom
           ("C-c t" . pkg/hydra/group/directory/body) ;; treemacs, neotree
           ("C-c i" . pkg/hydra/group/highlight/body) ;; highlight, highlight-thing
           ("C-c b" . pkg/hydra/group/bookmark/body) ;; bookmark, bm, helm-bm
           ("C-c d" . pkg/hydra/group/diff/body) ;; ediff, vdiff
           ("C-c o" . pkg/hydra/group/org/body) ;; org
           )

(defhydra pkg/hydra/group/window (:timeout 10)
  ("+" enlarge-window              "enlarge horizontally" :column "window size"  )
  ("=" enlarge-window-horizontally "enlarge vertically"                          )
  ("_" shrink-window               "shrink horizontally"                         )
  ("-" shrink-window-horizontally  "shrink vertically"                           )
  ("p" scroll-other-window-down    "scroll up"            :column "scroll window")
  ("n" scroll-other-window         "scroll down"                                 )
  ("u" winner-undo                 "undo"                 :column "winner"       )
  ("r" winner-redo                 "redo"                                        )
  ("h" buf-move-left               "left"                 :column "move buffer"  )
  ("l" buf-move-right              "right"                                       )
  ("k" buf-move-up                 "up"                                          )
  ("j" buf-move-down               "down"                                        )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/directory (:timeout 10 :exit t)
  ("t" pkg/hydra/group/treemacs/body "choose" :column "treemacs")
  ("n" pkg/hydra/group/neotree/body  "choose"  :column "neotree")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/treemacs (:timeout 10 :exit t)
  ("t" treemacs-select-window        "select"        :column "window")
  ("1" treemacs-delete-other-windows "delete others"                 )
  ("u" treemacs                      nil                             )
  ("F" treemacs-find-file            "find file"     :column "browse")
  ("T" treemacs-find-tag             "find tag"                      )
  ("B" treemacs-bookmark             "bookmark"                      )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/neotree (:timeout 10 :exit t)
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/highlight (:timeout 10)
  ("i" highlight-symbol-at-point       "at point"    :column "highlight"  )
  ("p" highlight-phrase                "phrase"                           )
  ("r" highlight-regexp                "regexp"                           )
  ("l" highlight-lines-matching-regexp "regexp line"                      )
  ("u" unhighlight-regexp              "regexp"      :column "unhighlight")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/bookmark (:timeout 10)
  ("m" bookmark-set        "set"      :column "bookmark")
  ("d" bookmark-delete     "unset"                      )
  ("r" bookmark-rename     "rename"                     )
  ("l" bookmark-bmenu-list "list"                       )
  ("p" bm-previous         "previous" :column "browse"  )
  ("n" bm-next             "next"                       )
  ("t" bm-toggle           "toggle"                     )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/diff (:timeout 10 :exit t)
  ("e" pkg/hydra/group/ediff/body "choose" :column "ediff")
  ("v" pkg/hydra/group/vdiff/body "choose" :column "vdiff")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/ediff (:timeout 10 :exit t)
  ("f" ediff-files        "2 files"   :column "file"  )
  ("F" ediff-files3       "3 files"                   )
  ("b" ediff-buffers      "2 buffers" :column "buffer")
  ("B" ediff-buffers3     "3 buffers"                 )
  ("d" ediff-current-file "current"                   )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/vdiff (:timeout 10 :exit t)
  ("f" vdiff-files        "2 files"   :column "file"  )
  ("F" vdiff-files3       "3 files"                   )
  ("b" vdiff-buffers      "2 buffers" :column "buffer")
  ("B" vdiff-buffers3     "3 buffers"                 )
  ("d" vdiff-current-file "current"                   )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/org (:timeout 10 :exit t)
  ("c" org-capture "capture" :column "org mode")
  ("a" org-agenda  "agenda"                    )
  ("q" pkg/hydra/quit nil :exit t))
