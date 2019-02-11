;; -*- coding: utf-8 -*-

;; 在foo-mode-map中绑定("x y" . foo)
;; (defhydra foo (foo-mode-map "x") ("y" . foo))
;; 在global-map中新增快捷键前缀"x"
;; (bind-key "x" #'foo/body global-map)
(use-package hydra
  :ensure t
  :demand t
  :preface
  (defconst pkg/hydra/timeout-sec 30)
  (defun pkg/hydra/quit ()
    (interactive)
    (message "Hydra Quit"))
  :init
  ;; 目前发现启用此项会导致，Hydra子窗口过小，无法完整地呈现提示文字
  ;; 此外，启用全局的zoom mode似乎也可以避免该问题
  (setq hydra-lv nil))

;; 与输入法切换键冲突
;; (global-set-key (kbd "C-S-SPC") 'set-mark-command)
;; (global-unset-key (kbd "C-SPC"))

(unbind-key "C-x f"      ) ;; (set-fill-column)
(unbind-key "C-x C-l"    ) ;; (downcase-region)
(unbind-key "C-x C-u"    ) ;; (upcase-region)
(unbind-key "C-M-v"      ) ;; (scroll-other-window)
(unbind-key "M-s h"      )
(unbind-key "C-x o"      ) ;; (other-window)
(unbind-key "C-x <left>" ) ;; (previous-buffer)
(unbind-key "C-x <right>") ;; (next-buffer)
(unbind-key "M-{"        ) ;; (forward-paragraph)
(unbind-key "M-}"        ) ;; (backward-paragraph)
(unbind-key "C-M-n"      ) ;; (forward-list)
(unbind-key "C-M-p"      ) ;; (backward-list)
(unbind-key "C-M-d"      ) ;; (down-list)
(unbind-key "C-M-u"      ) ;; (backward-up-list)
(unbind-key "C-M-a"      ) ;; (beginning-of-defun)
(unbind-key "C-M-e"      ) ;; (end-of-defun)
(unbind-key "C-x d"      ) ;; (dired)
(unbind-key "C-s"        ) ;; (isearch-forward)
(unbind-key "C-r"        ) ;; (isearch-backward)
(unbind-key "M-/"        ) ;; (dabbrev-expand)

;; 以下部分是重复绑定，目的是便于查阅
(bind-keys
 ("M-x"            . helm-M-x                )
 ("M-y"            . helm-show-kill-ring     )
 ("C-x C-f"        . helm-find-files         )
 ("C-x C-r"        . helm-recentf            )
 ("C-x b"          . helm-mini               )
 ("C-x C-b"        . helm-buffers-list       )
 ("C-o"            . helm-occur              )
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
 ;; ("C-S-y")      ;; tabbar
 ;; ("C-S-u")      ;; tabbar
 ;; ("C-S-i")      ;; tabbar
 ;; ("C-S-o")      ;; tabbar
 ;; ("C-:")        ;; avy, ace-jump-mode
 ;; ("C-;")        ;; avy, ace-jump-mode
 ;; ("C-\"")       ;; flyspell, flyspell-correct
 ;; ("C-'")        ;; flyspell, flyspell-correct
 ("M-c"            . pkg/hydra/group/body    )
 ;; , :: CEDET/Semantic
 ;; . :: CEDET/EDE
 )

(defhydra pkg/hydra/group (:timeout pkg/hydra/timeout-sec :exit t)
  ("h" helm-command-prefix
   "helm" :column "")
  ("w" pkg/hydra/group/window/body
   "window, windmove, winner, buffer-move, zoom")
  ("c" pkg/hydra/group/cursor/body
   "cursor, paredit")
  ("t" (lambda () (interactive)
         (cond
          ((my/package-enabled-p 'treemacs) (pkg/hydra/group/treemacs/body))
          ((my/package-enabled-p 'neotree) (pkg/hydra/group/neotree/body))
          (t (user-error "*pkg/hydra/group* no package enabled for \"t\""))))
   "dired, treemacs, neotree")
  ("i" pkg/hydra/group/highlight/body
   "highlight, highlight-thing")
  ("b" pkg/hydra/group/bookmark/body
   "bookmark, bm, helm-bm")
  ("d" pkg/hydra/group/diff/body
   "ediff, vdiff")
  ("o" pkg/hydra/group/org/body
   "org")
  ("p" pkg/hydra/group/project/body
   "projectile, helm-projectile")
  ("!" (lambda () (interactive)
         (cond
          ((my/package-enabled-p 'flymake) (pkg/hydra/group/flymake/body))
          ((my/package-enabled-p 'flycheck) (pkg/hydra/group/flycheck/body))
          (t (user-error "*pkg/hydra/group* no package enabled for \"!\""))))
   "flymake, flycheck")
  ("g" (lambda () (interactive)
         (cond
          ((my/package-enabled-p 'ggtags) (pkg/hydra/group/ggtags/body))
          ((my/package-enabled-p 'helm-gtags) (pkg/hydra/group/helm-gtags/body))
          (t (user-error "*pkg/hydra/group* no package enabled for \"g\""))))
   "ggtags, helm-gtags")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/window (:timeout pkg/hydra/timeout-sec)
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
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/cursor (:timeout pkg/hydra/timeout-sec :exit t)
  ("C-    f,b" nil "char     " :column "forward/backward")
  ("   M- f,b" nil "word     "                           )
  ("C-    e,a" nil "line     " :column "beginning/end   ")
  ("   M- e,a" nil "sentence "                           )
  ("C- M- e,a" nil "paragraph"                           )
  ("C- M- f,b" nil "sexp     " :column "s-expression    ")
  ("C- M- j,k" nil "tree down"                           )
  ("C- M- l,h" nil "tree up  "                           )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/dired (dired-mode-map "" :timeout pkg/hydra/timeout-sec :exit t)
  "
Number of marked: %(pkg/dired/count-marked)

"
  ("C-f" forward-char           nil)
  ("C-b" backward-char          nil)
  ("C-e" move-end-of-line       nil)
  ("C-a" move-beginning-of-line nil)
  ("C-l" recenter-top-bottom    nil)
  ("s"       dired-sort-toggle-or-edit    "sort             " :column "show    ")
  ("g"       dired-do-redisplay           "refresh          "                   )
  ("G"       revert-buffer                "revert           "                   )
  ("k"       dired-do-kill-lines          "hide line        "                   )
  ("K"       dired-hide-details-mode      "hide details     "                   )
  ("n"       dired-next-line              "next line        " :column "move    ")
  ("p"       dired-previous-line          "prev line"                           )
  ("C-n"     dired-next-dirline           "next dir         "                   )
  ("C-p"     dired-prev-dirline           "prev dir         "                   )
  ("o"       dired-goto-file              "goto             "                   )
  ("l"       dired-up-directory           "parent           "                   )
  ("m"       dired-mark                   "mark             " :column "mark    ")
  ("M"       dired-mark-files-regexp      "mark regexp      "                   )
  ("u"       dired-unmark                 "unmark           "                   )
  ("U"       dired-unmark-all-marks       "unmark all       "                   )
  ("t"       dired-toggle-marks           "toggle (un)marked"                   )
  ("M-n"     dired-next-marked-file       "next marked      "                   )
  ("M-p"     dired-prev-marked-file       "prev marked      "                   )
  ("v"       dired-view-file              "view             " :column "file    ")
  ("V"       dired-display-file           "display          "                   )
  ("f"       dired-find-file              "open             "                   )
  ("F"       dired-find-file-other-window "open other       "                   )
  ("c"       dired-do-copy                "copy             "                   )
  ("C"       dired-do-rename              "move             "                   )
  ("d"       dired-flag-file-deletion     "flag delete      " :column "delete  ")
  ("#"       dired-flag-auto-save-files   "flag auto-saved  "                   )
  ("~"       dired-flag-backup-files      "flag backup      "                   )
  ("."       dired-clean-directory        "flag clean       "                   )
  ("x"       dired-do-flagged-delete      "delete flagged   "                   )
  ("D"       dired-do-delete              "delete marked    "                   )
  (": m  "   dired-do-chmod               "chmod            " :column "property")
  (": o  "   dired-do-chown               "chown            "                   )
  (": g  "   dired-do-chgrp               "chgrp            "                   )
  (": t  "   dired-do-touch               "touch            "                   )
  (": d  "   dired-create-directory       "create dir       "                   )
  ("!"       dired-do-shell-command       "shell            " :column "external")
  ("&"       dired-do-async-shell-command "shell &          "                   ))

(defhydra pkg/hydra/group/treemacs (:timeout pkg/hydra/timeout-sec :exit t)
  ("d"   dired                         "enable       " :column "dired  ")
  ("t"   pkg/treemacs/select-window    "select       " :column "window ")
  ("1"   treemacs-delete-other-windows "delete others"                  )
  ("u"   treemacs                      nil                              )
  ("C-f" treemacs-follow-mode          "toggle       " :column "follow ")
  ("f"   treemacs-find-file            "file         "                  )
  ("s"   treemacs-find-tag             "tag          "                  )
  ("b"   treemacs-bookmark             "bookmark     "                  )
  ("p"   treemacs-projectile           "setup        " :column "project")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/neotree (:timeout pkg/hydra/timeout-sec :exit t)
  ("d" dired              "enable" :column "dired ")
  ("t" pkg/neotree/toggle "select" :column "window")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/highlight (:timeout pkg/hydra/timeout-sec)
  ("i" highlight-symbol-at-point       "at point   " :column "highlight  ")
  ("p" highlight-phrase                "phrase     "                      )
  ("r" highlight-regexp                "regexp     "                      )
  ("l" highlight-lines-matching-regexp "regexp line"                      )
  ("u" unhighlight-regexp              "regexp     " :column "unhighlight")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/bookmark (:timeout pkg/hydra/timeout-sec)
  ("m" bookmark-set        "set     " :column "bookmark")
  ("d" bookmark-delete     "unset   "                   )
  ("r" bookmark-rename     "rename  "                   )
  ("l" bookmark-bmenu-list "list    "                   )
  ("p" bm-previous         "previous" :column "browse  ")
  ("n" bm-next             "next    "                   )
  ("t" bm-toggle           "toggle  "                   )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/diff (:timeout pkg/hydra/timeout-sec :exit t)
  ("e" pkg/hydra/group/ediff/body "choose" :column "ediff")
  ("v" pkg/hydra/group/vdiff/body "choose" :column "vdiff")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/ediff (:timeout pkg/hydra/timeout-sec :exit t)
  ("f" ediff-files        "2 files  " :column "file  ")
  ("F" ediff-files3       "3 files  "                 )
  ("b" ediff-buffers      "2 buffers" :column "buffer")
  ("B" ediff-buffers3     "3 buffers"                 )
  ("d" ediff-current-file "current  "                 )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/vdiff (:timeout pkg/hydra/timeout-sec :exit t)
  ("f" vdiff-files        "2 files  " :column "file  ")
  ("F" vdiff-files3       "3 files  "                 )
  ("b" vdiff-buffers      "2 buffers" :column "buffer")
  ("B" vdiff-buffers3     "3 buffers"                 )
  ("d" vdiff-current-file "current  "                 )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/org (:timeout pkg/hydra/timeout-sec :exit t)
  ("c" org-capture "capture" :column "org mode")
  ("a" org-agenda  "agenda "                   )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/project (:timeout pkg/hydra/timeout-sec :exit t)
  "
PROJECT: %(projectile-project-root)

"
  ("h"   helm-projectile                            "helm          " :column "project  ")
  ("p"   helm-projectile-switch-project             "open          "                    )
  ("C-p" projectile-switch-project                  nil                                 )
  ("P"   projectile-switch-open-project             "switch        "                    )
  ("v"   projectile-vc                              "version       "                    )
  ("x"   projectile-remove-known-project            "remove        "                    )
  ("X"   projectile-cleanup-known-projects          "cleanup       "                    )
  ("b"   helm-projectile-switch-to-buffer           "switch        " :column "buffer   ")
  ("C-b" projectile-switch-to-buffer                nil                                 )
  ("k"   projectile-kill-buffers                    "kill          "                    )
  ("f"   helm-projectile-find-file                  "find          " :column "file     ")
  ("C-f" projectile-find-file                       nil                                 )
  ("F"   projectile-find-file-in-known-projects     "find all      "                    )
  ("r"   helm-projectile-recentf                    "recent        "                    )
  ("l"   projectile-find-file-in-directory          "find in dir   "                    )
  ("C-r" projectile-recentf                         nil                                 )
  ("t"   projectile-find-other-file                 "with same name"                    )
  ("d"   helm-projectile-find-dir                   "find          " :column "directory")
  ("C-d" projectile-find-dir                        nil                                 )
  ("D"   projectile-dired                           "dired         "                    )
  ("o"   helm-projectile-grep                       "grep          " :column "symbol   ")
  ("C-o" projectile-grep                            nil                                 )
  ("O"   projectile-multi-occur                     "occur         "                    )
  ("w"   projectile-replace                         "replace       "                    )
  ("!"   projectile-run-shell-command-in-root       "shell         " :column "external ")
  ("&"   projectile-run-async-shell-command-in-root "shell &       "                    )
  ("a"   helm-projectile-ag                         "ag            "                    )
  ("C-a" projectile-ag                              nil                                 )
  ("c"   helm-projectile-ack                        "ack           "                    )
  ("C-c" projectile-ack                             nil                                 )
  ;; todo
  ;; helm-projectile-browse-dirty-projects
  ;; C-c p V         projectile-browse-dirty-projects
  ;; C-c p c         projectile-compile-project
  ;; C-c p I         projectile-ibuffer
  ;; C-c p S         projectile-save-project-buffers
  ;; C-c p j         projectile-find-tag
  ;; C-c p R         projectile-regenerate-tags
  ;; C-c p i         projectile-invalidate-cache
  ;; C-c p z         projectile-cache-current-file
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/flymake (:timeout pkg/hydra/timeout-sec :exit t)
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/flycheck (:timeout pkg/hydra/timeout-sec :exit t)
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
  ("w" flycheck-copy-errors-as-kill    "copy     "                            )
  ("?" flycheck-describe-checker       "describe " :column "checker"          )
  ("e" pkg/flycheck/enable-checker     "enable   "                            )
  ("d" flycheck-disable-checker        "disable  "                            )
  ("s" flycheck-select-checker         "select   "                            )
  ("v" flycheck-verify-setup           "info     " :column "buffer "          )
  ("g" flycheck-buffer                 "refresh  "                            )
  ("G" flycheck-compile                "compile  "                            )
  ("k" flycheck-clear                  "clear    "                            )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/ggtags (:timeout pkg/hydra/timeout-sec :exit t)
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
  ("K"   ggtags-delete-tags              "clean      "                   )
  ("d"   ggtags-visit-project-root       "dired @root" :column "advance ")
  ("1"   ggtags-kill-file-buffers        "kill others"                   )
  ("v h" ggtags-browse-file-as-hypertext "view html  "                   )
  ("w"   ggtags-query-replace            "replace    "                   )
  ("C-q" ggtags-toggle-project-read-only "readonly   "                   )
  ("q" pkg/hydra/quit nil :exit t))

;; todo :: 在以下交互函数执行时，输入"C-u"，还可限定搜索的目录路径
(defhydra pkg/hydra/group/helm-gtags (:timeout pkg/hydra/timeout-sec :exit t)
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
  ("s"   helm-gtags-tags-in-this-function "in function" :column "select  ")
  ("f"   helm-gtags-parse-file            "in file    "                   )
  ("p"   helm-gtags-select                "in project "                   )
  ("d"   helm-gtags-select-path           "path       "                   )
  ("g"   helm-gtags-update-tags           "refresh    " :column "database")
  ("G"   helm-gtags-create-tags           "setup      "                   )
  ("k"   helm-gtags-clear-all-stacks      "clear stack"                   )
  ("K"   helm-gtags-clear-all-cache       "clear cache"                   )
  ("q" pkg/hydra/quit nil :exit t))


;; todo
(defhydra pkg/hydra/group/cedet (:timeout pkg/hydra/timeout-sec)
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
  ("b" semantic-mrub-switch-tags)
  )

(provide 'my/init-keys)
