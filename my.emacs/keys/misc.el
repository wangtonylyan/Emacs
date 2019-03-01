;; -*- coding: utf-8 -*-

(defhydra pkg/hydra/group/highlight
  (:timeout pkg/hydra/timeout-sec)
  ("i" highlight-symbol-at-point       "at point   " :column "highlight  ")
  ("s" highlight-phrase                "word       "                      )
  ("r" highlight-regexp                "regexp     "                      )
  ("l" highlight-lines-matching-regexp "regexp line"                      )
  ("u" unhighlight-regexp              "regexp     " :column "unhighlight")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/bookmark
  (:timeout pkg/hydra/timeout-sec)
  ("m" bookmark-set        "set     " :column "bookmark")
  ("d" bookmark-delete     "unset   "                   )
  ("r" bookmark-rename     "rename  "                   )
  ("l" bookmark-bmenu-list "list    "                   )
  ("p" bm-previous         "previous" :column "browse  ")
  ("n" bm-next             "next    "                   )
  ("t" bm-toggle           "toggle  "                   )
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/diff
  (:timeout pkg/hydra/timeout-sec :exit t)
  ("e" pkg/hydra/group/ediff/body "choose" :column "ediff")
  ("v" pkg/hydra/group/vdiff/body "choose" :column "vdiff")
  ("q" pkg/hydra/quit nil :exit t))

(defhydra pkg/hydra/group/ediff
  (:timeout pkg/hydra/timeout-sec :exit t)
  ("f" ediff-files        "2 files  " :column "file  ")
  ("F" ediff-files3       "3 files  "                 )
  ("b" ediff-buffers      "2 buffers" :column "buffer")
  ("B" ediff-buffers3     "3 buffers"                 )
  ("d" ediff-current-file "current  "                 )
  ("q" pkg/hydra/quit nil :exit t))

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
             ("C-c d h" . vdiff-hydra/body)))

(defhydra pkg/hydra/group/org
  (:timeout pkg/hydra/timeout-sec :exit t)
  ("c" org-capture "capture" :column "org mode")
  ("a" org-agenda  "agenda "                   )
  ("q" pkg/hydra/quit nil :exit t))

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
  ("q" pkg/hydra/quit nil :exit t))

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
     projectile-browse-dirty-projects)))


(provide 'my/keys/misc)
