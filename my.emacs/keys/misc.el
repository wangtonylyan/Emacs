;; -*- coding: utf-8 -*-

(defun pkg/hydra/group/highlight/body ()
  (interactive)
  (cond
   ((pkg/package/enabled-p 'symbol-overlay)
    (pkg/hydra/group/symbol-overlay/body))
   ((pkg/package/enabled-p 'highlight-symbol)
    (pkg/hydra/group/highlight-symbol/body))
   (t (pkg/hydra/group/hi-lock/body))))

(use-package symbol-overlay
  :defer t
  :commands (pkg/hydra/group/symbol-overlay/body)
  :config
  (setq symbol-overlay-map (make-sparse-keymap))
  (defhydra pkg/hydra/group/symbol-overlay
    (symbol-overlay-map "" :timeout pkg/hydra/timeout-sec :exit t)
    ("i" symbol-overlay-put                  "toggle at point" :column "highlight")
    ("t" symbol-overlay-toggle-in-scope      "toggle scope   "                    )
    ("u" symbol-overlay-remove-all           "unhighlight all"                    )
    ("n" symbol-overlay-jump-next            "next           " :column "move     ")
    ("p" symbol-overlay-jump-prev            "prev           "                    )
    ("<" symbol-overlay-jump-first           "first          "                    )
    (">" symbol-overlay-jump-last            "last           "                    )
    ("f" symbol-overlay-switch-forward       "forward        " :column "others   ")
    ("b" symbol-overlay-switch-backward      "backward       "                    )
    ("y" symbol-overlay-save-symbol          "copy           " :column "action   ")
    ("Y" symbol-overlay-rename               "rename         "                    )
    ("w" symbol-overlay-query-replace        "query replace  "                    )
    ("." symbol-overlay-jump-to-definition   "to definition  "                    )
    ("?" pkg/hydra/group/symbol-overlay/body "key bindings   " :column "help     ")))

(use-package highlight-symbol
  :defer t
  :commands (pkg/hydra/group/highlight-symbol/body)
  :config
  (defhydra pkg/hydra/group/highlight-symbol
    (:timeout pkg/hydra/timeout-sec)
    ("q" pkg/hydra/quit nil :exit t)))

(use-package hi-lock
  :defer t
  :commands (pkg/hydra/group/hi-lock/body)
  :config
  (defhydra pkg/hydra/group/hi-lock
    (:timeout pkg/hydra/timeout-sec)
    ("i" highlight-symbol-at-point       "at point   " :column "highlight  ")
    ("s" highlight-phrase                "word       "                      )
    ("r" highlight-regexp                "regexp     "                      )
    ("l" highlight-lines-matching-regexp "regexp line"                      )
    ("u" unhighlight-regexp              "regexp     " :column "unhighlight")
    ("q" pkg/hydra/quit nil :exit t)))

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

(use-package projectile
  :defer t
  :commands (pkg/hydra/group/projectile/body)
  :preface
  (defun pkg/hydra/projectile/alternate (func)
    (funcall (cond
              ((pkg/package/enabled-p 'helm-projectile)
               (intern (concat "helm-" (symbol-name func))))
              (t func))))
  :config
  ;; (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
  (defhydra pkg/hydra/group/projectile
    (:timeout pkg/hydra/timeout-sec :exit t)
    "
PROJECT: %(projectile-project-root)

"
    ("h" helm-projectile                            "helm     " :column "project  ")
    ("p" (pkg/hydra/projectile/alternate
          #'projectile-switch-project)              "open     "                    )
    ("P" projectile-switch-open-project             "switch   "                    )
    ("v" projectile-vc                              "version  "                    )
    ("x" projectile-remove-known-project            "remove   "                    )
    ("X" projectile-cleanup-known-projects          "cleanup  "                    )
    ("b" (pkg/hydra/projectile/alternate
          #'projectile-switch-to-buffer)            "switch   " :column "buffer   ")
    ("k" projectile-kill-buffers                    "kill     "                    )
    ("f" (pkg/hydra/projectile/alternate
          #'projectile-find-file)                   "this proj" :column "find file")
    ("F" projectile-find-file-in-known-projects     "all proj "                    )
    ("j" projectile-find-file-in-directory          "the dir  "                    )
    ("r" (pkg/hydra/projectile/alternate
          #'projectile-recentf)                     "recent   "                    )
    ("t" projectile-find-other-file                 "same name"                    )
    ("d" (pkg/hydra/projectile/alternate
          #'projectile-find-dir)                    "find     " :column "directory")
    ("D" projectile-dired                           "dired    "                    )
    ("o" (pkg/hydra/projectile/alternate
          #'projectile-grep)                        "grep     " :column "symbol   ")
    ("O" projectile-multi-occur                     "occur    "                    )
    ("w" projectile-replace                         "replace  "                    )
    ("!" projectile-run-shell-command-in-root       "shell    " :column "external ")
    ("&" projectile-run-async-shell-command-in-root "shell &  "                    )
    ("a" (pkg/hydra/projectile/alternate
          #'projectile-ag)                          "ag       "                    )
    ("c" (pkg/hydra/projectile/alternate
          #'projectile-ack)                         "ack      "                    )
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


(provide 'my/keys/misc)
