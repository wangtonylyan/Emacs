;; -*- coding: utf-8 -*-

;; 在foo-mode-map中绑定("x y" . foo)
;; (defhydra foo (foo-mode-map "x") ("y" . foo))
;; 在global-map中新增快捷键前缀"x"
;; (bind-key "x" #'foo/body global-map)

;; 与输入法切换键冲突
;; (global-set-key (kbd "C-S-SPC") 'set-mark-command)
;; (global-unset-key (kbd "C-SPC"))

(defhydra pkg/hydra/group
  (:timeout pkg/hydra/timeout-sec :exit t)
  ("h" pkg/hydra/group/helm/body          "helm, ivy" :column "")
  ("b" pkg/hydra/group/buffer/body        "window, windmove, winner, buffer-move, zoom, tabbar")
  ("c" pkg/hydra/group/cursor/body        "cursor, paredit, lispy, multiple-cursors")
  ("d" pkg/hydra/group/dired/body         "dired, treemacs, neotree")

  ("i" pkg/hydra/group/highlight/body     "highlight, highlight-thing")
  ("m" pkg/hydra/group/bookmark/body      "bookmark, bm, helm-bm")
  ("=" pkg/hydra/group/diff/body          "ediff, vdiff")
  ("o" pkg/hydra/group/org/body           "org")
  ("p" pkg/hydra/group/projectile/body    "projectile, helm-projectile")

  ("!" pkg/hydra/group/flymake&check/body "flymake, flycheck")
  ("." pkg/hydra/group/gtags/body         "ggtags, helm-gtags")

  ("q" pkg/hydra/quit nil :exit t))

;; 以下部分是重复绑定，目的是便于查阅
(bind-keys
 ("C-x f"          . nil) ;; (set-fill-column)
 ("C-x C-l"        . nil) ;; (downcase-region)
 ("C-x C-u"        . nil) ;; (upcase-region)
 ("M-s h"          . nil)
 ("C-M-k"          . nil) ;; (kill-sexp)
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
 ("M-z"            . nil) ;; (zap-to-char)
 ("C-x C--"        . downcase-region         )
 ("C-x C-="        . upcase-region           )
 ("C-S-a"          . mark-whole-buffer       )
 ("C-q"            . read-only-mode          )
 ("M-!"            . shell-command           )
 ("M-."            . xref-find-definitions   )
 ("M-,"            . xref-pop-marker-stack   )
 ("M-j"            . pkg/hydra/group/body    ))

(use-package kmacro
  :defer t
  :config
  (bind-keys ("C-x C-k" . ;; (kmacro-keymap)
              (lambda () (interactive) ;; no need to confirm
                (kill-buffer)))))

(use-package package
  :defer t
  :config
  (bind-keys :map package-menu-mode-map
             ("r" . nil)
             ("g" . package-menu-refresh)
             ("G" . package-refresh-contents)))

(use-package help-mode
  :defer t
  :config
  (bind-keys :map help-mode-map
             ("l" . nil) ("C-c C-b" . nil) ("h" . help-go-back)
             ("r" . nil) ("C-c C-f" . nil) ("l" . help-go-forward)
             ("C-c C-c" . nil) ("o" . help-follow-symbol)
             ("j" . forward-button)
             ("k" . backward-button)))

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
               ("M-a" . helm-mark-all))))

(use-package undo-tree
  :defer t
  :config
  (bind-keys :map undo-tree-map
             ("C-/" . nil) ;; (undo-tree-undo)
             ("C-_" . nil) ;; (undo-tree-undo)
             ("C-?" . nil) ;; (undo-tree-redo)
             ("M-_" . nil) ;; (undo-tree-redo)
             :map undo-tree-visualizer-mode-map
             ("d" . undo-tree-visualizer-toggle-diff)
             ("t" . undo-tree-visualizer-toggle-timestamps)
             ("q" . undo-tree-visualizer-quit)
             ("<return>" . undo-tree-visualizer-quit)
             ("C-g" . undo-tree-visualizer-abort)
             ("C-q" . undo-tree-visualizer-abort)
             ("\M-{" . nil) ("M-p" . undo-tree-visualize-undo-to-x)
             ("\M-}" . nil) ("M-n" . undo-tree-visualize-redo-to-x)))

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
               ("C-'" . flyspell-correct-wrapper))))


(provide 'my/keys/init)
