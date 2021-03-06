;; -*- coding: utf-8 -*-

(defun pkg/hydra/group/syntax/body ()
  (interactive)
  (cond
   ((pkg/package/enabled-p 'flymake) (pkg/hydra/group/flymake/body))
   ((pkg/package/enabled-p 'flycheck) (pkg/hydra/group/flycheck/body))
   (t (user-error "*pkg/hydra/group/flymake&check/body* no package is enabled"))))

(use-package flymake
  :defer t
  :config
  (defhydra pkg/hydra/group/flymake
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("q" pkg/hydra/quit nil :exit t)))

(use-package flycheck
  :defer t
  :config
  (unbind-key flycheck-keymap-prefix flycheck-mode-map)
  (defhydra pkg/hydra/group/flycheck
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("n" flycheck-next-error             "next     " :column "error  " :exit nil)
    ("p" flycheck-previous-error         "prev     "                   :exit nil)
    ("C-l" recenter-top-bottom nil :exit nil)
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
    ("e" pkg/flycheck/enable-checker-i   "enable   "                            )
    ("d" flycheck-disable-checker        "disable  "                            )
    ("j" flycheck-select-checker         "select   "                            )
    ("v" flycheck-verify-setup           "info     " :column "buffer "          )
    ("g" flycheck-buffer                 "refresh  "                            )
    ("G" flycheck-compile                "compile  "                            )
    ("k" flycheck-clear                  "clear    "                            )
    ("q" pkg/hydra/quit nil :exit t)))

(defun pkg/hydra/group/tagging/body ()
  (interactive)
  (cond
   ((pkg/package/enabled-p 'ggtags) (pkg/hydra/group/ggtags/body))
   ((pkg/package/enabled-p 'helm-gtags) (pkg/hydra/group/helm-gtags/body))
   (t (user-error "*pkg/hydra/group/gtags/body* no package is enabled"))))

(use-package ggtags
  :defer t
  :config
  (unbind-key ggtags-mode-prefix-key ggtags-mode-map)
  (bind-keys :map ggtags-mode-map
             ("M-." . ggtags-find-tag-dwim)
             ("M-*" . ggtags-find-reference)
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
    ("o *" ggtags-find-reference           "M-*        "                   )
    ("o o" ggtags-grep                     "grep       "                   )
    ("o O" ggtags-find-tag-regexp          "pattern    "                   )
    ("o s" ggtags-find-other-symbol        "symbol     "                   )
    ("o f" ggtags-find-file                "file       "                   )
    ("m"   ggtags-save-to-register         "register   " :column "history ")
    ("M"   jump-to-register                "restore    "                   )
    ("l"   ggtags-view-search-history      "show       "                   )
    ("g"   ggtags-update-tags              "refresh    " :column "database")
    ("G"   ggtags-create-tags              "setup      "                   )
    ("X"   ggtags-delete-tags              "cleanup    "                   )
    ("d"   ggtags-visit-project-root       "dired @root" :column "advance ")
    ("1"   ggtags-kill-file-buffers        "kill others"                   )
    ("v h" ggtags-browse-file-as-hypertext "view html  "                   )
    ("w"   ggtags-query-replace            "replace    "                   )
    ("C-q" ggtags-toggle-project-read-only "readonly   "                   )
    ("q" pkg/hydra/quit nil :exit t)))

;; todo :: 在以下交互函数执行时，输入"C-u"，还可限定搜索的目录路径
(use-package helm-gtags
  :defer t
  :config
  (unbind-key helm-gtags-prefix-key helm-gtags-mode-map)
  (bind-keys :map helm-gtags-mode-map
             ("M-." . helm-gtags-dwim)
             ("M-," . helm-gtags-pop-stack)
             ("M-*" . helm-gtags-find-rtag)
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
    ("o *" helm-gtags-find-rtag             "M-*        "                   )
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
    ("q" pkg/hydra/quit nil :exit t)))

(use-package company
  :defer t
  :config
  (bind-keys :map company-active-map
             ("C-w" . nil) ;; (company-show-location)
             ("<f1>" . nil) ;; (company-show-doc-buffer)
             ("C-M-s" . nil) ;; (company-filter-candidates)
             ("?" . pkg/hydra/group/company-active/body)
             :map company-search-map
             ("C-o" . nil) ;; (company-search-toggle-filtering)
             ("?" . pkg/hydra/group/company-search/body))
  (defun pkg/company/move-next ()
    (interactive)
    (company-complete-common-or-cycle 1))
  (defun pkg/company/move-prev ()
    (interactive)
    (company-complete-common-or-cycle -1))
  (defun pkg/company/search-candidates ()
    (interactive)
    (if (pkg/package/enabled-p 'helm-company)
        (helm-company) (company-search-candidates)))
  (defhydra pkg/hydra/group/company-active
    (company-active-map "" :timeout pkg/hydra/timeout-sec :exit t)
    ("C-n"      company-select-next           "next       " :column "move    ")
    ("C-p"      company-select-previous       "prev       "                   )
    ("M-n"      pkg/company/move-next         "next+common"                   )
    ("M-p"      pkg/company/move-prev         "prev+common"                   )
    ("<tab>"    company-complete-common       "common     " :column "complete")
    ("<return>" company-complete-selection    "select     "                   )
    ("C-h"      company-show-doc-buffer       "docstring  " :column "help    ")
    ("C-S-h"    company-show-location         "code       "                   )
    ("C-?"      company-diag                  "company    "                   )
    ("C-s"      pkg/company/search-candidates "search     " :column "search  ")
    ("C-S-f"    company-filter-candidates     "filter     "                   )
    ("C-g"      company-abort                 nil                             ))
  (defhydra pkg/hydra/group/company-search
    (company-search-map "" :timeout pkg/hydra/timeout-sec :exit t)
    ("C-n" company-select-next             "next       " :column "move  ")
    ("C-p" company-select-previous         "prev       "                 )
    ("M-n" pkg/company/move-next           "next+common"                 )
    ("M-p" pkg/company/move-prev           "prev+common"                 )
    ("C-s" company-search-repeat-forward   "forward    " :column "search")
    ("C-r" company-search-repeat-backward  "backward   "                 )
    ("C-t" company-search-toggle-filtering "toggle     "                 )
    ("C-g" company-search-abort            nil                           )))


(defun pkg/hydra/group/semantic/body ()
  (interactive)
  (cond
   ((derived-mode-p 'emacs-lisp-mode) (pkg/hydra/group/elisp/body))
   ((derived-mode-p 'c-mode 'c++-mode) (pkg/hydra/group/cpp/body))
   ((derived-mode-p 'python-mode) (pkg/hydra/group/python/body))
   ((derived-mode-p 'haskell-mode) (pkg/hydra/group/haskell/body))))

(defun pkg/hydra/group/elisp/body ()
  (interactive))

(defun pkg/hydra/group/cpp/body ()
  (interactive)
  (cond
   (t (pkg/hydra/group/cedet/body))))

(defun pkg/hydra/group/python/body ())

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
    ("b" semantic-mrub-switch-tags)))

(use-package ycmd
  :defer t
  :config
  (unbind-key ycmd-keymap-prefix ycmd-mode-map))

(use-package anaconda-mode
  :defer t
  :config
  (bind-keys :map anaconda-mode-map
             ("C-M-i" . nil) ;; (anaconda-mode-complete)
             ("M-=" . nil)   ;; (anaconda-mode-find-assignments)
             ("M-?" . nil)   ;; (anaconda-mode-show-doc)
             ("M-." . anaconda-mode-find-definitions)
             ("M-r" . nil) ("M-*" . anaconda-mode-find-references)))

(use-package haskell-mode
  :defer t
  :config
  (setq haskell-mode-map (make-sparse-keymap)
        interactive-haskell-mode-map (make-sparse-keymap))
  (defhydra pkg/hydra/group/haskell
    (:timeout pkg/hydra/timeout-sec :exit t)
    ("C-s" haskell-mode-toggle-scc-at-point "toggle scc")

    ("i f" haskell-mode-format-imports      "format   " :column "import ")
    ("i i" haskell-navigate-imports         "goto     "                  )
    ("C-c" haskell-compile                  "compile  " :column "compile")
    ("c x" haskell-process-cabal            "cabal    " :column "cabal  ")
    ("c c" haskell-process-cabal-build      "build    "                  )
    ("c v" haskell-cabal-visit-file         "visit    "                  )
    ("C-b" haskell-interactive-switch       "switch   " :column "GHC    ")
    ("C-f" haskell-process-load-file        "load     "                  )
    ("C-r" haskell-process-reload           "reload   "                  )
    ("C-q" haskell-process-restart          "restart  "                  )
    ("C-l" haskell-process-clear            "clear    "                  )
    ("C-L" haskell-interactive-mode-clear   "clean    "                  )
    ("C-d" haskell-process-interrupt        "interrupt"                  ))
  (bind-keys :map haskell-mode-map
             ("M-." . haskell-mode-jump-to-def-or-tag) ;; (haskell-mode-tag-find)
             ("<backtab>" . haskell-delete-indentation)
             :map haskell-cabal-mode-map ;; .cabal file
             :map highlight-uses-mode-map))


(provide 'my/keys/program)
