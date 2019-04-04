;; -*- coding: utf-8 -*-

;; 两个很有用的内置命令：(ignore), (undefined)

;; 在foo-mode-map中绑定("x y" . foo)
;; (defhydra foo (foo-mode-map "x") ("y" . foo))
;; 在global-map中新增快捷键前缀"x"
;; (bind-key "x" #'foo/body global-map)

;; 与输入法切换键冲突
;; (global-set-key (kbd "C-S-SPC") 'set-mark-command)
;; (global-unset-key (kbd "C-SPC"))

;; TODO: 使用pretty-hydra-define插件发现以下几点问题
;; 1. 按键字符串会被当做正则表达式，导致声明诸如"^"之类的按键会导致错误
;; 2. 同一列上的按键字符之间不会对齐于冒号
;; 3. 既然"SPC"表示空格，那么"<return>"是否有必要简化为"RET"?

(pretty-hydra-define pkg/hydra/group
  (:hint nil :exit t :quit-key "q")
  ("general"
   (("h" pkg/hydra/group/helm/body      "helm     ")
    ("b" pkg/hydra/group/buffer/body    "buffer   ")
    ("c" pkg/hydra/group/cursor/body    "cursor   ")
    ("d" pkg/hydra/group/dired/body     "dired    "))
   "editing"
   (("i" pkg/hydra/group/highlight/body "highlight")
    ("m" pkg/hydra/group/bookmark/body  "bookmark ")
    ("=" pkg/hydra/group/diff/body      "diff     ")
    ("o" pkg/hydra/group/org/body       "org      ")
    ("p" pkg/hydra/group/project/body   "project  "))
   "programming"
   (("!" pkg/hydra/group/syntax/body    "syntax   ")
    ("." pkg/hydra/group/tagging/body   "tagging  ")
    ("," pkg/hydra/group/semantic/body  "semantic "))))

;; 以下部分是重复绑定，目的是便于查阅
(bind-keys
 ("C-x f" . nil)   ;; (set-fill-column)
 ("C-x C-l" . nil) ;; (downcase-region)
 ("C-x C-u" . nil) ;; (upcase-region)
 ("M-l" . nil)     ;; (downcase-word)
 ("M-u" . nil)     ;; (upcase-word)
 ;; ("M-s h" . nil) ;; 'hi-lock
 ("C-M-k" . nil) ;; (kill-sexp)
 ("C-s" . nil)   ;; (isearch-forward)
 ("C-r" . nil)   ;; (isearch-backward)
 ("M-/" . nil)   ;; (dabbrev-expand)
 ("M-c" . nil)   ;; (capitalize-word)
 ("M-m" . nil)   ;; (back-to-indentation)
 ("M-j" . nil)   ;; (indent-new-comment-line)
 ("C-M-x" . nil) ;; (eval-defun)
 ("C-x e" . nil) ;; (kmacro-end-and-call-macro)
 ("C-h h" . nil) ;; (view-hello-file)
 ("C-h f" . nil) ;; (describe-function)
 ("C-h v" . nil) ;; (describe-variable)
 ("M-z" . nil)   ;; (zap-to-char)
 ("M-h" . nil)   ;; (mark-paragraph)
 ("M-t" . nil)   ;; (transpose-words)
 ("M-g" . nil)   ;; (goto-)
 ("C-x C-c" . nil) ("C-x C-z" . save-buffers-kill-terminal)
 ("C-x C--" . downcase-region)
 ("C-x C-=" . upcase-region)
 ("C-x h" . nil) ;; (mark-whole-buffer)
 ("C-q" . read-only-mode)
 ("M-!" . shell-command)
 ("C-_" . nil) ;; (undo), (undo-only)
 ("C-/" . nil) ;; (undo), (undo-only)
 ("C-x C-u" . undo)
 ("M-i" . nil) ("M-t" . tab-to-tab-stop)
 ("M-i" . pkg/hydra/group/highlight-at-point)
 ("M-m" . pkg/hydra/group/body))

(when (pkg/package/enabled-p 'pyim)
  (bind-keys ("M-j" . pyim-convert-string-at-point)))

(use-package kmacro
  :defer t
  :config
  (bind-keys ("C-x C-k" . ;; (kmacro-keymap)
              (lambda () (interactive) ;; no need to confirm
                (kill-buffer)))))

(use-package xref
  :defer t
  :config
  (bind-keys
   ("M-." . xref-find-definitions)
   ("M-," . xref-pop-marker-stack)
   ("M-?" . nil) ("M-*" . xref-find-references)))

(use-package simple
  :defer t
  :config
  (bind-keys :map special-mode-map
             ("SPC" . nil) ("n" . scroll-up-command)
             ("S-SPC" . nil) ("DEL" . nil) ("p" . scroll-down-command)
             ("<" . beginning-of-buffer) (">" . end-of-buffer)
             ("h" . nil) ("?" . nil)
             ("g" . revert-buffer)
             ("q" . quit-window) ("Q" . kill-buffer)))

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
             ("r" . nil) ("C-c C-f" . nil) ("f" . help-go-forward)
             ("l" . nil) ("C-c C-b" . nil) ("b" . help-go-back)
             ("C-c C-c" . nil) ("." . help-follow-symbol)
             ("n" . forward-button)
             ("p" . backward-button)))

(use-package info
  :defer t
  :config
  (bind-keys :map Info-mode-map
             ("." . nil) ("b" . nil) ("e" . nil)
             ("]" . nil) ("[" . nil) ("<" . nil) (">" . nil)
             ("SPC" . nil) ("S-SPC" . nil)
             ("d" . nil) ("t" . nil) ("u" . nil) ("^" . nil)
             ("T" . nil) ("n" . nil) ("p" . nil)
             ("f" . nil) ("C-m" . nil)
             ("g" . nil) ("s" . nil) ("S" . nil)
             ("i" . nil) ("I" . nil) ("m" . nil) ("," . nil)
             ("r" . nil) ("l" . nil) ("L" . nil)
             ("M-n" . nil) ("w" . nil) ("c" . nil)
             ("?" . pkg/hydra/group/info-help/body))
  (pretty-hydra-define pkg/hydra/group/info-help
    (Info-mode-map "" :hint nil :exit t :quit-key "q")
    ("buffer"
     (("n"   Info-next-reference      "next node")
      ("p"   Info-prev-reference      "prev node")
      ("<"   beginning-of-buffer      "beginning")
      (">"   end-of-buffer            "end      "))
     "browse"
     (("S-6" Info-up                  "parent   ")
      ("."   Info-follow-nearest-node "follow   ")
      ("RET" Info-follow-nearest-node nil        )
      ("C-s" Info-menu                "menu     ")
      ("l"   Info-goto-node           "list all "))
     "history"
     (("f"   Info-history-forward     "forward  ")
      ("b"   Info-history-back        "backward ")
      (","   Info-history-back        nil        )
      ("/"   Info-history             "show     "))
     "info"
     (("h"   Info-help                "help     ")))))

(pretty-hydra-define pkg/hydra/group/helm
  (global-map "" :hint nil :exit t :quit-key "q")
  ("emacs"
   (("M-x"     helm-M-x               "M-x              ")
    ;; (""        helm-colors            "colors           ")
    ;; (""        helm-calcul-expression "calcul-expression")
    )
   "ring"
   (("M-y    " helm-show-kill-ring    "show-kill-ring   ")
    ("C-h SPC" helm-all-mark-rings    "all-mark-rings   "))
   "file"
   (("C-x C-f" helm-find-files        "find-files       ")
    ("C-x C-r" helm-recentf           "recentf          "))
   "buffer"
   (("C-x b  " helm-mini              "mini             ")
    ("C-x C-b" helm-buffers-list      "buffers-list     "))
   "symbol"
   (("C-o  "   helm-occur             "occur            ")
    ("C-S-o"   helm-regexp            "regexp           ")
    ("C-s  "   helm-semantic-or-imenu "semantic-or-imenu")
    ("C-h f"   helm-apropos           "apropos          ")
    ("C-h v"   helm-apropos           "apropos          "))
   "external"
   (("C-h h"   helm-man-woman         "man-woman        ")
    ;; (""        helm-locate            "locate           ")
    ;; (""        helm-find              "find             ")
    )))

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
             ("C-x u" . undo-tree-visualize)
             ("C-x C-u" . undo-tree-visualize)
             :map undo-tree-visualizer-mode-map
             ("d" . undo-tree-visualizer-toggle-diff)
             ("t" . undo-tree-visualizer-toggle-timestamps)
             ("q" . undo-tree-visualizer-quit)
             ("RET" . undo-tree-visualizer-quit)
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
