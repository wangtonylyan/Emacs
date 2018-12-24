;; -*- coding: utf-8 -*-

(require 'my/prog)


(defun pkg/lispbox/init ()
  (let ((exec (my/locate-exec "sbcl")))
    (when (and exec
               (require 'slime nil t)
               (require 'slime-autoloads nil t))
      (setq inferior-lisp-program exec ;; 指定SLIME所依赖的Lisp实现环境
            ;; 指定当前加载的contributed package
            slime-contribs '(slime-fancy
                             slime-scratch
                             slime-editing-commands
                             slime-repl
                             inferior-slime
                             slime-autodoc)
            slime-description-autofocus nil)
      (slime-setup) ;; 使上述定制生效
      (defun pkg/lispbox/start ()
        ;; (when (fboundp 'slime-mode))
        (slime-mode 1) ;; 启用smile-mode
        (save-excursion (slime))) ;; 启动SBCL，并连接Swank
      (my/add-mode-hook "lisp" 'pkg/lispbox/start))))


(eval-after-load 'lisp-mode ;; /lisp/emacs-lisp/lisp-mode.el
  '(progn
     (pkg/lispbox/init)))




(use-package sml-mode
  :if (and (my/package-enabled-p 'sml-mode)
           (or (my/locate-exec "sml" nil t)  ;; e.g. sml.bat on Windows
               (my/locate-exec "sml" "/usr/share/smlnj/bin" t)))
  :init
  (add-to-list 'auto-mode-alist '("\\.sml$" . sml-mode) t)
  (add-to-list 'auto-mode-alist '("\\.sig$" . sml-mode) t)
  (setq sml-indent-level 2)
  (defun pkg/sml-mode/sml-mode-hook ()
    (setq indent-tabs-mode nil))
  (my/add-mode-hook "sml" 'pkg/sml-mode/sml-mode-hook)
  :config
  (bind-keys :map sml-mode-map
             ("M-SPC" . just-one-space)))




(defun pkg/haskell-mode/init ()
  (use-package haskell-mode
    :if (my/package-enabled-p 'haskell-mode)
    :init
    (setq haskell-process-type 'stack-ghci ;; rely entirely on Stack
          haskell-process-path-ghci "ghci"
          haskell-process-path-cabal "cabal"
          haskell-process-path-stack "stack"
          haskell-process-suggest-remove-import-lines t
          haskell-process-suggest-hoogle-imports t
          haskell-process-auto-import-loaded-modules t
          haskell-process-log t ;; *haskell-process-log* buffer
          haskell-stylish-on-save t
          haskell-mode-stylish-haskell-path "stylish-haskell"
          haskell-tags-on-save t
          haskell-hasktags-path "hasktags"
          haskell-interactive-types-for-show-ambiguous nil
          ;; (haskell-compile)执行的是以下三个命令之一，前两者都用于Cabal所管理的项目
          ;; 目前暂没有发现更多的配置选项，以支持定制Stack环境内的GHC
          ;; 因此采用以下临时性方案，建议多使用(haskell-process-*)相关命令
          ;; haskell-compile-cabal-build-command
          ;; haskell-compile-cabal-build-alt-command
          haskell-compile-command "stack ghc %s")
    (my/add-mode-hook "haskell" 'pkg/haskell-mode/start)
    :config
    (bind-keys :map haskell-mode-map
               ("M-." . haskell-mode-jump-to-def-or-tag) ;; (haskell-mode-tag-find)
               ("C-c C-c" . haskell-compile)
               ;; ("" . haskell-interactive-switch)
               ("C-c C-l" . haskell-process-load-file) ;; (haskell-process-load-or-reload)
               ("C-c C-r" . haskell-process-restart) ;; (haskell-process-clear)
               ("" . haskell-process-do-type)
               ("" . haskell-process-do-info)
               ("" . haskell-navigate-imports)
               ("" . haskell-mode-format-imports)
               ("" . haskell-sort-imports)
               ("" . haskell-align-imports)
               ("" . haskell-process-cabal)
               ("" . haskell-process-cabal-build)
               :map haskell-cabal-mode-map)
    (if (my/package-enabled-p 'hindent)
        (progn
          (use-package hindent
            :commands (hindent-mode))
          (my/add-mode-hook "haskell" 'hindent-mode))
      (my/add-mode-hook "haskell" 'haskell-indentation-mode) ;; (turn-on-haskell-indent(ation))
      )
    (my/add-mode-hook "haskell" 'haskell-doc-mode) ;; (turn-on-haskell-doc-(mode))
    ;; (my/add-mode-hook "haskell" 'turn-on-haskell-unicode-input-method)
    ;; (my/add-mode-hook "haskell" 'haskell-auto-insert-module-template)
    ))

(defun pkg/haskell-mode/start ()
  (setq haskell-indentation-electric-flag t))



(eval-after-load 'prog-mode
  '(progn
     (pkg/haskell-mode/init)))


(provide 'my/prog-fun)
