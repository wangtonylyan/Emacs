(require 'my-prog)

;===========================================================================
; Haskell-Mode
;===========================================================================
; https://github.com/haskell/haskell-mode
; 下载后执行以下函数完成插件的安装
(defun my-emacs-plugin-haskell-mode-install-script ()
  ; 必须切换至my-emacs-haskell-mode-compile-file列表中的文件同一目录下
  ; (或在上述目录中建立了一个文件存储下述脚本，并随后执行之)
  ; 模仿makefile中的内容，执行以下脚本
  (add-to-list 'load-path (expand-file-name "tests/compat") 'append)
  (when (< emacs-major-version 24)
    (setq byte-compile-warnings '(not cl-functions)))
  (setq byte-compile-error-on-warn t)
  (when (not (version< emacs-version "24.4")) (setq load-prefer-newer t))
  (setq my-emacs-haskell-mode-compile-file ; 需要被编译的源文件
        '("ghc-core.el"
          "ghci-script-mode.el"
          "highlight-uses-mode.el"
          "haskell-align-imports.el"
          "haskell-cabal.el"
          "haskell-checkers.el"
          "haskell-collapse.el"
          "haskell-modules.el"
          "haskell-sandbox.el"
          "haskell-commands.el"
          "haskell-compat.el"
          "haskell-compile.el"
          "haskell-complete-module.el"
          "haskell-completions.el"
          "haskell-customize.el"
          "haskell-debug.el"
          "haskell-decl-scan.el"
          "haskell-doc.el"
          "haskell.el"
          "haskell-font-lock.el"
          "haskell-hoogle.el"
          "haskell-indentation.el"
          "haskell-indent.el"
          "haskell-interactive-mode.el"
          "haskell-lexeme.el"
          "haskell-load.el"
          "haskell-menu.el"
          "haskell-mode.el"
          "haskell-move-nested.el"
          "haskell-navigate-imports.el"
          "haskell-presentation-mode.el"
          "haskell-process.el"
          "haskell-repl.el"
          "haskell-session.el"
          "haskell-sort-imports.el"
          "haskell-string.el"
          "haskell-unicode-input-method.el"
          "haskell-utils.el"
          "inf-haskell.el"))
  (mapc (lambda (filename)
          (byte-compile-file filename))
        my-emacs-haskell-mode-compile-file)
  ; 生成Emacs的autoloads文件
  (setq make-backup-files nil)
  (setq generated-autoload-file (expand-file-name "haskell-mode-autoloads.el")) ; 指定目标文件
  (update-directory-autoloads (expand-file-name ".")) ; 推荐以M-x交互式地执行该命令
  )

(defun my-plugin-haskell-mode-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "haskell-mode"))
  (when (require 'haskell-mode)
    (setq haskell-stylish-on-save t)
;    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    ))
(defun my-plugin-haskell-mode-start ()
  )

;===========================================================================
;===========================================================================
(defun my-haskell-mode-init ()
  (my-plugin-haskell-mode-init)
  )
(defun my-haskell-mode-start ()
  (my-plugin-haskell-mode-start)
  )

(eval-after-load 'prog-mode
  '(progn
     (my-haskell-mode-init)
     (add-hook 'prog-mode-hook 'my-haskell-mode-start)
     ))

(provide 'my-prog-haskell)
