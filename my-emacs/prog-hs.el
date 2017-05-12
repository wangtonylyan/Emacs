(require 'my-prog)

(setq my-prog-haskell-mode-start-hook '())

;; =============================================================================
;; Haskell-Mode
;; https://github.com/haskell/haskell-mode
;; -----------------------------------------------------------------------------
(defun my-emacs-plugin-haskell-mode-install-script ()
  ;; 必须切换至my-emacs-haskell-mode-compile-file列表中的文件同一目录下
  ;; (或在上述目录中建立了一个文件存储下述脚本，并随后执行之)
  ;; 模仿makefile中的内容，执行以下脚本
  (add-to-list 'load-path (expand-file-name "tests/compat") 'append)
  (setq byte-compile-error-on-warn t)
  (setq load-prefer-newer t)
  ;; 生成Emacs的autoloads文件
  (setq make-backup-files nil)
  (setq generated-autoload-file (expand-file-name "haskell-mode-autoloads.el")) ;; 指定目标文件名
  (update-directory-autoloads (expand-file-name ".")) ;; 还可以M-x交互式地执行该命令
  )

(defun my-plugin-haskell-mode-init ()
  (when (and (member 'haskell-mode package-selected-packages)
             (require 'haskell-mode-autoloads nil t))
    ;; (setq haskell-stylish-on-save t)
    ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    ))

(defun my-plugin-haskell-mode-start ()
  )

;; =============================================================================
;; =============================================================================
(defun my-haskell-mode-init ()
  (my-plugin-haskell-mode-init))

(defun my-haskell-mode-start ()
  (my-plugin-haskell-mode-start))

(eval-after-load 'prog-mode
  '(progn
     (my-haskell-mode-init)
     (add-hook 'prog-mode-hook 'my-haskell-mode-start)))

(provide 'my-prog-haskell)
