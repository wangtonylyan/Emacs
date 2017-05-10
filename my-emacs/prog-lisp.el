(require 'my-prog)

(defun my-plugin-lispbox-init ()
  (let ((exec "sbcl"))
    (when (eq system-type 'windows-nt)
      (setq exec "sbcl.exe")
      (let* ((path (executable-find (concat "SBCL/" exec)))
             (dir (when path (file-name-directory path))))
        (when dir
          (unless (member dir exec-path)
            (add-to-list 'exec-path dir t)))))
    (when (executable-find exec)
      (when (and (require 'slime nil t)
                 (require 'slime-autoloads nil t))
        ;; 指定SLIME所依赖的Lisp实现环境
        (setq inferior-lisp-program exec)
        ;; 指定当前加载的contributed package
        (setq slime-contribs '(slime-fancy
                               slime-scratch
                               slime-editing-commands
                               slime-repl
                               inferior-slime
                               slime-autodoc
                               ))
        (setq slime-description-autofocus nil)
        (slime-setup) ;; 使上述定制生效
        ))))

(defun my-plugin-lispbox-start ()
  (slime-mode 1) ;; 启用smile-mode
  (save-excursion (slime)) ;; 启动SBCL，并连接Swank
  (when (boundp 'ac-sources)
    (setq ac-sources
          (append my-prog-ac-sources '(ac-source-slime)))))

;; =============================================================================
;; =============================================================================
(defun my-lisp-mode-init ()
  (my-plugin-lispbox-init))

(defun my-lisp-mode-start ()
  (when (fboundp 'slime-mode)
    (my-plugin-lispbox-start)))

(eval-after-load 'lisp-mode ;; /lisp/emacs-lisp/lisp-mode.el
  '(progn
     (my-lisp-mode-init)
     (add-hook 'lisp-mode-hook 'my-lisp-mode-start)))

;; =============================================================================
;; =============================================================================
(defun my-emacs-lisp-mode-init ()
  )

(defun my-emacs-lisp-mode-start ()
  (when (boundp 'ac-sources)
    (setq ac-sources
          (append my-prog-ac-sources '(ac-source-functions
                                       ac-source-variables
                                       ac-source-symbols
                                       ac-source-features)))))

(eval-after-load 'lisp-mode ;; /lisp/emacs-lisp/lisp-mode.el
  '(progn
     (my-emacs-lisp-mode-init)
     (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-start)))

(provide 'my-prog-lisp)
