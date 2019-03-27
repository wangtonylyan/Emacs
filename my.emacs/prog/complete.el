;; -*- coding: utf-8 -*-

(defun my/prog/complete/add-hook (func)
  (my/add-mode-hook "my/prog/complete" func))

(defun my/prog/complete/run-hook ()
  (my/run-mode-hook "my/prog/complete"))

(my/add-mode-hook "init" #'my/prog/complete/run-hook)


(defun my/prog/complete/add-backends (defaults backends)
  (if (and (and (symbolp defaults) (boundp defaults)
                (listp (symbol-value defaults)))
           (or (listp backends) (symbolp backends)))
      (let ((local (make-local-variable
                    (intern (symbol-name defaults)))))
        (cond
         ((listp backends)
          (mapc (lambda (backend)
                  (let ((backend (if (listp backend)
                                     (delq nil backend) backend)))
                    (when backend (add-to-list local backend))))
                (delq nil (nreverse backends))))
         (backends
          (add-to-list local backends))))
    (user-error "*my/prog/complete/add-backends* illegal parameters")))


(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  :if (pkg/package/enabled-p 'yasnippet)
  :init
  (my/prog/complete/add-hook #'yas-global-mode)
  :config
  (setq yas-snippet-dirs (list (my/set-user-emacs-file "my.snippet/"))
        yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))
  (yas-reload-all)
  (use-package yasnippet-snippets
    :if (pkg/package/enabled-p 'yasnippet-snippets)
    :config
    (yasnippet-snippets-initialize)))

(use-package company
  :diminish company-mode
  :defer t
  :preface
  (defun pkg/company/backend-enabled-p (package)
    ;; package and backend mapping
    (let ((alist '((yasnippet . company-yasnippet)
                   (company-irony . company-irony)
                   (company-ycmd . company-ycmd)
                   (company-anaconda . company-anaconda)
                   (company-jedi . company-jedi))))
      (when (pkg/package/enabled-p package)
        (alist-get package alist))))
  (defun pkg/company/add-backends (backends)
    (my/prog/complete/add-backends 'company-backends backends))
  :if (pkg/package/enabled-p 'company)
  :init
  (my/add-mode-hooks '(("elisp" pkg/company/elisp-mode-hook)
                       ("c" pkg/company/c&c++-mode-hook)
                       ("c++" pkg/company/c&c++-mode-hook)
                       ("python" pkg/company/python-mode-hook)
                       ("org" pkg/company/org-mode-hook)))
  (my/prog/complete/add-hook #'global-company-mode)
  :config
  (setq company-clang-executable (my/locate-exec "clang")
        company-minimum-prefix-length 2
        company-idle-delay 0
        company-selection-wrap-around t
        company-frontends '(company-pseudo-tooltip-frontend)
        ;; 1. 从效率的角度而言，是没有必要为每个模式分别启用其独享的后端，因为筛选适用后端的过程非常效率
        ;; 2. make sure that 'company-backends is not changed in sub-package's loading
        ;;    I'd rather enable each backend in the corresponding mode hooks
        ;;    各种complete子插件都将通过后端的启用而被惰性加载
        company-backends `(;; company-semantic ;; 'semantic is too slow
                           ;; company-capf ;; use 'semantic by default
                           ;; company-css
                           (company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)
                           company-files
                           ;; company-nxml
                           ;; company-bbdb ;; Big Brother Database, an address book
                           ;; company-oddmuse
                           (,(pkg/company/backend-enabled-p 'yasnippet)
                            company-dabbrev)))
  (defun pkg/company/elisp-mode-hook ()
    (pkg/company/add-backends `((,(pkg/company/backend-enabled-p 'yasnippet)
                                 company-elisp))))
  (defun pkg/company/c&c++-mode-hook ()
    (pkg/company/add-backends `((,(pkg/company/backend-enabled-p 'yasnippet)
                                 ;; company-cmake ;; CMake
                                 ;; company-eclim ;; Eclipse
                                 ;; company-xcode ;; Xcode
                                 ,(when company-clang-executable
                                    (or (pkg/company/backend-enabled-p 'company-irony)
                                        (pkg/company/backend-enabled-p 'company-ycmd)
                                        'company-clang))))))
  (defun pkg/company/python-mode-hook ()
    (pkg/company/add-backends `((,(pkg/company/backend-enabled-p 'yasnippet)
                                 ,(pkg/company/backend-enabled-p 'company-anaconda)
                                 ,(pkg/company/backend-enabled-p 'company-jedi)))))
  (defun pkg/company/org-mode-hook ()
    (setq-local company-minimum-prefix-length 1))
  (use-package company-quickhelp
    :if (pkg/package/enabled-p 'company-quickhelp)
    :config
    (setq company-quickhelp-delay 0.5
          company-quickhelp-use-propertized-text t)
    (company-quickhelp-mode 1))
  (use-package company-box
    :defer t
    :preface
    (defun pkg/company-box/start ()
      (company-box-mode 1))
    :if (pkg/package/enabled-p 'company-box)
    :init
    (my/add-mode-hook "company" #'pkg/company-box/start)
    :config
    (setq company-box-enable-icon nil
          company-box-color-icon t
          company-box-show-single-candidate nil
          company-box-doc-enable t
          company-box-doc-delay 1.0))
  (use-package helm-company
    :defer t
    :if (pkg/package/enabled-p 'helm-company)
    :config
    (setq helm-company-candidate-number-limit 300
          helm-company-initialize-pattern-with-prefix t
          helm-company-fuzzy-match t
          helm-company-show-annotations t)))

(use-package auto-complete
  :diminish auto-complete-mode
  :defer t
  :preface
  (defun pkg/auto-complete/add-sources (sources)
    (my/prog/complete/add-backends 'ac-sources sources))
  :if (pkg/package/enabled-p 'auto-complete)
  :init
  (my/add-mode-hooks '(("yas" pkg/auto-complete/yas-mode-hook)
                       ("lisp" pkg/auto-complete/lisp-mode-hook)
                       ("elisp" pkg/auto-complete/elisp-mode-hook)
                       ("c" pkg/auto-complete/c&c++-mode-hook)
                       ("c++" pkg/auto-complete/c&c++-mode-hook)))
  (my/prog/complete/add-hook #'global-auto-complete-mode)
  :config
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories
               (my/get-user-emacs-file "my.ac-dict/" t))
  (ac-set-trigger-key "<tab>") ;; ac会在输入trigger key后立即强制生效
  (setq ac-trigger-commands '(self-insert-command
                              backward-delete-char
                              backward-delete-char-untabify)
        ac-ignore-case 'smart
        ac-dwim t
        ac-fuzzy-enable t
        ac-candidate-menu-height 8
        ;; [performance]
        ac-auto-start 3 ;; ac会在输入指定个数的字符后自动生效
        ac-delay 0.5
        ac-auto-show-menu nil ;; 不会自动显示候选词菜单
        ac-use-comphist t
        ac-candidate-limit 15 ;; 最大上限
        ;; [quick help]
        ac-use-quick-help t
        ac-quick-help-delay 1.0)
  (ac-linum-workaround) ;; 解决auto-complete与linum两个模式之间的冲突
  ;; [source]
  ;; auto-complete-config.el文件中定义了大量的扩展source
  ;; 从而使得auto-complete能与更多的插件相集成
  (setq ac-sources
        '(;; 以下分类反映的只是目前实际的使用情况，而非各自的局限范围：
          ac-source-filename
          ac-source-files-in-current-dir
          ;; ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ;; ac-source-words-in-all-buffer
          ;; ac-source-abbrev ;; Emacs abbreviation
          ;; ac-source-imenu ;; Emacs imenu
          ac-source-dictionary
          ;; 以下各源将在具体的编程模式启动时被添加
          ;; [java-mode]
          ;; ac-source-eclim
          ;; [python-mode]
          ;; ac-source-ropemacs
          ;; [other languages]
          ;; ac-source-ghc-mod ;; Haskell
          ;; ac-source-css-property ;; CSS
          ))
  (setq-default ac-sources ac-sources)
  ;; 只会在该列表中指定的模式下生效，无论是否全局性地启用
  ;; (setq ac-modes '())
  (defun pkg/auto-complete/yas-mode-hook ()
    (pkg/auto-complete/add-sources 'ac-source-yasnippet))
  (defun pkg/auto-complete/lisp-mode-hook ()
    (my/add-mode-hook "slime" ;; local minor mode
                      (lambda ()
                        (pkg/auto-complete/add-sources 'ac-source-slime))
                      :local))
  (defun pkg/auto-complete/elisp-mode-hook ()
    (pkg/auto-complete/add-sources '(ac-source-functions
                                     ac-source-variables
                                     ac-source-symbols
                                     ac-source-features)))
  (defun pkg/auto-complete/c&c++-mode-hook ()
    (when (my/minor-mode-on-p semantic-mode) ;; global minor mode
      (pkg/auto-complete/add-sources (if (cedet-gnu-global-version-check t)
                                         'ac-source-gtags 'ac-source-semantic)))))


(provide 'my/prog/complete)
