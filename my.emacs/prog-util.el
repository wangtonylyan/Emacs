;; -*- coding: utf-8 -*-

(defun my/prog-util/add-start-hook (func)
  (my/add-mode-hook "my/prog-util" func))

(defun my/prog-util/run-start-hook ()
  (my/run-mode-hook "my/prog-util"))

(defun my/prog-util/init ()
  (pkg/yasnippet/init)
  (pkg/company/init)
  (pkg/auto-complete/init)
  (my/add-mode-hook "prog" #'my/prog-util/run-start-hook))

(my/add-mode-hook "init" #'my/prog-util/init)


(defun pkg/yasnippet/init ()
  (use-package yasnippet
    :diminish yas-minor-mode
    :defer t
    :preface
    (defun pkg/yasnippet/start ()
      ;; 会自动执行(yas-reload-all)
      (yas-minor-mode 1))
    :if (my/package-enabled-p 'yasnippet)
    :init
    ;; 设置解决同名snippet的方式
    (setq yas-prompt-functions
          (if (eq system-type 'windows-nt)
              ;; Windows环境下推荐，其余支持不好
              '(yas-ido-prompt yas-dropdown-prompt)
            '(yas-x-prompt yas-dropdown-prompt)))
    (my/prog-util/add-start-hook #'pkg/yasnippet/start)
    :config
    (let ((dir (my/get-user-emacs-file "my.snippet/" t)))
      (when dir (add-to-list 'yas-snippet-dirs dir)))
    (when (or (my/package-enabled-p 'company)
              (my/package-enabled-p 'auto-complete))
      (unbind-key "<tab>" yas-minor-mode-map))))

(defun pkg/company/init ()
  (use-package company
    :diminish company-mode
    :defer t
    :preface
    (defun pkg/company/start ()
      (company-mode 1))
    :if (my/package-enabled-p 'company)
    :init
    (my/prog-util/add-start-hook #'pkg/company/start)
    :config
    ;; 没有必要为每个模式分别启用其独享的后端，因为筛选适用后端的过程非常效率
    (setq company-clang-executable (my/locate-exec "clang")
          company-minimum-prefix-length 3
          company-idle-delay 0
          company-selection-wrap-around t
          company-backends `(;; [Elisp]
                             company-elisp
                             ;; [C, C++]
                             ;; company-semantic ;; 'semantic is too slow
                             ,(when company-clang-executable
                                (if (and (my/package-enabled-p 'irony)
                                         (my/package-enabled-p 'company-irony))
                                    'company-irony 'company-clang))
                             ,(when (my/locate-exec "cmake")
                                'company-cmake)
                             ;; company-eclim ;; Eclipse
                             ;; company-xcode
                             ;; [Python]
                             ,(when (my/package-enabled-p 'company-jedi)
                                'company-jedi)
                             ;; [others]
                             ;; company-css
                             (company-dabbrev-code
                              company-gtags
                              company-etags
                              company-keywords)
                             ,(when (my/package-enabled-p 'irony)
                                ;; (completion-at-point-functions) use 'semantic by default
                                'company-capf)
                             company-files
                             ;; company-nxml
                             ;; company-bbdb ;; Big Brother Database, an address book
                             ;; company-oddmuse
                             company-dabbrev)
          company-backends (delq nil company-backends))
    (use-package company-quickhelp
      :if (my/package-enabled-p 'company-quickhelp)
      :init
      (setq company-quickhelp-delay 0.5
            company-quickhelp-use-propertized-text t)
      :config
      (company-quickhelp-mode 1))
    (use-package company-box
      :defer t
      :preface
      (defun pkg/company-box/start ()
        (company-box-mode 1))
      :if (my/package-enabled-p 'company-box)
      :init
      (setq company-box-enable-icon nil
            company-box-show-single-candidate nil
            company-box-doc-enable t
            company-box-doc-delay 0.5)
      (my/add-mode-hook "company" #'pkg/company-box/start))
    (use-package company-irony
      :defer t
      :preface
      (defun pkg/company-irony/setup ()
        (company-irony-setup-begin-commands))
      :if (and (my/package-enabled-p 'irony)
               (my/package-enabled-p 'company-irony))
      :init
      (setq company-irony-ignore-case nil)
      (my/add-mode-hook "irony" #'pkg/company-irony/setup))
    (use-package company-jedi
      :defer t
      :if (and (my/package-enabled-p 'company-jedi)))))

(defun pkg/auto-complete/init ()
  (use-package auto-complete
    :defer t
    :preface
    (defun pkg/auto-complete/start ()
      (auto-complete-mode 1))
    (defun pkg/auto-complete/add-source (srcs)
      (cond
       ((symbolp srcs)
        (setq-local ac-sources (add-to-list 'ac-sources srcs)))
       ((listp srcs)
        (dolist (src (nreverse srcs))
          (pkg/auto-complete/add-source src)))
       (t (user-error "*pkg/auto-complete/add-source* SRCS=%s" srcs))))
    :if (my/package-enabled-p 'auto-complete)
    :init
    (my/prog-util/add-start-hook #'pkg/auto-complete/start)
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
      (pkg/auto-complete/add-source 'ac-source-yasnippet))
    (defun pkg/auto-complete/lisp-mode-hook ()
      (my/add-mode-hook "slime" ;; local minor mode
                        (lambda ()
                          (pkg/auto-complete/add-source 'ac-source-slime)) t))
    (defun pkg/auto-complete/elisp-mode-hook ()
      (pkg/auto-complete/add-source '(ac-source-functions
                                      ac-source-variables
                                      ac-source-symbols
                                      ac-source-features)))
    (defun pkg/auto-complete/c&c++-mode-hook ()
      (when (my/minor-mode-on-p semantic-mode) ;; global minor mode
        (pkg/auto-complete/add-source (if (cedet-gnu-global-version-check t)
                                          'ac-source-gtags 'ac-source-semantic))))
    (my/add-modes-hook '(("yas" pkg/auto-complete/yas-mode-hook)
                         ("lisp" pkg/auto-complete/lisp-mode-hook)
                         ("elisp" pkg/auto-complete/elisp-mode-hook)
                         ("c" pkg/auto-complete/c&c++-mode-hook)
                         ("c++" pkg/auto-complete/c&c++-mode-hook)))))


(provide 'my/prog-util)
