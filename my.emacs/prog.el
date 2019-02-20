;; -*- coding: utf-8 -*-

(defun my/prog/add-start-hook (func)
  (my/add-mode-hook "my/prog" func))

(defun my/prog/run-start-hook ()
  (my/run-mode-hook "my/prog"))

(defun my/prog/init ()
  (pkg/prog-mode/init)
  (pkg/yasnippet/init)
  (pkg/company/init)
  (pkg/auto-complete/init)
  (pkg/flymake/init)
  (pkg/flycheck/init)
  (pkg/ggtags/init)
  (pkg/helm-gtags/init)
  (my/add-mode-hook "prog" #'my/prog/run-start-hook))

(my/add-mode-hook "init" #'my/prog/init)


(defun pkg/prog-mode/init ()
  (use-package prog-mode
    :defer t
    :preface
    (defun pkg/prog-mode/start ()
      (linum-mode -1)
      (prettify-symbols-mode 1))
    :init
    (setq prettify-symbols-unprettify-at-point t)
    (my/prog/add-start-hook #'pkg/prog-mode/start)
    :config
    ;; 在mode-line显示当前光标所在的函数名
    (which-function-mode 1)
    ;; show "lambda" as "λ"
    (add-to-list 'prettify-symbols-alist '("lambda" . 955))))

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
    (my/prog/add-start-hook #'pkg/yasnippet/start)
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
    (my/prog/add-start-hook #'pkg/company/start)
    :config
    ;; 没有必要为每个模式分别启用其独享的后端，因为筛选适用后端的过程非常效率
    (setq company-clang-executable (my/locate-exec "clang")
          company-backends `(company-elisp
                             ,(when (my/package-enabled-p 'company-jedi)
                                'company-jedi)
                             ;; company-semantic ;; too slow
                             ,(when company-clang-executable
                                (if (my/package-enabled-p 'company-irony)
                                    'company-irony 'company-clang))
                             company-cmake
                             ;; company-eclim ;; Eclipse
                             ;; company-xcode
                             ;; company-css
                             (company-dabbrev-code
                              company-gtags
                              company-etags
                              company-keywords)
                             company-capf ;; completion-at-point-functions
                             company-files
                             ;; company-nxml
                             ;; company-bbdb ;; Big Brother Database, an address book
                             ;; company-oddmuse
                             company-dabbrev)
          company-minimum-prefix-length 3
          company-idle-delay 0)
    ;; 常用的快捷键：
    ;; <tab>用于补全候选项中的公共字段，<return>用于补全所选项，C-g用于终止补全
    ;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (bind-keys :map company-active-map
               ("M-n" . nil) ;; (company-select-next)
               ("M-p" . nil) ;; (company-select-previous)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               :map company-search-map
               ("M-n" . nil) ;; (company-select-next)
               ("M-p" . nil) ;; (company-select-previous)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("C-t" . company-search-toggle-filtering))))

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
    (my/prog/add-start-hook #'pkg/auto-complete/start)
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

(defun pkg/flymake/init ()
  (use-package flymake
    :defer t
    :preface
    (defun pkg/flymake/start ()
      (flymake-mode 1))
    :if (my/package-enabled-p 'flymake)
    :init
    (my/prog/add-start-hook #'pkg/flymake/start)))

(defun pkg/flycheck/init ()
  (use-package flycheck
    :diminish flycheck-mode
    :defer t
    :preface
    (defun pkg/flycheck/start ()
      (flycheck-mode 1))
    (defun pkg/flycheck/enable-checker ()
      (interactive)
      (let ((current-prefix-arg t))
        (call-interactively #'flycheck-disable-checker)))
    (defun pkg/flycheck/checker-enabled-p (chk)
      (and (memq chk flycheck-checkers) ;; global variable
           (not (memq chk flycheck-disabled-checkers)))) ;; buffer-local variable
    :if (my/package-enabled-p 'flycheck)
    :init
    (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
          flycheck-checker-error-threshold 500
          flycheck-idle-change-delay 2.5
          flycheck-indication-mode 'left-fringe)
    (my/prog/add-start-hook #'pkg/flycheck/start)
    :config
    (flycheck-error-list-set-filter 'error)
    ;; (flycheck-list-errors)可以列出当前buffer中的所有error，优化显示窗口
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (side . bottom)
                   (reusable-frames . visible)
                   (window-height . 0.33)))
    (defun pkg/flycheck/elisp-mode-hook ()
      (when (pkg/flycheck/checker-enabled-p 'emacs-lisp)
        (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
        (setq flycheck-emacs-lisp-load-path 'inherit)))
    (defun pkg/flycheck/c&c++-mode-hook ()
      (when (pkg/flycheck/checker-enabled-p 'c/c++-gcc)
        ;; (setq flycheck-gcc-language-standard "c++11") ;; 由cpputils-cmake插件设置
        ))
    (use-package flycheck-pyflakes
      :after (python-mode)
      :if (and (my/package-enabled-p 'flycheck-pyflakes)
               (my/locate-exec "pyflakes")))
    (defun pkg/flycheck/python-mode-hook ()
      (when (pkg/flycheck/checker-enabled-p 'flycheck-pyflakes)
        (add-to-list 'flycheck-disabled-checkers 'python-flake8)
        (add-to-list 'flycheck-disabled-checkers 'python-pylint))
      (when (pkg/flycheck/checker-enabled-p 'python-flake8)
        (add-to-list 'flycheck-flake8-error-level-alist '("^E305$" . info) t)))
    (use-package flycheck-haskell
      :after (haskell-mode)
      :if (my/package-enabled-p 'flycheck-haskell)
      :init
      (my/add-mode-hook "haskell" #'flycheck-haskell-setup))
    (defun pkg/flycheck/haskell-mode-hook ()
      (when (and (pkg/flycheck/checker-enabled-p 'haskell-hlint)
                 (my/locate-exec "hlint"))
        ;; 'flycheck-haskell-stack-ghc-executable
        ;; 'flycheck-haskell-ghc-executable
        ;; 'flycheck-haskell-hlint-executable
        (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
        (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)))
    (my/add-modes-hook '(("elisp" pkg/flycheck/elisp-mode-hook)
                         ("c" pkg/flycheck/c&c++-mode-hook)
                         ("c++" pkg/flycheck/c&c++-mode-hook)
                         ("python" pkg/flycheck/python-mode-hook)
                         ("haskell" pkg/flycheck/haskell-mode-hook)))
    (use-package helm-flycheck
      :after (helm)
      :commands (helm-flycheck)
      :if (and (my/package-enabled-p 'helm)
               (my/package-enabled-p 'helm-flycheck)))))

(defun pkg/ggtags/init ()
  (use-package ggtags
    :diminish ggtags-mode
    :defer t
    :preface
    (defun pkg/ggtags/start ()
      (ggtags-mode 1))
    :if (and (my/package-enabled-p 'ggtags)
             (my/locate-exec "gtags"))
    :init
    (setq ggtags-use-idutils t
          ggtags-oversize-limit (* 100 1024 1024)
          ggtags-mode-line-project-name nil
          ggtags-sort-by-nearness t
          ggtags-mode-prefix-key (kbd "C-c g"))
    (dolist (mode '("c" "c++"))
      (my/add-mode-hook mode #'pkg/ggtags/start))))

(defun pkg/helm-gtags/init ()
  (use-package helm-gtags
    :after (helm)
    :defer t
    :preface
    (defun pkg/helm-gtags/start ()
      (helm-gtags-mode 1))
    :if (and (my/package-enabled-p 'helm)
             (my/package-enabled-p 'helm-gtags)
             (my/locate-exec "gtags"))
    :init
    (setq helm-gtags-path-style 'root ;; 'relative, 'absolute
          helm-gtags-ignore-case t
          helm-gtags-read-only t
          helm-gtags-use-input-at-cursor t
          helm-gtags-highlight-candidate t
          helm-gtags-maximum-candidates 1000
          helm-gtags-display-style nil ;; 'detail
          helm-gtags-fuzzy-match nil
          helm-gtags-direct-helm-completing nil
          helm-gtags-auto-update t
          helm-gtags-update-interval-second 60
          helm-gtags-pulse-at-cursor t
          helm-gtags-cache-select-result t
          helm-gtags-cache-max-result-size (* 100 1024 1024)
          helm-gtags-preselect nil
          helm-gtags-prefix-key (kbd "C-c g")
          ;; 启用以下配置项会使得某些常用快捷键不再绑定于上述前缀中
          ;; 例如将(helm-gtags-dwim)绑定于"M-."
          helm-gtags-suggested-key-mapping nil)
    (dolist (mode '("c" "c++"))
      (my/add-mode-hook mode #'pkg/helm-gtags/start))))


(provide 'my/prog)
