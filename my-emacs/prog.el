;; -*- coding: utf-8 -*-

(require 'my-init)

(defun my-func-prog-mode-beautify ()
  (interactive)
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    (let ((exe "uncrustify")
          (cfg "~/.uncrustify/alps.cfg"))
      (if (and (executable-find exe)
               (file-exists-p cfg))
          (message (shell-command-to-string
                    (concat exe " -l C -c " cfg " --no-backup " buffer-file-name)))
        (message "uncrustify unsupported!"))))
   ((eq major-mode 'python-mode)
    (if (and (my-func-package-enabled-p 'py-autopep8)
             (executable-find "autopep8")
             (fboundp 'py-autopep8-buffer))
        (py-autopep8-buffer)
      (message "autopep8 unsupported!")))
   ((derived-mode-p 'web-mode)
    (if (my-func-package-enabled-p 'web-beautify)
        (web-beautify-html)
      (message "html-beautify unsupported!")))
   (t (message "current major mode unsupported!"))))

(defvar my-prog-mode-start-hook '())

(defun my-plugin-prog-mode-init ()
  (use-package prog-mode
    :init
    (add-hook 'my-prog-mode-start-hook 'my-plugin-prog-mode-start t)
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    :config
    ;; lambda=λ
    (add-to-list 'prettify-symbols-alist '("lambda" . 955))))

(defun my-plugin-prog-mode-start ()
  )

;; =============================================================================
;; Yasnippet
;; 一个宏管理和应用的插件，允许用户自定义宏，并自动将其扩展
;; yas的脚本snippet以文件和目录的方式进行管理：每个文件中定义一个宏，每个目录对应于一个模式
;; 在yas模式下的文本中通过输入脚本文件名称以激活替换宏
;; 而脚本注释中的name属性只是作为替换成功后所呈现出的描述信息，或存在同名文件时的提示选择信息
;; -----------------------------------------------------------------------------
(defun my-plugin-yasnippet-init ()
  (use-package yasnippet
    :if (my-func-package-enabled-p 'yasnippet)
    :commands (yas-global-mode yas-minor-mode yas-minor-mode-on)
    :diminish yas-minor-mode
    :init
    (add-hook 'my-prog-mode-start-hook 'my-plugin-yasnippet-start t)
    :config
    ;; 为配合auto-complete或company等插件的使用，需禁用以下自带的补全快捷键
    (unbind-key "<tab>" yas-minor-mode-map)
    (add-to-list 'yas-snippet-dirs
                 (concat my-user-emacs-directory "snippets"))
    ;; 设置解决同名snippet的方式
    (setq yas-prompt-functions
          (if (eq system-type 'windows-nt)
              '(yas-ido-prompt yas-dropdown-prompt) ;; Windows环境下推荐，其余支持不好
            '(yas-x-prompt yas-dropdown-prompt)))
    ;; (yas-global-mode 1)
    ))

(defun my-plugin-yasnippet-start ()
  (yas-minor-mode-on) ;; 会自动执行(yas-reload-all)
  )

;; =============================================================================
;; Company (complete anything)
;; http://company-mode.github.io/
;; https://github.com/company-mode/company-mode
;; https://www.emacswiki.org/emacs/CompanyMode
;; 一个与auto-complete功能基本类似的补全插件，相比于后者，更新更为频繁
;; ----------------------------------------------------------------------------
(defun my-plugin-company-init ()
  (use-package company
    :if (my-func-package-enabled-p 'company)
    :commands (global-company-mode company-mode company-mode-on)
    :diminish company-mode
    :init
    (add-hook 'my-prog-mode-start-hook 'my-plugin-company-start t)
    :config
    ;; 常用的快捷键：
    ;; <tab>用于补全候选项中的公共字段，<return>用于补全所选项，C-g用于终止补全
    ;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (unbind-key "M-n" company-active-map)
    (unbind-key "M-p" company-active-map)
    (unbind-key "M-n" company-search-map)
    (unbind-key "M-p" company-search-map)
    (bind-keys :map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               :map company-search-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("C-t" . company-search-toggle-filtering))
    ;; 没有必要为每个模式分别启用其独享的后端，因为筛选适用后端的过程非常效率
    (setq company-backends `(company-elisp
                             ,(when (and (my-func-package-enabled-p 'company-jedi)
                                         (require 'company-jedi nil t))
                                'company-jedi)
                             (company-semantic ;; Semantic
                              ;; company-clang ;; Clang
                              company-gtags
                              company-etags)
                             ;; company-eclim ;; Eclipse
                             ;; company-xcode ;; Xcode
                             company-cmake ;; CMake
                             ;; company-css ;; CSS
                             (company-dabbrev-code company-keywords)
                             company-files
                             company-capf ;; completion-at-point-functions
                             ;; company-nxml
                             ;; company-bbdb ;; Big Brother Database, an address book
                             ;; company-oddmuse
                             company-dabbrev)
          company-minimum-prefix-length 1
          company-idle-delay 0)
    ;; (global-company-mode 1)
    ))

(defun my-plugin-company-start ()
  (company-mode-on))

;; =============================================================================
;; Auto-Complete
;; http://auto-complete.org/
;; https://github.com/auto-complete
;; 一个能够支持多种后台实现的补全界面，并自带了一些支持多种语言的补全字典
;; -----------------------------------------------------------------------------
(defun my-plugin-auto-complete-init ()
  (use-package auto-complete
    :if (my-func-package-enabled-p 'auto-complete)
    :commands (global-auto-complete-mode auto-complete-mode)
    :init
    (add-hook 'my-prog-mode-start-hook 'my-plugin-auto-complete-start t)
    :config
    (ac-config-default)
    (add-to-list 'ac-dictionary-directories
                 (concat my-user-emacs-directory "ac-dicts"))
    (ac-set-trigger-key "<tab>") ;; ac会在输入trigger key后立即强制生效
    (setq ac-trigger-commands '(self-insert-command
                                backward-delete-char
                                backward-delete-char-untabify)
          ac-ignore-case 'smart
          ac-dwim t ;; Do What I Mean
          ac-fuzzy-enable t
          ac-candidate-menu-height 8
          ;; performance
          ac-auto-start 2 ;; ac会在输入指定个数的字符后自动生效
          ac-delay 0.5
          ac-auto-show-menu nil ;; 不会自动显示候选词菜单
          ac-use-comphist t
          ac-candidate-limit 15 ;; 最大上限
          ;; quick help
          ac-use-quick-help t
          ac-quick-help-delay 1.0)
    (ac-linum-workaround) ;; 解决auto-complete与linum两个模式之间的冲突
    ;; source
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
            ;; 1) prog-mode
            ;; ac-source-yasnippet
            ;; 2) lisp-mode
            ;; ac-source-slime
            ;; 3) emacs-lisp-mode
            ;; ac-source-functions
            ;; ac-source-variables
            ;; ac-source-symbols
            ;; ac-source-features ;; (require)
            ;; 4) c-mode, c++-mode
            ;; ac-source-semantic
            ;; ac-source-semantic-raw
            ;; ac-source-gtags
            ;; 5) java-mode
            ;; ac-source-eclim
            ;; 6) python-mode
            ;; ac-source-ropemacs
            ;; 7) other languages
            ;; ac-source-ghc-mod ;; Haskell
            ;; ac-source-css-property ;; CSS
            ))
    (setq-default ac-sources ac-sources)
    ;; 只会在该列表中指定的模式下生效，无论是否全局性地启用
    ;; (setq ac-modes '())
    ;; (global-auto-complete-mode 1)
    ))

(defun my-plugin-auto-complete-start ()
  (auto-complete-mode 1)
  ;; 各继承于prog-mode的编程模式在启动时都将设置其buffer-local的'ac-sources
  (set (make-local-variable 'ac-sources)
       (add-to-list 'ac-sources 'ac-source-yasnippet t)))

;; =============================================================================
;; Flymake
;; Emacs内置，静态编译检查，效率低，准确度高，依赖于后台编译器的支持
;; -----------------------------------------------------------------------------
(defun my-plugin-flymake-init ()
  (use-package flymake
    :if (my-func-package-enabled-p 'flymake)
    :commands (flymake-mode flymake-mode-on)
    :init
    (add-hook 'my-prog-mode-start-hook 'my-plugin-flymake-start t)))

(defun my-plugin-flymake-start ()
  (flymake-mode-on))

;; =============================================================================
;; Flycheck
;; http://www.flycheck.org/
;; 静态语义分析，效率高，准确度低，依赖于后台语法解析器(或编译器前端)的支持
;; 针对不同语言需安装各自相应的后台支持，具体可参见
;; http://www.flycheck.org/manual/latest/Supported-languages.html
;; -----------------------------------------------------------------------------
;; 快捷键前缀：C-c !
;; C-c ! l :: (flycheck-list-errors)
;; RET :: Go to the current error in the source buffer
;; n :: Jump to the next error
;; p :: Jump to the previous error
;; e :: Explain the error
;; f :: Filter the error list by level
;; F :: Remove the filter
;; S :: Sort the error list by the column at point
;; g :: Check the source buffer and update the error list
;; q :: Quit the error list and hide its window
(defun my-plugin-flycheck-init ()
  (use-package flycheck
    :if (my-func-package-enabled-p 'flycheck)
    :commands (global-flycheck-mode flycheck-mode flycheck-mode-on-safe)
    :init
    (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
          flycheck-checker-error-threshold 200
          flycheck-idle-change-delay 2.5
          flycheck-indication-mode 'left-fringe)
    (add-hook 'my-prog-mode-start-hook 'my-plugin-flycheck-start t)
    :config
    (flycheck-error-list-set-filter 'error)
    ;; (flycheck-list-errors)可以列出当前buffer中的所有error，优化显示窗口
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (side            . bottom)
                   (reusable-frames . visible)
                   (window-height   . 0.33)))
    ;; Lisp
    (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
    (when (and (memq 'emacs-lisp flycheck-checkers)
               (not (memq 'emacs-lisp flycheck-disabled-checkers)))
      (add-hook 'emacs-lisp-mode-hook
                (lambda ()
                  (setq flycheck-emacs-lisp-load-path `(,my-user-emacs-directory)))
                t))
    ;; C/C++
    (when (and (memq 'c/c++-gcc flycheck-checkers)
               (not (memq 'c/c++-gcc flycheck-disabled-checkers)))
      (add-hook 'c++-mode-hook
                (lambda ()
                  (setq flycheck-gcc-language-standard "c++11"))
                t))
    ;; Python
    (use-package flycheck-pyflakes
      :if (my-func-package-enabled-p 'flycheck-pyflakes)
      :config
      (add-to-list 'flycheck-disabled-checkers 'python-flake8)
      (add-to-list 'flycheck-disabled-checkers 'python-pylint))
    (when (and (memq 'python-flake8 flycheck-checkers)
               (not (memq 'python-flake8 flycheck-disabled-checkers)))
      (add-to-list 'flycheck-flake8-error-level-alist '("^E305$" . info) t))
    ;; Haskell
    (use-package flycheck-haskell
      :if (my-func-package-enabled-p 'flycheck-haskell)
      :init
      (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))
    (when (and (memq 'haskell-hlint flycheck-checkers)
               (not (memq 'haskell-hlint flycheck-disabled-checkers))
               (my-func-executable-find "hlint"))
      ;; 'flycheck-haskell-stack-ghc-executable
      ;; 'flycheck-haskell-ghc-executable
      ;; 'flycheck-haskell-hlint-executable
      (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
      (add-to-list 'flycheck-disabled-checkers 'haskell-ghc))
    (setq-default flycheck-disabled-checkers flycheck-disabled-checkers)
    ;; (global-flycheck-mode 1)
    ;; 此外，若是使用插件helm-flycheck，则可以基于helm模式来呈现信息
    (use-package helm-flycheck
      :if (my-func-package-enabled-p 'helm-flycheck)
      :after helm
      :config
      (bind-key "C-c ! l" 'helm-flycheck flycheck-mode-map))))

(defun my-plugin-flycheck-start ()
  (flycheck-mode-on-safe))

;; =============================================================================
;; =============================================================================
(defun my-prog-mode-init ()
  (my-plugin-prog-mode-init)
  (my-plugin-yasnippet-init)
  (my-plugin-company-init)
  (my-plugin-auto-complete-init)
  (my-plugin-flymake-init)
  (my-plugin-flycheck-init)
  (add-hook 'prog-mode-hook 'my-prog-mode-start t))

(defun my-prog-mode-start ()
  (linum-mode 1)
  (run-hooks 'my-prog-mode-start-hook))

(add-hook 'after-init-hook 'my-prog-mode-init t)

(provide 'my-prog)
