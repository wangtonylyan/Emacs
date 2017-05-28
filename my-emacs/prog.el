(require 'my-init)

(defvar my-prog-mode-start-hook '())

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
    :bind (:map yas-minor-mode-map
                ;; 为配合auto-complete或company等插件的使用，需禁用以下自带的补全快捷键
                ("<tab>")
                ("TAB"))
    :init
    (add-hook 'my-prog-mode-start-hook 'my-plugin-yasnippet-start t)
    :config
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
    :bind (:map company-active-map
                ("C-n" . company-select-next)
                ("C-p" . company-select-previous)
                ("M-n")
                ("M-p")
                :map company-search-map
                ("C-n" . company-select-next)
                ("C-p" . company-select-previous)
                ("C-t" . company-search-toggle-filtering)
                ("M-n")
                ("M-p"))
    :init
    (add-hook 'my-prog-mode-start-hook 'my-plugin-company-start t)
    :config
    ;; (customize-group 'company)
    ;; 常用的快捷键：
    ;; TAB用于补全候选项中的公共字段，RETURN用于补全所选项，C-g用于终止补全
    ;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    ;; 没有必要为每个模式分别启用其独享的后端，因为筛选适用后端的过程非常效率
    (setq company-backends `(company-elisp
                             ,(when (and (my-func-package-enabled-p 'company-jedi)
                                         (require 'company-jedi nil t))
                                'company-jedi)
                             ;; company-bbdb ;; Big Brother Database, an address book
                             ;; company-nxml
                             company-semantic ;; Semantic
                             company-clang ;; Clang
                             ;; company-xcode ;; Xcode
                             company-cmake ;; CMake
                             ;; company-eclim ;; Eclipse
                             ;; company-css ;; CSS
                             company-capf ;; completion-at-point-functions
                             company-files ;;
                             (company-dabbrev-code company-gtags company-etags company-keywords)
                             company-oddmuse
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
;; 该插件的开发被拆分成了以下几个子组件，可能需要分别地独立安装：
;; 1) auto-complete: 插件主体
;; 2) popup-el: 提示弹出窗口
;; 3) fuzzy-el: 输入匹配纠正
;; -----------------------------------------------------------------------------
(defun my-plugin-auto-complete-init ()
  (use-package auto-complete
    :if (my-func-package-enabled-p 'auto-complete)
    :commands (global-auto-complete-mode auto-complete-mode)
    :init
    (add-hook 'my-prog-mode-start-hook 'my-plugin-auto-complete-start t)
    :config
    (require 'auto-complete-config)
    (add-to-list 'ac-dictionary-directories
                 (concat my-user-emacs-directory "ac-dicts"))
    (ac-set-trigger-key "TAB") ;; ac会在输入trigger key后立即强制生效
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
    (set-default 'ac-sources
                 '(;; 以下分类反映的只是目前实际的使用情况，而非各自的局限范围：
                   ac-source-filename
                   ac-source-files-in-current-dir
                   ;; ac-source-words-in-buffer
                   ac-source-words-in-same-mode-buffers
                   ;; ac-source-words-in-all-buffer
                   ;; ac-source-abbrev ;; Emacs abbreviation
                   ;; ac-source-imenu ;; Emacs imenu
                   ;; 以下各源将在具体的编程模式启动时被添加
                   ;; 1) prog-mode
                   ;; ac-source-dictionary
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
    ;; 只会在该列表中指定的模式下生效，无论是否全局性地启用
    ;; (setq ac-modes '())
    ;; (global-auto-complete-mode 1)
    ))

(defun my-plugin-auto-complete-start ()
  (auto-complete-mode 1)
  ;; 各继承于prog-mode的编程模式在启动时都将重设其buffer-local的ac-sources
  ;; 方法是各自追加my-prog-ac-sources链表
  ;; 优点是当同一个buffer多次切换不同的编程模式时，不会彼此影响
  (defvar my-prog-ac-sources
    (add-to-list ac-sources
                 '(ac-source-dictionary
                   ac-source-yasnippet)
                 t)))

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
    (add-hook 'my-prog-mode-start-hook 'my-plugin-flycheck-start t)
    :config
    (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
          flycheck-idle-change-delay 2.5
          flycheck-indication-mode nil)
    (setq-default flycheck-disabled-checkers
                  (add-to-list 'flycheck-disabled-checkers
                               'emacs-lisp-checkdoc t))

    (when (and (memq 'emacs-lisp flycheck-checkers)
               (not (memq 'emacs-lisp flycheck-disabled-checkers)))
      (add-hook 'emacs-lisp-mode-hook
                (lambda ()
                  (setq flycheck-emacs-lisp-load-path `(,my-user-emacs-directory)))
                t))
    (when (and (memq 'c/c++-gcc flycheck-checkers)
               (not (memq 'c/c++-gcc flycheck-disabled-checkers)))
      (add-hook 'c++-mode-hook
                (lambda ()
                  (setq flycheck-gcc-language-standard "c++11"))
                t))

    ;; (flycheck-list-errors)可以列出当前buffer中的所有error，优化显示窗口
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (side            . bottom)
                   (reusable-frames . visible)
                   (window-height   . 0.33)))
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
;; 插件helm-gtags在实现上并不依赖于插件ggtags，因此可完全代替之
(defun my-plugin-helm-gtags-init ()
  (with-eval-after-load 'helm
    (use-package helm-gtags
      :if (and (my-func-package-enabled-p 'helm-gtags)
               (executable-find "gtags"))
      :commands (helm-gtags-mode)
      :bind (:map helm-gtags-mode-map ;; 以下仅供参考
                  ("C-c g a" . helm-gtags-tags-in-this-function)
                  ("C-j" . helm-gtags-select)
                  ("M-." . helm-gtags-dwim)
                  ("M-," . helm-gtags-pop-stack)
                  ("C-c <" . helm-gtags-previous-history)
                  ("C-c >" . helm-gtags-next-history))
      :init
      (setq helm-gtags-path-style 'root
            helm-gtags-ignore-case t
            helm-gtags-read-only t
            helm-gtags-highlight-candidate t
            helm-gtags-display-style 'detail
            helm-gtags-fuzzy-match nil
            helm-gtags-direct-helm-completing nil
            helm-gtags-use-input-at-cursor t
            helm-gtags-pulse-at-cursor t
            helm-gtags-auto-update t
            helm-gtags-update-interval-second 60
            helm-gtags-prefix-key (kbd "C-c t")
            ;; 启用以下配置项会使得某些常用快捷键不再绑定于上述前缀中
            helm-gtags-suggested-key-mapping t)
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
                    (my-plugin-helm-gtags-start)))
                t)
      (add-hook 'dired-mode-hook 'my-plugin-helm-gtags-start t)
      (add-hook 'eshell-mode-hook 'my-plugin-helm-gtags-start t))))

(defun my-plugin-helm-gtags-start ()
  (helm-gtags-mode 1))

;; =============================================================================
;; =============================================================================
(defun my-prog-mode-init ()
  (my-plugin-yasnippet-init)
  (my-plugin-company-init)
  (my-plugin-auto-complete-init)
  (my-plugin-flymake-init)
  (my-plugin-flycheck-init)
  (my-plugin-helm-gtags-init)
  (add-hook 'prog-mode-hook 'my-prog-mode-start t))

(defun my-prog-mode-start ()
  (turn-on-font-lock)
  (linum-mode 1)
  (run-hooks 'my-prog-mode-start-hook))

(add-hook 'after-init-hook 'my-prog-mode-init t)

(provide 'my-prog)
