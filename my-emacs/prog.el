(require 'my-init)

(setq my-prog-mode-start-hook '())

;; =============================================================================
;; Yasnippet
;; 一个宏管理和应用的插件，允许用户自定义宏，并自动将其扩展
;; yas的脚本snippet以文件和目录的方式进行管理：每个文件中定义一个宏，每个目录对应于一个模式
;; 在yas模式下的文本中通过输入脚本文件名称以激活替换宏
;; 而脚本注释中的name属性只是作为替换成功后所呈现出的描述信息，或存在同名文件时的提示选择信息
;; -----------------------------------------------------------------------------
(defun my-plugin-yasnippet-init ()
  (when (and (member 'yasnippet package-selected-packages)
             (require 'yasnippet nil t))
    (add-to-list 'yas-snippet-dirs
                 (concat my-user-emacs-directory "snippets"))
    ;; 为配合auto-complete或company等插件的使用，需禁用以下自身的快捷键补全功能
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    ;; 设置解决同名snippet的方式
    (setq yas-prompt-functions
          (if (eq system-type 'windows-nt)
              '(yas-ido-prompt yas-dropdown-prompt) ;; Windows环境下推荐，其余支持不好
            '(yas-x-prompt yas-dropdown-prompt)))
    ;; (yas-global-mode 1)
    (add-hook 'my-prog-mode-start-hook 'my-plugin-yasnippet-start t)))

(defun my-plugin-yasnippet-start ()
  (yas-minor-mode 1) ;; 会自动执行(yas-reload-all)
  )

;; =============================================================================
;; Company (complete anything)
;; http://company-mode.github.io/
;; https://github.com/company-mode/company-mode
;; https://www.emacswiki.org/emacs/CompanyMode
;; 一个与auto-complete功能基本类似的补全插件，相比于后者，更新更为频繁
;; ----------------------------------------------------------------------------
(defun my-plugin-company-init ()
  (when (and (member 'company package-selected-packages)
             (require 'company nil t))
    ;; (customize-group 'company)
    ;; 常用的快捷键：
    ;; TAB用于补全候选项中的公共字段，RETURN用于补全所选项，C-g用于终止补全
    ;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    ;; 没有必要为每个模式分别启用其独享的后端，因为筛选适用后端的过程非常效率
    (setq company-backends `(company-elisp
                             ,(when (and (member 'company-jedi package-selected-packages)
                                         (require 'company-jedi nil t))
                                'company-jedi)
                             company-bbdb
                             company-nxml
                             ;; company-css ;; CSS
                             company-eclim ;; Eclipse
                             company-semantic ;; Semantic
                             company-clang ;; Clang
                             company-xcode ;; Xcode
                             company-cmake ;; CMake
                             company-capf ;; completion-at-point-functions
                             company-files ;;
                             (company-dabbrev-code company-gtags company-etags company-keywords)
                             company-oddmuse
                             company-dabbrev)
          company-minimum-prefix-length 1
          company-idle-delay 0)
    ;; (global-company-mode 1)
    (add-hook 'my-prog-mode-start-hook 'my-plugin-company-start t)))

(defun my-plugin-company-start ()
  (company-mode 1))

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
  (when (and (member 'auto-complete package-selected-packages)
             (require 'auto-complete-config nil t))
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
    (add-hook 'my-prog-mode-start-hook 'my-plugin-auto-complete-start t)))

(defun my-plugin-auto-complete-start ()
  (auto-complete-mode 1)
  ;; 各继承于prog-mode的编程模式在启动时都将重设其buffer-local的ac-sources
  ;; 方法是各自追加my-prog-ac-sources链表
  ;; 优点是当同一个buffer多次切换不同的编程模式时，不会彼此影响
  (setq my-prog-ac-sources
        (add-to-list ac-sources
                     '(ac-source-dictionary
                       ac-source-yasnippet)
                     t)))

;; =============================================================================
;; Flymake
;; Emacs内置，静态编译检查，效率低，准确度高，依赖于后台编译器的支持
;; -----------------------------------------------------------------------------
(defun my-plugin-flymake-init ()
  (when (and (member 'flymake package-selected-packages)
             (require 'flymake nil t))
    (add-hook 'my-prog-mode-start-hook 'my-plugin-flymake-start t)))

(defun my-plugin-flymake-start ()
  )

;; =============================================================================
;; Flycheck
;; http://www.flycheck.org/
;; 静态语义分析，效率高，准确度低，依赖于后台语法解析器(或编译器前端)的支持
;; 针对不同语言需安装各自相应的后台支持，具体可参见
;; http://www.flycheck.org/manual/latest/Supported-languages.html
;; -----------------------------------------------------------------------------
(defun my-plugin-flycheck-init ()
  (when (and (member 'flycheck package-selected-packages)
             (require 'flycheck nil t))
    ;; 可以通过以下方式定制每种模式，例如设置相应的checker(取自变量flycheck-checkers)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (setq flycheck-idle-change-delay 2.5
                      flycheck-emacs-lisp-load-path 'inherit))
              t)
    ;; (global-flycheck-mode 1)
    (add-hook 'my-prog-mode-start-hook 'my-plugin-flycheck-start t)))

(defun my-plugin-flycheck-start ()
  (flycheck-mode 1))

;; =============================================================================
;; Magit
;; https://magit.vc/
;; https://www.emacswiki.org/emacs/Magit
;; https://www.masteringemacs.org/article/introduction-magit-emacs-mode-git
;; -----------------------------------------------------------------------------
;; (setq magit-auto-revert-mode 0
;;      magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
;; (global-set-key (kbd "C-c g") 'magit-status)
(defun my-plugin-magit-init ()
  (when (and (member 'magit package-selected-packages)
             (require 'magit nil t))
    (add-hook 'my-prog-mode-start-hook 'my-plugin-magit-start)))

(defun my-plugin-magit-start ()
  )

;; =============================================================================
;; =============================================================================
(defun my-prog-mode-init ()
  (my-plugin-yasnippet-init)
  (my-plugin-company-init)
  (my-plugin-auto-complete-init)
  (my-plugin-flymake-init)
  (my-plugin-flycheck-init)
  (my-plugin-magit-init)
  (add-hook 'prog-mode-hook 'my-prog-mode-start t))

(defun my-prog-mode-start ()
  (turn-on-font-lock)
  (linum-mode 1)
  (run-hooks 'my-prog-mode-start-hook))

(add-hook 'after-init-hook 'my-prog-mode-init t)

(provide 'my-prog)
