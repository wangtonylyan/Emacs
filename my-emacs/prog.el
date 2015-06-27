(provide 'my-prog)
(require 'my-init)
;===========================================================================
; Yasnippet
;===========================================================================
; https://github.com/capitaomorte/yasnippet
; 一个类似于宏应用的插件，允许用户自定义宏，并自动将其扩展
; yas的脚本snippet以文件和目录的方式进行管理：每个文件中定义一个宏，每个目录对应于一个模式
; 在yas模式下的文本中通过输入脚本文件名称以激活替换宏
; 而脚本注释中的name属性只是作为替换成功后所呈现出的描述信息，或存在同名文件时的提示选择信息
;===========================================================================
(defun my-plugin-yasnippet-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "yasnippet"))
  (require 'yasnippet)
  (setq yas-snippet-dirs '()) ;删除默认值(可选)
  (add-to-list 'yas-snippet-dirs (concat my-emacs-config-file-path "snippets"))
  ;; 为配合auto-complete使用，需禁用以下自身的快捷键补全功能
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (setq yas-prompt-functions '(
;                               yas-x-prompt ;GTK环境下推荐
;                               yas-dropdown-prompt
;                               yas-completing-prompt
                               yas-ido-prompt ;Windows环境下推荐，其余支持不好
                               ))
;  (yas-global-mode 1) ;未全局性地启用
  ) ;end of my-plugin-yasnippet-init()

(defun my-plugin-yasnippet-start ()
  (yas-minor-mode 1) ;启用Yasnippet，会自动执行(yas-reload-all)
  ) ;end of my-plugin-yasnippet-start()

;===========================================================================
; Company (complete anything)
;===========================================================================
; http://company-mode.github.io/
; https://github.com/company-mode/company-mode
; 一个与auto-complete功能基本类似的补全插件，相比于后者，更新更为频繁，且支持ELPA安装
;===========================================================================
(defun my-plugin-company-init ()
  )
(defun my-plugin-company-start ()
  )

;===========================================================================
; Auto-Complete
;===========================================================================
; http://auto-complete.org/
; https://github.com/auto-complete
; 一个能够支持多种后台实现的补全界面，并自带了一些支持多种语言的补全字典
; 该插件的开发源代码在github上被拆分成了以下几个子部分，需要分别下载并安装：
; 1)auto-complete: 插件主体
; 2)popup-el: 提示弹出窗口
; 3)fuzzy-el: 输入匹配纠正
; 后两者只需将各自的单个.el文件拷贝进auto-complete的安装目录下即可完成安装
;===========================================================================
(defun my-plugin-auto-complete-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "auto-complete"))
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (concat my-emacs-config-file-path "ac-dicts"))
  (ac-set-trigger-key "TAB") ;ac会在输入trigger key后立即强制生效
  (setq ac-trigger-commands '(self-insert-command
                              backward-delete-char
                              backward-delete-char-untabify))
  (setq ac-ignore-case 'smart)
  (setq ac-dwim t) ; Do What I Mean
  (setq ac-fuzzy-enable t)
  (setq ac-candidate-menu-height 8)
  (ac-linum-workaround)
  ;; performance
  (setq ac-auto-start 2) ;ac会在输入指定个数的字符后自动生效
  (setq ac-delay 0.5)
  (setq ac-auto-show-menu nil) ;不会自动显示候选词菜单
  (setq ac-use-comphist t)
  (setq ac-candidate-limit 15) ;最大上限
  ;; quick help
  (setq ac-use-quick-help t)
  (setq ac-quick-help-delay 1.0)
  ;; source
  ;; auto-complete-config.el文件中定义了大量的扩展source
  ;; 从而使得auto-complete能与更多的插件相集成
  ;; 应定期更新之，或利用由网友提供的扩展配置，或自定义source
  (set-default 'ac-sources
               '(;; 以下分类反映的只是目前实际的使用情况，而非各自的局限范围：
                 ac-source-filename
                 ac-source-files-in-current-dir
;                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
;                 ac-source-words-in-all-buffer
;                 ac-source-abbrev ;Emacs abbreviation
;                 ac-source-imenu ;Emacs imenu
                 ;; 以下各源将在具体的编程模式启动时被添加
                 ;; prog-mode
;                 ac-source-dictionary
;                 ac-source-yasnippet
                 ;; lisp-mode
;                 ac-source-slime
                 ;; emacs-lisp-mode
;                 ac-source-functions
;                 ac-source-variables
;                 ac-source-symbols
;                 ac-source-features ;(require '
                 ;; c-mode, c++-mode
;                 ac-source-semantic
;                 ac-source-semantic-raw
;                 ac-source-gtags
                 ;; java-mode
;                 ac-source-eclim
                 ;; python-mode
;                 ac-source-ropemacs
                 ;; other languages
;                 ac-source-ghc-mod ;Haskell
;                 ac-source-css-property ;CSS
                 ))
  ; 只会在该列表中指定的模式下生效，无论是否全局性地启用
;  (setq ac-modes '())
;  (global-auto-complete-mode 1) ;未全局性地启用
  ) ;end of my-plugin-auto-complete-init()

(defun my-plugin-auto-complete-start ()
  (auto-complete-mode 1) ;启用Auto-Complete
  ) ;end of my-plugin-auto-complete-start()


;===========================================================================
; Flymake
;===========================================================================
; Emacs内置，动态编译检查，效率低，准确度高，依赖于后台编译器的支持
;===========================================================================

;===========================================================================
; Flycheck
;===========================================================================
; version: 0.24
; http://www.flycheck.org/
; https://github.com/flycheck/flycheck
; 静态语法检查，效率高，准确度低，依赖于后台语法解析器(或编译器前端)的支持
; 针对不同语言需安装各自相应的后台支持，具体可参见
; http://www.flycheck.org/manual/latest/Supported-languages.html
; 推荐使用ELPA安装，因为其还依赖于其他官网上没有提供的开发包
;===========================================================================
(defun my-plugin-flycheck-init ()
  ; 可以通过以下方式为每种模式设置相应的checker，取自变量flycheck-checkers
;  (add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-select-checker 'emacs-lisp))
;  (global-flycheck-mode 1)
  )
(defun my-plugin-flycheck-start ()
  (flycheck-mode 1) ;启用Flycheck
  )

;===========================================================================
;===========================================================================
(defun my-prog-mode-init ()
  (my-plugin-yasnippet-init)
  (my-plugin-auto-complete-init)
  (my-plugin-flycheck-init)
  )
(defun my-prog-mode-start ()
  (font-lock-mode 1) ;启用语法高亮
  (linum-mode 1) ;在buffer左边显示行号
  (when (fboundp 'yas-minor-mode)
    (my-plugin-yasnippet-start))
  (when (fboundp 'auto-complete-mode)
    (my-plugin-auto-complete-start)
    ;; 各继承于prog-mode的编程模式在启动时都将重设其buffer-local的ac-sources
    ;; 方法是各自追加my-prog-ac-sources链表
    ;; 优点是当同一个buffer多次切换不同的编程模式时，不会彼此影响
    (setq my-prog-ac-sources
          (append ac-sources '(ac-source-dictionary
                               ac-source-yasnippet))))
  (when (fboundp 'flycheck-mode)
    (my-plugin-flycheck-start))
  )
(eval-after-load 'simple ;/lisp/simple.el
  '(progn
     (my-prog-mode-init)
     (add-hook 'prog-mode-hook 'my-prog-mode-start)
     ))
