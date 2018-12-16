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
    (if (and (my/package-enabled-p 'py-autopep8)
             (executable-find "autopep8")
             (fboundp 'py-autopep8-buffer))
        (py-autopep8-buffer)
      (message "autopep8 unsupported!")))
   ((derived-mode-p 'web-mode)
    (if (my/package-enabled-p 'web-beautify)
        (web-beautify-html)
      (message "html-beautify unsupported!")))
   (t (message "current major mode unsupported!"))))

;; =============================================================================
(defvar pkg/prog-mode/start-hook '())

(defun pkg/prog-mode/add-start-hook (func)
  (add-hook 'pkg/prog-mode/start-hook func t))

(defun pkg/prog-mode/init ()
  (use-package prog-mode
    :init
    (pkg/prog-mode/add-start-hook 'pkg/prog-mode/start)
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    :config
    ;; 在mode-line显示当前光标所在的函数名
    (which-function-mode 1)
    ;; lambda=λ
    (add-to-list 'prettify-symbols-alist '("lambda" . 955))))

(defun pkg/prog-mode/start ()
  )

;; =============================================================================
;; Yasnippet
;; 一个宏管理和应用的插件，允许用户自定义宏，并自动将其扩展
;; yas的脚本snippet以文件和目录的方式进行管理：每个文件中定义一个宏，每个目录对应于一个模式
;; 在yas模式下的文本中通过输入脚本文件名称以激活替换宏
;; 而脚本注释中的name属性只是作为替换成功后所呈现出的描述信息，或存在同名文件时的提示选择信息
;; -----------------------------------------------------------------------------
(defun pkg/yasnippet/init ()
  (use-package yasnippet
    :if (my/package-enabled-p 'yasnippet)
    :commands (yas-global-mode yas-minor-mode yas-minor-mode-on)
    :diminish yas-minor-mode
    :init
    (pkg/prog-mode/add-start-hook 'pkg/yasnippet/start)
    :config
    ;; 为配合auto-complete或company等插件的使用，需禁用以下自带的补全快捷键
    (unbind-key "<tab>" yas-minor-mode-map)
    (add-to-list 'yas-snippet-dirs (my/get-user-emacs-file "snippets" t))
    ;; 设置解决同名snippet的方式
    (setq yas-prompt-functions
          (if (eq system-type 'windows-nt)
              '(yas-ido-prompt yas-dropdown-prompt) ;; Windows环境下推荐，其余支持不好
            '(yas-x-prompt yas-dropdown-prompt)))
    ;; (yas-global-mode 1)
    ))

(defun pkg/yasnippet/start ()
  (yas-minor-mode-on) ;; 会自动执行(yas-reload-all)
  )

;; =============================================================================
;; Company (complete anything)
;; http://company-mode.github.io/
;; https://github.com/company-mode/company-mode
;; https://www.emacswiki.org/emacs/CompanyMode
;; 一个与auto-complete功能基本类似的补全插件，相比于后者，更新更为频繁
;; ----------------------------------------------------------------------------
(defun pkg/company/init ()
  (use-package company
    :if (my/package-enabled-p 'company)
    :commands (global-company-mode company-mode company-mode-on)
    :diminish company-mode
    :init
    (pkg/prog-mode/add-start-hook 'pkg/company/start)
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
                             ,(when (and (my/package-enabled-p 'company-jedi)
                                         (require 'company-jedi nil t))
                                'company-jedi)
                             (company-semantic ;; Semantic
                              ;; company-clang ;; Clang
                              company-gtags
                              company-etags)
                             company-cmake ;; CMake
                             ;; company-eclim ;; Eclipse
                             ;; company-xcode ;; Xcode
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

(defun pkg/company/start ()
  (company-mode-on))

;; =============================================================================
;; Auto-Complete
;; http://auto-complete.org/
;; https://github.com/auto-complete
;; 一个能够支持多种后台实现的补全界面，并自带了一些支持多种语言的补全字典
;; -----------------------------------------------------------------------------
(defun pkg/auto-complete/init ()
  (use-package auto-complete
    :if (my/package-enabled-p 'auto-complete)
    :commands (global-auto-complete-mode auto-complete-mode)
    :init
    (pkg/prog-mode/add-start-hook 'pkg/auto-complete/start)
    :config
    (ac-config-default)
    (add-to-list 'ac-dictionary-directories
                 (concat my/user-emacs-directory "ac-dicts"))
    (ac-set-trigger-key "<tab>") ;; ac会在输入trigger key后立即强制生效
    (setq ac-trigger-commands '(self-insert-command
                                backward-delete-char
                                backward-delete-char-untabify)
          ac-ignore-case 'smart
          ac-dwim t
          ac-fuzzy-enable t
          ac-candidate-menu-height 8
          ;; [performance]
          ac-auto-start 2 ;; ac会在输入指定个数的字符后自动生效
          ac-delay 0.5
          ac-auto-show-menu nil ;; 不会自动显示候选词菜单
          ac-use-comphist t
          ac-candidate-limit 15 ;; 最大上限
          ;; [quick help]
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

(defun pkg/auto-complete/start ()
  (auto-complete-mode 1)
  ;; 各继承于prog-mode的编程模式在启动时都将设置其buffer-local的'ac-sources
  (set (make-local-variable 'ac-sources)
       (add-to-list 'ac-sources 'ac-source-yasnippet t)))

;; =============================================================================
;; Flymake
;; Emacs内置，静态编译检查，效率低，准确度高，依赖于后台编译器的支持
;; -----------------------------------------------------------------------------
(defun pkg/flymake/init ()
  (use-package flymake
    :if (my/package-enabled-p 'flymake)
    :commands (flymake-mode)
    :init
    (pkg/prog-mode/add-start-hook 'pkg/flymake/start)))

(defun pkg/flymake/start ()
  (flymake-mode t))

;; =============================================================================
;; Flycheck
;; http://www.flycheck.org/
;; 静态语义分析，效率高，准确度低，依赖于后台语法解析器(或编译器前端)的支持
;; 针对不同语言需安装各自相应的后台支持，具体可参见
;; http://www.flycheck.org/manual/latest/Supported-languages.html
;; -----------------------------------------------------------------------------
;; 快捷键前缀：C-c !
;; l :: (flycheck-list-errors)
;; C-c :: (flycheck-compile)
;; RET :: Go to the current error in the source buffer
;; n :: Jump to the next error
;; p :: Jump to the previous error
;; e :: Explain the error
;; f :: Filter the error list by level
;; F :: Remove the filter
;; S :: Sort the error list by the column at point
;; g :: Check the source buffer and update the error list
;; q :: Quit the error list and hide its window
(defun pkg/flycheck/init ()
  (use-package flycheck
    :preface
    (defun pkg/flycheck/checker-enabled-p (chk)
      (and (memq chk flycheck-checkers) ;; global variable
           (not (memq chk flycheck-disabled-checkers)))) ;; buffer-local variable
    :if (my/package-enabled-p 'flycheck)
    :commands (global-flycheck-mode flycheck-mode flycheck-mode-on-safe)
    :init
    (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
          flycheck-checker-error-threshold 500
          flycheck-idle-change-delay 2.5
          flycheck-indication-mode 'left-fringe)
    (pkg/prog-mode/add-start-hook 'pkg/flycheck/start)
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
    (defun pkg/flycheck/elisp-mode-hook ()
      (when (pkg/flycheck/checker-enabled-p 'emacs-lisp)
        (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
        (setq flycheck-emacs-lisp-load-path 'inherit)))
    (my/add-language-mode-hook "elisp" 'pkg/flycheck/elisp-mode-hook)
    ;; C/C++
    (defun pkg/flycheck/c++-mode-hook ()
      (when (pkg/flycheck/checker-enabled-p 'c/c++-gcc)
        ;; (setq flycheck-gcc-language-standard "c++11") ;; 由cpputils-cmake插件设置
        ))
    (my/add-language-mode-hook "c++" 'pkg/flycheck/c++-mode-hook)
    ;; Python
    (use-package flycheck-pyflakes
      :if (my/package-enabled-p 'flycheck-pyflakes)
      :demand t)
    (defun pkg/flycheck/python-mode-hook ()
      (when (my/package-enabled-p 'flycheck-pyflakes)
        (add-to-list 'flycheck-disabled-checkers 'python-flake8)
        (add-to-list 'flycheck-disabled-checkers 'python-pylint))
      (when (pkg/flycheck/checker-enabled-p 'python-flake8)
        (add-to-list 'flycheck-flake8-error-level-alist '("^E305$" . info) t)))
    (my/add-language-mode-hook "python" 'pkg/flycheck/python-mode-hook)
    ;; Haskell
    (use-package flycheck-haskell
      :if (my/package-enabled-p 'flycheck-haskell)
      :init
      (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))
    (defun pkg/flycheck/haskell-mode-hook ()
      (when (and (pkg/flycheck/checker-enabled-p 'haskell-hlint)
                 (my/find-executable "hlint"))
        ;; 'flycheck-haskell-stack-ghc-executable
        ;; 'flycheck-haskell-ghc-executable
        ;; 'flycheck-haskell-hlint-executable
        (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
        (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)))
    (my/add-language-mode-hook "haskell" 'pkg/flycheck/haskell-mode-hook)
    (use-package helm-flycheck
      :if (my/package-enabled-p 'helm-flycheck)
      :after helm
      :config
      (bind-key "C-c ! l" 'helm-flycheck flycheck-mode-map))
    ;; (global-flycheck-mode 1)
    ))

(defun pkg/flycheck/start ()
  (flycheck-mode-on-safe))

;; =============================================================================
;; 常用的tagging system主要有两个，即GNU Global (gtags)和Ctags
;; 而ctags又分为了两个版本，Exuberant Ctags和Universal Ctags
;; ctags相比于gtags支持更多的语言，而gtags本身也支持以ctags作为解析后端
;; 此外，gtags还支持由Python实现的pygments作为解析后端
;; $ gtags --gtagslabel=ctags     # exuberant ctags
;; $ gtags --gtagslabel=new-ctags # universal ctags
;; $ gtags --gtagslabel=pygments  # pygments
;; Emacs中有两个独立支持gtags的前端插件，即ggtags和helm-gtags
;; 此外，Emacs中还自带了etags，提供了类似的功能
(defun pkg/gtags/init ()
  (defun pkg/gtags/add-hook (func)
    ;; gtags暂仅用于C、C++
    (my/add-language-mode-hook "c" func)
    (my/add-language-mode-hook "c++" func))
  (use-package ggtags
    :if (and (my/package-enabled-p 'ggtags)
             (executable-find "gtags"))
    :init
    (pkg/gtags/add-hook 'pkg/gtags/start))
  (with-eval-after-load 'helm
    (use-package helm-gtags
      :if (and (my/package-enabled-p 'helm-gtags)
               (executable-find "gtags"))
      :commands (helm-gtags-mode)
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
            helm-gtags-cache-max-result-size 10485760 ;; 10Mb
            helm-gtags-preselect nil
            helm-gtags-prefix-key (kbd "C-c c")
            ;; 启用以下配置项会使得某些常用快捷键不再绑定于上述前缀中
            ;; 例如将(helm-gtags-dwim)绑定于"M-."
            helm-gtags-suggested-key-mapping nil)
      (pkg/gtags/add-hook 'pkg/gtags/start)
      :config
      ;; 在以下快捷键前输入"C-u"，还可以限定搜索的目录路径
      (bind-keys :map helm-gtags-mode-map
                 ("M-." . helm-gtags-dwim) ;; 替代(xref-find-definitions)
                 ("M-/" . helm-gtags-show-stack) ;; 所有跳转位置都会被记录于一个栈中，打印整个栈，以供选择
                 ("M-," . helm-gtags-pop-stack) ;; 替代(xref-pop-marker-stack)，删除栈顶记录
                 ("" . helm-gtags-previous-history) ;; 遍历栈
                 ("" . helm-gtags-next-history)
                 ;; 以下(helm-gtags-find-*)中的多数可由(helm-gtags-dwim)替代
                 ("C-c c t" . helm-gtags-find-tag)     ;; jump to definitions
                 ("C-c c r" . helm-gtags-find-rtag)    ;; jump to references
                 ("C-c c s" . helm-gtags-find-symbol)  ;; jump to symbols
                 ("C-c c p" . helm-gtags-find-pattern) ;; jump to patterns
                 ("" . helm-gtags-find-files)          ;; jump to files
                 ("C-c c l" . helm-gtags-select) ;; 列出所有已被gtags识别出的符号，以供选择
                 ("C-c c a" . helm-gtags-tags-in-this-function) ;; 列出当前函数中已被识别的符号
                 ("" . helm-gtags-select-path) ;; 类似于打开文件的功能
                 ("C-c c n" . helm-gtags-create-tags)
                 ("C-c c u" . helm-gtags-update-tags)))))

(defun pkg/gtags/start ()
  (when (my/package-enabled-p 'ggtags))
  (when (my/package-enabled-p 'helm-gtags)
    (helm-gtags-mode 1)))

;; =============================================================================
;; cmake-mode, cmake-font-lock, cmake-ide, cmake-project
(defun pkg/cmake/init ()
  ;; 项目目录示例
  ;; project root folder
  ;; |-- CMakeLists.txt
  ;; |-- src :: 头文件和源文件
  ;; |-- project1 :: 一个子项目或模块
  ;; |---- CMakeLists.txt
  ;; |---- src
  ;; |-- project2
  ;; |---- CMakeLists.txt
  ;; |---- src
  (use-package cmake-mode
    :if (my/package-enabled-p 'cmake-mode)
    :init
    (pkg/prog-mode/add-start-hook 'pkg/cmake/start)
    :config
    (use-package cmake-font-lock
      :if (my/package-enabled-p 'cmake-font-lock)
      :init
      ;; (cmake-font-lock-activate)在实现上会覆盖原本font-lock-mode的效果
      ;; 因此其必须在后者生效之后执行，于是暂采用以下手段
      ;; 但其要求(global-font-lock-mode)在实现上要与(turn-on-font-lock)类似
      ;; 即保证在font-lock-mode启动后执行时，不会再次重启
      (add-hook 'cmake-mode-hook 'turn-on-font-lock nil)
      (add-hook 'cmake-mode-hook 'cmake-font-lock-activate t))
    )
  (use-package cmake-ide
    )
  (use-package cmake-project
    )
  )

(defun pkg/cmake/start ()
  )

;; =============================================================================
;; =============================================================================
(defun my-prog/init ()
  (pkg/prog-mode/init)
  (pkg/yasnippet/init)
  (pkg/company/init)
  (pkg/auto-complete/init)
  (pkg/flymake/init)
  (pkg/flycheck/init)
  (pkg/gtags/init)
  (pkg/cmake/init)
  (add-hook 'prog-mode-hook 'my-prog/start t))

(defun my-prog/start ()
  (linum-mode 1)
  (run-hooks 'pkg/prog-mode/start-hook))

(add-hook 'after-init-hook 'my-prog/init t)

(provide 'my-prog)
