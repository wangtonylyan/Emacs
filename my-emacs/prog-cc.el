;; -*- coding: utf-8 -*-

(defun my/prog-cc/add-start-hook (func)
  (my/add-mode-hook "my/prog-cc" func))

(defun my/prog-cc/run-start-hook ()
  (my/run-mode-hook "my/prog-cc"))


(defun pkg/cc-mode/init ()
  (use-package cc-mode
    :init
    (my/prog-cc/add-start-hook #'pkg/cc-mode/start)
    :config
    (c-add-style "my/cc-style"
                 '("stroustrup"
                   (c-basic-offset . 4)
                   (c-offsets-alist . ((substatement-open . 0)))
                   ;; (c-indent-comment-alist . )
                   ;; (c-indent-comments-syntactically-p . )
                   ;; (c-comment-only-line-offset . )
                   ;; (c-special-indent-hook . ())
                   ;; (c-label-minimum-indentation . )
                   (c-tab-always-indent . t)
                   (c-echo-syntactic-information-p . nil)
                   (c-report-syntactic-errors . t)
                   (c-comment-prefix-regexp . ((c-mode . "//+\\|\\**")
                                               (c++-mode . "//+\\|\\**")))
                   (c-block-comment-prefix . "*") ;; 设置block comment在换行时自动添加的前缀
                   (c-ignore-auto-fill . (string cpp code)) ;; 设置在哪些地方禁用auto-fill功能，目前仅在注释中启用
                   ;; 设置大括号的前后是否应换行，即"{"将位于行头或行尾或单行或任意位置
                   ;; 不在以下alist中列出的syntactic symbol，将执行默认行为：(before after)
                   (c-hanging-braces-alist . ((defun-open)
                                              (defun-close)
                                              (class-open)
                                              (class-close)
                                              (inline-open)
                                              (inline-close)
                                              (block-open)
                                              (block-close           . c-snug-do-while)
                                              (statement-cont)
                                              (substatement-open     . after)
                                              (statement-case-open   . (before after))
                                              (brace-list-open)
                                              (brace-list-close)
                                              (brace-list-intro)
                                              (brace-entry-open)
                                              (extern-lang-open      . after)
                                              (extern-lang-close     . before)
                                              (namespace-open        . after)
                                              (namespace-close       . before)
                                              (module-open           . after)
                                              (module-close          . before)
                                              (composition-open      . after)
                                              (composition-close     . before)
                                              (inexpr-class-open     . after)
                                              (inexpr-class-close    . before)
                                              (arglist-cont-nonempty)))
                   ;; 设置冒号的前后是否应换行
                   ;; 不在以下alist中列出的syntactic symbol，将执行默认行为：()
                   (c-hanging-colons-alist . ((case-label)
                                              (label        . after)
                                              (access-label . after)))
                   ;; 设置分号和逗号的前后是否应换行
                   (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-before-nonblanks
                                                     c-semi&comma-inside-parenlist
                                                     c-semi&comma-no-newlines-for-oneline-inliners))
                   ;; 清理whitespace，并作为上述hanging的补充(在其后生效)
                   (c-cleanup-list . (brace-else-brace
                                      brace-elseif-brace
                                      brace-catch-brace
                                      ;; empty-defun-braces
                                      defun-close-semi
                                      list-close-comma
                                      scope-operator
                                      comment-close-slash))
                   (c-backslash-column . 48)
                   (c-backslash-max-column . 72)
                   (c-auto-align-backslashes . t)))
    (bind-keys :map c-mode-base-map
               ;; 换行后自动缩进
               ("<return>" . c-context-line-break))))

(defun pkg/cc-mode/start ()
  (c-set-style "my/cc-style") ;; 也可以通过(setq c-default-style)实现
  (c-toggle-syntactic-indentation 1) ;; 启用根据语法缩进，否则任何基于语法的style都将失效
  (c-toggle-auto-newline 1) ;; 启用auto newline
  (c-toggle-electric-state 1) ;; 按下某些符号如semicolon后自动格式化当前行
  )


(defun pkg/cedet/init()
  ;; CEDET的相关配置可以通过在相应buffer或模式下利用以下命令查看
  ;; (semantic-describe-buffer)
  ;; (semantic-c-describe-environment)
  (use-package cedet
    :preface
    (defun pkg/cedet/submode-enabled-p (mode)
      (member mode semantic-default-submodes))
    :init
    (use-package cedet-global
      :commands (cedet-gnu-global-version-check))
    (use-package ede
      :disabled
      :config
      ;; 支持利用makefile和automake所管理的项目，暂不支持cmake等
      ;; 默认使用Unix上的Locate命令来定位文件，此外还支持使用GNU Global
      ;; 但目前Emacs内置的CEDET中删除了ede-locate.el文件，因此也就暂不支持后者了
      ;; (setq ede-locate-setup-options '(ede-locate-global ede-locate-base))
      ;; 1. 对于复杂的项目，可利用(ede-new)新建并利用自动生成的Project.ede文件定制
      ;; 2. 对于简单的项目，可手写脚本定制具体信息，以下为示例
      ;; 通常应以项目根目录下已有的makefile或readme等固定文件作为"锚"
      ;; (ede-emacs-project)
      (defun pkg/cedet/new-ede-project (name file)
        (when (my/file-exists-p file)
          (ede-cpp-root-project name ;; project name
                                :file file ;; anchor in root folder
                                ;; :include-path '("./include")
                                ;; :system-include-path '("")
                                ;; :spp-table '(("MACRO" . "VALUE"))
                                ;; :compile-command "cd build && make"
                                )))
      ;; 目前手写的EDE项目配置信息由每个系统中的ede-projects.el文件统一地维护
      (mapc #'my/load-file pvt/project/ede-config-files)
      (global-ede-mode 1)
      (ede-enable-generic-projects))
    (use-package semantic
      :commands (semantic-mode semantic-toggle-minor-mode-globally)
      :init ;; 以下是由semantic及其子模块所定义的配置项，其必须在相应模块被加载前被设置
      ;; 由于(semantic-mode)是全局性的，通过设置以下过滤函数可避免其对其他语言也提供支持
      (add-hook 'semantic-inhibit-functions
                ;; 当前semantic仅用于C和C++，没有针对ObjC或Java等进行任何适配
                (lambda () (not (member major-mode '(c-mode c++-mode))))
                t)
      ;; 此外，也可局部性地启用，即将其子模式加入至各mode-hook中
      ;; 但目前发现idle相关的几个子模式必须全局性地启用，可能是由于被其他子模式所依赖的缘故
      ;; (my/prog-cc/add-start-hook #'semantic-idle-scheduler-mode)
      ;; 除了设置'semantic-default-submodes，还可调用以下函数来启用支持指定功能的子模块
      ;; (semantic-load-enable-minimum-features)
      ;; (semantic-load-enable-code-helpers)
      ;; (semantic-load-enable-guady-code-helpers)
      ;; (semantic-load-enable-excessive-code-helpers)
      ;; (semantic-load-enable-semantic-debugging-helpers)
      (setq semantic-default-submodes '(;; [Idle Scheduler]
                                        global-semantic-idle-scheduler-mode
                                        ;; 目前发现该模式高亮的符号并不全，即使启用它，也没有必要禁用highlight-thing插件
                                        ;; global-semantic-idle-local-symbol-highlight-mode
                                        ;; global-semantic-idle-summary-mode ;; 基于Smart Summary
                                        ;; global-semantic-idle-completions-mode ;; 基于Smart Completion，用company替代
                                        ;; [SemanticDB] tag database
                                        global-semanticdb-minor-mode
                                        ;; [Display and Decoration]
                                        ;; 该模式可以使用stickyfunc-enhance插件进行增强
                                        ;; 此外，其主要功能可被内置的(which-function-mode)替代
                                        ;; 由于其还会与tabbar插件相冲突，因此两者只能选其一
                                        ;; global-semantic-stickyfunc-mode
                                        ;; global-semantic-highlight-func-mode
                                        ;; global-semantic-decoration-mode
                                        ;; [Tag]
                                        global-semantic-mru-bookmark-mode ;; mostly recently used
                                        ;; [Debug]
                                        ;; global-semantic-show-unmatched-syntax-mode
                                        ;; global-semantic-show-parser-state-mode
                                        ;; global-semantic-highlight-edits-mode
                                        ;; [Mouse Context Menu]
                                        ;; global-cedet-m3-minor-mode
                                        ;; [CEDET 1.x] 以下功能并没有集成于Emacs内置的CEDET 2.x中
                                        ;; global-semantic-tag-folding-mode ;; (require 'semantic-tag-folding)
                                        ))
      (setq semantic-idle-scheduler-idle-time 3
            semantic-idle-scheduler-work-idle-time 15
            semantic-idle-scheduler-max-buffer-size 10240
            semantic-idle-scheduler-verbose-flag nil ;; 与semantic-idle-summary-mode冲突，故禁用
            ;; 比较耗时的任务
            semantic-idle-work-update-headers-flag t
            semantic-idle-work-parse-neighboring-files-flag t)
      (setq semanticdb-default-save-directory ;; 作为缺省路径，仅主动生成的数据库的文件才会保存于此
            (my/set-user-emacs-file ".semanticdb/")
            ;; semanticdb-default-file-name ""
            semanticdb-persistent-path '(always)
            ;; 可以预先主动地对某些目录生成数据库，以便今后复用
            semanticdb-search-system-databases t
            semanticdb-project-root-functions (when (my/package-enabled-p 'projectile)
                                                '(projectile-project-root)))
      (setq semantic-decoration-styles '(("semantic-tag-boundary" . t)
                                         ("semantic-decoration-on-private-members" . nil)
                                         ("semantic-decoration-on-protected-members" . nil)
                                         ("semantic-decoration-on-includes" . nil)))
      (setq semantic-c-obey-conditional-section-parsing-flag t)
      (defun pkg/cedet/setup-semantic () ;; 会在每次semantic被重新启用后执行
        (use-package semantic/symref
          :if (my/package-enabled-p 'projectile)
          :config
          (defalias 'semantic-symref-calculate-rootdir #'projectile-project-root))
        (use-package semantic/bovine/gcc
          :if (my/locate-exec "gcc")
          :config
          (semantic-gcc-setup))
        ;; (semantic-reset-system-include 'c-mode) ;; 若要完全地自定义，则需先重置再追加
        (semantic-add-system-include "/usr/include" 'c-mode)
        (semantic-add-system-include "/usr/include" 'c++-mode)
        (semantic-add-system-include "/usr/local/include" 'c-mode)
        (semantic-add-system-include "/usr/local/include" 'c++-mode)
        (semantic-add-system-include "/usr/include/boost" 'c++-mode)
        ;; 指定用于支持SemanticDB的tagging system，默认使用的是Ebrowse
        (use-package semantic/db-ebrowse ;; Ebrowse
          :disabled)
        (use-package semantic/db-global ;; GNU Global
          :if (cedet-gnu-global-version-check t)
          :config
          (semanticdb-enable-gnu-global-databases 'c-mode)
          (semanticdb-enable-gnu-global-databases 'c++-mode))
        ;; 若Semantic始终不能正常解析某些特定的符号，则可作如下设置
        ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("symbol" . "value"))
        ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file "path/file")
        (defun pkg/cedet/setup-c++-boost (path)
          (when (file-accessible-directory-p path)
            (let ((cfiles (cedet-files-list-recursively path "\\(config\\|user\\)\\.hpp")))
              (dolist (file cfiles)
                (add-to-list 'semantic-lex-c-preprocessor-symbol-file file))))))
      (my/add-mode-hook "SEMANTIC" #'pkg/cedet/setup-semantic)
      (defun pkg/cedet/c&c++-mode-hook ()
        ;; 不知为何在:config中使用CEDET中定义的(setq-mode-local)无法生效
        ;; 以下设置完全可以在(pkg/cedet/start)中被执行
        ;; 使用mode-hook的方式触发，仅仅是为了与上面的配置项相邻排列
        (setq semantic-stickyfunc-sticky-classes '(type function)) ;; variable, include, package
        (setq semanticdb-find-default-throttle
              '(file local project recursive) ;; unloaded, system, omniscience
              semanticdb-project-system-databases
              (mapcar (lambda (path)
                        (semanticdb-create-database
                         semanticdb-new-database-class path))
                      ;; e.g. "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
                      '("/usr/include" "/usr/local/include")))
        ;; 1. 对于'semanticdb-find-default-throttle中的'project，主要可交由EDE或JDE等组件控制
        ;; (add-hook 'semanticdb-project-predicate-functions)
        ;; (add-hook 'semanticdb-project-root-functions)
        ;; 甚至还可以通过设置以下变量，来具体指定某些项目的根目录
        ;; 注意，'semantic-project-root-functions中注册的函数也会修改该变量
        ;; (setq semanticdb-project-roots '())
        ;; 2. 对于'semanticdb-find-default-throttle中的'system，主要是利用编译器的输出信息
        )
      :config
      (my/add-mode-hook "c" #'pkg/cedet/c&c++-mode-hook)
      (my/add-mode-hook "c++" #'pkg/cedet/c&c++-mode-hook))
    (my/prog-cc/add-start-hook #'pkg/cedet/start)
    :config
    (semantic-mode 1) ;; global minor mode

    ;; todo: move into (pkg/cedet/setup-semantic)
    ;; 以下模块未必随semantic的启用而被加载，因此可于'semantic-init-hook中被执行
    ;; 但又由于其大多是对key-map的设置，在semantic/:config中也能被执行，目前只是暂置于此
    (defun pkg/cedet/fold-block ()
      (interactive)
      (if (pkg/cedet/submode-enabled-p 'global-semantic-tag-folding-mode)
          (semantic-tag-folding-fold-block) (senator-fold-tag)))
    (defun pkg/cedet/unfold-block ()
      (interactive)
      (if (pkg/cedet/submode-enabled-p 'global-semantic-tag-folding-mode)
          ;; (senator-unfold-tag)
          (semantic-tag-folding-show-block) (senator-fold-tag-toggle)))
    (bind-keys :map semantic-mode-map
               ("C-c ," . pkg/hydra/group/cedet/body))

    ;; -------------------------------------------------------------------------
    ;; 以下是代码浏览功能的相关设置，主要涉及到了Complete、Senator、IA这三个组件
    ;; 这三个组件在以下功能上是有很多重叠的
    ;; 1. 符号补全，可由auto-complete或company插件替代
    ;; 2. 符号跳转/搜索，可由gtags或ctags工具替代
    ;; 3. 书签标记/跳转，可由bm插件替代
    ;; 4. 代码折叠，暂无替代
    ;; 目前体验下来，相比于CEDET组件，上述替代插件/工具的准确性和效率都更好
    (use-package semantic/complete
      ;; Complete跳转需要手动输入符号
      :commands (semantic-complete-jump
                 semantic-complete-jump-local
                 semantic-complete-jump-local-members
                 semantic-complete-analyze-inline)
      :init
      (setq semantic-complete-inline-analyzer-idle-displayor-class ;; 以何种方式显示
            ;; 'semantic-displayor-ghost ;; inline
            ;; 'semantic-displayor-tooltip ;; tooltip
            'semantic-displayor-traditional ;; separate window
            semantic-displayor-tooltip-mode ;; 显示多少
            ;; 'quiet ;; 只有当数量小于initial-max-tags时才显示
            ;; 'verbose ;; 显示所有，貌似有bug，慎用
            'standard ;; initial-max-tags
            semantic-displayor-tooltip-initial-max-tags 8))
    (use-package semantic/senator
      ;; Senator可在当前文件中已被解析出的符号所出现的位置之间按序地跳转的
      ;; 相当于是在遍历符号表，似乎实际用处不大
      ;; Senator还提供了代码折叠的功能，但没有semantic-tag-folding的功能全面
      :commands (senator-next-tag
                 senator-previous-tag
                 senator-go-to-up-reference
                 senator-copy-tag-to-register
                 senator-kill-tag
                 senator-yank-tag
                 senator-transpose-tags-down
                 senator-transpose-tags-up
                 senator-copy-tag
                 senator-fold-tag
                 senator-unfold-tag
                 senator-fold-tag-toggle))
    (use-package semantic/ia
      :commands (semantic-ia-complete-tip
                 semantic-ia-complete-symbol
                 semantic-ia-complete-symbol-menu
                 semantic-ia-fast-jump
                 semantic-ia-show-summary
                 semantic-ia-show-doc))
    (use-package semantic/mru-bookmark
      ;; 'semantic-mru-bookmark-mode-map
      :commands (semantic-mrub-switch-tags)
      :if (pkg/cedet/submode-enabled-p 'global-semantic-mru-bookmark-mode))
    (use-package semantic-tag-folding
      ;; 'semantic-tag-folding-mode-map
      :commands (semantic-tag-folding-fold-block
                 semantic-tag-folding-show-block
                 semantic-tag-folding-fold-all
                 semantic-tag-folding-show-all)
      :if (pkg/cedet/submode-enabled-p 'global-semantic-tag-folding-mode))
    (use-package semantic/sb ;; 此为CEDET中内置的Speedbar，暂不启用
      :disabled)
    (use-package stickyfunc-enhance
      :demand t
      :if (and (my/package-enabled-p 'stickyfunc-enhance)
               (pkg/cedet/submode-enabled-p 'global-semantic-stickyfunc-mode)))
    ))

(defun pkg/cedet/start ()
  )


(defun pkg/ecb/init ()
  (use-package ecb
    :if (my/package-enabled-p 'ecb)
    :commands (ecb-minor-mode)
    :init
    (my/prog-cc/add-start-hook #'pkg/ecb/start)
    :config
    (save-excursion
      (unless (boundp 'stack-trace-on-error)
        (defvar stack-trace-on-error nil)) ;; 兼容性
      (setq ecb-layout-name "left15"
            ;; ecb-toggle-layout-sequence '()
            ;; ecb-layout-window-sizes nil ;; 推荐通过调用ecb-change-layout命令，以交互式的方式修改
            ecb-windows-width 0.2
            ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
            ecb-tip-of-the-day nil
            ;; ecb-auto-compatibility-check nil
            ;; [directories window]
            ecb-source-path '("~")
            ecb-tree-buffer-style 'image
            ecb-auto-expand-directory-tree 'best
            ecb-excluded-directories-regexps '("^\\(\\.\\|\\.\\.\\)$")
            ecb-show-sources-in-directories-buffer '("left15")
            ;; [sources window]
            ;; ecb-source-file-regexps '()
            ;; ecb-sources-exclude-cvsignore '()
            ;; [methods window]
            ecb-process-non-semantic-files nil ;; 禁用non-semantic-sources
            ;; [history window]
            ;; ecb-history-exclude-file-regexps '()
            ;; [compilation window]
            ecb-compile-window-height 0.2
            ecb-compile-window-width 'edit-window
            ecb-compile-window-temporally-enlarge 'both
            ecb-enlarged-compilation-window-max-height 0.5
            ecb-compilation-buffer-names ;; 以下名称的buffer内容将被呈现于该窗口
            (append ecb-compilation-buffer-names '(("*Process List*")
                                                   ("*Proced*")
                                                   (".notes")
                                                   ("*appt-buf*")
                                                   ("*Compile-Log*")
                                                   ("*etags tmp*")
                                                   ("*svn-process*")
                                                   ("*svn-info-output*")
                                                   ("*Python Output*")
                                                   ("*Org Agenda*")
                                                   ("*EMMS Playlist*")
                                                   ("*Moccur*")
                                                   ("*Directory")))
            ecb-compilation-major-modes ;; 以下模式的buffer内容将被呈现于该窗口
            (append ecb-compilation-major-modes '(change-log-mode
                                                  calendar-mode
                                                  diary-mode
                                                  diary-fancy-display-mode
                                                  xgtags-select-mode
                                                  svn-status-mode
                                                  svn-info-mode
                                                  svn-status-diff-mode
                                                  svn-log-view-mode
                                                  svn-log-edit-mode
                                                  erc-mode
                                                  gud-mode)))
      (ecb-minor-mode 1) ;; global minor mode
      )))

(defun pkg/ecb/start ()
  )


;; todo
;; 此插件会读取CMake所使用的项目配置文件，包括CMakeLists.txt
;; 用于设置但不依赖于以下插件的存在，其仅依赖于CMake
;; [Flymake]
;; [Flycheck]
;; 'flycheck-clang-language-standard
;; 'flycheck-gcc-language-standard
;; 'flycheck-clang-include-path
;; 'flycheck-clang-definitions
;; [Company]
;; 'company-clang-arguments
;; [Company-C-Headers]
;; 'company-c-headers-path-system
;; [Auto-Complete]
;; 'ac-clang-flags
;; [Semantic]
;; (semantic-add-system-include)
;; (semantic-remove-system-include)
(defun pkg/cpputils-cmake/init ()
  (use-package cpputils-cmake
    :if (and (my/package-enabled-p 'cmake-mode)
             (my/package-enabled-p 'cpputils-cmake))
    :commands (cppcm-reload-all)
    :init
    (setq cppcm-write-flymake-makefile nil ;; since flymake is not used for now
          ;; optional, specify extra preprocess flags forwarded to compiler
          ;; cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG")
          )
    (my/prog-cc/add-start-hook #'pkg/cpputils-cmake/start)))

(defun pkg/cpputils-cmake/start ()
  (cppcm-reload-all))


;; Grand Unified Debugger
;; optional, avoid typing full path when starting gdb
;; (bind-keys ("C-c C-g" . (lambda () (interactive)
;; (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer))))))
(defun pkg/gud/init ()
  (use-package gud
    :init
    (my/prog-cc/add-start-hook #'pkg/gud/start)
    )
  )

(defun pkg/gud/start ()
  )


(defun my/prog-cc/init ()
  (pkg/cc-mode/init)
  ;; 所有依赖于CEDET的插件都必须在CEDET之后被加载/启用
  ;; 否则其会自动加载/启用CEDET，导致上述对于CEDET的设置失效
  ;; 所有与CEDET相互关联的插件的加载/启动顺序将在此被显示地指定
  ;; 而不是也不应依赖于hook的执行顺序
  (pkg/cedet/init)
  (pkg/ecb/init)
  (pkg/cpputils-cmake/init)
  (pkg/gud/init)
  (my/add-mode-hook "c" #'my/prog-cc/start)
  (my/add-mode-hook "c++" #'my/prog-cc/start))

(defun my/prog-cc/start ()
  (my/prog-cc/run-start-hook))

(eval-after-load 'cc-mode '(add-hook 'c-initialization-hook #'my/prog-cc/init t))

(provide 'my/prog-cc)
