;; -*- coding: utf-8 -*-

(defun my/prog-cc/add-start-hook (func)
  (my/add-mode-hook "my/prog-cc" func))

(defun my/prog-cc/run-start-hook ()
  (my/run-mode-hook "my/prog-cc"))

(defun my/prog-cc/init ()
  (pkg/cc-mode/init)
  (pkg/cedet/init)
  (pkg/irony/init)
  (my/add-mode-hook "c" #'my/prog-cc/run-start-hook)
  (my/add-mode-hook "c++" #'my/prog-cc/run-start-hook))

(my/add-mode-hook "CC" #'my/prog-cc/init)


(defun pkg/cc-mode/init ()
  (use-package cc-mode
    :preface
    (defun pkg/cc-mode/start ()
      ;; 也可以通过(setq c-default-style)实现
      (c-set-style "my/cc-style")
      ;; 启用根据语法缩进，否则任何基于语法的style都将失效
      (c-toggle-syntactic-indentation 1)
      ;; 按下某些符号如semicolon后自动格式化当前行
      (c-toggle-electric-state 1)
      (c-toggle-auto-newline 1)
      (c-toggle-auto-hungry-state -1))
    :init
    (my/prog-cc/add-start-hook #'pkg/cc-mode/start)
    :config
    (c-add-style "my/cc-style"
                 '("bsd"
                   (c-basic-offset . 4)
                   (c-tab-always-indent . t)
                   (c-echo-syntactic-information-p . nil)
                   (c-report-syntactic-errors . nil)
                   (c-ignore-auto-fill . (string cpp code)) ;; 设置在哪些地方禁用auto-fill功能，目前仅在注释中启用
                   ;; (c-special-indent-hook . ())
                   ;; (c-label-minimum-indentation . )
                   (c-indent-comments-syntactically-p . t)
                   (c-comment-only-line-offset . 0)
                   (c-comment-prefix-regexp . ((c-mode . "//+\\|\\**")
                                               (c++-mode . "//+\\|\\**")))
                   (c-block-comment-prefix . "*") ;; 设置block comment在换行时自动添加的前缀
                   (c-offsets-alist . ((case-label . +)))
                   (c-indent-comment-alist . ((empty-line . (column . nil))
                                              (anchored-comment . (column . nil))
                                              (end-block . (space . 1))
                                              (cpp-end-block . (space . 1))
                                              (other . (align . (space . 1)))))
                   ;; 设置大括号的前后是否应换行，即"{"将位于行头或行尾或单行或任意位置
                   ;; 不在以下alist中列出的syntactic symbol，将执行默认行为：(before after)
                   (c-hanging-braces-alist . ((defun-open . (before))
                                              (defun-close . (before))
                                              (class-open . (before))
                                              (class-close . (before))
                                              (inline-open . (before))
                                              (inline-close . (before))
                                              (block-open . (before))
                                              (block-close . c-snug-do-while) ;;
                                              (statement-cont)
                                              (substatement-open . (before))
                                              (statement-case-open . (before after)) ;;
                                              (brace-list-open)
                                              (brace-list-close)
                                              (brace-list-intro)
                                              (brace-entry-open)
                                              (extern-lang-open . (before))
                                              (extern-lang-close . (before))
                                              (namespace-open . (before))
                                              (namespace-close . (before))
                                              (module-open . (before))
                                              (module-close . (before))
                                              (composition-open . (before))
                                              (composition-close . (before))
                                              (inexpr-class-open . (before))
                                              (inexpr-class-close . (before))
                                              (arglist-cont-nonempty)))
                   ;; 设置冒号的前后是否应换行
                   ;; 不在以下alist中列出的syntactic symbol，将执行默认行为：()
                   (c-hanging-colons-alist . ((case-label . (after))
                                              (label . (after))
                                              (access-label . (after))
                                              (member-init-intro . (after))
                                              (inher-intro . (after))))
                   ;; 设置分号和逗号的前后是否应换行
                   (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-for-oneline-inliners
                                                     c-semi&comma-inside-parenlist
                                                     c-semi&comma-no-newlines-before-nonblanks))
                   ;; 清理whitespace，并作为上述hanging的补充(在其后生效)
                   (c-cleanup-list . (;; brace-else-brace
                                      ;; brace-elseif-brace
                                      ;; brace-catch-brace
                                      ;; empty-defun-braces
                                      ;; one-liner-defun
                                      defun-close-semi
                                      list-close-comma
                                      scope-operator
                                      ;; space-before-funcall
                                      ;; compact-empty-funcall
                                      comment-close-slash))
                   (c-backslash-column . 48)
                   (c-backslash-max-column . 72)
                   (c-auto-align-backslashes . t)))
    (bind-keys :map c-mode-base-map
               ;; 换行后自动缩进
               ("<return>" . c-context-line-break))))

(defun pkg/cedet/init ()
  (use-package cedet
    ;; CEDET的相关配置可以通过在相应buffer或模式下利用以下命令查看
    ;; (cedet-version)
    ;; (semantic-describe-buffer)
    ;; (semantic-c-describe-environment)
    :preface
    (defun pkg/cedet/start ())
    (defun pkg/cedet/submode-enabled-p (mode)
      (member mode semantic-default-submodes))
    :init
    (use-package cedet-global
      :commands (cedet-gnu-global-version-check))
    (my/prog-cc/add-start-hook #'pkg/cedet/start)
    :config
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
    (use-package srecode
      :disabled)
    (use-package semantic
      :init ;; 以下是由semantic及其子模块所定义的配置项，其必须在相应模块被加载前被设置
      (pkg/semantic/init) ;; 在'semantic被加载时执行
      (my/add-mode-hook "SEMANTIC" #'pkg/semantic/setup) ;; 在每次'semantic被全局地(重新)启用时执行
      (my/prog-cc/add-start-hook #'pkg/semantic/start) ;; 在每次打开文件时执行
      :config
      (semantic-mode 1)
      (bind-keys :map semantic-mode-map
                 ("C-c ," . pkg/hydra/group/cedet/body)))


    ;; todo: move into (pkg/semantic/setup)
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
      :if (and (my/package-enabled-p 'stickyfunc-enhance)
               (pkg/cedet/submode-enabled-p 'global-semantic-stickyfunc-mode)))))

(defun pkg/semantic/init ()
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
  (setq semantic-c-obey-conditional-section-parsing-flag t))

(defun pkg/semantic/setup ()
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
  (defun pkg/semantic/setup-c++-boost (path)
    (when (file-accessible-directory-p path)
      (let ((cfiles (cedet-files-list-recursively path "\\(config\\|user\\)\\.hpp")))
        (dolist (file cfiles)
          (add-to-list 'semantic-lex-c-preprocessor-symbol-file file))))))

(defun pkg/semantic/start ()
  ;; 不知为何在:config中使用CEDET中定义的(setq-mode-local)无法生效
  ;; 以下设置也可以在(pkg/cedet/start)中被执行
  (setq semantic-stickyfunc-sticky-classes '(type function)) ;; variable, include, package
  (setq semanticdb-find-default-throttle
        '(file local project recursive) ;; unloaded, system, omniscience
        semanticdb-project-system-databases
        (my/map (lambda (path)
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

(defun pkg/irony/init ()
  (use-package irony
    :preface
    (defun pkg/irony/setup ()
      (irony-cdb-autosetup-compile-options)
      (use-package irony-completion
        :init
        (setq irony-duplicate-candidates-filter t)
        :config
        (bind-keys :map irony-mode-map
                   ([remap complete-symbol] . irony-completion-at-point-async)
                   ([remap completion-at-point] . irony-completion-at-point-async))))
    (defun pkg/irony/start ()
      (irony-mode 1))
    :if (my/package-enabled-p 'irony)
    :init
    (setq irony-server-install-prefix (my/set-user-emacs-file ".irony/")
          irony-user-dir irony-server-install-prefix
          irony-supported-major-modes '(c-mode c++-mode))
    (my/add-mode-hook "irony" #'pkg/irony/setup)
    (my/prog-cc/add-start-hook #'pkg/irony/start)))


(provide 'my/prog-cc)
