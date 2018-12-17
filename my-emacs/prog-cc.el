;; -*- coding: utf-8 -*-
;; 本文件中的设置仅针对于C和C++，未必适用于Objective-C, Java等

(require 'my-prog)

;; replace built-in CEDET with an external one, if exists
;; $ git clone http://git.code.sf.net/p/cedet/git cedet
;; $ make
;; CEDET及其现状的介绍
;; https://www.emacswiki.org/emacs/CollectionOfEmacsDevelopmentEnvironmentTools
;; https://stackoverflow.com/questions/12711765/status-of-cedet-and-ecb-in-emacs-24-2
;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
(when (boundp 'my-private-project-root-directory)
  (let* ((path (file-name-as-directory
                (concat my-private-project-root-directory "cedet")))
         (file (concat path "cedet-devel-load.el")))
    (when (file-exists-p file)
      (load-file file)
      (add-to-list 'load-path (concat path "contrib"))
      (add-to-list 'Info-directory-list (concat path "doc/info/")))))

(defvar my-prog-cc-mode-start-hook '())

;; =============================================================================
;; CC-mode
;; http://cc-mode.sourceforge.net/
(defun my-plugin-cc-mode-init ()
  (use-package cc-mode
    :init
    (add-hook 'my-prog-cc-mode-start-hook 'my-plugin-cc-mode-start t)
    :config
    (c-add-style "my-c-style"
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

(defun my-plugin-cc-mode-start ()
  (c-set-style "my-c-style" nil) ;; 也可以通过(setq c-default-style)实现
  (c-toggle-syntactic-indentation 1) ;; 启用根据语法缩进，否则任何基于语法的style都将失效
  (c-toggle-auto-newline 1) ;; 启用auto newline
  (c-toggle-electric-state 1) ;; 按下某些符号如semicolon后自动格式化当前行
  )

;; =============================================================================
;; CEDET
(defun my-plugin-cedet-init()
  ;; CEDET的相关配置可以通过在相应buffer或模式下利用以下命令查看
  ;; (semantic-describe-buffer)
  ;; (semantic-c-describe-environment)
  (use-package cedet
    :demand t
    :commands (semantic-mode semantic-toggle-minor-mode-globally)
    :init
    ;; 由于(semantic-mode)是全局性的，为避免其对其他语言也提供支持
    ;; 因此应局部性地启用部分子模式，即将其加入至'my-prog-cc-mode-start-hook中
    ;; 但目前发现idle相关的几个子模式必须全局性地启用，可能是由于被其他子模式所依赖的缘故
    ;; (add-hook 'my-prog-cc-mode-start-hook 'semantic-idle-scheduler-mode t)
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
                                      global-semantic-idle-summary-mode ;; 基于Smart Summary
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
                                      )
          ;; -------------------------------------------------------------------
          semantic-complete-inline-analyzer-idle-displayor-class ;; 以何种方式显示
          ;; 'semantic-displayor-ghost ;; inline
          ;; 'semantic-displayor-tooltip ;; tooltip
          'semantic-displayor-traditional ;; separate window
          semantic-displayor-tooltip-mode ;; 显示多少
          ;; 'quiet ;; 只有当数量小于initial-max-tags时才显示
          ;; 'verbose ;; 显示所有，貌似有bug，慎用
          'standard ;; initial-max-tags
          semantic-displayor-tooltip-initial-max-tags 8
          ;; -------------------------------------------------------------------
          semantic-idle-scheduler-idle-time 3
          semantic-idle-scheduler-work-idle-time 15
          semantic-idle-scheduler-max-buffer-size 10240
          semantic-idle-scheduler-verbose-flag nil ;; 与semantic-idle-summary-mode冲突，故禁用
          ;; 比较耗时的任务
          semantic-idle-work-update-headers-flag t
          semantic-idle-work-parse-neighboring-files-flag t
          ;; -------------------------------------------------------------------
          ;; 作为缺省路径，仅主动生成的数据库的文件才会保存于此
          semanticdb-default-save-directory (concat user-emacs-directory "semanticdb")
          ;; semanticdb-default-file-name ""
          semanticdb-persistent-path '(always)
          ;; 可以预先主动地对某些目录生成数据库，以便今后复用
          semanticdb-search-system-databases t
          ;; -------------------------------------------------------------------
          semantic-decoration-styles '(("semantic-tag-boundary" . t)
                                       ("semantic-decoration-on-private-members" . nil)
                                       ("semantic-decoration-on-protected-members" . nil)
                                       ("semantic-decoration-on-includes" . nil))
          ;; -------------------------------------------------------------------
          semantic-c-obey-conditional-section-parsing-flag t)
    (defun pkg/cedet/cc-mode-hook ()
      ;; 不知为何在:config中使用CEDET中定义的(setq-mode-local)无法生效
      ;; 以下设置完全可以在(my-plugin-cedet-start)中被执行
      ;; 使用mode-hook的方式触发，仅仅是为了与上面的配置项相邻排列
      (setq semantic-stickyfunc-sticky-classes
            '(type function) ;; variable, include, package
            semanticdb-find-default-throttle
            '(file local project unloaded system recursive) ;; omniscience
            semanticdb-project-system-databases
            (mapcar (lambda (path)
                      (semanticdb-create-database
                       semanticdb-new-database-class path))
                    ;; e.g. "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
                    '("/usr/include" "/usr/local/include"))))
    (my/add-language-mode-hook "cc" 'pkg/cedet/cc-mode-hook)
    ;; 设置'semanticdb-find-default-throttle中的'project，主要交由EDE或JDE等组件控制
    ;; (add-hook semanticdb-project-predicate-functions ) ;; 此项交由EDE设置
    ;; (add-hook semanticdb-project-root-functions ) ;; 此项交由EDE设置
    ;; 设置'semanticdb-find-default-throttle中的'system，可以利用编译器的输出信息
    ;; 甚至可以具体指定一些项目的根目录，该变量也会被semantic-project-root-functions中注册的函数修改
    ;; (setq semanticdb-project-roots '())
    (use-package cedet-global
      :commands (cedet-gnu-global-version-check))
    (add-hook 'my-prog-cc-mode-start-hook 'my-plugin-cedet-start t)
    :config
    (semantic-mode 1) ;; global minor mode
    (use-package stickyfunc-enhance
      :if (and (my-func-package-enabled-p 'stickyfunc-enhance)
               (member 'global-semantic-stickyfunc-mode semantic-default-submodes))
      :demand t)
    (use-package semantic/bovine/gcc
      :if (executable-find "gcc")
      :config
      (semantic-gcc-setup))
    ;; 若要完全地自定义，则需先重置再追加，例如(semantic-reset-system-include 'c-mode)
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
    (defun my-plugin/cedet/c++-setup-boost (path)
      (when (file-accessible-directory-p path)
        (let ((cfiles (cedet-files-list-recursively path "\\(config\\|user\\)\\.hpp")))
          (dolist (file cfiles)
            (add-to-list 'semantic-lex-c-preprocessor-symbol-file file)))))
    ;; -------------------------------------------------------------------------
    ;; 以下是代码浏览功能的相关设置，主要涉及到了Complete、Senator、IA这三个组件
    ;; 这三个组件在以下功能上是有很多重叠的
    ;; 1. 符号补全，可由auto-complete或company插件替代
    ;; 2. 符号跳转/搜索，可由gtags或ctags工具替代
    ;; 3. 书签标记/跳转，可由bm插件替代
    ;; 4. 代码折叠，暂无替代
    ;; 目前体验下来，相比于CEDET组件，上述替代插件/工具的准确性和效率都更好
    (use-package semantic/complete
      :config
      ;; Complete跳转需要手动输入符号
      (unbind-key "C-c , J" semantic-mode-map) ;; (semantic-complete-jump)
      (unbind-key "C-c , j" semantic-mode-map) ;; (semantic-complete-jump-local)
      (unbind-key "C-c , m" semantic-mode-map) ;; (semantic-complete-jump-local-members)
      (unbind-key "C-c , SPC" semantic-mode-map) ;; (semantic-complete-analyze-inline)
      )
    (use-package semantic/senator
      :config
      ;; Senator可在当前文件中已被解析出的符号所出现的位置之间按序地跳转的
      ;; 相当于是在遍历符号表，似乎实际用处不大
      (unbind-key "C-c , n" semantic-mode-map) ;; (senator-next-tag)
      (unbind-key "C-c , p" semantic-mode-map) ;; (senator-previous-tag)
      (unbind-key "C-c , u" semantic-mode-map) ;; (senator-go-to-up-referenc)
      (unbind-key "C-c , r" semantic-mode-map) ;; (senator-copy-tag-to-register)
      (unbind-key "C-c , C-w" semantic-mode-map) ;; (senator-kill-tag)
      (unbind-key "C-c , C-y" semantic-mode-map) ;; (senator-yank-tag)
      (unbind-key "C-c , <down>" semantic-mode-map) ;; (senator-transpose-tags-down)
      (unbind-key "C-c , <up>" semantic-mode-map) ;; (senator-transpose-tags-up)
      (unbind-key "C-c , M-w" semantic-mode-map) ;; (senator-copy-tag)
      ;; Senator还提供了代码折叠的功能，但没有semantic-tag-folding的功能全面
      (unless (member 'global-semantic-tag-folding-mode semantic-default-submodes)
        (bind-keys :map semantic-mode-map
                   ("C-c , -" . senator-fold-tag)
                   ;; ("C-c , =" . senator-unfold-tag)
                   ("C-c , =" . senator-fold-tag-toggle))))
    (use-package semantic/ia
      :config
      (bind-keys :map semantic-mode-map
                 ("" . semantic-ia-complete-tip)
                 ("" . semantic-ia-complete-symbol)
                 ("" . semantic-ia-complete-symbol-menu)
                 ("C-c , ," . semantic-ia-fast-jump)
                 ("C-c , ." . semantic-ia-show-summary)
                 ("C-c , /" . semantic-ia-show-doc)))
    (use-package semantic/mru-bookmark
      :if (member 'global-semantic-mru-bookmark-mode semantic-default-submodes)
      :config
      (unbind-key "C-x B" semantic-mru-bookmark-mode-map) ;; (semantic-mrub-switch-tags)
      (bind-keys :map semantic-mru-bookmark-mode-map
                 ("C-c , b" . semantic-mrub-switch-tags)))
    ;; 杂项
    (unbind-key "C-c , l" semantic-mode-map) ;; (semantic-analyze-possible-completions)
    (bind-keys :map semantic-mode-map
               ("C-c , G" . semantic-symref) ;; 寻找光标所在函数被引用的地方
               ("C-c , g" . semantic-symref-symbol) ;; 寻找光标所在符号被引用的地方
               ("C-c , i" . semantic-decoration-include-visit) ;; jump to include file
               ("C-c , t" . semantic-analyze-proto-impl-toggle))
    (use-package semantic/sb ;; 此为CEDET中内置的Speedbar，不启用
      :disabled)
    (use-package semantic-tag-folding
      :if (member 'global-semantic-tag-folding-mode semantic-default-submodes)
      :config
      (bind-keys :map semantic-tag-folding-mode-map
                 ("C-c , -" . semantic-tag-folding-fold-block)
                 ("C-c , =" . semantic-tag-folding-show-block)
                 ("C-c , _" . semantic-tag-folding-fold-all)
                 ("C-c , +" . semantic-tag-folding-show-all)))
    ;; -------------------------------------------------------------------------
    (use-package ede
      :disabled
      :config
      ;; 支持利用makefile和automake所管理的项目，暂不支持cmake等
      ;; 默认使用Unix上的Locate命令来定位文件，此外还支持使用GNU Global
      ;; 但目前Emacs内置的CEDET中删除了ede-locate.el文件，因此也就暂不支持后者了
      ;; (setq ede-locate-setup-options '(ede-locate-global ede-locate-base))
      ;; 1. 对于复杂的项目，可利用(ede-new)新建并利用自动生成的Project.ede文件定制
      ;; 2. 对于简单的项目，可手写脚本定制具体信息，以下为示例
      (when (boundp 'my-private-project-root-directory)
        ;; 通常应以项目根目录下已有的makefile或readme等固定文件作为锚
        ;; (ede-emacs-project)
        (let ((anchor (concat my-private-project-root-directory "Emacs/README.md")))
          (when (file-exists-p anchor)
            (ede-cpp-root-project "Emacs" ;; name
                                  :file anchor ;; root folder
                                  ;; :include-path '("./include")
                                  ;; :system-include-path '("")
                                  ;; :spp-table '(("MACRO1" . "VALUE1"))
                                  ;; :compile-command "cd build && make"
                                  ))))
      ;; 目前手写的EDE项目配置信息由每个系统中的ede-projects.el文件统一地维护
      (when (boundp 'my-private-project-ede-config-file)
        (load my-private-project-ede-config-file nil nil t))
      (global-ede-mode 1)
      (ede-enable-generic-projects))))

(defun my-plugin-cedet-start ()
  )

;; =============================================================================
;; ECB (Emacs Code Browser)
(defun my-plugin-ecb-init ()
  (use-package ecb
    :if (my-func-package-enabled-p 'ecb)
    :demand t
    :commands (ecb-minor-mode)
    :init
    (add-hook 'my-prog-cc-mode-start-hook 'my-plugin-ecb-start t)
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
            ;; directories window
            ecb-source-path '("~")
            ecb-tree-buffer-style 'image
            ecb-auto-expand-directory-tree 'best
            ecb-excluded-directories-regexps '("^\\(\\.\\|\\.\\.\\)$")
            ecb-show-sources-in-directories-buffer '("left15")
            ;; sources window
            ;; ecb-source-file-regexps '()
            ;; ecb-sources-exclude-cvsignore '()
            ;; methods window
            ecb-process-non-semantic-files nil ;; 禁用non-semantic-sources
            ;; history window
            ;; ecb-history-exclude-file-regexps '()
            ;; compilation window
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
      (ecb-minor-mode 1) ;;global minor mode
      )))

(defun my-plugin-ecb-start ()
  )

;; =============================================================================
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
(defun my-plugin/cpputils-cmake/init ()
  (use-package cpputils-cmake
    :if (and (my-func-package-enabled-p 'cmake-mode)
             (my-func-package-enabled-p 'cpputils-cmake))
    :commands (cppcm-reload-all)
    :init
    (setq cppcm-write-flymake-makefile nil ;; since flymake is not used for now
          ;; optional, specify extra preprocess flags forwarded to compiler
          ;; cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG")
          )
    (add-hook 'my-prog-cc-mode-start-hook 'my-plugin/cpputils-cmake/start t)))

(defun my-plugin/cpputils-cmake/start ()
  (cppcm-reload-all))

;; Grand Unified Debugger
;; optional, avoid typing full path when starting gdb
;; (bind-keys ("C-c C-g" . (lambda () (interactive)
;; (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer))))))

;; =============================================================================
;; =============================================================================
(defun my-prog-cc-mode-init ()
  (my-plugin-cc-mode-init)
  ;; 所有依赖于CEDET的插件都必须在CEDET之后被加载/启用
  ;; 否则其会自动加载/启用CEDET，导致上述对于CEDET的设置失效
  ;; 所有与CEDET相互关联的插件的加载/启动顺序将在此被显示地指定
  ;; 而不是也不应依赖于hook的执行顺序
  (my-plugin-cedet-init)
  (my-plugin-ecb-init)
  (my-plugin/cpputils-cmake/init)
  (add-hook 'c-mode-common-hook 'my-prog-cc-mode-start t))

(defun my-prog-cc-mode-start ()
  (run-hooks 'my-prog-cc-mode-start-hook))

(eval-after-load 'cc-mode '(add-hook 'c-initialization-hook 'my-prog-cc-mode-init t))

(provide 'my-prog-cc)
