(require 'my-prog)
;===========================================================================
; Style
;===========================================================================
(defun my-cc-style-init ()
  (load-file (concat my-emacs-config-file-path "prog-cc-style.el"))
  (define-key c-mode-base-map "\C-m" 'c-context-line-break) ;换行后自动缩进
  )
(defun my-cc-style-start ()
  (c-set-style "my-c-style") ;也可以通过(setq c-default-style)实现
  (c-toggle-syntactic-indentation 1) ;启用根据语法缩进，否则任何基于语法的style都将失效
  (c-toggle-auto-newline 1) ;启用auto newline
  (c-toggle-electric-state 1) ;按下某些符号如semicolon后自动格式化当前行
;  (read-only-mode 1) ;不推荐在这里启用只读模式，会与其他插件冲突
  )


;===========================================================================
; CEDET (Collection of Emacs Development Environment Tools)
;===========================================================================
; Semantic
;---------------------------------------------------------------------------
; 其核心是two parser generators：Bovine和Wisent
; 前者可用于支持C、C++、Lisp，而后者(基于GNU Bison实现)则可用于支持Java、Javascript、Python
; 利用上述语法分析器所得到的结果，Semantic还进一步地提供了以下一系列扩展功能
; 与之相比的是，旧版本的Emacs中采用正则表达式作为下述功能的基础支持，就显得比较落后了
; 1) Semantic/Analyzer
; 主要从语法分析的角度提供了代码补全、跳转(浏览)、信息汇总等编程辅助功能
; semantic-ia-complete-symbol
; semantic-ia-complete-symbol-menu
; semantic-ia-complete-tip
; semantic-complete-analyze-inline
; semantic-complete-analyze-inline-idle
; semantic-analyze-possible-completions
; semantic-ia-fast-jump
; 2) Senator (SEmantic/NAvigaTOR)
; 其提供的功能与Analyzer类似，但实现方式上更为简单
; 也因此导致其结果往往准确度较低，但响应速度更快
; senator-complete-symbol
; senator-completion-menu-popup
; 3) Semantic/Symref (SYMbol REFerence)
; 其提供的是查询某个符号在项目代码中被引用到的所有地方，但主要作为前端接口
; 而后台实现则完全基于外部工具，包括了GNU Global、Cscope、find/grep
; 4) Idle Scheduler
; 在用户操作闲置时，实施语法分析、SemanticDB生成、信息提示等任务
; 主要基于的是Semantic/Analyzer
; 5) SemanticDB
; 其以文件的形式，保存了由parser或tagging system所生成的标签信息
; 于是，代码补全、跳转、信息汇总等功能的实现，不仅可以基于实时的语法分析
; 也可以通过复用数据库的方式，从而也就提高了这些常用功能的执行效率
; 此外，以Semantic的输出作为输入源，它还能与许多非CEDET集合中的插件配合使用
; 其中最常用的就包括了auto-complete, eassist
;---------------------------------------------------------------------------
;===========================================================================
; 以下配置主要面向于内置版，但基本上能兼容于独立版
;===========================================================================
(defun my-plugin-cedet-init ()
  (require 'cedet)

  ;-------------------------------------------------------------------------
  ; Semantic
  ;-------------------------------------------------------------------------
  ;; 先设置semantic-default-submodes，再调用semantic-mode
  (setq semantic-default-submodes '(;; SemanticDB
                                    global-semanticdb-minor-mode
                                    ;; Idle Scheduler
                                    global-semantic-idle-scheduler-mode
                                    global-semantic-idle-summary-mode ;基于Smart Summary
;                                    global-semantic-idle-completions-mode ;基于Smart Completion
                                    global-semantic-idle-local-symbol-highlight-mode
                                    ;; Display and Decoration
                                    global-semantic-stickyfunc-mode
                                    global-semantic-highlight-func-mode
                                    global-semantic-decoration-mode
                                    ;; Senator
                                    global-semantic-mru-bookmark-mode ;mostly recently used
                                    ;; 未知
;                                    global-cedet-m3-minor-mode
                                    ;; Debug
;                                    global-semantic-show-unmatched-syntax-mode
                                    global-semantic-show-parser-state-mode
;                                    global-semantic-highlight-edits-mode
                                    ))
  (semantic-mode 1) ;global minor mode
  ;; 除此之外，还可以同时利用以下一系列函数来定制Semantic功能
;  (semantic-load-enable-minimum-features)
;  (semantic-load-enable-code-helpers)
;  (semantic-load-enable-guady-code-helpers)
;  (semantic-load-enable-excessive-code-helpers)
;  (semantic-load-enable-semantic-debugging-helpers)

  ;-------------------------------------------------------------------------
  ; Idle Scheduler
  ;-------------------------------------------------------------------------
  (setq semantic-idle-scheduler-idle-time 1)
  (setq semantic-idle-scheduler-max-buffer-size 10000)
  ;; 以下两个的显示与semantic-idle-summary-mode相冲突，故
  (setq semantic-idle-scheduler-verbose-flag nil) ;前者被禁用
  (setq semantic-idle-scheduler-no-working-message nil)
  (setq semantic-idle-scheduler-working-in-modeline-flag t) ;后者被转移
  ;; 比较耗时的任务
  (setq semantic-idle-scheduler-work-idle-time 30)
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  ;; Idle Completion
  ; 以何种方式显示
  (setq semantic-complete-inline-analyzer-idle-displayor-class
;        'semantic-displayor-ghost ;inline
;        'semantic-displayor-tooltip ;tooltip
        'semantic-displayor-traditional ;separate window
        )
  ; 显示多少
  (setq semantic-displayor-tooltip-mode
        'standard ;initial-max-tags
;        'quiet ;只有当数量小于initial-max-tags时才显示
;        'verbose ;显示所有，貌似有bug，慎用
        )
  (setq semantic-displayor-tooltip-initial-max-tags 8)

  ;-------------------------------------------------------------------------
  ; SemanticDB
  ;-------------------------------------------------------------------------
  ;; 数据库文件保存设置
;  (setq semanticdb-default-save-directory nil) ;作为缺省路径，仅主动生成的数据库的文件才会保存于此
;  (setq semanticdb-default-file-name "")
  (setq semanticdb-persistent-path '(always)) ;'project为由EDE所管理的项目的路径下
  ;; 优化SemanticDB的搜索/parse
  ;; 1) 限定搜索范围
  (mapc (lambda (mode)
          (setq-mode-local mode semanticdb-find-default-throttle
                           '(
                             file
                             local
                             project
                             system
                             recursive
                             unloaded ;若搜索到的文件的SemanticDB没有导入/生成，则导入/生成之
                             omniscience ;自己创建的数据库就属于此类
                             )))
        '(c-mode c++-mode))
  ;; 2) 设置上述限定范围中的project类型，主要交由EDE或JDE等组件控制
;  (add-hook semanticdb-project-predicate-functions ) ;此项交由EDE设置
;  (add-hook semanticdb-project-root-functions ) ;此项交由EDE设置
  ; 甚至可以具体指定一些项目的根目录，该变量也会被semantic-project-root-functions中注册的函数修改
;  (setq semanticdb-project-roots '())
  ;; 3) 设置上述限定范围中的system类型，即变量semantic-dependency-system-include-path
  ; 利用gcc的输出信息
  (when (executable-find "gcc")
    (require 'semantic/bovine/gcc)
    (semantic-gcc-setup)
    )
  ; 若要完全地自定义，则需先重置，再追加
;  (semantic-reset-system-include 'c-mode)
;  (semantic-reset-system-include 'c++-mode)
  (mapc (lambda (path)
          (semantic-add-system-include path 'c-mode)
          (semantic-add-system-include path 'c++-mode))
        '(;此处可以加入各种常用的第三方库文件路径
          "." "./include" "./inc" "./common" "./public"
          ".." "../include" "../inc" "../common" "../public"
;          "C:/MinGW/include"
;          "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
          ))
  ; 若Semantic仍不能正常解析某些符号，则需要进一步做如下指定
;  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("symbol" . "value"))
;  (add-to-list 'semantic-lex-c-preprocessor-symbol-file  "path/file")
;  (setq semantic-c-obey-conditional-section-parsing-flag nil)
  ;; 4) 事先主动地为某些常用目录生成数据库，以供复用
  (setq semanticdb-search-system-databases t)
  (setq my-semanticdb-list '())
  ; 若指定目录已存在数据库文件，则不会重复创建
  (mapc (lambda (path)
          (add-to-list 'my-semanticdb-list
                       (semanticdb-create-database semanticdb-new-database-class path)))
        '(
          "/usr/include"
          "/usr/local/include"
;          "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
          ))
  ; 随后每次启动时加载之前已创建的数据库
  (mapc (lambda (mode)
          (setq-mode-local mode semanticdb-project-system-databases my-semanticdb-list))
        '(c-mode c++-mode))
  ;; 5) 修改SemanticDB后台支持，目前Emacs内置版暂只可依赖于以下两种(独立版还支持使用Cscope)
  ;; (a) Ebrowse
  ;; (require 'semantic/db-ebrowse)
  ;; 作为默认的选择，性能较差
  ;; (b) GNU Global
  ;; (require 'semantic/db-global)
  (require 'cedet-global)
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path (concat my-emacs-exec-bin-path "global/bin"))
    )
  (when (and (executable-find "global")
             (cedet-gnu-global-version-check t))
    (require 'semantic/db-global)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode)
    )

  ;-------------------------------------------------------------------------
  ; EDE
  ;-------------------------------------------------------------------------
  (require 'ede)
  (global-ede-mode 1) ;配合semantic-mode全局性地启用
  ;; EDE默认使用Unix上的Locate命令来定位文件，此外还支持使用GNU Global
  ;; 但目前Emacs内置的CEDET中删除了ede-locate.el文件，因此也就不支持修改了
;  (setq ede-locate-setup-options '(ede-locate-global ede-locate-base))
  ; 具体项目的EDE信息由prog-cc-ede.emacs配置文件独立地维护
  (load-file (concat my-emacs-config-file-path "prog-cc-ede.el"))

  ;-------------------------------------------------------------------------
  ; Display and Decoration
  ;-------------------------------------------------------------------------
  (setq semantic-stickyfunc-sticky-classes '(
                                             function
                                             type
;                                             variable
;                                             include
;                                             package
                                             ))
  (setq semantic-decoration-styles '(("semantic-tag-boundary" . t)
                                     ("semantic-decoration-on-private-members" . nil)
                                     ("semantic-decoration-on-protected-members" . nil)
                                     ("semantic-decoration-on-includes" . nil)))

  ;-------------------------------------------------------------------------
  ; Speedbar
  ;-------------------------------------------------------------------------
  ;; 此处为CEDET中集成的Speedbar，目前暂未启用
;  (require 'semantic/sb)

  ;-------------------------------------------------------------------------
  ; 代码浏览的相关功能设置(待完善)
  ;-------------------------------------------------------------------------
  (require 'semantic/ia)
  (define-key semantic-mode-map "" 'semantic-ia-fast-jump)
  (define-key semantic-mode-map "" 'semantic-complete-jump)
  (define-key semantic-mode-map "" 'semantic-complete-jump-local)
  (define-key semantic-mode-map "" 'semantic-complete-jump-local-members)
  (define-key semantic-mode-map "" 'semantic-decoration-include-visit) ;jump to include file
  (define-key semantic-mode-map "" 'semantic-mrub-switch-tag)
  (require 'semantic/symref)
  (define-key semantic-mode-map "" 'semantic-symref)
  (define-key semantic-mode-map "" 'semantic-symref-symbol)
  (require 'semantic/senator)
  (define-key semantic-mode-map "" 'senator-next-tag)
  (define-key semantic-mode-map "" 'senator-previous-tag)
  (define-key semantic-mode-map "" 'senator-go-to-up-reference)
  ) ;end of my-plugin-cedet-init()

(defun my-plugin-cedet-start ()
  (when (boundp 'ac-sources)
    (setq ac-sources
          (append my-prog-ac-sources '(ac-source-semantic)))
    (when (and (executable-find "global")
               (cedet-gnu-global-version-check t))
      (add-to-list 'ac-sources 'ac-source-gtags))
    )
  ) ;end of my-plugin-cedet-start()


;===========================================================================
; ECB (Emacs Code Browser)
;===========================================================================
; 将源代码下载并解压缩至load-path下，即可使用，以下编译过程可提高执行性能(可选)
; 1) 修改或仿照make.bat文件中的内容
; 首先ECB目录下创建ecb-compile-script-init文件，并写入下述脚本
; (add-to-list 'load-path "E:/.emacs.d/site-lisp/ecb") ;ECB所在目录
; (add-to-list 'load-path "D:/softwares/Emacs/lisp/cedet") ;CEDET所在目录
; (load-file "D:/softwares/Emacs/lisp/cedet/cedet.el") ;加载CEDET核心文件
; (require 'ecb)
; (setq debug-on-error t)
; 最终执行以下shell命令即可完成ECB的编译工作
; [$] cd E:/.emacs.d/site-lisp/ecb
; [$] emacs -Q -l ecb-compile-script-init --eval "(ecb-byte-compile t)"
; 忽视编译过程中的所有warning，编译完成后可删除ecb-compile-script-init等文件
; 2) 在启动Emacs并require ECB后，执行ecb-byte-compile命令即可
;===========================================================================
(defun my-plugin-ecb-init ()
  (save-excursion
    (add-to-list 'load-path (concat my-emacs-plugin-load-path "ecb"))
    (when (require 'ecb nil t)
      (unless (boundp 'stack-trace-on-error)
        (defvar stack-trace-on-error nil)) ;兼容性
      (setq ecb-layout-name "left15"
;            ecb-toggle-layout-sequence '()
;            ecb-layout-window-sizes nil ;推荐通过调用ecb-change-layout命令，以交互式的方式修改
            ecb-windows-width 0.2
            ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
            ecb-tip-of-the-day nil
;            ecb-auto-compatibility-check nil
            )
      ;; directories window
      (setq ecb-source-path '("~"))
      (setq ecb-tree-buffer-style 'image)
      (setq ecb-auto-expand-directory-tree 'best)
      (setq ecb-excluded-directories-regexps '("^\\(\\.\\|\\.\\.\\)$"))
      (setq ecb-show-sources-in-directories-buffer '("left15"))
      ;; sources window
;      (setq ecb-source-file-regexps '())
;      (setq ecb-sources-exclude-cvsignore '())
      ;; methods window
      (setq ecb-process-non-semantic-files nil) ;禁用non-semantic-sources
      ;; history window
;      (setq ecb-history-exclude-file-regexps '())
      ;; compilation window
      (setq ecb-compile-window-height 0.2
            ecb-compile-window-width 'edit-window
            ecb-compile-window-temporally-enlarge 'both
            ecb-enlarged-compilation-window-max-height 0.5
            )
      (setq ecb-compilation-buffer-names ;以下名称的buffer内容将被呈现于该窗口
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
                                                   ("*Directory"))))
      (setq ecb-compilation-major-modes ;以下模式的buffer内容将被呈现于该窗口
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
      (ecb-minor-mode 1) ;global minor mode
      ))
  ) ;end of my-plugin-ecb-init()

(defun my-plugin-ecb-start ()
  ) ;end of my-plugin-ecb-start()


;=========================================================================
; Source Code Tagging System
;=========================================================================
; http://stackoverflow.com/questions/12922526/tags-for-emacs-relationship-between-etags-ebrowse-cscope-gnu-global-and-exhu
; 开源的主要有以下几种：
; 1) Etags
; 功能最为简单，较为少用
; 2) Ctags
; 能够支持多达41种语言，相比于Etags会生成更多的metadata
; 官方主要支持于VIM，对于Emacs而言，由于其无法使用这些额外的metadata，因此功能几乎等同于Etags
; 3) Cscope
; 对于C/C++和Java而言异常强大，但对其他语言支持不足，且自带用户界面
; 4) GNU Global，Gtags
; 功能类似于Cscope，优点是实现上独立与任何编辑环境，也因此可集成于绝大多数编辑环境
;===========================================================================
; GNU Global
;===========================================================================
; version: 6.4
; http://www.gnu.org/software/global/
; http://www.tamacom.com/global.html
; 在Emacs环境中，可独立使用，也可作为以下组件的后台支持：
; 1)CEDET/Semantic/SemanticDB
; 2)CEDET/Semantic/Symref
; 3)CEDET/EDE/Locate
; 推荐配合Emacs插件ggtags来使用GNU Global
;===========================================================================
; ggtags
;===========================================================================
; version: 0.8.10
; http://elpa.gnu.org/packages/ggtags.html
; https://github.com/leoliu/ggtags
;===========================================================================
(defun my-plugin-ggtags-init ()
  (when (require 'ggtags nil t)
    )
  ) ;end of my-plugin-ggtags-init()
(defun my-plugin-ggtags-start ()
  (ggtags-mode 1) ;local minor mode
  ) ;end of my-plugin-ggtags-start()


;===========================================================================
;===========================================================================
(defun my-cc-mode-init ()
  (my-cc-style-init)
  ;; 所有依赖于CEDET的插件都必须在CEDET之后被加载/启用
  ;; 否则其会自动加载/启用CEDET，导致上述对于CEDET的设置失效
  ;; 所有与CEDET相互关联的插件的加载/启动顺序将在此被显示地指定
  ;; 而不是也不应依赖于hook的执行顺序
  (my-plugin-cedet-init)
  (my-plugin-ecb-init)
  )
(defun my-cc-mode-start ()
  (my-cc-style-start)
  (when (fboundp 'semantic-mode)
    (my-plugin-cedet-start)
    (my-plugin-ecb-start))
  )
(eval-after-load 'cc-mode ;/lisp/progmodes/cc-mode.el
  '(progn
     (add-hook 'c-initialization-hook 'my-cc-mode-init)
     (add-hook 'c-mode-hook 'my-cc-mode-start)
     (add-hook 'c++-mode-hook 'my-cc-mode-start)
     ))


;===========================================================================
; JDEE (Java Development Environment for Emacs)
;===========================================================================
; version: 2.4.2
; http://jdee.sourceforge.net/
; https://github.com/emacsmirror/jdee
;===========================================================================
(defun my-plugin-jdee-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "jdee/lisp"))
  (load "jde")
  ;; 配置JDK环境
  (setq jde-jdk-environment-variable nil)
  (setq jde-jdk-registry '(("1.8.0" . "C:/Program Files/Java/jdk1.8.0_40"))) ;可以注册多个JDK版本
  (setq jde-jdk '("1.8.0")) ;指定当前使用的JDK版本
  ) ;end of my-pluign-jdee-init()

(defun my-plugin-jdee-start ()
  (when (boundp 'ac-sources)
    (setq ac-sources
          (append my-prog-ac-sources '(ac-source-eclim)))
    )
  ) ;end of my-plugin-jdee-start()

(provide 'my-prog-cc)
