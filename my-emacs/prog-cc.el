;; -*- coding: utf-8 -*-

(require 'my-prog)

(defvar my-prog-cc-mode-start-hook '())

(when nil
  (add-hook 'after-save-hook
            (lambda ()
              (when (and (or (eq major-mode 'c-mode)
                             (eq major-mode 'c++-mode))
                         (executable-find "uncrustify"))
                (auto-revert-notify-rm-watch)
                (shell-command (concat "uncrustify -l C -c ~/.uncrustify/alps.cfg --no-backup "
                                       buffer-file-name))
                (auto-revert-notify-add-watch)))))

;; =============================================================================
;; Style
;; -----------------------------------------------------------------------------
(defun my-cc-style-init ()
  (load-file (concat my-user-emacs-directory "prog-cc-style.el"))
  (define-key c-mode-base-map "\C-m" 'c-context-line-break) ;; 换行后自动缩进
  )

(defun my-cc-style-start ()
  (c-set-style "my-c-style") ;; 也可以通过(setq c-default-style)实现
  (c-toggle-syntactic-indentation 1) ;; 启用根据语法缩进，否则任何基于语法的style都将失效
  (c-toggle-auto-newline 1) ;; 启用auto newline
  (c-toggle-electric-state 1) ;; 按下某些符号如semicolon后自动格式化当前行
  ;; (read-only-mode 1) ;; 不推荐在这里启用只读模式，会与其他插件冲突
  )

;; =============================================================================
;; CEDET
;; 以下设置仅针对于C和C++
(defun my-plugin-cedet-init()
  (use-package cedet
    :commands (semantic-mode semantic-toggle-minor-mode-globally)
    :init
    (setq semantic-default-submodes '(;; Idle Scheduler
                                      global-semantic-idle-scheduler-mode
                                      global-semantic-idle-summary-mode ;; 基于Smart Summary
                                      global-semantic-idle-local-symbol-highlight-mode
                                      ;; global-semantic-idle-completions-mode ;; 基于Smart Completion，用company替代
                                      ;; global-semantic-idle-breadcrumbs-mode
                                      ;; SemanticDB
                                      global-semanticdb-minor-mode
                                      ;; Display and Decoration
                                      global-semantic-stickyfunc-mode
                                      global-semantic-highlight-func-mode
                                      global-semantic-decoration-mode
                                      ;; Senator
                                      global-semantic-mru-bookmark-mode ;; mostly recently used
                                      ;; Debug
                                      ;; global-semantic-show-unmatched-syntax-mode
                                      global-semantic-show-parser-state-mode
                                      ;; global-semantic-highlight-edits-mode
                                      ;; 未知
                                      ;; global-cedet-m3-minor-mode
                                      )
          ;; 除了设置'semantic-default-submodes，还可调用以下函数来启用支持指定功能的子模块
          ;; (semantic-load-enable-minimum-features)
          ;; (semantic-load-enable-code-helpers)
          ;; (semantic-load-enable-guady-code-helpers)
          ;; (semantic-load-enable-excessive-code-helpers)
          ;; (semantic-load-enable-semantic-debugging-helpers)
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
          semantic-idle-scheduler-idle-time 1
          semantic-idle-scheduler-work-idle-time 30
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
          semanticdb-find-default-throttle '(file local project system recursive
                                                  unloaded ;; 若搜索到的文件的SemanticDB没有导入/生成，则导入/生成之
                                                  omniscience ;; 自己创建的数据库就属于此类
                                                  )
          ;; 可以预先主动地对某些目录生成数据库，以便今后复用
          semanticdb-search-system-databases t
          semanticdb-project-system-databases
          (let ((lst '()))
            (mapcar (lambda (path)
                      (add-to-list 'lst
                                   (semanticdb-create-database semanticdb-new-database-class path)
                                   t))
                    '("/usr/include" "/usr/local/include"
                      ;; "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
                      )))
          ;; -------------------------------------------------------------------
          semantic-stickyfunc-sticky-classes '(function type) ;; variable, include, package
          semantic-decoration-styles '(("semantic-tag-boundary" . t)
                                       ("semantic-decoration-on-private-members" . nil)
                                       ("semantic-decoration-on-protected-members" . nil)
                                       ("semantic-decoration-on-includes" . nil))
          ;; -------------------------------------------------------------------
          semantic-c-obey-conditional-section-parsing-flag t)
    (setq-default semantic-stickyfunc-sticky-classes semantic-stickyfunc-sticky-classes)
    (use-package cedet-global
      :commands (cedet-gnu-global-version-check))
    (add-hook 'my-prog-cc-mode-start-hook 'my-plugin-cedet-start t)
    :config
    ;; 设置'semanticdb-find-default-throttle中的'project，主要交由EDE或JDE等组件控制
    ;; (add-hook semanticdb-project-predicate-functions ) ;; 此项交由EDE设置
    ;; (add-hook semanticdb-project-root-functions ) ;; 此项交由EDE设置
    ;; 设置'semanticdb-find-default-throttle中的'system，可以利用编译器的已有配置
    ;; 甚至可以具体指定一些项目的根目录，该变量也会被semantic-project-root-functions中注册的函数修改
    ;; (setq semanticdb-project-roots '())
    (use-package semantic/bovine/gcc
      :if (executable-find "gcc")
      :config
      (semantic-gcc-setup))
    ;; 若要完全地自定义，则需先重置再追加，例如(semantic-reset-system-include 'c-mode)
    (semantic-add-system-include "/usr/include")
    (semantic-add-system-include "/usr/local/include")
    (semantic-add-system-include "/usr/include/boost" 'c++-mode)
    ;; 指定用于支持SemanticDB的tagging system，默认使用的是Ebrowse
    (use-package semantic/db-ebrowse ;; Ebrowse
      :disabled)
    (use-package semantic/db-global ;; GNU Global
      :if (cedet-gnu-global-version-check t)
      :config
      (semanticdb-enable-gnu-global-databases 'c-mode)
      (semanticdb-enable-gnu-global-databases 'c++-mode))
    ;; 以下是代码浏览功能的相关设置，待完善
    (use-package semantic/ia
      :disabled
      :config
      (bind-key :map semantic-mode-map
                ("" . semantic-ia-fast-jump)
                ("" . semantic-complete-jump)
                ("" . semantic-complete-jump-local)
                ("" . semantic-complete-jump-local-members)
                ("" . semantic-decoration-include-visit) ;; jump to include file
                ("" . semantic-mrub-switch-tag)))
    (use-package semantic/symref
      :disabled
      :config
      (bind-key :map semantic-mode-map
                ("" . semantic-symref)
                ("" . semantic-symref-symbol)))
    (use-package semantic/senator
      :disabled
      :config
      (bind-key :map semantic-mode-map
                ("" . senator-next-tag)
                ("" . senator-previous-tag)
                ("" . senator-jump)
                ("" . senator-go-to-up-reference)))
    ;; 若Semantic始终不能正常解析某些特定的符号，则作如下设置
    ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("symbol" . "value"))
    ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file  "path/file")
    (use-package semantic/sb ;; 此为CEDET中内置的Speedbar，不启用
      :disabled)
    (use-package ede
      :disabled
      :config
      (global-ede-mode 1) ;; 配合semantic-mode全局性地启用
      ;; EDE默认使用Unix上的Locate命令来定位文件，此外还支持使用GNU Global
      ;; 但目前Emacs内置的CEDET中删除了ede-locate.el文件，因此也就不支持修改了
      ;; (setq ede-locate-setup-options '(ede-locate-global ede-locate-base))
      ;; 具体项目的EDE信息由prog-cc-ede.emacs配置文件独立地维护
      (load-file (concat my-user-emacs-directory "prog-cc-ede.el")))))

(defun my-plugin-cedet-start ()
  (semantic-mode 1)
  (when (bound-and-true-p ac-sources)
    (add-to-list 'ac-sources (if (cedet-gnu-global-version-check t)
                                 'ac-source-gtags 'ac-source-semantic) t)))

;; =============================================================================
;; ECB (Emacs Code Browser)
;; 将源代码下载并解压缩至load-path下，即可使用，以下编译过程可提高执行性能(可选)
;; 1) 修改或仿照make.bat文件中的内容
;; 首先ECB目录下创建ecb-compile-script-init文件，并写入下述脚本
;; (add-to-list 'load-path "E:/.emacs.d/site-lisp/ecb") ;ECB所在目录
;; (add-to-list 'load-path "D:/softwares/Emacs/lisp/cedet") ;CEDET所在目录
;; (load-file "D:/softwares/Emacs/lisp/cedet/cedet.el") ;加载CEDET核心文件
;; (require 'ecb)
;; (setq debug-on-error t)
;; 最终执行以下shell命令即可完成ECB的编译工作
;; [$] cd E:/.emacs.d/site-lisp/ecb
;; [$] emacs -Q -l ecb-compile-script-init --eval "(ecb-byte-compile t)"
;; 忽视编译过程中的所有warning，编译完成后可删除ecb-compile-script-init等文件
;; 2) 在启动Emacs并require ECB后，执行ecb-byte-compile命令即可
;; -----------------------------------------------------------------------------
(defun my-plugin-ecb-init ()
  (save-excursion
    (add-to-list 'load-path (concat my-emacs-plugin-load-path "ecb"))
    (when (require 'ecb nil t)
      (unless (boundp 'stack-trace-on-error)
        (defvar stack-trace-on-error nil)) ;; 兼容性
      (setq ecb-layout-name "left15"
            ;; ecb-toggle-layout-sequence '()
            ;; ecb-layout-window-sizes nil ;; 推荐通过调用ecb-change-layout命令，以交互式的方式修改
            ecb-windows-width 0.2
            ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
            ecb-tip-of-the-day nil
            ;; ecb-auto-compatibility-check nil
            )
      ;; directories window
      (setq ecb-source-path '("~"))
      (setq ecb-tree-buffer-style 'image)
      (setq ecb-auto-expand-directory-tree 'best)
      (setq ecb-excluded-directories-regexps '("^\\(\\.\\|\\.\\.\\)$"))
      (setq ecb-show-sources-in-directories-buffer '("left15"))
      ;; sources window
      ;; (setq ecb-source-file-regexps '())
      ;; (setq ecb-sources-exclude-cvsignore '())
      ;; methods window
      (setq ecb-process-non-semantic-files nil) ;; 禁用non-semantic-sources
      ;; history window
      ;; (setq ecb-history-exclude-file-regexps '())
      ;; compilation window
      (setq ecb-compile-window-height 0.2
            ecb-compile-window-width 'edit-window
            ecb-compile-window-temporally-enlarge 'both
            ecb-enlarged-compilation-window-max-height 0.5
            )
      (setq ecb-compilation-buffer-names ;; 以下名称的buffer内容将被呈现于该窗口
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
      (setq ecb-compilation-major-modes ;; 以下模式的buffer内容将被呈现于该窗口
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
;; 插件ggtags和helm-gtags都是对于GNU Global的支持，且两者相互独立，实现上互不依赖
(defun my-plugin-helm-gtags-init ()
  (with-eval-after-load 'helm
    (use-package helm-gtags
      :if (and (my-func-package-enabled-p 'helm-gtags)
               (executable-find "gtags"))
      :commands (helm-gtags-mode)
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
            helm-gtags-prefix-key (kbd "C-c c")
            ;; 启用以下配置项会使得某些常用快捷键不再绑定于上述前缀中
            helm-gtags-suggested-key-mapping t)
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (when (derived-mode-p 'c-mode 'c++-mode)
                    (my-plugin-helm-gtags-start)))
                t)
      (add-hook 'dired-mode-hook 'my-plugin-helm-gtags-start t)
      (add-hook 'eshell-mode-hook 'my-plugin-helm-gtags-start t)
      :config
      (bind-keys :map helm-gtags-mode-map ;; 以下仅供参考
                 ("M-." . helm-gtags-dwim)
                 ("M-," . helm-gtags-pop-stack)
                 ("C-j" . helm-gtags-select)

                 ("C-c c s" . helm-gtags-find-symbol)
                 ("C-c c r" . helm-gtags-find-rtag)
                 ("C-c c a" . helm-gtags-tags-in-this-function)


                 helm-gtags-find-files
                 helm-gtags-show-stack


                 ("C-c <" . helm-gtags-previous-history)
                 ("C-c >" . helm-gtags-next-history)))))

(defun my-plugin-helm-gtags-start ()
  (helm-gtags-mode 1))

;; =============================================================================
;; ggtags
(defun my-plugin-ggtags-init ()
  (use-package ggtags
    :disabled
    :commands (ggtags-mode)))

(defun my-plugin-ggtags-start ()
  (ggtags-mode 1))

;; =============================================================================
;; =============================================================================
(defun my-prog-cc-mode-init ()
  (my-cc-style-init)
  ;; 所有依赖于CEDET的插件都必须在CEDET之后被加载/启用
  ;; 否则其会自动加载/启用CEDET，导致上述对于CEDET的设置失效
  ;; 所有与CEDET相互关联的插件的加载/启动顺序将在此被显示地指定
  ;; 而不是也不应依赖于hook的执行顺序
  (my-plugin-cedet-init)
  (my-plugin-ecb-init)
  (my-plugin-helm-gtags-init)
  (my-plugin-ggtags-init)
  (add-hook 'c-mode-hook 'my-prog-cc-mode-start)
  (add-hook 'c++-mode-hook 'my-prog-cc-mode-start))

(defun my-prog-cc-mode-start ()
  (run-hooks 'my-prog-cc-mode-start-hook))

(eval-after-load 'cc-mode '(add-hook 'c-initialization-hook 'my-prog-cc-mode-init))

(provide 'my-prog-cc)
