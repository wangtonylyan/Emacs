(provide 'my-prog-lisp)
(require 'my-prog)
;===========================================================================
; Common Lisp Implementations
;===========================================================================
; 常用的几种Common Lisp方言的实现环境：
; 1) CLISP (ANSI Common Lisp)
; http://www.clisp.org/
; 目前已长期未更新
; 2) SBCL (Steel Bank Common Lisp)
; http://www.sbcl.org/
; 仍持续维护
; 3) CCL (Clozure Common Lisp)
; http://ccl.clozure.com/
; 4) ABCL (Armed Bear Common Lisp)
; https://common-lisp.net/project/armedbear/
; 运行在JVM上
; 此外还有很多，可以参考:
; http://tianchunbinghe.blog.163.com/blog/static/700120089175316746/
;===========================================================================


;===========================================================================
; Lispbox
;===========================================================================
; https://common-lisp.net/projects/lispbox/
; 作为一款IDE，以Emacs为平台，默认集成了SLIME、Quicklisp和CCL三大工具
; 下载并安装后即可使用，而此处仍选择自己安装和配置这些插件
;===========================================================================
(defun my-plugin-lispbox-init ()
  (when (eq system-type 'windows-nt)
    (setq my-lisp-interpreter-path (concat my-emacs-exec-bin-path "programming/SBCL"))
    (add-to-list 'exec-path my-lisp-interpreter-path)
    )
  (when (executable-find "sbcl")
    ;-----------------------------------------------------------------------
    ; SLIME (The Superior Lisp Interaction Mode for Emacs)
    ;-----------------------------------------------------------------------
    ; https://common-lisp.net/project/slime/
    ; https://github.com/slime/slime
    ; 该插件的作用是使用户可以在Emacs环境下使用任意Lisp实现环境，而非Emacs Lisp
    ; 其架构是将SMILE集成于Emacs中作为客户端呈现
    ; 而将Swank作为服务器端与具体的Lisp实现环境对接
    ; SMILE与Swank将以Socket方式相互通信
    ; 官方提供的SMILE手册中以SBCL为例，因此推荐新手使用SMILE+SBCL的组合
    ; SLIME有两种安装和使用方式：注意这些方式彼此冲突，只能选其一
    ;-----------------------------------------------------------------------
    ; 方式1)从网上下载SMILE源码或利用MELPA下载，并作为Emacs的插件被加载和运行
    (add-to-list 'load-path (concat my-emacs-plugin-load-path "slime"))
    (require 'slime)
    (require 'slime-autoloads)
    ;-----------------------------------------------------------------------
    ; 方式2)利用Quicklisp安装SMILE包，作为某个具体的Common Lisp实现的组件被加载和运行
    ; 基于Quicklisp所提供的quicklisp-slime-helper库，可参考：
    ; https://github.com/quicklisp/quicklisp-slime-helper
    ; 以SBCL为例（前提是已安装有Quicklisp），运行以下命令即可安装SLIME
    ; [SBCL]* (ql:quickload "quicklisp-slime-helper")
    ; 安装完成后会在Quicklisp安装目录下额外生成slime-helper.el文件
    ; 随后只需在Emacs中加载该文件即可启用SMILE
;    (setq my-emacs-quicklisp-install-path "C:/Users/WM/quicklisp/")
;    (load (concat my-emacs-quicklisp-install-path "slime-helper.el"))
    ; 注意此方式下对于SMILE的定制和启动完全在SBCL环境中，以下定制代码不适用！
    ;-----------------------------------------------------------------------
    ;; 以下是对于SMILE的定制
    ; 指定SLIME所依赖的Lisp实现环境
    (setq inferior-lisp-program
          (if (eq system-type 'windows-nt)
              (concat my-lisp-interpreter-path "sbcl")
            "sbcl"))
    ; 指定当前加载的contributed package
    (setq slime-contribs '(
                           slime-fancy
                           slime-scratch
                           slime-editing-commands
                           slime-repl
                           inferior-slime
                           slime-autodoc
                           ))
    (setq slime-description-autofocus nil)
    (slime-setup) ;使上述定制生效

    ;-----------------------------------------------------------------------
    ; Quicklisp
    ;-----------------------------------------------------------------------
    ; http://www.quicklisp.org/
    ; https://github.com/quicklisp
    ; Quicklisp是一个可以被安装在绝大多数Common Lisp实现环境中的库管理工具
    ; 以下是安装Quicklisp的命令，需要在某个具体的Common Lisp实现环境中执行
    ; 以SBCL为例: [shell]$ sbcl --load quicklisp.lisp
    ; [SBCL]* (quicklisp-quickstart:install) ;在SBCL环境中安装Quicklisp组件
    ; [SBCL]* (ql:system-apropos "vecto") ;查询vecto库相关信息
    ; [SBCL]* (ql:quickload "vecto") ;下载并安装vecto库
    ; [SBCL]* (ql:add-to-init-file) ;生成~/.sbclrc文件，以使SBCL在启动时自动启用Quicklisp
    ; 注意无论是否使用quick-slime-helper，都推荐安装Quicklisp作为Common Lisp开发环境中的库管理工具
    ;-----------------------------------------------------------------------
    )
  ) ;end of my-plugin-lispbox-init()

(defun my-plugin-lispbox-start ()
  (slime-mode 1) ;启用smile-mode
  (save-excursion (slime)) ;启动SBCL，并连接Swank
  (when (boundp 'ac-sources)
    (setq ac-sources
          (append my-prog-ac-sources '(ac-source-slime)))
    )
  ) ;end of my-plugin-lispbox-start()


;===========================================================================
;===========================================================================
(defun my-lisp-mode-init ()
  (my-plugin-lispbox-init)
  )
(defun my-lisp-mode-start ()
  (when (fboundp 'slime-mode)
    (my-plugin-lispbox-start)
    ))
(eval-after-load 'lisp-mode ;/lisp/emacs-lisp/lisp-mode.el
  '(progn
     (my-lisp-mode-init)
     (add-hook 'lisp-mode-hook 'my-lisp-mode-start)
     ))
;===========================================================================
;===========================================================================
(defun my-emacs-lisp-mode-init ()
  )
(defun my-emacs-lisp-mode-start ()
  (when (boundp 'ac-sources)
    (setq ac-sources
          (append my-prog-ac-sources '(ac-source-functions
                                       ac-source-variables
                                       ac-source-symbols
                                       ac-source-features)))
    ))
(eval-after-load 'lisp-mode ;/lisp/emacs-lisp/lisp-mode.el
  '(progn
     (my-emacs-lisp-mode-init)
     (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-start)
     ))
