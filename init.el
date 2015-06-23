(provide 'my-init)
;===========================================================================
;; Emacs initialization file
; ~/.emacs > ~/.emacs.el > ~/.emacs.d/init.el
;1)加载插件的方式
;load：每次调用时都会（重新）加载
;require：若未加载，则加载之，而不会重新加载
;autoload：使用时才require
;eval-after-load：在一次load操作之后且在执行该loaded file/feature中的脚本之前
;例如：eval-after-load中还"来得及"注册某个模式的hook
;其第二个参数必须为quote form，否则的话会在作为传参时被立即计算
;2)目前所采取的策略
;在Emacs启动时load所有配置文件
;配置文件中的函数定义在load时一同evaluate
;插件的初始化工作使用eval-after-load，或注册于诸如c-initialization-hook的hook中，随对应模式的首次启动而执行其初始化工作
;全局性插件的启动紧随其自身的初始化完成之后，局部性插件的启动则随对应模式的启动


;===========================================================================
; Windows
;===========================================================================
; 1）在C:\Users\用户名\AppData\Roaming\.emacs文件中指示导入本文件
;(load-file "E:/home/wm/.emacs.d/init.el")
; 2）以下是本文件的内容
; 设置Emacs内部的环境变量，此方式需要每次在Emacs启动时被重新设置，不会保存
; 也可以设置Windows系统的环境变量，但并不推荐，因为会影响到其他同样依赖于该变量的软件
(when (eq system-type 'windows-nt)
  (setenv "HOME" "E:/")
  ; 设置默认工作目录
  (setq default-directory "~/")
  ; 设置shell，使用由Emacs提供的cmdproxy.exe
  (setq shell-file-name "D:/softwares/programming/emacs/bin/cmdproxy.exe")
  (setq shell-command-switch "-c")
  )


;===========================================================================
; Execution Path
;===========================================================================
;; 配置Emacs的exec-path就无需修改操作系统的环境变量了
;; 当然也可以设置(setenv "PATH" "")，效果是一样的
(when (eq system-type 'windows-nt)
  (setq my-emacs-exec-bin-path "D:/softwares/")
  (add-to-list 'exec-path my-emacs-exec-bin-path)
  )


;===========================================================================
; 下述内容会因用户在Customize界面的保存操作而由Emacs自动写入
;===========================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "Chinese-GB")
 '(tool-bar-mode nil) ;取消工具栏
 '(electric-indent-mode nil) ;取消全局性的自动缩进
 '(global-font-lock-mode nil) ;取消全局性的语法高亮
 '(ido-everywhere nil) ;仅使ido补全find-file和switch-to-buffer
 '(ido-enable-flex-matching nil)
 '(ecb-options-version "2.40") ;ecb-minor-mode
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-changes ((t (:background "#CCFFCC"))))
 '(highlight-changes-delete ((t (:background "#CCFFCC"))))
 )


;===========================================================================
; 配置杂项
;===========================================================================
;; Account
(setq user-full-name "Tony")
(setq user-mail-address "")

;; UI
;(setq inhibit-startup-message 1) ;取消启动界面
(setq frame-title-format "emacs@%b") ;设置标题栏显示为buffer名字
;; 关于smooth scrolling可以参考
;; http://www.emacswiki.org/emacs/SmoothScrolling
;; 其提供的主要解决方案是基于两个插件：smooth-scroll.el和smooth-scrolling.el
;; 这里暂不使用平滑滚动，而是通过设置以下变量以尽可能地避免页面滚动时画面的频繁跳动
(setq redisplay-dont-pause t
      ;; mouse wheel scrolling
      mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      ;; keyboard scrolling
      scroll-margin 1
      scroll-step 3
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      )
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)
(mouse-avoidance-mode 'animate) ;当光标移动至鼠标位置时，为避免遮挡视线，自动移开鼠标
(setq-default line-spacing 0) ;行距
(column-number-mode 1) ;在mode-line显示列数
(set-face-background 'default "#C7EDCC") ;设置背景颜色
(set-face-attribute 'default nil :family "Monaco" :weight 'semi-bold) ;设置字体
(set-frame-font "11" nil t) ;设置字号
(show-paren-mode 1) ;左右括号相匹配显示
(setq show-paren-style 'parentheses)
(setq debug-on-error t) ;显示错误信息
(fset 'yes-or-no-p 'y-or-n-p) ;以y/n替换yes/no
(setq visible-bell t) ;以窗口闪烁的方式代替错误提示音
(global-visual-line-mode t)

;; Edit
(setq-default indent-tabs-mode nil) ;make indentation commands use space only
(setq-default tab-width 4)
;(setq tab-always-indent t)
(global-highlight-changes-mode 1)
(setq highlight-changes-global-changes-existing-buffers nil)
(setq highlight-changes-visibility-initial-state t)
(setq highlight-changes-face-list nil)
(setq highlight-changes-colors nil)
; 每次保存buffer时都将删除现有的改动高亮
; 替换成这两个hook就会无效，原因未知：write-content-functions或write-file-functions
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;删除每行末尾的空格
            (highlight-changes-remove-highlight (point-min) (point-max))
            ))

;; Backup and Revert
(setq make-backup-files t) ;启用自动备份
(setq version-control t) ;启用版本控制，即可以备份多次
(setq kept-old-versions 1) ;备份最旧的版本个数
(setq kept-new-versions 1) ;备份最新的版本个数
(setq delete-old-versions t)
(setq dired-kept-versions 2)
(setq backup-directory-alist '(("." . "~/.emacs.d.backups"))) ;设置备份文件的路径
(setq backup-by-copying t) ;备份设置方式为直接拷贝
(global-auto-revert-mode t) ;当硬盘上的文件被修改后，Emacs会提示用户重新读取该文件

;; File Extension
;(setq auto-mode-alist
;      (cons '("\\.emacs\\'" . emacs-lisp-mode) auto-mode-alist))

;; Key
;(global-set-key "\C-c s" 'func)
;(define-key lisp-mode (kbd "C-c ;") 'func)

;; Mode
;; 以下两个都可以在minibuff中提供补全功能，且支持同样多的命令
;; 但前者需要TAB键触发，而后者自动呈现，且默认仅支持find-file和switch-to-buffer
(icomplete-mode t)
(ido-mode t)
;(uniquify-mode 1) ;buffer命名
;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil) ;不使用image方式
(setq speedbar-show-unknown-files t)


;===========================================================================
; 加载全局性的第三方插件
;===========================================================================
;; 第三方插件资源：
;; https://github.com/emacsmirror

(setq my-emacs-config-file-path "~/.emacs.d/my-emacs/")
(setq my-emacs-plugin-load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path my-emacs-plugin-load-path)

;===========================================================================
; Color Theme
;===========================================================================
; http://www.nongnu.org/color-theme/
; 该插件已集成于Emacs24以上版本中，作为默认的主题管理组件
; 其自带了一些主题，网上还有许多基于该插件独立发布的主题
;===========================================================================
(setq my-emacs-theme-load-path (concat my-emacs-plugin-load-path "color-theme"))
(add-to-list 'custom-theme-load-path my-emacs-theme-load-path)
(add-to-list 'load-path my-emacs-theme-load-path)
(setq my-emacs-enabled-theme-name nil)
;---------------------------------------------------------------------------
; [theme] Solarized
;---------------------------------------------------------------------------
; http://ethanschoonover.com/solarized
; https://github.com/altercation/solarized
; https://github.com/sellout/emacs-color-theme-solarized
; 下载下来后只需保留三个.el脚本文件即可：
; color-theme-solarized.el, solarized-theme.el, solarized-definitions.el
;===========================================================================
(when (equal my-emacs-enabled-theme-name "solarized")
  ;; 包含有2种模式
  (let ((mode
         'dark
;         'light
         ))
    (load-theme 'solarized t)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (set-frame-parameter frame 'background-mode mode)
                (set-terminal-parameter frame 'background-mode mode)
                (enable-theme 'solarized)))))
;---------------------------------------------------------------------------
; [theme] Tomorrow
;---------------------------------------------------------------------------
; https://github.com/chriskempson/tomorrow-theme
;===========================================================================
(when (equal my-emacs-enabled-theme-name "tomorrow")
  ;; 包含有5个主题
  (let ((theme
         'tomorrow-day
;         'tomorrow-night
;         'tomorrow-night-blue
;         'tomorrow-night-bright
;         'tomorrow-night-eighties
         ))
    (load-theme theme t)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (enable-theme theme)))))


;===========================================================================
; 加载其他配置文件
;===========================================================================
(mapc (lambda (name)
        (load (concat my-emacs-config-file-path name) t nil nil t))
      '(
        "prog" ;prog-mode
        "prog-cc" ;cc-mode (c-mode, c++-mode, java-mode)
        "prog-lisp" ;lisp-mode, emacs-lisp-mode, lisp-interaction-mode
        "prog-py" ;python-mode
        "text-tex" ;tex-mode, latex-mode
        "web-browser" ;web browser
        ))
