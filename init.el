(when (eq system-type 'windows-nt)
;  (setenv "HOME" "E:/")
  ; 设置默认工作目录
  (setq default-directory "~/")
  ; 设置shell，使用由Emacs提供的cmdproxy.exe
  (setq my-shell-file-name "D:/softwares/programming/emacs-24.5/libexec/emacs/24.5/i686-pc-mingw32/cmdproxy.exe")
  (when (executable-find my-shell-file-name)
    (setq shell-file-name my-shell-file-name)
    (setq shell-command-switch "-c"))
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
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-changes ((t (:background "#CCFFCC"))))
 '(highlight-changes-delete ((t (:background "#CCFFCC")))))


;===========================================================================
; 配置杂项
;===========================================================================
;; Account
(setq user-full-name "TonyLYan")
(setq user-mail-address "wangtonylyan@outlook.com")

;; UI
;(setq inhibit-startup-message 1) ;取消启动界面
(tool-bar-mode -1) ;取消工具栏
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
(scroll-bar-mode -1) ;取消滚动条
(mouse-avoidance-mode 'animate) ;当光标移动至鼠标位置时，为避免遮挡视线，自动移开鼠标
(setq-default line-spacing 0) ;行距
(column-number-mode 1) ;在mode-line显示列数
(set-face-background 'default "#C7EDCC") ;设置背景颜色

;字体的名字源自于.ttf或.otf文件内自带的元信息，包括family和style等
;以下使用不同的中英文字体和字号的目的是为了有助于美观，例如同一字体下的中文字符通常都比英文字符更高
(if (eq system-type 'windows-nt)
    (progn
      (if (> emacs-major-version 24)
          (progn ;Windows系统上的Emacs25.1版本对中文字体的显示存在问题，打开中文文档时会存在卡顿的现象，必须手动指定中文字体为宋体才可避免。
            (set-default-font "Consolas 11")
            (set-fontset-font "fontset-default" 'unicode "宋体 10"))
        (progn
          (set-default-font "Consolas 11")
          (set-fontset-font "fontset-default" 'unicode "Microsoft YaHei Mono 10"))))
  (progn
    (set-default-font "YaHei Consolas Hybrid 10")
    (set-fontset-font "fontset-default" 'unicode "Source Han Serif SC SemiBold 9") ;或替换成"Microsoft YaHei Mono 10"
    ))
;(set-face-attribute 'default nil :family "Microsoft YaHei Mono" :weight 'normal :height 110) ;设置字体，包括字号等
;(set-frame-font "10" nil t) ;设置字号, 同(set-face-attribute)中的:height

(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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
(electric-indent-mode -1) ;取消全局性的自动缩进模式
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
;(global-font-lock-mode -1) ;取消全局性的语法高亮模式
;(add-hook 'org-mode-hook 'turn-on-font-lock) ;可以通过注册hook的方式在特定模式下启用

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
(global-set-key (kbd "C-x a") 'mark-whole-buffer)
(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x l") 'windmove-right)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-up>") 'text-scale-increase)
(global-set-key (kbd "<C-down>") 'text-scale-decrease)
(global-set-key (kbd "C-x C--") 'downcase-region)
(global-set-key (kbd "C-x C-=") 'upcase-region)
(global-unset-key (kbd "C-x C-l")) ;(downcase-region)
(global-unset-key (kbd "C-x C-u")) ;(upcase-region)
(put 'downcase-region 'disabled nil) ;去除每次执行此命令时的提示，强制执行
(put 'upcase-region 'disabled nil)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-unset-key (kbd "C-q"))

;; Mode
;; icomplete和ido这两个模式都能提供以下在minibuff中的补全功能
;; find-file, switch-to-buffer, execute-extended-command
;; 前者需要TAB键触发，而通常TAB键都会触发新建子窗口呈现补全，所以一般不用此模式
;; 后者总是自动呈现，但缺省仅支持前两种补全功能
;; 插件Smex的实现就是基于后者对于execute-extended-command的支持
(icomplete-mode -1)
(when (require 'ido nil t)
  (ido-mode t)
  (ido-everywhere -1) ;仅使ido支持find-file和switch-to-buffer
  (setq ido-enable-flex-matching nil)
  (setq ido-enable-prefix t)
  (setq ido-enter-matching-directory nil))
;(uniquify-mode 1) ;buffer命名
;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil) ;不使用image方式
(setq speedbar-show-unknown-files t)


;===========================================================================
; 管理和加载全局性的第三方插件
;===========================================================================
; 1) ELPA (Emacs Lisp Package Archive)
; 可以通过Emacs24以上版本内置的ELPA工具来安装和管理第三方插件
;---------------------------------------------------------------------------
;(setq url-proxy-services '(("http" . "10.25.71.1:8080")))  ;不支持authentication

(when (require 'package nil t)
  ; Emacs使用的默认更新源为：("gnu" . "http://elpa.gnu.org/")
  ; 添加更新源：MELPA每天更新，其包含了绝大多数插件
  ; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (setq package-load-list '(;以下插件的声明顺序需符合彼此之间的依赖关系
;                            (dash t) (epl t) (let-alist t) (pkg-info t) (flycheck t)
;                            all
                            (geiser t) (paredit t) (auctex t)
                            )) ;指定由以下方式所加载的插件
  (setq package-enable-at-startup nil) ;方式1) 随Emacs的启动而自动加载插件
  (package-initialize) ;方式2) 主动执行该函数以加载插件
  )
;---------------------------------------------------------------------------
; 2) 从网上下载、编译、安装第三方插件
; 资源网站:
; https://github.com/emacsmirror
;---------------------------------------------------------------------------
(setq my-emacs-plugin-load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path my-emacs-plugin-load-path)

; auctex
(when (and nil (require 'auctex nil t))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  )

;===========================================================================
; Smex
;===========================================================================
; https://github.com/nonsequitur/smex
; 其完全基于ido-mode实现，提供了对于execute-extended-command更为强大的支持
; 包括了记录用户操作历史、快速查询输入命令的快捷键及函数说明等功能
;===========================================================================
(when (require 'smex nil t)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-x M-x") 'execute-extended-command) ;原M-x快捷键能
  )

;===========================================================================
; Minimap
;===========================================================================
; https://github.com/dustinlacewell/emacs-minimap
; 提供一个类似于Sublime编辑器中的minimap功能
; 其全部的可配置选项见于(customize-group minimap)中
;===========================================================================
(when (and nil (require 'minimap nil t))
  (setq minimap-always-recenter nil) ;设置为nil才有效?
  (setq minimap-recenter-type 'middle)
  (setq minimap-buffer-name-prefix "MINI") ;不能为空，否则无法启动minimap窗口
  (setq minimap-hide-fringes t)
  (setq minimap-hide-scroll-bar t)
  (setq minimap-update-delay 1.0)
  (setq minimap-window-location 'right)
  (setq minimap-display-semantic-overlays nil)
  (setq minimap-enlarge-certain-faces nil)
  )

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
;---------------------------------------------------------------------------
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
;---------------------------------------------------------------------------
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
; Powerline
;===========================================================================
; https://github.com/jonathanchu/emacs-powerline
; 其提供了一个漂亮的mode line皮肤，缺点是当字体太多或字太多时，会显示不下所有内容
;===========================================================================
(setq my-emacs-if-enable-powerline nil)
(when (and my-emacs-if-enable-powerline
           (require 'powerline nil t))
  (setq powerline-arrow-shape
;        'arrow
;        'curve
        'arrow14
        ))

;===========================================================================
; Paredit
;===========================================================================
(when (require 'paredit nil t)
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  (add-hook 'org-mode 'enable-paredit-mode)
  )

;===========================================================================
; 加载其他配置文件
;===========================================================================
(provide 'my-init)
(setq my-emacs-config-file-path "~/.emacs.d/my-emacs/")
(mapc (lambda (name)
        (load (concat my-emacs-config-file-path name) t nil nil t))
      '(
        "prog" ;prog-mode
        "prog-cc" ;cc-mode (c-mode, c++-mode, java-mode)
        "prog-lisp" ;lisp-mode, emacs-lisp-mode, lisp-interaction-mode
        "prog-py" ;python-mode
        "prog-haskell" ;haskell-mode
        "text-tex" ;tex-mode, latex-mode
        "web-browser" ;web browser
        ))


;===========================================================================
; org-mode
;===========================================================================
;(add-to-list 'org-babel-load-languages '(sh . t))
;(add-to-list 'org-babel-load-languages '(ruby . t))
;(org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
(setq org-src-fontify-natively t)
(setq org-directory "~/.emacs.d/org/")
(setq org-default-notes-file (concat org-directory "default.org"))
(setq org-todo-keywords
      '(
        (sequence "NEW(n)" "TODO(t)" "DOING(i)" "PEND(p)" "|" "CANCEL(c)" "DONE(d)")
        (type "HOME(h)" "WORK(w)")
        ))
(setq org-todo-keyword-faces
      '(
        ("NEW" . (:background "orange" :foreground "black" :weight bold))
        ("TODO" . (:background "yellow" :foreground "black" :weight bold))
        ("DOING" . (:background "red" :foreground "black" :weight bold))
        ("PEND" . (:background "pink" :foreground "black" :weight bold))
        ("CANCEL" . (:background "lime green" :foreground "black" :weight bold))
        ("DONE" . (:background "green" :foreground "black" :weight bold))
        ))
(let ((my-org-file-task (concat org-directory "task.org")))
  (setq org-capture-templates
        `(
          ("t" "Templates for task")
          ("tn" "new" entry (file+datetree ,my-org-file-task) "* NEW %? %T %^G\n")
          ("tt" "todo" entry (file+datetree ,my-org-file-task) "* TODO %? %T %^G\n")
          ("ti" "doing" entry (file+datetree ,my-org-file-task) "* DOING %? %T %^G\n")
          ("tp" "pend" entry (file+datetree ,my-org-file-task) "* PEND %? %T %^G\n")
          ("tc" "cancel" entry (file+datetree ,my-org-file-task) "* CANCEL %? %T %^G\n")
          ("td" "done" entry (file+datetree ,my-org-file-task) "* DONE %? %T %^G\n")

          ("o" "Templates for note")
          ("oo" "basic" entry (file+datetree (concat org-directory "note.org")) "* NOTE %? %T %^G\n")
          ("c" "Templates for calendar")
          ("cc" "basic" entry (file+datetree (concat org-directory "task.org")) "* CALENDAR %? %T %^G\n")
          ("p" "Templates for project")
          ("pp" "basic" entry (file+datetree (concat org-directory "project.org")) "* PROJECT %? %T %^G\n")
          )))

(add-hook 'org-mode-hook
          (lambda ()
            (progn
              (setq truncate-lines nil)
              (org-indent-mode t))))
