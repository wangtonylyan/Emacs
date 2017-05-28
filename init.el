;; 判断Emacs版本可以基于以下两个变量：'emacs-major-version和'emacs-minor-version

(defun my-func-executable-find (dir exe &optional add)
  (let* ((dir (if (and (stringp dir) (string-blank-p dir))
                  ;; 统一传参的形式
                  (concat (directory-file-name dir) "/") ""))
         (path (executable-find (concat dir exe))))
    (when (file-executable-p path)
      (when add
        (add-to-list 'exec-path (directory-file-name path) t))
      path)))

(defalias 'my-func-package-enabled-p 'package--user-selected-p)

(defalias 'my-func-minor-mode-on-p 'bound-and-true-p)

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "D:/softwares" t)
  (let ((path (my-func-executable-find
               "Emacs25/libexec/emacs/24.5/i686-pc-mingw32"
               "cmdproxy.exe")))
    (when path
      (setq shell-file-name path
            shell-command-switch "-c"))))

;; (setq user-init-file "~/.emacs.d/init.el")
;; (load user-init-file)

;; 由于以下两个变量在当前文件被执行时就有buffer-local的初始值了
;; 因此为使其自此生效，就必须同时修改局部和全局值
(setq default-directory "~/"
      user-emacs-directory "~/.emacs.d/"
      command-line-default-directory default-directory)
(setq-default default-directory default-directory
              user-emacs-directory user-emacs-directory)
(defconst my-user-emacs-directory (concat user-emacs-directory "my-emacs/"))

;; (normal-top-level-add-subdirs-to-load-path)
;; (normal-top-level-add-to-load-path)

;; 指定由(customize)写入配置信息的文件，随后每当Emacs自动写入时就不会再修改当前文件了
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; (setq url-proxy-services '(("http" . "10.25.71.1:8080"))) ;; 不支持authentication
(when (require 'package nil t)
  ;; 设置安装包的存储目录，该目录也需要被包含至'load-path中
  ;; (add-to-list 'package-directory-list "~/.emacs.d/elpa" t) ;; system-wide dir
  (setq package-user-dir (concat user-emacs-directory "elpa")) ;; user-wide dir
  ;; Emacs使用的默认更新源为：("gnu" . "http://elpa.gnu.org/")
  ;; 添加更新源：MELPA每天更新，其包含了绝大多数插件
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  ;; 以下列表用于设置被允许加载的插件，因此无论是在安装还是使用插件的过程中
  ;; 都必须提前详细地列举出所有的插件，且要根据插件之间的依赖关系进行先后地声明
  (setq package-load-list '(all
                            ;; e.g.
                            ;; (dash) (epl) (let-alist) (pkg-info) (flycheck)
                            ;; (atom-one-dark-theme t) (material-theme t)
                            ))
  ;; 设置加载上述列表中所指定插件的时机
  (setq package-enable-at-startup nil) ;; 方式1) 随Emacs的启动而自动加载插件
  (package-initialize) ;; 方式2) 主动执行该函数以加载插件
  ;; 目前使用此全局变量来管理插件的启用/禁用，其中包括了ELPA更新源中所没有的插件
  (setq package-selected-packages '(atom-one-dark-theme
                                    powerline
                                    avy ;; ace-jump-mode
                                    ace-pinyin
                                    helm ;; icomplete, anything, ido, smex, ivy
                                    helm-gtags
                                    flyspell
                                    ;; flyspell-correct-helm
                                    flycheck ;; flymake
                                    helm-flycheck
                                    projectile
                                    helm-projectile
                                    yasnippet
                                    company ;; auto-complete
                                    company-jedi
                                    magit
                                    elpy ;; ropemacs
                                    py-autopep8
                                    auctex
                                    use-package))
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install-selected-packages))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

;; 指定第三方主题的安装目录
(let ((path (concat my-user-emacs-directory "theme")))
  (add-to-list 'load-path path t)
  (add-to-list 'custom-theme-load-path path t))
((lambda (theme)
   (cond
    ((string-equal "atom-one-dark" theme)
     (when (require 'atom-one-dark-theme nil t)
       (load-theme 'atom-one-dark t)))
    ((string-prefix-p "material" theme)
     (when (require 'material-theme nil t)
       (cond
        ((string-match-p "dark" theme)
         (load-theme 'material t))
        ((string-match-p "light" theme)
         (load-theme 'material-light t)))))
    ((string-prefix-p "solarized" theme)
     (when (require 'solarized nil t)
       (let ((mode (if (string-match-p "dark" theme) 'dark 'light)))
         (load-theme 'solarized t)
         (add-hook 'after-make-frame-functions
                   (lambda (frame)
                     (set-frame-parameter frame 'background-mode mode)
                     (set-terminal-parameter frame 'background-mode mode)
                     (enable-theme 'solarized))
                   t))))))
 "atom-one-dark")

(use-package powerline
  :if (my-func-package-enabled-p 'powerline)
  :config
  (setq powerline-arrow-shape 'arrow14)
  (powerline-default-theme))

(use-package minimap
  :if (my-func-package-enabled-p 'minimap)
  :config
  (setq minimap-always-recenter nil ;; 设置为nil才有效?
        minimap-recenter-type 'middle
        minimap-buffer-name-prefix "MINI" ;; 不能为空，否则无法启动minimap窗口
        minimap-hide-fringes t
        minimap-hide-scroll-bar t
        minimap-update-delay 1.0
        minimap-window-location 'right
        minimap-display-semantic-overlays nil
        minimap-enlarge-certain-faces nil))

(use-package flyspell
  :if (and (my-func-package-enabled-p 'flyspell)
           (executable-find "aspell"))
  :config
  (setq ispell-program-name (executable-find "aspell") ;; 设置后台支持程序
        ;; ispell-dictionary "english" ;; default dictionary
        ;; ispell-personal-dictionary ""
        flyspell-issue-message-flag nil))

(use-package paredit
  :if (my-func-package-enabled-p 'paredit)
  :config
  (mapc (lambda (hook)
          (add-hook hook 'enable-paredit-mode t))
        '(lisp-mode-hook
          emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          scheme-mode-hook
          org-mode)))

;; =============================================================================
;; 配置杂项
;; -----------------------------------------------------------------------------
(setq user-full-name "TonyLYan"
      user-mail-address "wangtonylyan@outlook.com"
      inhibit-startup-message 1 ;; 取消启动界面
      frame-title-format '(buffer-file-name "%f" ("%b")) ;; 设置标题栏显示为buffer名字
      uniquify-buffer-name-style 'post-forward-angle-brackets ;; 重名buffer的命名
      visible-bell t ;; 以窗口闪烁的方式代替错误提示音
      echo-keystrokes 0.1
      debug-on-error nil ;; 显示错误信息
      select-enable-clipboard t
      current-language-environment "Chinese-GB"
      auto-revert-use-notify t
      auto-revert-verbose nil
      auto-revert-stop-on-user-input t
      auto-revert-interval 5
      delete-by-moving-to-trash t
      make-backup-files t ;; 启用自动备份
      version-control t ;; 启用版本控制，即可以备份多次
      kept-old-versions 1 ;; 备份最旧的版本个数
      kept-new-versions 1 ;; 备份最新的版本个数
      delete-old-versions t
      dired-kept-versions 2
      backup-directory-alist '(("." . "~/.emacs.d.backups"))
      backup-by-copying t)
(setq-default buffer-file-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p) ;; 以y/n替换yes/no
(tool-bar-mode -1)
(menu-bar-mode -1)
(prefer-coding-system 'utf-8)
;; 该模式用于监控磁盘上的文件是否被外部程序修改，并提示用户或自动重新加载该文件
(global-auto-revert-mode 1)
(recentf-mode 1)
(auto-image-file-mode 1) ;; 允许打开图片
(auto-compression-mode 1) ;; 允许查看和写入压缩包

;; (set-face-background 'default "#C7EDCC") ;; 设置背景颜色为绿色护眼色
;; 字体的名字源自于.ttf或.otf文件内自带的元信息，包括family和style等
;; 以下使用不同的中英文字体和字号的目的是为了提升美观性，例如同一字体下的中文字符通常都比英文字符更高
(let* ((rslt (< (* (display-pixel-width) (display-pixel-height)) 2000000))
       (efont (if rslt 10 11))
       (cfont (if rslt 9 10))
       (fcnct (lambda (font size) (concat font " " (number-to-string size)))))
  (if (eq system-type 'windows-nt)
      ;; Windows系统上的Emacs25版本对中文字体的显示存在问题，打开中文文档时会存在卡顿的现象
      ;; 必须手动指定中文字体为宋体才可避免。
      (progn
        (set-default-font (eval `(,fcnct "Consolas" ,efont)))
        (set-fontset-font "fontset-default" 'unicode (eval `(,fcnct "宋体" ,cfont))))
    (progn
      (set-default-font (eval `(,fcnct "YaHei Consolas Hybrid" ,efont)))
      (set-fontset-font "fontset-default" 'unicode ;; 或替换成"Microsoft YaHei Mono"
                        (eval `(,fcnct "Source Han Serif SC SemiBold" ,cfont))))))
;; (set-face-attribute 'default nil :family "Microsoft YaHei Mono" :weight 'normal :height 110) ;; 设置字体，包括字号等
;; (set-frame-font "10" nil t) ;; 设置字号, 同(set-face-attribute)中的:height

(global-font-lock-mode 1) ;; 语法高亮
;; (add-hook 'xxx-mode-hook 'turn-on-font-lock) ;; (font-lock-mode 1)
;; (global-linum-mode 1) ;; 左侧行号，推荐仅将其显示于主要的编辑文档中
;; (add-hook 'xxx-mode-hook 'linum-mode)
(global-highlight-changes-mode 1)
(mouse-avoidance-mode 'animate) ;; 当光标移动至鼠标位置时，为避免遮挡视线，自动移开鼠标
;; (save-place-mode 1) ;; 记录光标在每个文件中最后一次访问时所在的位置
(set-cursor-color "white")
;; (blink-cursor-mode -1)
(column-number-mode 1) ;; 在mode-line显示列数
(scroll-bar-mode -1) ;; 取消滚动条
(global-hl-line-mode 1)
(global-visual-line-mode -1) ;; 对中文支持不好
(show-paren-mode 1) ;; 显示匹配的左右括号
(electric-pair-mode -1)
(electric-quote-mode -1)
(electric-indent-mode -1) ;; 自动缩进
(setq font-lock-maximum-decoration t
      transient-mark-mode t
      shift-select-mode nil
      highlight-changes-global-changes-existing-buffers nil
      highlight-changes-visibility-initial-state t
      highlight-changes-face-list nil
      highlight-changes-colors nil
      blink-cursor-blinks 0
      ;; 这里暂不使用平滑滚动，而是通过设置以下变量以尽可能地避免页面滚动时画面的频繁跳动
      ;; mouse wheel scrolling
      mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      ;; keyboard scrolling
      scroll-margin 1
      scroll-step 3
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      truncate-lines nil
      truncate-partial-width-windows nil
      word-wrap nil
      line-move-visual t
      track-eol t
      require-final-newline t
      show-paren-style 'parentheses
      tab-always-indent 'complete)
(setq-default cursor-type '(bar . 3)
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01
              line-spacing 0 ;; 行距
              ;; fill-column 80 ;; 超过80字符就换行显示
              indicate-empty-lines nil
              indent-tabs-mode nil ;; make indentation commands use space only
              truncate-lines truncate-lines
              word-wrap word-wrap
              tab-width 4)
;; 每次保存buffer时都将删除现有的改动高亮
;; 替换成另外两个hook就会无效，原因未知：write-content-functions或write-file-functions
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;; 删除每行末尾的空格
            (highlight-changes-remove-highlight (point-min) (point-max)))
          t)

(use-package icomplete
  :if (not (my-func-package-enabled-p 'icomplete))
  :config
  (icomplete-mode -1))

(use-package ido
  :if (my-func-package-enabled-p 'ido)
  :config
  (ido-mode 1)
  (ido-everywhere -1) ;; 仅使ido支持find-file和switch-to-buffer
  (setq ido-enable-prefix t
        ido-enable-flex-matching t
        ido-use-filename-at-point t
        ido-enter-matching-directory nil))

(use-package smex
  :if (my-func-package-enabled-p 'smex)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; 该插件会自动替换原M-x快捷键所绑定的命令，若想保留则可重新绑定之
         ("C-x M-x" . execute-extended-command))
  :config
  (smex-initialize))

(use-package helm
  :if (my-func-package-enabled-p 'helm)
  ;; Helm提供了一套在功能上与部分Emacs原生命令相重合的命令集
  ;; 并将其默认绑定在了以'helm-command-prefix-key为前缀的快捷键集中
  ;; 可以通过输入该前缀来触发相关命令
  :bind (("C-c h" . helm-command-prefix) ;; 替换前缀
         ("C-x c")
         ;; 也可以将部分常用命令直接替换Emacs原快捷键
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-s" . helm-occur)
         :map helm-map
         ;; 'helm-execute-persistent-action相比于'helm-select-action更常用
         ("<tab>" . helm-execute-persistent-action)
         ("<C-return>" . helm-select-action)
         :map minibuffer-local-map
         ("M-p" . helm-minibuffer-history)
         ("M-n" . helm-minibuffer-history))
  :config
  (require 'helm-config)
  (setq helm-split-window-in-side-p t
        helm-display-header-line nil
        helm-echo-input-in-header-line nil
        helm-autoresize-max-height 30
        helm-autoresize-min-height 0
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-mode-fuzzy-match nil ;; global
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        ;; helm-apropos-fuzzy-match t
        ;; helm-etags-fuzzy-match t
        helm-move-to-line-cycle-in-source t
        helm-buffer-skip-remote-checking t
        ;; 配置该参数可以指定不同的后台支持，包括imenu、ido、smex等
        ;; helm-completing-read-handlers-alist
        )
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package projectile
  :if (my-func-package-enabled-p 'projectile)
  :init
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-global-mode 1)
  ;; (add-to-list 'projectile-other-file-alist '("html" "js"))
  (use-package helm-projectile
    :if (my-func-package-enabled-p 'helm-projectile)
    :after helm
    :init
    (setq helm-projectile-fuzzy-match t)
    :config
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)
    (helm-projectile-on)))

(use-package magit
  :if (my-func-package-enabled-p 'magit)
  :bind (("C-c g" . magit-status))
  :init
  (when (eq system-type 'windows-nt)
    (let ((path (my-func-executable-find "Git" "git.exe")))
      (when path
        (setq magit-git-executable path))))
  :config
  (setq magit-auto-revert-mode t
        magit-auto-revert-immediately t
        magit-auto-revert-tracked-only t
        ;; magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-repository-directories `((,(expand-file-name "project") . 3)
                                       (,(expand-file-name "Project") . 3))))

;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil ;; 不使用image方式
      speedbar-show-unknown-files t)

;; Key
;; 命令集前缀
;; C-c h :: helm
;; C-c t :: helm-gtags
;; C-c p :: projectile, helm-projectile
;; C-c g :: magit
;; C-c o :: org
(global-set-key (kbd "C-S-a") 'mark-whole-buffer)
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-l") 'windmove-right)
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-unset-key (kbd "C-x o")) ;; (other-window)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-up>") 'text-scale-increase)
(global-set-key (kbd "<C-down>") 'text-scale-decrease)
(global-set-key (kbd "C-x C--") 'downcase-region)
(global-set-key (kbd "C-x C-=") 'upcase-region)
(global-unset-key (kbd "C-x C-l")) ;; (downcase-region)
(global-unset-key (kbd "C-x C-u")) ;; (upcase-region)
(put 'downcase-region 'disabled nil) ;; 去除每次执行此命令时的提示，强制执行
(put 'upcase-region 'disabled nil)
(global-set-key (kbd "C-q") 'read-only-mode)
(global-unset-key (kbd "C-x C-q"))
;; 与输入法切换键冲突
;; (global-set-key (kbd "C-S-SPC") 'set-mark-command)
;; (global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o a") 'org-agenda)

(use-package ace-jump-mode
  :if (my-func-package-enabled-p 'ace-jump-mode)
  :bind (("C-:" . ace-jump-mode-pop-mark)
         ("C-'" . ace-jump-char-mode))
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package avy
  :if (my-func-package-enabled-p 'avy)
  :bind (("C-:" . avy-goto-char-timer) ;; (avy-goto-char)
         ("C-'" . avy-pop-mark))
  :config
  ;; (avy-setup-default)
  (setq avy-timeout-seconds 0.5))

(let ((plg (or (my-func-package-enabled-p 'ace-jump-mode)
               (my-func-package-enabled-p 'avy))))
  (when (and nil plg (my-func-package-enabled-p 'ace-pinyin))
    (when (eq plg 'ace-jump-mode)
      (setq ace-pinyin-use-avy nil)
      (ace-pinyin-global-mode 1)))
  (when (eq plg 'avy)
    (ace-pinyin-global-mode 1)))

;; File Extension
;; (setq auto-mode-alist (cons '("\\.emacs\\'" . emacs-lisp-mode) auto-mode-alist))

;; 调整窗口大小
((lambda ()
   ;; 全屏
   ;; (interactive)
   ;; (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
   ;; 窗口最大化需要分别经过水平和垂直两个方向的最大化
   (interactive)
   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                          '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
   (interactive)
   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                          '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0))))

(provide 'my-init)

;; =============================================================================
;; 加载其他配置文件
(let ((path my-user-emacs-directory))
  (mapc (lambda (name)
          (load (concat path name) t nil nil t))
        '(;; prog-mode与text-mode是相互独立的
          "prog" ;; prog-mode
          ;; "prog-cc" ;; cc-mode (c-mode, c++-mode, java-mode)
          ;; "prog-lisp" ;; lisp-mode, emacs-lisp-mode, lisp-interaction-mode
          "prog-py" ;; python-mode
          ;; "prog-hs" ;; haskell-mode
          "text-tex" ;; tex-mode, latex-mode
          ;; "web-browser" ;; web browser
          )))

(message "emacs init time = %s" (emacs-init-time))
