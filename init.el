;; -*- coding: utf-8 -*-

;; 判断Emacs版本可以基于以下两个变量：'emacs-major-version和'emacs-minor-version

(defun my-func-executable-find (exe &optional dir add)
  (let* ((dir (if (and (stringp dir) (> (length dir) 0))
                  ;; 统一传参的形式
                  (file-name-as-directory dir) ""))
         (path (executable-find (concat dir exe))))
    (when (and path (file-executable-p path))
      (when add
        (add-to-list 'exec-path (directory-file-name
                                 (file-name-directory path) t)))
      path)))

(defalias 'my-func-package-enabled-p 'package--user-selected-p)

(defalias 'my-func-minor-mode-on-p 'bound-and-true-p)

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "D:/softwares" t)
  (let ((path (my-func-executable-find "cmdproxy.exe"
                                       "Emacs25/libexec/emacs/24.5/i686-pc-mingw32")))
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
(defconst my-private-emacs-directory (concat user-emacs-directory ".private/"))

(load (concat my-private-emacs-directory "init.el") t)
;; ******************** sample ********************
;; (defconst my-private-project-root-directory "~/project/")
;; (defconst my-private-project-ede-config-file
;;   (concat my-private-project-root-directory "ede-projects.el"))
;; ************************************************

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
  ;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
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
  (setq package-selected-packages '(;; all-the-icons-dired
                                    spaceline-all-the-icons ;; powerline, spaceline
                                    ;; smart-mode-line, smart-mode-line-powerline-theme
                                    zenburn-theme ;; atom-one-dark-theme, doom-themes, github-theme, solarized-theme
                                    ;; doom-themes-neotree
                                    ;; nlinum-hl
                                    ;; yascroll
                                    buffer-move
                                    rainbow-delimiters
                                    rainbow-identifiers
                                    ;; fill-column-indicator, whitespace
                                    avy ;; ace-jump-mode
                                    ;; ace-pinyin
                                    undo-tree
                                    smart-hungry-delete
                                    highlight-thing
                                    ;; evil
                                    bm
                                    helm-bm
                                    ;; neotree, sr-speedbar
                                    ;; sublimity, minimap
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
                                    stickyfunc-enhance
                                    ;; Python
                                    elpy ;; ropemacs
                                    flycheck-pyflakes
                                    py-autopep8
                                    auto-virtualenvwrapper ;; virtualenvwrapper
                                    ;; Haskell
                                    haskell-mode
                                    hindent
                                    flycheck-haskell
                                    ;; Standard ML
                                    ;; sml-mode
                                    ;; HTML
                                    ;; web-mode
                                    ;; LaTeX
                                    ;; auctex
                                    ;; pdf-tools
                                    ;; w3m
                                    ;; erc ;; circe, rcirc
                                    use-package))
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install-selected-packages))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish nil t)

(use-package sublimity
  :if (my-func-package-enabled-p 'sublimity)
  :commands (sublimity-global-mode sublimity-mode)
  :init
  (setq sublimity-map-set-delay 3)
  :config
  ;; (require 'sublimity-scroll)
  ;; (require 'sublimity-attractive)
  (require 'sublimity-map nil t))

(use-package minimap
  :if (my-func-package-enabled-p 'minimap)
  :config
  (setq minimap-always-recenter nil ;; 设置为nil才有效?
        minimap-recenter-type 'middle
        minimap-buffer-name-prefix "MINI" ;; 不能为空，否则无法启动minimap窗口
        minimap-hide-fringes t
        minimap-hide-scroll-bar t
        minimap-update-delay 1.0
        minimap-window-location 'left
        minimap-display-semantic-overlays nil
        minimap-enlarge-certain-faces nil))

(use-package flyspell
  :if (and (my-func-package-enabled-p 'flyspell)
           (executable-find "aspell"))
  :config
  (setq ispell-program-name (executable-find "aspell") ;; 设置后台支持程序
        ;; ispell-dictionary "english" ;; default dictionary
        ;; ispell-personal-dictionary ""
        flyspell-issue-message-flag nil)
  (add-hook 'text-mode-hook 'flyspell-mode t)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode t)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN" . "^#+END") t))

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
      help-window-select t
      visible-bell nil ;; 以窗口闪烁的方式代替错误提示音
      echo-keystrokes 0.1
      debug-on-error nil ;; 显示错误信息
      debug-on-signal nil
      debug-on-quit nil
      select-enable-clipboard t
      auto-revert-use-notify t
      auto-revert-interval 1
      auto-revert-verbose nil
      auto-revert-stop-on-user-input t
      delete-by-moving-to-trash t
      make-backup-files t ;; 启用自动备份
      version-control t ;; 启用版本控制，即可以备份多次
      kept-old-versions 1 ;; 备份最旧的版本个数
      kept-new-versions 1 ;; 备份最新的版本个数
      delete-old-versions t
      dired-kept-versions 2
      backup-directory-alist '(("." . "~/.emacs.d.backups"))
      backup-by-copying t)
(defalias 'yes-or-no-p 'y-or-n-p) ;; 以y/n替换yes/no
(tool-bar-mode -1)
(menu-bar-mode -1)
;; 该模式用于监控磁盘上的文件是否被外部程序修改，并提示用户或自动重新加载该文件
(global-auto-revert-mode 1)
(recentf-mode 1)
(auto-image-file-mode 1) ;; 允许打开图片
(auto-compression-mode 1) ;; 允许查看和写入压缩包

;; -----------------------------------------------------------------------------
;; 若打开的中文文件中仍然存在乱码，则尝试执行(revert-buffer-with-coding-system 'gb18030)
;; 字符集和编码的名称可查询：'language-info-alist
(set-language-environment "Chinese-GB18030")
(prefer-coding-system 'utf-8)
;; (set-face-background 'default "#C7EDCC") ;; 设置背景颜色为绿色护眼色
;; 字体的名字源自于.ttf或.otf文件内自带的元信息，包括family和style等
;; 英文字体：Consolas
;; 中文字体：SimSun，MicrosoftYaHei，SourceHanSerifSC (思源宋体简体中文)，SourceHanSansSC (思源黑体简体中文)
;; 混合字体：YaHeiConsolasHybrid，MicrosoftYaHeiMono
;; Emacs中设置中文字体有以下几种方案
;; 1) 默认编码：英文字体，中文编码：中文字体
;; 此方案存在的缺陷在于，中英文字体高度不同，导致含有中文字体的行与纯英文字体的行之间行距不均
;; 而以下设置又不知为何无法生效
;; (add-to-list 'face-font-rescale-alist '("SimSun" . 0.8) t)
;; 2) 默认编码：中英文混合字体
;; 网上提供的混合字体，拥有统一的行高，但通常都不能完善地支持斜体、粗体等形式

;; http://emacser.com/torture-emacs.htm
(let* ((rsltn (* (display-pixel-width) (display-pixel-height)))
       ;; 针对中英文字体分别设置两种字号
       (efont (cond ((<= rsltn (* 1600 900)) 12)
                    ((<= rsltn (* 1920 1080)) 13)
                    (t 14)))
       (cfont (- efont 2)))
  (if (eq system-type 'windows-nt)
      ;; Windows系统上的Emacs25版本对中文字体的显示存在问题，打开中文文档时会存在卡顿的现象
      ;; 必须手动指定中文字体为宋体才可避免。
      (progn
        (set-face-font 'default (font-spec :family "Consolas" :size efont))
        (set-fontset-font "fontset-default" 'han
                          (font-spec :family "SimSun" :size cfont) nil 'prepend))
    (progn
      ;; e.g. 设置字体的方式有以下三种
      ;; (set-frame-font (font-spec))
      ;; (set-face-attribute 'default nil :font (font-spec))
      (set-face-font 'default (font-spec :family "YaHeiConsolasHybrid" :size efont))
      ;; 'charset-script-alist
      (set-fontset-font "fontset-default" 'han
                        (font-spec :family "YaHeiConsolasHybrid" :size cfont) nil 'prepend))))

;; -----------------------------------------------------------------------------
(global-font-lock-mode 1) ;; 语法高亮
;; (add-hook 'xxx-mode-hook 'turn-on-font-lock) ;; (font-lock-mode 1)
;; (global-linum-mode 1) ;; 左侧行号，推荐仅将其显示于主要的编辑文档中
;; (add-hook 'xxx-mode-hook 'linum-mode)
(global-hi-lock-mode 1)
(global-hl-line-mode 1)
;; (global-highlight-changes-mode 1)
(mouse-avoidance-mode 'animate) ;; 当光标移动至鼠标位置时，为避免遮挡视线，自动移开鼠标
;; (save-place-mode 1) ;; 记录光标在每个文件中最后一次访问时所在的位置
;; (set-cursor-color "gold")
;; (blink-cursor-mode -1)
(column-number-mode 1) ;; 在mode-line显示列数
(scroll-bar-mode -1) ;; 取消滚动条
(global-visual-line-mode -1) ;; 对中文支持不好
(add-hook 'text-mode-hook 'visual-line-mode t)
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
      ;; highlight-changes-colors nil
      blink-cursor-blinks 0
      ;; 这里暂不使用平滑滚动，而是通过设置以下变量以尽可能地避免页面滚动时画面的频繁跳动
      ;; mouse wheel scrolling
      mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      ;; keyboard scrolling
      scroll-bar-adjust-thumb-portion nil
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
      blink-matching-paren t
      blink-matching-paren-on-screen t
      tab-always-indent 'complete)
(setq-default cursor-type '(bar . 3)
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01
              line-spacing 1 ;; 行距
              fill-column 100 ;; 在auto-fill-mode模式下，超过指定字符就会被强制换行
              indicate-empty-lines nil
              indent-tabs-mode nil ;; make indentation commands use space only
              truncate-lines truncate-lines
              word-wrap word-wrap
              tab-width 4)

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;; 删除每行末尾的空格
            (when (derived-mode-p 'lisp-mode 'emacs-lisp-mode)
              (indent-region (point-min) (point-max)))
            ;; 每次保存buffer时都将删除现有的改动高亮
            ;; 替换成另外两个hook就会无效，原因未知：write-content-functions或write-file-functions
            (highlight-changes-remove-highlight (point-min) (point-max)))
          t)

;; Key Binding
;; 命令集前缀
;; 以C-c加单个字母为前缀，且全局性key map的前缀互不相同
;; C-c h :: helm
;; C-c c :: helm-gtags
;; C-c p :: projectile, helm-projectile
;; C-c g :: magit
;; C-c o :: org
;; C-c i :: highlight
;; C-c b :: bm, helm-bm
;; C-c w :: window layout: windmove, winner, buffer-move
(unbind-key "C-x f") ;; (set-fill-column)
(unbind-key "C-x C-l") ;; (downcase-region)
(unbind-key "C-x C-u") ;; (upcase-region)
(unbind-key "C-M-v") ;; (scroll-other-window)
(unbind-key "M-s h")
(bind-keys ("C-S-a" . mark-whole-buffer)
           ("C-M-p" . scroll-other-window-down)
           ("C-M-n" . scroll-other-window)
           ("<C-wheel-up>" . text-scale-increase)
           ("<C-wheel-down>" . text-scale-decrease)
           ("<C-up>" . text-scale-increase)
           ("<C-down>" . text-scale-decrease)
           ("C-x C--" . downcase-region)
           ("C-x C-=" . upcase-region)
           ("C-q" . read-only-mode)
           ("C-c i i" . highlight-symbol-at-point)
           ("C-c i p" . highlight-phrase)
           ("C-c i r" . highlight-regexp)
           ("C-c i l" . highlight-lines-matching-regexp)
           ("C-c i u" . unhighlight-regexp))
(put 'downcase-region 'disabled nil) ;; 去除每次执行此命令时的提示，强制执行
(put 'upcase-region 'disabled nil)
;; 与输入法切换键冲突
;; (global-set-key (kbd "C-S-SPC") 'set-mark-command)
;; (global-unset-key (kbd "C-SPC"))

;; File Extension
;; (setq auto-mode-alist (cons '("\\.emacs\\'" . emacs-lisp-mode) auto-mode-alist))

(winner-mode 1)
(unbind-key "C-x o") ;; (other-window)
(unbind-key "C-x <left>") ;; (previous-buffer)
(unbind-key "C-x <right>") ;; (next-buffer)
(bind-keys ("C-+" . enlarge-window)
           ("C-_" . shrink-window)
           ("C-=" . enlarge-window-horizontally)
           ("C--" . shrink-window-horizontally))
(use-package windmove
  :demand t
  :bind (("C-S-h" . windmove-left)
         ("C-S-l" . windmove-right)
         ("C-S-k" . windmove-up)
         ("C-S-j" . windmove-down))
  :config
  ;; <shift-up/down/left/right>
  (windmove-default-keybindings))
(use-package winner
  :demand t
  :bind (("C-c w u" . winner-undo)
         ("C-c w r" . winner-redo)))
(use-package buffer-move
  :if (my-func-package-enabled-p 'buffer-move)
  :bind (("C-c w h" . buf-move-left)
         ("C-c w l" . buf-move-right)
         ("C-c w k" . buf-move-up)
         ("C-c w j" . buf-move-down)))

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
         ;; 也可以将部分常用命令直接替换Emacs原快捷键
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-o" . helm-occur))
  :diminish helm-mode
  :init
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
        helm-follow-input-idle-delay 0.5
        helm-follow-mode-persistent nil
        ;; helm-source-names-using-follow '("Occur")
        helm-buffer-skip-remote-checking t
        ;; 配置该参数可以指定不同的后台支持，包括imenu、ido、smex等
        ;; helm-completing-read-handlers-alist
        )
  :config
  (unbind-key "C-x c")
  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action)
             ("M-x" . helm-select-action)
             ("<C-tab>" . helm-follow-mode)
             :map minibuffer-local-map
             ("M-p" . helm-minibuffer-history)
             ("M-n" . helm-minibuffer-history))
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package projectile
  :preface
  (defvar my-plugin-projectile-switch-hook '())
  (defun my-plugin-projectile-switch-action ()
    (run-hooks 'my-plugin-projectile-switch-hook))
  :if (my-func-package-enabled-p 'projectile)
  :init
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-keymap-prefix (kbd "C-c p")
        projectile-switch-project-action 'my-plugin-projectile-switch-action)
  :config
  (projectile-mode 1)
  ;; (add-to-list 'projectile-other-file-alist '("html" "js"))
  (use-package helm-projectile
    :if (my-func-package-enabled-p 'helm-projectile)
    :demand t ;; 初始化后就立即启用，基于project的方式管理各类文件
    :after helm
    :init
    (setq helm-projectile-fuzzy-match t
          projectile-completion-system 'helm)
    :config
    (add-hook 'my-plugin-projectile-switch-hook 'helm-projectile t)
    (helm-projectile-on)))

(use-package magit
  :if (my-func-package-enabled-p 'magit)
  :bind (("C-c g" . magit-status))
  :config
  (when (eq system-type 'windows-nt)
    (let ((path (my-func-executable-find "git.exe" "Git")))
      (when path
        (setq magit-git-executable path))))
  (setq magit-auto-revert-mode t
        magit-auto-revert-immediately t
        magit-auto-revert-tracked-only t
        ;; magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-repository-directories `((,(expand-file-name "project") . 3)
                                       (,(expand-file-name "Project") . 3)
                                       (,(expand-file-name "projects") . 3)
                                       (,(expand-file-name "Projects") . 3))))

(use-package sr-speedbar
  :if (my-func-package-enabled-p 'sr-speedbar)
  :bind (("C-S-s" . sr-speedbar-toggle))
  :init
  (setq speedbar-use-images nil
        speedbar-show-unknown-files t
        ;; speedbar-verbosity-level 0
        sr-speedbar-right-side nil
        sr-speedbar-max-width 30
        sr-speedbar-delete-windows nil
        sr-speedbar-skip-other-window-p t)
  :config
  (bind-keys :map speedbar-file-key-map
             ("<tab>" . speedbar-edit-line)))

(use-package bm
  :if (my-func-package-enabled-p 'bm)
  :bind (("C-c b m" . bookmark-set)
         ("C-c b d" . bookmark-delete)
         ("C-c b r" . bookmark-rename)
         ("C-c b l" . bookmark-bmenu-list)
         ("C-c b n" . bm-next)
         ("C-c b p" . bm-previous)
         ("C-c b t" . bm-toggle))
  :config
  (unbind-key "C-x r")
  (setq bm-marker 'bm-marker-left
        bm-cycle-all-buffers t
        temporary-bookmark-p t
        bm-buffer-persistence t
        bm-restore-repository-on-load t
        bm-repository-file (concat user-emacs-directory "bm-repository"))
  (setq-default bm-buffer-persistence bm-buffer-persistence)
  (add-hook' after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (use-package helm-bm
    :if (my-func-package-enabled-p 'helm-bm)
    :demand t
    :after helm
    :bind (("C-c b b" . helm-bm))))

(use-package neotree
  :preface
  (defun my-plugin-neotree-toggle ()
    (interactive)
    (let ((root (when (and (fboundp 'projectile-project-p)
                           (fboundp 'projectile-project-root)
                           (projectile-project-p))
                  (projectile-project-root)))
          (file (buffer-file-name)))
      (neotree-toggle)
      (if (and root file (neo-global--window-exists-p))
          (progn
            (neotree-dir root)
            (neotree-find file))
        (message "neotree: could not find projectile project"))))
  :if (my-func-package-enabled-p 'neotree)
  :ensure all-the-icons
  :bind (("C-S-s" . my-plugin-neotree-toggle))
  :init
  (setq neo-theme (if (display-graphic-p) 'icons ;; (require 'all-the-icons)
                    'nerd)
        neo-smart-open t
        neo-show-hidden-files nil
        neo-show-updir-line t
        neo-window-width 28)
  :config
  (bind-keys :map neotree-mode-map
             ("n" . neotree-next-line)
             ("p" . neotree-previous-line)
             ("C-n" . neotree-select-next-sibling-node)
             ("C-p" . neotree-select-previous-sibling-node)
             ("u" . neotree-select-up-node)
             ("a" . neotree-hidden-file-toggle))
  (unbind-key "s" neotree-mode-map)
  (unbind-key "S" neotree-mode-map)
  (unbind-key "D" neotree-mode-map)
  (unbind-key "H" neotree-mode-map)
  (unbind-key "U" neotree-mode-map)
  (when (bound-and-true-p my-plugin-projectile-switch-hook)
    (add-hook 'my-plugin-projectile-switch-hook 'neotree-projectile-action t)))

(use-package org
  :bind (("C-c o c" . org-capture)
         ("C-c o a" . org-agenda))
  :init
  (setq org-src-fontify-natively t)
  :config
  (add-hook 'org-mode-hook 'org-indent-mode t))

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

(use-package undo-tree
  :if (my-func-package-enabled-p 'undo-tree)
  :init
  (setq undo-tree-visualizer-diff nil
        undo-tree-visualizer-relative-timestamps nil)
  :config
  (bind-keys :map undo-tree-visualizer-mode-map
             ("<return>" . undo-tree-visualizer-quit)
             ("C-p" . undo-tree-visualize-undo-to-x)
             ("C-n" . undo-tree-visualize-redo-to-x))
  (unbind-key "C-_" undo-tree-map)
  (global-undo-tree-mode 1))

(use-package smart-hungry-delete
  :if (my-func-package-enabled-p 'smart-hungry-delete)
  :demand t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :config
  (smart-hungry-delete-add-default-hooks))

(use-package highlight-thing
  :if (my-func-package-enabled-p 'highlight-thing)
  :init
  (setq highlight-thing-what-thing 'symbol
        highlight-thing-exclude-thing-under-point nil
        highlight-thing-delay-seconds 0.5
        highlight-thing-limit-to-defun nil
        highlight-thing-case-sensitive-p t)
  :config
  (global-hl-line-mode -1)
  (add-hook 'text-mode-hook 'hl-line-mode t)
  (add-hook 'prog-mode-hook 'highlight-thing-mode t))

(use-package evil
  :if (my-func-package-enabled-p 'evil)
  :config
  (bind-keys :map evil-normal-state-map
             ("q" . read-only-mode)
             ("C-a" . move-beginning-of-line)
             ("C-e" . move-end-of-line)
             ("C-S-h" . evil-window-left)
             ("C-S-l" . evil-window-right)
             ("C-S-j" . evil-window-down)
             ("C-S-k" . evil-window-up)
             ("C-v" . evil-scroll-page-down)
             ("M-v" . evil-scroll-page-up))
  (evil-mode 1))

(use-package eshell
  :config
  ;; (add-to-list 'eshell-visual-commands)
  (bind-key "C-c e" (lambda ()
                      (interactive)
                      (if (eq major-mode 'eshell-mode)
                          (progn
                            (insert "exit")
                            (eshell-send-input)
                            (delete-window))
                        (progn
                          (let* ((dir (if (buffer-file-name)
                                          (file-name-directory (buffer-file-name))
                                        default-directory))
                                 (name (car (last (split-string dir "/" t)))))
                            (split-window-vertically (- (/ (window-total-height) 3)))
                            (other-window 1)
                            (eshell "new")
                            (rename-buffer (concat "*eshell: " name "*"))))))))

(provide 'my-init)

;; 加载其他配置文件
(let ((path my-user-emacs-directory))
  (mapc (lambda (name)
          (load (concat path name) t nil nil t))
        '(;; prog-mode与text-mode是相互独立的
          "prog" ;; prog-mode
          "prog-cc" ;; cc-mode (c-mode, c++-mode, java-mode)
          ;; "prog-lisp" ;; lisp-mode, emacs-lisp-mode, lisp-interaction-mode
          "prog-py" ;; python-mode
          "prog-hs" ;; haskell-mode
          "prog-web" ;; web-mode
          "text-tex" ;; tex-mode, latex-mode
          ;; "web-browser" ;; web browser
          )))

;; =============================================================================
;; 调整窗口大小
(when (fboundp 'x-send-client-message)
  ((lambda ()
     ;; 全屏
     ;; (interactive)
     ;; (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
     ;; 或
     ;; (set-frame-parameter nil 'fullscreen 'fullboth)
     ;; 窗口最大化需要分别经过水平和垂直两个方向的最大化
     (interactive)
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
     (interactive)
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))))

(use-package all-the-icons
  :ensure t
  :init
  ;; 此插件在首次使用前需要额外地安装字体，否则启用后mode-line中的图片会显示为乱码
  ;; 执行以下命令会自动下载并安装所需字体，Windows上只能手动执行
  ;; 但目前发现Linux上会因权限问题而导致安装失败，因此仍推荐手动执行
  ;; 字体下载目录默认为HOME/.local/share/fonts
  ;; (all-the-icons-install-fonts)
  :config
  (when (and (my-func-package-enabled-p 'all-the-icons-dired)
             (require 'all-the-icons-dired nil t))))

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'arrow
        powerline-default-separator-dir '(left . right))
  (when (my-func-package-enabled-p 'powerline)
    (powerline-default-theme)))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (when (my-func-package-enabled-p 'spaceline)
    ;; (spaceline-emacs-theme)
    (spaceline-spacemacs-theme))
  (eval-after-load 'helm '(spaceline-helm-mode)))

(use-package spaceline-all-the-icons
  :if (my-func-package-enabled-p 'spaceline-all-the-icons)
  :config
  (spaceline-all-the-icons-theme)
  ;; (spaceline-all-the-icons--setup-neotree)
  (spaceline-all-the-icons--setup-package-updates))

(use-package smart-mode-line
  :if (or (my-func-package-enabled-p 'smart-mode-line)
          (my-func-package-enabled-p 'smart-mode-line-powerline-theme))
  :config
  (setq sml/theme (if (and (my-func-package-enabled-p 'smart-mode-line-powerline-theme)
                           (require 'smart-mode-line-powerline-theme nil t))
                      'powerline 'automatic)
        sml/no-confirm-load-theme t
        sml/shorten-directory t
        sml/shorten-modes t)
  (smart-mode-line-enable))

(let ((path (concat my-user-emacs-directory "theme")))
  (add-to-list 'load-path path t)
  (add-to-list 'custom-theme-load-path path t))
(use-package atom-one-dark-theme
  :if (my-func-package-enabled-p 'atom-one-dark-theme)
  :config
  (load-theme 'atom-one-dark t))
(use-package doom-themes
  :if (my-func-package-enabled-p 'doom-themes)
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (doom-themes-neotree-config)
  (when visible-bell
    (doom-themes-visual-bell-config)))
(use-package github-theme
  :if (my-func-package-enabled-p 'github-theme)
  :init
  (setq github-override-colors-alist '(("github-white" . "#FBF9E1")
                                       ("github-comment" . "#009E73")
                                       ("github-text" . "#000000")))
  :config
  (load-theme 'github t))
(use-package solarized-theme
  :if (my-func-package-enabled-p 'solarized-theme)
  :config
  ;; (load-theme 'solarized-light t)
  (load-theme 'solarized-dark t))
(use-package zenburn-theme
  :if (my-func-package-enabled-p 'zenburn-theme)
  :init
  (setq zenburn-override-colors-alist '(("zenburn-fg" . "#EDEDDD")))
  :config
  (load-theme 'zenburn t))

(when (my-func-package-enabled-p 'neotree)
  (with-eval-after-load 'neotree
    (cond
     ((my-func-package-enabled-p 'doom-themes-neotree)
      (use-package doom-themes
        :config
        (doom-themes-neotree-config)))
     ((my-func-package-enabled-p 'spaceline-all-the-icons)
      (use-package spaceline-all-the-icons
        :config
        (spaceline-all-the-icons--setup-neotree))))))

(use-package nlinum-hl
  :if (my-func-package-enabled-p 'nlinum-hl)
  :config
  (run-with-idle-timer 5 t 'nlinum-hl-flush-window)
  (run-with-idle-timer 30 t 'nlinum-hl-flush-all-windows)
  (add-hook 'focus-in-hook 'nlinum-hl-flush-all-windows)
  (add-hook 'focus-out-hook 'nlinum-hl-flush-all-windows)
  (advice-add 'select-window :before 'nlinum-hl-do-flush)
  (advice-add 'select-window :after 'nlinum-hl-do-flush))

(use-package yascroll
  :if (my-func-package-enabled-p 'yascroll)
  :init
  (setq yascroll:delay-to-hide nil)
  :config
  (add-to-list 'yascroll:disabled-modes 'neotree-mode)
  (global-yascroll-bar-mode 1))

;; 嵌套的括号通过大小而不仅是颜色来进行区分
(use-package rainbow-delimiters
  :if (my-func-package-enabled-p 'rainbow-delimiters)
  :init
  (setq rainbow-delimiters-max-face-count 9
        rainbow-delimiters-outermost-only-face-count 0)
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode t))

;; 修改默认字体颜色，从而将文字与符号区分开来
(use-package rainbow-identifiers
  :if (my-func-package-enabled-p 'rainbow-identifiers)
  :init
  ;; dark theme: '(rainbow-identifiers-identifier-1 ((t (:foreground "#CCCCCC"))))
  ;; light theme: '(rainbow-identifiers-identifier-1 ((t (:foreground "#333333"))))
  (setq rainbow-identifiers-face-count 1)
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode t))

(use-package whitespace
  :if (my-func-package-enabled-p 'whitespace)
  :init
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column 80))

(use-package fill-column-indicator
  :if (my-func-package-enabled-p 'fill-column-indicator)
  :init
  (setq fci-rule-use-dashes nil
        fci-rule-column 100)
  :config
  (define-globalized-minor-mode global-fci-mode fci-mode
    ;; 避免在special buffers、dired、shell等特殊模式下启用
    (lambda () (when buffer-file-name (fci-mode 1))))
  (global-fci-mode 1))

;; =============================================================================
(use-package pdf-tools
  :if (my-func-package-enabled-p 'pdf-tools)
  :init
  (setq pdf-view-continuous t)
  :config
  (pdf-tools-install))

(use-package w3m
  :preface
  (defvar-local my-plugin-w3m-exe
    (if (eq system-type 'windows-nt)
        (my-func-executable-find "w3m.exe" "w3m" t)
      (executable-find "w3m")))
  :if (and (my-func-package-enabled-p 'w3m) my-plugin-w3m-exe)
  :config
  (setq w3m-home-page "http://www.baidu.com/"
        w3m-command-arguments '("-cookie" "-F")
        w3m-quick-start t
        w3m-use-cookies t
        w3m-use-favicon t
        w3m-use-symbol t
        w3m-default-display-inline-images t
        w3m-show-graphic-icons-in-header-line nil
        w3m-show-graphic-icons-in-mode-line nil)
  (setq browse-url-browser-function 'w3m-browse-url)
  (add-hook 'w3m-mode-hook 'visual-line-mode t))

(use-package erc
  :if (my-func-package-enabled-p 'erc)
  :config
  ;; (unbind-key "<return>" erc-mode-map)
  (bind-keys :map erc-mode-map
             ("C-<return>" . erc-send-current-line))
  (setq erc-autojoin-channels-alist nil ;; '(("freenode.net" "#emacs"))
        erc-interpret-mirc-color t
        erc-kill-buffer-on-part t))

(use-package circe
  :if (my-func-package-enabled-p 'circe)
  :config
  (setq circe-network-options '(("Freenode" ;; http://freenode.net/
                                 :nick ""
                                 :sasl-username ""
                                 :sasl-password ""
                                 :channels ("#emacs" "#c_lang_cn")))))

;; =============================================================================
(message "emacs init time = %s" (emacs-init-time))

;; =============================================================================
;; http://www.smlnj.org/doc/Emacs/sml-mode.html
(use-package sml-mode
  :if (my-func-package-enabled-p 'sml-mode)
  :init
  (add-to-list 'load-path "/usr/local/sml/bin") ;; path to executable (sml.bat on Windows)
  (add-to-list 'auto-mode-alist '("\\.sml$" . sml-mode))
  (add-to-list 'auto-mode-alist '("\\.sig$" . sml-mode))
  (add-hook 'sml-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil
                    sml-indent-args 2)))
  :config
  (bind-keys :map sml-mode-map
             ("M-SPC" . just-one-space)))
