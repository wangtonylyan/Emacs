;; -*- coding: utf-8 -*-

;; =======================================================================================
;; 若打开的中文文件中仍然存在乱码，则尝试执行(revert-buffer-with-coding-system 'gb18030)
;; 字符集和编码的名称可查询：'language-info-alist
(set-language-environment "Chinese-GB18030")
(prefer-coding-system 'utf-8)
;; (set-face-background 'default "#C7EDCC") ;; 设置背景颜色为绿色护眼色
;; 字体的名字源自于.ttf或.otf文件内自带的元信息，包括family和style等
;; 英文字体：Consolas, Fira Mono, Source Code Pro
;; 中文字体：SimSun, MicrosoftYaHei, SourceHanSerifSC(思源宋体), SourceHanSansSC(思源黑体)
;; 混合字体：YaHeiConsolasHybrid, MicrosoftYaHeiMono
;; Emacs中设置中文字体有以下几种方案
;; 1. 默认编码：英文字体，中文编码：中文字体
;; 此方案存在的缺陷在于，中英文字体高度不同，导致含有中文字体的行与纯英文字体的行之间行距不均
;; 而以下设置又不知为何无法生效
;; (add-to-list 'face-font-rescale-alist '("SimSun" . 0.8) t)
;; 2. 默认编码：中英文混合字体
;; 网上提供的混合字体，拥有统一的行高，但通常都不能完善地支持斜体、粗体等形式
(let* ((rsltn (* (display-pixel-width) (display-pixel-height)))
       ;; 针对中英文字体分别设置两种字号
       (efont (cond ((<= rsltn (* 1600 900)) 11)
                    ((< rsltn (* 1920 1080)) 12)
                    ((< rsltn (* 2560 1440)) 13)
                    (t 15)))
       (cfont (- efont 1))
       (espec (font-spec
               :family (if (eq system-type 'windows-nt)
                           "Consolas" "Fira Mono")
               ;; :weight 'semi-bold
               :size efont))
       ;; Windows系统上的Emacs25版本对中文字体的显示存在问题，打开中文文档时会存在卡顿的现象
       ;; 必须手动指定中文字体为宋体才可避免。
       (cspec (font-spec
               :family (if (eq system-type 'windows-nt)
                           "SimSun" "SourceHanSansSC")
               :size cfont)))
  ;; (set-face-attribute 'default nil :font espec)
  (set-face-font 'default espec) ;; ASCII
  (dolist (chset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font t chset cspec nil)) ;; non-ASCII
  (setq-default line-spacing (/ efont 3)))


(setq user-full-name "TonyLYan"
      user-mail-address "wangtonylyan@outlook.com"
      inhibit-startup-message 1 ;; 取消启动界面
      frame-title-format '(buffer-file-name "%f" ("%b")) ;; 设置标题栏显示为buffer名字
      uniquify-buffer-name-style 'post-forward-angle-brackets ;; 重名buffer的命名
      help-window-select t
      visible-bell t ;; 以窗口闪烁的方式代替错误提示音
      echo-keystrokes 0.1
      debug-on-error nil ;; 显示错误信息
      debug-on-signal nil
      debug-on-quit nil
      select-enable-clipboard t
      delete-by-moving-to-trash t
      auto-save-list-file-prefix (my/set-user-emacs-file
                                  ".emacs.auto-save/.saves-")
      font-lock-maximum-decoration t
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
      blink-matching-paren t
      blink-matching-paren-on-screen t
      tab-always-indent 'complete)
(setq-default cursor-type '(bar . 3)
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01
              fill-column 100 ;; 在auto-fill-mode模式下，超过指定字符就会被强制换行
              indicate-empty-lines nil
              indent-tabs-mode nil ;; make indentation commands use space only
              truncate-lines truncate-lines
              word-wrap word-wrap
              tab-width 4)

(defalias 'yes-or-no-p 'y-or-n-p) ;; 以y/n替换yes/no
(tool-bar-mode -1)
(menu-bar-mode -1)
(auto-image-file-mode 1) ;; 允许打开图片
(auto-compression-mode 1) ;; 允许查看和写入压缩包
(global-font-lock-mode 1) ;; 语法高亮
;; (add-hook 'xxx-mode-hook #'turn-on-font-lock) ;; (font-lock-mode 1)
(global-linum-mode -1) ;; (setq linum-format "%5d")
;; (add-hook 'xxx-mode-hook #'linum-mode)
(global-hi-lock-mode 1)
(global-hl-line-mode 1)
;; (global-highlight-changes-mode 1)
(mouse-avoidance-mode 'animate) ;; 当光标移动至鼠标位置时，为避免遮挡视线，自动移开鼠标
;; (set-cursor-color "gold")
;; (blink-cursor-mode -1)
(column-number-mode 1) ;; 在mode-line显示列数
(scroll-bar-mode -1) ;; 取消滚动条
(global-visual-line-mode -1) ;; 对中文支持不好
;; (my/add-mode-hook "text" #'visual-line-mode)
(electric-pair-mode -1)
(electric-quote-mode -1)
(electric-indent-mode -1)

(put 'downcase-region 'disabled nil) ;; 去除每次执行此命令时的提示，强制执行
(put 'upcase-region 'disabled nil)

(use-package files
  :defer t
  :init
  (setq make-backup-files t
        backup-by-copying t
        backup-directory-alist `(("." . ,(my/set-user-emacs-file
                                          ".emacs.backup/")))
        version-control t
        kept-old-versions 1
        kept-new-versions 1
        delete-old-versions t
        dired-kept-versions 2
        require-final-newline t)
  :config
  (add-hook 'find-file-hook 'my/find-file-read-only t)
  (add-hook 'before-save-hook 'my/reformat-current-file t))

(use-package recentf
  :init
  (setq recentf-save-file (my/set-user-emacs-file ".emacs.recentf"))
  :config
  (recentf-mode 1))

(use-package autorevert
  :init
  (setq auto-revert-use-notify t
        auto-revert-interval 1
        auto-revert-verbose nil
        auto-revert-stop-on-user-input t)
  :config
  (global-auto-revert-mode 1))

(use-package grep
  :defer t
  :config
  (mapc (lambda (dir)
          (add-to-list 'grep-find-ignored-directories dir))
        '(".semanticdb"))
  (mapc (lambda (file)
          (add-to-list 'grep-find-ignored-files file))
        '("*.zip" "*.rar"
          "*.bmp" "*.jpg" "*.jpeg" "*.png" "*.gif" "*.svg"
          "TAGS" "GTAGS" "GRTAGS" "GPATH"
          ".*")))

(use-package desktop
  :init
  (setq desktop-save 'ask-if-exists
        desktop-dirname (my/set-user-emacs-file ".emacs.desktop/")
        desktop-path (list (my/set-user-emacs-file ".emacs.desktop/"))
        desktop-base-file-name "desktop"
        desktop-base-lock-name "desktop.lock"
        desktop-restore-frames t
        desktop-restore-reuses-frames t
        desktop-restore-in-current-display t
        desktop-restore-forces-onscreen t
        desktop-auto-save-timeout (* 60 10))
  :config
  (desktop-save-mode 1))

(use-package winner
  :config
  (winner-mode 1))

(use-package paren
  :init
  (setq show-paren-style 'parenthesis
        show-paren-ring-bell-on-mismatch nil
        show-paren-when-point-inside-paren nil
        show-paren-when-point-in-periphery nil)
  :config
  (show-paren-mode 1))

(use-package saveplace
  :init
  (setq save-place-file (my/set-user-emacs-file ".emacs.save-place")
        save-place-version-control nil
        save-place-limit 400
        save-place-forget-unreadable-files t
        save-place-save-skipped nil)
  :config
  (save-place-mode 1))


(toggle-frame-maximized)


(provide 'my/init/emacs)
