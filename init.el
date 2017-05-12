;; =============================================================================
(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "D:/softwares" t)
  (let ((path (executable-find
               "Emacs25/libexec/emacs/24.5/i686-pc-mingw32/cmdproxy.exe")))
    (when path
      (setq shell-file-name path
            shell-command-switch "-c"))))

;; (setq user-init-file "~/.emacs.d/init.el")
;; (load user-init-file)

(setq default-directory "~"
      command-line-default-directory default-directory
      user-emacs-directory "~/.emacs.d/" ;; 注意此路径比较特殊，以"/"结尾
      my-user-emacs-directory (concat user-emacs-directory "my-emacs" "/"))
;; (normal-top-level-add-subdirs-to-load-path)
;; (normal-top-level-add-to-load-path)

;; =============================================================================
;; (setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
;; (load custom-file)
;; 下述内容会因用户在Customize界面的保存操作而由Emacs自动写入
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
 )

;; =============================================================================
;; 管理和加载全局性的第三方插件
;; 1) ELPA (Emacs Lisp Package Archive)
;; 可以通过Emacs24以上版本内置的ELPA工具来安装和管理第三方插件
;; 2) 从网上下载、编译、安装第三方插件
;; 资源网站: https://github.com/emacsmirror
;; 通常的安装方式就是将.el源文件放置于'load-path所包含的目录中即可
;; -----------------------------------------------------------------------------
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

  ;; 目前使用此全局变量来管理插件的启用/禁用
  ;; 未包含于ELPA库中的插件也同样通过该链表中进行管理
  (setq package-selected-packages '(;; 1) theme
                                    atom-one-dark-theme
                                    ;; 2) programming
                                    yasnippet company
                                    magit
                                    ;; flycheck
                                    ;; 3) python
                                    elpy py-autopep8
                                    ;; ropemacs
                                    ))
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install-selected-packages))

;; =============================================================================
;; Color Theme
;; http://www.nongnu.org/color-theme/
;; 该插件已集成于Emacs24以上版本中，作为默认的主题管理组件
;; 其自带了一些主题，网上还有许多基于该插件独立发布的主题
;; -----------------------------------------------------------------------------
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
                     (enable-theme 'solarized))))))))
 "atom-one-dark")

;; =============================================================================
;; Auctex
;; -----------------------------------------------------------------------------
(when (and (member 'auctex package-selected-packages)
           (require 'auctex nil t))
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil))

;; =============================================================================
;; Smex
;; https://github.com/nonsequitur/smex
;; 其完全基于ido-mode实现，提供了对于execute-extended-command更为强大的支持
;; 包括了记录用户操作历史、快速查询输入命令的快捷键及函数说明等功能
;; -----------------------------------------------------------------------------
(when (and (member 'smex package-selected-packages)
           (require 'smex nil t))
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; 与原M-x快捷键能冲突
  (global-set-key (kbd "C-x M-x") 'execute-extended-command))

;; =============================================================================
;; Minimap
;; 其全部的可配置选项见于(customize-group minimap)中
;; -----------------------------------------------------------------------------
(when (and (member 'minimap package-selected-packages)
           (require 'minimap nil t))
  (setq minimap-always-recenter nil ;; 设置为nil才有效?
        minimap-recenter-type 'middle
        minimap-buffer-name-prefix "MINI" ;; 不能为空，否则无法启动minimap窗口
        minimap-hide-fringes t
        minimap-hide-scroll-bar t
        minimap-update-delay 1.0
        minimap-window-location 'right
        minimap-display-semantic-overlays nil
        minimap-enlarge-certain-faces nil))

;; =============================================================================
;; Powerline
;; 其提供了一个漂亮的mode line皮肤，缺点是当字体太多或字太多时，会显示不下所有内容
;; -----------------------------------------------------------------------------
(when (and (member 'powerline package-selected-packages)
           (require 'powerline nil t))
  (setq powerline-arrow-shape
        ;; 'arrow
        ;; 'curve
        'arrow14))

;; =============================================================================
;; Paredit
;; -----------------------------------------------------------------------------
(when (and (member 'paredit package-selected-packages)
           (require 'paredit nil t))
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  (add-hook 'org-mode 'enable-paredit-mode))

;; =============================================================================
;; 配置杂项
;; -----------------------------------------------------------------------------
;; Account
(setq user-full-name "TonyLYan"
      user-mail-address "wangtonylyan@outlook.com")

;; UI
(setq inhibit-startup-message 1) ;; 取消启动界面
(tool-bar-mode -1) ;; 取消工具栏
(setq frame-title-format "emacs@%b") ;; 设置标题栏显示为buffer名字
;; 关于smooth scrolling可以参考: http://www.emacswiki.org/emacs/SmoothScrolling
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
      scroll-preserve-screen-position 1)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)
(scroll-bar-mode -1) ;; 取消滚动条
(mouse-avoidance-mode 'animate) ;; 当光标移动至鼠标位置时，为避免遮挡视线，自动移开鼠标
;; (save-place-mode 1) ;; 记录光标在每个文件中最后一次访问时所在的位置
(setq-default line-spacing 0) ;; 行距
(column-number-mode 1) ;; 在mode-line显示列数

(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode 1) ;; 左右括号相匹配显示
(setq show-paren-style 'parentheses)
(setq debug-on-error t) ;; 显示错误信息
(fset 'yes-or-no-p 'y-or-n-p) ;; 以y/n替换yes/no
(setq visible-bell t) ;; 以窗口闪烁的方式代替错误提示音
(global-visual-line-mode 1)

;; (set-face-background 'default "#C7EDCC") ;; 设置背景颜色为绿色护眼色
;; 字体的名字源自于.ttf或.otf文件内自带的元信息，包括family和style等
;; 以下使用不同的中英文字体和字号的目的是为了提升美观性，例如同一字体下的中文字符通常都比英文字符更高
(if (eq system-type 'windows-nt)
    (progn
      (if (> emacs-major-version 24)
          (progn
            ;; Windows系统上的Emacs25版本对中文字体的显示存在问题，打开中文文档时会存在卡顿的现象
            ;; 必须手动指定中文字体为宋体才可避免。
            (set-default-font "Consolas 11")
            (set-fontset-font "fontset-default" 'unicode "宋体 10"))
        (progn
          (set-default-font "Consolas 11")
          (set-fontset-font "fontset-default" 'unicode "Microsoft YaHei Mono 10"))))
  (progn
    (set-default-font "YaHei Consolas Hybrid 10")
    (set-fontset-font "fontset-default"
                      'unicode "Source Han Serif SC SemiBold 9") ;; 或替换成"Microsoft YaHei Mono 10"
    ))
;; (set-face-attribute 'default nil :family "Microsoft YaHei Mono" :weight 'normal :height 110) ;; 设置字体，包括字号等
;; (set-frame-font "10" nil t) ;; 设置字号, 同(set-face-attribute)中的:height

;; Edit
(setq-default indent-tabs-mode nil ;; make indentation commands use space only
              tab-width 4)
;; (setq tab-always-indent t)
(electric-indent-mode -1) ;; 取消全局性的自动缩进模式
(global-highlight-changes-mode 1)
(setq highlight-changes-global-changes-existing-buffers nil
      highlight-changes-visibility-initial-state t
      highlight-changes-face-list nil
      highlight-changes-colors nil)
;; 每次保存buffer时都将删除现有的改动高亮
;; 替换成这两个hook就会无效，原因未知：write-content-functions或write-file-functions
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;; 删除每行末尾的空格
            (highlight-changes-remove-highlight (point-min) (point-max))))
(setq require-final-newline t)
;; (global-font-lock-mode -1) ;; 取消全局性的语法高亮模式
;; (add-hook 'xxx-mode-hook 'turn-on-font-lock) ;; 可以通过注册hook的方式在特定模式下启用
;; (global-linum-mode 1)
;; (add-hook 'xxx-mode-hook 'linum-mode)

;; Backup and Revert
(setq make-backup-files t ;; 启用自动备份
      version-control t ;; 启用版本控制，即可以备份多次
      kept-old-versions 1 ;; 备份最旧的版本个数
      kept-new-versions 1 ;; 备份最新的版本个数
      delete-old-versions t
      dired-kept-versions 2
      backup-directory-alist '(("." . "~/.emacs.d.backups")) ;; 设置备份文件的路径
      backup-by-copying t) ;; 备份设置方式为直接拷贝
(global-auto-revert-mode 1) ;; 当硬盘上的文件被修改后，Emacs会提示用户重新读取该文件

;; File Extension
;; (setq auto-mode-alist (cons '("\\.emacs\\'" . emacs-lisp-mode) auto-mode-alist))

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
(global-unset-key (kbd "C-x C-l")) ;; (downcase-region)
(global-unset-key (kbd "C-x C-u")) ;; (upcase-region)
(put 'downcase-region 'disabled nil) ;; 去除每次执行此命令时的提示，强制执行
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
;; 此外，还有helm模式可以提供补全功能，只不过它并不局限于在minibuff中输出信息
;; https://github.com/emacs-helm/helm
(icomplete-mode -1)
(when (require 'ido nil t)
  (ido-mode 1)
  (ido-everywhere -1) ;; 仅使ido支持find-file和switch-to-buffer
  (setq ido-enable-flex-matching t
        ido-enable-prefix t
        ido-enter-matching-directory nil))
;; uniquify-mode可以为重名的buffer命名
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil ;; 不使用image方式
      speedbar-show-unknown-files t)

(provide 'my-init)

;; =============================================================================
;; 加载其他配置文件
(let ((path my-user-emacs-directory))
  (mapc (lambda (name)
          (load (concat path name) t nil nil t))
        '(
          "prog" ;; prog-mode
          ;; "prog-cc" ;; cc-mode (c-mode, c++-mode, java-mode)
          ;; "prog-lisp" ;; lisp-mode, emacs-lisp-mode, lisp-interaction-mode
          "prog-py" ;; python-mode
          ;; "prog-hs" ;; haskell-mode
          ;; "text-tex" ;; tex-mode, latex-mode
          ;; "web-browser" ;; web browser
          )))

(message "emacs init time = %s" (emacs-init-time))
