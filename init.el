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

;; 指定由(customize)写入配置信息的文件，随后每当Emacs自动写入时就不会再修改当前文件了
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; 判断Emacs版本可以基于以下两个变量：'emacs-major-version和'emacs-minor-version

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

  ;; 目前使用此全局变量来管理插件的启用/禁用，其中包括了ELPA更新源中所没有的插件
  (setq package-selected-packages '(;; 1) enhancement
                                    atom-one-dark-theme
                                    ido ;; icomplete, anything, helm, smex, ivy
                                    ;; 2) programming
                                    yasnippet company
                                    flycheck ;; flymake
                                    magit
                                    elpy py-autopep8
                                    company-jedi
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
;; Minimap
;; 其全部的可配置选项见于(customize-group 'minimap)
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
;; (setq-default fill-column 80) ;; 超过80字符就换行显示

(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode 1) ;; 显示匹配的左右括号
(setq show-paren-style 'parentheses)
(setq debug-on-error t) ;; 显示错误信息
(fset 'yes-or-no-p 'y-or-n-p) ;; 以y/n替换yes/no
(setq visible-bell t) ;; 以窗口闪烁的方式代替错误提示音
(global-visual-line-mode -1) ;; 对中文支持不好

;; (set-face-background 'default "#C7EDCC") ;; 设置背景颜色为绿色护眼色
;; 字体的名字源自于.ttf或.otf文件内自带的元信息，包括family和style等
;; 以下使用不同的中英文字体和字号的目的是为了提升美观性，例如同一字体下的中文字符通常都比英文字符更高
(if (eq system-type 'windows-nt)
    (progn
      ;; Windows系统上的Emacs25版本对中文字体的显示存在问题，打开中文文档时会存在卡顿的现象
      ;; 必须手动指定中文字体为宋体才可避免。
      (set-default-font "Consolas 11")
      (set-fontset-font "fontset-default" 'unicode "宋体 10"))
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
;; 替换成另外两个hook就会无效，原因未知：write-content-functions或write-file-functions
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;; 删除每行末尾的空格
            (highlight-changes-remove-highlight (point-min) (point-max))))
(setq require-final-newline t)
(global-font-lock-mode 1) ;; 语法高亮
;; (add-hook 'xxx-mode-hook 'turn-on-font-lock) ;; (font-lock-mode 1)
;; (global-linum-mode 1) ;; 左侧行号，推荐仅将其显示于主要的编辑文档中
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

;; 以下插件提供了对于minibuffer中几个常用命令的补全功能
;; 包括(find-file), (switch-to-buffer), (execute-extended-command)等
;; 1) icomplete
;; Emacs内置，需要按下TAB键被动触发，且会将提示的内容呈现于一个新建的子窗口中
;; 2) Anything
;; 与icomplete类似，默认情况下同样需要额外地敲击按键或执行命令来触发
;; 优点是支持额外的源，可以提供更为强大的补全方案
;; 3) Helm
;; https://emacs-helm.github.io/helm/
;; 继承自并已代替了Anything
;; 4) IDO (interactively do things)
;; Emacs内置，自动呈现提示内容，但不支持(execute-extended-command)
;; 5) Smex
;; 基于IDO来实现，并作为其补充，提供了对于(execute-extended-command)的支持
;; 此外还提供了记录用户操作历史、快速查询输入命令的快捷键及函数说明等功能
;; 6) Ivy
;; 作为find-file-in-project、Swiper等插件的依赖库
(when (not (member 'icomplete package-selected-packages))
  (icomplete-mode -1))
(when (and (member 'ido package-selected-packages)
           (require 'ido nil t))
  (ido-mode 1)
  (ido-everywhere -1) ;; 仅使ido支持find-file和switch-to-buffer
  (setq ido-enable-flex-matching t
        ido-enable-prefix t
        ido-enter-matching-directory nil))
(when (and (member 'smex package-selected-packages)
           (require 'smex nil t))
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; 该插件会自动替换原M-x快捷键所绑定的命令，若仍想使用的话可以如下重新绑定之
  ;; (global-set-key (kbd "C-x M-x") 'execute-extended-command)
  )
(when (and (member 'helm package-selected-packages)
           (require 'helm nil t))
  )

;; uniquify-mode可以为重名的buffer命名
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil ;; 不使用image方式
      speedbar-show-unknown-files t)

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

;; 调整窗口大小
((lambda ()
   ;; 全屏
   ;; (interactive)
   ;; (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
   ;; '(2 "_NET_WM_STATE_FULLSCREEN" 0))
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
