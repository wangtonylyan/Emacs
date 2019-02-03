;; -*- coding: utf-8 -*-

;; 'emacs-major-version, 'emacs-minor-version

;; prefix convention
;; my :: global
;; my/prog, my/prog-cc, ... :: file-local
;; pvt :: private
;; pkg :: package

;; file, path ::
;; directory :: 以斜杠结尾
;; dwim :: Do What I Mean

(defun my/map (func seq)
  (delq nil (mapcar func seq)))

(defun my/mapcar (func seq)
  (car (my/map func seq)))

(defalias 'my/get-file-directory 'file-name-directory)

(defun my/get-file-path (file)
  (when (stringp file)
    (let ((dir (my/get-file-directory file)))
      (when dir (directory-file-name dir)))))

(defun my/concat-directory-file (dir file)
  (let ((dir (when (stringp dir)
               (file-name-as-directory dir))))
    (concat dir file)))

;; 'nil = '(), satisfy both (string-or-null-p) and (listp)
(defun my/directory-exists-p (file &optional dirs)
  (cond
   ((string-or-null-p dirs) ;; include the case of '()
    (let ((file (my/concat-directory-file dirs file)))
      (when (and (file-directory-p file)
                 (file-accessible-directory-p file))
        (expand-file-name (file-name-as-directory file)))))
   ((listp dirs)
    (my/map (lambda (dir)
              (my/directory-exists-p file dir))
            dirs))))

(defun my/path-exists-p (file &optional dirs)
  (let ((dirs (my/directory-exists-p file dirs)))
    (if (listp dirs) ;; include the case of 'nil
        (my/map (lambda (dir)
                  (when dir (directory-file-name dir)))
                dirs)
      (directory-file-name dirs))))

(defun my/file-exists-p (file &optional dirs)
  (cond
   ((string-or-null-p dirs) ;; in 'default-directory by default
    (let ((file (my/concat-directory-file dirs file)))
      (when (file-regular-p file)
        (expand-file-name file))))
   ((listp dirs)
    (my/map (lambda (dir)
              (my/file-exists-p file dir))
            dirs))))

(defun my/exists-p (file &optional dirs)
  (or (my/directory-exists-p file dirs)
      (my/file-exists-p file dirs)))

(defun my/locate (type dir file add)
  (let ((file (my/concat-directory-file dir file)))
    (cond
     ;; 1. locate directory or file, in 'load-path by default
     ((equal type 'exist)
      (let ((file (or (my/exists-p file)
                      (car (my/exists-p file load-path)))))
        (when add
          (cond ((my/directory-exists-p file)
                 (add-to-list 'load-path (my/path-exists-p file)))
                ((my/file-exists-p file)
                 (add-to-list 'load-path (my/get-file-path file)))))
        file))
     ;; 2. locate readable file, in 'load-path by default
     ((equal type 'file)
      (let ((file (or (my/file-exists-p file)
                      (locate-file file load-path)))) ;; as-is the 'file
        (when (and file (file-readable-p file))
          (when add (add-to-list 'load-path (my/get-file-path file)))
          file)))
     ;; 3. locate executable file, in 'exec-path by default
     ((equal type 'exec)
      (let ((file (or (my/file-exists-p file)
                      (executable-find file))))
        (when (and file (file-executable-p file))
          (when add (add-to-list 'exec-path (my/get-file-path file)))
          file)))
     (t (user-error "*my/locate* TYPE=%s FILE=%s" type file)))))

(defun my/locate-file (file &optional dir add)
  (my/locate 'file dir file add))

(defun my/locate-exec (file &optional dir add)
  (my/locate 'exec dir file add))

;; 目前暂没有对于该函数实现上的额外需求
;; (load)本身的实现逻辑就类似于(my/locate-file)
(defun my/load-file (file &optional dir) ;; todo
  (load (my/concat-directory-file dir file) t))

(defun my/package-enabled-p (pkg)
  (car (package--user-selected-p pkg)))

(defalias 'my/minor-mode-on-p 'bound-and-true-p)

(defconst my/mode-hook-dict
  (let ((dict (make-hash-table :test 'equal)))
    (mapc (lambda (tup) (puthash (car tup) (cadr tup) dict))
          '(;; [edit]
            ("text"           text-mode-hook            )
            ("org"            org-mode-hook             )
            ;; [programming]
            ("prog"           prog-mode-hook            )
            ("yas"            yas-minor-mode-hook       ) ;; when enabled as local mode
            ("YAS"            yas-global-mode-hook      ) ;; when enabled as global mode
            ("flycheck"       flycheck-mode-hook        )
            ("SEMANTIC"       semantic-init-hook        ) ;; when enabled as global mode
            ("semantic"       semantic-init-mode-hook   ) ;; mode-local hook
            ;; [language]
            ("lisp"           lisp-mode-hook            )
            ("elisp"          emacs-lisp-mode-hook      )
            ("ilisp"          lisp-interaction-mode-hook)
            ("slime"          slime-mode-hook           )
            ("scheme"         scheme-mode-hook          )
            ("cc"             c-mode-common-hook        ) ;; c, c++, objc, java, idl, pike, awk
            ("c"              c-mode-hook               )
            ("c++"            c++-mode-hook             )
            ("python"         python-mode-hook          )
            ("elpy"           elpy-mode-hook            )
            ("sml"            sml-mode-hook             )
            ("haskell"        haskell-mode-hook         )
            ;; [others]
            ("w3m"            w3m-mode-hook             )
            ;; [my]
            ("my/prog"        my/prog/start-hook        )
            ("my/prog-cc"     my/prog-cc/start-hook     )
            ("my/prog-py"     my/prog-py/start-hook     )
            ))
    dict))

(defun my/get-mode-hook (mode)
  (let ((hook (gethash mode my/mode-hook-dict)))
    (unless hook (user-error "*my/get-mode-hook* MODE=%s" mode))
    hook))

(defun my/add-mode-hook (mode func &optional local)
  (add-hook (my/get-mode-hook mode) func t local))

(defun my/del-mode-hook (mode func &optional local)
  (remove-hook (my/get-mode-hook mode) func local))

(defun my/add-modes-hook (list)
  (mapc (lambda (elem)
          (my/add-mode-hook (car elem) (cadr elem) (caddr elem)))
        list))

(defun my/run-mode-hook (mode)
  (run-hooks (my/get-mode-hook mode)))

(cond ;; os-related
 ((eq system-type 'windows-nt)
  (mapc (lambda (dir)
          (let ((path (my/path-exists-p dir)))
            (when path (add-to-list 'exec-path path))))
        '("D:/softwares"))
  (let ((path (my/locate-exec "cmdproxy.exe"
                              "Emacs25/libexec/emacs/24.5/i686-pc-mingw32")))
    (when path
      (setq shell-file-name path
            shell-command-switch "-c")))
  (my/locate-exec "git.exe" "Git" t))
 ((eq system-type 'gnu/linux)
  (mapc (lambda (dir)
          (let ((path (my/path-exists-p dir)))
            (when path (add-to-list 'exec-path path))))
        '("~/.local/bin"))))

;; (setq user-init-file "~/.emacs.d/init.el")
;; (load user-init-file)
(setq default-directory "~/"
      user-emacs-directory "~/.emacs.d/"
      command-line-default-directory default-directory)
(setq-default default-directory default-directory
              user-emacs-directory user-emacs-directory)

(defconst my/self-emacs-directory
  (my/locate 'exist user-emacs-directory "my-emacs/" t))

(defun my/set-user-emacs-file (file &optional self)
  (let ((dir (if self my/self-emacs-directory
               user-emacs-directory)))
    (when dir (my/concat-directory-file dir file))))

(defun my/get-user-emacs-file (&optional file self)
  (my/exists-p (my/set-user-emacs-file file self)))

(defconst my/private-emacs-directory
  (my/get-user-emacs-file ".private/"))

(defun my/get-private-emacs-file (file)
  (let ((dir my/private-emacs-directory))
    (when dir (my/exists-p file dir))))

(my/load-file (my/get-private-emacs-file "init.el"))
;; *************************** sample code in .private/init.el ***************************
;; (defvar pvt/project/root-directories '("~/Projects/" "~/projects/"))
;; (defvar pvt/project/ede-config-file-names '("ede-projects.el"))
;; ***************************************************************************************

(defconst pvt/project/root-directories
  (when (boundp 'pvt/project/root-directories)
    (my/map 'my/directory-exists-p pvt/project/root-directories)))

(defconst pvt/project/ede-config-files
  (when (and pvt/project/root-directories
             (boundp 'pvt/project/ede-config-file-names))
    (mapcan (lambda (file)
              (my/file-exists-p file pvt/project/root-directories))
            pvt/project/ede-config-file-names)))

;; (normal-top-level-add-subdirs-to-load-path)
;; (normal-top-level-add-to-load-path)

;; 指定由(customize)写入配置信息的文件，随后每当Emacs自动写入时就不会再修改当前文件了
(setq custom-file (my/get-user-emacs-file "custom.el"))
(my/load-file custom-file)

(setq url-max-password-attempts 2
      ;; 不支持authentication
      ;; url-proxy-services '(("http" . "10.25.71.1:8080"))
      )

(when (require 'package nil t)
  ;; 设置安装包的存储目录，该目录也需要被包含至'load-path中
  ;; (add-to-list 'package-directory-list "~/.emacs.d/elpa/" t) ;; system-wide dir
  (setq package-user-dir (my/set-user-emacs-file "elpa/")) ;; user-wide dir
  ;; Emacs使用的默认更新源为：("gnu" . "http://elpa.gnu.org/")
  ;; 添加更新源：MELPA每天更新，其包含了绝大多数插件
  ;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;; 以下使用国内的清华镜像源
  ;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
  ;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  ;; 以下列表用于设置被允许加载的插件，因此无论是在安装还是使用插件的过程中
  ;; 都必须提前详细地列举出所有的插件，且要根据插件之间的依赖关系进行先后地声明
  (setq package-load-list '(all ;; e.g. (dash) (epl) (let-alist) (pkg-info) (flycheck)
                            ))
  ;; 设置加载上述列表中所指定插件的时机
  (setq package-enable-at-startup nil) ;; 方式1) 随Emacs的启动而自动加载插件
  (package-initialize) ;; 方式2) 主动执行该函数以加载插件
  ;; 目前使用此全局变量来管理插件的启用/禁用，其中包括了ELPA更新源中所没有的插件
  (setq package-selected-packages '(;; [UI]
                                    all-the-icons
                                    diminish
                                    dashboard
                                    nyan-mode
                                    spaceline ;; spaceline-all-the-icons, smart-mode-line
                                    doom-themes ;; atom-one-dark-theme, github-theme, solarized-theme, zenburn-theme
                                    beacon
                                    ;; nlinum-hl
                                    ;; yascroll
                                    ;; sublimity, minimap
                                    ;; [Layout]
                                    tabbar ;; awesome-tab
                                    treemacs ;; neotree, sr-speedbar, ecb
                                    buffer-move
                                    ;; dimmer
                                    zoom
                                    ;; [Edit]
                                    ;; fill-column-indicator, whitespace
                                    rainbow-delimiters
                                    ;; rainbow-identifiers ;; 会覆盖配色主题所使用的字体颜色
                                    highlight-thing
                                    avy ;; ace-jump-mode
                                    ;; ace-pinyin
                                    undo-tree
                                    smart-hungry-delete
                                    paredit
                                    ;; evil
                                    flyspell
                                    ;; flyspell-correct-helm


                                    ;; [Tools]
                                    ;; all-the-icons-dired
                                    hydra
                                    helm ;; icomplete, anything, ido, smex, ivy
                                    bm
                                    helm-bm
                                    ediff ;; vdiff

                                    ;; [Project]
                                    projectile ;; eproject
                                    helm-projectile
                                    treemacs-projectile
                                    magit
                                    ;; vdiff-magit


                                    ;; [Programming]
                                    yasnippet
                                    flycheck ;; flymake
                                    helm-flycheck
                                    company ;; auto-complete
                                    company-jedi
                                    helm-gtags ;; ggtags
                                    asn1-mode
                                    ;; [C, C++]
                                    ;; stickyfunc-enhance
                                    cmake-mode ;; cmake-ide, cmake-project
                                    cmake-font-lock
                                    cpputils-cmake
                                    ;; [Python]
                                    ;; elpy ;; ropemacs
                                    ;; flycheck-pyflakes
                                    ;; py-autopep8
                                    ;; auto-virtualenvwrapper ;; virtualenvwrapper
                                    ;; [Haskell]
                                    haskell-mode
                                    hindent
                                    flycheck-haskell
                                    ;; [Standard ML]
                                    sml-mode
                                    ;; [HTML]
                                    ;; web-mode
                                    ;; [LaTeX]
                                    ;; auctex
                                    ;; pdf-tools
                                    ;; [Web]
                                    ;; w3m
                                    ;; erc ;; circe, rcirc
                                    use-package))
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install-selected-packages))

(eval-when-compile
  ;; disabled, diminish
  ;; ensure, after, demand, defer, commands
  ;; preface, if, init, config
  (require 'use-package))
(require 'bind-key)

(use-package all-the-icons
  :ensure t
  :init
  ;; 此插件在首次使用前需要额外地安装字体，否则启用后mode-line中的图片会显示为乱码
  ;; 执行以下命令会自动下载并安装所需字体，Windows上只能手动执行
  ;; 但目前发现Linux上会因权限问题而导致安装失败，因此仍推荐手动执行
  ;; 字体下载目录默认为HOME/.local/share/fonts
  ;; (all-the-icons-install-fonts)
  )

(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))

(use-package hydra
  :ensure t
  :demand t
  :preface
  (defun pkg/hydra/quit ()
    (interactive)
    (message "Hydra Quit"))
  :init
  ;; 目前发现启用此项会导致，Hydra子窗口过小，无法完整地呈现提示文字
  ;; 此外，启用全局的zoom mode似乎也可以避免该问题
  (setq hydra-lv nil))

;; =============================================================================
;; 配置杂项
;; -----------------------------------------------------------------------------
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
                    ((< rsltn (* 1920 1080)) 13)
                    ((< rsltn (* 2560 1440)) 14)
                    (t 16)))
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
(my/add-mode-hook "text" 'visual-line-mode)
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

(put 'downcase-region 'disabled nil) ;; 去除每次执行此命令时的提示，强制执行
(put 'upcase-region 'disabled nil)

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;; 删除每行末尾的空格
            (when (derived-mode-p 'lisp-mode 'emacs-lisp-mode)
              (indent-region (point-min) (point-max)))
            ;; 每次保存buffer时都将删除现有的改动高亮
            ;; 替换成另外两个hook就会无效，原因未知：write-content-functions或write-file-functions
            (highlight-changes-remove-highlight (point-min) (point-max)))
          t)

;; File Extension
;; (setq auto-mode-alist (cons '("\\.emacs\\'" . emacs-lisp-mode) auto-mode-alist))

(use-package icomplete
  :if (not (my/package-enabled-p 'icomplete))
  :config
  (icomplete-mode -1))

(use-package ido
  :if (my/package-enabled-p 'ido)
  :config
  (ido-mode 1)
  (ido-everywhere -1) ;; 仅使ido支持find-file和switch-to-buffer
  (setq ido-enable-prefix t
        ido-enable-flex-matching t
        ido-use-filename-at-point t
        ido-enter-matching-directory nil))

(use-package smex
  :if (my/package-enabled-p 'smex)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; 该插件会自动替换原M-x快捷键所绑定的命令，若想保留则可重新绑定之
         ("C-x M-x" . execute-extended-command))
  :config
  (smex-initialize))

(use-package helm
  :diminish helm-mode
  :ensure t
  :commands (helm-command-prefix
             helm-M-x
             helm-show-kill-ring
             helm-find-files
             helm-recentf
             helm-mini
             helm-buffers-list
             helm-occur)
  :if (my/package-enabled-p 'helm)
  :init
  (require 'helm-config)
  (setq helm-split-window-inside-p t
        helm-full-frame nil
        helm-use-frame-when-more-than-two-windows nil
        helm-display-header-line nil
        helm-echo-input-in-header-line nil
        helm-autoresize-max-height 30
        helm-autoresize-min-height 0
        helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t
        helm-mode-fuzzy-match nil ;; globally disabled
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
             ;; ("C-c C-f" . helm-follow-mode)
             :map minibuffer-local-map
             ("M-p" . helm-minibuffer-history)
             ("M-n" . helm-minibuffer-history))
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package ediff
  :after winner
  :commands (ediff-current-file
             ediff-files
             ediff-files3
             ediff-buffers
             ediff-buffers3)
  :if (my/package-enabled-p 'ediff)
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-make-buffers-readonly-at-startup nil
        ;; ediff-diff-options "" ;; "-w" = "##", "-i" = "#c"
        ;; ediff-forward-word-function 'forward-char ;; "@", "*"
        ediff-highlight-all-diffs nil)
  (defun pkg/ediff/setup-keymap ()
    ;; 可参考(ediff-setup-keymap)，或激活ediff后输入"?"
    ;; (bind-keys :map ediff-mode-map)
    ;; (unbind-key "" ediff-mode-map) ;; ()
    )
  (add-hook 'ediff-keymap-setup-hook 'pkg/ediff/setup-keymap t)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo t))

(use-package vdiff
  :commands (vdiff-current-file
             vdiff-files
             vdiff-files3
             vdiff-buffers
             vdiff-buffers3)
  :if (my/package-enabled-p 'vdiff)
  :init
  (setq vdiff-diff-algorithm (if (my/locate-exec "git") 'git-diff 'diff-u)
        vdiff-diff3-command '("diff3")
        vdiff-lock-scrolling t
        vdiff-default-refinement-syntax-code "w"
        vdiff-auto-refine nil
        vdiff-subtraction-style 'full
        vdiff-subtraction-fill-char ?\s
        vdiff-disable-folding nil
        vdiff-may-close-fold-on-point nil
        vdiff-min-fold-size 4
        vdiff-fold-padding 6
        vdiff-fold-string-function 'vdiff-fold-string-default)
  :config
  (define-key vdiff-mode-map (kbd "C-c d") vdiff-mode-prefix-map)
  (bind-keys :map vdiff-mode-map
             ("C-c d h" . vdiff-hydra/body)))

(use-package projectile
  :diminish projectile-mode
  :ensure t
  :defer t
  :preface
  (defvar pkg/projectile/switch-hook)
  (defun pkg/projectile/switch-action ()
    (run-hooks 'pkg/projectile/switch-hook))
  :init
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-project-search-path pvt/project/root-directories
        projectile-switch-project-action 'pkg/projectile/switch-action)
  :config
  ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1)
  ;; (add-to-list 'projectile-other-file-alist '("html" "js"))
  ;; 使用helm-projectile包装原projectile插件
  ;; 包括替换'projectile-mode-map中的快捷键
  (use-package helm-projectile
    :ensure t
    :after helm
    :commands (helm-projectile)
    :init
    (setq projectile-completion-system 'helm
          helm-projectile-fuzzy-match t)
    :config
    (add-hook 'pkg/projectile/switch-hook 'helm-projectile t)
    (helm-projectile-on)))

(use-package magit
  :commands (magit-status)
  :if (and (my/package-enabled-p 'magit)
           (my/locate-exec "git"))
  :init
  (setq magit-auto-revert-mode t
        magit-auto-revert-immediately t
        magit-auto-revert-tracked-only t
        magit-ediff-dwim-show-on-hunks t
        ;; magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        ;; 执行(magit-list-repositories)，可以打印出以下列表所指示的路径下搜索到的git项目
        magit-repository-directories
        (my/map (lambda (dir) (cons dir 1)) ;; directory depth = 1
                pvt/project/root-directories))
  :config
  (use-package vdiff-magit
    :if (and (my/package-enabled-p 'vdiff)
             (my/package-enabled-p 'vdiff-magit))
    :config
    (bind-keys :map magit-mode-map
               ("e" . vdiff-magit-dwim)
               ("E" . vdiff-magit-popup))
    (setcdr (assoc ?e (plist-get magit-dispatch-popup :actions))
            '("vdiff dwim" 'vdiff-magit-dwim))
    (setcdr (assoc ?E (plist-get magit-dispatch-popup :actions))
            '("vdiff popup" 'vdiff-magit-popup))))

(use-package bm
  :commands (bm-next
             bm-previous
             bm-toggle)
  :if (my/package-enabled-p 'bm)
  :config
  (unbind-key "C-x r")
  (setq bm-marker 'bm-marker-left
        bm-cycle-all-buffers t
        temporary-bookmark-p t
        bm-buffer-persistence t
        bm-restore-repository-on-load t
        bm-repository-file (my/set-user-emacs-file "bm-repository/"))
  (setq-default bm-buffer-persistence bm-buffer-persistence)
  (add-hook' after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'kill-emacs-hook '(lambda nil
                                (progn
                                  (bm-buffer-save-all)
                                  (bm-repository-save))))
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (use-package helm-bm
    :after helm
    :demand t
    :if (my/package-enabled-p 'helm-bm)
    :bind (("C-c b b" . helm-bm))))

(use-package org
  :commands (org-capture
             org-agenda)
  :init
  (setq org-src-fontify-natively t)
  :config
  (my/add-mode-hook "org" 'org-indent-mode))

(use-package eshell
  :config
  ;; (add-to-list 'eshell-visual-commands)
  (bind-key "C-c e"
            (lambda ()
              (interactive)
              (if (eq major-mode 'eshell-mode)
                  (progn
                    (insert "exit")
                    (eshell-send-input)
                    (delete-window))
                (progn
                  (let* ((dir (if (buffer-file-name)
                                  (my/get-file-directory (buffer-file-name))
                                default-directory))
                         (name (car (last (split-string dir "/" t)))))
                    (split-window-vertically (- (/ (window-total-height) 3)))
                    (other-window 1)
                    (eshell "new")
                    (rename-buffer (concat "*eshell: " name "*"))))))))

(provide 'my/init)

;; 加载其他配置文件
(mapc (lambda (file)
        (my/load-file (my/set-user-emacs-file file t)))
      '("init-edit"
        "prog" ;; prog-mode
        "prog-cc" ;; cc-mode
        ;; lisp-mode, emacs-lisp-mode, lisp-interaction-mode,
        ;; sml-mode, haskell-mode
        "prog-fun"
        ;; "prog-py" ;; python-mode
        ;; "prog-web" ;; web-mode
        ;; "text-tex" ;; tex-mode, latex-mode
        ;; "web-browser" ;; web browser
        "init-ui"
        "init-keys"))

;; =============================================================================
(use-package pdf-tools
  :if (my/package-enabled-p 'pdf-tools)
  :init
  (setq pdf-view-continuous t)
  :config
  (pdf-tools-install))

(use-package w3m
  :preface
  (defvar pkg/w3m/exists-p
    (if (eq system-type 'windows-nt)
        (my/locate-exec "w3m.exe" "w3m" t)
      (my/locate-exec "w3m")))
  :if (and (my/package-enabled-p 'w3m) pkg/w3m/exists-p)
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
  (my/add-mode-hook "w3m" 'visual-line-mode))

(use-package erc
  :if (my/package-enabled-p 'erc)
  :config
  ;; (unbind-key "<return>" erc-mode-map)
  (bind-keys :map erc-mode-map
             ("C-<return>" . erc-send-current-line))
  (setq erc-autojoin-channels-alist nil ;; '(("freenode.net" "#emacs"))
        erc-interpret-mirc-color t
        erc-kill-buffer-on-part t))

(use-package circe
  :if (my/package-enabled-p 'circe)
  :config
  (setq circe-network-options '(("Freenode" ;; http://freenode.net/
                                 :nick ""
                                 :sasl-username ""
                                 :sasl-password ""
                                 :channels ("#emacs" "#c_lang_cn")))))

;; =============================================================================
(message "emacs init time = %s" (emacs-init-time))
