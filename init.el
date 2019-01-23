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
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;; 以下使用国内的清华镜像源
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  ;; 以下列表用于设置被允许加载的插件，因此无论是在安装还是使用插件的过程中
  ;; 都必须提前详细地列举出所有的插件，且要根据插件之间的依赖关系进行先后地声明
  (setq package-load-list '(all ;; e.g. (dash) (epl) (let-alist) (pkg-info) (flycheck)
                            ))
  ;; 设置加载上述列表中所指定插件的时机
  (setq package-enable-at-startup nil) ;; 方式1) 随Emacs的启动而自动加载插件
  (package-initialize) ;; 方式2) 主动执行该函数以加载插件
  ;; 目前使用此全局变量来管理插件的启用/禁用，其中包括了ELPA更新源中所没有的插件
  (setq package-selected-packages '(;; [UI]
                                    dashboard
                                    ;; all-the-icons-dired
                                    spaceline ;; powerline, spaceline-all-the-icons
                                    ;; smart-mode-line, smart-mode-line-powerline-theme
                                    doom-themes ;; atom-one-dark-theme, github-theme, solarized-theme, zenburn-theme
                                    ;; doom-themes-neotree
                                    rainbow-delimiters
                                    ;; rainbow-identifiers ;; 会覆盖配色主题所使用的字体颜色
                                    highlight-thing
                                    ;; nlinum-hl
                                    ;; yascroll
                                    tabbar
                                    treemacs ;; neotree, sr-speedbar, ecb
                                    ;; sublimity, minimap
                                    ;; fill-column-indicator, whitespace
                                    buffer-move
                                    zoom
                                    ;; [Edit]
                                    hydra
                                    avy ;; ace-jump-mode
                                    ;; ace-pinyin
                                    undo-tree
                                    smart-hungry-delete
                                    paredit
                                    ;; evil
                                    bm
                                    helm-bm
                                    helm ;; icomplete, anything, ido, smex, ivy
                                    flyspell
                                    ;; flyspell-correct-helm
                                    ediff ;; vdiff
                                    ;; [Programming]
                                    yasnippet
                                    flycheck ;; flymake
                                    helm-flycheck
                                    company ;; auto-complete
                                    company-jedi
                                    helm-gtags ;; ggtags
                                    asn1-mode
                                    ;; [Project]
                                    projectile ;; eproject
                                    helm-projectile
                                    magit
                                    ;; vdiff-magit
                                    cmake-mode ;; cmake-ide, cmake-project
                                    cmake-font-lock
                                    ;; [C, C++]
                                    cpputils-cmake
                                    ;; stickyfunc-enhance
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
                                    diminish
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

(use-package sublimity
  :if (my/package-enabled-p 'sublimity)
  :init
  (setq sublimity-map-size 17
        sublimity-map-max-fraction 0.2
        sublimity-map-text-scale -7)
  :config
  ;; (require 'sublimity-scroll)
  ;; (require 'sublimity-attractive)
  (require 'sublimity-map nil t)
  (sublimity-map-set-delay 5)
  (sublimity-mode 1))

(use-package minimap
  :if (my/package-enabled-p 'minimap)
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
  :if (and (my/package-enabled-p 'flyspell)
           (my/locate-exec "aspell"))
  :config
  (setq ispell-program-name (my/locate-exec "aspell") ;; 设置后台支持程序
        ;; ispell-dictionary "english" ;; default dictionary
        ;; ispell-personal-dictionary ""
        flyspell-issue-message-flag nil)
  (my/add-mode-hook "text" flyspell-mode)
  ;; (my/add-mode-hook "prog" flyspell-prog-mode)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN" . "^#+END") t))

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

(defconst my/key-binding-file (my/get-user-emacs-file "key-bindings.el"))
(my/load-file my/key-binding-file)

;; File Extension
;; (setq auto-mode-alist (cons '("\\.emacs\\'" . emacs-lisp-mode) auto-mode-alist))

(use-package windmove
  :ensure t
  :commands (windmove-left
             windmove-right
             windmove-up
             windmove-down)
  :config
  ;; <shift-up/down/left/right>
  (windmove-default-keybindings))

(use-package winner
  :ensure t
  :config
  (winner-mode 1))

(use-package buffer-move
  :commands (buf-move-left
             buf-move-right
             buf-move-up
             buf-move-down)
  :if (my/package-enabled-p 'buffer-move))

(use-package zoom
  :diminish zoom-mode
  :commands (zoom)
  :if (my/package-enabled-p 'zoom)
  :init
  (setq zoom-minibuffer-preserve-layout nil)
  :config
  ;; (zoom-mode 1)
  )

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
  :if (my/package-enabled-p 'helm)
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
  (setq helm-split-window-side-p t
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
  :preface
  (defvar pkg/projectile/switch-hook '())
  (defun pkg/projectile/switch-action ()
    (run-hooks 'pkg/projectile/switch-hook))
  :if (my/package-enabled-p 'projectile)
  :init
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-project-search-path pvt/project/root-directories
        projectile-switch-project-action 'pkg/projectile/switch-action)
  :config
  ;; 输入"C-c p C-h"可以查询所有'projectile-mode-map中的快捷键，常用的有
  ;; p :: (helm-projectile-switch-project)
  ;; d :: (helm-projectile-find-dir)
  ;; D :: (projectile-dired)
  ;; f :: (helm-projectile-find-file)
  ;; l :: (projectile-find-file-in-directory)
  ;; b :: (helm-projectile-switch-to-buffer)
  ;; k :: (projectile-kill-buffers)
  ;; o :: (projectile-multi-occur)
  ;; r :: (projectile-replace)
  ;; e :: (helm-projectile-recentf)
  ;; ! :: (projectile-run-shell-command-in-root)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1)
  ;; (add-to-list 'projectile-other-file-alist '("html" "js"))
  ;; 使用helm-projectile包装原projectile插件
  ;; 包括替换'projectile-mode-map中的快捷键
  (use-package helm-projectile
    :after helm
    :demand t ;; 初始化后就立即启用，基于project的方式管理各类文件
    :if (my/package-enabled-p 'helm-projectile)
    :init
    (setq helm-projectile-fuzzy-match t
          projectile-completion-system 'helm)
    :config
    (add-hook 'pkg/projectile/switch-hook 'helm-projectile t)
    (helm-projectile-on)))

(use-package magit
  :if (my/package-enabled-p 'magit)
  :bind (("C-c g" . magit-status))
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

(use-package sr-speedbar
  :if (my/package-enabled-p 'sr-speedbar)
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

(use-package treemacs
  :commands (treemacs-select-window)
  :if (my/package-enabled-p 'treemacs)
  :init
  (setq treemacs-python-executable          (my/locate-exec "python")
        treemacs-persist-file               (my/set-user-emacs-file ".cache/treemacs")
        treemacs-display-in-side-window     t
        treemacs-is-never-other-window      t
        treemacs-no-delete-other-windows    t
        treemacs-position                   'left
        treemacs-width                      35
        treemacs-show-cursor                nil
        treemacs-indentation                1
        treemacs-indentation-string         " "
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          nil
        treemacs-no-png-images              nil
        treemacs-space-between-root-nodes   nil
        treemacs-collapse-dirs              (if treemacs-python-executable 3 0)
        treemacs-follow-after-init          t
        treemacs-project-follow-cleanup     nil
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-follow-recenter-distance   0.2
        treemacs-file-follow-delay          1
        treemacs-file-event-delay           5000
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-tag-follow-delay           1.5
        treemacs-tag-follow-cleanup         t
        treemacs-silent-refresh             t
        treemacs-silent-filewatch           t
        treemacs-deferred-git-apply-delay   0.5
        treemacs-git-command-pipe           ""
        treemacs-max-git-entries            5000)
  :config
  (treemacs-resize-icons 10)
  ;; (treemacs-follow-mode 1)
  ;; (treemacs-tag-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode 1)
  (bind-keys :map treemacs-mode-map
             ([mouse-1] . treemacs-single-click-expand-action))
  (use-package treemacs-icons-dired
    :ensure t
    :config
    (treemacs-icons-dired-mode))
  (use-package treemacs-projectile
    :after projectile
    :if (my/package-enabled-p 'projectile))

  ;; todo: key bindings
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  )

(use-package neotree
  :ensure all-the-icons
  :preface
  (defun pkg/neotree/toggle ()
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
        (user-error "*neotree* could not find projectile project"))))
  :if (my/package-enabled-p 'neotree)
  :bind (("C-S-s" . pkg/neotree/toggle))
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
  (when (boundp 'pkg/projectile/switch-hook)
    (add-hook 'pkg/projectile/switch-hook 'neotree-projectile-action t)))

(use-package org
  :commands (org-capture
             org-agenda)
  :init
  (setq org-src-fontify-natively t)
  :config
  (my/add-mode-hook "org" 'org-indent-mode))

(use-package ace-jump-mode
  :if (my/package-enabled-p 'ace-jump-mode)
  :bind (("C-:" . ace-jump-mode-pop-mark)
         ("C-'" . ace-jump-char-mode))
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package avy
  :if (my/package-enabled-p 'avy)
  :bind (("C-:" . avy-goto-char-timer) ;; (avy-goto-char)
         ("C-'" . avy-pop-mark))
  :config
  ;; (avy-setup-default)
  (setq avy-timeout-seconds 0.5))

(use-package ace-pinyin
  :preface
  (defvar pkg/ace-pinyin/enabled-p
    (or (my/package-enabled-p 'ace-jump-mode)
        (my/package-enabled-p 'avy)))
  :if (and pkg/ace-pinyin/enabled-p
           (my/package-enabled-p 'ace-pinyin))
  :config
  (when (eq pkg/ace-pinyin/enabled-p 'ace-jump-mode)
    (setq ace-pinyin-use-avy nil))
  (ace-pinyin-global-mode 1))

(use-package undo-tree
  :diminish undo-tree-mode
  :if (my/package-enabled-p 'undo-tree)
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
  :ensure t
  :if (my/package-enabled-p 'smart-hungry-delete)
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :config
  (smart-hungry-delete-add-default-hooks))

(use-package paredit
  :diminish paredit-mode
  :if (my/package-enabled-p 'paredit)
  :config
  (mapc (lambda (mode)
          (my/add-mode-hook mode 'enable-paredit-mode))
        '("org" "lisp" "elisp" "ilisp" "slime" "scheme")))

(use-package highlight-thing
  :diminish highlight-thing-mode
  :if (my/package-enabled-p 'highlight-thing)
  :init
  (setq highlight-thing-what-thing 'symbol
        highlight-thing-exclude-thing-under-point t
        highlight-thing-delay-seconds 0.5
        highlight-thing-limit-to-defun nil
        highlight-thing-case-sensitive-p t)
  :config
  ;; (global-hl-line-mode -1)
  (my/add-mode-hook "text" 'hl-line-mode)
  (my/add-mode-hook "prog" 'highlight-thing-mode))

(use-package evil
  :if (my/package-enabled-p 'evil)
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
      '(;; [programming]
        "prog" ;; prog-mode
        "prog-cc" ;; cc-mode
        ;; lisp-mode, emacs-lisp-mode, lisp-interaction-mode,
        ;; sml-mode, haskell-mode
        "prog-fun"
        ;; "prog-py" ;; python-mode
        ;; "prog-web" ;; web-mode
        ;; [others]
        ;; "text-tex" ;; tex-mode, latex-mode
        ;; "web-browser" ;; web browser
        ))

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

(use-package dashboard
  :if (my/package-enabled-p 'dashboard)
  :init
  (setq dashboard-banner-logo-title "Welcome to Emacs"
        dashboard-startup-banner 'official
        dashboard-items '((recents  . 10)
                          (bookmarks . 10)
                          (projects . 10)))
  :config
  (dashboard-setup-startup-hook))

(use-package all-the-icons
  :ensure t
  :init
  ;; 此插件在首次使用前需要额外地安装字体，否则启用后mode-line中的图片会显示为乱码
  ;; 执行以下命令会自动下载并安装所需字体，Windows上只能手动执行
  ;; 但目前发现Linux上会因权限问题而导致安装失败，因此仍推荐手动执行
  ;; 字体下载目录默认为HOME/.local/share/fonts
  ;; (all-the-icons-install-fonts)
  :config
  (when (and (my/package-enabled-p 'all-the-icons-dired)
             (require 'all-the-icons-dired nil t))))

(use-package powerline
  :if (my/package-enabled-p 'powerline)
  :config
  (setq powerline-default-separator 'arrow
        powerline-default-separator-dir '(left . right))
  ;; (powerline-center-theme)
  ;; (powerline-center-evil-theme)
  ;; (powerline-vim-theme)
  ;; (powerline-nano-theme)
  (powerline-default-theme))

(use-package spaceline
  :if (or (my/package-enabled-p 'spaceline)
          (my/package-enabled-p 'spaceline-all-the-icons))
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-emacs-theme) ;; (spaceline-spacemacs-theme)
  (eval-after-load 'helm '(spaceline-helm-mode))
  (use-package spaceline-all-the-icons
    :if (my/package-enabled-p 'spaceline-all-the-icons)
    :config
    (spaceline-all-the-icons-theme)
    ;; (spaceline-all-the-icons--setup-neotree)
    (spaceline-all-the-icons--setup-package-updates)))

(use-package smart-mode-line
  :if (or (my/package-enabled-p 'smart-mode-line)
          (my/package-enabled-p 'smart-mode-line-powerline-theme))
  :config
  (setq sml/theme
        (if (and (my/package-enabled-p 'smart-mode-line-powerline-theme)
                 (require 'smart-mode-line-powerline-theme nil t))
            'powerline 'automatic)
        sml/no-confirm-load-theme t
        sml/shorten-directory t
        sml/shorten-modes t)
  (smart-mode-line-enable))

(let* ((dir (my/set-user-emacs-file "theme/" t))
       (dir (my/locate 'exist dir nil t))
       (path (my/get-file-path dir)))
  (when path
    (add-to-list 'custom-theme-load-path path)))
(use-package atom-one-dark-theme
  :if (my/package-enabled-p 'atom-one-dark-theme)
  :config
  (load-theme 'atom-one-dark t))
(use-package doom-themes
  :if (my/package-enabled-p 'doom-themes)
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil
        ;; 'doom-one
        ;; 'doom-spacegrey
        ;; 'doom-nova
        ;; 'doom-spacegrey
        doom-spacegrey-brighter-modeline t
        doom-spacegrey-brighter-comments t
        doom-spacegrey-comment-bg nil)
  (load-theme 'doom-spacegrey t)
  (when visible-bell
    (doom-themes-visual-bell-config)))
(use-package github-theme
  :if (my/package-enabled-p 'github-theme)
  :init
  (setq github-override-colors-alist '(("github-white" . "#FBF9E1")
                                       ("github-comment" . "#009E73")
                                       ("github-text" . "#000000")))
  :config
  (load-theme 'github t))
(use-package solarized-theme
  :if (my/package-enabled-p 'solarized-theme)
  :init
  (setq solarized-distinct-fringe-background nil
        solarized-distinct-doc-face t
        solarized-high-contrast-mode-line t
        solarized-use-more-italic t
        solarized-emphasize-indicators t)
  :config
  ;; (load-theme 'solarized-dark t)
  (load-theme 'solarized-light t))
(use-package zenburn-theme
  :if (my/package-enabled-p 'zenburn-theme)
  :init
  ;; (setq zenburn-override-colors-alist '(("zenburn-fg" . "#EDEDDD")))
  :config
  (load-theme 'zenburn t))

(when (my/package-enabled-p 'neotree)
  (with-eval-after-load 'neotree
    (cond
     ((my/package-enabled-p 'doom-themes-neotree)
      (use-package doom-themes
        :config
        (doom-themes-neotree-config)))
     ((my/package-enabled-p 'spaceline-all-the-icons)
      (use-package spaceline-all-the-icons
        :config
        (spaceline-all-the-icons--setup-neotree))))))

(use-package nlinum-hl
  :if (my/package-enabled-p 'nlinum-hl)
  :config
  (run-with-idle-timer 5 t 'nlinum-hl-flush-window)
  (run-with-idle-timer 30 t 'nlinum-hl-flush-all-windows)
  (add-hook 'focus-in-hook 'nlinum-hl-flush-all-windows)
  (add-hook 'focus-out-hook 'nlinum-hl-flush-all-windows)
  (advice-add 'select-window :before 'nlinum-hl-do-flush)
  (advice-add 'select-window :after 'nlinum-hl-do-flush))

(use-package yascroll
  :if (my/package-enabled-p 'yascroll)
  :init
  (setq yascroll:delay-to-hide nil)
  :config
  (add-to-list 'yascroll:disabled-modes 'neotree-mode)
  (global-yascroll-bar-mode 1))

;; 嵌套的括号通过大小而不仅是颜色来进行区分
(use-package rainbow-delimiters
  :if (my/package-enabled-p 'rainbow-delimiters)
  :config
  (my/add-mode-hook "prog" 'rainbow-delimiters-mode))

;; 修改默认字体颜色，从而将文字与符号区分开来
(use-package rainbow-identifiers
  :if (my/package-enabled-p 'rainbow-identifiers)
  :init
  (setq rainbow-identifiers-face-count 1)
  :config
  (my/add-mode-hook "prog" 'rainbow-identifiers-mode))

(use-package whitespace
  :if (my/package-enabled-p 'whitespace)
  :init
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column 80))

(use-package fill-column-indicator
  :if (my/package-enabled-p 'fill-column-indicator)
  :init
  (setq fci-rule-use-dashes nil
        fci-rule-column 100)
  :config
  (define-globalized-minor-mode global-fci-mode fci-mode
    ;; 避免在special buffers、dired、shell等特殊模式下启用
    (lambda () (when buffer-file-name (fci-mode 1))))
  (global-fci-mode 1))

(use-package tabbar
  :if (my/package-enabled-p 'tabbar)
  :config
  (tabbar-mode 1)
  (bind-keys :map tabbar-mode-map
             ("C-c C-b" . tabbar-backward-tab)
             ("C-c C-f" . tabbar-forward-tab)
             ("C-c C-p" . tabbar-backward-group)
             ("C-c C-n" . tabbar-forward-group)))

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
