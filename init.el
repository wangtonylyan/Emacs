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
(defun my/load-file (file &optional dir) ;; TODO
  (load (my/concat-directory-file dir file) t))

(defun my/package-enabled-p (pkg)
  (car (package--user-selected-p pkg)))

(defalias 'my/minor-mode-on-p 'bound-and-true-p)

(defconst my/mode-hook-dict
  (let ((dict (make-hash-table :test 'equal)))
    (mapc (lambda (tup) (puthash (car tup) (cadr tup) dict))
          '(;; [emacs]
            ("init"           after-init-hook           )
            ("my/init-gui"    my/init-gui/start-hook    )
            ("my/init-vis"    my/init-vis/start-hook    )
            ("my/prog"        my/prog/start-hook        )
            ("my/prog-util"   my/prog-util/start-hook   )
            ("my/prog-cc"     my/prog-cc/start-hook     )
            ("my/prog-py"     my/prog-py/start-hook     )
            ;; [edit]
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
            ("CC"             c-initialization-hook     ) ;; c, c++, objc, java, idl, pike, awk
            ("cc"             c-mode-common-hook        )
            ("c"              c-mode-hook               )
            ("c++"            c++-mode-hook             )
            ("python"         python-mode-hook          )
            ("elpy"           elpy-mode-hook            )
            ("sml"            sml-mode-hook             )
            ("haskell"        haskell-mode-hook         )
            ;; [others]
            ("DIRED"          dired-load-hook           )
            ("dired"          dired-mode-hook           )
            ("w3m"            w3m-mode-hook             )))
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
  (my/locate-exec "git.exe" "Git" t)
  (setq inhibit-compacting-font-caches t))
 ((eq system-type 'gnu/linux)
  (mapc (lambda (dir)
          (let ((path (my/path-exists-p dir)))
            (when path (add-to-list 'exec-path path))))
        '("~/.local/bin"))
  (let ((path (my/locate-exec "zsh")))
    (when path
      (setq shell-file-name path)))))

;; (setq user-init-file "~/.emacs.d/init.el")
;; (load user-init-file)
(setq default-directory (my/directory-exists-p "~/")
      user-emacs-directory (my/directory-exists-p "~/.emacs.d/")
      command-line-default-directory default-directory)
(setq-default default-directory default-directory
              user-emacs-directory user-emacs-directory)

(defconst my/self-emacs-directory
  (my/locate 'exist user-emacs-directory "my.emacs/" t))

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

(when (require 'package)
  ;; 设置安装包的存储目录，该目录也需要被包含至'load-path中
  ;; (add-to-list 'package-directory-list "~/.emacs.d/elpa/" t) ;; system-wide dir
  (setq package-user-dir (my/set-user-emacs-file "elpa/")) ;; user-wide dir
  (setq package-archives
        (let ((mirror
               ;; 'origin
	           'china ;; 'tsinghua
               ))
          (cond
           ((eq mirror 'origin)
            '(("gnu" . "http://elpa.gnu.org/packages/")
              ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
              ("melpa" . "http://melpa.org/packages/")
              ;; ("org" . "http://orgmode.org/elpa/")
              ))
           ((eq mirror 'china)
            '(("gnu" . "http://elpa.emacs-china.org/gnu/")
              ;; ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
              ("melpa" . "http://elpa.emacs-china.org/melpa/")
              ;; ("org" . "http://elpa.emacs-china.org/org/")
              ))
           ((eq mirror 'tsinghua)
            '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
              ;; ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
              ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
              ;; ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
              )))))
  ;; 以下列表用于设置被允许加载的插件，因此无论是在安装还是使用插件的过程中
  ;; 都必须提前详细地列举出所有的插件，且要根据插件之间的依赖关系进行先后地声明
  (setq package-load-list '(all ;; e.g. (dash) (epl) (let-alist) (pkg-info) (flycheck)
                            ))
  ;; 设置加载上述列表中所指定插件的时机
  (setq package-enable-at-startup nil) ;; 方式1) 随Emacs的启动而自动加载插件
  (package-initialize) ;; 方式2) 主动执行该函数以加载插件
  ;; 目前使用此全局变量来管理插件的启用/禁用，其中包括了ELPA更新源中所没有的插件
  (setq package-selected-packages
        '(;; =============================================================================
          ;; [interface] init-gui.el
          doom-themes ;; solarized-theme, zenburn-theme
          dashboard
          nyan-mode
          doom-modeline ;; spaceline, spaceline-all-the-icons, smart-mode-line
          tabbar ;; awesome-tab
          treemacs ;; neotree, sr-speedbar, ecb
          treemacs-projectile
          ;; treemacs-icons-dired
          ;; =============================================================================
          ;; [visual] init-vis.el
          beacon
          ;; nlinum-hl
          ;; yascroll
          ;; sublimity, minimap
          ;; whitespace, fill-column-indicator
          rainbow-delimiters
          ;; rainbow-identifiers ;; 会覆盖配色主题所使用的字体颜色
          highlight-thing
          zoom ;; dimmer
          ;; =============================================================================
          ;; [utility] init-util.el
          ;; shell, term, ansi-term, eshell
          ;; all-the-icons-dired
          ;; dired-hacks-utils ;; TODO
          ediff ;; vdiff
          ;; vdiff-magit
          projectile ;; eproject
          magit
          ;; =============================================================================
          ;; [editing] init-edit.el
          windmove
          buffer-move
          avy ;; ace-jump-mode
          ;; ace-pinyin
          undo-tree
          smart-hungry-delete
          lispy ;; paredit, parinfer
          flyspell
          flyspell-correct
          ;; evil
          hydra ;; which-key
          ;; =============================================================================
          ;; [programming] prog.el
          yasnippet
          company ;; auto-complete
          company-jedi
          flycheck ;; flymake
          ;; flycheck-pyflakes
          flycheck-haskell
          ggtags ;; helm-gtags, counsel-gtags, counsel-etags
          ;; =============================================================================
          ;; [c, c++] prog-cc.el
          ;; stickyfunc-enhance

          ;; =============================================================================
          ;; =============================================================================
          ;; [TODO]
          bm
          ;; [text.el]
          ;; pdf-tools
          org
          ;; markdown-mode
          ;; markdown-preview-mode
          ;; auctex
          asn1-mode
          ;; cmake-mode ;; cmake-ide, cmake-project
          ;; cmake-font-lock
          ;; cpputils-cmake
          ;; [python]
          ;; elpy ;; ropemacs
          ;; py-autopep8
          ;; auto-virtualenvwrapper ;; virtualenvwrapper
          ;; [haskell]
          haskell-mode
          hindent
          ;; [ml]
          sml-mode
          ;; [web]
          ;; web-mode
          ;; w3m
          ;; erc ;; circe, rcirc
          ;; [ensure]
          use-package
          diminish
          bind-key
          all-the-icons))
  (defconst pkg/package/completion-backend
    '(helm ivy smex ido icomplete))
  (defconst pkg/package/helm-enabled-list
    '(helm
      ;; helm-bm
      helm-projectile
      helm-flycheck
      ;; helm-gtags
      flyspell-correct-helm
      ))
  (defconst pkg/package/ivy-enabled-list
    '(ivy
      counsel
      counsel-projectile
      ;; counsel-gtags, counsel-etags
      flyspell-correct-ivy
      ))
  (let ((bkd (car pkg/package/completion-backend)))
    (mapc (lambda (pkg)
            (add-to-list 'package-selected-packages pkg))
          (cond
           ((eq bkd 'helm) pkg/package/helm-enabled-list)
           ((eq bkd' ivy) pkg/package/ivy-enabled-list)
           (t (list bkd)))))
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install-selected-packages))

(eval-when-compile
  ;; disabled, diminish
  ;; ensure, requires
  ;; after, demand
  ;; defer
  ;; commands, bind, hook
  ;; mode, magic, interpreter
  ;; preface, if, init, config
  (setq use-package-always-defer nil
        use-package-verbose nil)
  (require 'use-package))

(require 'bind-key)

(use-package all-the-icons
  :ensure t
  :defer t
  :init
  ;; 此插件在首次使用前需要额外地安装字体，否则启用后mode-line中的图片会显示为乱码
  ;; 执行以下命令会自动下载并安装所需字体，Windows上只能手动执行
  ;; 但目前发现Linux上会因权限问题而导致安装失败，因此仍推荐手动执行
  ;; 字体下载目录默认为HOME/.local/share/fonts
  ;; (all-the-icons-install-fonts)
  )

(use-package diminish
  :ensure t
  :hook (after-init . pkg/diminish/start)
  :preface
  (defun pkg/diminish/start ()
    (diminish 'eldoc-mode)
    (diminish 'abbrev-mode)
    (diminish 'hi-lock-mode)))

(use-package transient
  :defer t
  :init
  (let ((dir (my/set-user-emacs-file ".transient/")))
    (setq transient-levels-file (concat dir "levels.el")
          transient-values-file (concat dir "values.el")
          transient-history-file (concat dir "history.el"))))


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
                                  ".emacs.auto-save/.saves-"))
(defalias 'yes-or-no-p 'y-or-n-p) ;; 以y/n替换yes/no
(tool-bar-mode -1)
(menu-bar-mode -1)
(auto-image-file-mode 1) ;; 允许打开图片
(auto-compression-mode 1) ;; 允许查看和写入压缩包

(use-package files
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
  :hook (after-init . pkg/desktop/start)
  :preface
  (defun pkg/desktop/start ()
    (desktop-save-mode 1))
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
        desktop-auto-save-timeout (* 60 10)))

(use-package winner
  :hook (after-init . pkg/winner/start)
  :preface
  (defun pkg/winner/start ()
    (winner-mode 1)))

(use-package paren
  :hook (after-init . pkg/paren/start)
  :preface
  (defun pkg/paren/start ()
    (show-paren-mode 1))
  :init
  (setq show-paren-style 'parenthesis
        show-paren-ring-bell-on-mismatch nil
        show-paren-when-point-inside-paren nil
        show-paren-when-point-in-periphery nil))

(use-package saveplace
  :hook (after-init . pkg/saveplace/start)
  :preface
  (defun pkg/saveplace/start ()
    (save-place-mode 1))
  :init
  (setq save-place-file (my/set-user-emacs-file ".emacs.save-place")
        save-place-version-control nil
        save-place-limit 400
        save-place-forget-unreadable-files t
        save-place-save-skipped nil))

;; -----------------------------------------------------------------------------
;; 若打开的中文文件中仍然存在乱码，则尝试执行(revert-buffer-with-coding-system 'gb18030)
;; 字符集和编码的名称可查询：'language-info-alist
(set-language-environment "Chinese-GB18030")
(prefer-coding-system 'utf-8)
;; (set-face-background 'default "#C7EDCC") ;; 设置背景颜色为绿色护眼色
;; 字体的名字源自于.ttf或.otf文件内自带的元信息，包括family和style等
;; 英文字体：Consolas，Fira Mono
;; 中文字体：SimSun，MicrosoftYaHei，SourceHanSerifSC (思源宋体简体中文)，SourceHanSansSC (思源黑体简体中文)
;; 混合字体：YaHeiConsolasHybrid，MicrosoftYaHeiMono
;; Emacs中设置中文字体有以下几种方案
;; 1. 默认编码：英文字体，中文编码：中文字体
;; 此方案存在的缺陷在于，中英文字体高度不同，导致含有中文字体的行与纯英文字体的行之间行距不均
;; 而以下设置又不知为何无法生效
;; (add-to-list 'face-font-rescale-alist '("SimSun" . 0.8) t)
;; 2. 默认编码：中英文混合字体
;; 网上提供的混合字体，拥有统一的行高，但通常都不能完善地支持斜体、粗体等形式

;; http://emacser.com/torture-emacs.htm
(let* ((rsltn (* (display-pixel-width) (display-pixel-height)))
       ;; 针对中英文字体分别设置两种字号
       (efont (cond ((<= rsltn (* 1600 900)) 11)
                    ((< rsltn (* 1920 1080)) 12)
                    ((< rsltn (* 2560 1440)) 13)
                    (t 15)))
       (cfont (- efont 1))
       (chsets '(kana han symbol cjk-misc bopomofo)))
  (if (eq system-type 'windows-nt)
      ;; Windows系统上的Emacs25版本对中文字体的显示存在问题，打开中文文档时会存在卡顿的现象
      ;; 必须手动指定中文字体为宋体才可避免。
      (progn
        (set-face-font 'default (font-spec :family "Consolas" :size efont))
        (dolist (chset chsets)
          (set-fontset-font t chset
                            (font-spec :family "SimSun" :size cfont) nil 'prepend)))
    (progn
      ;; e.g. 设置字体的方式有以下三种
      ;; (set-frame-font (font-spec))
      ;; (set-face-attribute 'default nil :font (font-spec))
      (set-face-font 'default (font-spec :family "Fira Mono" :size efont))
      ;; 'charset-script-alist
      (dolist (chset chsets)
        (set-fontset-font t chset
                          (font-spec :family "SourceHanSansSC" :size cfont) nil 'prepend))))
  (setq-default line-spacing (/ efont 3)))

;; -----------------------------------------------------------------------------
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

(put 'downcase-region 'disabled nil) ;; 去除每次执行此命令时的提示，强制执行
(put 'upcase-region 'disabled nil)

;; 加载其他配置文件
(mapc (lambda (file)
        (my/load-file (my/set-user-emacs-file file t)))
      '("init-util"
        "init-edit"
        "prog" ;; prog-mode, asn1-mode
        "prog-util"
        "prog-cc" ;; cc-mode
        ;; lisp-mode, emacs-lisp-mode, lisp-interaction-mode,
        ;; sml-mode, haskell-mode
        "prog-fun"
        ;; "prog-py" ;; python-mode
        ;; "prog-web" ;; web-mode
        ;; "text-tex" ;; tex-mode, latex-mode
        ;; "web-browser" ;; web browser
        "init-vis"
        "init-gui"
        "init-keys"))

(message "emacs init time = %s" (emacs-init-time))


(defun my/find-file-read-only ()
  (let ((file (buffer-file-name))
        (skiplist '(".*-autoloads.el$")))
    (unless (or (not file)
                (my/map (lambda (regexp)
                          (string-match-p regexp file))
                        skiplist))
      (read-only-mode 1))))

(defun my/save-point (func)
  (let ((line (line-number-at-pos)))
    (funcall func)
    (goto-char (point-min))
    (when (>= line 1) (forward-line (- line 1)))
    (recenter-top-bottom)))

(defun my/reformat-current-file ()
  (interactive)
  (defun my/reformat-wrapper (func)
    (let ((state buffer-read-only))
      (when state (read-only-mode -1))
      (delete-trailing-whitespace) ;; 删除每行末尾的空格
      (when (< (- (point-max) (point-min)) (* 1024 1024))
        (funcall func))
      ;; 每次保存buffer时都将删除现有的改动高亮，替换成以下两个hook无法生效，原因未知
      ;; write-content-functions或write-file-functions
      (highlight-changes-remove-highlight (point-min) (point-max))
      (when state (read-only-mode 1))))
  (defun my/reformat ()
    (let* ((file (my/locate-file buffer-file-name))
           (mode (when file (assoc-default file auto-mode-alist
                                           'string-match))))
      (cond
       ((and (provided-mode-derived-p mode 'c-mode)
             (derived-mode-p 'c-mode))
        (my/reformat-current-file/c))
       ((and (provided-mode-derived-p mode 'lisp-mode 'emacs-lisp-mode)
             (derived-mode-p 'lisp-mode 'emacs-lisp-mode))
        (my/reformat-current-file/lisp))
       (t nil))))
  (my/reformat-wrapper 'my/reformat))

(defun my/reformat-current-file/c ()
  (interactive)
  (my/save-point
   (lambda ()
     (let ((cfg (my/set-user-emacs-file "my.config/uncrustify.c.cfg"))
           (cmd (my/locate-exec "uncrustify" "/usr/local/bin/")))
       (when (and cfg cmd)
         (shell-command-on-region (point-min) (point-max)
                                  (concat cmd " -l C -c " cfg " --no-backup")
                                  t t "*Shell Error Output*"))))))

(defun my/reformat-current-file/lisp ()
  (interactive)
  (indent-region (point-min) (point-max)))


(provide 'my/init)
