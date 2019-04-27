;; 重构
;; 1. 检查包含以下字符串的符号名
;;    my-, plugin
;;    user-emacs, private
;;    add-hook, cc

;; (pkg/xxx/init)
;; (pkg/xxx/setup)
;; (pkg/xxx/start)

;; this is a template for 'use-package
(use-package package
  :disabled
  :diminish (mode1
             mode2)
  ;; =====================================================================================
  ;; 1. used for packages in (package-list-packages) only
  ;;    especially not used for built-in packages
  :ensure t
  ;; 1. list dependent packages not shown in (package-list-packages)
  ;; 2. this keyword DOES NOT introduce any autoload
  :requires (package1
             package2)
  :after (package1
          package2)
  :hook ((before-init . pkg/package/init)
         (after-init . pkg/package/start))
  ;; =====================================================================================
  :demand t
  ;; 1. this package is explicitly required by some others
  ;; 2. when (pkg/xxx/start) is used in :init to lazily load itself
  ;; 3. mostly used in init-keys.el file
  ;; the following three keywords have nothing to do with :after and :hook
  ;; those two don't always imply the loading, and when they don't and :defer occurs,
  ;; an explicit loading is still required for this package
  :defer t
  :mode ((regexp . minor-mode)
         (regexp . minor-mode))
  ;; 1. list commands that are bound in init-keys.el file
  ;; 2. non-autoload commands
  :commands (command1
             command2)
  ;; 1. mostly this should be defined in init-keys.el
  :bind (("key1" . command1)
         ([remap command1] . command2))
  ;; =====================================================================================
  :preface
  ;; When this function is added to 'after-init-hook, it should either directly load this package,
  ;; or register some triggers for loading this package. In the former case, the remaining procedure
  ;; in this function is executed right after :config section.
  (defun pkg/package/start ()
    nil)
  :if (my/package-enabled-p 'package)
  :init
  ;; 1. executed after this package has been loaded or whenever reloaded
  :config
  (setq customized-config nil))
;; 以下部分没有绝对可以遵循的原则，可能需要根据每个package的实现来调整
;; 1. :after与use-package声明顺序之间的关系
;; 2. (setq)执行于:init还是:config部分



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
  (my/add-mode-hook "w3m" #'visual-line-mode))

(use-package erc
  :if (my/package-enabled-p 'erc)
  :config
  (bind-keys :map erc-mode-map
             ;; ("<return>" . nil)
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


(defun my-func-prog-mode-beautify ()
  (interactive)
  (cond
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'c++-mode))
    (let ((exe "uncrustify")
          (cfg "~/.uncrustify/alps.cfg"))
      (if (and (executable-find exe)
               (file-exists-p cfg))
          (message (shell-command-to-string
                    (concat exe " -l C -c " cfg " --no-backup " buffer-file-name)))
        (message "uncrustify unsupported!"))))
   ((eq major-mode 'python-mode)
    (if (and (my/package-enabled-p 'py-autopep8)
             (executable-find "autopep8")
             (fboundp 'py-autopep8-buffer))
        (py-autopep8-buffer)
      (message "autopep8 unsupported!")))
   ((derived-mode-p 'web-mode)
    (if (my/package-enabled-p 'web-beautify)
        (web-beautify-html)
      (message "html-beautify unsupported!")))
   (t (message "current major mode unsupported!"))))


;; replace built-in CEDET with an external one, if exists
;; $ git clone http://git.code.sf.net/p/cedet/git cedet
;; $ make
;; CEDET及其现状的介绍
;; https://www.emacswiki.org/emacs/CollectionOfEmacsDevelopmentEnvironmentTools
;; https://stackoverflow.com/questions/12711765/status-of-cedet-and-ecb-in-emacs-24-2
;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
(when (boundp 'my-private-project-root-directory)
  (let* ((path (file-name-as-directory
                (concat my-private-project-root-directory "cedet")))
         (file (concat path "cedet-devel-load.el")))
    (when (file-exists-p file)
      (load-file file)
      (add-to-list 'load-path (concat path "contrib"))
      (add-to-list 'Info-directory-list (concat path "doc/info/")))))

(defun my/prog/todo ()
  ;; =============================================================================
  ;; todo
  ;; 项目目录示例
  ;; project root folder
  ;; |-- CMakeLists.txt
  ;; |-- src :: 头文件和源文件
  ;; |-- project1 :: 一个子项目或模块
  ;; |---- CMakeLists.txt
  ;; |---- src
  ;; |-- project2
  ;; |---- CMakeLists.txt
  ;; |---- src
  (use-package cmake-mode
    :if (my/package-enabled-p 'cmake-mode)
    :init
    (my/prog/add-start-hook #'pkg/cmake/start)
    :config
    (use-package cmake-font-lock
      :if (my/package-enabled-p 'cmake-font-lock)
      :init
      ;; (cmake-font-lock-activate)在实现上会覆盖原本font-lock-mode的效果
      ;; 因此其必须在后者生效之后执行，于是暂采用以下手段
      ;; 但其要求(global-font-lock-mode)在实现上要与(turn-on-font-lock)类似
      ;; 即保证在font-lock-mode启动后执行时，不会再次重启
      (add-hook 'cmake-mode-hook 'turn-on-font-lock nil)
      (add-hook 'cmake-mode-hook 'cmake-font-lock-activate t)))

  (use-package cmake-ide
    :if (my/package-enabled-p 'cmake-ide)
    :config
    (cmake-ide-setup))

  (use-package cmake-project
    :if (my/package-enabled-p 'cmake-project)))

(defun pkg/ecb/init ()
  (use-package ecb
    :if (my/package-enabled-p 'ecb)
    :commands (ecb-minor-mode)
    :init
    (my/prog-cc/add-start-hook #'pkg/ecb/start)
    :config
    (save-excursion
      (unless (boundp 'stack-trace-on-error)
        (defvar stack-trace-on-error nil)) ;; 兼容性
      (setq ecb-layout-name "left15"
            ;; ecb-toggle-layout-sequence '()
            ;; ecb-layout-window-sizes nil ;; 推荐通过调用ecb-change-layout命令，以交互式的方式修改
            ecb-windows-width 0.2
            ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
            ecb-tip-of-the-day nil
            ;; ecb-auto-compatibility-check nil
            ;; [directories window]
            ecb-source-path '("~")
            ecb-tree-buffer-style 'image
            ecb-auto-expand-directory-tree 'best
            ecb-excluded-directories-regexps '("^\\(\\.\\|\\.\\.\\)$")
            ecb-show-sources-in-directories-buffer '("left15")
            ;; [sources window]
            ;; ecb-source-file-regexps '()
            ;; ecb-sources-exclude-cvsignore '()
            ;; [methods window]
            ecb-process-non-semantic-files nil ;; 禁用non-semantic-sources
            ;; [history window]
            ;; ecb-history-exclude-file-regexps '()
            ;; [compilation window]
            ecb-compile-window-height 0.2
            ecb-compile-window-width 'edit-window
            ecb-compile-window-temporally-enlarge 'both
            ecb-enlarged-compilation-window-max-height 0.5
            ecb-compilation-buffer-names ;; 以下名称的buffer内容将被呈现于该窗口
            (append ecb-compilation-buffer-names '(("*Process List*")
                                                   ("*Proced*")
                                                   (".notes")
                                                   ("*appt-buf*")
                                                   ("*Compile-Log*")
                                                   ("*etags tmp*")
                                                   ("*svn-process*")
                                                   ("*svn-info-output*")
                                                   ("*Python Output*")
                                                   ("*Org Agenda*")
                                                   ("*EMMS Playlist*")
                                                   ("*Moccur*")
                                                   ("*Directory")))
            ecb-compilation-major-modes ;; 以下模式的buffer内容将被呈现于该窗口
            (append ecb-compilation-major-modes '(change-log-mode
                                                  calendar-mode
                                                  diary-mode
                                                  diary-fancy-display-mode
                                                  xgtags-select-mode
                                                  svn-status-mode
                                                  svn-info-mode
                                                  svn-status-diff-mode
                                                  svn-log-view-mode
                                                  svn-log-edit-mode
                                                  erc-mode
                                                  gud-mode)))
      (ecb-minor-mode 1) ;; global minor mode
      )))

(defun pkg/ecb/start ()
  )


;; todo
;; 此插件会读取CMake所使用的项目配置文件，包括CMakeLists.txt
;; 用于设置但不依赖于以下插件的存在，其仅依赖于CMake
;; [Flymake]
;; [Flycheck]
;; 'flycheck-clang-language-standard
;; 'flycheck-gcc-language-standard
;; 'flycheck-clang-include-path
;; 'flycheck-clang-definitions
;; [Company]
;; 'company-clang-arguments
;; [Company-C-Headers]
;; 'company-c-headers-path-system
;; [Auto-Complete]
;; 'ac-clang-flags
;; [Semantic]
;; (semantic-add-system-include)
;; (semantic-remove-system-include)
(defun pkg/cpputils-cmake/init ()
  (use-package cpputils-cmake
    :if (and (my/package-enabled-p 'cmake-mode)
             (my/package-enabled-p 'cpputils-cmake))
    :commands (cppcm-reload-all)
    :init
    (setq cppcm-write-flymake-makefile nil ;; since flymake is not used for now
          ;; optional, specify extra preprocess flags forwarded to compiler
          ;; cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG")
          )
    (my/prog-cc/add-start-hook #'pkg/cpputils-cmake/start)))

(defun pkg/cpputils-cmake/start ()
  (cppcm-reload-all))


;; Grand Unified Debugger
;; optional, avoid typing full path when starting gdb
;; (bind-keys ("C-c C-g" . (lambda () (interactive)
;; (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer))))))
(defun pkg/gud/init ()
  (use-package gud
    :init
    (my/prog-cc/add-start-hook #'pkg/gud/start)))

(defun pkg/gud/start ()
  )

(defun pkg/shell/start ()
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
        (rename-buffer (concat "*EShell: " name "*"))))))

(use-package bm ;; TODO
  :commands (bm-next
             bm-previous
             bm-toggle)
  :if (my/package-enabled-p 'bm)
  :config
  (setq bm-marker 'bm-marker-left
        bm-cycle-all-buffers t
        temporary-bookmark-p t
        bm-buffer-persistence t
        bm-restore-repository-on-load t
        bm-repository-file (my/set-user-emacs-file "bm-repository/"))
  (setq-default bm-buffer-persistence bm-buffer-persistence)
  (add-hook' after-init-hook #'bm-repository-load t)
  (add-hook 'find-file-hooks #'bm-buffer-restore t)
  (add-hook 'kill-buffer-hook #'bm-buffer-save t)
  (add-hook 'kill-emacs-hook (lambda ()
                               (progn
                                 (bm-buffer-save-all)
                                 (bm-repository-save))) t)
  (add-hook 'after-save-hook #'bm-buffer-save t)
  (add-hook 'after-revert-hook #'bm-buffer-restore t)
  (use-package helm-bm
    :after (helm)
    :bind (("C-c b b" . helm-bm))
    :if (and (my/package-enabled-p 'helm)
             (my/package-enabled-p 'helm-bm)))
  (unbind-key "C-x r"))


(setq wocaotodo '(;; =============================================================================
                  ;; [navigation] --> editing.el
                  bm
                  ;; [text.el]
                  ;; pdf-tools
                  org
                  ;; markdown-mode
                  ;; markdown-preview-mode
                  ;; auctex
                  ;; cmake-mode ;; cmake-ide, cmake-project
                  ;; cmake-font-lock
                  ;; cpputils-cmake
                  ;; [ml]
                  sml-mode
                  ;; [web]
                  ;; web-mode
                  ;; w3m
                  ;; erc ;; circe, rcirc
                  ))


(use-package helm-dash
  :if (pkg/package/enabled-p 'helm-dash)
  :init
  (setq helm-dash-docsets-path (my/set-user-emacs-file ".docsets")
        helm-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master"))
;; https://github.com/areina/helm-dash


(global-eldoc-mode -1)

;; eldoc-overlay, eldoc-box
(use-package eldoc
  :defer t
  :if (pkg/package/enabled-p 'eldoc)
  :init
  (setq eldoc-idle-delay 0.5)
  (dolist (mode '("elisp" "ilisp"))
    (my/add-mode-hook mode #'eldoc-mode)))
;; 'semantic-idle-summary

;; ycmd-server-command nil
;; ycmd-server-args '("--log=debug" )
;; ycmd-file-parse-result-hook nil
;; ycmd-idle-change-delay 0.5
;; ycmd-keepalive-period 600
;; ycmd-startup-timeout 3
;; ycmd-delete-process-delay 3
;; ycmd-parse-conditions '(save new-line mode-enabled)
;; ycmd-default-tags-file-name "tags"
;; ycmd-auto-trigger-semantic-completion t
;; ycmd-hide-url-status t
;; ycmd-bypass-url-proxy-services t
;; ycmd-tag-files nil
;; ycmd-file-type-map
;; ycmd-min-num-chars-for-completion 2
;; ycmd-max-num-identifier-candidates 10
;; ycmd-seed-identifiers-with-keywords nil
;; ycmd-get-keywords-function 'ycmd--get-keywords-from-alist
;; ycmd-gocode-binary-path nil
;; ycmd-godef-binary-path nil
;; ycmd-rust-src-path nil
;; ycmd-swift-src-path nil
;; ycmd-racerd-binary-path nil
;; ycmd-python-binary-path nil
;; ycmd-global-modes t
;; ycmd-confirm-fixit t
;; ycmd-after-exception-hook nil
;; ycmd-after-teardown-hook nil
;; ycmd-mode-line-prefix "ycmd"
;; ycmd-completing-read-function #'completing-read



;; awesome packages
[Editing]
Helm-swoop - Efficiently jump between matched string/lines.
bm - Visual Bookmarks, provides an easy way to navigate in a buffer.
vertigo.el - Jump lines using the home row.
multifiles.el - View and edit parts of multiple files in one buffer.
lentic - Create views of the same content in two Emacs buffers.
ialign - Interactively align lines using a regular expression.
crux - A Collection of Ridiculously Useful eXtensions for Emacs.
god-mode - Global minor mode for entering Emacs commands without modifier keys.
modalka - Introduce native modal editing of your own design.
xah-fly-keys - A modal keybinding for emacs (like vim), but based on command frequency and ergonomics.
ergoemacs-mode - Global minor mode to use both common interface keys and ergonomic keys for emacs.
general - A convient, unified interface for key definitions - like use-package but for key-bindings.
[Interface Enhancement]
ace-popup-menu - Replace GUI popup menu with something more efficient.
ElScreen - Utility for multiple screens.
workgroups2 - Session manager, saves all your opened buffers, their location and sizes on disk to restore later.
Eyebrowse - A simple-minded way of managing window configs in emacs.
[File Manager]
Dired+ - Functional & interface extensions for Dired.
dired-k - Highlight Dired buffer by file size, modified time, git status.
Direx - directory tree explorer.
ztree - Directory tree comparison mode.
Ranger - ranger like file manager based on Dired.
Sunrise Commander - Twin-pane file manager for Emacs based on Dired and inspired by Midnight Commander.
tramp-hdfs - Browse HDFS in Emacs with dired using Tramp.
[Programming]
auto-yasnippet - Advanced copy-paste using Yasnippet.
tiny - Templates based on linear range transformations.
evil-nerd-commenter - Comment/uncomment lines efficiently. Like Nerd Commenter in Vim. This program can be used independently without evil-mode.
mmm-mode - allows Multiple Major Modes to coexist in one buffer (ex: Embedded CSS & JS in HTML file).
SmartParens - Deals with parens pairs and tries to be smart about it.
indent-guide - Show vertical lines to guide indentation.
Doxymacs - Doxymacs is Doxygen + {X}Emacs.
whitespace-cleanup-mode - Intelligently call whitespace-cleanup on save.
realgud - A modular front-end for interacting with external debuggers.
ws-butler - Unobtrusively trim extraneous white-space ONLY in lines edited.
format-all - Auto-format source code in many languages using the same command.
Aggressive-indent - Keeps your code always indented automatically.
Helm-dash - Browse Dash docsets via Helm interface.
eldoc - [built-in] shows function arguments / variable doc in minibuffer when coding.
Dumb Jump - easy jump to definition package for multiple languages using ag or grep.
vimish-fold - Vim-like text folding.
hideshow - [built-in] Folding regions by balanced-expression code.
hideshowvis - Based on hideshow, just display its nodes on fringe.
Origami.el - Feature rich text folding minor mode.
