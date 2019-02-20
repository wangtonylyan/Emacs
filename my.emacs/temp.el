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
  ;; =====================================================================================
  ;; 1. (eval-after-load xxx) the following body
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
  ;; or register some triggers for loading this package.
  ;; In the former case, the remaining procedure in this function is executed right after :config part.
  (defun pkg/package/start ()
    nil)
  :if (my/package-enabled-p 'package)
  ;; 1. executed during emacs initialization, i.e. loading init.el file
  :init
  (setq customized-config nil)
  ;; 1. executed after this package has been loaded or whenever reloaded
  :config)



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



# http_proxy=http://CHT1HTSH3191:Alps1911@10.25.71.1:8080
# https_proxy=https://CHT1HTSH3191:Alps1911@10.25.71.1:8080


[http]
proxy = http://CHT1HTSH3191:Alps1911@10.25.71.1:8080
sslverify = false
[https]
proxy = https://CHT1HTSH3191:Alps1911@10.25.71.1:8080


;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(use-package irony ;; c-mode, c++-mode, java-mode
  :init
  (setq irony-server-install-prefix (my/set-user-emacs-file ".irony/")
        irony-user-dir irony-server-install-prefix
        irony-supported-major-modes '(c++-mode c-mode objc-mode))
  :config
  (use-package irony-completion
    :init
    (setq irony-duplicate-candidates-filter t))

  )

(use-package company-irony
  :init
  (setq company-irony-ignore-case nil)
  )

(require ')
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'c-mode-hook 'irony-mode)

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
    :if (my/package-enabled-p 'cmake-ide))

  (use-package cmake-project
    :if (my/package-enabled-p 'cmake-project))

  (use-package asn1-mode
    :if (my/package-enabled-p 'asn1-mode)))

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
