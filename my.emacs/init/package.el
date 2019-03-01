;; -*- coding: utf-8 -*-

(defun pkg/package/enabled-p (package)
  (car (package--user-selected-p package)))

(defun pkg/package/select (packages)
  (cond
   ((listp packages)
    (mapc #'pkg/package/select packages))
   ((symbolp packages)
    (add-to-list 'package-selected-packages packages t))
   (t (user-error "*pkg/package/select* fails on %s" packages))))


(setq url-max-password-attempts 2
      ;; 不支持authentication
      ;; url-proxy-services '(("http" . "10.25.71.1:8080"))
      )

(require 'package)
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
(package-initialize)                 ;; 方式2) 主动执行该函数以加载插件
;; 目前使用此全局变量来管理插件的启用/禁用，其中包括了ELPA更新源中所没有的插件
(setq package-selected-packages '(use-package diminish bind-key all-the-icons))
;; [Interface]
(pkg/package/select '(doom-themes ;; solarized-theme, zenburn-theme
                      dashboard
                      nyan-mode
                      doom-modeline ;; spaceline, spaceline-all-the-icons, smart-mode-line
                      tabbar        ;; awesome-tab
                      treemacs      ;; neotree, sidebar, sr-speedbar, ecb
                      treemacs-projectile
                      ;; treemacs-icons-dired
                      ))
;; [Visual]
(pkg/package/select '(beacon
                      ;; nlinum, nlinum-hl
                      ;; yascroll
                      ;; sublimity, minimap
                      ;; whitespace, fill-column-indicator
                      rainbow-delimiters
                      ;; color-identifiers, rainbow-identifiers ;; 会覆盖配色主题所使用的字体颜色
                      highlight-thing
                      zoom ;; dimmer
                      ;; typo
                      ))
;; [Editting]
(pkg/package/select '(windmove
                      ace-window ;; switch-window
                      buffer-move
                      avy ;; ace-jump-mode
                      ;; ace-pinyin
                      ace-link
                      undo-tree
                      smart-hungry-delete
                      lispy ;; paredit, parinfer
                      expand-region
                      multiple-cursors ;; highlight-symbol
                      flyspell
                      flyspell-correct
                      hydra ;; which-key
                      ;; evil
                      ))
;; [Utility]
(pkg/package/select '(;; shell, term, ansi-term, eshell
                      ;; all-the-icons-dired
                      ;; dired-hacks-utils ;; TODO
                      helm ;; ivy, smex, ido, icicles, icomplete
                      ediff ;; vdiff
                      ;; vdiff-magit
                      projectile ;; find-file-in-project, eproject
                      magit))
;; [Programming]
(pkg/package/select '(;; prog-mode
                      flycheck ;; flymake
                      ;; flycheck-pyflakes
                      flycheck-haskell
                      ggtags ;; helm-gtags, counsel-gtags, counsel-etags
                      ;; asn1-mode
                      ))
;; [Completion]
(pkg/package/select '(yasnippet
                      company ;; auto-complete
                      company-quickhelp ;; company-box
                      company-jedi))
;; [C, C++]
(pkg/package/select '(;; cc-mode
                      ;; cedet, semantic
                      ;; stickyfunc-enhance
                      ycmd ;; irony
                      flycheck-ycmd ;; flycheck-irony
                      company-ycmd  ;; company-irony
                      ))
(cond
 ((pkg/package/enabled-p 'helm)
  (pkg/package/select '(;; helm-bm
                        helm-dash
                        helm-projectile
                        helm-flycheck
                        ;; helm-gtags
                        flyspell-correct-helm)))
 ((pkg/package/enabled-p 'ivy)
  (pkg/package/select '(counsel
                        counsel-projectile
                        ;; counsel-gtags, counsel-etags
                        flyspell-correct-ivy))))
(when (not package-archive-contents)
  (package-refresh-contents))
(package-install-selected-packages)


(use-package use-package
  :defer t
  :init
  (setq use-package-always-defer nil
        use-package-verbose nil))

(use-package diminish
  :defer t
  :config
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode)
  (diminish 'hi-lock-mode))

(use-package all-the-icons
  :defer t
  :init
  ;; 此插件在首次使用前需要额外地安装字体，否则启用后mode-line中的图片会显示为乱码
  ;; 执行以下命令会自动下载并安装所需字体，Windows上只能手动执行
  ;; 但目前发现Linux上会因权限问题而导致安装失败，因此仍推荐手动执行
  ;; 字体下载目录默认为HOME/.local/share/fonts
  ;; (all-the-icons-install-fonts)
  )

(use-package transient
  :defer t
  :init
  (let ((dir (my/set-user-emacs-file ".transient/")))
    (setq transient-levels-file (my/concat-directory-file dir "levels.el")
          transient-values-file (my/concat-directory-file dir "values.el")
          transient-history-file (my/concat-directory-file dir "history.el"))))

(use-package request
  :defer t
  :init
  (setq request-message-level 'error))


(provide 'my/init/package)
