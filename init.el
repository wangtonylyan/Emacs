;; �ж�Emacs�汾���Ի�����������������'emacs-major-version��'emacs-minor-version

(defun my-func-executable-find (dir exe &optional add)
  (let* ((dir (if (and (stringp dir) (> (length dir) 0))
                  ;; ͳһ���ε���ʽ
                  (concat (directory-file-name dir) "/") ""))
         (path (executable-find (concat dir exe))))
    (when (and path (file-executable-p path))
      (when add
        (add-to-list 'exec-path (directory-file-name path) t))
      path)))

(defalias 'my-func-package-enabled-p 'package--user-selected-p)

(defalias 'my-func-minor-mode-on-p 'bound-and-true-p)

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "D:/softwares" t)
  (let ((path (my-func-executable-find
               "Emacs25/libexec/emacs/24.5/i686-pc-mingw32"
               "cmdproxy.exe")))
    (when path
      (setq shell-file-name path
            shell-command-switch "-c"))))

;; (setq user-init-file "~/.emacs.d/init.el")
;; (load user-init-file)

;; �����������������ڵ�ǰ�ļ���ִ��ʱ����buffer-local�ĳ�ʼֵ��
;; ���Ϊʹ���Դ���Ч���ͱ���ͬʱ�޸ľֲ���ȫ��ֵ
(setq default-directory "~/"
      user-emacs-directory "~/.emacs.d/"
      command-line-default-directory default-directory)
(setq-default default-directory default-directory
              user-emacs-directory user-emacs-directory)
(defconst my-user-emacs-directory (concat user-emacs-directory "my-emacs/"))

;; (normal-top-level-add-subdirs-to-load-path)
;; (normal-top-level-add-to-load-path)

;; ָ����(customize)д��������Ϣ���ļ������ÿ��Emacs�Զ�д��ʱ�Ͳ������޸ĵ�ǰ�ļ���
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; (setq url-proxy-services '(("http" . "10.25.71.1:8080"))) ;; ��֧��authentication
(when (require 'package nil t)
  ;; ���ð�װ���Ĵ洢Ŀ¼����Ŀ¼Ҳ��Ҫ��������'load-path��
  ;; (add-to-list 'package-directory-list "~/.emacs.d/elpa" t) ;; system-wide dir
  (setq package-user-dir (concat user-emacs-directory "elpa")) ;; user-wide dir
  ;; Emacsʹ�õ�Ĭ�ϸ���ԴΪ��("gnu" . "http://elpa.gnu.org/")
  ;; ��Ӹ���Դ��MELPAÿ����£�������˾���������
  ;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;; �����б��������ñ�������صĲ��������������ڰ�װ����ʹ�ò���Ĺ�����
  ;; ��������ǰ��ϸ���оٳ����еĲ������Ҫ���ݲ��֮���������ϵ�����Ⱥ������
  (setq package-load-list '(all
                            ;; e.g.
                            ;; (dash) (epl) (let-alist) (pkg-info) (flycheck)
                            ;; (atom-one-dark-theme t) (material-theme t)
                            ))
  ;; ���ü��������б�����ָ�������ʱ��
  (setq package-enable-at-startup nil) ;; ��ʽ1) ��Emacs���������Զ����ز��
  (package-initialize) ;; ��ʽ2) ����ִ�иú����Լ��ز��
  ;; Ŀǰʹ�ô�ȫ�ֱ�����������������/���ã����а�����ELPA����Դ����û�еĲ��
  (setq package-selected-packages '(atom-one-dark-theme
                                    ;; all-the-icons ;; �״ΰ�װ����Ҫ����ذ�װ����
                                    powerline ;; smart-mode-line-powerline-theme
                                    ;; smart-mode-line
                                    avy ;; ace-jump-mode
                                    ;; ace-pinyin
                                    sr-speedbar
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
                                    elpy ;; ropemacs
                                    py-autopep8
                                    auctex
                                    ;; circe, rcirc
                                    use-package))
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install-selected-packages))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

;; ָ������������İ�װĿ¼
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
                     (enable-theme 'solarized))
                   t))))))
 "atom-one-dark")

(use-package all-the-icons
  :if (my-func-package-enabled-p 'all-the-icons)
  :init
  (all-the-icons-install-fonts))

(use-package powerline
  :if (my-func-package-enabled-p 'powerline)
  :config
  (setq powerline-default-separator 'contour
        powerline-default-separator-dir '(left . right))
  (powerline-default-theme))

(use-package smart-mode-line
  :if (my-func-package-enabled-p 'smart-mode-line)
  :config
  (setq sml/theme (if (and (my-func-package-enabled-p 'smart-mode-line-powerline-theme)
                           (require 'smart-mode-line-powerline-theme nil t))
                      'powerline 'automatic)
        sml/no-confirm-load-theme t
        sml/shorten-directory t
        sml/shorten-modes t)
  (smart-mode-line-enable))

(use-package minimap
  :if (my-func-package-enabled-p 'minimap)
  :config
  (setq minimap-always-recenter nil ;; ����Ϊnil����Ч?
        minimap-recenter-type 'middle
        minimap-buffer-name-prefix "MINI" ;; ����Ϊ�գ������޷�����minimap����
        minimap-hide-fringes t
        minimap-hide-scroll-bar t
        minimap-update-delay 1.0
        minimap-window-location 'right
        minimap-display-semantic-overlays nil
        minimap-enlarge-certain-faces nil))

(use-package flyspell
  :if (and (my-func-package-enabled-p 'flyspell)
           (executable-find "aspell"))
  :config
  (setq ispell-program-name (executable-find "aspell") ;; ���ú�̨֧�ֳ���
        ;; ispell-dictionary "english" ;; default dictionary
        ;; ispell-personal-dictionary ""
        flyspell-issue-message-flag nil))

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
;; ��������
;; -----------------------------------------------------------------------------
(setq user-full-name "TonyLYan"
      user-mail-address "wangtonylyan@outlook.com"
      inhibit-startup-message 1 ;; ȡ����������
      frame-title-format '(buffer-file-name "%f" ("%b")) ;; ���ñ�������ʾΪbuffer����
      uniquify-buffer-name-style 'post-forward-angle-brackets ;; ����buffer������
      visible-bell t ;; �Դ�����˸�ķ�ʽ���������ʾ��
      echo-keystrokes 0.1
      debug-on-error nil ;; ��ʾ������Ϣ
      select-enable-clipboard t
      current-language-environment "Chinese-GB"
      auto-revert-use-notify t
      auto-revert-verbose nil
      auto-revert-stop-on-user-input t
      auto-revert-interval 5
      delete-by-moving-to-trash t
      make-backup-files t ;; �����Զ�����
      version-control t ;; ���ð汾���ƣ������Ա��ݶ��
      kept-old-versions 1 ;; ������ɵİ汾����
      kept-new-versions 1 ;; �������µİ汾����
      delete-old-versions t
      dired-kept-versions 2
      backup-directory-alist '(("." . "~/.emacs.d.backups"))
      backup-by-copying t)
(setq-default buffer-file-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p) ;; ��y/n�滻yes/no
(tool-bar-mode -1)
(menu-bar-mode -1)
(prefer-coding-system 'utf-8)
;; ��ģʽ���ڼ�ش����ϵ��ļ��Ƿ��ⲿ�����޸ģ�����ʾ�û����Զ����¼��ظ��ļ�
(global-auto-revert-mode 1)
(recentf-mode 1)
(auto-image-file-mode 1) ;; �����ͼƬ
(auto-compression-mode 1) ;; ����鿴��д��ѹ����

;; (set-face-background 'default "#C7EDCC") ;; ���ñ�����ɫΪ��ɫ����ɫ
;; ���������Դ����.ttf��.otf�ļ����Դ���Ԫ��Ϣ������family��style��
;; ����ʹ�ò�ͬ����Ӣ��������ֺŵ�Ŀ����Ϊ�����������ԣ�����ͬһ�����µ������ַ�ͨ������Ӣ���ַ�����
(let* ((rslt (<= (* (display-pixel-width) (display-pixel-height)) (* 1366 768)))
       (efont (if rslt 10 11))
       (cfont (if rslt 9 10))
       (fcnct (lambda (font size) (concat font " " (number-to-string size)))))
  (if (eq system-type 'windows-nt)
      ;; Windowsϵͳ�ϵ�Emacs25�汾�������������ʾ�������⣬�������ĵ�ʱ����ڿ��ٵ�����
      ;; �����ֶ�ָ����������Ϊ����ſɱ��⡣
      (progn
        (set-default-font (eval `(,fcnct "Consolas" ,efont)))
        (set-fontset-font "fontset-default" 'unicode (eval `(,fcnct "����" ,cfont))))
    (progn
      (set-default-font (eval `(,fcnct "YaHei Consolas Hybrid" ,efont)))
      (set-fontset-font "fontset-default" 'unicode ;; ���滻��"Microsoft YaHei Mono"
                        (eval `(,fcnct "Source Han Serif SC SemiBold" ,cfont))))))
;; (set-face-attribute 'default nil :family "Microsoft YaHei Mono" :weight 'normal :height 110) ;; �������壬�����ֺŵ�
;; (set-frame-font "10" nil t) ;; �����ֺ�, ͬ(set-face-attribute)�е�:height

(global-font-lock-mode 1) ;; �﷨����
;; (add-hook 'xxx-mode-hook 'turn-on-font-lock) ;; (font-lock-mode 1)
;; (global-linum-mode 1) ;; ����кţ��Ƽ���������ʾ����Ҫ�ı༭�ĵ���
;; (add-hook 'xxx-mode-hook 'linum-mode)
(global-highlight-changes-mode 1)
(mouse-avoidance-mode 'animate) ;; ������ƶ������λ��ʱ��Ϊ�����ڵ����ߣ��Զ��ƿ����
;; (save-place-mode 1) ;; ��¼�����ÿ���ļ������һ�η���ʱ���ڵ�λ��
(set-cursor-color "white")
;; (blink-cursor-mode -1)
(column-number-mode 1) ;; ��mode-line��ʾ����
(scroll-bar-mode -1) ;; ȡ��������
(global-hl-line-mode 1)
(global-visual-line-mode -1) ;; ������֧�ֲ���
(show-paren-mode 1) ;; ��ʾƥ�����������
(electric-pair-mode -1)
(electric-quote-mode -1)
(electric-indent-mode -1) ;; �Զ�����
(setq font-lock-maximum-decoration t
      transient-mark-mode t
      shift-select-mode nil
      highlight-changes-global-changes-existing-buffers nil
      highlight-changes-visibility-initial-state t
      highlight-changes-face-list nil
      highlight-changes-colors nil
      blink-cursor-blinks 0
      ;; �����ݲ�ʹ��ƽ������������ͨ���������±����Ծ����ܵر���ҳ�����ʱ�����Ƶ������
      ;; mouse wheel scrolling
      mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      ;; keyboard scrolling
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
      tab-always-indent 'complete)
(setq-default cursor-type '(bar . 3)
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01
              line-spacing 0 ;; �о�
              ;; fill-column 80 ;; ����80�ַ��ͻ�����ʾ
              indicate-empty-lines nil
              indent-tabs-mode nil ;; make indentation commands use space only
              truncate-lines truncate-lines
              word-wrap word-wrap
              tab-width 4)
;; ÿ�α���bufferʱ����ɾ�����еĸĶ�����
;; �滻����������hook�ͻ���Ч��ԭ��δ֪��write-content-functions��write-file-functions
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;; ɾ��ÿ��ĩβ�Ŀո�
            (highlight-changes-remove-highlight (point-min) (point-max)))
          t)

(use-package icomplete
  :if (not (my-func-package-enabled-p 'icomplete))
  :config
  (icomplete-mode -1))

(use-package ido
  :if (my-func-package-enabled-p 'ido)
  :config
  (ido-mode 1)
  (ido-everywhere -1) ;; ��ʹido֧��find-file��switch-to-buffer
  (setq ido-enable-prefix t
        ido-enable-flex-matching t
        ido-use-filename-at-point t
        ido-enter-matching-directory nil))

(use-package smex
  :if (my-func-package-enabled-p 'smex)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; �ò�����Զ��滻ԭM-x��ݼ����󶨵�������뱣��������°�֮
         ("C-x M-x" . execute-extended-command))
  :config
  (smex-initialize))

(use-package helm
  :if (my-func-package-enabled-p 'helm)
  ;; Helm�ṩ��һ���ڹ������벿��Emacsԭ���������غϵ����
  ;; ������Ĭ�ϰ�������'helm-command-prefix-keyΪǰ׺�Ŀ�ݼ�����
  ;; ����ͨ�������ǰ׺�������������
  :bind (("C-c h" . helm-command-prefix) ;; �滻ǰ׺
         ;; Ҳ���Խ����ֳ�������ֱ���滻Emacsԭ��ݼ�
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-o" . helm-occur)
         :map helm-map
         ;; 'helm-execute-persistent-action�����'helm-select-action������
         ("<tab>" . helm-execute-persistent-action)
         ("<C-return>" . helm-select-action)
         :map minibuffer-local-map
         ("M-p" . helm-minibuffer-history)
         ("M-n" . helm-minibuffer-history))
  :diminish helm-mode
  :config
  (require 'helm-config)
  (unbind-key "C-x c")
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
        helm-buffer-skip-remote-checking t
        ;; ���øò�������ָ����ͬ�ĺ�̨֧�֣�����imenu��ido��smex��
        ;; helm-completing-read-handlers-alist
        )
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package projectile
  :if (my-func-package-enabled-p 'projectile)
  :init
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode 1)
  ;; (add-to-list 'projectile-other-file-alist '("html" "js"))
  (use-package helm-projectile
    :if (my-func-package-enabled-p 'helm-projectile)
    :after helm
    :init
    (setq helm-projectile-fuzzy-match t)
    :config
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile)
    (helm-projectile-on)))

(use-package magit
  :if (my-func-package-enabled-p 'magit)
  :bind (("C-c g" . magit-status))
  :config
  (when (eq system-type 'windows-nt)
    (let ((path (my-func-executable-find "Git" "git.exe")))
      (when path
        (setq magit-git-executable path))))
  (setq magit-auto-revert-mode t
        magit-auto-revert-immediately t
        magit-auto-revert-tracked-only t
        ;; magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-repository-directories `((,(expand-file-name "project") . 3)
                                       (,(expand-file-name "Project") . 3))))

(use-package sr-speedbar
  :if (my-func-package-enabled-p 'sr-speedbar)
  :bind (("C-S-s" . sr-speedbar-toggle)
         :map speedbar-file-key-map
         ("<tab>" . speedbar-edit-line))
  :init
  (setq speedbar-use-images nil ;; ��ʹ��image��ʽ
        speedbar-show-unknown-files t))

;; Key
;; ���ǰ׺
;; C-c h :: helm
;; C-c t :: helm-gtags
;; C-c p :: projectile, helm-projectile
;; C-c g :: magit
;; C-c o :: org
(unbind-key "C-x o") ;; (other-window)
(unbind-key "C-x f") ;; (set-fill-column)
(unbind-key "C-x C-l") ;; (downcase-region)
(unbind-key "C-x C-u") ;; (upcase-region)
(bind-keys ("C-S-a" . mark-whole-buffer)
           ("C-S-h" . windmove-left)
           ("C-S-l" . windmove-right)
           ("C-S-j" . windmove-down)
           ("C-S-k" . windmove-up)
           ("<C-wheel-up>" . text-scale-increase)
           ("<C-wheel-down>" . text-scale-decrease)
           ("<C-up>" . text-scale-increase)
           ("<C-down>" . text-scale-decrease)
           ("C-x C--" . downcase-region)
           ("C-x C-=" . upcase-region)
           ("C-q" . read-only-mode))
(put 'downcase-region 'disabled nil) ;; ȥ��ÿ��ִ�д�����ʱ����ʾ��ǿ��ִ��
(put 'upcase-region 'disabled nil)
;; �����뷨�л�����ͻ
;; (global-set-key (kbd "C-S-SPC") 'set-mark-command)
;; (global-unset-key (kbd "C-SPC"))

(use-package org
  :bind (("C-c o c" . org-capture)
         ("C-c o a" . org-agenda)))

(use-package ace-jump-mode
  :if (my-func-package-enabled-p 'ace-jump-mode)
  :bind (("C-:" . ace-jump-mode-pop-mark)
         ("C-'" . ace-jump-char-mode))
  :config
  (ace-jump-mode-enable-mark-sync))

(use-package avy
  :if (my-func-package-enabled-p 'avy)
  :bind (("C-:" . avy-goto-char) ;; (avy-goto-char-timer)
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

;; File Extension
;; (setq auto-mode-alist (cons '("\\.emacs\\'" . emacs-lisp-mode) auto-mode-alist))

;; �������ڴ�С
(when (fboundp 'x-send-client-message)
  ((lambda ()
     ;; ȫ��
     ;; (interactive)
     ;; (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0))
     ;; ���������Ҫ�ֱ𾭹�ˮƽ�ʹ�ֱ������������
     (interactive)
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
     (interactive)
     (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                            '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))))

(provide 'my-init)

;; =============================================================================
(use-package circe
  :if (my-func-package-enabled-p 'circe)
  :config
  (setq circe-network-options '(("Freenode" ;; http://freenode.net/
                                 :nick ""
                                 :sasl-username ""
                                 :sasl-password ""
                                 :channels ("#emacs" "#c_lang_cn")))))

;; =============================================================================
;; �������������ļ�
(let ((path my-user-emacs-directory))
  (mapc (lambda (name)
          (load (concat path name) t nil nil t))
        '(;; prog-mode��text-mode���໥������
          "prog" ;; prog-mode
          ;; "prog-cc" ;; cc-mode (c-mode, c++-mode, java-mode)
          ;; "prog-lisp" ;; lisp-mode, emacs-lisp-mode, lisp-interaction-mode
          "prog-py" ;; python-mode
          ;; "prog-hs" ;; haskell-mode
          "text-tex" ;; tex-mode, latex-mode
          ;; "web-browser" ;; web browser
          )))

(message "emacs init time = %s" (emacs-init-time))
