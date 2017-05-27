;; �ж�Emacs�汾���Ի�����������������'emacs-major-version��'emacs-minor-version

(defun my-func-executable-find (dir exe &optional add)
  (let* ((dir (if (and (stringp dir) (string-blank-p dir))
                  ;; ͳһ���ε���ʽ
                  (concat (directory-file-name dir) "/") ""))
         (path (executable-find (concat dir exe))))
    (when (file-executable-p path)
      (when add
        (add-to-list 'exec-path (directory-file-name path) t))
      path)))

(defalias 'my-func-minor-mode-on-p 'bound-and-true-p)

(defun my-func-package-enabled-p (pkg)
  (and (member pkg package-selected-packages)
       (require pkg nil t)))

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
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/") t)
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
  (setq package-selected-packages '(;; use-package
                                    atom-one-dark-theme
                                    powerline
                                    avy ;; ace-jump-mode
                                    ace-pinyin
                                    helm ;; icomplete, anything, ido, smex, ivy
                                    ;; helm-gtags
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
                                    auctex))
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install-selected-packages))

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

(when (and (member 'powerline package-selected-packages)
           (require 'powerline nil t))
  (powerline-default-theme))

(let ((exe (executable-find "aspell")))
  (when (and exe
             (member 'flyspell package-selected-packages)
             (require 'ispell nil t)
             (require 'flyspell nil t))
    (setq ispell-program-name exe ;; ���ú�̨֧�ֳ���
          ;; ispell-dictionary "english" ;; default dictionary
          ;; ispell-personal-dictionary ""
          flyspell-issue-message-flag nil)))

(when (and (member 'minimap package-selected-packages)
           (require 'minimap nil t))
  (setq minimap-always-recenter nil ;; ����Ϊnil����Ч?
        minimap-recenter-type 'middle
        minimap-buffer-name-prefix "MINI" ;; ����Ϊ�գ������޷�����minimap����
        minimap-hide-fringes t
        minimap-hide-scroll-bar t
        minimap-update-delay 1.0
        minimap-window-location 'right
        minimap-display-semantic-overlays nil
        minimap-enlarge-certain-faces nil))

(when (and (member 'powerline package-selected-packages)
           (require 'powerline nil t))
  (setq powerline-arrow-shape
        ;; 'arrow
        ;; 'curve
        'arrow14))

(when (and (member 'paredit package-selected-packages)
           (require 'paredit nil t))
  (add-hook 'lisp-mode-hook 'enable-paredit-mode t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode t)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode t)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode t)
  (add-hook 'org-mode 'enable-paredit-mode t))

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
(let* ((rslt (< (* (display-pixel-width) (display-pixel-height)) 2000000))
       (efont (if rslt 10 11))
       (cfont (if rslt 9 10))
       (fcnct (lambda (font size) (concat font " " (number-to-string size)))))
  (if (eq system-type 'windows-nt)
      (progn
        ;; Windowsϵͳ�ϵ�Emacs25�汾�������������ʾ�������⣬�������ĵ�ʱ����ڿ��ٵ�����
        ;; �����ֶ�ָ����������Ϊ����ſɱ��⡣
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

(when (not (member 'icomplete package-selected-packages))
  (icomplete-mode -1))
(when (and (member 'ido package-selected-packages)
           (require 'ido nil t))
  (ido-mode 1)
  (ido-everywhere -1) ;; ��ʹido֧��find-file��switch-to-buffer
  (setq ido-enable-prefix t
        ido-enable-flex-matching t
        ido-use-filename-at-point t
        ido-enter-matching-directory nil))
(when (and (member 'smex package-selected-packages)
           (require 'smex nil t))
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; �ò�����Զ��滻ԭM-x��ݼ����󶨵�������뱣��������°�֮
  (global-set-key (kbd "C-x M-x") 'execute-extended-command))
(when (and (member 'helm package-selected-packages)
           (require 'helm nil t)
           (require 'helm-config nil t))
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
  ;; Helm�ṩ��һ���ڹ������벿��Emacsԭ���������غϵ����
  ;; ������Ĭ�ϰ�������'helm-command-prefix-keyΪǰ׺�Ŀ�ݼ�����
  ;; ����ͨ�������ǰ׺�������������
  (global-set-key (kbd "C-c h") 'helm-command-prefix) ;; �滻ǰ׺
  (global-unset-key (kbd "C-x c"))
  ;; 'helm-execute-persistent-action�����'helm-select-action������
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<C-return>") 'helm-select-action)
  ;; Ҳ���Խ����ֳ�������ֱ���滻Emacsԭ��ݼ�
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-s") 'helm-occur)
  (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
  (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(when (and (member 'projectile package-selected-packages)
           (require 'projectile nil t))
  (projectile-global-mode 1)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t)
  ;; (add-to-list 'projectile-other-file-alist '("html" "js"))
  (when (member 'helm-projectile package-selected-packages)
    (with-eval-after-load 'helm
      (setq helm-projectile-fuzzy-match t)
      (when (require 'helm-projectile nil t)
        (setq projectile-completion-system 'helm
              projectile-switch-project-action 'helm-projectile)
        (helm-projectile-on)))))

;; ���helm-gtags��ʵ���ϲ��������ڲ��ggtags����˿���ȫ����֮
(with-eval-after-load 'helm
  (when (member 'helm-gtags package-selected-packages)
    ;; �ò���Ĳ����������Ҫ�ڼ���ǰ�����ã�����Ƽ�ʹ��customize
    ;; �˴�Ϊ�˼�㣬ȫ��ͳһ��(require)ǰ����
    (setq helm-gtags-path-style 'root
          helm-gtags-ignore-case t
          helm-gtags-read-only t
          helm-gtags-highlight-candidate t
          helm-gtags-display-style 'detail
          helm-gtags-fuzzy-match nil
          helm-gtags-direct-helm-completing nil
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-auto-update t
          helm-gtags-update-interval-second 60
          helm-gtags-prefix-key (kbd "C-c t")
          ;; ���������������ʹ��ĳЩ���ÿ�ݼ����ٰ�������ǰ׺��
          helm-gtags-suggested-key-mapping t)
    (when (require 'helm-gtags nil t)
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
                    (helm-gtags-mode 1)))))
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    ;; �����о��˲��ֿ�ݼ����ã������ο�
    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)))

;; =============================================================================
;; Magit
;; https://magit.vc/
;; https://www.emacswiki.org/emacs/Magit
;; https://www.masteringemacs.org/article/introduction-magit-emacs-mode-git
;; -----------------------------------------------------------------------------
(when (and (member 'magit package-selected-packages)
           (require 'magit nil t))
  (when (eq system-type 'windows-nt)
    (let ((path (my-func-executable-find "Git" "git.exe")))
      (when path
        (setq magit-git-executable path))))
  (setq magit-auto-revert-mode t
        magit-auto-revert-immediately t
        magit-auto-revert-tracked-only t
        ;; magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-repository-directories `((,(expand-file-name "project") . 3)
                                       (,(expand-file-name "Project") . 3)))
  (global-set-key (kbd "C-c g") 'magit-status))

;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil ;; ��ʹ��image��ʽ
      speedbar-show-unknown-files t)

;; Key
;; ���ǰ׺
;; C-c h :: helm
;; C-c t :: helm-gtags
;; C-c p :: projectile, helm-projectile
;; C-c g :: magit
;; C-c o :: org
(global-set-key (kbd "C-S-a") 'mark-whole-buffer)
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-l") 'windmove-right)
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-unset-key (kbd "C-x o")) ;; (other-window)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-up>") 'text-scale-increase)
(global-set-key (kbd "<C-down>") 'text-scale-decrease)
(global-set-key (kbd "C-x C--") 'downcase-region)
(global-set-key (kbd "C-x C-=") 'upcase-region)
(global-unset-key (kbd "C-x C-l")) ;; (downcase-region)
(global-unset-key (kbd "C-x C-u")) ;; (upcase-region)
(put 'downcase-region 'disabled nil) ;; ȥ��ÿ��ִ�д�����ʱ����ʾ��ǿ��ִ��
(put 'upcase-region 'disabled nil)
(global-set-key (kbd "C-q") 'read-only-mode)
(global-unset-key (kbd "C-x C-q"))
;; �����뷨�л�����ͻ
;; (global-set-key (kbd "C-S-SPC") 'set-mark-command)
;; (global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o a") 'org-agenda)

(when (my-func-package-enabled-p 'ace-jump-mode)
  (ace-jump-mode-enable-mark-sync)
  (global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
  (global-set-key (kbd "C-c SPC") 'ace-jump-char-mode))
(when (my-func-package-enabled-p 'avy)
  ;; (avy-setup-default)
  (setq avy-timeout-seconds 0.5)
  (global-set-key (kbd "C-:") 'avy-goto-char) ;; (avy-goto-char-timer)
  (global-set-key (kbd "C-'") 'avy-pop-mark))
(let ((plg (or (my-func-package-enabled-p 'ace-jump-mode)
               (my-func-package-enabled-p 'avy))))
  (when (and plg (my-func-package-enabled-p 'ace-pinyin))
    (when (eq plg 'ace-jump-mode)
      (setq ace-pinyin-use-avy nil)
      (ace-pinyin-global-mode 1)))
  (when (eq plg 'avy)
    (ace-pinyin-global-mode 1)))

;; File Extension
;; (setq auto-mode-alist (cons '("\\.emacs\\'" . emacs-lisp-mode) auto-mode-alist))

;; �������ڴ�С
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
                          '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0))))

(provide 'my-init)

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
