;; =============================================================================
(when (eq system-type 'windows-nt)
  (setq default-directory "~/")
  (let ((path "D:/softwares/emacs/libexec/emacs/24.5/i686-pc-mingw32/cmdproxy.exe"))
    (when (executable-find path)
      (setq shell-file-name path)
      (setq shell-command-switch "-c")))
  (let ((path "D:/softwares/"))
    (add-to-list 'exec-path path t)))

;; =============================================================================
;; �������ݻ����û���Customize����ı����������Emacs�Զ�д��
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
;; ����ͼ���ȫ���Եĵ��������
;; 1) ELPA (Emacs Lisp Package Archive)
;; ����ͨ��Emacs24���ϰ汾���õ�ELPA��������װ�͹�����������
;; 2) ���������ء����롢��װ���������
;; ��Դ��վ: https://github.com/emacsmirror
;; ͨ���İ�װ��ʽ���ǽ�.elԴ�ļ�������'load-path��ָ����Ŀ¼�м���
;; -----------------------------------------------------------------------------
;; (setq url-proxy-services '(("http" . "10.25.71.1:8080"))) ;; ��֧��authentication
(when (require 'package nil t)
  ;; Emacsʹ�õ�Ĭ�ϸ���ԴΪ��("gnu" . "http://elpa.gnu.org/")
  ;; ��Ӹ���Դ��MELPAÿ����£�������˾���������
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (setq package-load-list '(;; all
                            ;; ���²��������˳������ϱ˴�֮���������ϵ
                            (dash) (epl) (let-alist) (pkg-info) (flycheck)
                            (geiser) (paredit) (auctex)
                            (atom-one-dark-theme t) (material-theme t)
                            )) ;; ָ�������·�ʽ�����صĲ��
  (setq package-enable-at-startup nil) ;; ��ʽ1) ��Emacs���������Զ����ز��
  (package-initialize) ;; ��ʽ2) ����ִ�иú����Լ��ز��

  (setq package-selected-packages '(atom-one-dark-theme))
  (when (not package-archive-contents)
    (package-refresh-contents))
  ;; (package-install-selected-packages) ;; (re)install
  (mapc (lambda (pkg)
          (unless (package-installed-p pkg)
            (package-install pkg nil)))
        package-selected-packages))
;; ���ð�װ���Ĵ洢Ŀ¼����Ŀ¼Ҳ��Ҫ��������'load-path��
;; (add-to-list 'package-directory-list "~/.emacs.d/elpa") ;; system-wide dir
;; (setq package-user-dir "~/.emacs.d/elpa") ;; user-wide dir

;; =============================================================================
;; Color Theme
;; http://www.nongnu.org/color-theme/
;; �ò���Ѽ�����Emacs24���ϰ汾�У���ΪĬ�ϵ�����������
;; ���Դ���һЩ���⣬���ϻ��������ڸò����������������
;; -----------------------------------------------------------------------------
;; ָ������������İ�װĿ¼
(let ((path (concat package-user-dir "/color-theme")))
  (add-to-list 'custom-theme-load-path path)
  (add-to-list 'load-path path))

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
(when (and nil (require 'auctex nil t))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

;; =============================================================================
;; Smex
;; https://github.com/nonsequitur/smex
;; ����ȫ����ido-modeʵ�֣��ṩ�˶���execute-extended-command��Ϊǿ���֧��
;; �����˼�¼�û�������ʷ�����ٲ�ѯ��������Ŀ�ݼ�������˵���ȹ���
;; -----------------------------------------------------------------------------
(when (and nil (require 'smex nil t))
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; ��ԭM-x��ݼ��ܳ�ͻ
  (global-set-key (kbd "C-x M-x") 'execute-extended-command))

;; =============================================================================
;; Minimap
;; https://github.com/dustinlacewell/emacs-minimap
;; �ṩһ��������Sublime�༭���е�minimap����
;; ��ȫ���Ŀ�����ѡ�����(customize-group minimap)��
;; -----------------------------------------------------------------------------
(when (and nil (require 'minimap nil t))
  (setq minimap-always-recenter nil) ;����Ϊnil����Ч?
  (setq minimap-recenter-type 'middle)
  (setq minimap-buffer-name-prefix "MINI") ;����Ϊ�գ������޷�����minimap����
  (setq minimap-hide-fringes t)
  (setq minimap-hide-scroll-bar t)
  (setq minimap-update-delay 1.0)
  (setq minimap-window-location 'right)
  (setq minimap-display-semantic-overlays nil)
  (setq minimap-enlarge-certain-faces nil))

;; ===========================================================================
;; Powerline
;; https://github.com/jonathanchu/emacs-powerline
;; ���ṩ��һ��Ư����mode lineƤ����ȱ���ǵ�����̫�����̫��ʱ������ʾ������������
;; -----------------------------------------------------------------------------
(when (and nil (require 'powerline nil t))
  (setq powerline-arrow-shape
        ;; 'arrow
        ;; 'curve
        'arrow14))

;; ===========================================================================
;; Paredit
;; -----------------------------------------------------------------------------
(when (and nil (require 'paredit nil t))
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  (add-hook 'org-mode 'enable-paredit-mode))

;; =============================================================================
;; ��������
;; ----------------------------------------------------------------------------
;; Account
(setq user-full-name "TonyLYan")
(setq user-mail-address "wangtonylyan@outlook.com")

;; UI
;; (setq inhibit-startup-message 1) ;; ȡ����������
(tool-bar-mode -1) ;; ȡ��������
(setq frame-title-format "emacs@%b") ;; ���ñ�������ʾΪbuffer����
;; ����smooth scrolling���Բο�: http://www.emacswiki.org/emacs/SmoothScrolling
;; ���ṩ����Ҫ��������ǻ������������smooth-scroll.el��smooth-scrolling.el
;; �����ݲ�ʹ��ƽ������������ͨ���������±����Ծ����ܵر���ҳ�����ʱ�����Ƶ������
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
(scroll-bar-mode -1) ;; ȡ��������
(mouse-avoidance-mode 'animate) ;; ������ƶ������λ��ʱ��Ϊ�����ڵ����ߣ��Զ��ƿ����
;; (save-place-mode) ;; ��¼�����ÿ���ļ������һ�η���ʱ���ڵ�λ��
(setq-default line-spacing 0) ;; �о�
(column-number-mode) ;; ��mode-line��ʾ����

(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode) ;; ����������ƥ����ʾ
(setq show-paren-style 'parentheses)
(setq debug-on-error t) ;; ��ʾ������Ϣ
(fset 'yes-or-no-p 'y-or-n-p) ;; ��y/n�滻yes/no
(setq visible-bell t) ;; �Դ�����˸�ķ�ʽ���������ʾ��
(global-visual-line-mode t)

;; (set-face-background 'default "#C7EDCC") ;; ���ñ�����ɫΪ��ɫ����ɫ
;; ���������Դ����.ttf��.otf�ļ����Դ���Ԫ��Ϣ������family��style��
;; ����ʹ�ò�ͬ����Ӣ��������ֺŵ�Ŀ����Ϊ�����������ԣ�����ͬһ�����µ������ַ�ͨ������Ӣ���ַ�����
(if (eq system-type 'windows-nt)
    (progn
      (if (> emacs-major-version 24)
          (progn
            ;; Windowsϵͳ�ϵ�Emacs25�汾�������������ʾ�������⣬�������ĵ�ʱ����ڿ��ٵ����󣬱����ֶ�ָ����������Ϊ����ſɱ��⡣
            (set-default-font "Consolas 11")
            (set-fontset-font "fontset-default" 'unicode "���� 10"))
        (progn
          (set-default-font "Consolas 11")
          (set-fontset-font "fontset-default" 'unicode "Microsoft YaHei Mono 10"))))
  (progn
    (set-default-font "YaHei Consolas Hybrid 10")
    (set-fontset-font "fontset-default" 'unicode "Source Han Serif SC SemiBold 9") ;; ���滻��"Microsoft YaHei Mono 10"
    ))
;; (set-face-attribute 'default nil :family "Microsoft YaHei Mono" :weight 'normal :height 110) ;; �������壬�����ֺŵ�
;; (set-frame-font "10" nil t) ;; �����ֺ�, ͬ(set-face-attribute)�е�:height

;; Edit
(setq-default indent-tabs-mode nil) ;; make indentation commands use space only
(setq-default tab-width 4)
;; (setq tab-always-indent t)
(electric-indent-mode -1) ;; ȡ��ȫ���Ե��Զ�����ģʽ
(global-highlight-changes-mode)
(setq highlight-changes-global-changes-existing-buffers nil)
(setq highlight-changes-visibility-initial-state t)
(setq highlight-changes-face-list nil)
(setq highlight-changes-colors nil)
;; ÿ�α���bufferʱ����ɾ�����еĸĶ�����
;; �滻��������hook�ͻ���Ч��ԭ��δ֪��write-content-functions��write-file-functions
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;; ɾ��ÿ��ĩβ�Ŀո�
            (highlight-changes-remove-highlight (point-min) (point-max))))
(setq require-final-newline t)
;; (global-font-lock-mode -1) ;; ȡ��ȫ���Ե��﷨����ģʽ
;; (add-hook 'org-mode-hook 'turn-on-font-lock) ;; ����ͨ��ע��hook�ķ�ʽ���ض�ģʽ������

;; Backup and Revert
(setq make-backup-files t) ;; �����Զ�����
(setq version-control t) ;; ���ð汾���ƣ������Ա��ݶ��
(setq kept-old-versions 1) ;; ������ɵİ汾����
(setq kept-new-versions 1) ;; �������µİ汾����
(setq delete-old-versions t)
(setq dired-kept-versions 2)
(setq backup-directory-alist '(("." . "~/.emacs.d.backups"))) ;; ���ñ����ļ���·��
(setq backup-by-copying t) ;; �������÷�ʽΪֱ�ӿ���
(global-auto-revert-mode t) ;; ��Ӳ���ϵ��ļ����޸ĺ�Emacs����ʾ�û����¶�ȡ���ļ�

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
(put 'downcase-region 'disabled nil) ;; ȥ��ÿ��ִ�д�����ʱ����ʾ��ǿ��ִ��
(put 'upcase-region 'disabled nil)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-unset-key (kbd "C-q"))

;; Mode
;; icomplete��ido������ģʽ�����ṩ������minibuff�еĲ�ȫ����
;; find-file, switch-to-buffer, execute-extended-command
;; ǰ����ҪTAB����������ͨ��TAB�����ᴥ���½��Ӵ��ڳ��ֲ�ȫ������һ�㲻�ô�ģʽ
;; ���������Զ����֣���ȱʡ��֧��ǰ���ֲ�ȫ����
;; ���Smex��ʵ�־��ǻ��ں��߶���execute-extended-command��֧��
;; ���⣬����helmģʽ�����ṩ��ȫ���ܣ�ֻ������������������minibuff�������Ϣ
;; https://github.com/emacs-helm/helm
(icomplete-mode -1)
(when (require 'ido nil t)
  (ido-mode t)
  (ido-everywhere -1) ;; ��ʹido֧��find-file��switch-to-buffer
  (setq ido-enable-flex-matching t)
  (setq ido-enable-prefix t)
  (setq ido-enter-matching-directory nil))
;; uniquify-mode����Ϊ������buffer����
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil) ;; ��ʹ��image��ʽ
(setq speedbar-show-unknown-files t)

;; =============================================================================
;; �������������ļ�
(provide 'my-init)
(let ((path "~/.emacs.d/my-emacs/"))
  (mapc (lambda (name)
          (load (concat path name) t nil nil t))
        '(
          "prog" ;; prog-mode
          "prog-cc" ;; cc-mode (c-mode, c++-mode, java-mode)
          "prog-lisp" ;; lisp-mode, emacs-lisp-mode, lisp-interaction-mode
          "prog-py" ;; python-mode
          "prog-haskell" ;; haskell-mode
          "text-tex" ;; tex-mode, latex-mode
          "web-browser" ;; web browser
          ))

  ;; =============================================================================
  ;; org-mode
  ;; -----------------------------------------------------------------------------
  ;; (add-to-list 'org-babel-load-languages '(sh . t))
  ;; (add-to-list 'org-babel-load-languages '(ruby . t))
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))

  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (setq org-src-fontify-natively t)
  (setq org-directory "~/.emacs.d/org/")
  (setq org-default-notes-file (concat org-directory "default.org"))
  (setq org-todo-keywords
        '(
          (sequence "NEW(n)" "TODO(t)" "DOING(i)" "PEND(p)" "|" "CANCEL(c)" "DONE(d)")
          (type "HOME(h)" "WORK(w)")
          ))
  (setq org-todo-keyword-faces
        '(
          ("NEW" . (:background "orange" :foreground "black" :weight bold))
          ("TODO" . (:background "yellow" :foreground "black" :weight bold))
          ("DOING" . (:background "red" :foreground "black" :weight bold))
          ("PEND" . (:background "pink" :foreground "black" :weight bold))
          ("CANCEL" . (:background "lime green" :foreground "black" :weight bold))
          ("DONE" . (:background "green" :foreground "black" :weight bold))
          ))
  (let ((my-org-file-task (concat org-directory "task.org")))
    (setq org-capture-templates
          `(
            ("t" "Templates for task")
            ("tn" "new" entry (file+datetree ,my-org-file-task) "* NEW %? %T %^G\n")
            ("tt" "todo" entry (file+datetree ,my-org-file-task) "* TODO %? %T %^G\n")
            ("ti" "doing" entry (file+datetree ,my-org-file-task) "* DOING %? %T %^G\n")
            ("tp" "pend" entry (file+datetree ,my-org-file-task) "* PEND %? %T %^G\n")
            ("tc" "cancel" entry (file+datetree ,my-org-file-task) "* CANCEL %? %T %^G\n")
            ("td" "done" entry (file+datetree ,my-org-file-task) "* DONE %? %T %^G\n")

            ("o" "Templates for note")
            ("oo" "basic" entry (file+datetree (concat org-directory "note.org")) "* NOTE %? %T %^G\n")
            ("c" "Templates for calendar")
            ("cc" "basic" entry (file+datetree (concat org-directory "task.org")) "* CALENDAR %? %T %^G\n")
            ("p" "Templates for project")
            ("pp" "basic" entry (file+datetree (concat org-directory "project.org")) "* PROJECT %? %T %^G\n")
            )))

  (add-hook 'org-mode-hook
            (lambda ()
              (progn
                (setq truncate-lines nil)
                (org-indent-mode t))))
