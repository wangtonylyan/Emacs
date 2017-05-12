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
      user-emacs-directory "~/.emacs.d/" ;; ע���·���Ƚ����⣬��"/"��β
      my-user-emacs-directory (concat user-emacs-directory "my-emacs" "/"))
;; (normal-top-level-add-subdirs-to-load-path)
;; (normal-top-level-add-to-load-path)

;; =============================================================================
;; (setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
;; (load custom-file)
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
;; ͨ���İ�װ��ʽ���ǽ�.elԴ�ļ�������'load-path��������Ŀ¼�м���
;; -----------------------------------------------------------------------------
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

  ;; Ŀǰʹ�ô�ȫ�ֱ�����������������/����
  ;; δ������ELPA���еĲ��Ҳͬ��ͨ���������н��й���
  (setq package-selected-packages '(;; 1) theme
                                    atom-one-dark-theme
                                    ;; 2) programming
                                    yasnippet company
                                    magit
                                    ;; flycheck
                                    ;; 3) python
                                    elpy py-autopep8
                                    ;; ropemacs
                                    ))
  (when (not package-archive-contents)
    (package-refresh-contents))
  (package-install-selected-packages))

;; =============================================================================
;; Color Theme
;; http://www.nongnu.org/color-theme/
;; �ò���Ѽ�����Emacs24���ϰ汾�У���ΪĬ�ϵ�����������
;; ���Դ���һЩ���⣬���ϻ��������ڸò����������������
;; -----------------------------------------------------------------------------
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
;; Smex
;; https://github.com/nonsequitur/smex
;; ����ȫ����ido-modeʵ�֣��ṩ�˶���execute-extended-command��Ϊǿ���֧��
;; �����˼�¼�û�������ʷ�����ٲ�ѯ��������Ŀ�ݼ�������˵���ȹ���
;; -----------------------------------------------------------------------------
(when (and (member 'smex package-selected-packages)
           (require 'smex nil t))
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; ��ԭM-x��ݼ��ܳ�ͻ
  (global-set-key (kbd "C-x M-x") 'execute-extended-command))

;; =============================================================================
;; Minimap
;; ��ȫ���Ŀ�����ѡ�����(customize-group minimap)��
;; -----------------------------------------------------------------------------
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

;; =============================================================================
;; Powerline
;; ���ṩ��һ��Ư����mode lineƤ����ȱ���ǵ�����̫�����̫��ʱ������ʾ������������
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
;; ��������
;; -----------------------------------------------------------------------------
;; Account
(setq user-full-name "TonyLYan"
      user-mail-address "wangtonylyan@outlook.com")

;; UI
(setq inhibit-startup-message 1) ;; ȡ����������
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
;; (save-place-mode 1) ;; ��¼�����ÿ���ļ������һ�η���ʱ���ڵ�λ��
(setq-default line-spacing 0) ;; �о�
(column-number-mode 1) ;; ��mode-line��ʾ����

(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode 1) ;; ����������ƥ����ʾ
(setq show-paren-style 'parentheses)
(setq debug-on-error t) ;; ��ʾ������Ϣ
(fset 'yes-or-no-p 'y-or-n-p) ;; ��y/n�滻yes/no
(setq visible-bell t) ;; �Դ�����˸�ķ�ʽ���������ʾ��
(global-visual-line-mode 1)

;; (set-face-background 'default "#C7EDCC") ;; ���ñ�����ɫΪ��ɫ����ɫ
;; ���������Դ����.ttf��.otf�ļ����Դ���Ԫ��Ϣ������family��style��
;; ����ʹ�ò�ͬ����Ӣ��������ֺŵ�Ŀ����Ϊ�����������ԣ�����ͬһ�����µ������ַ�ͨ������Ӣ���ַ�����
(if (eq system-type 'windows-nt)
    (progn
      (if (> emacs-major-version 24)
          (progn
            ;; Windowsϵͳ�ϵ�Emacs25�汾�������������ʾ�������⣬�������ĵ�ʱ����ڿ��ٵ�����
            ;; �����ֶ�ָ����������Ϊ����ſɱ��⡣
            (set-default-font "Consolas 11")
            (set-fontset-font "fontset-default" 'unicode "���� 10"))
        (progn
          (set-default-font "Consolas 11")
          (set-fontset-font "fontset-default" 'unicode "Microsoft YaHei Mono 10"))))
  (progn
    (set-default-font "YaHei Consolas Hybrid 10")
    (set-fontset-font "fontset-default"
                      'unicode "Source Han Serif SC SemiBold 9") ;; ���滻��"Microsoft YaHei Mono 10"
    ))
;; (set-face-attribute 'default nil :family "Microsoft YaHei Mono" :weight 'normal :height 110) ;; �������壬�����ֺŵ�
;; (set-frame-font "10" nil t) ;; �����ֺ�, ͬ(set-face-attribute)�е�:height

;; Edit
(setq-default indent-tabs-mode nil ;; make indentation commands use space only
              tab-width 4)
;; (setq tab-always-indent t)
(electric-indent-mode -1) ;; ȡ��ȫ���Ե��Զ�����ģʽ
(global-highlight-changes-mode 1)
(setq highlight-changes-global-changes-existing-buffers nil
      highlight-changes-visibility-initial-state t
      highlight-changes-face-list nil
      highlight-changes-colors nil)
;; ÿ�α���bufferʱ����ɾ�����еĸĶ�����
;; �滻��������hook�ͻ���Ч��ԭ��δ֪��write-content-functions��write-file-functions
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;; ɾ��ÿ��ĩβ�Ŀո�
            (highlight-changes-remove-highlight (point-min) (point-max))))
(setq require-final-newline t)
;; (global-font-lock-mode -1) ;; ȡ��ȫ���Ե��﷨����ģʽ
;; (add-hook 'xxx-mode-hook 'turn-on-font-lock) ;; ����ͨ��ע��hook�ķ�ʽ���ض�ģʽ������
;; (global-linum-mode 1)
;; (add-hook 'xxx-mode-hook 'linum-mode)

;; Backup and Revert
(setq make-backup-files t ;; �����Զ�����
      version-control t ;; ���ð汾���ƣ������Ա��ݶ��
      kept-old-versions 1 ;; ������ɵİ汾����
      kept-new-versions 1 ;; �������µİ汾����
      delete-old-versions t
      dired-kept-versions 2
      backup-directory-alist '(("." . "~/.emacs.d.backups")) ;; ���ñ����ļ���·��
      backup-by-copying t) ;; �������÷�ʽΪֱ�ӿ���
(global-auto-revert-mode 1) ;; ��Ӳ���ϵ��ļ����޸ĺ�Emacs����ʾ�û����¶�ȡ���ļ�

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
  (ido-mode 1)
  (ido-everywhere -1) ;; ��ʹido֧��find-file��switch-to-buffer
  (setq ido-enable-flex-matching t
        ido-enable-prefix t
        ido-enter-matching-directory nil))
;; uniquify-mode����Ϊ������buffer����
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil ;; ��ʹ��image��ʽ
      speedbar-show-unknown-files t)

(provide 'my-init)

;; =============================================================================
;; �������������ļ�
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
