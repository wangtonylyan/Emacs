;===========================================================================
;; Emacs initialization file
; ~/.emacs > ~/.emacs.el > ~/.emacs.d/init.el
;1)���ز���ķ�ʽ
;load��ÿ�ε���ʱ���ᣨ���£�����
;require����δ���أ������֮�����������¼���
;autoload��ʹ��ʱ��require
;eval-after-load����һ��load����֮������ִ�и�loaded file/feature�еĽű�֮ǰ
;���磺eval-after-load�л�"���ü�"ע��ĳ��ģʽ��hook
;��ڶ�����������Ϊquote form������Ļ�������Ϊ����ʱ����������
;2)Ŀǰ����ȡ�Ĳ���
;��Emacs����ʱload���������ļ�
;�����ļ��еĺ���������loadʱһͬevaluate
;����ĳ�ʼ������ʹ��eval-after-load����ע��������c-initialization-hook��hook�У����Ӧģʽ���״�������ִ�����ʼ������
;ȫ���Բ������������������ĳ�ʼ�����֮�󣬾ֲ��Բ�������������Ӧģʽ������


;===========================================================================
; Windows
;===========================================================================
; 1����C:\Users\�û���\AppData\Roaming\.emacs�ļ���ָʾ���뱾�ļ�
; (load-file "E:/home/wm/.emacs.d/init.el")
; ����Ӳ�����ϵͳ�Ļ�������HOME
; 2�������Ǳ��ļ�������
; ����Emacs�ڲ��Ļ����������˷�ʽ��Ҫÿ����Emacs����ʱ���������ã����ᱣ��
(when (eq system-type 'windows-nt)
;  (setenv "HOME" "E:/")
  ; ����Ĭ�Ϲ���Ŀ¼
  (setq default-directory "~/")
  ; ����shell��ʹ����Emacs�ṩ��cmdproxy.exe
  (setq my-shell-file-name "D:/softwares/programming/emacs-24.5/libexec/emacs/24.5/i686-pc-mingw32/cmdproxy.exe")
  (when (executable-find my-shell-file-name)
    (setq shell-file-name my-shell-file-name)
    (setq shell-command-switch "-c"))
  )


;===========================================================================
; Execution Path
;===========================================================================
;; ����Emacs��exec-path�������޸Ĳ���ϵͳ�Ļ���������
;; ��ȻҲ��������(setenv "PATH" "")��Ч����һ����
(when (eq system-type 'windows-nt)
  (setq my-emacs-exec-bin-path "D:/softwares/")
  (add-to-list 'exec-path my-emacs-exec-bin-path)
  )


;===========================================================================
; �������ݻ����û���Customize����ı����������Emacs�Զ�д��
;===========================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "Chinese-GB")
 '(ecb-options-version "2.40") ;ecb-minor-mode
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-changes ((t (:background "#CCFFCC"))))
 '(highlight-changes-delete ((t (:background "#CCFFCC"))))
 )


;===========================================================================
; ��������
;===========================================================================
;; Account
(setq user-full-name "TonyLYan")
(setq user-mail-address "wangtonylyan@outlook.com")

;; UI
;(setq inhibit-startup-message 1) ;ȡ����������
(tool-bar-mode -1) ;ȡ��������
(setq frame-title-format "emacs@%b") ;���ñ�������ʾΪbuffer����
;; ����smooth scrolling���Բο�
;; http://www.emacswiki.org/emacs/SmoothScrolling
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
      scroll-preserve-screen-position 1
      )
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)
(scroll-bar-mode -1) ;ȡ��������
(mouse-avoidance-mode 'animate) ;������ƶ������λ��ʱ��Ϊ�����ڵ����ߣ��Զ��ƿ����
(setq-default line-spacing 0) ;�о�
(column-number-mode 1) ;��mode-line��ʾ����
(set-face-background 'default "#C7EDCC") ;���ñ�����ɫ
(set-face-attribute 'default nil :family "Microsoft YaHei Mono" :weight 'normal :height 110) ;��������, e.g. Consolas
;(set-frame-font "11" nil t) ;�����ֺ�, ͬ(set-face-attribute)�е�:height
(show-paren-mode 1) ;����������ƥ����ʾ
(setq show-paren-style 'parentheses)
(setq debug-on-error t) ;��ʾ������Ϣ
(fset 'yes-or-no-p 'y-or-n-p) ;��y/n�滻yes/no
(setq visible-bell t) ;�Դ�����˸�ķ�ʽ���������ʾ��
(global-visual-line-mode t)

;; Edit
(setq-default indent-tabs-mode nil) ;make indentation commands use space only
(setq-default tab-width 4)
;(setq tab-always-indent t)
(electric-indent-mode -1) ;ȡ��ȫ���Ե��Զ�����ģʽ
(global-highlight-changes-mode 1)
(setq highlight-changes-global-changes-existing-buffers nil)
(setq highlight-changes-visibility-initial-state t)
(setq highlight-changes-face-list nil)
(setq highlight-changes-colors nil)
; ÿ�α���bufferʱ����ɾ�����еĸĶ�����
; �滻��������hook�ͻ���Ч��ԭ��δ֪��write-content-functions��write-file-functions
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace) ;ɾ��ÿ��ĩβ�Ŀո�
            (highlight-changes-remove-highlight (point-min) (point-max))
            ))
(global-font-lock-mode -1) ;ȡ��ȫ���Ե��﷨����ģʽ

;; Backup and Revert
(setq make-backup-files t) ;�����Զ�����
(setq version-control t) ;���ð汾���ƣ������Ա��ݶ��
(setq kept-old-versions 1) ;������ɵİ汾����
(setq kept-new-versions 1) ;�������µİ汾����
(setq delete-old-versions t)
(setq dired-kept-versions 2)
(setq backup-directory-alist '(("." . "~/.emacs.d.backups"))) ;���ñ����ļ���·��
(setq backup-by-copying t) ;�������÷�ʽΪֱ�ӿ���
(global-auto-revert-mode t) ;��Ӳ���ϵ��ļ����޸ĺ�Emacs����ʾ�û����¶�ȡ���ļ�

;; File Extension
;(setq auto-mode-alist
;      (cons '("\\.emacs\\'" . emacs-lisp-mode) auto-mode-alist))

;; Key
;(global-set-key "\C-c s" 'func)
;(define-key lisp-mode (kbd "C-c ;") 'func)
(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x l") 'windmove-right)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x a") 'mark-whole-buffer)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-up>") 'text-scale-increase)
(global-set-key (kbd "<C-down>") 'text-scale-decrease)

;; Mode
;; icomplete��ido������ģʽ�����ṩ������minibuff�еĲ�ȫ����
;; find-file, switch-to-buffer, execute-extended-command
;; ǰ����ҪTAB����������ͨ��TAB�����ᴥ���½��Ӵ��ڳ��ֲ�ȫ������һ�㲻�ô�ģʽ
;; ���������Զ����֣���ȱʡ��֧��ǰ���ֲ�ȫ����
;; ���Smex��ʵ�־��ǻ��ں��߶���execute-extended-command��֧��
(icomplete-mode -1)
(when (require 'ido nil t)
  (ido-mode t)
  (ido-everywhere -1) ;��ʹido֧��find-file��switch-to-buffer
  (setq ido-enable-flex-matching nil)
  (setq ido-enable-prefix t)
  (setq ido-enter-matching-directory nil))
;(uniquify-mode 1) ;buffer����
;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil) ;��ʹ��image��ʽ
(setq speedbar-show-unknown-files t)


;===========================================================================
; ����ͼ���ȫ���Եĵ��������
;===========================================================================
; 1) ELPA (Emacs Lisp Package Archive)
; ����ͨ��Emacs24���ϰ汾���õ�ELPA��������װ�͹�����������
;---------------------------------------------------------------------------
(when (require 'package nil t)
  ; Emacsʹ�õ�Ĭ�ϸ���ԴΪ��("gnu" . "http://elpa.gnu.org/")
  ; ��Ӹ���Դ��MELPAÿ����£�������˾���������
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (setq package-load-list '(;���²��������˳������ϱ˴�֮���������ϵ
;                            (dash t) (epl t) (let-alist t) (pkg-info t) (flycheck t)
                            all
                            )) ;ָ�������·�ʽ�����صĲ��
  (setq package-enable-at-startup nil) ;��ʽ1) ��Emacs���������Զ����ز��
  (package-initialize) ;��ʽ2) ����ִ�иú����Լ��ز��
  )
;---------------------------------------------------------------------------
; 2) ���������ء����롢��װ���������
; ��Դ��վ:
; https://github.com/emacsmirror
;---------------------------------------------------------------------------
(setq my-emacs-plugin-load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path my-emacs-plugin-load-path)

;===========================================================================
; Smex
;===========================================================================
; https://github.com/nonsequitur/smex
; ����ȫ����ido-modeʵ�֣��ṩ�˶���execute-extended-command��Ϊǿ���֧��
; �����˼�¼�û�������ʷ�����ٲ�ѯ��������Ŀ�ݼ�������˵���ȹ���
;===========================================================================
(when (require 'smex nil t)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-x M-x") 'execute-extended-command) ;ԭM-x��ݼ���
  )

;===========================================================================
; Minimap
;===========================================================================
; https://github.com/dustinlacewell/emacs-minimap
; �ṩһ��������Sublime�༭���е�minimap����
; ��ȫ���Ŀ�����ѡ�����(customize-group minimap)��
;===========================================================================
(when (and nil (require 'minimap nil t))
  (setq minimap-always-recenter nil) ;����Ϊnil����Ч?
  (setq minimap-recenter-type 'middle)
  (setq minimap-buffer-name-prefix "MINI") ;����Ϊ�գ������޷�����minimap����
  (setq minimap-hide-fringes t)
  (setq minimap-hide-scroll-bar t)
  (setq minimap-update-delay 1.0)
  (setq minimap-window-location 'right)
  (setq minimap-display-semantic-overlays nil)
  (setq minimap-enlarge-certain-faces nil)
  )

;===========================================================================
; Color Theme
;===========================================================================
; http://www.nongnu.org/color-theme/
; �ò���Ѽ�����Emacs24���ϰ汾�У���ΪĬ�ϵ�����������
; ���Դ���һЩ���⣬���ϻ��������ڸò����������������
;===========================================================================
(setq my-emacs-theme-load-path (concat my-emacs-plugin-load-path "color-theme"))
(add-to-list 'custom-theme-load-path my-emacs-theme-load-path)
(add-to-list 'load-path my-emacs-theme-load-path)
(setq my-emacs-enabled-theme-name nil)
;---------------------------------------------------------------------------
; [theme] Solarized
;---------------------------------------------------------------------------
; http://ethanschoonover.com/solarized
; https://github.com/altercation/solarized
; https://github.com/sellout/emacs-color-theme-solarized
; ����������ֻ�豣������.el�ű��ļ����ɣ�
; color-theme-solarized.el, solarized-theme.el, solarized-definitions.el
;---------------------------------------------------------------------------
(when (equal my-emacs-enabled-theme-name "solarized")
  ;; ������2��ģʽ
  (let ((mode
         'dark
;         'light
         ))
    (load-theme 'solarized t)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (set-frame-parameter frame 'background-mode mode)
                (set-terminal-parameter frame 'background-mode mode)
                (enable-theme 'solarized)))))
;---------------------------------------------------------------------------
; [theme] Tomorrow
;---------------------------------------------------------------------------
; https://github.com/chriskempson/tomorrow-theme
;---------------------------------------------------------------------------
(when (equal my-emacs-enabled-theme-name "tomorrow")
  ;; ������5������
  (let ((theme
         'tomorrow-day
;         'tomorrow-night
;         'tomorrow-night-blue
;         'tomorrow-night-bright
;         'tomorrow-night-eighties
         ))
    (load-theme theme t)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (enable-theme theme)))))

;===========================================================================
; Powerline
;===========================================================================
; https://github.com/jonathanchu/emacs-powerline
; ���ṩ��һ��Ư����mode lineƤ����ȱ���ǵ�����̫�����̫��ʱ������ʾ������������
;===========================================================================
(setq my-emacs-if-enable-powerline nil)
(when (and my-emacs-if-enable-powerline
           (require 'powerline nil t))
  (setq powerline-arrow-shape
;        'arrow
;        'curve
        'arrow14
        ))


;===========================================================================
; �������������ļ�
;===========================================================================
(provide 'my-init)
(setq my-emacs-config-file-path "~/.emacs.d/my-emacs/")
(mapc (lambda (name)
        (load (concat my-emacs-config-file-path name) t nil nil t))
      '(
        "prog" ;prog-mode
        "prog-cc" ;cc-mode (c-mode, c++-mode, java-mode)
        "prog-lisp" ;lisp-mode, emacs-lisp-mode, lisp-interaction-mode
        "prog-py" ;python-mode
        "prog-haskell" ;haskell-mode
        "text-tex" ;tex-mode, latex-mode
        "web-browser" ;web browser
        ))
