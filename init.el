(provide 'my-init)
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
;(load-file "E:/home/wm/.emacs.d/init.el")
; 2�������Ǳ��ļ�������
; ����Emacs�ڲ��Ļ����������˷�ʽ��Ҫÿ����Emacs����ʱ���������ã����ᱣ��
; Ҳ��������Windowsϵͳ�Ļ����������������Ƽ�����Ϊ��Ӱ�쵽����ͬ�������ڸñ��������
(when (eq system-type 'windows-nt)
  (setenv "HOME" "E:/")
  ; ����Ĭ�Ϲ���Ŀ¼
  (setq default-directory "~/")
  ; ����shell��ʹ����Emacs�ṩ��cmdproxy.exe
  (setq shell-file-name "D:/softwares/programming/emacs/bin/cmdproxy.exe")
  (setq shell-command-switch "-c")
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
 '(tool-bar-mode nil) ;ȡ��������
 '(electric-indent-mode nil) ;ȡ��ȫ���Ե��Զ�����
 '(global-font-lock-mode nil) ;ȡ��ȫ���Ե��﷨����
 '(ido-everywhere nil) ;��ʹido��ȫfind-file��switch-to-buffer
 '(ido-enable-flex-matching nil)
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
(setq user-full-name "Tony")
(setq user-mail-address "")

;; UI
;(setq inhibit-startup-message 1) ;ȡ����������
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
(mouse-avoidance-mode 'animate) ;������ƶ������λ��ʱ��Ϊ�����ڵ����ߣ��Զ��ƿ����
(setq-default line-spacing 0) ;�о�
(column-number-mode 1) ;��mode-line��ʾ����
(set-face-background 'default "#C7EDCC") ;���ñ�����ɫ
(set-face-attribute 'default nil :family "Monaco" :weight 'semi-bold) ;��������
(set-frame-font "11" nil t) ;�����ֺ�
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

;; Mode
;; ����������������minibuff���ṩ��ȫ���ܣ���֧��ͬ���������
;; ��ǰ����ҪTAB���������������Զ����֣���Ĭ�Ͻ�֧��find-file��switch-to-buffer
(icomplete-mode t)
(ido-mode t)
;(uniquify-mode 1) ;buffer����
;; built-in Speedbar (rather than CEDET Speedbar)
(setq speedbar-use-images nil) ;��ʹ��image��ʽ
(setq speedbar-show-unknown-files t)


;===========================================================================
; ����ȫ���Եĵ��������
;===========================================================================
;; �����������Դ��
;; https://github.com/emacsmirror

(setq my-emacs-config-file-path "~/.emacs.d/my-emacs/")
(setq my-emacs-plugin-load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path my-emacs-plugin-load-path)

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
;===========================================================================
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
;===========================================================================
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
; �������������ļ�
;===========================================================================
(mapc (lambda (name)
        (load (concat my-emacs-config-file-path name) t nil nil t))
      '(
        "prog" ;prog-mode
        "prog-cc" ;cc-mode (c-mode, c++-mode, java-mode)
        "prog-lisp" ;lisp-mode, emacs-lisp-mode, lisp-interaction-mode
        "prog-py" ;python-mode
        "text-tex" ;tex-mode, latex-mode
        "web-browser" ;web browser
        ))
