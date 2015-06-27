(provide 'my-prog)
(require 'my-init)
;===========================================================================
; Yasnippet
;===========================================================================
; https://github.com/capitaomorte/yasnippet
; һ�������ں�Ӧ�õĲ���������û��Զ���꣬���Զ�������չ
; yas�Ľű�snippet���ļ���Ŀ¼�ķ�ʽ���й���ÿ���ļ��ж���һ���꣬ÿ��Ŀ¼��Ӧ��һ��ģʽ
; ��yasģʽ�µ��ı���ͨ������ű��ļ������Լ����滻��
; ���ű�ע���е�name����ֻ����Ϊ�滻�ɹ��������ֳ���������Ϣ�������ͬ���ļ�ʱ����ʾѡ����Ϣ
;===========================================================================
(defun my-plugin-yasnippet-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "yasnippet"))
  (require 'yasnippet)
  (setq yas-snippet-dirs '()) ;ɾ��Ĭ��ֵ(��ѡ)
  (add-to-list 'yas-snippet-dirs (concat my-emacs-config-file-path "snippets"))
  ;; Ϊ���auto-completeʹ�ã��������������Ŀ�ݼ���ȫ����
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (setq yas-prompt-functions '(
;                               yas-x-prompt ;GTK�������Ƽ�
;                               yas-dropdown-prompt
;                               yas-completing-prompt
                               yas-ido-prompt ;Windows�������Ƽ�������֧�ֲ���
                               ))
;  (yas-global-mode 1) ;δȫ���Ե�����
  ) ;end of my-plugin-yasnippet-init()

(defun my-plugin-yasnippet-start ()
  (yas-minor-mode 1) ;����Yasnippet�����Զ�ִ��(yas-reload-all)
  ) ;end of my-plugin-yasnippet-start()

;===========================================================================
; Company (complete anything)
;===========================================================================
; http://company-mode.github.io/
; https://github.com/company-mode/company-mode
; һ����auto-complete���ܻ������ƵĲ�ȫ���������ں��ߣ����¸�ΪƵ������֧��ELPA��װ
;===========================================================================
(defun my-plugin-company-init ()
  )
(defun my-plugin-company-start ()
  )

;===========================================================================
; Auto-Complete
;===========================================================================
; http://auto-complete.org/
; https://github.com/auto-complete
; һ���ܹ�֧�ֶ��ֺ�̨ʵ�ֵĲ�ȫ���棬���Դ���һЩ֧�ֶ������ԵĲ�ȫ�ֵ�
; �ò���Ŀ���Դ������github�ϱ���ֳ������¼����Ӳ��֣���Ҫ�ֱ����ز���װ��
; 1)auto-complete: �������
; 2)popup-el: ��ʾ��������
; 3)fuzzy-el: ����ƥ�����
; ������ֻ�轫���Եĵ���.el�ļ�������auto-complete�İ�װĿ¼�¼�����ɰ�װ
;===========================================================================
(defun my-plugin-auto-complete-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "auto-complete"))
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (concat my-emacs-config-file-path "ac-dicts"))
  (ac-set-trigger-key "TAB") ;ac��������trigger key������ǿ����Ч
  (setq ac-trigger-commands '(self-insert-command
                              backward-delete-char
                              backward-delete-char-untabify))
  (setq ac-ignore-case 'smart)
  (setq ac-dwim t) ; Do What I Mean
  (setq ac-fuzzy-enable t)
  (setq ac-candidate-menu-height 8)
  (ac-linum-workaround)
  ;; performance
  (setq ac-auto-start 2) ;ac��������ָ���������ַ����Զ���Ч
  (setq ac-delay 0.5)
  (setq ac-auto-show-menu nil) ;�����Զ���ʾ��ѡ�ʲ˵�
  (setq ac-use-comphist t)
  (setq ac-candidate-limit 15) ;�������
  ;; quick help
  (setq ac-use-quick-help t)
  (setq ac-quick-help-delay 1.0)
  ;; source
  ;; auto-complete-config.el�ļ��ж����˴�������չsource
  ;; �Ӷ�ʹ��auto-complete�������Ĳ���༯��
  ;; Ӧ���ڸ���֮���������������ṩ����չ���ã����Զ���source
  (set-default 'ac-sources
               '(;; ���·��෴ӳ��ֻ��Ŀǰʵ�ʵ�ʹ����������Ǹ��Եľ��޷�Χ��
                 ac-source-filename
                 ac-source-files-in-current-dir
;                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
;                 ac-source-words-in-all-buffer
;                 ac-source-abbrev ;Emacs abbreviation
;                 ac-source-imenu ;Emacs imenu
                 ;; ���¸�Դ���ھ���ı��ģʽ����ʱ�����
                 ;; prog-mode
;                 ac-source-dictionary
;                 ac-source-yasnippet
                 ;; lisp-mode
;                 ac-source-slime
                 ;; emacs-lisp-mode
;                 ac-source-functions
;                 ac-source-variables
;                 ac-source-symbols
;                 ac-source-features ;(require '
                 ;; c-mode, c++-mode
;                 ac-source-semantic
;                 ac-source-semantic-raw
;                 ac-source-gtags
                 ;; java-mode
;                 ac-source-eclim
                 ;; python-mode
;                 ac-source-ropemacs
                 ;; other languages
;                 ac-source-ghc-mod ;Haskell
;                 ac-source-css-property ;CSS
                 ))
  ; ֻ���ڸ��б���ָ����ģʽ����Ч�������Ƿ�ȫ���Ե�����
;  (setq ac-modes '())
;  (global-auto-complete-mode 1) ;δȫ���Ե�����
  ) ;end of my-plugin-auto-complete-init()

(defun my-plugin-auto-complete-start ()
  (auto-complete-mode 1) ;����Auto-Complete
  ) ;end of my-plugin-auto-complete-start()


;===========================================================================
; Flymake
;===========================================================================
; Emacs���ã���̬�����飬Ч�ʵͣ�׼ȷ�ȸߣ������ں�̨��������֧��
;===========================================================================

;===========================================================================
; Flycheck
;===========================================================================
; version: 0.24
; http://www.flycheck.org/
; https://github.com/flycheck/flycheck
; ��̬�﷨��飬Ч�ʸߣ�׼ȷ�ȵͣ������ں�̨�﷨������(�������ǰ��)��֧��
; ��Բ�ͬ�����谲װ������Ӧ�ĺ�̨֧�֣�����ɲμ�
; http://www.flycheck.org/manual/latest/Supported-languages.html
; �Ƽ�ʹ��ELPA��װ����Ϊ�仹����������������û���ṩ�Ŀ�����
;===========================================================================
(defun my-plugin-flycheck-init ()
  ; ����ͨ�����·�ʽΪÿ��ģʽ������Ӧ��checker��ȡ�Ա���flycheck-checkers
;  (add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-select-checker 'emacs-lisp))
;  (global-flycheck-mode 1)
  )
(defun my-plugin-flycheck-start ()
  (flycheck-mode 1) ;����Flycheck
  )

;===========================================================================
;===========================================================================
(defun my-prog-mode-init ()
  (my-plugin-yasnippet-init)
  (my-plugin-auto-complete-init)
  (my-plugin-flycheck-init)
  )
(defun my-prog-mode-start ()
  (font-lock-mode 1) ;�����﷨����
  (linum-mode 1) ;��buffer�����ʾ�к�
  (when (fboundp 'yas-minor-mode)
    (my-plugin-yasnippet-start))
  (when (fboundp 'auto-complete-mode)
    (my-plugin-auto-complete-start)
    ;; ���̳���prog-mode�ı��ģʽ������ʱ����������buffer-local��ac-sources
    ;; �����Ǹ���׷��my-prog-ac-sources����
    ;; �ŵ��ǵ�ͬһ��buffer����л���ͬ�ı��ģʽʱ������˴�Ӱ��
    (setq my-prog-ac-sources
          (append ac-sources '(ac-source-dictionary
                               ac-source-yasnippet))))
  (when (fboundp 'flycheck-mode)
    (my-plugin-flycheck-start))
  )
(eval-after-load 'simple ;/lisp/simple.el
  '(progn
     (my-prog-mode-init)
     (add-hook 'prog-mode-hook 'my-prog-mode-start)
     ))
