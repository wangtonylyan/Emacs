(require 'my-init)

;; =============================================================================
;; Yasnippet
;; һ��������Ӧ�õĲ���������û��Զ���꣬���Զ�������չ
;; yas�Ľű�snippet���ļ���Ŀ¼�ķ�ʽ���й���ÿ���ļ��ж���һ���꣬ÿ��Ŀ¼��Ӧ��һ��ģʽ
;; ��yasģʽ�µ��ı���ͨ������ű��ļ������Լ����滻��
;; ���ű�ע���е�name����ֻ����Ϊ�滻�ɹ��������ֳ���������Ϣ�������ͬ���ļ�ʱ����ʾѡ����Ϣ
;; -----------------------------------------------------------------------------
(defun my-plugin-yasnippet-init ()
  (when (and (member 'yasnippet package-selected-packages)
             (require 'yasnippet nil t))
    (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "my-emacs/snippets"))
    ;; Ϊ���auto-complete��company�Ȳ����ʹ�ã��������������Ŀ�ݼ���ȫ����
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    ;; ���ý��ͬ��snippet�ķ�ʽ
    (setq yas-prompt-functions
          (if (eq system-type 'windows-nt)
              '(yas-ido-prompt) ;; Windows�������Ƽ�������֧�ֲ���
            '(yas-x-prompt yas-dropdown-prompt)))
    ;; (yas-global-mode 1) ;; δȫ���Ե�����
    ))

(defun my-plugin-yasnippet-start ()
  (yas-minor-mode 1) ;; ����Yasnippet�����Զ�ִ��(yas-reload-all)
  )

;; =============================================================================
;; Company (complete anything)
;; http://company-mode.github.io/
;; https://github.com/company-mode/company-mode
;; һ����auto-complete���ܻ������ƵĲ�ȫ���������ں��ߣ����¸�ΪƵ��
;; ----------------------------------------------------------------------------
;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
;; http://tuhdo.github.io/c-ide.html#sec-2
;; company�ĺ���кܶ࣬����������ϣ�����������M-x customize-group company ��Company Backends���濴��

;; =============================================================================
;; Auto-Complete
;; http://auto-complete.org/
;; https://github.com/auto-complete
;; һ���ܹ�֧�ֶ��ֺ�̨ʵ�ֵĲ�ȫ���棬���Դ���һЩ֧�ֶ������ԵĲ�ȫ�ֵ�
;; �ò���Ŀ�������ֳ������¼����������������Ҫ�ֱ�ض�����װ��
;; 1) auto-complete: �������
;; 2) popup-el: ��ʾ��������
;; 3) fuzzy-el: ����ƥ�����
;; -----------------------------------------------------------------------------
(defun my-plugin-auto-complete-init ()
  (when (and (member 'auto-complete package-selected-packages)
             (require 'auto-complete-config nil t))
    (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dicts"))
    (ac-set-trigger-key "TAB") ;; ac��������trigger key������ǿ����Ч
    (setq ac-trigger-commands '(self-insert-command
                                backward-delete-char
                                backward-delete-char-untabify))
    (setq ac-ignore-case 'smart)
    (setq ac-dwim t) ;; Do What I Mean
    (setq ac-fuzzy-enable t)
    (setq ac-candidate-menu-height 8)
    (ac-linum-workaround)
    ;; performance
    (setq ac-auto-start 2) ;; ac��������ָ���������ַ����Զ���Ч
    (setq ac-delay 0.5)
    (setq ac-auto-show-menu nil) ;; �����Զ���ʾ��ѡ�ʲ˵�
    (setq ac-use-comphist t)
    (setq ac-candidate-limit 15) ;; �������
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
                   ;; ac-source-words-in-buffer
                   ac-source-words-in-same-mode-buffers
                   ;; ac-source-words-in-all-buffer
                   ;; ac-source-abbrev ;; Emacs abbreviation
                   ;; ac-source-imenu ;; Emacs imenu
                   ;; ���¸�Դ���ھ���ı��ģʽ����ʱ�����
                   ;; 1) prog-mode
                   ;; ac-source-dictionary
                   ;; ac-source-yasnippet
                   ;; 2) lisp-mode
                   ;; ac-source-slime
                   ;; 3) emacs-lisp-mode
                   ;; ac-source-functions
                   ;; ac-source-variables
                   ;; ac-source-symbols
                   ;; ac-source-features ;; (require)
                   ;; 4) c-mode, c++-mode
                   ;; ac-source-semantic
                   ;; ac-source-semantic-raw
                   ;; ac-source-gtags
                   ;; 5) java-mode
                   ;; ac-source-eclim
                   ;; 6) python-mode
                   ;; ac-source-ropemacs
                   ;; 7) other languages
                   ;; ac-source-ghc-mod ;; Haskell
                   ;; ac-source-css-property ;; CSS
                   ))
    ;; ֻ���ڸ��б���ָ����ģʽ����Ч�������Ƿ�ȫ���Ե�����
    ;; (setq ac-modes '())
    ;; (global-auto-complete-mode 1) ;; δȫ���Ե�����
    ))

(defun my-plugin-auto-complete-start ()
  (auto-complete-mode 1) ;; ����Auto-Complete
  )

;; =============================================================================
;; Flycheck
;; version: 0.24
;; http://www.flycheck.org/
;; https://github.com/flycheck/flycheck
;; ��̬���������Ч�ʸߣ�׼ȷ�ȵͣ������ں�̨�﷨������(�������ǰ��)��֧��
;; ��Բ�ͬ�����谲װ������Ӧ�ĺ�̨֧�֣�����ɲμ�
;; http://www.flycheck.org/manual/latest/Supported-languages.html
;; -----------------------------------------------------------------------------
(defun my-plugin-flycheck-init ()
  (when (and (member 'flycheck package-selected-packages)
             (require 'flycheck nil t))
    ;; ����ͨ�����·�ʽ����ÿ��ģʽ������������Ӧ��checker(ȡ�Ա���flycheck-checkers)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (setq flycheck-idle-change-delay 2.5)
                (setq flycheck-emacs-lisp-load-path 'inherit)))
    ;; (global-flycheck-mode 1)
    ))

(defun my-plugin-flycheck-start ()
  (flycheck-mode 1) ;; ����Flycheck
  )

;; =============================================================================
;; Flymake
;; Emacs���ã���̬�����飬Ч�ʵͣ�׼ȷ�ȸߣ������ں�̨��������֧��

;===========================================================================
;===========================================================================
(defun my-prog-mode-init ()
  (my-plugin-yasnippet-init)
  (my-plugin-auto-complete-init)
  (my-plugin-flycheck-init))

(defun my-prog-mode-start ()
  (font-lock-mode 1) ;; �����﷨����
  (linum-mode 1) ;; ��buffer�����ʾ�к�
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
    (my-plugin-flycheck-start)))

(eval-after-load 'simple ;; /lisp/simple.el
  '(progn
     (my-prog-mode-init)
     (add-hook 'prog-mode-hook 'my-prog-mode-start)))

(provide 'my-prog)
