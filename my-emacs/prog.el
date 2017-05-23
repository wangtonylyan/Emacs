(require 'my-init)

(defvar my-prog-mode-start-hook '())

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
    (add-to-list 'yas-snippet-dirs
                 (concat my-user-emacs-directory "snippets"))
    ;; Ϊ���auto-complete��company�Ȳ����ʹ�ã��������������Ŀ�ݼ���ȫ����
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    ;; ���ý��ͬ��snippet�ķ�ʽ
    (setq yas-prompt-functions
          (if (eq system-type 'windows-nt)
              '(yas-ido-prompt yas-dropdown-prompt) ;; Windows�������Ƽ�������֧�ֲ���
            '(yas-x-prompt yas-dropdown-prompt)))
    ;; (yas-global-mode 1)
    (add-hook 'my-prog-mode-start-hook 'my-plugin-yasnippet-start t)))

(defun my-plugin-yasnippet-start ()
  (yas-minor-mode 1) ;; ���Զ�ִ��(yas-reload-all)
  )

;; =============================================================================
;; Company (complete anything)
;; http://company-mode.github.io/
;; https://github.com/company-mode/company-mode
;; https://www.emacswiki.org/emacs/CompanyMode
;; һ����auto-complete���ܻ������ƵĲ�ȫ���������ں��ߣ����¸�ΪƵ��
;; ----------------------------------------------------------------------------
(defun my-plugin-company-init ()
  (when (and (member 'company package-selected-packages)
             (require 'company nil t))
    ;; (customize-group 'company)
    ;; ���õĿ�ݼ���
    ;; TAB���ڲ�ȫ��ѡ���еĹ����ֶΣ�RETURN���ڲ�ȫ��ѡ�C-g������ֹ��ȫ
    ;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    ;; û�б�ҪΪÿ��ģʽ�ֱ����������ĺ�ˣ���Ϊɸѡ���ú�˵Ĺ��̷ǳ�Ч��
    (setq company-backends `(company-elisp
                             ,(when (and (member 'company-jedi package-selected-packages)
                                         (require 'company-jedi nil t))
                                'company-jedi)
                             ;; company-bbdb ;; Big Brother Database, an address book
                             ;; company-nxml
                             company-semantic ;; Semantic
                             company-clang ;; Clang
                             ;; company-xcode ;; Xcode
                             company-cmake ;; CMake
                             ;; company-eclim ;; Eclipse
                             ;; company-css ;; CSS
                             company-capf ;; completion-at-point-functions
                             company-files ;;
                             (company-dabbrev-code company-gtags company-etags company-keywords)
                             company-oddmuse
                             company-dabbrev)
          company-minimum-prefix-length 1
          company-idle-delay 0)
    ;; (global-company-mode 1)
    (add-hook 'my-prog-mode-start-hook 'my-plugin-company-start t)))

(defun my-plugin-company-start ()
  (company-mode 1))

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
    (add-to-list 'ac-dictionary-directories
                 (concat my-user-emacs-directory "ac-dicts"))
    (ac-set-trigger-key "TAB") ;; ac��������trigger key������ǿ����Ч
    (setq ac-trigger-commands '(self-insert-command
                                backward-delete-char
                                backward-delete-char-untabify)
          ac-ignore-case 'smart
          ac-dwim t ;; Do What I Mean
          ac-fuzzy-enable t
          ac-candidate-menu-height 8
          ;; performance
          ac-auto-start 2 ;; ac��������ָ���������ַ����Զ���Ч
          ac-delay 0.5
          ac-auto-show-menu nil ;; �����Զ���ʾ��ѡ�ʲ˵�
          ac-use-comphist t
          ac-candidate-limit 15 ;; �������
          ;; quick help
          ac-use-quick-help t
          ac-quick-help-delay 1.0)
    (ac-linum-workaround) ;; ���auto-complete��linum����ģʽ֮��ĳ�ͻ
    ;; source
    ;; auto-complete-config.el�ļ��ж����˴�������չsource
    ;; �Ӷ�ʹ��auto-complete�������Ĳ���༯��
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
    ;; (global-auto-complete-mode 1)
    (add-hook 'my-prog-mode-start-hook 'my-plugin-auto-complete-start t)))

(defun my-plugin-auto-complete-start ()
  (auto-complete-mode 1)
  ;; ���̳���prog-mode�ı��ģʽ������ʱ����������buffer-local��ac-sources
  ;; �����Ǹ���׷��my-prog-ac-sources����
  ;; �ŵ��ǵ�ͬһ��buffer����л���ͬ�ı��ģʽʱ������˴�Ӱ��
  (defvar my-prog-ac-sources
    (add-to-list ac-sources
                 '(ac-source-dictionary
                   ac-source-yasnippet)
                 t)))

;; =============================================================================
;; Flymake
;; Emacs���ã���̬�����飬Ч�ʵͣ�׼ȷ�ȸߣ������ں�̨��������֧��
;; -----------------------------------------------------------------------------
(defun my-plugin-flymake-init ()
  (when (and (member 'flymake package-selected-packages)
             (require 'flymake nil t))
    (add-hook 'my-prog-mode-start-hook 'my-plugin-flymake-start t)))

(defun my-plugin-flymake-start ()
  (flymake-mode-on))

;; =============================================================================
;; Flycheck
;; http://www.flycheck.org/
;; ��̬���������Ч�ʸߣ�׼ȷ�ȵͣ������ں�̨�﷨������(�������ǰ��)��֧��
;; ��Բ�ͬ�����谲װ������Ӧ�ĺ�̨֧�֣�����ɲμ�
;; http://www.flycheck.org/manual/latest/Supported-languages.html
;; -----------------------------------------------------------------------------
(defun my-plugin-flycheck-init ()
  ;; ��ݼ�ǰ׺��C-c !
  ;; C-c ! l :: (flycheck-list-errors)
  ;; RET :: Go to the current error in the source buffer
  ;; n :: Jump to the next error
  ;; p :: Jump to the previous error
  ;; e :: Explain the error
  ;; f :: Filter the error list by level
  ;; F :: Remove the filter
  ;; S :: Sort the error list by the column at point
  ;; g :: Check the source buffer and update the error list
  ;; q :: Quit the error list and hide its window
  (when (and (member 'flycheck package-selected-packages)
             (require 'flycheck nil t))
    (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
          flycheck-idle-change-delay 2.5
          flycheck-indication-mode nil)
    (setq-default flycheck-disabled-checkers
                  (add-to-list 'flycheck-disabled-checkers
                               'emacs-lisp-checkdoc t))

    (when (and (member 'emacs-lisp flycheck-checkers)
               (not (member 'emacs-lisp flycheck-disabled-checkers)))
      (add-hook 'emacs-lisp-mode-hook
                (lambda ()
                  (setq flycheck-emacs-lisp-load-path `(,my-user-emacs-directory)))
                t))
    (when (and (member 'c/c++-gcc flycheck-checkers)
               (not (member 'c/c++-gcc flycheck-disabled-checkers)))
      (add-hook 'c++-mode-hook
                (lambda ()
                  (setq flycheck-gcc-language-standard "c++11"))
                t))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*Flycheck errors*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side            . bottom)
                   (reusable-frames . visible)
                   (window-height   . 0.33)))
    ;; (global-flycheck-mode 1)
    (add-hook 'my-prog-mode-start-hook 'my-plugin-flycheck-start t)))

(defun my-plugin-flycheck-start ()
  (flycheck-mode-on-safe) ;; (flycheck-mode 1)
  )

;; =============================================================================
;; Magit
;; https://magit.vc/
;; https://www.emacswiki.org/emacs/Magit
;; https://www.masteringemacs.org/article/introduction-magit-emacs-mode-git
;; -----------------------------------------------------------------------------
;; (setq magit-auto-revert-mode 0
;;      magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
(defun my-plugin-magit-init ()
  (when (and (member 'magit package-selected-packages)
             (require 'magit nil t))
    (when (eq system-type 'windows-nt)
      (let ((path (my-func-executable-find "Git" "git.exe")))
        (when path
          (setq magit-git-executable path))))
    (setq magit-auto-revert-mode t
          magit-auto-revert-immediately t
          magit-auto-revert-tracked-only t
          magit-repository-directories `((,(expand-file-name "project") . 3)
                                         (,(expand-file-name "Project") . 3)))
    (global-set-key (kbd "C-c g") 'magit-status)
    (add-hook 'my-prog-mode-start-hook 'my-plugin-magit-start t)))

(defun my-plugin-magit-start ()
  )

;; =============================================================================
;; =============================================================================
(defun my-prog-mode-init ()
  (my-plugin-yasnippet-init)
  (my-plugin-company-init)
  (my-plugin-auto-complete-init)
  (my-plugin-flymake-init)
  (my-plugin-flycheck-init)
  (my-plugin-magit-init)
  (add-hook 'prog-mode-hook 'my-prog-mode-start t))

(defun my-prog-mode-start ()
  (turn-on-font-lock)
  (linum-mode 1)
  (run-hooks 'my-prog-mode-start-hook))

(add-hook 'after-init-hook 'my-prog-mode-init t)

(provide 'my-prog)
