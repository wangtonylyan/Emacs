;===========================================================================
; Style
;===========================================================================
(defun my-cc-style-init ()
  (load-file (concat my-emacs-config-file-path "prog-cc-style.emacs"))
  (define-key c-mode-base-map "\C-m" 'c-context-line-break) ;���к��Զ�����
  )
(defun my-cc-style-start ()
  (c-set-style "my-c-style") ;Ҳ����ͨ��(setq c-default-style)ʵ��
  (c-toggle-syntactic-indentation 1) ;���ø����﷨�����������κλ����﷨��style����ʧЧ
  (c-toggle-auto-newline 1) ;����auto newline
  (c-toggle-electric-state 1) ;����ĳЩ������semicolon���Զ���ʽ����ǰ��
;  (read-only-mode 1) ;���Ƽ�����������ֻ��ģʽ���������������ͻ
  )


;===========================================================================
; CEDET (Collection of Emacs Development Environment Tools)
;===========================================================================
; http://cedet.sourceforge.net/
; http://cedet.cvs.sourceforge.net/
; http://sourceforge.net/p/cedet/git/ci/master/tree/
; ���ڸò���Ķ����棬������CVS���ṩ�Ķ��Ǿ�δ���µ�ԭv1.1�汾
; ��ֻ��Git�潫ԭv1.1��������˼�����Emacs24
; ԭv1.1�汾�޷�������Emacs23���ϰ汾������Emacs23��ʼ�Ѽ��������ð�v2.x
; λ���䰲װĿ¼/lisp/cedet�£���Ȼ���ð��ڹ����������ԭv1.1���ǲ�������
; �������Ƽ�ʹ�����ð棬��Ϊ����Ŀǰ��δ������Ψһ��ά�ָ��µİ汾
; �����𽥷�չΪEmacs�ĺ������������Emacs��ͬ����
;===========================================================================
; �˲������Ȼ������Ͽ���֧�ֶ������ԣ���Ŀǰ��ʵ���н�����C/C++��֧����Ϊ����
; ��ˣ������Ƽ����������ԵĿ���������ʹ��
; �ü��ϰ��������¶�����(ֵ��ע����ǣ����֮����������Эͬ����)
;---------------------------------------------------------------------------
; Semantic
;---------------------------------------------------------------------------
; �������two parser generators��Bovine��Wisent
; ǰ�߿�����֧��C��C++��Lisp��������(����GNU Bisonʵ��)�������֧��Java��Javascript��Python
; ���������﷨���������õ��Ľ����Semantic����һ�����ṩ������һϵ����չ����
; ��֮��ȵ��ǣ��ɰ汾��Emacs�в���������ʽ��Ϊ�������ܵĻ���֧�֣����ԵñȽ������
; 1)Semantic/Analyzer (Smart Completion��Jump��Summary)
; ��Ҫ���﷨�����ĽǶ��ṩ�˴��벹ȫ����ת(���)����Ϣ���ܵȱ�̸�������
; semantic-ia-complete-symbol
; semantic-ia-complete-symbol-menu
; semantic-ia-complete-tip
; semantic-complete-analyze-inline-idle
; semantic-analyze-possible-completions
; semantic-ia-fast-jump
; 2)Senator (SEmantic/NAvigaTOR)
; ���ṩ�Ĺ�����Analyzer���ƣ���ʵ�ַ�ʽ�ϸ�Ϊ��
; Ҳ��˵�����������׼ȷ�Ƚϵͣ�����Ӧ�ٶȸ���
; senator-complete-symbol
; senator-completion-menu-popup
; 3)Semantic/Symref (SYMbol REFerence)
; ���ṩ���ǲ�ѯĳ����������Ŀ�����б����õ������еط�������Ҫ��Ϊǰ�˽ӿ�
; ����̨ʵ������ȫ�����ⲿ���ߣ�������GNU Global��Cscope��find/grep
; 4)Idle Scheduler
; ���û���������ʱ��ʵʩ�﷨������SemanticDB���ɡ���Ϣ��ʾ������
; ��Ҫ���ڵ���Semantic/Analyzer
; 5)SemanticDB
; �����ļ�����ʽ����������parser��tagging system�����ɵı�ǩ��Ϣ
; ���ǣ����벹ȫ����ת����Ϣ���ܵȹ��ܵ�ʵ�֣��������Ի���ʵʱ���﷨����
; Ҳ����ͨ���������ݿ�ķ�ʽ���Ӷ�Ҳ���������Щ���ù��ܵ�ִ��Ч��
; ���⣬��Semantic�������Ϊ����Դ��������������CEDET�����еĲ�����ʹ��
; ������õľͰ�����auto-complete, eassist
;---------------------------------------------------------------------------
; EDE (Emacs Development Environment)
;---------------------------------------------------------------------------
; emulating a typical IDE���ṩ�˶���Ӵ���
;---------------------------------------------------------------------------
; Speedbar
;---------------------------------------------------------------------------
; �ļ�Ŀ¼�ṹ�ĳ��֣����Semanticʹ���������Գ����ļ����ݵĽṹ
; �˲������ΪEmacs�Ļ�����������뼯����CEDET��ͬ������໥�����ҿ��ܰ汾��������
; Ŀǰͳһʹ��Emacs�Դ��Ĳ����Emacs����ʱ���Զ�����
;---------------------------------------------------------------------------
; SRecode (Semantic Recoder)
;---------------------------------------------------------------------------
; EIEIO (Enhanced Implementation of Emacs Interpreted Objects)
;---------------------------------------------------------------------------
; a CLOS (Common Lisp Object System) compatibility layer for Emacs Lisp
;---------------------------------------------------------------------------
; Cogre (COnnected GRaph Editor)
;---------------------------------------------------------------------------
; ���������������UMLͼ
;===========================================================================
; ����������Ҫ���������ð棬���������ܼ����ڶ�����
;===========================================================================
(defun my-plugin-cedet-init ()
  (require 'cedet)

  ;-------------------------------------------------------------------------
  ; Semantic
  ;-------------------------------------------------------------------------
  ;; ������semantic-default-submodes���ٵ���semantic-mode
  (setq semantic-default-submodes '(;; SemanticDB
                                    global-semanticdb-minor-mode
                                    ;; Idle Scheduler
                                    global-semantic-idle-scheduler-mode
                                    global-semantic-idle-summary-mode ;����Smart Summary
;                                    global-semantic-idle-completions-mode ;����Smart Completion
                                    global-semantic-idle-local-symbol-highlight-mode
                                    ;; Display and Decoration
                                    global-semantic-stickyfunc-mode
                                    global-semantic-highlight-func-mode
                                    global-semantic-decoration-mode
                                    ;; Senator
                                    global-semantic-mru-bookmark-mode ;mostly recently used
                                    ;; δ֪
;                                    global-cedet-m3-minor-mode
                                    ;; Debug
;                                    global-semantic-show-unmatched-syntax-mode
                                    global-semantic-show-parser-state-mode
;                                    global-semantic-highlight-edits-mode
                                    ))
  (semantic-mode 1) ;global minor mode
  ;; ����֮�⣬������ͬʱ��������һϵ�к���������Semantic����
;  (semantic-load-enable-minimum-features)
;  (semantic-load-enable-code-helpers)
;  (semantic-load-enable-guady-code-helpers)
;  (semantic-load-enable-excessive-code-helpers)
;  (semantic-load-enable-semantic-debugging-helpers)

  ;-------------------------------------------------------------------------
  ; Idle Scheduler
  ;-------------------------------------------------------------------------
  (setq semantic-idle-scheduler-idle-time 1)
  (setq semantic-idle-scheduler-max-buffer-size 10000)
  ;; ������������ʾ��semantic-idle-summary-mode���ͻ����
  (setq semantic-idle-scheduler-verbose-flag nil) ;ǰ�߱�����
  (setq semantic-idle-scheduler-no-working-message nil)
  (setq semantic-idle-scheduler-working-in-modeline-flag t) ;���߱�ת��
  ;; �ȽϺ�ʱ������
  (setq semantic-idle-scheduler-work-idle-time 30)
  (setq semantic-idle-work-parse-neighboring-files-flag t)
  ;; Idle Completion
  ; �Ժ��ַ�ʽ��ʾ
  (setq semantic-complete-inline-analyzer-idle-displayor-class
;        'semantic-displayor-ghost ;inline
;        'semantic-displayor-tooltip ;tooltip
        'semantic-displayor-traditional ;separate window
        )
  ; ��ʾ����
  (setq semantic-displayor-tooltip-mode
        'standard ;initial-max-tags
;        'quiet ;ֻ�е�����С��initial-max-tagsʱ����ʾ
;        'verbose ;��ʾ���У�ò����bug������
        )
  (setq semantic-displayor-tooltip-initial-max-tags 8)

  ;-------------------------------------------------------------------------
  ; SemanticDB
  ;-------------------------------------------------------------------------
  ;; ���ݿ��ļ���������
  (setq semanticdb-default-save-directory nil) ;��Ϊȱʡ·�������������ɵ����ݿ���ļ��Żᱣ���ڴ�
;  (setq semanticdb-default-file-name "")
  (setq semanticdb-persistent-path '(project)) ;��������EDE���������Ŀ��·����
  ;; �Ż�SemanticDB������/parse
  ;; 1)�޶�������Χ
  (mapc (lambda (mode)
          (setq-mode-local mode semanticdb-find-default-throttle
                           '(
                             file
                             local
                             project
                             system
                             recursive
                             unloaded ;�����������ļ���SemanticDBû�е���/���ɣ�����/����֮
                             omniscience ;�Լ����������ݿ�����ڴ���
                             )))
        '(c-mode c++-mode))
  ;; 2)���������޶���Χ�е�project���ͣ���Ҫ����EDE��JDE���������
;  (add-hook semanticdb-project-predicate-functions ) ;�����EDE����
;  (add-hook semanticdb-project-root-functions ) ;�����EDE����
  ; �������Ծ���ָ��һЩ��Ŀ�ĸ�Ŀ¼���ñ���Ҳ�ᱻsemantic-project-root-functions��ע��ĺ����޸�
;  (setq semanticdb-project-roots '())
  ;; 3)���������޶���Χ�е�system���ͣ�������semantic-dependency-system-include-path
  ; ����gcc�������Ϣ
  (when (executable-find "gcc")
    (require 'semantic/bovine/gcc)
    (semantic-gcc-setup)
    )
  ; ��Ҫ��ȫ���Զ��壬���������ã���׷��
;  (semantic-reset-system-include 'c-mode)
;  (semantic-reset-system-include 'c++-mode)
  (mapc (lambda (path)
          (semantic-add-system-include path 'c-mode)
          (semantic-add-system-include path 'c++-mode))
        '(;�˴����Լ�����ֳ��õĵ��������ļ�·��
          "." "./include" "./inc" "./common" "./public"
          ".." "../include" "../inc" "../common" "../public"
;          "C:/MinGW/include"
;          "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
          ))
  ; ��Semantic�Բ�����������ĳЩ���ţ�����Ҫ��һ��������ָ��
;  (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("symbol" . "value"))
;  (add-to-list 'semantic-lex-c-preprocessor-symbol-file  "path/file")
;  (setq semantic-c-obey-conditional-section-parsing-flag nil)
  ;; 4)����������ΪĳЩ����Ŀ¼�������ݿ⣬�Թ�����
  (setq semanticdb-search-system-databases t)
  (setq my-semanticdb-list '())
  ; ��ָ��Ŀ¼�Ѵ������ݿ��ļ����򲻻��ظ�����
  (mapc (lambda (path)
          (add-to-list 'my-semanticdb-list
                       (semanticdb-create-database semanticdb-new-database-class path)))
        '(
          "/usr/include"
;          "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
          ))
  ; ���ÿ������ʱ����֮ǰ�Ѵ��������ݿ�
  (mapc (lambda (mode)
          (setq-mode-local mode semanticdb-project-system-databases my-semanticdb-list))
        '(c-mode c++-mode))
  ;; 5)�޸�SemanticDB��̨֧�֣�ĿǰEmacs���ð���ֻ����������������(�����滹֧��ʹ��Cscope)
  ;; (a)Ebrowse
  ;; (require 'semantic/db-ebrowse)
  ;; ��ΪĬ�ϵ�ѡ�����ܽϲ�
  ;; (b)GNU Global
  ;; (require 'semantic/db-global)
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path (concat my-emacs-exec-bin-path "global/bin"))
    )
  (when (executable-find "global")
    (require 'semantic/db-global)
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode)
    )

  ;-------------------------------------------------------------------------
  ; EDE
  ;-------------------------------------------------------------------------
  (require 'ede)
  (global-ede-mode 1) ;���semantic-modeȫ���Ե�����
  ;; EDEĬ��ʹ��Unix�ϵ�Locate��������λ�ļ������⻹֧��ʹ��GNU Global
  ;; ��ĿǰEmacs���õ�CEDET��ɾ����ede-locate.el�ļ������Ҳ�Ͳ�֧���޸���
;  (setq ede-locate-setup-options '(ede-locate-global ede-locate-base))
  ; ������Ŀ��EDE��Ϣ��prog-cc-ede.emacs�����ļ�������ά��
  (load-file (concat my-emacs-config-file-path "prog-cc-ede.emacs"))

  ;-------------------------------------------------------------------------
  ; Display and Decoration
  ;-------------------------------------------------------------------------
  (setq semantic-stickyfunc-sticky-classes '(
                                             function
                                             type
;                                             variable
;                                             include
;                                             package
                                             ))
  (setq semantic-decoration-styles '(("semantic-tag-boundary" . t)
                                     ("semantic-decoration-on-private-members" . nil)
                                     ("semantic-decoration-on-protected-members" . nil)
                                     ("semantic-decoration-on-includes" . nil)))

  ;-------------------------------------------------------------------------
  ; Speedbar
  ;-------------------------------------------------------------------------
  ;; �˴�ΪCEDET�м��ɵ�Speedbar��Ŀǰ��δ����
;  (require 'semantic/sb)

  ;-------------------------------------------------------------------------
  ; �����������ع�������(������)
  ;-------------------------------------------------------------------------
  (require 'semantic/ia)
  ;semantic-ia-fast-jump
  (require 'semantic/symref)
  ;semantic-symref-symbol
  (require 'semantic/senator)
  ;senator���

  ) ;end of my-plugin-cedet-init()

(defun my-plugin-cedet-start ()
  (when (boundp 'ac-sources)
    (setq ac-sources
          (append my-prog-ac-sources '(ac-source-semantic)))
    )
;  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  ) ;end of my-plugin-cedet-start()


;===========================================================================
; ECB (Emacs Code Browser)
;===========================================================================
; version: 2.40
; http://ecb.sourceforge.net/
; �˲���Դ�2009��֮��Ͳ��ٸ��£����հ汾Ϊv2.4������CEDET v1.x
; ��˲���������Emacs23���ϰ汾�����õ�CEDET v2.x����Ҫ�޸�Դ���룬��������������£�
; 1)��Դ�ļ�������������Ϣ�����޸ģ���ЩΪCEDET�ɰ汾�Ľӿ�
; (provide 'semantic-analyze) ----> (require 'semantic/analyze)
; (provide 'semantic-ctxt)    ----> (require 'semantic/ctxt)
; (provide 'semanticdb)       ----> ɾ��
; (provide 'semanticdb-find)  ----> ɾ��
; (provide 'semanticdb-mode)  ----> ɾ��
; (provide 'semantic-load)    ----> ɾ��
; �����������漰�������������ļ���ecb-analyse.el��ecb-cedet-wrapper.el��ecb-method-browser.el
; 2)ע�ͻ�ɾ����ecb-upgrade.el�ļ��еļ��CEDET�汾��������䣺
; ;; check if vedet-version is correct
; (when (or (not (boundp 'cedet-version))
;     (ecb-package-version-list<
;      (ecb-package-version-str2list cedet-version)
;      ecb-required-cedet-version-min)
;     (ecb-package-version-list<
;      ecb-required-cedet-version-max
;      (ecb-package-version-str2list cedet-version)))
;  (setq version-error (concat "cedet ["
;                 cedet-required-version-str-min
;                 ", "
;                 cedet-required-version-str-max
;                 "]")))
; 3)���밲װ�Ĺ��̿ɷ���make.bat�ļ��е�����
; ����ECBĿ¼�´���ecb-compile-script-init�ļ�����д�����½ű�
; (add-to-list 'load-path "E:/.emacs.d/site-lisp/ecb") ;ECB����Ŀ¼
; (add-to-list 'load-path "D:/softwares/Emacs/lisp/cedet") ;CEDET����Ŀ¼
; (load-file "D:/softwares/Emacs/lisp/cedet/cedet.el") ;����CEDET�����ļ�
; (require 'ecb)
; (setq debug-on-error t)
; ����ִ������shell��������ECB�ı��밲װ����
; [$] cd E:/.emacs.d/site-lisp/ecb
; [$] emacs -Q -l ecb-compile-script-init --eval "(ecb-byte-compile t)"
; ���ӱ�������е�����warning��������ɺ��ɾ��ecb-compile-script-init�ļ�
;===========================================================================
; version: 2.40.1
; https://github.com/emacsmirror/ecb
; �˰汾����˰��������ᵽ�ļ����ش�ļ��������⣬������֧��Emacs23���ϰ汾
; ���밲װ����ͬ�ϣ�����������Ȼ����
;===========================================================================
(defun my-plugin-ecb-init ()
  (save-excursion
    (add-to-list 'load-path (concat my-emacs-plugin-load-path "ecb"))
    (require 'ecb)
    (unless (boundp 'stack-trace-on-error)
      (defvar stack-trace-on-error nil)) ;������
    (setq ecb-layout-name "left15"
;          ecb-toggle-layout-sequence '()
;          ecb-layout-window-sizes nil ;�Ƽ�ͨ������ecb-change-layout����Խ���ʽ�ķ�ʽ�޸�
          ecb-windows-width 0.2
          ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1
          ecb-tip-of-the-day nil
;          ecb-auto-compatibility-check nil
          )
    ;; directories window
    (setq ecb-source-path '("~"))
    (setq ecb-tree-buffer-style 'image)
    (setq ecb-auto-expand-directory-tree 'best)
    (setq ecb-excluded-directories-regexps '("^\\(\\.\\|\\.\\.\\)$"))
    (setq ecb-show-sources-in-directories-buffer '("left15"))
    ;; sources window
;    (setq ecb-source-file-regexps '())
;    (setq ecb-sources-exclude-cvsignore '())
    ;; methods window
    (setq ecb-process-non-semantic-files nil) ;����non-semantic-sources
    ;; history window
;    (setq ecb-history-exclude-file-regexps '())
    ;; compilation window
    (setq ecb-compile-window-height 0.2
          ecb-compile-window-width 'edit-window
          ecb-compile-window-temporally-enlarge 'both
          ecb-enlarged-compilation-window-max-height 0.5
          )
    (setq ecb-compilation-buffer-names ;�������Ƶ�buffer���ݽ��������ڸô���
          (append ecb-compilation-buffer-names '(("*Process List*")
                                                 ("*Proced*")
                                                 (".notes")
                                                 ("*appt-buf*")
                                                 ("*Compile-Log*")
                                                 ("*etags tmp*")
                                                 ("*svn-process*")
                                                 ("*svn-info-output*")
                                                 ("*Python Output*")
                                                 ("*Org Agenda*")
                                                 ("*EMMS Playlist*")
                                                 ("*Moccur*")
                                                 ("*Directory"))
                  ))
    (setq ecb-compilation-major-modes ;����ģʽ��buffer���ݽ��������ڸô���
          (append ecb-compilation-major-modes '(change-log-mode
                                                calendar-mode
                                                diary-mode
                                                diary-fancy-display-mode
                                                xgtags-select-mode
                                                svn-status-mode
                                                svn-info-mode
                                                svn-status-diff-mode
                                                svn-log-view-mode
                                                svn-log-edit-mode
                                                erc-mode
                                                gud-mode)
                  ))
    (ecb-minor-mode 1) ;global minor mode
    )
  ) ;end of my-plugin-ecb-init()

(defun my-plugin-ecb-start ()
  ) ;end of my-plugin-ecb-start()


;=========================================================================
; Source Code Tagging System
;=========================================================================
; http://stackoverflow.com/questions/12922526/tags-for-emacs-relationship-between-etags-ebrowse-cscope-gnu-global-and-exhu
; ��Դ����Ҫ�����¼��֣�
; 1)Etags
; ������Ϊ�򵥣���Ϊ����
; 2)Ctags
; �ܹ�֧�ֶ��41�����ԣ������Etags�����ɸ����metadata
; �ٷ���Ҫ֧����VIM������Emacs���ԣ��������޷�ʹ����Щ�����metadata����˹��ܼ�����ͬ��Etags
; 3)Cscope
; ����C/C++��Java�����쳣ǿ�󣬵�����������֧�ֲ��㣬���Դ��û�����
; 4)GNU Global��Gtags
; ����������Cscope���ŵ���ʵ���϶������κα༭������Ҳ��˿ɼ����ھ�������༭����
;===========================================================================
; GNU Global
;===========================================================================
; version: 6.4
; http://www.gnu.org/software/global/
; http://www.tamacom.com/global.html
; ��Emacs�����У��ɶ���ʹ�ã�Ҳ����Ϊ��������ĺ�̨֧�֣�
; 1)CEDET/Semantic/SemanticDB
; 2)CEDET/Semantic/Symref
; 3)CEDET/EDE/Locate
; �Ƽ����Emacs���ggtags��ʹ��GNU Global
;===========================================================================
; ggtags
;===========================================================================
; version: 0.8.10
; http://elpa.gnu.org/packages/ggtags.html
; https://github.com/leoliu/ggtags
;===========================================================================
(defun my-plugin-ggtags-init ()
  (require 'ggtags)
  )
(defun my-plugin-ggtags-start ()
  (ggtags-mode 1) ;local minor mode
  )


;===========================================================================
;===========================================================================
(defun my-cc-mode-init ()
  (my-cc-style-init)
  ;; ����������CEDET�Ĳ����������CEDET֮�󱻼���/����
  ;; ��������Զ�����/����CEDET��������������CEDET������ʧЧ
  ;; ������CEDET�໥�����Ĳ���ļ���/����˳���ڴ˱���ʾ��ָ��
  ;; ������Ҳ��Ӧ������hook��ִ��˳��
  (my-plugin-cedet-init)
  (my-plugin-ecb-init)
  )
(defun my-cc-mode-start ()
  (my-cc-style-start)
  (my-plugin-cedet-start)
  (my-plugin-ecb-start)
  )
(eval-after-load 'cc-mode ;/lisp/progmodes/cc-mode.el
  '(progn
     (add-hook 'c-initialization-hook 'my-cc-mode-init)
     (add-hook 'c-mode-hook 'my-cc-mode-start)
     (add-hook 'c++-mode-hook 'my-cc-mode-start)
     ))


;===========================================================================
; JDEE (Java Development Environment for Emacs)
;===========================================================================
; version: 2.4.2
; http://jdee.sourceforge.net/
; https://github.com/emacsmirror/jdee
;===========================================================================
(defun my-plugin-jdee-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "jdee/lisp"))
  (load "jde")
  ;; ����JDK����
  (setq jde-jdk-environment-variable nil)
  (setq jde-jdk-registry '(("1.8.0" . "C:/Program Files/Java/jdk1.8.0_40"))) ;����ע����JDK�汾
  (setq jde-jdk '("1.8.0")) ;ָ����ǰʹ�õ�JDK�汾
  ) ;end of my-pluign-jdee-init()

(defun my-plugin-jdee-start ()
  (when (boundp 'ac-sources)
    (setq ac-sources
          (append my-prog-ac-sources '(ac-source-eclim)))
    )
  ) ;end of my-plugin-jdee-start()
