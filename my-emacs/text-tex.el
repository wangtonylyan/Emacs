(require 'my-init)
;===========================================================================
; TeX
;===========================================================================
; TeX��������Knuth������һ�ű��ʽ������
; 1) engine (�൱�ڱ�����)
; TeX :: ��ԭʼ��TeX
; pdfTeX :: ��tex��ʽ��ͨ��dvi��ʽ��ֱ������pdf��ʽ
; XeTeX :: ������Unicode��OpenType��֧��
; LuaTeX ::����Lua��Ϊ��չ����
; 2) �궨�弯(�൱���﷨��չ�Ϳ�����)
; plain TeX :: ��ԭʼ�����
; AMSTeX :: ������ѧЭ���ṩ��һ�׺꼯
; LaTeX :: ʹ���ĵ����Ű����ֱ�۷���
; ConTeXt :: ��LaTeX�����������
; 3) distribution
; TeX Live :: �ɹ���TeX��֯TUG��������֧�ָ�ƽ̨����Linux����Ϊ����
; MiKTeX :: Windowsƽ̨��ʹ����Ϊ�㷺��һ�����а�
; ConTeXt Minimal :: ֧��ConTeXt
; 4) editor
; TeXworks :: ��෢�а����ṩ�ı༭��
; WinEdt
; Kile
; ��Ȼ����Emacs��Vim��Ҳ��ͨ�����ɲ������ʽ֧��TeX�ı༭
;---------------------------------------------------------------------------
; CTAN (the Comprehensive TeX Archive Network)
; http://www.ctan.org/
; CTAN is a set of Internet sites around the world
; that offer TeX-related material for download.
; ���ھ���:
; http://mirrors.xmu.edu.cn/CTAN/
; http://mirrors.lifetoy.org/CTAN/
;---------------------------------------------------------------------------

;===========================================================================
; AUCTeX
;===========================================================================
; http://www.gnu.org/software/auctex/
; http://ftp.gnu.org/gnu/auctex/
; Emacs��������һ����tex major mode������֧��TeX�ı༭����
; �����Ƽ�ʹ�ô˲��������Ч�����ȫ����/��������ж�Ӧ��majorģʽ
; TeX-mode (�̳���text-mode), plain-TeX-mode, LaTeX-mode, ams-tex-mode,
; ConTeXt-mode, Texinfo-mode, docTex-mode (���ϼ̳���TeX-mode)
; �ò��������SLIME����Ϊ���ַ��а��ǰ��Emacs�ӿ�
;---------------------------------------------------------------------------
; Windowsƽ̨�Ͽ�ʹ��������������ṩ��Ԥ����İ汾
; ���غ󽫽�ѹ��Ŀ¼��share/emacs/site-lisp�е������ļ�����Ŀ¼
; ��site-start.el��tex-site.el��site-start.d��auctex
; �ڲ��ı���֮�����λ�õ�ǰ���£������κ���load-path��ָ����·���¼���
; ���⻹��ѡ���Եؽ���ѹ��Ŀ¼��share/info��Ŀ¼������Info-directory-list
; ��Ԥ����汾�Ĵ����ʽ������site-start.el��ӵؼ���site-start.dĿ¼�µ�
; auctex.el��preview-latex.el�ļ�--����ͬ��
; ��site-start.el�ļ�����Ӧ��������Emacs����ʱ���Զ����ص�Ŀ¼��(��/lisp)
; site-start.dĿ¼�µ��ļ���tex-site.el�ļ���auctex��Ŀ¼Ӧ��������load-pathָ��Ŀ¼��
;---------------------------------------------------------------------------
; Linuxƽ̨�������INSTALL�ļ�˵�����в�����
; 1) ����configure�����޷��ܺõ��Զ�ʶ��site-lispĿ¼λ�ã�������ֶ����ã����磺
; ./configure --with-lispdir=/home/wm/.emacs.d/site-lisp/auctex��
; ./configure --with-lispdir=/usr/local/share/emacs/24.5/site-lisp
; 2) make
; 3) make install�ɹ��������auctex.el��preview-latex.el��tex-site.el��auctex���ļ���
; ��Щ�ļ���Ŀ¼�����λ��ֻҪ���ı䣬���������ƶ����κ�load-pathָ����·����
; ���⣬���ۺ���ƽ̨�ϣ�����ֱ������ELPA���߰�װ
;---------------------------------------------------------------------------
(defun my-plugin-auctex-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "auctex"))
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "auctex/site-start.d"))
  (when (and (load "auctex.el" t)
             (load "preview-latex.el" t))
    (when (eq system-type 'windows-nt)
      (add-to-list 'exec-path (concat my-emacs-exec-bin-path "MiKTeX/miktex/bin/x64/"))
      (when (executable-find "miktex-texworks")
        (require 'tex-mik nil t))
      ))
  ) ;end of my-plugin-auctex-init()

(defun my-plugin-auctex-start ()
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
;  (setq-default TeX-master nil)
  (reftex-mode t)
  (TeX-fold-mode t)
  ) ;end of my-plugin-auctex-start()

;===========================================================================
;===========================================================================
(defun my-TeX-mode-init ()
  (my-plugin-auctex-init)
  )
(defun my-TeX-mode-start ()
  (font-lock-mode 1)
  (linum-mode 1)
  (when (fboundp 'TeX-mode)
    (my-plugin-auctex-start))
  )
(eval-after-load 'simple ;/lisp/simple.el
  '(progn
     (my-TeX-mode-init)
     (when (boundp 'TeX-mode-hook)
       (add-hook 'TeX-mode-hook 'my-TeX-mode-start))
     ))


(defun my-LaTeX-mode-init ()
  )
(defun my-LaTeX-mode-start ()
  )
(eval-after-load 'tex-mode ;/lisp/textmodes/tex-mode.el
  '(progn
     (my-LaTeX-mode-init)
     (when (boundp 'LaTeX-mode-hook)
       (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-start))
     ))


;===========================================================================
;===========================================================================
(defun my-tex-mode-init ()
)
(defun my-tex-mode-start ()
  (font-lock-mode 1)
  (linum-mode 1)
  )
(eval-after-load 'tex-mode ;/lisp/textmodes/tex-mode.el
  '(progn
     (my-tex-mode-init)
     (add-hook 'tex-mode-hook 'my-tex-mode-start)
     ))


(defun my-latex-mode-init ()
  )
(defun my-latex-mode-start ()
  (font-lock-mode 1)
  (linum-mode 1)
  )
(eval-after-load 'tex-mode ;/lisp/textmodes/tex-mode.el
  '(progn
     (my-latex-mode-init)
     (add-hook 'latex-mode-hook 'my-latex-mode-start)
     ))

(provide 'my-text-tex)
