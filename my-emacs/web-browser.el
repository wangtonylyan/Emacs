(require 'my-init)
;===========================================================================
; Text-Based Web Browser
;===========================================================================
; ������ҳ���������Ϊ�ı����ն�����������������ı��༭�����ն���ʹ�ã�����Ҫ�ľ�����������
; 1)w3m
; version: 0.5.3
; http://w3m.sourceforge.net/
; �����ṩ����Դ�룬���Ա��밲װ������Windowsƽ̨���ԣ�����ʹ���������ṩ��.exe��ִ�г���
; ��Emacs�п�����emacs-w3m���ʹ��
; 2)Lynx
; http://lynx.browser.org/
; 3)Links
; http://www.jikos.cz/~mikulas/links/


;===========================================================================
; emacs-w3m
;===========================================================================
; http://emacs-w3m.namazu.org/
; https://github.com/emacsmirror/w3m
; �˲����ΪEmacsʹ��w3m������Ľӿ�
; Ŀǰ�������ṩ���صķ�����v1.4.4��֧��Emacs23���ϰ汾����Ҫ�������¿�����
; �������ǿ����汾����辭����ע����ʵʱ�����Ӷ�����bug
; 1)��CVS������
; http://cvs.namazu.org/Development/emacs-w3m/
; [shell]$ cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m
; �����������������ṩ��github����
; https://github.com/doitian/emacs-w3m
; 2)��װ�ò��֮ǰ�������w3m�İ�װ���������ִ�г�������·�����뵽PATH��
; [shell]$ emacs -Q -l w3mhack.el NONE -f w3mhack-nonunix-install
;===========================================================================
(defun my-plugin-w3m-init ()
  (add-to-list 'exec-path (concat my-emacs-exec-bin-path "w3m"))
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "emacs-w3m"))
  (when (and (executable-find "w3m")
             (require 'w3m-load nil t))
    (setq w3m-home-page "http://www.baidu.com/")
    (setq w3m-use-cookies t)
    (setq w3m-use-favicon nil))
  ) ;end of my-plugin-w3m-init()

(defun my-plugin-w3m-start ()
  (w3m-mode)
  ) ;end of my-plugin-w3m-start()

(provide 'my-web-browser)
