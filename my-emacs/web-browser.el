(require 'my-init)
;===========================================================================
; Text-Based Web Browser
;===========================================================================
; 此类网页浏览器被称为文本或终端浏览器，适用于在文本编辑器或终端中使用，最主要的就是以下三者
; 1)w3m
; version: 0.5.3
; http://w3m.sourceforge.net/
; 官网提供的是源码，可以编译安装；对于Windows平台而言，可以使用由网友提供的.exe可执行程序
; 在Emacs中可与插件emacs-w3m配合使用
; 2)Lynx
; http://lynx.browser.org/
; 3)Links
; http://www.jikos.cz/~mikulas/links/


;===========================================================================
; emacs-w3m
;===========================================================================
; http://emacs-w3m.namazu.org/
; https://github.com/emacsmirror/w3m
; 此插件作为Emacs使用w3m浏览器的接口
; 目前官网上提供下载的发布版v1.4.4不支持Emacs23以上版本，需要下载最新开发版
; 但由于是开发版本因此需经常关注，以实时跟进从而避免bug
; 1)从CVS上下载
; http://cvs.namazu.org/Development/emacs-w3m/
; [shell]$ cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m
; 或下载以下由网友提供的github镜像
; https://github.com/doitian/emacs-w3m
; 2)安装该插件之前需先完成w3m的安装，并将其可执行程序所在路径加入到PATH中
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
