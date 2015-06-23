(provide 'my-text-tex)
(require 'my-init)
;===========================================================================
; TeX
;===========================================================================
; TeX是最早由Knuth发明的一门标记式宏语言
; 1) engine (相当于编译器)
; TeX :: 最原始的TeX
; pdfTeX :: 将tex格式不通过dvi格式而直接生成pdf格式
; XeTeX :: 增加了Unicode和OpenType的支持
; LuaTeX ::　以Lua作为扩展语言
; 2) 宏定义集(相当于语法扩展和开发库)
; plain TeX :: 最原始的命令集
; AMSTeX :: 美国数学协会提供的一套宏集
; LaTeX :: 使得文档的排版更加直观方便
; ConTeXt :: 比LaTeX更加灵活自由
; 3) distribution
; TeX Live :: 由国际TeX组织TUG开发，虽支持各平台，但Linux上最为适用
; MiKTeX :: Windows平台上使用最为广泛的一个发行版
; ConTeXt Minimal :: 支持ConTeXt
; 4) editor
; TeXworks :: 许多发行版所提供的编辑器
; WinEdt
; Kile
; 当然诸如Emacs、Vim等也可通过集成插件的形式支持TeX的编辑
;---------------------------------------------------------------------------
; CTAN (the Comprehensive TeX Archive Network)
; http://www.ctan.org/
; CTAN is a set of Internet sites around the world
; that offer TeX-related material for download.
; 国内镜像:
; http://mirrors.xmu.edu.cn/CTAN/
; http://mirrors.lifetoy.org/CTAN/
;---------------------------------------------------------------------------

;===========================================================================
; AUCTeX
;===========================================================================
; http://www.gnu.org/software/auctex/
; Emacs中内置了一整套tex major mode，用于支持TeX的编辑功能
; 但仍推荐使用此插件，其生效后会完全覆盖/替代内置中对应的major模式
; TeX-mode (继承于text-mode), plain-TeX-mode, LaTeX-mode, ams-tex-mode,
; ConTeXt-mode, Texinfo-mode, docTex-mode (以上继承于TeX-mode)
; 该插件类似于SLIME，作为各种发行版的前端Emacs接口
;---------------------------------------------------------------------------
; 其官网上为Windows平台提供了预编译的版本
; 下载后将解压缩目录下share/emacs/site-lisp子目录中的所有文件
; 在不改变site-start.d、site-start.el、tex-site.el此三者相对位置的前提下
; 全部移至Emacs的插件存放目录中即可，目前还暂将上述三者共同移至了auctex子目录中
; 原本site-start.d中的文件应放置于某个Emacs启动时会自动加载的目录下(如/lisp)
; 但目前采用由以下代码加载的方式
; 此外，还可将解压缩目录下share/info子目录加入至Info-directory-list
;---------------------------------------------------------------------------
(defun my-plugin-auctex-init ()
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "auctex"))
  (add-to-list 'load-path (concat my-emacs-plugin-load-path "auctex/site-start.d"))
  (load "auctex.el")
  (load "preview-latex.el")
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path (concat my-emacs-exec-bin-path "MiKTeX/miktex/bin/x64/"))
    (when (executable-find "miktex-texworks")
      (require 'tex-mik))
    )

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
(eval-after-load 'tex-mode ;/lisp/textmodes/tex-mode.el
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
)
(eval-after-load 'tex-mode ;/lisp/textmodes/tex-mode.el
  '(progn
     (my-tex-mode-init)
     (add-hook 'tex-mode-hook 'my-tex-mode-start)
     ))


(defun my-latex-mode-init ()
  )
(defun my-latex-mode-start ()
  )
(eval-after-load 'tex-mode ;/lisp/textmodes/tex-mode.el
  '(progn
     (my-latex-mode-init)
     (add-hook 'latex-mode-hook 'my-latex-mode-start)
     ))
