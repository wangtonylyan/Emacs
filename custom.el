;; 下述内容会因用户在Customize界面的保存操作而由Emacs自动写入
(custom-set-variables
 '(rainbow-delimiters-max-face-count 9)
 '(rainbow-delimiters-outermost-only-face-count 0)

 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; :weight :: ultra-bold, extra-bold, bold, semi-bold, normal

 '(flyspell-incorrect ((t (:underline (:color "dim gray" :style wave)))))
 '(flyspell-duplicate ((t (:underline "dim gray"))))

 ;; '(highlight-thing ((t (:background "#DDDDDD")))) ;; light
 '(highlight-thing ((t (:background "#525252")))) ;; dark

 ;; 嵌套的括号通过大小而不仅是颜色来进行区分
 '(rainbow-delimiters-depth-1-face  ((t (:foreground "#70C070" :height 1.26 :weight semi-bold))))
 '(rainbow-delimiters-depth-2-face  ((t (:foreground "#70BC70" :height 1.24 :weight semi-bold))))
 '(rainbow-delimiters-depth-3-face  ((t (:foreground "#70B870" :height 1.22 :weight semi-bold))))
 '(rainbow-delimiters-depth-4-face  ((t (:foreground "#70B470" :height 1.20 :weight bold))))
 '(rainbow-delimiters-depth-5-face  ((t (:foreground "#70B070" :height 1.18 :weight bold))))
 '(rainbow-delimiters-depth-6-face  ((t (:foreground "#70AC70" :height 1.16 :weight bold))))
 '(rainbow-delimiters-depth-7-face  ((t (:foreground "#70A870" :height 1.14 :weight extra-bold))))
 '(rainbow-delimiters-depth-8-face  ((t (:foreground "#70A470" :height 1.12 :weight extra-bold))))
 '(rainbow-delimiters-depth-9-face  ((t (:foreground "#70A070" :height 1.10 :weight extra-bold))))

 ;; '(rainbow-identifiers-identifier-1 ((t (:foreground "#333333")))) ;; light
 ;; '(rainbow-identifiers-identifier-1 ((t (:foreground "#CCCCCC")))) ;; dark

 '(aw-leading-char-face ((t (:foreground "red" :weight ultra-bold :height 2.0))))
 )
