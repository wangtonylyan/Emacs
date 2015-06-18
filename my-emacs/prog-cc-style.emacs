;===========================================================================
; Style for C, C++, Java
;===========================================================================
(defconst my-c-style
  '(
    ; inherited from built-in stroustrup style
    "stroustrup"

    ;-----------------------------------------------------------------------
    ; Indentation
    ; [Emacs Info] CC Mode: Indentation Engine Basics, Customizing Indentation
    ;-----------------------------------------------------------------------
    (c-basic-offset . 4)
;   (c-offsets-alist . ((SYMBOL . OFFSET)))
;   (c-indent-comment-alist . )
;   (c-indent-comments-syntactically-p . )
;   (c-comment-only-line-offset . )
;   (c-special-indent-hook . ())
;   (c-label-minimum-indentation . )
    (c-tab-always-indent . t)
    (c-echo-syntactic-information-p . t)
    (c-report-syntactic-errors . t)

    ;-----------------------------------------------------------------------
    ; Auto Filling
    ; [Emacs Info] CC Mode: Custom Filling and Breaking
    ;-----------------------------------------------------------------------
    ; 用于识别出注释行
    (c-comment-prefix-regexp . ((c-mode    . "//+\\|\\**")
                                (c++-mode  . "//+\\|\\**")
                                (java-mode . "//+\\|\\**")))
    ; 设置block comment在换行时自动添加的前缀
    (c-block-comment-prefix . "*")
    ; 设置在哪些地方禁用auto-fill功能，目前仅在注释中启用
    (c-ignore-auto-fill . (string cpp code))

    ;-----------------------------------------------------------------------
    ; Auto Newlines
    ; [Emacs Info] CC Mode: Custom Auto-newlines, Clean-ups
    ;-----------------------------------------------------------------------
    ; 设置大括号的前后是否应换行，即"{"将位于行头或行尾或单行或任意位置
    ; 不在以下alist中列出的syntactic symbol，将执行默认行为：(before after)
    (c-hanging-braces-alist . ((defun-open)
                               (defun-close)
                               (class-open)
                               (class-close)
                               (inline-open)
                               (inline-close)
                               (block-open)
                               (block-close)
                               (statement-cont)
                               (substatement-open)
                               (statement-case-open   . (before after))
                               (brace-list-open)
                               (brace-list-close)
                               (brace-list-intro)
                               (brace-entry-open)
                               (extern-lang-open      . (after))
                               (extern-lang-close     . (before))
                               (namespace-open        . (after))
                               (namespace-close       . (before))
                               (module-open           . (after))
                               (module-close          . (before))
                               (composition-open      . (after))
                               (composition-close     . (before))
                               (inexpr-class-open     . (after))
                               (inexpr-class-close    . (before))
                               (arglist-cont-nonempty)))
    ; 设置冒号的前后是否应换行
    ; 不在以下alist中列出的syntactic symbol，将执行默认行为：()
    (c-hanging-colons-alist . ((case-label)
                               (label          . (after))
                               (access-label   . (after))))
    ; 设置分号和逗号的前后是否应换行
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-before-nonblanks
                                      c-semi&comma-inside-parenlist
                                      c-semi&comma-no-newlines-for-oneline-inliners))
    ; 清理whitespace，并作为上述hanging的补充（在其后生效）
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
;                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator
                       comment-close-slash))

    ;-----------------------------------------------------------------------
    ; Macro Alignment
    ; [Emacs Info] CC Mode: Custom Macros
    ;-----------------------------------------------------------------------
    (c-backslash-column       . 48)
    (c-backslash-max-column   . 72)
    (c-auto-align-backslashes . t)
    )
  ) ;end of my-c-style

(c-add-style "my-c-style" my-c-style) ;into c-style-alist
