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
    ; ����ʶ���ע����
    (c-comment-prefix-regexp . ((c-mode    . "//+\\|\\**")
                                (c++-mode  . "//+\\|\\**")
                                (java-mode . "//+\\|\\**")))
    ; ����block comment�ڻ���ʱ�Զ���ӵ�ǰ׺
    (c-block-comment-prefix . "*")
    ; ��������Щ�ط�����auto-fill���ܣ�Ŀǰ����ע��������
    (c-ignore-auto-fill . (string cpp code))

    ;-----------------------------------------------------------------------
    ; Auto Newlines
    ; [Emacs Info] CC Mode: Custom Auto-newlines, Clean-ups
    ;-----------------------------------------------------------------------
    ; ���ô����ŵ�ǰ���Ƿ�Ӧ���У���"{"��λ����ͷ����β���л�����λ��
    ; ��������alist���г���syntactic symbol����ִ��Ĭ����Ϊ��(before after)
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
    ; ����ð�ŵ�ǰ���Ƿ�Ӧ����
    ; ��������alist���г���syntactic symbol����ִ��Ĭ����Ϊ��()
    (c-hanging-colons-alist . ((case-label)
                               (label          . (after))
                               (access-label   . (after))))
    ; ���÷ֺźͶ��ŵ�ǰ���Ƿ�Ӧ����
    (c-hanging-semi&comma-criteria . (c-semi&comma-no-newlines-before-nonblanks
                                      c-semi&comma-inside-parenlist
                                      c-semi&comma-no-newlines-for-oneline-inliners))
    ; ����whitespace������Ϊ����hanging�Ĳ��䣨�������Ч��
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
