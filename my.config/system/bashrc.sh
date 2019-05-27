alias ls='ls -l --color=tty'
alias la='ls -al'
alias cl='clear'
cdl() { cd "$@" && pwd ; ls -alF ; }
alias .='cdl .'
alias ..='cdl ..'

if command -v conda > /dev/null; then
    alias em='conda activate emacs && emacs'
else
    alias em='emacs'
fi
