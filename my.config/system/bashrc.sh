echo "my bashrc.sh loading ..."

###############################################################################

alias ls='ls -l --color=tty'
alias la='ls -al'
alias cl='clear'
cdl() { cd "$@" && pwd ; ls -alF ; }
# alias .='cdl .'  # 有些脚本中会使用句号作为source命令
alias ..='cdl ..'

## Emacs
if command -v conda > /dev/null; then
    alias em='conda activate emacs && emacs'
else
    alias em='emacs'
fi

## Go
if command -v go > /dev/null; then
    go env -w GOPATH="$HOME/go"
    go env -w GO111MODULE="on"
    go env -w GOPROXY="https://goproxy.io,direct"
fi

###############################################################################

echo "my bashrc.sh loaded"
