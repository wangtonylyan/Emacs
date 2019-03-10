if [ "$USER" = "wm" ]; then
    export http_proxy="http://CHT1HTSH3191:Alps1912@10.25.71.1:8080"
    export https_proxy="https://CHT1HTSH3191:Alps1912@10.25.71.1:8080"
else
    export http_proxy=
    export https_proxy=
fi
export no_proxy="127.0.0.1, localhost"

CONDA_PATH="$HOME/miniconda3"
if [ -d "$CONDA_PATH" ]; then
    # export PATH="$CONDA_PATH/bin:$PATH"
    source "$CONDA_PATH/etc/profile.d/conda.sh"
fi

alias ll='ls -l'
alias la='ls -al'
alias cl='clear'

if command -v conda > /dev/null; then
    alias em='conda activate emacs && emacs'
else
    alias em='emacs'
fi

cdl() { cd "$@" && pwd ; ls -alF; }
alias .='cdl .'
alias ..='cdl ..'
