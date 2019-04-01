Sudo () { echo "$USER" | sudo -S -k "$@" > /dev/null 2>&1 }
# unset -f Sudo


# export http_proxy="http://<username>:<password>@<server>:<port>"
# export https_proxy="https://<username>:<password>@<server>:<port>"
# export no_proxy="127.0.0.1, localhost"

# 解决Emacs中无法激活搜狗输入法的问题
export LC_CTYPE=zh_CN.UTF-8


MY_CFG_EMACS_DIR="$HOME/.emacs.d"
MY_CFG_DIR_ROOT="$MY_CFG_EMACS_DIR/my.config"
if [ ! 0 ]; then
    MY_CFG_BASHRC="$MY_CFG_EMACS_DIR/.private/bashrc.sh"
    MY_CFG_APT_CONF="$MY_CFG_EMACS_DIR/.private/apt.conf"
    MY_CFG_GIT_CONF="$MY_CFG_EMACS_DIR/.private/gitconfig"
else
    MY_CFG_BASHRC=""
    MY_CFG_APT_CONF="$MY_CFG_DIR_ROOT/system/apt.conf"
    MY_CFG_GIT_CONF="$MY_CFG_DIR_ROOT/system/gitconfig"
fi
# MY_CFG_APT_SOURCES="$MY_CFG_DIR_ROOT/system/apt.sources.list.china"
MY_CFG_APT_SOURCES="$MY_CFG_DIR_ROOT/system/apt.sources.list.tsinghua"
MY_CFG_DOCKER_DEFAULT="$MY_CFG_DIR_ROOT/system/docker.default"
MY_CFG_DOCKER_DAEMON="$MY_CFG_DIR_ROOT/system/docker.daemon.json"
MY_CFG_ZSHRC="$MY_CFG_DIR_ROOT/system/zshrc.sh"
MY_CFG_TMUX_CONF="$MY_CFG_DIR_ROOT/system/tmux.conf"


if [ -d "$HOME/Projects/Emacs" ] && [ ! -e "$MY_CFG_EMACS_DIR" ]; then
    ln -s "$HOME/Projects/Emacs" "$MY_CFG_EMACS_DIR"
fi

if [ -e "$MY_CFG_BASHRC" ]; then
    source "$MY_CFG_BASHRC"
fi

if [ -e "$MY_CFG_APT_CONF" ]; then
    Sudo rm -f '/etc/apt/apt.conf'
    Sudo ln -s "$MY_CFG_APT_CONF" '/etc/apt/apt.conf'
fi
if [ -e "$MY_CFG_APT_SOURCES" ]; then
    Sudo rm -f '/etc/apt/sources.list'
    Sudo ln -s "$MY_CFG_APT_SOURCES" '/etc/apt/sources.list'
fi
if [ -e "$MY_CFG_DOCKER_DEFAULT" ]; then
    Sudo rm -f '/etc/default/docker'
    Sudo ln -s "$MY_CFG_DOCKER_DEFAULT" '/etc/default/docker'
fi
if [ -e "$MY_CFG_DOCKER_DAEMON" ]; then
    Sudo rm -f '/etc/docker/daemon.json'
    Sudo ln -s "$MY_CFG_DOCKER_DEFAULT" '/etc/docker/daemon.json'
fi

if [ -e "$MY_CFG_GIT_CONF" ]; then
    rm -f "$HOME/.gitconfig"
    ln -s "$MY_CFG_GIT_CONF" "$HOME/.gitconfig"
fi
if [ -e "$MY_CFG_ZSHRC" ]; then
    rm -f "$HOME/.zshrc"
    ln -s "$MY_CFG_ZSHRC" "$HOME/.zshrc"
fi
if [ -e "$MY_CFG_TMUX_CONF" ]; then
    rm -f "$HOME/.tmux.conf"
    ln -s "$MY_CFG_TMUX_CONF" "$HOME/.tmux.conf"
fi


CONDA_PATH="$HOME/miniconda3"
if [ -d "$CONDA_PATH" ]; then
    # export PATH="$CONDA_PATH/bin:$PATH"
    source "$CONDA_PATH/etc/profile.d/conda.sh"
fi


alias ll='ls -l'
alias la='ls -al'
alias cl='clear'
cdl() { cd "$@" && pwd ; ls -alF; }
alias .='cdl .'
alias ..='cdl ..'

if command -v conda > /dev/null; then
    alias em='conda activate emacs && emacs'
else
    alias em='emacs'
fi
