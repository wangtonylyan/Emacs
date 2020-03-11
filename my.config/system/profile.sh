unset all_proxy
unset http_proxy
unset https_proxy
unset no_proxy

# export http_proxy=  "http://<username>:<password>@<server>:<port>"
# export https_proxy= "https://<username>:<password>@<server>:<port>"
# export no_proxy=    "127.0.0.1, localhost"

## v2rayN proxy
# export all_proxy="socks5://127.0.0.1:10808"
# export http_proxy="http://127.0.0.1:10809"
# export https_proxy="http://127.0.0.1:10809"


export LC_ALL=en_US.UTF-8   # WSL error
export LC_CTYPE=zh_CN.UTF-8 # use Sougou Input in Emacs


###############################################################################

Sudo () { echo "$USER" | sudo -S -k "$@" > /dev/null 2>&1 ; }

Link() {
    link=$1
    target=$2
    Sudo rm -f "$link"
    ln -s "$target" "$link" > /dev/null 2>&1
    if [ "$?" -ne "0" ]; then
        Sudo ln -s "$target" "$link"
    fi
}

LinkAlways() {
    dir=$1
    link=$2
    target=$3
    if [ -d "$dir" ] && [ -e "$target" ]; then
        Link "$link" "$target"
    fi
}

LinkExists() {
    dir=$1
    link=$2
    target=$3
    if [ -d "$dir" ] && [ -e "$link" ] && [ -e "$target" ]; then
        Link "$link" "$target"
    fi
}

AddToPath() {
    path=$1
    if [ -d "$path" ]; then
        export PATH="$path":$PATH
    fi
}


###############################################################################

## Emacs
MY_CFG_EMACS_DIR="$HOME/.emacs.d"
MY_CFG_DIR_ROOT="$MY_CFG_EMACS_DIR/my.config"
if [ -d "$HOME/Projects/Emacs" ] && [ ! -e "$MY_CFG_EMACS_DIR" ]; then
    Link "$MY_CFG_EMACS_DIR" "$HOME/Projects/Emacs"
fi

## Bash
if $MY_CFG_PRIVATE_ENABLED; then
    MY_CFG_BASHRC="$MY_CFG_EMACS_DIR/.private/bashrc.sh"
    if [ -e "$MY_CFG_BASHRC" ]; then
        source "$MY_CFG_BASHRC"
    fi
fi

## Apt, Docker, SSH
if $MY_CFG_PRIVATE_ENABLED; then
    MY_CFG_APT_CONF="$MY_CFG_EMACS_DIR/.private/apt.conf"
else
    MY_CFG_APT_CONF="$MY_CFG_DIR_ROOT/system/apt.conf"
fi
# MY_CFG_APT_SOURCES="$MY_CFG_DIR_ROOT/system/apt.sources.list.china"
# MY_CFG_APT_SOURCES="$MY_CFG_DIR_ROOT/system/apt.sources.list.tsinghua"
MY_CFG_APT_SOURCES="$MY_CFG_DIR_ROOT/system/apt.sources.list.aliyun"
LinkAlways "/etc/apt"     "/etc/apt/apt.conf"       "$MY_CFG_APT_CONF"
LinkAlways "/etc/apt"     "/etc/apt/sources.list"   "$MY_CFG_APT_SOURCES"
LinkAlways "/etc/default" "/etc/default/docker"     "$MY_CFG_DIR_ROOT/system/docker.default"
LinkAlways "/etc/docker"  "/etc/docker/daemon.json" "$MY_CFG_DIR_ROOT/system/docker.daemon.json"
LinkAlways "/etc/ssh"     "/etc/ssh/ssh_config"     "$MY_CFG_DIR_ROOT/system/ssh_config"
LinkAlways "/etc/ssh"     "/etc/ssh/sshd_config"    "$MY_CFG_DIR_ROOT/system/sshd_config"

# Git, Zsh, Tmux, VSCode
if $MY_CFG_PRIVATE_ENABLED; then
    MY_CFG_GIT_CONF="$MY_CFG_EMACS_DIR/.private/gitconfig"
else
    MY_CFG_GIT_CONF="$MY_CFG_DIR_ROOT/system/gitconfig"
fi
LinkExists "$HOME"                   "$HOME/.gitconfig"                         "$MY_CFG_GIT_CONF"
LinkExists "$HOME"                   "$HOME/.zshrc"                             "$MY_CFG_DIR_ROOT/system/zshrc.sh"
LinkExists "$HOME"                   "$HOME/.tmux.conf"                         "$MY_CFG_DIR_ROOT/system/tmux.conf"
LinkExists "$HOME/.config/Code/User" "$HOME/.config/Code/User/settings.json"    "$MY_CFG_DIR_ROOT/vscode/settings.json"
LinkExists "$HOME/.config/Code/User" "$HOME/.config/Code/User/keybindings.json" "$MY_CFG_DIR_ROOT/vscode/keybindings.json"

## Node.js
MY_CFG_NODEJS_VERSION="v12.13.0"
MY_CFG_NODEJS_DISTRO="linux-x64"
MY_CFG_NODEJS_BIN_DIR="/usr/local/lib/nodejs/node-$MY_CFG_NODEJS_VERSION-$MY_CFG_NODEJS_DISTRO/bin"
# AddToPath "$MY_CFG_NODEJS_BIN_DIR"
LinkAlways "$MY_CFG_NODEJS_BIN_DIR" "/usr/local/bin/node" "$MY_CFG_NODEJS_BIN_DIR/node"
LinkAlways "$MY_CFG_NODEJS_BIN_DIR" "/usr/local/bin/npm"  "$MY_CFG_NODEJS_BIN_DIR/npm"
LinkAlways "$MY_CFG_NODEJS_BIN_DIR" "/usr/local/bin/npx"  "$MY_CFG_NODEJS_BIN_DIR/npx"

## Haskell
AddToPath "$HOME/.local/bin"
AddToPath "/opt/ghc/bin"
LinkExists "$HOME/.config/brittany" "$HOME/.config/brittany/config.yaml" "$MY_CFG_DIR_ROOT/program/brittany.yaml"


###############################################################################

unset -f Sudo Link LinkAlways LinkExists AddToPath
