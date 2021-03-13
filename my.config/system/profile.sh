export LC_ALL=en_US.UTF-8   # fix WSL error
export LC_CTYPE=zh_CN.UTF-8 # use Sougou Input in Emacs


###############################################################################

## 这里利用了账户名与密码相同的特点
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

## Python
LinkExists "$HOME/.config"           "$HOME/.config/pycodestyle"                "$MY_CFG_DIR_ROOT/program/pycodestyle.cfg"

## Go
AddToPath "$HOME/Projects/go/bin"

## Haskell
AddToPath "$HOME/.local/bin"
AddToPath "/opt/ghc/bin"
LinkExists "$HOME/.config/brittany"  "$HOME/.config/brittany/config.yaml"       "$MY_CFG_DIR_ROOT/program/brittany.yaml"


###############################################################################

## ("$1", "$2")       => all="$1://$2" && apt="http://$2"
## ("$1", "$2", "$3") => all="$1://$2" && apt="http://$3"
SetProxy () {
    ## 据说$ALL_PROXY可用于应对一些不由$all_proxy控制的软件
    ## 例如，git、npm、yarn等，因此建议同时设置两者
    ## 此外，无论如何，apt的代理都需由apt.conf文件来控制
    export ALL_PROXY="$1://$2"
    export all_proxy="$1://$2"

    if [ -e "/etc/apt/apt.conf" ]; then
        if [ ! -z "$3" ]; then apt=$3; else apt=$2; fi
        Sudo sh -c "echo \"Acquire::http::Proxy  \\\"http://$apt\\\";\" >> /etc/apt/apt.conf"
        Sudo sh -c "echo \"Acquire::https::Proxy \\\"http://$apt\\\";\" >> /etc/apt/apt.conf"
    fi

    curl https://ip.gs  # 检测代理设置
}

UnsetProxy () {
    ## 以下这些变量的设置，也可写入/etc/environment文件
    ## 常用的协议为http(s)和sock5，URL格式则为<username>:<password>@<server>:<port>
    unset ALL_PROXY    # "sock5://127.0.0.1:34561"
    unset all_proxy    # "sock5://127.0.0.1:34561"
    unset http_proxy   # "http://127.0.0.1:34560"
    unset https_proxy  # "http://127.0.0.1:34560"
    unset no_proxy     # "127.0.0.1, localhost"

    if [ -e "/etc/apt/apt.conf" ]; then
        ## 删除所有以指定字符串开始的行
        Sudo sed -i --follow-symlinks -e "/^\s*Acquire::http::Proxy/d"  "/etc/apt/apt.conf"
        Sudo sed -i --follow-symlinks -e "/^\s*Acquire::https::Proxy/d" "/etc/apt/apt.conf"
    fi
}

UnsetProxy
# SetProxy "http" "localhost:34560"


###############################################################################

unset -f Sudo Link LinkAlways LinkExists AddToPath SetProxy UnsetProxy

echo "my profile.sh loaded"
