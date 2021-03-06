## 所有环境变量的设置，也可写入/etc/environment文件
export LC_ALL=en_US.UTF-8    # fix WSL error
export LC_CTYPE=zh_CN.UTF-8  # use Sougou Input in Emacs

## @param: all_protocol, all_port, http_protocol, http_port
Proxy () {
    local proxy='localhost'

    ## if in WSL environment
    local proxy=`ip route | grep default | awk '{print $3}'`
    # proxy=`cat /etc/resolv.conf | grep nameserver | awk '{ print $2 }'`

    ## 常用的协议为http(s)和socks5，URL格式皆为<username>:<password>@<server>:<port>
    ## 需要同时设置以下所有的相关变量，以使更多的软件生效，因为部分软件在实现上，仅读取其中的指定变量
    ALL_PROXY="$1://$proxy:$2"  # e.g. "socks5://127.0.0.1:34561"
    all_proxy="$ALL_PROXY"      # e.g. "http://127.0.0.1:34560"
    http_proxy="$ALL_PROXY"
    https_proxy="$ALL_PROXY"
    no_proxy="localhost"
    if [ ! -z "$3" ] && [ ! -z "$4" ]; then
        http_proxy="$3://$proxy:$4"
        https_proxy="$http_proxy"
    fi
    export ALL_PROXY all_proxy http_proxy https_proxy no_proxy

    ## 但无论如何，以下软件的代理，都必须由其配置文件来设置
    ## e.g. apt

    ## 检测网络访问
    # curl -v https://ip.gs
    # curl ipinfo.io
    # curl cip.cc
}
unset ALL_PROXY all_proxy http_proxy https_proxy no_proxy
# Proxy 'socks5' '34561' 'http' '34560'


###############################################################################

## 这里利用了账户名与密码相同的特点
Sudo () { echo "$USER" | sudo -S -k "$@" > /dev/null 2>&1 ; }

Link() {
    local link=$1
    local target=$2

    Sudo rm -f "$link"
    ln -s "$target" "$link" > /dev/null 2>&1
    if [ "$?" -ne "0" ]; then
        Sudo ln -s "$target" "$link"
    fi
}

LinkAlways() {
    local dir=$1
    local link=$2
    local target=$3

    if [ -d "$dir" ] && [ -e "$target" ]; then
        Link "$link" "$target"
    fi
}

LinkExists() {
    local dir=$1
    local link=$2
    local target=$3

    if [ -d "$dir" ] && [ -e "$link" ] && [ -e "$target" ]; then
        Link "$link" "$target"
    fi
}

AddToPath() {
    local path=$1

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

## Apt
AptCopyAlways() {
    local apt_dir="/etc/apt"
    local apt_cfg="$apt_dir/apt.conf"
    local apt_src="$apt_dir/sources.list"

    if $MY_CFG_PRIVATE_ENABLED; then
        local config="$MY_CFG_EMACS_DIR/.private/apt.conf"
    else
        local config="$MY_CFG_DIR_ROOT/system/apt.conf"
    fi
    local source="$MY_CFG_DIR_ROOT/system/apt.sources.aliyun"
    # local source="$MY_CFG_DIR_ROOT/system/apt.sources.china"
    # local source="$MY_CFG_DIR_ROOT/system/apt.sources.tsinghua"

    if [ -d "$apt_dir" ] && [ -e "$config" ]; then
        Sudo rm -f "$apt_cfg"
        Sudo cp -f "$config" "$apt_cfg"  # copy instead of link

        Sudo sed -i -e "/^\s*Acquire::http::Proxy/d"  "$apt_cfg"
        Sudo sed -i -e "/^\s*Acquire::https::Proxy/d" "$apt_cfg"
        if [ ! -z "$http_proxy" ] && [ ! -z "$https_proxy" ]; then
            Sudo sh -c "echo \"Acquire::http::Proxy  \\\"$http_proxy\\\";\"  >> $apt_cfg"
            Sudo sh -c "echo \"Acquire::https::Proxy \\\"$https_proxy\\\";\" >> $apt_cfg"
        fi
    fi
    if [ -d "$apt_dir" ] && [ -e "$source" ]; then
        local codename=`lsb_release --codename | cut -f2`
        Sudo sh -c "sed \"s/<codename>/${codename}/g\" $source > $apt_src"
    fi
}
AptCopyAlways

## Docker, SSH
# LinkAlways "/etc/default" "/etc/default/docker"     "$MY_CFG_DIR_ROOT/system/docker.default"
# LinkAlways "/etc/docker"  "/etc/docker/daemon.json" "$MY_CFG_DIR_ROOT/system/docker.daemon.json"
# LinkAlways "/etc/ssh"     "/etc/ssh/ssh_config"     "$MY_CFG_DIR_ROOT/system/ssh_config"
# LinkAlways "/etc/ssh"     "/etc/ssh/sshd_config"    "$MY_CFG_DIR_ROOT/system/sshd_config"

## Git, Zsh, Tmux, VSCode
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
LinkExists "$HOME/.cabal"            "$HOME/.cabal/config"                      "$MY_CFG_DIR_ROOT/program/cabal.config"
LinkExists "$HOME/.stack"            "$HOME/.stack/config.yaml"                 "$MY_CFG_DIR_ROOT/program/stack-config.yaml"
LinkExists "$HOME/.config/brittany"  "$HOME/.config/brittany/config.yaml"       "$MY_CFG_DIR_ROOT/program/brittany.yaml"


###############################################################################

unset -f Sudo Proxy Link LinkAlways LinkExists AddToPath AptCopyAlways

echo "my profile.sh loaded"
