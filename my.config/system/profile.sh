

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

## @param: all_protocol, all_port, http_protocol, http_port
Proxy () {
    local proxy='localhost'

    ## if in WSL environment
    local proxy=`ip route | grep default | awk '{print $3}'`
    # local proxy=`cat /etc/resolv.conf | grep nameserver | awk '{ print $2 }'`

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
    ## e.g. Apt

    ## 检测网络访问
    # curl -v https://ip.gs
    # curl ipinfo.io
    # curl cip.cc
}

## @return: projects_dir, config_dir
Setup() {
    ## 所有环境变量的设置，也可写入/etc/environment文件
    export LC_ALL=en_US.UTF-8    # fix WSL error
    export LC_CTYPE=zh_CN.UTF-8  # use Sougou Input in Emacs

    unset ALL_PROXY all_proxy http_proxy https_proxy no_proxy
    # Proxy 'socks5' '34561' 'http' '34560'

    ## Emacs
    local dirs="project projects Project Projects"
    for d in "$dirs"; do
        if [ -d "$HOME/$d/Emacs" ]; then
            projects_dir="$HOME/$d"
            break
        fi
    done
    if [ -z "$projects_dir" ]; then
        echo 'Please clone "https://github.com/wangtonylyan/Emacs.git" first.'
        exit 1
    fi

    if [ ! -e "$HOME/.emacs.d" ]; then
        Link "$HOME/.emacs.d" "$projects_dir/Emacs"
    fi
    config_dir="$HOME/.emacs.d/my.config"

    ## Apt
    local apt_dir="/etc/apt"
    local apt_cfg="$apt_dir/apt.conf"
    local apt_src="$apt_dir/sources.list"

    local config="$config_dir/system/apt.conf"
    local source="$config_dir/system/apt.sources.aliyun"
    # local source="$config_dir/system/apt.sources.china"
    # local source="$config_dir/system/apt.sources.tsinghua"

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
Setup


## Docker, SSH
# LinkAlways "/etc/default" "/etc/default/docker"     "$config_dir/system/docker.default"
# LinkAlways "/etc/docker"  "/etc/docker/daemon.json" "$config_dir/system/docker.daemon.json"
# LinkAlways "/etc/ssh"     "/etc/ssh/ssh_config"     "$config_dir/system/ssh_config"
# LinkAlways "/etc/ssh"     "/etc/ssh/sshd_config"    "$config_dir/system/sshd_config"

## Git, Zsh, Tmux, VSCode
LinkExists "$HOME"                   "$HOME/.gitconfig"                         "$config_dir/system/gitconfig"
LinkExists "$HOME"                   "$HOME/.zshrc"                             "$config_dir/system/zshrc.sh"
LinkExists "$HOME"                   "$HOME/.tmux.conf"                         "$config_dir/system/tmux.conf"
LinkExists "$HOME/.config/Code/User" "$HOME/.config/Code/User/settings.json"    "$config_dir/vscode/settings.json"
LinkExists "$HOME/.config/Code/User" "$HOME/.config/Code/User/keybindings.json" "$config_dir/vscode/keybindings.json"

## Python
LinkExists "$HOME/.config"           "$HOME/.config/pycodestyle"                "$config_dir/program/pycodestyle.cfg"

## Go
AddToPath "$HOME/Projects/go/bin"

## Haskell
AddToPath "$HOME/.local/bin"
AddToPath "/opt/ghc/bin"
LinkExists "$HOME/.cabal"            "$HOME/.cabal/config"                      "$config_dir/program/cabal.config"
LinkExists "$HOME/.stack"            "$HOME/.stack/config.yaml"                 "$config_dir/program/stack-config.yaml"
LinkExists "$HOME/.config/brittany"  "$HOME/.config/brittany/config.yaml"       "$config_dir/program/brittany.yaml"


###############################################################################

echo "my profile.sh loaded"
