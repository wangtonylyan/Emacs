## 这里利用了账户名与密码相同的特点
function Sudo () { echo "$USER" | sudo -S -k "$@" > /dev/null 2>&1 ; }

function AddToPath() {
    local path=$1

    if [ -d "$path" ]; then
        export PATH="$path":$PATH
    fi
}

function Link() {
    local link=$1
    local target=$2

    Sudo rm -f "$link"
    ln -s "$target" "$link" > /dev/null 2>&1
    if [ "$?" -ne "0" ]; then
        Sudo ln -s "$target" "$link"
    fi
}
function LinkAlways() {
    local dir=$1
    local link=$2
    local target=$3

    if [ -d "$dir" ] && [ -e "$target" ]; then
        Link "$link" "$target"
    fi
}
function LinkExists() {
    local dir=$1
    local link=$2
    local target=$3

    if [ -d "$dir" ] && [ -e "$link" ] && [ -e "$target" ]; then
        Link "$link" "$target"
    fi
}

###############################################################################

function Setup () {
    ###########################################################################
    ############################## configuration ##############################
    ###########################################################################
    # local sys_proxy='localhost'
    # local sys_proxy=`ip route | grep default | awk '{print $3}'`  # for WSL environment
    # local sys_proxy=`cat /etc/resolv.conf | grep nameserver | awk '{ print $2 }'`

    # local sys_proxy_all='http://%s:34560'
    local sys_proxy_all='socks5://%s:34561'
    local sys_proxy_http='http://%s:34560'

    local apt_source="tsinghua"
    # local apt_source="aliyun"
    ###########################################################################
    ###########################################################################

    ## 所有环境变量的设置，也可写入/etc/environment文件
    export LC_ALL=en_US.UTF-8    # fix WSL error
    export LC_CTYPE=zh_CN.UTF-8  # use Sougou Input in Emacs

    Proxy || return 1

    ## Emacs
    for d in "project" "projects" "Project" "Projects"; do
        if [ -d "$HOME/$d/Emacs" ]; then
            local projects_dir="$HOME/$d"
            break
        fi
    done
    if [ -z "$projects_dir" ]; then
        echo '[WARN] Please clone "https://github.com/wangtonylyan/Emacs.git" first!'
        return 1
    fi

    if [ ! -d "$HOME/.emacs.d" ]; then
        Link "$HOME/.emacs.d" "$projects_dir/Emacs"
    fi
    local config_dir="$HOME/.emacs.d/my.config"

    ## applications
    Apt || return 1
    Others || return 1
}

function Proxy () {
    unset ALL_PROXY all_proxy http_proxy https_proxy no_proxy

    if [ -z "$sys_proxy" ]; then  # no proxy
        return 0
    fi
    if [ -z "$sys_proxy_all" ]; then
        echo "[WARN] error return from Proxy()!"
        return 1
    fi

    ## 常用的协议为http(s)和socks5，URL格式皆为<username>:<password>@<server>:<port>
    ## 需要同时设置以下所有的相关变量，以使更多的软件生效，因为部分软件在实现上，仅读取其中的指定变量
    ALL_PROXY=`printf "$sys_proxy_all" "$sys_proxy"`
    all_proxy="$ALL_PROXY"

    if [ -z "$sys_proxy_http" ]; then
        http_proxy="$ALL_PROXY"
    else
        http_proxy=`printf "$sys_proxy_http" "$sys_proxy"`
    fi
    https_proxy="$http_proxy"

    local eth0=`ifconfig eth0 | grep "inet " | awk '{print $2}'`
    no_proxy="localhost, $eth0"

    export ALL_PROXY all_proxy http_proxy https_proxy no_proxy

    ## 但无论如何，以下软件的代理，都必须由其配置文件来设置
    ## e.g. Apt

    ## 检测网络访问
    # curl -v https://ip.gs
    # curl ipinfo.io
    # curl cip.cc
}

function Apt () {
    local apt_dir="/etc/apt"
    local apt_cfg="$apt_dir/apt.conf"
    local apt_src="$apt_dir/sources.list"

    if [ ! -d "$config_dir" ]; then
        echo "[WARN] error return from Apt()!"
        return 1
    fi
    if [ -z "$apt_source" ]; then
        local apt_source="default"
    fi

    local config="$config_dir/system/apt.conf"
    local source=`printf "$config_dir/system/apt.sources.%s" "$apt_source"`

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

        Sudo sh -c "echo '' >> $apt_src"
        Sudo sh -c "echo '# deb [arch=amd64] https://download.docker.com/linux/ubuntu/ $codename stable' >> $apt_src"
        Sudo sh -c "echo '# deb-src [arch=amd64] https://download.docker.com/linux/ubuntu/ $codename stable' >> $apt_src"

        Sudo sh -c "echo '' >> $apt_src"
        Sudo sh -c "echo '# deb https://ppa.launchpad.net/hvr/ghc/ubuntu/ $codename main' >> $apt_src"
        Sudo sh -c "echo '# deb-src https://ppa.launchpad.net/hvr/ghc/ubuntu/ $codename main' >> $apt_src"
        Sudo sh -c "echo '# sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 063DAB2BDC0B3F9FCEBC378BFF3AEACEF6F88286' >> $apt_src"
    fi
}

function Others () {
    if [ ! -d "$config_dir" ]; then
        echo "[WARN] error return from Others()!"
        return 1
    fi

    # LinkAlways "/etc/default"            "/etc/default/docker"                      "$config_dir/system/docker.default"     # Docker
    # LinkAlways "/etc/docker"             "/etc/docker/daemon.json"                  "$config_dir/system/docker.daemon.json"
    # LinkAlways "/etc/ssh"                "/etc/ssh/ssh_config"                      "$config_dir/system/ssh_config"         # SSH
    # LinkAlways "/etc/ssh"                "/etc/ssh/sshd_config"                     "$config_dir/system/sshd_config"

    LinkExists "$HOME"                   "$HOME/.zshrc"                             "$config_dir/system/zshrc.sh"           # Zsh
    LinkExists "$HOME"                   "$HOME/.tmux.conf"                         "$config_dir/system/tmux.conf"          # Tmux
    LinkExists "$HOME"                   "$HOME/.gitconfig"                         "$config_dir/system/gitconfig"          # Git
    LinkExists "$HOME/.config/Code/User" "$HOME/.config/Code/User/settings.json"    "$config_dir/vscode/settings.json"      # VSCode
    LinkExists "$HOME/.config/Code/User" "$HOME/.config/Code/User/keybindings.json" "$config_dir/vscode/keybindings.json"
    LinkExists "$HOME/.config"           "$HOME/.config/pycodestyle"                "$config_dir/program/pycodestyle.cfg"   # Python
    AddToPath "$HOME/Projects/go/bin"                                                                                       # Go
    LinkExists "$HOME/.cabal"            "$HOME/.cabal/config"                      "$config_dir/program/cabal.config"      # Haskell
    LinkExists "$HOME/.stack"            "$HOME/.stack/config.yaml"                 "$config_dir/program/stack-config.yaml"
    LinkExists "$HOME/.config/brittany"  "$HOME/.config/brittany/config.yaml"       "$config_dir/program/brittany.yaml"
    AddToPath "$HOME/.local/bin"
    AddToPath "/opt/ghc/bin"
}

###############################################################################

Setup || exit 1

unset Sudo AddToPath Link LinkAlways LinkExists Setup Proxy Apt Others

echo "my profile.sh loaded"
