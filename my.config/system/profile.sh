function Sudo () {
    local cmd="$@"

    sh -c "$cmd" > /dev/null 2>&1
    if [ "$?" -ne 0 ]; then
        ## 这里利用了账户名与密码相同的特点
        echo "$USER" | sudo -S -k sh -c "$cmd" > /dev/null 2>&1
    fi
}

function AddToPath () {
    if [ -d "$1" ]; then
        export PATH="$1:$PATH"
    fi
}
function LinkAlways () {
    if [ -d `basename "$1"` ] && [ -e "$2" ]; then
        Sudo ln -sf "$2" "$1"
    fi
}
function LinkExists () {
    if [ -e "$1" ] && [ -e "$2" ]; then
        Sudo ln -sf "$2" "$1"
    fi
}
function CopyAlways () {
    if [ -d `basename "$1"` ] && [ -e "$2" ]; then
        Sudo cp -f "$2" "$1"
    fi
}
function CopyExists () {
    if [ -e "$1" ] && [ -e "$2" ]; then
        Sudo cp -f "$2" "$1"
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

    # local sys_proxy_all='http://%s:34560/'
    local sys_proxy_all='socks5://%s:34561/'
    local sys_proxy_http='http://%s:34560/'

    if [ -z "$sys_proxy" ]; then  # domestic mirror only if no proxy
        local apt_source="tsinghua"
        # local apt_source="aliyun"
    fi
    ###########################################################################
    ###########################################################################

    ## 所有环境变量的设置，也可写入/etc/environment文件
    export LC_ALL=en_US.UTF-8    # fix WSL error
    export LC_CTYPE=zh_CN.UTF-8  # use Sougou Input in Emacs

    Proxy || return 1

    ## Emacs
    local dir=
    for dir in "project" "projects" "Project" "Projects"; do
        if [ -d "$HOME/$dir/Emacs" ]; then
            local projects_dir="$HOME/$dir"
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
    unset ALL_PROXY HTTP_PROXY HTTPS_PROXY NO_PROXY
    unset all_proxy http_proxy https_proxy no_proxy

    if [ -z "$sys_proxy" ]; then  # no proxy
        return 0
    fi
    if [ -z "$sys_proxy_all" ]; then
        echo "[WARN] error return from Proxy()!"
        return 1
    fi

    ## 常用的协议为http(s)和socks5，URL格式皆为<username>:<password>@<server>:<port>
    ## 需要同时设置所有大小写同名的变量，以使更多的软件生效，因为部分软件仅读取指定的变量
    ALL_PROXY=`printf "$sys_proxy_all" "$sys_proxy"`

    if [ -z "$sys_proxy_http" ]; then
        HTTP_PROXY="$ALL_PROXY"
    else
        HTTP_PROXY=`printf "$sys_proxy_http" "$sys_proxy"`
    fi
    HTTPS_PROXY="$HTTP_PROXY"

    local eth0=`ifconfig eth0 | grep "inet " | awk '{print $2}'`
    NO_PROXY="localhost, $eth0"

    all_proxy="$ALL_PROXY"
    http_proxy="$HTTP_PROXY"
    https_proxy="$HTTPS_PROXY"
    no_proxy="$NO_PROXY"

    export ALL_PROXY HTTP_PROXY HTTPS_PROXY NO_PROXY
    export all_proxy http_proxy https_proxy no_proxy

    ## 但无论如何，以下软件的代理，都必须由其配置文件来设置
    ## e.g. Apt

    ## 检测网络访问，还可以启用curl -v选项
    # curl https://ip.gs
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

        Sudo sed -i "/^\s*Acquire::http::Proxy/d"  "$apt_cfg"
        Sudo sed -i "/^\s*Acquire::https::Proxy/d" "$apt_cfg"

        if [ ! -z "$HTTP_PROXY" ] && [ ! -z "$HTTPS_PROXY" ]; then
            Sudo "echo \"Acquire::http::Proxy  \\\"$HTTP_PROXY\\\";\"  >> $apt_cfg"
            Sudo "echo \"Acquire::https::Proxy \\\"$HTTPS_PROXY\\\";\" >> $apt_cfg"
        fi
    fi

    if [ -d "$apt_dir" ] && [ -e "$source" ]; then
        local codename=`lsb_release --codename | cut -f2`
        Sudo "sed \"s/<codename>/${codename}/g\" $source > $apt_src"

        Sudo "echo '' >> $apt_src"
        Sudo "echo '# deb [arch=amd64] https://download.docker.com/linux/ubuntu/ $codename stable' >> $apt_src"
        Sudo "echo '# deb-src [arch=amd64] https://download.docker.com/linux/ubuntu/ $codename stable' >> $apt_src"

        Sudo "echo '' >> $apt_src"
        Sudo "echo '# deb https://ppa.launchpad.net/hvr/ghc/ubuntu/ $codename main' >> $apt_src"
        Sudo "echo '# deb-src https://ppa.launchpad.net/hvr/ghc/ubuntu/ $codename main' >> $apt_src"
        Sudo "echo '# sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 063DAB2BDC0B3F9FCEBC378BFF3AEACEF6F88286' >> $apt_src"
    fi

    if [ -z "$MY_CFG_PRIVATE_ENABLED" ] ; then  # if first time loading
        echo "$USER" | sudo -S -k apt update &&
        echo "$USER" | sudo -S -k apt -y dist-upgrade &&
        echo "$USER" | sudo -S -k apt -y autoremove && {
            local pkg=
            for pkg in "net-tools" "jq"; do
                dpkg -s "$pkg" > /dev/null 2>&1 ||
                echo "$USER" | sudo -S -k apt install -y "$pkg"
            done
        }
    fi
}

function Others () {
    if [ ! -d "$config_dir" ]; then
        echo "[WARN] error return from Others()!"
        return 1
    fi

    ## Docker
    # CopyExists "/etc/default/docker"       "$config_dir/system/docker.default"      # for init or upstart
    # "/etc/docker/daemon.json"   "$config_dir/system/docker.daemon.json"  # for systemd
    # "$HOME/.docker/config.json" "$config_dir/system/docker.config.json"
    ## 对于json文件建议使用jq命令，代替sed
    ## 根据源json文件中的键值对，加入或替换目标文件中的键值对
    # if [ ! -z "$HTTP_PROXY" ] && [ ! -z "$HTTPS_PROXY" ]; then; fi

    ## SSH
    # LinkAlways "/etc/ssh/ssh_config"  "$config_dir/system/ssh_config"
    # LinkAlways "/etc/ssh/sshd_config" "$config_dir/system/sshd_config"

    LinkExists "$HOME/.zshrc"                             "$config_dir/system/zshrc.sh"           # Zsh
    LinkExists "$HOME/.tmux.conf"                         "$config_dir/system/tmux.conf"          # Tmux
    LinkExists "$HOME/.gitconfig"                         "$config_dir/system/gitconfig"          # Git
    LinkExists "$HOME/.config/Code/User/settings.json"    "$config_dir/vscode/settings.json"      # VSCode
    LinkExists "$HOME/.config/Code/User/keybindings.json" "$config_dir/vscode/keybindings.json"
    LinkExists "$HOME/.config/pycodestyle"                "$config_dir/program/pycodestyle.cfg"   # Python
    AddToPath "$HOME/lib/go/bin"                                                                  # Go
    LinkExists "$HOME/.cabal/config"                      "$config_dir/program/cabal.config"      # Haskell
    LinkExists "$HOME/.stack/config.yaml"                 "$config_dir/program/stack-config.yaml"
    LinkExists "$HOME/.config/brittany/config.yaml"       "$config_dir/program/brittany.yaml"
    AddToPath "/opt/ghc/bin"
}

###############################################################################

Setup && echo "my profile.sh loaded" || echo "my profile.sh failed"

unset Sudo AddToPath LinkAlways LinkExists CopyAlways CopyExists Setup Proxy Apt Others
