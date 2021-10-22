function Sudo () {
    local cmd="$@"

    sh -c "$cmd" > /dev/null 2>&1 ||
    echo "$USER" | sudo -S -k sh -c "$cmd"  # 利用账户名与密码相同的特点
}
function SudoQ () {  # quietly
    Sudo "$@" > /dev/null 2>&1
}

function AddToPath () {
    if [ -d "$1" ]; then
        export PATH="$1:$PATH"
    fi
}
function LinkAlways () {
    if [ -d `dirname "$1"` ] && [ -e "$2" ]; then
        SudoQ ln -sf "$2" "$1"
    fi
}
function LinkExists () {
    if [ -e "$1" ] && [ -e "$2" ]; then
        SudoQ ln -sf "$2" "$1"
    fi
}
function CopyAlways () {
    if [ -d `dirname "$1"` ] && [ -e "$2" ]; then
        SudoQ cp -f "$2" "$1"
    fi
}
function CopyExists () {
    if [ -e "$1" ] && [ -e "$2" ]; then
        SudoQ cp -f "$2" "$1"
    fi
}
function CopyExistsWindows() {
    if [ -e "$1" ] && [ -e "$2" ]; then
        # cmd.exe /c copy `wslpath -m "$2"` `wslpath -m "$1"`  # CMD does not support UNC paths
        powershell.exe -command Copy-Item `wslpath -m "$2"` -Destination `wslpath -m "$1"`
    fi
}

function ErrorReturn () {
    echo "[WARN] error return from $1 !!!"
    [ -n "$2" ] && echo "$2"
}

###############################################################################

function Setup () {
    ###########################################################################
    ############################## configuration ##############################
    ###########################################################################
    if [ -z "$MY_CFG_PRIVATE_ENABLED" ]; then  # which should be declared in .profile
        local init_load=true
        echo "this is an initial loading ..."
    else
        local init_load=false
    fi

    if [ -n "$IS_WSL" ] || [ -n "$WSL_DISTRO_NAME" ]; then
        local env_wsl=true
    else
        local env_wsl=false
    fi

    if [ "$env_wsl" = true ]; then
        ## for WSL1
        # local sys_proxy=`ifconfig eth0 | grep "inet " | awk '{ print $2 }'`
        # local sys_proxy=`ifconfig wifi0 | grep "inet " | awk '{ print $2 }'`

        ## for WSL2
        # local sys_proxy=`ip route | grep default | awk '{print $3}'`
        # local sys_proxy=`cat /etc/resolv.conf | grep nameserver | awk '{ print $2 }'`
        :
    else
        # local sys_proxy='localhost'
        :
    fi

    if [ -n "$sys_proxy" ]; then
        local sys_proxy_all='http://%s:34560/'
        # local sys_proxy_all='socks5://%s:34561/'
        # local sys_proxy_http='http://%s:34560/'
    else
        local apt_source="tsinghua"
        # local apt_source="aliyun"
        :
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
        ErrorReturn "Setup[0]" "Please clone https://github.com/wangtonylyan/Emacs.git first."
        return 1
    fi

    if [ ! -d "$HOME/.emacs.d" ]; then
        LinkAlways "$HOME/.emacs.d" "$projects_dir/Emacs"
    fi

    local config_dir="$HOME/.emacs.d/my.config"

    ## Applications
    # AddToPath "$HOME/.local/bin"  # already added by .profile
    Apt || return 1
    Docker || return 1
    Others || return 1
}

function Proxy () {
    unset ALL_PROXY HTTP_PROXY HTTPS_PROXY NO_PROXY
    unset all_proxy http_proxy https_proxy no_proxy

    if [ -z "$sys_proxy" ]; then
        return 0
    fi
    if [ -z "$sys_proxy_all" ]; then
        ErrorReturn "Proxy[0]"
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
    NO_PROXY="localhost"

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
    local codename=`lsb_release -cs`

    if [ ! -d "$config_dir" ]; then
        ErrorReturn "Apt[0]"
        return 1
    fi
    if [ -z "$apt_source" ]; then
        local apt_source="default"
    fi

    local config="$config_dir/system/apt.conf"
    local source=`printf "$config_dir/system/apt.sources.%s" "$apt_source"`

    if [ -d "$apt_dir" ] && [ -e "$config" ]; then
        SudoQ rm -f "$apt_cfg"
        SudoQ cp -f "$config" "$apt_cfg"

        SudoQ sed -i "/^\s*Acquire::http::Proxy/d"  "$apt_cfg"
        SudoQ sed -i "/^\s*Acquire::https::Proxy/d" "$apt_cfg"

        if [ -n "$HTTP_PROXY" ] && [ -n "$HTTPS_PROXY" ]; then
            SudoQ "echo \"Acquire::http::Proxy  \\\"$HTTP_PROXY\\\";\"  >> $apt_cfg"
            SudoQ "echo \"Acquire::https::Proxy \\\"$HTTPS_PROXY\\\";\" >> $apt_cfg"
        fi
    fi

    if [ -d "$apt_dir" ] && [ -e "$source" ]; then
        SudoQ "sed \"s/<codename>/${codename}/g\" $source > $apt_src"
    fi

    if [ "$init_load" = true ]; then
        Sudo apt update && Sudo apt -y dist-upgrade && Sudo apt -y autoremove && {
            local pkg=
            for pkg in "net-tools" "build-essential" "jq"; do
                dpkg -s "$pkg" > /dev/null 2>&1 || Sudo apt install -y "$pkg"
            done
        }
    fi
}

function Docker () {
    local apt_dir="/etc/apt"
    local apt_cfg="$apt_dir/apt.conf"
    local apt_src="$apt_dir/sources.list"
    local codename=`lsb_release -cs`

    if [ "$env_wsl" = true ]; then
        return 0
    fi
    if [ ! -e "$apt_cfg" ] || [ ! -e "$apt_src" ]; then
        ErrorReturn "Docker[0]"
        return 1
    fi

    ## https://docs.docker.com/engine/install/ubuntu/
    if [ "$init_load" = true ]; then
        Sudo "curl -vfsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -" || {
            ErrorReturn "Docker[1]"
            return 1
        }
    fi

    SudoQ "echo '' >> $apt_src"
    SudoQ "echo '## Docker' >> $apt_src"
    if grep -q -e "^\s*Acquire::http::Proxy" -e "^\s*Acquire::https::Proxy" "$apt_cfg"; then
        # Sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $codename stable"
        SudoQ "echo 'deb [arch=amd64] https://download.docker.com/linux/ubuntu $codename stable' >> $apt_src"
        SudoQ "echo 'deb-src [arch=amd64] https://download.docker.com/linux/ubuntu $codename stable' >> $apt_src"
    else
        ## https://mirrors.tuna.tsinghua.edu.cn/help/docker-ce/
        # Sudo add-apt-repository "deb [arch=amd64] https://mirrors.tuna.tsinghua.edu.cn/docker-ce/linux/ubuntu $codename stable"
        SudoQ "echo 'deb [arch=amd64] https://mirrors.tuna.tsinghua.edu.cn/docker-ce/linux/ubuntu $codename stable' >> $apt_src"
        SudoQ "echo 'deb-src [arch=amd64] https://mirrors.tuna.tsinghua.edu.cn/docker-ce/linux/ubuntu $codename stable' >> $apt_src"
    fi

    if [ "$init_load" = true ]; then
        Sudo apt update &&
        Sudo apt install -y docker-ce docker-ce-cli containerd.io || {
            ErrorReturn "Docker[2]"
            return 1
        }
    fi
}

function Others () {
    if [ ! -d "$config_dir" ]; then
        ErrorReturn "Others[0]"
        return 1
    fi

    ## Docker Daemon
    # CopyExists "/etc/default/docker"       "$config_dir/system/docker.default"      # for init or upstart
    # CopyExists "/etc/docker/daemon.json"   "$config_dir/system/docker.daemon.json"  # for systemd
    ## Docker Container
    # "$HOME/.docker/config.json" "$config_dir/system/docker.config.json"

    ## SSH
    # LinkAlways "/etc/ssh/ssh_config"  "$config_dir/system/ssh_config"
    # LinkAlways "/etc/ssh/sshd_config" "$config_dir/system/sshd_config"

    ## Zsh
    LinkExists "$HOME/.zshrc"                             "$config_dir/system/zshrc.sh"
    ## Tmux
    LinkExists "$HOME/.tmux.conf"                         "$config_dir/system/tmux.conf"

    if [ "$env_wsl" = true ]; then
        ## 注意，每次在GUI中修改此类设置后，写入的都是Windows中的settings.json
        ## 因此，还需手动同步至GitHub项目中的对应文件

        local homepath="`wslupath --home`"         # %HOMEPATH%
        local appdata="$homepath/AppData/Roaming"  # %APPDATA%

        ## Git
        CopyExistsWindows "$homepath/.gitconfig"          "$config_dir/system/gitconfig"

        ## VSCode
        local vscode_dir="$appdata/Code/User"
        CopyExistsWindows "$vscode_dir/settings.json"     "$config_dir/vscode/settings.json"
        CopyExistsWindows "$vscode_dir/keybindings.json"  "$config_dir/vscode/keybindings.json"
    else
        LinkExists "$HOME/.gitconfig"                     "$config_dir/system/gitconfig"

        local vscode_dir="$HOME/.config/Code/User"
        CopyExists "$vscode_dir/settings.json"            "$config_dir/vscode/settings.json"
        CopyExists "$vscode_dir/keybindings.json"         "$config_dir/vscode/keybindings.json"
    fi

    ## Go
    AddToPath  "$HOME/lib/go/bin"
    ## Rust
    AddToPath  "$HOME/.cargo/bin"
    ## Python
    LinkExists "$HOME/.config/pycodestyle"                "$config_dir/program/pycodestyle.cfg"
    ## Node.js
    AddToPath  "$HOME/lib/nodejs/node-v14.17.5-linux-x64/bin"
    ## Haskell
    LinkExists "$HOME/.cabal/config"                      "$config_dir/program/cabal.config"
    LinkExists "$HOME/.stack/config.yaml"                 "$config_dir/program/stack-config.yaml"
    LinkExists "$HOME/.config/brittany/config.yaml"       "$config_dir/program/brittany.yaml"
    AddToPath  "/opt/ghc/bin"
}

###############################################################################

echo "my profile.sh loading ..."
Setup && echo "my profile.sh loaded" && export | grep "proxy" || echo "my profile.sh failed"

unset Sudo SudoQ AddToPath LinkAlways LinkExists CopyAlways CopyExists ErrorReturn
unset Setup Proxy Apt Docker Others
