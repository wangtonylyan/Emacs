#!/usr/bin/bash

function Sudo() {
    local -r cmd="$*"

    bash -c "$cmd" >/dev/null 2>&1 ||
        echo "$USER" | sudo -S -k bash -c "$cmd" # 利用账户名与密码相同的特点
}
function SudoQ() { # quietly
    Sudo "$*" >/dev/null 2>&1
}

function AddToPath() {
    if [ -d "$1" ]; then
        export PATH="$1:$PATH"
    fi
}
function LinkAlways() {
    if [ -d "$(dirname "$1")" ] && [ -e "$2" ]; then
        SudoQ ln -sf "$2" "$1"
    fi
}
function LinkExists() {
    if [ -e "$1" ] && [ -e "$2" ]; then
        SudoQ ln -sf "$2" "$1"
    fi
}

function CopyAlways() {
    if [ -d "$(dirname "$1")" ] && [ -e "$2" ]; then
        SudoQ cp -f "$2" "$1"
    fi
}
function CopyExists() {
    if [ -e "$1" ] && [ -e "$2" ] && [ "$1" -ot "$2" ]; then
        SudoQ cp -f "$2" "$1"
    fi
}
function CopyAlwaysWindows() {
    if [ -d "$(dirname "$1")" ] && [ -e "$2" ]; then
        # cmd.exe /c copy "$(wslpath -m "$2")" "$(wslpath -m "$1")" # CMD does not support UNC paths
        powershell.exe -command Copy-Item "$(wslpath -m "$2")" -Destination "$(wslpath -m "$1")"
        echo "copy \"$(basename "$2")\" to \"$1\""
    fi
}
function CopyExistsWindows() {
    if [ -e "$1" ] && [ -e "$2" ] && [ "$1" -ot "$2" ]; then
        powershell.exe -command Copy-Item "$(wslpath -m "$2")" -Destination "$(wslpath -m "$1")"
        echo "copy \"$(basename "$2")\" to \"$1\""
    fi
}

function ErrorReturn() {
    echo "[WARN] error return from $1 !!!"
    [ -n "$2" ] && echo "$2"
}
function CheckGlobalVar() {
    if [ -z "$g_INIT_LOAD" ]; then
        ErrorReturn "CheckGlobalVar[g_INIT_LOAD]"
        return 1
    fi
    if [ -z "$g_WSL_ENV" ]; then
        ErrorReturn "CheckGlobalVar[g_WSL_ENV]"
        return 1
    fi
    if [ ! -d "$g_CFG_HOME" ]; then
        ErrorReturn "CheckGlobalVar[g_CFG_HOME]"
        return 1
    fi
}

###############################################################################

function Setup() {
    if [ -z "$MY_CFG_PRIVATE_ENABLED" ]; then # which should be declared in .profile
        local -r g_INIT_LOAD=true
        echo "this is an initial loading ..."
    else
        local -r g_INIT_LOAD=false
    fi
    if [ -n "$IS_WSL" ] || [ -n "$WSL_DISTRO_NAME" ]; then
        local -r g_WSL_ENV=true
    else
        local -r g_WSL_ENV=false
    fi

    if [ "$g_WSL_ENV" = true ]; then
        ## for WSL1
        # local -r g_PROXY_SRV=$(ifconfig eth0 | grep "inet " | awk '{ print $2 }')
        # local -r g_PROXY_SRV=$(ifconfig wifi0 | grep "inet " | awk '{ print $2 }')

        ## for WSL2
        # local -r g_PROXY_SRV=$(ip route | grep default | awk '{print $3}')
        # local -r g_PROXY_SRV=$(grep nameserver </etc/resolv.conf | awk '{ print $2 }')
        # local -r g_PROXY_SRV=$(hostname)       # == hostname.domainname == 127.0.1.1
        # local -r g_PROXY_SRV=$(hostname).local # == 192.168.1.x

        # local -r g_USR_PROFILE=$(wslupath --home) # deprecated
        local -r g_USR_PROFILE=$(wslpath "$(wslvar USERPROFILE)") # %HOMEPATH%
        local -r g_APP_DATA="$g_USR_PROFILE/AppData/Roaming"      # %APPDATA%
    else
        # local -r g_PROXY_SRV='localhost'

        local -r g_USR_PROFILE="$HOME"
        local -r g_APP_DATA="$g_USR_PROFILE/.config"
    fi

    ###########################################################################

    ## 所有环境变量的设置，也可写入/etc/environment文件
    export LC_ALL=en_US.UTF-8   # fix WSL error
    export LC_CTYPE=zh_CN.UTF-8 # use Sougou Input in Emacs

    ## Emacs
    local dir
    for dir in "project" "projects" "Project" "Projects"; do
        if [ -d "$HOME/$dir/Emacs" ]; then
            local -r emacs_dir="$HOME/$dir"
            break
        fi
    done
    if [ -z "$emacs_dir" ]; then
        ErrorReturn "Setup[0]" "Please clone https://github.com/wangtonylyan/Emacs.git first."
        return 1
    fi

    if [ ! -d "$HOME/.emacs.d" ]; then
        LinkAlways "$HOME/.emacs.d" "$emacs_dir"
    fi

    local -r g_CFG_HOME="$HOME/.emacs.d/my.config"

    ###########################################################################

    # AddToPath "$HOME/.local/bin" # already added by .profile
    Proxy || return 1
    Apt || return 1
    VSCode || return 1
    Others || return 1
}

function Proxy() {
    unset WSL_HOST_IP
    unset ALL_PROXY HTTP_PROXY HTTPS_PROXY NO_PROXY
    unset all_proxy http_proxy https_proxy no_proxy

    if [ -z "$g_PROXY_SRV" ]; then
        return 0
    fi

    CheckGlobalVar || return 1

    if [ "$g_WSL_ENV" = true ]; then
        WSL_HOST_IP="$g_PROXY_SRV"
        export WSL_HOST_IP # maybe useful for some cases
    fi

    ## 常用的协议为http(s)和socks5，URL格式皆为<username>:<password>@<server>:<port>
    ## 需要同时设置所有大小写同名的变量，以使更多的软件生效，因为部分软件仅读取特定大小写的变量

    ALL_PROXY=$(printf 'http://%s:34560/' "$g_PROXY_SRV")
    # ALL_PROXY=$(printf 'socks5://%s:34561/' "$g_PROXY_SRV")
    HTTP_PROXY="$ALL_PROXY"
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

    ## 检测网络访问，还可以启用curl -v选项，查看详细日志
    # curl https://ip.gs
    # curl ipinfo.io
    # curl cip.cc
}

function Apt() {
    local -r apt_dir="/etc/apt"
    local -r apt_cfg="$apt_dir/apt.conf"
    local -r apt_src="$apt_dir/sources.list"
    local -r codename=$(lsb_release -cs)

    CheckGlobalVar || return 1

    if [ -n "$g_PROXY_SRV" ]; then
        local -r src="default"
    else
        local -r src="tsinghua"
        # local -r src="aliyun"
    fi

    local -r config="$g_CFG_HOME/system/apt.conf"
    local -r source=$(printf "$g_CFG_HOME/system/apt.sources.%s" "$src")

    if [ -d "$apt_dir" ] && [ -e "$config" ]; then
        SudoQ rm -f "$apt_cfg"
        SudoQ cp -f "$config" "$apt_cfg"

        SudoQ sed -i "/^\s*Acquire::http::Proxy/d" "$apt_cfg"
        SudoQ sed -i "/^\s*Acquire::https::Proxy/d" "$apt_cfg"

        if [ -n "$HTTP_PROXY" ] && [ -n "$HTTPS_PROXY" ]; then
            SudoQ "echo \"Acquire::http::Proxy  \\\"$HTTP_PROXY\\\";\"  >> $apt_cfg"
            SudoQ "echo \"Acquire::https::Proxy \\\"$HTTPS_PROXY\\\";\" >> $apt_cfg"
        fi
    fi

    if [ -d "$apt_dir" ] && [ -e "$source" ]; then
        SudoQ "sed \"s/<codename>/${codename}/g\" $source > $apt_src"
    fi

    if [ "$g_INIT_LOAD" = true ]; then
        local pkg_list=("net-tools" "build-essential" "jq")

        if [ "$g_WSL_ENV" = true ]; then
            ## https://wslutiliti.es/wslu/
            ## https://github.com/wslutilities/wslu
            ## https://launchpad.net/~wslutilities/+archive/ubuntu/wslu
            Sudo add-apt-repository ppa:wslutilities/wslu
            pkg_list+=("wslu")
        fi

        local -r pkg_list
        Sudo apt update && Sudo apt -y dist-upgrade && Sudo apt -y autoremove && {
            local pkg
            for pkg in "${pkg_list[@]}"; do
                dpkg -s "$pkg" >/dev/null 2>&1 || Sudo apt install -y "$pkg"
            done
        }
    fi
}

function VSCode() {
    local -r vsc_dir="$g_APP_DATA/Code/User"
    local -r vsc_cfg="$vsc_dir/settings.json"
    local -r vsc_key="$vsc_dir/keybindings.json"

    CheckGlobalVar || return 1

    ## 注意，在GUI中修改设置后，写入的都是Windows中的json文件
    ## 因此，还需手动同步并替换，GitHub项目中的对应文件
    local -r config="$g_CFG_HOME/vscode/settings.json"
    local -r config_tmp="${config}.tmp"
    local -r keyset="$g_CFG_HOME/vscode/keybindings.json"

    if [ -e "$vsc_cfg" ] && [ -e "$config" ] && [ ! "$vsc_cfg" -nt "$config" ]; then
        if [ -n "$HTTP_PROXY" ]; then
            if grep -q -e "^\s*\"http.proxy\":\s*\"$HTTP_PROXY\"" "$vsc_cfg"; then
                local -r vsc_cfg_proxy=false
            else
                local -r vsc_cfg_proxy=true
            fi
        else
            if grep -q -e "^\s*\"http.proxy\"" "$vsc_cfg"; then
                local -r vsc_cfg_proxy=true
            else
                local -r vsc_cfg_proxy=false
            fi
        fi

        if [ "$vsc_cfg_proxy" = true ] || [ "$vsc_cfg" -ot "$config" ]; then
            if [ -n "$HTTP_PROXY" ]; then
                # jq only accepts strict json, i.e. fails to recognize comments
                sed 's/\(^\/\/.*\|\s\/\/.*\)//' "$config" |
                    jq --indent 4 "del(.[\"http.proxy\"]) | .[\"http.proxy\"] = \"$HTTP_PROXY\"" >"$config_tmp"
            else
                sed 's/\(^\/\/.*\|\s\/\/.*\)//' "$config" |
                    jq --indent 4 "del(.[\"http.proxy\"])" >"$config_tmp"
            fi

            touch -r "$config_tmp" -m "$config"

            if [ "$g_WSL_ENV" = true ]; then
                CopyAlwaysWindows "$vsc_cfg" "$config_tmp"
            else
                CopyAlways "$vsc_cfg" "$config_tmp"
            fi
        fi
    fi

    if [ -e "$vsc_key" ] && [ -e "$keyset" ] && [ ! "$vsc_key" -nt "$keyset" ]; then
        if [ "$vsc_key" -ot "$keyset" ]; then
            if [ "$g_WSL_ENV" = true ]; then
                CopyAlwaysWindows "$vsc_key" "$keyset"
            else
                CopyAlways "$vsc_key" "$keyset"
            fi
        fi
    fi
}

function Others() {
    CheckGlobalVar || return 1

    ## SSH
    # LinkAlways "/etc/ssh/ssh_config" "$g_CFG_HOME/system/ssh_config"
    # LinkAlways "/etc/ssh/sshd_config" "$g_CFG_HOME/system/sshd_config"
    ## Zsh
    # LinkExists "$HOME/.zshrc" "$g_CFG_HOME/system/zshrc.sh"
    ## Tmux
    # LinkExists "$HOME/.tmux.conf" "$g_CFG_HOME/system/tmux.conf"

    ## Git
    if [ "$g_WSL_ENV" = true ]; then
        CopyExistsWindows "$g_USR_PROFILE/.gitconfig" "$g_CFG_HOME/system/gitconfig"
    fi
    LinkExists "$HOME/.gitconfig" "$g_CFG_HOME/system/gitconfig"

    ## Go
    AddToPath "$HOME/lib/go/bin"
    ## Rust
    AddToPath "$HOME/.cargo/bin"
    ## Python
    LinkExists "$HOME/.config/pycodestyle" "$g_CFG_HOME/program/pycodestyle.cfg"
    ## Node.js
    AddToPath "$HOME/lib/nodejs/node-v14.17.5-linux-x64/bin"
    ## Haskell
    LinkExists "$HOME/.cabal/config" "$g_CFG_HOME/program/cabal.config"
    LinkExists "$HOME/.stack/config.yaml" "$g_CFG_HOME/program/stack-config.yaml"
    LinkExists "$HOME/.config/brittany/config.yaml" "$g_CFG_HOME/program/brittany.yaml"
    AddToPath "/opt/ghc/bin"
}

###############################################################################

echo "my profile.sh loading ..."
Setup && echo "my profile.sh loaded" || echo "my profile.sh failed"
export | grep "WSL_HOST_IP"
export | grep "PROXY"

unset Sudo SudoQ AddToPath LinkAlways LinkExists
unset CopyAlways CopyExists CopyAlwaysWindows CopyExistsWindows
unset ErrorReturn CheckGlobalVar
unset Setup Proxy Apt VSCode Others
