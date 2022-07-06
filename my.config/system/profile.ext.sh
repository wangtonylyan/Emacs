function Docker() {
    local -r apt_dir="/etc/apt"
    local -r apt_cfg="$apt_dir/apt.conf"
    local -r apt_src="$apt_dir/sources.list"
    local -r codename=$(lsb_release -cs)

    if [ "$sys_WSL_ENV" = true ]; then
        return 0
    fi
    if [ ! -e "$apt_cfg" ] || [ ! -e "$apt_src" ]; then
        ErrorReturn "Docker[0]"
        return 1
    fi

    ## https://docs.docker.com/engine/install/ubuntu/
    if [ "$sys_INIT_LOAD" = true ]; then
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

    if [ "$sys_INIT_LOAD" = true ]; then
        Sudo apt update &&
            Sudo apt install -y docker-ce docker-ce-cli containerd.io || {
            ErrorReturn "Docker[2]"
            return 1
        }
    fi

    ## Docker Daemon
    CopyExists "/etc/default/docker"       "$sys_CFG_HOME/system/docker.default"      # for init or upstart
    CopyExists "/etc/docker/daemon.json"   "$sys_CFG_HOME/system/docker.daemon.json"  # for systemd
    ## Docker Container
    "$HOME/.docker/config.json" "$sys_CFG_HOME/system/docker.config.json"
}

## https://github.com/craftzdog/dotfiles-public
function FishShell() {
    ## Fish shell
    ## https://launchpad.net/~fish-shell/+archive/ubuntu/release-3
    sudo add-apt-repository ppa:fish-shell/release-3
    sudo apt update
    sudo apt install fish

    fish

    ## Fisher
    ## https://github.com/jorgebucaran/fisher
    curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher

    ## Tide
    ## https://github.com/IlanCosman/tide
    fisher install IlanCosman/tide@v5 # 选择Lean Prompt Style，以避免安装额外的字体

    ## Z
    ## https://github.com/jethrokuan/z
    fisher install jethrokuan/z

    ## use fish as default shell
    # echo /usr/bin/fish | sudo tee -a /etc/shells
    chsh -s /usr/bin/fish
}
