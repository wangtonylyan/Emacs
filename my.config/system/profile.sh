Sudo () { echo "$USER" | sudo -S -k "$@" > /dev/null 2>&1 ; }
# unset -f Sudo


export http_proxy=   # "http://<username>:<password>@<server>:<port>"
export https_proxy=  # "https://<username>:<password>@<server>:<port>"
export no_proxy=     # "127.0.0.1, localhost"

# v2rayN proxy
export http_proxy="http://127.0.0.1:10809"
export https_proxy="http://127.0.0.1:10809"
export all_proxy="socks5://127.0.0.1:10808"


export LC_ALL=en_US.UTF-8   # WSL error
export LC_CTYPE=zh_CN.UTF-8 # use Sougou Input in Emacs


MY_CFG_EMACS_DIR="$HOME/.emacs.d"
MY_CFG_DIR_ROOT="$MY_CFG_EMACS_DIR/my.config"
if $MY_CFG_PRIVATE_ENABLED; then
    MY_CFG_BASHRC="$MY_CFG_EMACS_DIR/.private/bashrc.sh"
    MY_CFG_APT_CONF="$MY_CFG_EMACS_DIR/.private/apt.conf"
    MY_CFG_GIT_CONF="$MY_CFG_EMACS_DIR/.private/gitconfig"
else
    MY_CFG_BASHRC=""
    MY_CFG_APT_CONF="$MY_CFG_DIR_ROOT/system/apt.conf"
    MY_CFG_GIT_CONF="$MY_CFG_DIR_ROOT/system/gitconfig"
fi

# MY_CFG_APT_SOURCES="$MY_CFG_DIR_ROOT/system/apt.sources.list.china"
# MY_CFG_APT_SOURCES="$MY_CFG_DIR_ROOT/system/apt.sources.list.tsinghua"
MY_CFG_APT_SOURCES="$MY_CFG_DIR_ROOT/system/apt.sources.list.aliyun"

MY_CFG_DOCKER_DEFAULT="$MY_CFG_DIR_ROOT/system/docker.default"
MY_CFG_DOCKER_DAEMON="$MY_CFG_DIR_ROOT/system/docker.daemon.json"

MY_CFG_ZSHRC="$MY_CFG_DIR_ROOT/system/zshrc.sh"
MY_CFG_TMUX_CONF="$MY_CFG_DIR_ROOT/system/tmux.conf"
MY_CFG_SSH_CONFIG="$MY_CFG_DIR_ROOT/system/ssh_config"
MY_CFG_SSHD_CONFIG="$MY_CFG_DIR_ROOT/system/sshd_config"

MY_CFG_VSCODE_SETTINGS="$MY_CFG_DIR_ROOT/vscode/settings.json"
MY_CFG_VSCODE_KEYBINDINGS="$MY_CFG_DIR_ROOT/vscode/keybindings.json"

MY_CFG_NODEJS_VERSION="v12.13.0"
MY_CFG_NODEJS_DISTRO="linux-x64"
MY_CFG_NODEJS_INSTALL_DIR="/usr/local/lib/nodejs/node-$MY_CFG_NODEJS_VERSION-$MY_CFG_NODEJS_DISTRO"


if [ -d "$HOME/Projects/Emacs" ] && [ ! -e "$MY_CFG_EMACS_DIR" ]; then
    ln -s "$HOME/Projects/Emacs" "$MY_CFG_EMACS_DIR"
fi

if [ -e "$MY_CFG_BASHRC" ]; then
    source "$MY_CFG_BASHRC"
fi

if [ -d "/etc/apt" ] && [ -e "$MY_CFG_APT_CONF" ]; then
    Sudo rm -f '/etc/apt/apt.conf'
    Sudo ln -s "$MY_CFG_APT_CONF" '/etc/apt/apt.conf'
fi
if [ -d "/etc/apt" ] && [ -e "$MY_CFG_APT_SOURCES" ]; then
    Sudo rm -f '/etc/apt/sources.list'
    Sudo ln -s "$MY_CFG_APT_SOURCES" '/etc/apt/sources.list'
fi

if [ -d "/etc/default" ] && [ -e "$MY_CFG_DOCKER_DEFAULT" ]; then
    Sudo rm -f '/etc/default/docker'
    Sudo ln -s "$MY_CFG_DOCKER_DEFAULT" '/etc/default/docker'
fi
if [ -d "/etc/docker" ] && [ -e "$MY_CFG_DOCKER_DAEMON" ]; then
    Sudo rm -f '/etc/docker/daemon.json'
    Sudo ln -s "$MY_CFG_DOCKER_DEFAULT" '/etc/docker/daemon.json'
fi

if [ -e "$HOME/.gitconfig" ] && [ -e "$MY_CFG_GIT_CONF" ]; then
    rm -f "$HOME/.gitconfig"
    ln -s "$MY_CFG_GIT_CONF" "$HOME/.gitconfig"
fi
if [ -e "$HOME/.zshrc" ] && [ -e "$MY_CFG_ZSHRC" ]; then
    rm -f "$HOME/.zshrc"
    ln -s "$MY_CFG_ZSHRC" "$HOME/.zshrc"
fi
if [ -e "$HOME/.tmux.conf" ] && [ -e "$MY_CFG_TMUX_CONF" ]; then
    rm -f "$HOME/.tmux.conf"
    ln -s "$MY_CFG_TMUX_CONF" "$HOME/.tmux.conf"
fi

if [ -d "/etc/ssh" ] && [ -e "$MY_CFG_SSH_CONFIG" ]; then
    Sudo rm -f "/etc/ssh/ssh_config"
    Sudo ln -s "$MY_CFG_SSH_CONFIG" "/etc/ssh/ssh_config"
fi
if [ -d "/etc/ssh" ] && [ -e "$MY_CFG_SSHD_CONFIG" ]; then
    Sudo rm -f "/etc/ssh/sshd_config"
    Sudo ln -s "$MY_CFG_SSHD_CONFIG" "/etc/ssh/sshd_config"
fi

if [ -d "$HOME/.config/Code/User" ] && [ -e "$MY_CFG_VSCODE_SETTINGS" ]; then
    rm -f "$HOME/.config/Code/User/settings.json"
    ln -s "$MY_CFG_VSCODE_SETTINGS" "$HOME/.config/Code/User/settings.json"
fi
if [ -d "$HOME/.config/Code/User" ] && [ -e "$MY_CFG_VSCODE_KEYBINDINGS" ]; then
    rm -f "$HOME/.config/Code/User/keybindings.json"
    ln -s "$MY_CFG_VSCODE_KEYBINDINGS" "$HOME/.config/Code/User/keybindings.json"
fi

if [ -d "$MY_CFG_NODEJS_INSTALL_DIR/bin" ]; then
    export PATH="$MY_CFG_NODEJS_INSTALL_DIR/bin":$PATH
    Sudo ln -s "$MY_CFG_NODEJS_INSTALL_DIR/bin/node" "/usr/local/bin/node"
    Sudo ln -s "$MY_CFG_NODEJS_INSTALL_DIR/bin/npm"  "/usr/local/bin/npm"
    Sudo ln -s "$MY_CFG_NODEJS_INSTALL_DIR/bin/npx"  "/usr/local/bin/npx"
fi

if [ -d "$HOME/.local/bin" ]; then
    export PATH="$HOME/.local/bin":$PATH
fi
if [ -d "/opt/ghc/bin" ]; then
    export PATH="/opt/ghc/bin":$PATH
fi
