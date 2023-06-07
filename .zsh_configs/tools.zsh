########################################
## tools
# tmux
autoload -Uz add-zsh-hook
function rename_tmux_window() {
  if [ $TERM = "screen" ]; then
    local current_path=`pwd | sed -e s/\ /_/g`
    local current_dir=`basename $current_path`
    tmux rename-window $current_dir
  fi
}
add-zsh-hook precmd rename_tmux_window

# direnv
eval "$(direnv hook zsh)"

########################################
# k8s
source <(kubectl completion zsh)
# kubec-ps1 : brew install kube-ps1
source "$(brew --prefix)/opt/kube-ps1/share/kube-ps1.sh"
# GKE
USE_GKE_GCLOUD_AUTH_PLUGIN=True

########################################
## lang
# golang
export PATH="$PATH:/usr/local/opt/go/libexec/bin"
export GOPATH="$HOME/.go"
export PATH="$PATH:$GOPATH/bin"

# anyenv
export PATH="$HOME/.anyenv/bin:$PATH"
if [ -e ~/.anyenv ]; then
  eval "$(anyenv init -)"
fi

# ruby:rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
export PATH="/opt/homebrew/bin/rbenv:$PATH"
if [ -e ~/.rbenv ]; then
  eval "$(rbenv init - zsh)"
fi

# nodejs:nodebrew
export PATH="$HOME/.nodebrew/current/bin:$PATH"
if [ -f ~/.nodebrew/nodebrew ]; then
  fpath=($HOME/.nodebrew/completions/zsh $fpath)
fi
#export NODE_PATH=/usr/local/lib/node_modules
#export PATH=$PATH:$NODE_PATH

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# python:pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if [ -e ~/.pyenv ]; then
  eval "$(pyenv init -)"
fi
# pyenv-virtualenv
if [ -e ~/.pyenv/plugins/pyenv-virtualenv ]; then
  eval "$(pyenv virtualenv-init -)"
fi

# poetry
export PATH="$HOME/.poetry/bin:$PATH"
fpath=(~/.zsh/.zfunc/_poetry $fpath)
rm -f ~/.zcompdump; compinit

# php-nabe
export PATH=$HOME/.php-nabe/bin:$PATH

# composer
export PATH="$HOME/.composer:$PATH"
export PATH="$HOME/.composer/vendor/bin:$PATH"
alias composer="composer.phar"

# envs update
function envs_update(){
    cur=$(pwd)
    dirs=($HOME/.rbenv $HOME/.rbenv/plugins/ruby-build $HOME/.pyenv $HOME/.pyenv/plugins/pyenv-virtualenv)
    for dir in $dirs; do
        if [ -e $dir ]; then
          cd $dir
          pwd
          git pull origin master
        fi
    done
    cd $cur
}

export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"
export PATH=$HOME/.local/bin:$PATH

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/sugano-kosuke/work/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/sugano-kosuke/work/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/sugano-kosuke/work/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/sugano-kosuke/work/google-cloud-sdk/completion.zsh.inc'; fi
