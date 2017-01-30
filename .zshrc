########################################
# 環境変数
export LANG=ja_JP.UTF-8

# 色を使用出来るようにする
autoload -Uz colors
colors

# emacs 風キーバインドにする
bindkey -e

# ヒストリの設定
HISTFILE=${HOME}/.zsh_history

# メモリに保存される履歴の件数
HISTSIZE=100000

# 履歴ファイルに保存される履歴の件数
SAVEHIST=1000000

# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default
# ここで指定した文字は単語区切りとみなされる
# / も区切りと扱うので、^W でディレクトリ１つ分を削除できる
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

########################################
# 補完
# 補完機能を有効にする
autoload -Uz compinit
compinit

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..

# sudo の後ろでコマンド名を補完する
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
       /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'

########################################
# vcs_info
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
RPROMPT="%1(v|%F{green}%1v%f|)"

########################################
# オプション
# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# beep を無効にする
setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# '#' 以降をコメントとして扱う
setopt interactive_comments

# ディレクトリ名だけでcdする
setopt auto_cd

# cd したら自動的にpushdする
setopt auto_pushd

# = の後はパス名として補完する
setopt magic_equal_subst

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# ヒストリファイルに保存するとき、すでに重複したコマンドがあったら古い方を削除する
setopt hist_save_nodups

# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space

# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks

# 補完候補が複数あるときに自動的に一覧表示する
setopt auto_menu

# 高機能なワイルドカード展開を使用する
setopt extended_glob

########################################
# キーバインド
# ^R で履歴検索をするときに * でワイルドカードを使用出来るようにする
bindkey '^R' history-incremental-pattern-search-backward

########################################
# エイリアス
alias la='ls -a'
alias ll='ls -l'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias g='git'
alias e='emacs'

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

# グローバルエイリアス
alias -g L='| less'
alias -g G='| grep'
alias -g R="| uniq -c | sort -k1nr | sed -e 's/^[ ]*//g' "

########################################
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

########################################
# zsh plugin
# zplug
if [ -e ~/.zplug ]; then
  source ~/.zplug/init.zsh
  zplug "zplug/zplug"

  # autosaggestions
  zplug "zsh-users/zsh-autosuggestions"

  # enhancd
  zplug "b4b4r07/enhancd", use:init.sh
  ENHANCD_FILTER=peco

  # cd-gitroot
  zplug "mollifier/cd-gitroot"
  alias cdu='cd-gitroot'

  # zsh-completions
  zplug "zsh-users/zsh-completions"

  # 256-color
  zplug "chrissicool/zsh-256color"

  # zsh-syntax-highlighting は compinit の前に読み込まれる必要がある
  # （2 以上は compinit 後に読み込まれるようになる）
  zplug "zsh-users/zsh-syntax-highlighting", defer:2

  # 未インストール項目をインストールする
  if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
      echo; zplug install
    fi
  fi

  # コマンドをリンクして、PATH に追加し、プラグインは読み込む
  zplug load --verbose
fi

########################################
# 拡張
# peco
if type peco >/dev/null 2>&1; then
  alias -g P='| peco'
  function peco-select-history() {
      local tac
      if which tac > /dev/null; then
        tac="tac"
      else
        tac="tail -r"
      fi
      BUFFER=$(\history -n 1 | \
                 eval $tac | \
                 # ソート
                 #uniq -c | sort -k1nr | \
                 # 先頭の空白と数字を削除
                 #sed -e 's/^[ ]*//g' | cut -d ' ' -f 2- | \
                 awk '!a[$0]++' | \
                 peco --query "$LBUFFER")
      CURSOR=$#BUFFER
      zle clear-screen
  }
  zle -N peco-select-history
  bindkey '^r' peco-select-history

  # 開始と終了を記録
  setopt EXTENDED_HISTORY
fi

########################################
# OS 別の設定
case ${OSTYPE} in
#Mac用の設定
darwin*)
    ########################################
    # terminalに関する設定
    # プロンプト
    PROMPT="%{${fg[red]}%}${bg[white]}%}[%n@%m]%{${reset_color}%} %~
%# "

    export CLICOLOR=1
    export LSCOLORS=gxfxcxdxbxegedabagacad
    alias ls='ls -G -F'

    #brew
    alias brew="env PATH=${PATH/\/Users\/sinner\/\.pyenv\/shims:/} brew"
    export PATH="/bin:/usr/bin:/usr/local/bin:/sbin:$PATH"
    export PATH="/usr/local/sbin:$PATH"
    export PATH="/usr/local/bin:$PATH"

    #tex
    export PATH="$PATH:/usr/local/texlive/2014/bin/x86_64-darwin"

    # emacs
    alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'

    # MySQL Path Setting
    export PATH="$PATH:/usr/local/mysql/bin"

    # postgres
    export PGDATA="/usr/local/var/postgres"
    ;;
########################################
#Linux用の設定
linux*)
    # プロンプト
    PROMPT="%{${fg[cyan]}%}[%n@%m]%{${reset_color}%} %~
%# "

    #ls 色付け
    alias ls='ls -F --color'

    #golang
    export GOROOT="/usr/local/go"
    export PATH="$PATH:$GOROOT/bin"
    ;;
esac

########################################
# 言語
# golang
export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"

# ruby:rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
if [ -e ~/.rbenv ]; then
  eval "$(rbenv init -)"
fi

# nodejs:nodebrew
export PATH="$HOME/.nodebrew/current/bin:$PATH"
#export NODE_PATH=/usr/local/lib/node_modules
#export PATH=$PATH:$NODE_PATH

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

########################################
# Etc
# hub command
if type hub >/dev/null 2>&1; then
  function git(){hub "$@"}
fi

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# added by travis gem
[ -f $HOME/.travis/travis.sh ] && source $HOME/.travis/travis.sh
