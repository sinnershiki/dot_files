########################################
# 環境変数
export LANG=ja_JP.UTF-8

# 色を使用出来るようにする
autoload -Uz colors
colors

# emacs 風キーバインドにする
bindkey -e

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
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

# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# = の後はパス名として補完する
setopt magic_equal_subst

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

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

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

alias g='git'

# グローバルエイリアス
alias -g L='| less'
alias -g G='| grep'

########################################
#tmux
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
# OS 別の設定
case ${OSTYPE} in
    darwin*)
        #Mac用の設定
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

        export PATH=/bin:/usr/bin:/usr/local/bin:/sbin:$PATH
        export PATH="/usr/local/sbin:$PATH"
        ########################################
        # 言語
        #anyenv
        export PATH="$HOME/.anyenv/bin:$PATH"
        eval "$(anyenv init -)"

        #rbenv
        export PATH=$HOME/.rbenv/bin:$PATH
        eval "$(rbenv init - zsh)"

        # added by travis gem
        [ -f /Users/sinner/.travis/travis.sh ] && source /Users/sinner/.travis/travis.sh

        # pyenv
        export PYENV_ROOT="$HOME/.pyenv"
        export PATH="$PYENV_ROOT/bin:$PATH"
        eval "$(pyenv init -)"
        eval "$(pyenv virtualenv-init -)"

        # Go PATH
        export GOPATH=$HOME/go
        export PATH=$PATH:$GOPATH/bin

        #nodebrew
        export PATH=$HOME/.nodebrew/current/bin:$PATH
        #export NODE_PATH=/usr/local/lib/node_modules
        #export PATH=$PATH:$NODE_PATH

        #tex
        export PATH=$PATH:/usr/local/texlive/2014/bin/x86_64-darwin
        ########################################
        # Editor
        # emacs
        alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
        alias e='emacs'

        # vim
        alias vim='/Applications/MacVim.app/Contents/MacOS/MacVim'

        #vscode
        vscode () {
            VSCODE_CWD="$PWD" open -n -b "com.microsoft.VSCode" --args $*
        }
        ########################################
        # Etc
        # Git
        export PATH=/usr/local/bin:$PATH
        export PATH=$PATH:/usr/local/Cellar/git/2.7.4

        # hub command
        function git(){hub "$@"}

        # MySQL Path Setting
        export PATH=$PATH:/usr/local/mysql/bin

        # postgres
        export PGDATA=/usr/local/var/postgres

        ### Added by the Heroku Toolbelt
        export PATH=/usr/local/heroku/bin:$PATH

        #mosquitto
        alias mosquitto_pub=/usr/local/bin/mosquitto_pub
        alias mosquitto_sub=/usr/local/bin/mosquitto_sub

        ########################################
        # zsh拡張plugin
        source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
        ;;
    ########################################
    linux*)
        #Linux用の設定
        # プロンプト
        PROMPT="%{${fg[cyan]}%}[%n@%m]%{${reset_color}%}%~
%# "

        #ls 色付け
        alias ls='ls -F --color'
        ;;
esac
