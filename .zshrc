########################################
# 環境変数
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8

# zshではマッチしないglob表現を含む場合no match error をprintしてコマンドをrunしなくなる。
setopt +o nomatch

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

function cd_up() {
  \cd ..
  zle .accept-line
}
zle -N cd_up
bindkey '^u' cd_up

########################################
# 分割ファイルの読み込み

# git管理しないやつ
if [ -e ~/.zshrc_ignore ]; then
  source ~/.zshrc_ignore
fi

# .zsh配下の .zsh 拡張子ファイルの読み込み
ZSHHOME="${HOME}/.zsh"
if [ -d $ZSHHOME -a -r $ZSHHOME -a \
     -x $ZSHHOME ]; then
    for i in $ZSHHOME/*; do
        [[ ${i##*/} = *.zsh ]] &&
            [ \( -f $i -o -h $i \) -a -r $i ] && . $i
    done
fi

########################################
# プロンプト
PROMPT="%{${fg[red]}%}${bg[white]}%}[%n@%m]%{${reset_color}%} %~
%# "
RPROMPT="$(kube_ps1)%1(v|%F{green}%1v%f|)"

########################################
# OS 別の設定
case ${OSTYPE} in
#Mac用の設定
darwin*)
    source ~/.zsh/os.zsh.mac
    ;;
########################################
#Linux用の設定
linux*)
    source ~/.zsh/os.zsh.linux
    ;;
esac
