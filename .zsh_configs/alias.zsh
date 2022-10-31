########################################
# エイリアス
alias la='ls -a'
alias ll='ls -l'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias g='git'
alias grep='grep --color'

alias pbcopy="nkf -w | __CF_USER_TEXT_ENCODING=0x$(printf %x $(id -u)):0x08000100:14 pbcopy"

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

# グローバルエイリアス
alias -g L='| less'
alias -g G='| grep'
alias -g R="| uniq -c | sort -k1nr | sed -e 's/^[ ]*//g' "

## k8s
alias k='/Users/sugano-kosuke/work/google-cloud-sdk/bin/kubectl'
alias kc='kubectx | peco | xargs kubectx'
alias kn='kubens | peco | xargs kubens'
alias kl='/Users/sugano-kosuke/work/google-cloud-sdk/bin/kubectl get pods | tail -n +2 | awk "{ print \$1 }" | peco | xargs kubectl logs '
alias ke='/Users/sugano-kosuke/work/google-cloud-sdk/bin/kubectl get pods | tail -n +2 | awk "{ print \$1 }" | peco | xargs -I POD kubectl exec POD '
