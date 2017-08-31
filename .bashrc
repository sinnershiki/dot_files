# .bashrc
PS_OK=$'\n''%F{green}CURRENT=%~'$'\n%F{yellow}✖╹◡╹✖ %F{magenta}☆☆☆  %F{nomal}'
PS_NG=$'\n''%F{red}CURRENT=%~'$'\n%F{yellow}xX_Xx %F{magenta}☆☆☆  %F{nomal}'

PS1="%0(?|${PS_OK}|${PS_NG})"

if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi
