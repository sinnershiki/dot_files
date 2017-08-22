# .bashrc
PS1="\[\e[36m\e[40m\][\u@\h \W \d \t]\[\e[0m\]\n\\$ "

if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi
