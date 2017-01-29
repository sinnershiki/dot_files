########################################
# 基本設定
########################################
# Prefix
set-option -g prefix C-t
bind-key C-t send-prefix
unbind-key C-b
unbind-key -n C-a
# bind C-r source-file ~/.tmux.conf

# pane & window
unbind 1
unbind ^C
unbind &
bind 1 break-pane
bind 2 split-window -v
bind 3 split-window -h
bind C-k kill-pane
bind i display-panes
bind C-K kill-window
bind C-t next-window
bind c  new-window

# copy
bind y copy-mode
bind p paste-buffer