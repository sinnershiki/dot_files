# Dot files

dot files管理Repo

## how to setup

初回対応

```shell
# brewで必要なやつ入れる
brew bundle

# terminal / tool系
ln -s .zshrc ~/.zshrc
ln -s .zsh_configs/* ~/.zsh/.
ln -s .byobu ~/.byobu
ln -s .peco ~/.peco
ln -s .tmux.conf ~/.tmux.conf

# git
ln -s .gitignore_global ~/.gitignore_global
cp -pr .gitconfig.example ~/.gitconfig
vi .gitconfig
```

## .zsh tools

zshの拡張まわり

- ansible-zsh-completion
- cd-gitroot
- enhancd
- zsh-256color
- zsh-autosuggestions
- zsh-completions
