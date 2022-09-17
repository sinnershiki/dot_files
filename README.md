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

- [cd-gitroot](https://github.com/mollifier/cd-gitroot)
- [enhancd](https://github.com/b4b4r07/enhancd)
- [zsh-256color](https://github.com/chrissicool/zsh-256color)
- [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions)
- [zsh-completions](https://github.com/zsh-users/zsh-completions)

install

```shell
git clone https://github.com/mollifier/cd-gitroot.git ~/.zsh/cd-gitroot
git clone https://github.com/b4b4r07/enhancd ~/.zsh/enhancd
git clone https://github.com/chrissicool/zsh-256color ~/.zsh/zsh-256color
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.zsh/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-completions.git ~/.zsh/zsh-completions
```
