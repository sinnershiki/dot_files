# Dot files

dot files管理Repo

## how to setup

初回対応

```shell
# clone dot_files
mkdir -p ~/work/repo
cd ~/work/repo
git clone git@github.com:sinnershiki/dot_files.git
cd dot_files

# brewで必要なやつ入れる
brew bundle

# install ricty font
cp -f /opt/homebrew/opt/ricty/share/fonts/Ricty*.ttf ~/Library/Fonts/
cp -f /usr/local/opt/ricty/share/fonts/Ricty*.ttf ~/Library/Fonts/

# terminal / tool系
mkdir -p ~/.zsh
ln -s ${PWD}/.zshrc ~/.zshrc
ln -s ${PWD}/.zsh_configs/* ~/.zsh/.
ln -s ${PWD}/.byobu ~/.byobu
ln -s ${PWD}/.peco ~/.peco
ln -s ${PWD}/.tmux.conf ~/.tmux.conf

# git
ln -s ${PWD}/.gitignore_global ~/.gitignore_global
cp -pr ${PWD}/.gitconfig.example ~/.gitconfig
vi ~/.gitconfig
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
