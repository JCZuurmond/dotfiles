# dotfiles

## Setup on a new machine

1. Clone the repo:
```sh
git clone https://github.com/JCZuurmond/dotfiles.git ~/github/JCZuurmond/dotfiles
cd ~/github/JCZuurmond/dotfiles
```

2. Use [GNU Stow](https://www.gnu.org/software/stow/) to symlink config directories into `$HOME`:
```sh
stow -t $HOME <directory>
```

For example:
```sh
stow -t $HOME zsh
stow -t $HOME git
stow -t $HOME claude
```

3. For Claude Code, symlink manually since `~/.claude/` contains non-stowable runtime files:
```sh
ln -s ~/github/JCZuurmond/dotfiles/claude/CLAUDE.md ~/.claude/CLAUDE.md
ln -s ~/github/JCZuurmond/dotfiles/claude/settings.json ~/.claude/settings.json
```

# Mac

Install:
- brew

Brew install iterm
- zsh
- iterm2

Settings
- iTerm2 > Preferences > Profiles > Colors > Color Presets > Solarized Dark
- Keyboard > Modifier Keys > Set Caps Lock to Escape

## Oh-my-zsh

Install power level theme
git clone https://github.com/romkatv/powerlevel10k.git $ZSH_CUSTOM/themes/powerlevel10k

## Emacs

Follow [install instructions for Doom Emacs](https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org)

# Linux

## (Re)generate i3 config

```
>> cat ~/.config/i3/i3.local     # settings for laptop
set $prim_screen eDP1
set $sec_screen DP1
set $dpi 
set $mode
>> i3-regenerate      #  in dotfiles/local/.local/bin
```

For work laptop I had the following settings:

```
>> cat ~/.config/i3/i3.local     # settings for work laptop
set $prim_screen eDP-1
set $sec_screen DP3
set $dpi --dpi 220
set $mode --mode 3840x2160
```
