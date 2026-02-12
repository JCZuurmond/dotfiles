# dotfiles
'Stow' the directories to use the settings

## Stow
Stow the following:

``` sh
$ stow -t $HOME pip
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
