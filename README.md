# dotfiles
'Stow' the directories to use the settings


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
