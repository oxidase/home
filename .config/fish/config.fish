set fish_greeting
source ~/.config/fish/conf.d/done.fish
source ~/.config/fish/functions/fish_prompt.fish
if test -f ~/.config/fish/(hostname)
   source ~/.config/fish/(hostname)
end
