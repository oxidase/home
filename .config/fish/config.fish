set fish_greeting
source ~/.config/fish/conf.d/done.fish
source ~/.config/fish/functions/fish_prompt.fish
source ~/.config/fish/completions/bazel.fish
if test -f ~/.config/fish/(hostname)
   source ~/.config/fish/(hostname)
end


# for name in Ubuntu UbuntuMono DejaVuSansMono ;
#     unzip -o (curl -sL https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/$name.zip | psub) -d ~/.local/share/fonts/$name
# end;
# fc-cache -f ~/.local/share/fonts/