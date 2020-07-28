set fish_greeting
set -x SHELL /usr/bin/fish
source ~/.config/fish/conf.d/done.fish
source ~/.config/fish/functions/fish_prompt.fish
source ~/.config/fish/completions/bazel.fish
if test -f ~/.config/fish/(hostname)
   source ~/.config/fish/(hostname)
end

function ll --description 'List contents of directory using long format'
    ls -la $argv
end

function git-remote-refs  --description 'List remote references'
    git for-each-ref --sort=committerdate --format='%(committerdate) %09 %(align:width=24,position=left)%(authorname)%(end) %09 %(refname)' refs/remotes
end

# for name in Ubuntu UbuntuMono DejaVuSansMono ;
#     unzip -o (curl -sL https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/$name.zip | psub) -d ~/.local/share/fonts/$name
# end;
# fc-cache -f ~/.local/share/fonts/
