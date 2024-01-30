set fish_greeting
set -l hostfile (hostname -s)
if test -f ~/.config/fish/$hostfile
   source ~/.config/fish/$hostfile
end
source ~/.config/fish/conf.d/done.fish
source ~/.config/fish/functions/fish_prompt.fish
source ~/.config/fish/completions/bazel.fish


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

bind --user \cq "emacsclient -n --eval (printf '(find-file-goto \"%s\" \"%s\")' (xclip -o) (pwd))"

alias R "R --no-save"


function urlsafe_b64encode_nopad
    python3 -c "import sys,base64; print(base64.urlsafe_b64encode(bytes.fromhex(sys.argv[1])).rstrip(b'=').decode())" $argv[1]
end


function aws-cancel-jobs
    for i in $(aws batch list-jobs --job-queue $argv[1] --job-status runnable --output text --query "jobSummaryList[*].[jobId]")
        echo "Cancel Job: $i"
        aws batch cancel-job --job-id $i --reason "Cancelling job."
        echo "Job $i canceled"
    end
end
