function git-remote-refs --description 'List remote references'
    git for-each-ref --sort=committerdate --format='%(committerdate) %09 %(align:width=24,position=left)%(authorname)%(end) %09 %(refname)' refs/remotes
end
