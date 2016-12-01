## specific host settings

export HOSTNAME
case $HOSTNAME in
pc*)
    rcfiles="pcmpi $HOSTNAME" ;;
node*.*.mpimd|otto*)
    rcfiles="otto" ;;
*neclus*)
    rcfiles=neclus ;;
*)
    rcfiles=$HOSTNAME ;;
esac

for h in $rcfiles ; do
    [ -f "$HOME/.bashrc.d/$h" ] && source "$HOME/.bashrc.d/$h"
done

## locale settings
export LANG="en_US.UTF-8"
export LC_TIME="de_DE.UTF-8"
export LC_PAPER="en_GB.UTF-8"
export LC_MEASUREMENT="en_GB.UTF-8"
export LC_ALL="en_US.UTF-8"

## common settings
export PYTHONSTARTUP=~/.pythonrc
export PS1='\[\033[01;32m\][\u@\h]\[\033[00m\] \[\033[01;34m\]$(x="\w";echo "${x#${x%/*/*}/}")\[\033[00m\] \$ '
export HISTCONTROL=ignoredups

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
# Enable programmable sdb completion features.
if [ -f ~/.sdb/.sdb-completion.bash ]; then
 source ~/.sdb/.sdb-completion.bash
fi

## git helper
alias git-showlost='git fsck --full --no-reflogs --unreachable --lost-found && ls -1 .git/lost-found/commit/ | xargs -n 1 git log -n 1 --pretty=oneline'

export NVM_DIR="/home/miha/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
