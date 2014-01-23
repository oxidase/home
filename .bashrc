## specific host settings

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
export LC_TIME="en_GB.UTF-8"
export LC_PAPER="en_GB.UTF-8"
export LC_MEASUREMENT="en_GB.UTF-8"

## common settings
export PYTHONSTARTUP=~/.pythonrc
export PS1='[\u@\h] $(x="\w";echo "${x#${x%/*/*}/}") \$ '
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
