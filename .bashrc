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
#export LANG="en_US.UTF-8"
#export LC_TIME="en_GB.UTF-8"
#export LC_PAPER="en_GB.UTF-8"
#export LC_MEASUREMENT="en_GB.UTF-8"

## common settings
export PYTHONSTARTUP=~/.pythonrc
export PS1='[\u@\h] $(x="\w";echo "${x#${x%/*/*}/}") \$ '
export HISTCONTROL=ignoredups

alias ll="env LANG=POSIX ls -al --color " # --group-directories-first
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"
alias psx="ps -auxw | grep $1" 
mkcd() { mkdir -p "$1" && cd "$1"; }
