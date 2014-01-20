# -*- mode: sh -*-



## Colorize the ls output ##
alias ls='env LANG=POSIX ls --color=auto'

## Use a long listing format ##
alias ll='env LANG=POSIX ls -la --color --group-directories-first'

## Show hidden files ##
alias l.='env LANG=POSIX ls -d .* --color=auto'


alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"

## Colorize the grep command output for ease of use (good for log files)##
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

alias psx="ps auxw | grep $1"

alias mkdir='mkdir -pv'

alias mount='mount |column -t'

alias ports='netstat -tulanp'


# get web server headers #
alias header='curl -I'

if [ $TERM = "xterm" ]
then
    ## colorizer for Qt test output
    nrm=$(tput sgr0)
    red=$(tput bold;tput setaf 1)
    green=$(tput setaf 2)
    blue=$(tput bold;tput setaf 4)
    alias colorize_qtest="sed -e 's/^\(FAIL\|QWARN\|WARNING\).*/$red\\0$nrm/' -e 's/^\(PASS\|XPASS\|XFAIL\|SKIP\).*/$green\\0$nrm/' -e 's/^QDEBUG.*/$blue\\0$nrm/'"
    unset nrm red green blue
fi

