# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

PATH=~/bin:~/go/bin:$PATH

alias ls="ls --color=auto"
alias ll="ls -l"

set -o vi
export EDITOR=vim

alias gvim="UBUNTU_MENUPROXY= gvim"

export PS1="\[\033[G\]$PS1"

alias tmux="TERM=screen-256color tmux"
alias hdfs="hadoop fs"
alias hdsf="hdfs"
