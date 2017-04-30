# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi
if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
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
alias r="R"

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
setxkbmap -option caps:escape
