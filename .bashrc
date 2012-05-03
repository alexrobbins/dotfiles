# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

source /main/scripts/env-common.sh
PATH=./bin:./script:./target/bin:~/bin:~/go/bin:$PATH

alias ssh="ssh -X"
alias mm="mm --local --notify"
alias ls="ls --color=auto"
alias ll="ls -l"

set -o vi

alias gvim="UBUNTU_MENUPROXY= gvim"

export PS1="\[\033[G\]$PS1"

PATH=$HOME/.rvm/bin:$PATH # Add RVM to PATH for scripting
source ~/.rvm/scripts/rvm
