# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

source /main/scripts/env-common.sh
PATH=./bin:./target/bin:~/bin:~/go/bin:~/intellij/bin:$PATH

alias ssh="ssh -X"
alias mm="mm --local --notify"
alias ls="ls --color=auto"
alias ll="ls -l"

set -o vi

alias gvim="UBUNTU_MENUPROXY= gvim"

export TERM="xterm-256color"
