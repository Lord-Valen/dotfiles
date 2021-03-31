#!/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Disable ctrl-s and ctrl-q.
stty -ixon

# Allows you to cd into directory merely by typing the directory name.
#shopt -s autocd

# Sets shell to vi mode for vim keys.
set -o vi
export EDITOR='vi'
export VISUAL='vi'

# Infinite history.
HISTSIZE=10000 HISTFILESIZE=

# PS1 style.
export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

# Load shortcut aliases
[ -f "$HOME/.config/bash/shortcutrc" ] && source "$HOME/.config/bash/shortcutrc"
[ -f "$HOME/.config/bash/aliasrc" ] && source "$HOME/.config/bash/aliasrc"

#eval $(dircolors -b $HOME/.dircolors)
alias ls='ls --color'
LS_COLORS='di=1;35:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:*.rpm=90:*.png=35:*.gif=36:*.jpg=35:*.c=92:*.jar=33:*.py=93:*.h=90:*.txt=94:*.doc=104:*.docx=104:*.odt=104:*.csv=102:*.xlsx=102:*.xlsm=102:*.rb=31:*.cpp=92:*.sh=92:*.html=96:*.zip=4;33:*.tar.gz=4;33:*.mp4=105:*.mp3=106'
export LS_COLORS
