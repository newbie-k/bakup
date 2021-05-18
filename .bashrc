#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

alias la='ls --color=auto -a'
alias ll='ls --color=auto -al'
alias line.exe='env WINEPREFIX="/home/krsakr/.wine" wine C:\\windows\\command\\start.exe /Unix /home/krsakr/.wine/dosdevices/c:/users/krsakr/Start\ Menu/Programs/LINE/LINE.lnk'

#PS1='\e[35m[\u@\h \W]\$\e[0m '
