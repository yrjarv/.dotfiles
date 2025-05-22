#
# ~/.bash_profile
#

# When on UiO servers, autostart zsh
# (because I don't have access to chsh)
if [[
    $(cat /etc/hostname) == *uio* &&
    -z "$ZSH_ALREADY_EXECED" &&
	"$0" == "-bash"
  ]]; then
  export ZSH_ALREADY_EXECED=1
  exec zsh
fi

# Use .bashrc
[[ -f ~/.bashrc ]] && . ~/.bashrc
