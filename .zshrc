# Plugins
export ZSH=/home/celine/.oh-my-zsh

plugins=(
  git
  zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd
unsetopt beep
# End of lines configured by zsh-newuser-install

#source /usr/share/doc/pkgfile/command-not-found.zsh

source ${HOME}/.cache/wal/colors.sh

# vi mode
function set_prompt() {
  #echo "${${KEYMAP/vicmd/[% NORMAL]%}/(main|viins)/[% INSERT]%}"
    case $KEYMAP in
	"vicmd")
	    vi_mode_prompt="n"
	    ;;
	*)
	    vi_mode_prompt=">"
	    ;;
    esac
    PROMPT="%F{3}%~ %F{2}${vi_mode_prompt} "
}


# Updates prompt when keymap changes
function zle-keymap-select() {
    set_prompt
    zle reset-prompt
    zle -R
}

bindkey -v
zle -N zle-keymap-select
set_prompt

export KEYTIMEOUT=1
bindkey "jk" vi-cmd-mode
bindkey "kj" vi-cmd-mode

# Integrate system clipboard with vi-mode commands
# From https://github.com/robbyrussell/oh-my-zsh/pull/3616/commits/0f6e49b455e498bd051d1d18d62dec4e6872d3e8
[[ -n $DISPLAY ]] && (( $+commands[xclip] )) && {
  function cutbuffer() {
    zle .$WIDGET
    echo $CUTBUFFER | xclip -selection clipboard
  }

  zle_cut_widgets=(
    vi-backward-delete-char
    vi-change
    vi-change-eol
    vi-change-whole-line
    vi-delete
    vi-delete-char
    vi-kill-eol
    vi-substitute
    vi-yank
    vi-yank-eol
  )

  for widget in $zle_cut_widgets; do
    zle -N $widget cutbuffer
  done

  function putbuffer() {
    zle copy-region-as-kill "$(xclip -o)"
    zle .$WIDGET
  }

  zle_put_widgets=(
    vi-put-after
    vi-put-before
  )

  for widget in $zle_put_widgets; do
    zle -N $widget putbuffer
  done
}

# aliases
alias .git='git --git-dir=${HOME}/.gitdots/ --work-tree=${HOME}'
alias emacs='emacsclient -c'
alias sxlock='sxlock -f "-misc-fantasque sans mono-medium-r-normal--0-100-0-0-M-0-*-*"'
