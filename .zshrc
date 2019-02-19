source ${HOME}/.profile
source ${HOME}/.aliases

# Plugins
export ZSH=/home/celine/.oh-my-zsh

plugins=(
  git
  zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory autocd
unsetopt beep

source ${HOME}/.cache/wal/colors.sh

autoload -U promptinit
promptinit
prompt spaceship

SPACESHIP_PROMPT_ORDER=(
  time          # Time stamps section
  user          # Username section
  dir           # Current directory section
  host          # Hostname section
  char          # Prompt character
)

SPACESHIP_RPROMPT_ORDER=(
  vi_mode       # Vi-mode indicator
  git           # Git section (git_branch + git_status)
  hg            # Mercurial section (hg_branch  + hg_status)
  package       # Package version
  node          # Node.js section
  ruby          # Ruby section
  elixir        # Elixir section
  xcode         # Xcode section
  swift         # Swift section
  golang        # Go section
  php           # PHP section
  rust          # Rust section
  haskell       # Haskell Stack section
  julia         # Julia section
  docker        # Docker section
  aws           # Amazon Web Services section
  venv          # virtualenv section
  conda         # conda virtualenv section
  pyenv         # Pyenv section
  dotnet        # .NET section
  ember         # Ember.js section
  kubecontext   # Kubectl context section
  terraform     # Terraform workspace section
  exec_time     # Execution time
  battery       # Battery level and status
  jobs          # Background jobs indicator
  exit_code     # Exit code section
)

export SPACESHIP_PROMPT_ADD_NEWLINE="false"
export SPACESHIP_PROMPT_SEPARATE_LINE="false"

export SPACESHIP_CHAR_SYMBOL=" "
export SPACESHIP_CHAR_SYMBOL_SECONDARY=" "
export SPACESHIP_CHAR_SYMBOL_ROOT=" "

export SPACESHIP_DIR_TRUNC="0"
export SPACESHIP_DIR_TRUNC_REPO="true"
export SPACESHIP_DIR_TRUNC_PREFIX=" "

export SPACESHIP_GIT_PREFIX=""
export SPACESHIP_GIT_STATUS_PREFIX=" "
export SPACESHIP_GIT_STATUS_SUFFIX=""

export SPACESHIP_BATTERY_SHOW="true"
export SPACESHIP_BATTERY_SYMBOL_CHARGING=""
export SPACESHIP_BATTERY_SYMBOL_DISCHARGING=""

export SPACESHIP_VI_MODE_INSERT=""
export SPACESHIP_VI_MODE_NORMAL="N"
export SPACESHIP_VI_MODE_SHOW=true
spaceship_vi_mode_enable

bindkey -v
export KEYTIMEOUT=1

bindkey "jk" vi-cmd-mode
bindkey "kj" vi-cmd-mode

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line

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
