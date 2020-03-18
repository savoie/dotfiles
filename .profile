export PANEL_FIFO="/tmp/panel-fifo"

export PATH=${HOME}/.local/bin:${PATH}
export EDITOR='emacsclient -c --alternate-editor=""'
export RANGER_LOAD_DEFAULT_RC=false

export WORKON_HOME=${HOME}/.virtualenvs
export PROJECT_HOME=${HOME}/projects
source ${HOME}/.local/bin/virtualenvwrapper.sh

export SMLROOT=${HOME}/sml

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx &> ~/.Xoutput
fi
