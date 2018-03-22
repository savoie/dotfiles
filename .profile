export PATH=${HOME}/.local/bin:${PATH}
export RANGER_LOAD_DEFAULT_RC=false

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    exec startx
fi
