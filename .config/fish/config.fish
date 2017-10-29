# Refresh wal on new shells
eval {$HOME}/.local/bin/wal -rt &

# Set dotfiles version control shortcut
function .git
	/usr/bin/git --git-dir=$HOME/.gitdots/ --work-tree=$HOME $argv
end

# Start vi mode
fish_vi_key_bindings
