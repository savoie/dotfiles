# Replaces fish_prompt
function fish_mode_prompt --description 'Displays the current mode'
	echo -n -s "$USER" @ (hostname) " ~"

	# if in vi mode
	if test "$fish_key_bindings" = "fish_vi_key_bindings"
		switch $fish_bind_mode
			case default
				echo "n"
			case insert
				echo ">"
			case replace-one
				echo "r"
			case visual
				echo "v"
		end
		set_color normal
		printf " "
	else
		echo ">"

	end
	echo " "
end
