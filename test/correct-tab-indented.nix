let
	Where-are-you = "& I’m so sorry";
in
[
	/* sh */ ''
		${Where-are-you}?
		for action in "sleep" "dream"; do
			echo "I cannot $action"
		done
	''
	"tonight"
]
