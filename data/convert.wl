input := 600500553290;
tobase := 2;

output := 0;
position := 1;
shift := 10;
while (input > 0) do (
	remain := input - ((input / tobase) * tobase);
	output := output + (remain * position);
	position := position * shift;
	input := input / tobase;
)
