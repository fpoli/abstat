// input start;
findafter := 1000000 * 1000000;

current := findafter;
found := 0;
while (found == 0) do (
	isdivisible := 0;
	
	// Find divisor
	divisor := 2;
	while (divisor <= current/divisor && isdivisible == 0) do (
		if ((current/divisor)*divisor == current) then (
			isdivisible := 1;
		) else skip;
		divisor := divisor + 1;
	);

	if (isdivisible == 0 && current != 1) then (
		found := 1;
		firstprime := current;
	) else skip;

	current := current + 1;
)

// print firstprime;
