from := 1000;

// cateto lungo
a := from;

found := 0;
while (found == 0) do (
	// cateto corto
	b := from;

	while (found == 0 && b <= a) do (
		// ipotenusa
		c := a;

		while (found == 0 && c < a+b) do (
			if (a*a + b*b == c*c) then
				found := 1;
			else
				c := c + 1;
		);

		if (found == 0) then
			b := b + 1;
		else skip;
	);

	if (found == 0) then
		a := a + 1;
	else skip;
);

// print a, b, c
