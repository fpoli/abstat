// Factorial calculator (of course).
n := 5;           // Number whose factorial will be compued.

tmp := n - 1;
factorial := n;
while tmp > 1 do(
	factorial := factorial * tmp;
	tmp := tmp - 1;
);

// print n;
// print factorial;
