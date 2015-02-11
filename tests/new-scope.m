// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

i := 123;
x := 0;
for i in [1..10] do
   x +:= i;
end for;
assert x eq 55;
assert i eq 123;

i := 0;
function Test(x)
   i := 10;
   return x + i;
end function;
assert Test(1) eq 11;
assert i eq 0;

i := 0;
function Test1(i)
	function TestTest1(i)
		i +:= 1;
		return i;
	end function;
	return TestTest1(i);
end function;
assert Test1(1) eq 2;

// A := 1; B := 2;
// function X(a)
// 	C := 3;
// 	function Y(b)
// 		return A + C + b;
// 	end function;
// 	return B + Y(a);
// end function;
// assert X(1) eq 7;

A := 1;
function X(a)
	a +:= A;
	// A := 10; // this should give an error
	return a;
end function;
