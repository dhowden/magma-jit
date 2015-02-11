// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

assert true;
assert not false;

assert true eq true;
assert false eq false;
assert not (true eq false);
assert not false eq true;

assert true or true;
assert true or false;
assert false or true;
assert not (false or false);
assert false or false eq false;

assert not true or true;
assert not false or false;

assert not true and true eq false;
assert false and false eq false;

assert true or false;
assert true and false eq false;

assert not (true and false);

assert 1+1 eq 2;
assert 2*3 eq 6;
assert 1+2*3 eq 7;
assert (1+2)*3 eq 9;

assert 1 mod 2 eq 1;
assert 7 mod 5 eq 2;

assert 7 div 2 eq 3;
assert 2 div 2 eq 1;

assert 1 + 7 div 7 eq 2;
assert (1 + 7) div 8 eq 1;

x := 1;
y := 7;
z := 2;
t := 8;
assert x + y div y eq z;
assert (x + y) div t eq x;

assert 2 gt 1;
assert 10 * 4 gt 5;
assert 60 div 15 lt 5;

assert 1 - 2 eq -1;
assert -1 + 1 eq 0;
assert -1 - 1 eq -2;
assert -1 + -1 eq -2;
assert -1 - -1 eq 0;

assert -1 * -1 eq 1;
assert -1 * 1 eq -1;
assert -1 * -1 * -1 eq -1;

assert -1 div -1 eq 1;
assert -1 div 1 eq -1;
// assert 1 div -1 eq -1;  // fails here, but works in magma :-(

a := true;
a and:= true;
assert a;

a := true;
a and:= false;
assert not a;

a := false;
a and:= true;
assert not a;

a := false;
a and:= false;
assert not a;

a := true;
a or:= true;
assert a;

a := true;
a or:= false;
assert a;

a := false;
a or:= true;
assert a;

a := false;
a or:= false;
assert not a;

a := true;
a xor:= true;
assert not a;

a := true;
a xor:= false;
assert a;

a := false;
a xor:= true;
assert a;

a := false;
a xor:= false;
assert not a;
