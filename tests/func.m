// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Basic tests for functions

// Function return types
function BooleanTrue::BoolElt()
    return true;
end function;
assert BooleanTrue() eq true;

function Int1::RngIntElt()
    return 1;
end function;
assert Int1() eq 1;

function SeqEnumBool::SeqEnum[BoolElt]()
   return [true, false];
end function;
x := SeqEnumBool();
assert x[1];
assert not x[2];

function SeqEnumInt::SeqEnum[RngIntElt]()
   return [1, 2];
end function;
y := SeqEnumInt();
assert y[1] eq 1;
assert y[2] eq 2;

function Sum(X :: SeqEnum[RngIntElt])
   total := 0;
   for i in [1..#X] do
      total +:= X[i];
   end for;
   return total;
end function;
assert Sum([1,2,3]) eq 6;

function Sum2(X :: SeqEnum[RngIntElt])
   total := 0;
   for i in [1..#X] do
      total +:= X[i];
   end for;
   return total;
end function;
assert Sum2([-1,2,3]) eq 4;

// function NoChange(X :: SeqEnum[RngIntElt])
//    X[1] := 10;
//    return 0;
// end function;
// y := [1,2,3];
// NoChange(y);
// assert y[1] eq 1;

function fib(x)
   if x le 2 then
      return 1;
   end if;
   return fib(x - 1) + fib(x - 2);
end function;

function iter_fib(x)
   if x le 2 then return 1; end if;
   a := 1;
   b := 1;
   while x gt 2 do
      c := a + b;
      b := a;
      a := c;
      x := x - 1;
   end while;
   return c;
end function;
assert iter_fib(92) eq 7540113804746346429;

for i in [1..30] do
   assert iter_fib(i) eq fib(i);
end for;

