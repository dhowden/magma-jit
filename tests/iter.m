// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// for testing scoping of the loop variable
i := 123;

x := 0;
for i in [1..10] do
   x +:= 1;
end for;
assert x eq 10;
assert i eq 123;

x := 0;
for i in [1..10] do
   x +:= i;
end for;
assert x eq 55;
assert i eq 123;

x := 0;
for i in [1..10], j in [1..10] do
   x +:= 1;
end for;
assert x eq 100;
assert i eq 123;

x := 0;
for i in [1..100 by 2] do
   x +:= 1;
end for;
assert x eq 50;
assert i eq 123;

x := 0;
for i := 1 to 100 by 2 do
   x +:= 1;
end for;
assert x eq 50;

x := 0;
for i := 1 to 100 do
   x +:= 1;
end for;
assert x eq 100;
assert i eq 123;

start := 1;
last  := 100;
x := 0;
for i := start to last do
   x +:= 1;
end for;
assert x eq (last - start + 1);

// Testing creating variables inside loop environments
for i in [1..10] do
   a := i;
end for;
assert a eq 10;

for i in [1..10] do
   b := i;
   break;
end for;
assert b eq 1;

for i in [1..10] do
   for j in [1..10] do
      c := j;
   end for;
end for;
assert c eq 10;

s := [1,2,3];
for i in s do
   print i;
end for;

t := [true, false, true];
for b in t do
   print b;
end for;

x := 0;
s := [];
for i in s do
   x := 10;
end for;
assert x eq 0;

x := 0;
for i in [1..0] do
   x := 1;
end for;
assert x eq 0;
