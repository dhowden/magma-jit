// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/* plain break */
x := 0;
for i in [1..100] do
    x := i;
   if i eq 10 then
      break;
      assert false;
   end if;
end for;
assert x eq 10;

/* test break i; */
y := 0;
for i in [1..10] do
   x := i;
   for j in [1..10] do
      y := j;
      if j eq 2 then
         break i;
         assert false;
      end if;
   end for;
end for;
assert x eq 1;
assert y eq 2;

i := 0;
while i lt 10 do
   if i eq 5 then
      break;
      assert false;
   end if;
   i := i + 1;
end while;
assert i eq 5;

i := 0;
repeat
   if i eq 5 then
      break;
      assert false;
   end if;
   i := i + 1;
   break;
   assert false;
until i eq 10;
assert i eq 1;
