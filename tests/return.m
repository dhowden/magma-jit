// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

function Test(x)
   return 1;
   print 1;
end function;

function IfThenElseReturnTest(x)
   if x eq 1 then
      return 0;
   else
      return 1;
   end if;
end function;
assert IfThenElseReturnTest(1) eq 0;
assert IfThenElseReturnTest(2) eq 1;
assert IfThenElseReturnTest(3) eq 1;

function IfReturnTest(x)
   if 1 eq x then
      return 0;
   end if;
   return 1;
end function;
assert IfReturnTest(0) eq 1;
assert IfReturnTest(1) eq 0;

function WhileReturnTest(x)
   i := 0;
   while i lt 10 do
      return 0;
   end while;
   return 1;
end function;
assert WhileReturnTest(0) eq 0;

function ForReturnTest(x)
   for i in [1..10] do
      return 0;
   end for;
   return 1;
end function;
assert ForReturnTest(0) eq 0;
