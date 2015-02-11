// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

procedure Proc1(x)
   assert x eq 1;
end procedure;
Proc1(1);

a := 1;
procedure Proc2(~x)
   assert x eq 1;
   x := 5;
end procedure;
Proc2(~a);
assert a eq 5;

a := 1;
b := 2;
procedure Proc3(~x, y)
   x := y;
   y := 3;
end procedure;
Proc3(~a, b);
assert a eq b;

a := 1;
b := 2;
procedure Proc4(~x, ~y)
   x := y;
   y := 3;
end procedure;
Proc4(~a, ~b);
assert a eq 2;
assert b eq 3;

procedure Proc5(~x)
   function F(a)
      x := a;
      return 0;
   end function;
   F(2);
end procedure;
a := 1;
Proc5(~a);
assert a eq 1;
