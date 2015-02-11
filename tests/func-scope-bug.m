// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

A := 1;
function F1(x)
   function F2(x)
      return 2*A*x;
   end function;

   function F3(x)
      return F2(x);
   end function;
   return F3(x);
end function;

assert F1(1) eq 2;
