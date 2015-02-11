// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

i := 0; j := 0; k := 0;
while i lt 10000 do
   j := 0;
   while j lt 10000 do
      k := k + j + 1;
      j +:= 1;
   end while;
   i +:= 1;
end while;
assert k eq 500050000000;
