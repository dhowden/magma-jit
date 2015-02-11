// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

function Loop1 :: SeqEnum[RngIntElt] (X :: SeqEnum[BoolElt], t, f)
   result := [];
   for i in [1..#X] do
      if X[i] then
         result[i] := t;
      else
         result[i] := f;
      end if;
   end for;
   return result;
end function;

print Loop1([true, true, false], 10, 20);
