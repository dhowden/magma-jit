// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

/* Tests for types */
function Size::RngIntElt (X :: SeqEnum[RngIntElt])
   return #X;
end function;
assert Size([1,2,3]) eq 3;