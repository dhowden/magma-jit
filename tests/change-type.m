// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

a := 1;
assert a eq 1;

a := true;
assert a;

a := [1,2,3];
assert #a eq 3;
assert a[1] eq 1;
assert a[2] eq 2;
assert a[3] eq 3;
