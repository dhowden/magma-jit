// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

b := false;
if true then
   b := true;
end if;
assert b;

b := true;
if false then
   b := false;
end if;
assert b;

b := false;
if true then
   b := true;
else
   b := false;
end if;
assert b;
