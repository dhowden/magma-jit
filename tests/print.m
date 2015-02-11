// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

print 1;
print true, false;
print "String";

print [1,2,3];
print #[1,2,3];
x := [4,5,6];
print x;
print #x;
print x[1];
print x[2];
print x[3];

// print <1,2,3>;
print #<1,2,3>;
y := <1,2,3>;
print y[1];
print y[2];
print y[3];

print 1, true, "String", [1,2,3], #[1,2,3], x, #x, x[1], x[2], x[3];

// Now we test the printf stuff
printf "Testing";
printf "%o", "Testing";
printf "\nTesting";
printf "%o\n", "\nTesting";
printf "\"Quoted Text\"\n";
printf "%%\n";
printf "1: %o\ntrue: %o\n[1,2,3]: %o\n", 1, true, [1,2,3];
