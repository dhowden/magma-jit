// Copyright 2015, David Howden
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// basic construction and testing
seq1 := [1,2,3];
assert #seq1 eq 3;
assert seq1[1] eq 1;
assert seq1[2] eq 2;
assert seq1[3] eq 3;

seq1[3] := 4;
assert seq1[3] eq 4;

seq1[3] := 3;
assert seq1[3] eq 3;

seq1[4] := 4;
assert #seq1 eq 4;
assert seq1[4] eq 4;

a := 10;
for i in [1..a] do
   a := 100;
   b := i;
end for;
assert b eq 100;

// Test some printing
print [];

seqseq := [ [1], [2,3], [4,5,6], [7,8,9] ];
print seqseq;

s := seqseq[1];
s[1] := 7;
print seqseq;

// seqseq[1][1] := 8;
// print seqseq;

for i in [1..#seqseq] do
   seqseq_elt := seqseq[i];
   for j in [1..#seqseq_elt] do
      print seqseq_elt[j];
   end for;
end for;

for i in [1..#seqseq] do
    for j in [1..#seqseq[i]] do
        print seqseq[i][j];
    end for;
end for;

seq3 := [ false, true, true ];
for i in [1..#seq3] do
   print seq3[i];
end for;

seq4 := [ "shot", "in", "the", "dark" ];
print seq4;
for i in [1..#seq4] do
   print seq4[i];
end for;
