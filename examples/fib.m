function fib(x)
   if x le 2 then
      return 1;
   end if;
   return fib(x - 1) + fib(x - 2);
end function;

n := 0;
time for i in [1..40] do
   n := n + fib(i);
end for;

function iter_fib(x)
   if x le 2 then return 1; end if;
   a := 1;
   b := 1;
   while x gt 2 do
      c := a + b;
      b := a;
      a := c;
      x := x - 1;
   end while;
   return c;
end function;

time for i in [1..92] do
   print iter_fib(i);
end for;

print "Done.";
