/* Examples for testing */

true;
if false then true else false; 

0; 
succ (pred 0);
iszero (pred (succ (succ 0))); 

if succ(0) then true else (if true then succ(2) else succ(3));