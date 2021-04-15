/* Examples for testing */

true;
if false then true else false; 

0; 
iszero (succ true);
succ (succ 0);
succ (pred 0);
iszero (pred (succ (succ 1))); 
iszero (0);
switch 0 case0 1 case1 2 default false;
switch 0 case0 succ (succ 0) case1 1 default 1;
switch 0 case0 true case1 1 default 2;