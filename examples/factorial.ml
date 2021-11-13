fun f x = if eq(x,0) then 1 else mul(x,f sub(x,1));

val x = f 10;
