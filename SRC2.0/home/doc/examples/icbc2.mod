// IC/BC consistency via modified IC
math icbc2 {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=0.25;
  x.min=0; x.max=1; x.delta=0.1;
  real u(t,x);
  extern real uin(t); 
  when (t=t.min) u = if (x=x.min) uin else 0; 
  when (x=x.min) u = uin; 
  when (x=x.max) u:x = 0;
  u:t = u:x:x - u:x;
}
