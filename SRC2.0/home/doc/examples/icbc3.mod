// IC/BC consistency via modified BC
math icbc3 {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=0.25;
  x.min=0; x.max=1; x.delta=0.1;
  real u(t,x);
  extern real uin(t); 
  when (t=t.min) u = 0; 
  when (x=x.min) u = if (t=t.min) 0 else uin; 
  when (x=x.max) u:x = 0;
  u:t = u:x:x - u:x;
}
