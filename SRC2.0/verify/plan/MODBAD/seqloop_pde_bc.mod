// seqloops error: PDE BC
math main {
  realDomain t, x, y;
  t.min=0; t.max=4; t.delta=1;
  x.min=0; x.max=1; x.delta=.1;
  y.min=0; y.max=1; y.delta=.1;
  real u(t, x);
  real a(y) = y^2;
  when (t=t.min) { u=x; }
  when (x=x.min) { u+u:x=a; }
  when (x=x.max) { u:x = 0; }
  u:t = u:x:x;
}
