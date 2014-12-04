// ODE/PDE tanks with recirculation

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real uIn(t) = 1 - exp(-t);
  real uOut(t);
  real u(t,x), v(t);
  when (t=t.min) { u=x; v=0; }
  when (x=x.min) u = uIn + v; // solved v! wrong
  when (x=x.max) {
    u:x=0;
    uOut = u;
  }
  u:t = u:x:x - u:x;
  v:t = uOut - v;
}

