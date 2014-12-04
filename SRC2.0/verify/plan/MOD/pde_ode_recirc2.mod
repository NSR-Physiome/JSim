// ODE/PDE tanks with recirculation - 2 stages

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real uIn(t) = 1 - exp(-t);
  real u1Out(t), u2Out(t);
  real u1(t,x), v1(t);
  real u2(t,x), v2(t);
  when (t=t.min) { u1=x; v1=0; u2=x; v2=0; }
  when (x=x.min) { u1 = uIn + v1; u2 = v2; }
  when (x=x.max) {
    u1:x=0; u2:x=0;
    u1Out = u1; u2Out = u2;
  }
  u1:t = u1:x:x - u1:x;
  v1:t = u1Out - v1;
  u2:t = u2:x:x - u2:x;
  v2:t = u2Out - v2;
}

