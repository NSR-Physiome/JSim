// ode muVar E rejected due to extra domain loop

math main {
  realDomain t, n;
  t.min=0; t.max=1; t.delta=.5;
  n.min=1; n.max=2; n.delta=1;
  
  real E(t,n) = t+n;
  real u(t);
  when (t=t.min) u=0;
  u:t = sum(E@n);
}
