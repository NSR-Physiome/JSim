math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  
  real u(t) = t^2;
  real delay = 1;
  real v(t) = if (t < delay) u(t.min) else u(t-delay);
}


