// event - action vars has different domains

math main {
  realDomain t, x;
  t.min=0; t.max=10; t.delta=1;
  x.min=1; x.max=1; x.delta=.2;
  real u(t) = t;
  real v(t,x) = t+x;
  event(t>5) {
      u=u+1;
      v=v+1;
  }
}
