// event on 2D realState

math main {
  realDomain t, x;
  t.min=0; t.max=10; t.delta=1;
  x.min=0; x.max=1; x.delta=.1;
  realState r(t,x);
  when(t=t.min) r=1;
  event(t>5) r=r+1;
}
