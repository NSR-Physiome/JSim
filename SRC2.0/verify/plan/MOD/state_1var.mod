// event on realState

math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  realState r(t);
  when(t=t.min) r=1;
  event(t>5) r=r+1;
}
