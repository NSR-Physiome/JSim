// event with 2 actions

math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  realState r(t);
  intState i(t);
  when (t=t.min) {
     r=1.5;
     i=2;
  }
  event (t>3) {
     r=r+1.5;
     i=i+1;
  }
}
