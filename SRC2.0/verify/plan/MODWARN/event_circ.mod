// circ dependency in event

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=1;
  realState u(t);
  real v(t);
  when (t=t.min) {
     u = 1;
     v = u(t.max);
  }
  v:t = v;
  event (v>5) u = u+1;
}
