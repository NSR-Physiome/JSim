// event cascade: events depend on other events

math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  realState a(t);
  realState u(t);
  when (t=t.min) { a=1; u=2; }
  event (t>5) a = a + u;
  event (t>2) u = u+2;
  event (t>1) u = u+1;
}
