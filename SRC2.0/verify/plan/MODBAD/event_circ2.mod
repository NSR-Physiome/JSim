// circ dep in event

math main {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real a(t) = t^2;
  real u(t) = a+1;
  event (t>5) a = a + u;
}

