math main {
  realDomain t;
  t.min=1; t.max=10; t.delta=1;
  realState u(t);
  when (t=t.min) u=0;
  event (sin(t*PI/3)^2<.1) {
     u = u+1;
  }
}
