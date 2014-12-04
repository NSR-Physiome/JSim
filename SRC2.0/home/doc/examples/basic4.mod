// using "when" to extract final value
math main {
  realDomain t;   
  t.min=0; t.max=5; t.delta=0.1;
  real u(t) = exp(-t);
  real ufinal;
  when (t=t.max) ufinal = u;
}
