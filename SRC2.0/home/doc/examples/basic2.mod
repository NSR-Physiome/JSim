// model with a domain
math main {
  realDomain t;   // time
  t.min=0;        //    starts at t=0
  t.max=3;        //    stops at t=3
  t.delta=0.5;    //    with time-step 0.5
  real u0, u(t);
  u0 = 10;        // initial value of u at t=t.min
  u = u0*exp(-t); // exponenential decay
}
