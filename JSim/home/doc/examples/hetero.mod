// heterogeneous methods
math hetero {
  realDomain t;
  t.min=0; t.max=10; t.delta=1;
  real a(t), b, c(t);
  extern real d(t);     // constrains d 
  a = c^2;              // constrains  a
  when (t=t.min) c=1;   // initial condition for c 
  when (t=t.max) b=c+a; // constrains b
  c:t = a-c+d;          // state equation for c 
}
