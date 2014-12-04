math main {
  realDomain n; n.min=0; n.max=1; n.ct=2;
  real a1(n);
  real a2(n);
  real a3(n);
  real a4(n);
  real a5(n);
  real k = 1;
  real t1 = 1*0.5;
  real t2 = 2*0.5;
  real t3 = 3*0.5;
  real t4 = 4*0.5;
  real t5 = 5*0.5;
  real delta = 0;
  a1 + a2*t1^(2-1) + a3*t1^(3-1) + a4*t1^(4-1) + a5*t1^(5-1) = exp(-k*t1) + delta;
  a1 + a2*t2^(2-1) + a3*t2^(3-1) + a4*t2^(4-1) + a5*t2^(5-1) = exp(-k*t2) + delta;
  a1 + a2*t3^(2-1) + a3*t3^(3-1) + a4*t3^(4-1) + a5*t3^(5-1) = exp(-k*t3) + delta;
  a1 + a2*t4^(2-1) + a3*t4^(3-1) + a4*t4^(4-1) + a5*t4^(5-1) = exp(-k*t4) + delta;
  a1 + a2*t5^(2-1) + a3*t5^(3-1) + a4*t5^(4-1) + a5*t5^(5-1) = exp(-k*t5) + delta;
  real a1avg = sum(a1@n)/n.ct;
  real a2avg = sum(a2@n)/n.ct;
}
