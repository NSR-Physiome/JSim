math main {
  realDomain t; t.min=0; t.max=1; t.ct=2;
  real a = 0;
  real x1(t);
  real x2(t);
  real x3(t);
  real x4(t);
  real a1(t) = a + 1 + t;
  real a2(t) = a + 2 + t;
  real a3(t) = a + 3 + t;
  real a4(t) = a + 4 + t;
  real delta(t) = (x1+x2+x3+x4)/4;
  (x1-a1)^2 = delta;
  (x2-a2)^2 = delta;
  (x3-a3)^2 = delta;
  (x4-a4)^2 = delta;
  real xmean = sum(delta@t)/t.ct;
}
