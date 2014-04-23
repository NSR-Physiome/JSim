// disjoint xedges needed to sequence

math test {
  realDomain t; t.min=0; t.max=30; t.delta=0.05;
  realDomain x; x.min=0; x.max=1; x.ct=5;    

  real A1(t) = t;
  real B1(x) = x;

  real A2(t) = B1(x.max);
  real B2(x) = A1(t.max); 
} 
