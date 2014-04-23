// array reversal via java procedure
source procedure reverse(u@t; v@t) {
  language="java";
  maincode={{
    RegularGridData t = (RegularGridData) u.grid(0);
    for (int i=0; i<t.ct(); i++) {
      double uval = u.realVal(i);
      int inx = t.ct()-i-1;
      v.set(inx, uval);
    }
  }};
}

math main {
  realDomain t;
  t.min=0; t.max=6; t.delta=2;
  real u(t) = t*t;
  real v(t);
  reverse(u@t, v@t); 
}
