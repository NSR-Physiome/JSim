// geometric mean via java function
source real function gmean(a, b) {
  language="java";
  maincode={{
    double aval = a.realVal();
    double bval = b.realVal();
    return Math.sqrt(aval*bval);
  }};
}

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=1;
  real v(t) = gmean(t, t^3);
}
