// F&P reentrant tag for multi-processing support
source real function square(a) {
  language="java";
  reentrant="true";
  maincode={{
    double aval = a.realVal();
    return aval*aval;
  }};
}

math reentrant {
  realDomain t;
  t.min=0; t.max=5; t.delta=1;
  real v(t) = square(t);
}
