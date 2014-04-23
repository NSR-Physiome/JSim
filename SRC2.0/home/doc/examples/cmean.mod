// C source function computing geometric mean
//   note: requires C compiler installed, will not run in applet
source real function gmean(a, b) {
  language="C";
  maincode={{
    double aval = a->realVal[0];
    double bval = b->realVal[0];
    JSIM_RETURN(sqrt(aval*bval));
  }};
}

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=1;
  real v(t) = gmean(t, t^3);
}
