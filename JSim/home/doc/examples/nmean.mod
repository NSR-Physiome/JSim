// native function for geometric mean
//   requires C compiler, mylib library,  will not run in applet
native real function gmean(a, b) {
  language="C";
  library="mylib";
  name="mygmean";
}

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=1;
  real v(t) = gmean(t, t^3);
}
