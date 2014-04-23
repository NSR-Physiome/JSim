// native F&P example
//   requires installed C compiler, newlib library, will not run in applet
native real function cintegral(a@t) {
  language = "C";
  library = "newlib";
  name = "cintegral";
}

native procedure reverse(a@t; b@t) {
  language = "C";
  library = "newlib";
  name = "crev";
}

math main {
  realDomain t;
  t.min=0; t.max=4; t.delta=1;
  real u(t) = t^2;
  real v = cintegral(u@t);
  real w(t);
  reverse(u@t, w@t);
}
