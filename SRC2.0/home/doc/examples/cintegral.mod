// C function implementation for 1D integral
//   requires installed C compiler,  will not run in applet
source real function cintegral(a@t) {
  language = "C";
  maincode={{
    int ct, i, mult;
    double tot = 0;
    JSimGrid *t = a->grids;
    ct = t->ct;
    for (i=0; i<ct; i++) {
      mult = (i==0 || i==(ct-1)) ? 1 : 2;
      tot += a->realVal[i]*mult;
    }
    tot *= (t->max - t->min) / (2 * (t->ct - 1));
    JSIM_RETURN(tot);
  }};
}

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=1;
  real u(t) = t*t;
  real v = cintegral(u@t);
}

