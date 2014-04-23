// C function for reversal of time series
//   requires installed C compiler,  will not work in applet
source procedure reverse(u@t; v@t) {
  language="C";
  maincode={{
    int i;
    int ct = u->grids[0].ct;
    for (i=0; i<ct; i++)
      v->realVal[i] = u->realVal[ct-1-i];
    JSIM_RETURN();
  }};
}

math main {
  realDomain t;
  t.min=0; t.max=6; t.delta=2;
  real u(t) = t*t;
  real v(t);
  reverse(u@t, v@t);
}
