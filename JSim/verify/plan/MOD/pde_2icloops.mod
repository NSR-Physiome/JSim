// PDEs with different IC loops

math fpxmtp {
  realDomain t; t.min=0; t.max=10; t.delta=0.01;
  realDomain x; x.min=0; x.max=0.1; x.ct=31;

  real w(t,x), wout(t);
  when (t=t.min) w=0;
  when (x=x.min) w:x=0;
  when (x=x.max) {w:x=0; wout=w;}
  w:t=w:x:x;

  real v(t,x), vout(t);
  when(t=t.min)  v=t;
  when (x=x.min) v:x=0;
  when (x=x.max) {v:x=0; vout=v;}
  v:t=v:x:x;
}
