// PDE ICs entangled: querytool A(t.min, x.max) vreq s/b A(t.min), not A

math liver { 
  realDomain t;  t.min=0; t.max=30; t.delta=0.05;
  realDomain x; x.min=0; x.max=1; x.ct=2;  
  realDomain x2; x2.min=0; x2.max=1; x2.ct=2;    

  real A(t,x), Aout(t), B(t,x2); 

  when (t=t.min) A = 0;
  when (x=x.min) A:x = 0;
  when (x=x.max) A:x = 0; 
  A:t = A:x:x + B(t,x2.max);

  when  (x=x.max) Aout = A; 

  when (t=t.min)   B = Aout;
  when (x2=x2.min) B:x2 = 0;
  when (x2=x2.max) B:x2 = 0; 
  B:t = B:x2:x2;
}
