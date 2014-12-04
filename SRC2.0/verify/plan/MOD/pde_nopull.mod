// PDE B should not be pulled, was in earlier vers.

math main { 
  realDomain t; t.min=0; t.max=30; t.delta=0.2;
  realDomain x1; x1.min =0; x1.max =0.1; x1.ct=2;
  realDomain x;  x.min=0.1; x.max= 0.2; x.ct=2;
 
  real A(t,x1), B(t, x), Aout(t);
  when (t=t.min) { A = 0; B = Aout; }     
  when (x1=x1.min) A:x1 =0;
  when (x1=x1.max) { A:x1 = 0; Aout = A; }
  when (x=x.min) B:x=0;
  when (x=x.max) B:x=0;
  A:t = A:x1:x1;
  B:t = B:x:x;

  real C(t,x);
  when (t=t.min) C=0; 
  when (x=x.min) C:x=0;
  when (x=x.max) C:x=0;
  C:t = C:x:x;             

}
