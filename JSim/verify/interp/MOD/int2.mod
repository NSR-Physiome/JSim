// artificial 2D interpolation test
// this assumes interpolation "nearly zero" threshold is 1e-7
// and samples interpolated values that should fall within
// the "nearly zero" range
// correct behaviour will return no NaNs in v,  with u=v

math main {
   realDomain t,x ;
   t.min=0; t.max=2*PI; t.ct=31;
   x.min=0; x.max=PI; x.ct=37;
   real del = 1e-16;
   
   real u(x,t) = sin(x+t);
   real v(x,t) = u(x+del, t+del);
}
