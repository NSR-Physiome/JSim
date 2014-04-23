// 2 PDE advection-diffusion with recirculation, diff space domains

math main {
  realDomain t, x, y;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  y.min=0; y.max=1; y.delta=.1;
  real Cin(t) = 1 - exp(-t);
  real u(t,x), v(t,y), uOut(t), vOut(t);
  when (t=t.min) { u=0; v=0; }
  when (x=x.min) u = Cin + vOut;
  when (y=y.min) v = uOut;
  when (x=x.max) {
     u:x=0; 
     uOut = u;
  }
  when (y=y.max) {
     v:y=0; 
     vOut = v;
  }
  u:t = u:x:x - u:x;
  v:t = v:y:y - v:y;
}

