// 2 PDE advection-diffusion with recirculation w/ same space domain

math main {
  realDomain t, x;
  t.min=0; t.max=3; t.delta=.1;
  x.min=0; x.max=1; x.delta=.05;
  real Cin(t) = 1 - exp(-t);
  real u(t,x), v(t,x), uOut(t), vOut(t);
  when (t=t.min) { u=0; v=0; }
  when (x=x.min) {
    u = Cin + vOut;
    v = uOut;
  }
  when (x=x.max) { 
     u:x=0; v:x=0; 
     uOut = u;
     vOut = v;
  }
  u:t = u:x:x - u:x;
  v:t = v:x:x - v:x;
}

