// vfc MAX used indirectly in ODE - should fail
//   currently fails, uninformatively, in toolbox
//   better DETool would postpone to sequencing, with more user info

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=.1;
  real u(t), v(t); 
  when (t=t.min) u=1;
  u:t = v; 
  v(t) = u(t.max);
}
