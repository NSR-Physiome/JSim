// ODE with codependent ExprTool var

math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=.2;
  real u(t), v(t);
  when (t=t.min) u=1;
  u:t = u+v;
  v = u+1;
}
