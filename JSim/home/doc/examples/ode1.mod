// radioactive decay
unit conversion on;
import nsrunit;
math main {
  realDomain t sec;
  t.min=0; t.max=4; t.delta=0.1;
  real rate = 1 1/sec;
  real u(t) kg;        // ODE variable declaration
  when (t=t.min) u=1;  // initial condition for u
  u:t = -rate*u;       // state equation for u
}
