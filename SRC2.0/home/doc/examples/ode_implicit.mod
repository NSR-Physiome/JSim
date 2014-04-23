// ODEs with implicit IC and state equations
unit conversion on;
import nsrunit;
math ode_implicit {
  realDomain t sec; 
  t.min=0; t.max=10; t.delta=0.1;
  real u(t) m, v(t) m;
  when (t=t.min) {
    u+v = 3;
    u-v = 1;
  }
  u:t + v:t = (3 m/sec^2) * t;
  u:t - 2*v:t = 0;
}

