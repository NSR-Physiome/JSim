// 2nd order ODE: u:t:t = -u
unit conversion on;
import nsrunit;
math order2 {
  realDomain t sec;     // time
  t.min=0; t.max=30; t.delta=0.2;
  real u(t) m;            // 
  real u_t(t) m/sec;
  when (t=t.min) { 
    u = 0;
    u_t = 1;
  }
  u:t = u_t;
  u_t:t = (-1 1/sec^2) * u;
}

