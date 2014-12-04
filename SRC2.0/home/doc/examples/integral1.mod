// 2 examples of integral() operator
unit conversion on;
import nsrunit;
math integral1 {
  realDomain t sec; t.min=0; t.max=5; t.delta=1; // time (seconds)
  real u(t) = t^2 * (1 m/sec^2);  // u (meters)
  real v(t) = integral(t=t.min to t, u); // v (meter*sec)
  real w = integral(t=t.min to t.max, u); // w (meter*sec)
}
