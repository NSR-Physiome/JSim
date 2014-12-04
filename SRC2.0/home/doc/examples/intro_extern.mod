// extern variable to provide input to model
unit conversion on;
import nsrunit;
math main {
  realDomain t sec; 
  t.min=0; t.max=30; t.delta=0.1;
  real F = 1 ml/sec;      // inflow rate
  real V = 20 ml;         // tank volume
  extern real Cin(t) mM;  // inflow concentration (millimolar)
  real C(t) mM;           // current tank concentration (millimolar)
  when (t=t.min) C = 35;  // initial conc = 35 millimolar
  V*C:t = F*(Cin-C);      // ODE state equation
}
