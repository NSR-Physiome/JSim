// 3 compartment diffusion
unit conversion on;
import nsrunit;
math comp3 {
  realDomain t sec;     // time
  t.min=0; t.max=30; t.delta=0.2;
  real V=.07 ml;        // compartment volumes
  real PS1=.05 ml/sec;  // perm * surf area between compartments 1 & 2
  real PS2=.02 ml/sec;  // perm * surf area between compartments 2 & 3
  real C1(t) mM, C2(t) mM, C3(t) mM;       // conc in compartments 1,2,3
  when (t=t.min) { C1=5; C2=3; C3=0; }     // initial concentrations
  C1:t = PS1/V*(C2 - C1);                  // C1 state eqn
  C2:t = PS1/V*(C1 - C2) + PS2/V*(C3-C2);  // C2 state eqn
  C3:t = PS2/V*(C2 - C3);                  // C3 state eqn
}

