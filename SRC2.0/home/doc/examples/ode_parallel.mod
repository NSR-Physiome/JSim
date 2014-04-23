// flow through parallel compartments
unit conversion on;
import nsrunit;
math comp3 {
  realDomain t sec;     // time
  t.min=0; t.max=30; t.delta=0.2;
  real N=5;             // # compartments
  realDomain n;         // index of parallel compartments
  n.min=1; n.max=N; n.delta=1; // n values slaved to N
  private n.min, n.max, n.delta; // prevent user alterations
  real F = .1 ml/sec;      // flow rate
  real V(n)=n * (.07 ml); // compartment volumes
  real Cin(t) = (5 mM) * sin(t/(1 sec)); // input concentration
  real C(t, n) mM;        // conc in compartments 1-N
  when (t=t.min) C = 0;   // initial concentrations
  C:t = F/V*(Cin - C);    // state equation(s)
  real Cout(t) = sum((C*V)@n)/sum(V@n); // output concentration
}

