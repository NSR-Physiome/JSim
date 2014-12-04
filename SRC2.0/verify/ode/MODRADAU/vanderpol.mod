// VAN DER POL'S EQUATION
// Suggest you plot y1 vs y2, fails for all solvers except radau
// Making t.delta 1e-05 causes radau to fail also.

math main { 
  realDomain t; t.min=0.0; t.max=2.0; t.delta=0.002;
  real rpar = 1e-06;
  real y1(t), y2(t);
  when (t=t.min) {y1=2.0; y2=-0.66;}
  y1:t=y2;
  y2:t=((1-y1^2)*y2-y1)/rpar;
}

