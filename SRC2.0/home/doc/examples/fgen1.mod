// function generator tutorial
math fgen1 {
  realDomain t; t.min=0; t.max=60; t,delta=0.1;
  extern real Cin(t);  // we'll assign a function generator to Cin
  real Cout(t) = Cin^2; // calculates the square of the input
}
