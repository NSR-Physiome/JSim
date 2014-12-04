// Intertwined ODE & fzero caused abort during ODE export(tf)
//   message: Variables {} exceeded specified zero-finder error: (NaN>1.0E-6)
//   bug fixed for JSim 2.12 and above
 
math transporter {

realDomain t; t.min=0; t.max=0.1; t.delta=0.01;

real A = 1;

real B(t) = A;

real C(t), D(t);

when (t=t.min) C = 1;
C:t = -D;

D^2 = B * C;
D >= 0;
D <= 2;
}
