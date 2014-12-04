// pull query passes 2 domains, check outer one for phase reentry

math CompNFlowDelay {
  realDomain t; t.min=0; t.max=30; t.delta=0.01;
  realDomain N; N.min=1; N.delta=1; N.max=2;

  real Cp(t,N), Cout(t);

  when(t=t.min) Cp = 0;
  Cp:t=Cp;
  Cout = Cp(t.max,N.max); 
}

