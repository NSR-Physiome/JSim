// fma, opd properties
property fma=string, opb=string; // ontology references
math main {
  realDomain t;
  t.min=0; t.max=5; t.delta=1;
  extern real Cin(t);
  real u(t) = Cin^2;
  u.desc = "input concentration";
  real Vp = 1;
  Vp.desc = "plasma volume";
  Vp.help = "typically 0.3-2.5\nsee Atkins & Bee JAP 1997";
  Vp.fma = "fluid.blood.plasma"; 
  Vp.opb = "volume";
}
