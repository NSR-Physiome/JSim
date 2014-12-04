//// Model heart mml text:
source real function slow(a, b) {
	language="java";
        maincode={{
            double aval = a.realVal();
	    long bval = (long) b.realVal();
	    try { Thread.sleep(bval); }
		catch (Exception e) { }
            return aval;
        }};
}
 
math main {
  realDomain t;
  t.min=0; t.max=60; t.delta=0.1;
  real period = 3;
  real ts(t) = PI*slow(t, t.delta*1000)/period;
  real Vra(t) = 50 + 50*abs(sin(ts)); 
  real Vrv(t) = 75 + 75*abs(sin(ts)); 
  real Vla(t) = 50 + 50*abs(cos(ts)); 
  real Vlv(t) = 75 + 75*abs(cos(ts)); 
}

