// Sensitivity analysis tutorial model
math sens1 { 
	realDomain t;
	t.min=0; t.max=4*PI; t.delta=0.1;
	real amp = 1;
	real phase = 0;
	real decay = 1;

	real u(t) = amp*exp(-decay*t);
	real v(t) = u * sin(t+phase);	
	real w(t) = v * cos(t+phase);	
}
