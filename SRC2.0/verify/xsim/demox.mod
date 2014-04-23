JSim v1.1 

import XSim;

XSim main {
	library lib("demox");
	ivar time;
	time.min=0; time.max=5; time.delta=0.2; time.loc = 130;

	realInput period = 6; period.loc = 1;
	realInput amplitude = 1; amplitude.loc = 2;
	realInput offset = 0; offset.loc = 3;
	realInput phase = 0; phase.loc = 4;
	realInput halflife = 10; halflife.loc = 5;
	realInput start_time = 2; start_time.loc = 6;
	private realInput delay_start = 100;
	delay_start.loc = 10;
	private realInput delay = 0;
	delay.loc = 11;

	realOutput Sine(time); Sine.loc = 201;
	realOutput Cosine(time); Cosine.loc = 202;
	realOutput Tangent(time); Tangent.loc = 203;
}

