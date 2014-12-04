/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Extended Pulse3 function

package JSim.fgen;

import JSim.util.*;


public class FgenExtendedPulse3 extends FgenSlave {
        double trep;
	double off;
        int loop3;
        int loop2;
        int loop1;
        double p1strt;
	double p1dur;
	double p1amp;
        double p1AmpL;
        double p2strt;
	double p2dur;
	double p2amp;
        double p2AmpL;
        double p3strt;
	double p3dur;
	double p3amp;
	double p3AmpL;

	// constructor
	public FgenExtendedPulse3(FgenMaster f, String n, FgenContext ctxt) 
	throws Xcept { 
	    super(f,n,ctxt);
	}

	// dimensionality
	public int ndim() { return 1; }

	// initialize at beginning of run
	public void init() throws Xcept {
	    super.init();
            trep = realVal("timeToRepeat");
 	    off  = realVal("offset");
            loop3 = Math.max(1, intVal("p3LoopCnt"));
            loop2 = Math.max(1, intVal("p2LoopCnt"));
            loop1 = Math.max(1, intVal("p1LoopCnt"));
            p1strt = realVal("p1Start");
	    p1dur  = realVal("p1Duration");
	    p1amp  = realVal("p1Amplitude");
            p1AmpL = realVal("p1AmpIncr");
            p2strt = realVal("p2Start");
	    p2dur  = realVal("p2Duration");
	    p2amp  = realVal("p2Amplitude");
            p2AmpL = realVal("p2AmpIncr");
            p3strt = realVal("p3Start");
	    p3dur  = realVal("p3Duration");
	    p3amp  = realVal("p3Amplitude");
	    p3AmpL = realVal("p3AmpIncr");
	}

	// function value for particular domain values
	public double realVal(double[] vals) throws Xcept {
	    double returnValue=0;
	    double t1 = vals[0];

	    double p1t;
            double p2t;
            double totlength = (double)(loop1*loop2*loop3);

            if(trep<=0) {p1t=t1; p2t=t1;}
	    else {
		p1t=t1%(trep); p2t=t1%(trep*totlength);
                if(p1t<0) {p1t=p1t+trep;}
                if(p2t<0) {p2t=p2t+trep*totlength;}
	    }
            int ithisloop=1;
            if(trep>0) {ithisloop=(int)(Math.ceil(p2t/trep+1E-7));}
            int iLoop1=1;
            int iLoop2=1;
            int iLoop3=1;
            iLoop3=ithisloop%loop3;
            if(iLoop3<=0) {iLoop3=iLoop3+loop3;}
            iLoop2=((ithisloop-iLoop3)/loop3+1) % loop2;
            if(iLoop2<=0) {iLoop2=iLoop2+loop2;}
            iLoop1 = ((ithisloop-iLoop3)/(loop3*loop2)+1) % loop1;
            if(iLoop1<=0) {iLoop1=iLoop1+loop1;}
//Pulse1 and offset

	    if(p1t>=p1strt && p1t<p1strt+p1dur) {
		returnValue=returnValue+(iLoop1-1)*p1AmpL+p1amp+off;}
	    else
		{returnValue=returnValue+off;
	    }
//Pulse 2
	    if(p1t>=p2strt && p1t<p2strt+p2dur) {
		returnValue=returnValue+(iLoop2-1)*p2AmpL+p2amp;}
	    else
		{returnValue=returnValue;
	    }
//Pulse 3
	    if(p1t>=p3strt && p1t<p3strt+p3dur) {
		returnValue=returnValue+(iLoop3-1)*p3AmpL+p3amp;}
	    else
		{returnValue=returnValue;
	    }
	    return returnValue;
	}

}


