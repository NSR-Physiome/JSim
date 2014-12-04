/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 3 Pulse function

package JSim.fgen;

import JSim.util.*;


public class FgenPulse3 extends FgenSlave {
        double p1strt;
	double p1dur;
	double p1amp;
	double p1off;
        double p1trep;
        double p2strt;
	double p2dur;
	double p2amp;
        double p2trep;
        double p3strt;
	double p3dur;
	double p3amp;
        double p3trep;

	// constructor
	public FgenPulse3(FgenMaster f, String n, FgenContext ctxt) 
	throws Xcept { 
	    super(f,n,ctxt);
	}

	// dimensionality
	public int ndim() { return 1; }

	// initialize at beginning of run
	public void init() throws Xcept {
	    super.init();
            p1strt = realVal("p1Start");
	    p1dur  = realVal("p1Duration");
	    p1amp  = realVal("p1Amplitude");
	    p1off  = realVal("offset");
            p1trep = realVal("p1TimeToRepeat");
            p2strt = realVal("p2Start");
	    p2dur  = realVal("p2Duration");
	    p2amp  = realVal("p2Amplitude");
            p2trep = realVal("p2TimeToRepeat");
            p3strt = realVal("p3Start");
	    p3dur  = realVal("p3Duration");
	    p3amp  = realVal("p3Amplitude");
            p3trep = realVal("p3TimeToRepeat");
	}

	// function value for particular domain values
	public double realVal(double[] vals) throws Xcept {
	    double returnValue=0;
	    double t1 = vals[0];
	    double t2 = vals[0];
	    double t3 = vals[0];
//Pulse 1
	    double p1t;

            if(p1trep<=0) {p1t=t1;}
	    else {
		p1t=t1%(p1trep);
                if(p1t<0) p1t=p1t+p1trep;
	    }
	    if(p1t>=p1strt && p1t<p1strt+p1dur) {
		returnValue=returnValue+p1amp+p1off;}
	    else
		{returnValue=returnValue+p1off;
	    }
//Pulse 2
	    double p2t;

            if(p2trep<=0) {p2t=t2;}
	    else {
		p2t=t2%p2trep;
                if(p2t<0) p2t=p2t+p2trep;
	    }
	    if(p2t>=p2strt && p2t<p2strt+p2dur) {
		returnValue=returnValue+p2amp;}
	    else
		{returnValue=returnValue;
	    }
//Pulse 3
	    double p3t;

            if(p3trep<=0) {p3t=t3;}
	    else {
		p3t=t3%p3trep;
                if(p3t<0) p3t=p3t+p3trep;
	    }
	    if(p3t>=p3strt && p3t<p3strt+p3dur) {
		returnValue=returnValue+p3amp;}
	    else
		{returnValue=returnValue;
	    }
	    return returnValue;
	}

}


