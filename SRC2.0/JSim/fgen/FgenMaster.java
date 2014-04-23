/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Function-generator master

package JSim.fgen;

import JSim.util.*;
import JSim.data.*;

public class FgenMaster implements DiagInfo {
	private String name;
	private FgenSlave slave;

	// constructor
	public FgenMaster(String n) {
	    name = n;
	}
	
	// clear before next run
	public void clear() { slave = null; }

	// initialize run for given context
	public void init(FgenContext ctxt) throws Xcept{
	    int which = namedVal("which", ctxt).intVal();
	    switch (which) {
	    case 0: slave = new FgenPulse1(this, "Pulse1", ctxt); break;
	    case 1: slave = new FgenPulse2(this, "Pulse2", ctxt); break;
	    case 2: slave = new FgenPulse3(this, "Pulse3", ctxt); break;
	    case 3: slave = new FgenExtendedPulse3(this, "ExtendedPulse3", ctxt); break;
	    case 4: slave = new FgenRamp(this, "Ramp", ctxt); break;
	    case 5: slave = new FgenSquare(this, "SquareWaveTrain", ctxt); break;
	    case 6: slave = new FgenSawtooth(this, "SawtoothTrain", ctxt); break;
	    case 7: slave = new FgenSine(this, "SineTrain", ctxt); break;
	    case 8: slave = new FgenExpon(this, "Exponential", ctxt); break;
	    case 9: slave = new FgenGauss(this, "Gaussian", ctxt); break;
	    case 10: slave = new FgenLaggedNormal(this, "LagNormal", ctxt); break;
	    case 11: slave = new FgenLongtail(this, "Longtail", ctxt); break;
	    case 12: slave = new FgenGamma(this, "GammaVar", ctxt); break;
	    case 13: slave = new FgenPoisson(this, "Poisson", ctxt); break;
	    case 14: slave = new FgenRandomWalk(this, "RandomWalk", ctxt); break;
	    case 15: slave = new FgenData(this, "DataCurve", ctxt); break;
	    default: throw new Xcept(
	    	"FuncGen " + name + " invalid which during init()");
	    }
	} 

	// single realVal
	public double realVal(FgenContext ctxt) throws Xcept {
	    if (slave == null) init(ctxt);
	    return slave.realVal(ctxt);
	}

	// entire fgen data 
	public RealNData data() throws Xcept {
	    if (slave == null) throw new Xcept(
	    	"FgenMaster.data() called while not initialized");
	    return slave.data();
	}

	// get named value
	public NamedVal namedVal(String n, FgenContext ctxt) 
	throws Xcept {
	    return ctxt.namedVal(name + "." + n);
	}
	
	// query
	public String name() { return name; }
	public String diagInfo() { return "FuncGen master " + name; }

	// static defaults
	private static NamedVal.NList defVals;
	public static NamedVal.NList getDefaults() { return defVals; }

	// static initializer
	static {
	    defVals = new NamedVal.NList();
	    defVals.add(NamedVal.create("Exponential.area", 1.0));	 
	    defVals.add(NamedVal.create("Exponential.tMean", 5.0));
	    defVals.add(NamedVal.create("Exponential.RD", 0.3));
	    defVals.add(NamedVal.create("Exponential.frPeak", 1.0e-06));
	    defVals.add(NamedVal.create("Exponential.timeToRepeat", 0.0));
	    defVals.add(NamedVal.create("ExtendedPulse3.offset", 0.0));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.timeToRepeat", 3.0));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p1Start", 0.0));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p1Duration", 0.5));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p1Amplitude", 1.0));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p1LoopCnt",1));
	    defVals.add(NamedVal.create("ExtendedPulse3.p1AmpIncr",0.0));
	    defVals.add(NamedVal.create("ExtendedPulse3.p2Start", 1.0));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p2Duration", 0.5));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p2Amplitude", 2.0));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p2LoopCnt",3));
	    defVals.add(NamedVal.create("ExtendedPulse3.p2AmpIncr",0.5));
	    defVals.add(NamedVal.create("ExtendedPulse3.p3Start", 2.0));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p3Duration", 0.5));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p3Amplitude", 3.0));	 
	    defVals.add(NamedVal.create("ExtendedPulse3.p3LoopCnt",0));
	    defVals.add(NamedVal.create("ExtendedPulse3.p3AmpIncr",0.0));
	    defVals.add(NamedVal.create("GammaVar.area", 1.0));	 
	    defVals.add(NamedVal.create("GammaVar.tMean", 5.0));
	    defVals.add(NamedVal.create("GammaVar.RD", 0.3));
	    defVals.add(NamedVal.create("GammaVar.skewn", 1.3));
	    defVals.add(NamedVal.create("GammaVar.frPeak", 1e-6));
	    defVals.add(NamedVal.create("GammaVar.timeToRepeat", 0.0));
	    defVals.add(NamedVal.create("Gaussian.area", 1.0));	 
	    defVals.add(NamedVal.create("Gaussian.tMean", 5.0));
	    defVals.add(NamedVal.create("Gaussian.RD", 0.3));
	    defVals.add(NamedVal.create("Gaussian.frPeak", 1.0e-06));
	    defVals.add(NamedVal.create("Gaussian.timeToRepeat", 0.0));
	    defVals.add(NamedVal.create("LagNormal.area", 1.0));	 
	    defVals.add(NamedVal.create("LagNormal.tMean", 5.0));
	    defVals.add(NamedVal.create("LagNormal.RD", 0.3));
	    defVals.add(NamedVal.create("LagNormal.skewn", 1.3));
	    defVals.add(NamedVal.create("LagNormal.frPeak", 1e-6));
	    defVals.add(NamedVal.create("LagNormal.upslope", 0));
	    defVals.add(NamedVal.create("LagNormal.timeToRepeat", 0.0));
	    defVals.add(NamedVal.create("Longtail.PDF", 0));	 
	    defVals.add(NamedVal.create("Longtail.area", 1.0));	 
	    defVals.add(NamedVal.create("Longtail.tMean", 5.0));
	    defVals.add(NamedVal.create("Longtail.RD", 0.3));
	    defVals.add(NamedVal.create("Longtail.skewn", 1.3));
	    defVals.add(NamedVal.create("Longtail.frPeak", 1e-6));
	    defVals.add(NamedVal.create("Longtail.upslope", 0));
	    defVals.add(NamedVal.create("Longtail.tORfr", 1));	 
	    defVals.add(NamedVal.create("Longtail.tJoin", 10.0));
            defVals.add(NamedVal.create("Longtail.frJoin", 0.25));	 
	    defVals.add(NamedVal.create("Longtail.expORpow", 0));	 
	    defVals.add(NamedVal.create("Longtail.nExp", 1.0));
            defVals.add(NamedVal.create("Longtail.w1", 1.0));
            defVals.add(NamedVal.create("Longtail.w2", 0.25));
            defVals.add(NamedVal.create("Longtail.w3", 0.0625));
            defVals.add(NamedVal.create("Longtail.w4", 0.015625));
            defVals.add(NamedVal.create("Longtail.k1", 1.0));
            defVals.add(NamedVal.create("Longtail.k2", 0.25));
            defVals.add(NamedVal.create("Longtail.k3", 0.0625));
            defVals.add(NamedVal.create("Longtail.k4", 0.015625));
	    defVals.add(NamedVal.create("Longtail.nPow", 1.0));	 
	    defVals.add(NamedVal.create("Longtail.wpow1", 1.0));	 
	    defVals.add(NamedVal.create("Longtail.wpow2", 0.5));	 
	    defVals.add(NamedVal.create("Longtail.wpow3", 0.25));	 
	    defVals.add(NamedVal.create("Longtail.wpow4", 0.125));	 
	    defVals.add(NamedVal.create("Longtail.beta1", 2.0));	 
	    defVals.add(NamedVal.create("Longtail.beta2", 1.5));	 
	    defVals.add(NamedVal.create("Longtail.beta3", 1.0));	 
	    defVals.add(NamedVal.create("Longtail.beta4", 0.5));	 
	    defVals.add(NamedVal.create("Longtail.timeToRepeat", 0.0));
	    defVals.add(NamedVal.create("Poisson.area", 1.0));	 
	    defVals.add(NamedVal.create("Poisson.tMean", 5.0));
	    defVals.add(NamedVal.create("Poisson.RD", 0.3));
	    defVals.add(NamedVal.create("Poisson.frPeak", 1e-06));
	    defVals.add(NamedVal.create("Poisson.timeToRepeat", 0.0));
	    defVals.add(NamedVal.create("Pulse1.startTime", 0.0));	 
	    defVals.add(NamedVal.create("Pulse1.duration", 1.0));	 
	    defVals.add(NamedVal.create("Pulse1.amplitude", 1.0));	 
	    defVals.add(NamedVal.create("Pulse1.offset", 0.0));	 
	    defVals.add(NamedVal.create("Pulse1.timeToRepeat", 10.0));
	    defVals.add(NamedVal.create("Pulse2.p1Start", 0.0));	 
	    defVals.add(NamedVal.create("Pulse2.p1Duration", 1.0));	 
	    defVals.add(NamedVal.create("Pulse2.p1Amplitude", 1.0));	 
	    defVals.add(NamedVal.create("Pulse2.offset", 0.0));	 
	    defVals.add(NamedVal.create("Pulse2.p1TimeToRepeat", 10));	 
	    defVals.add(NamedVal.create("Pulse2.p2Start", 1.0));	 
	    defVals.add(NamedVal.create("Pulse2.p2Duration", 2.0));	 
	    defVals.add(NamedVal.create("Pulse2.p2Amplitude", 2.0));
	    defVals.add(NamedVal.create("Pulse2.p2TimeToRepeat", 0.0));	 
	    defVals.add(NamedVal.create("Pulse3.p1Start", 0.0));	 
	    defVals.add(NamedVal.create("Pulse3.p1Duration", 1.0));	 
	    defVals.add(NamedVal.create("Pulse3.p1Amplitude", 1.0));	 
	    defVals.add(NamedVal.create("Pulse3.offset", 0.0));	 
	    defVals.add(NamedVal.create("Pulse3.p1TimeToRepeat", 10.0));	 
	    defVals.add(NamedVal.create("Pulse3.p2Start", 1.0));	 
	    defVals.add(NamedVal.create("Pulse3.p2Duration", 2.0));	 
	    defVals.add(NamedVal.create("Pulse3.p2Amplitude", 2.0));	 
	    defVals.add(NamedVal.create("Pulse3.p2TimeToRepeat", 0.0));	 
	    defVals.add(NamedVal.create("Pulse3.p3Start", 3.0));	 
	    defVals.add(NamedVal.create("Pulse3.p3Duration", 3.0));	 
	    defVals.add(NamedVal.create("Pulse3.p3Amplitude", 3.0));	 
	    defVals.add(NamedVal.create("Pulse3.p3TimeToRepeat", 0.0));	 
	    defVals.add(NamedVal.create("Ramp.startTime", 1.0));	 
	    defVals.add(NamedVal.create("Ramp.duration", 5.0));	 
	    defVals.add(NamedVal.create("Ramp.amplitude", -1.0));	 
	    defVals.add(NamedVal.create("Ramp.offset", 1.0));	 
	    defVals.add(NamedVal.create("Ramp.exponent",3.0));
	    defVals.add(NamedVal.create("Ramp.timeToRepeat", 10.0));
	    defVals.add(NamedVal.create("RandomWalk.area", 1.0));	 
	    defVals.add(NamedVal.create("RandomWalk.tMean", 5.0));
	    defVals.add(NamedVal.create("RandomWalk.RD", 0.3));
	    defVals.add(NamedVal.create("RandomWalk.skewn", 1.0));
	    defVals.add(NamedVal.create("RandomWalk.frPeak", 1.0e-6));
	    defVals.add(NamedVal.create("RandomWalk.timeToRepeat", 0.0));
	    defVals.add(NamedVal.create("SawtoothTrain.startTime", 0.0));	 
	    defVals.add(NamedVal.create("SawtoothTrain.duration", 10.0));	 
	    defVals.add(NamedVal.create("SawtoothTrain.amplitude", 1.0));	 
	    defVals.add(NamedVal.create("SawtoothTrain.offset", 0.0));	 
	    defVals.add(NamedVal.create("SawtoothTrain.shapeFactor", 0.5));
	    defVals.add(NamedVal.create("SawtoothTrain.phase", 0.0));
	    defVals.add(NamedVal.create("SawtoothTrain.period",5.0));
	    defVals.add(NamedVal.create("SawtoothTrain.timeToRepeat", 0.0));
	    defVals.add(NamedVal.create("SineTrain.startTime", 0.0));	 
	    defVals.add(NamedVal.create("SineTrain.duration", 5.0));	 
	    defVals.add(NamedVal.create("SineTrain.amplitude", 1.0));	 
	    defVals.add(NamedVal.create("SineTrain.offset", 0.0));	 
	    defVals.add(NamedVal.create("SineTrain.phase", 0.0));
	    defVals.add(NamedVal.create("SineTrain.period", 5.0));
	    defVals.add(NamedVal.create("SineTrain.timeToRepeat", 10.0));
	    defVals.add(NamedVal.create("SquareWaveTrain.startTime", 0.0));	 
	    defVals.add(NamedVal.create("SquareWaveTrain.duration", 10.0));	 
	    defVals.add(NamedVal.create("SquareWaveTrain.amplitude", 1.0));	 
	    defVals.add(NamedVal.create("SquareWaveTrain.offset", 0.0));	 
	    defVals.add(NamedVal.create("SquareWaveTrain.shapeFactor", 0.5));
	    defVals.add(NamedVal.create("SquareWaveTrain.phase", 0.0));
	    defVals.add(NamedVal.create("SquareWaveTrain.period",5.0));
	    defVals.add(NamedVal.create("SquareWaveTrain.timeToRepeat", 0.0));
	}
}

	
