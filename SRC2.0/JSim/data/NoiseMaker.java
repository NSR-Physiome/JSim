/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Noisy data generator

package JSim.data;

import JSim.util.*;
import JSim.expr.*;

import java.io.*;
import java.util.Random;

public class NoiseMaker {
	private int dist;
	private double magnitude;
	private int addMethod;
	private Random random;	

	// dist constants 
	public static final int UNIFORM = 0;
	public static final int GAUSSIAN = 1;
	public static final int POISSON = 2;

	// addMethod constants
	public static final int PROPORTIONAL = 0;
	public static final int ADDITIVE = 1;

	// constructor
	public NoiseMaker() {
	    random = new Random(); // randomized
	    dist = GAUSSIAN;
	    magnitude = 0.1;
	    addMethod = PROPORTIONAL;
	}
	
	// set stuff
	public void setDist(int i) { dist = i; }
	public void setMagnitude(double d) { magnitude = d; }
	public void setAddMethod(int i) { addMethod = i; }
	public void setRandomSeed(int i) { 
	    if (i != 0) random.setSeed(i); 
	}
	
	// create new data curve
	public RealNData makeNoisy(Data base) throws
	Xcept {
	    GridData[] grids = new GridData[base.ndim()];
	    for (int i=0; i<base.ndim(); i++)
	    	grids[i] = base.grid(i);
	    RealNData noisy = new RealNData(base.desc(), 
	    	base.unit(), grids);
	    noisy.setName(base.name());
	    for (int i=0; i<base.nsamples(); i++) 
	    	noisy.set(i, addNoise(base.realVal(i)));
	    return noisy;
	}
	
	// add noise to a sample
	private double addNoise(double s) throws Xcept {
	    double n = Double.NaN;
	    switch (dist) {
	    case UNIFORM: n = uniformSample(); break;
	    case GAUSSIAN: n = gaussianSample(); break;
	    case POISSON: n = poissonSample(); break;
	    default: throw new Xcept("Unknown NoiseMaker.dist=" + dist);
	    }
	    n *= magnitude;
	    switch (addMethod) {
	    case PROPORTIONAL: 
	    	return s * (1 + n);
	    case ADDITIVE:
	        return s + n;
	    default:
	        throw new Xcept("Unknown NoiseMaker.addMethod=" +
		    addMethod);
	    }
	}
	    
	// uniform distribution sample
	private double uniformSample() { 
	    return (random.nextDouble() - 0.5) * 2;
	}
	
	// gaussian distribution sample
	private double gaussianSample() {
	    return random.nextGaussian();
	}
	
	// poisson distribution sample
	private double poissonSample() throws Xcept {
	    throw new Xcept("NoiseMaker.poisson not yet supported");
	}

	// test harness
	public static final void main(String[] args) throws Xcept {

	    // process command line
	    if (args.length != 5) throw new Xcept(
	    	"Usage: NoiseMaker dist mag addMethod seed data-file");
	    int dist = Util.toInt(args[0]);
	    double mag = Util.toDouble(args[2]);
	    int addMethod = Util.toInt(args[3]);
	    int seed = Util.toInt(args[4]);
	    File inf = new File(args[5]);

	    // read data file
	    String sfx = UtilIO.fileSuffix(inf);
	    DataFormat.List fmts = new DataFormat.List();
	    DataFormat fmt = fmts.forSuffix(sfx);
	    DataReader rdr = fmt.createReader(inf);
	    Data.List indata = rdr.readData();
	    
	    // create generator
	    NoiseMaker gen = new NoiseMaker();
	    gen.setDist(dist);
	    gen.setMagnitude(mag);
	    gen.setAddMethod(addMethod);
	    gen.setRandomSeed(seed);
	    
	    // create noisy data, write to stdout
	    Data.List outdata = new Data.List(indata.size());
	    for (int i=0; i<indata.size(); i++) 
	    	outdata.add(gen.makeNoisy(indata.data(i)));
	    DataWriter wrt = fmt.createWriter();
	    wrt.writeData(System.out, outdata);
	}
	    
}

