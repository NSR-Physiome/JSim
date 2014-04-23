/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
	
// Genetic algorithm,  implemented by Deepak Chandran
	 
package JSim.nml.opt;
import java.lang.*;
import java.util.*;
import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;
	
public class Genetic extends Optimizer {
	
	// static class initialization 
	public static OptimAlg.Info algInfo() {
	OptimAlg.Info algInfo = new OptimAlg.Info();
	algInfo.name = "genetic";
	algInfo.boundsNeeded = true;
	algInfo.sensMatNeeded = false;
	algInfo.parsNeeded = new String[] { "maxCalls", "populationSize", "mutationRate",  "crossoverRate", 
	                                    "mutationStep","selectMethod","eliteCutoff"};
	algInfo.optimClassName = Genetic.class.getName();
	return algInfo;
	}
	
	// constructor
	public Genetic() { }
	
	// run optimizer
	public void optimize(RTContext ctxt, OptimResults res, OptimCallbacks cbs) throws Xcept {        
	
	    // initialize
	    OptimArgs args = res.args;
	
	    // System.err.println("GS mut=" + args.mutationRate);
	    if (args.mutationRate < 0 || args.mutationRate > 1)
	        throw new Xcept(
	        "Genetic Optimizer: 0<=Mutation Rate<=1. Illegal mutationRate=" + args.mutationRate);
	    if (args.crossoverRate < 0 || args.crossoverRate > 1)
	        throw new Xcept(
	        "Genetic Optimizer: 0<=Crossover rate<=1. Illegal crossoverRate=" + args.crossoverRate);
	    if (args.populationSize<1)
	        throw new Xcept(
	        "Genetic Optimizer: Population Size>0. Illegal Population Size=" + args.populationSize);
	    if ((args.selectMethod!=0) && (args.selectMethod!=1) && (args.selectMethod!=2) )
	        throw new Xcept(
	        "Genetic Optimizer: Select Method must equal 0, 1 or 2");
	
	    //initialize
	    int numParams = args.xstart.length;
	    double[] minValues = args.xmin;
	    double[] maxValues = args.xmax;
	    double[] xstart    = args.xstart;
	    double mutationProb = args.mutationRate;
	    double crossoverProb = args.crossoverRate;
	    int maxCalls = args.maxCalls;
	    int populationSize = args.populationSize;
	    if(maxCalls<populationSize) populationSize=maxCalls;
	    if(populationSize<0) populationSize=1;
	    int numIterations = maxCalls/populationSize;
	    int selectionMethod = (int)(args.selectMethod);
	    double mutationStep = args.mutationStep;
	    double cutoff = args.eliteCutoff;
	
	    //setup for GA
	    double[][] population = initialize(populationSize, numParams, xstart, minValues, maxValues, ctxt);
	    double best;
	
	    //main GA loop
	    for (int i=0; i < numIterations; ++i) {
	
	        //next generation
	        best = oneStep( population, 
	                        selectionMethod, 
	                        minValues, 
	                        maxValues, 
	                        mutationProb, 
	                        mutationStep, 
	                        crossoverProb, 
	                        cutoff, 
	                        ctxt,  //for random number generator
	                        res, 
	                        cbs);
	        if (best < 0) //done
	        break;
	    }
	}
	
	// update OptimResults term info
	private void term(OptimResults res, String crit) {
	    res.status = OptimResults.NORMAL;
	    res.termMsg = "Met " + crit + " stopping criterion";
	}
	
	// query
	public String diagInfo() { 
	    return "Genetic Algorithm Optimizer"; 
	}
	
	public boolean allowMP() { 
	    return true; 
	}
	
	/******************************************
	   Genetic algorithm functions
	******************************************/
	
	// make copy of double array x, call if v
	private static double[] cloneArray(double[] x) {
	    double[] v = new double[ x.length ];
	    for (int i=0; i < x.length; ++i) {
	        v[i] = x[i];
	    }
	    return v;
	}
	
	// calculate the residual error multiplied by -1
	private static double[] calcFitness(double[][] population, RTContext ctxt, OptimResults res, OptimCallbacks cbs) {
	    if (population.length < 1) { 
	        return new double[0];
	    }
	    double[] residuals = new double[ population.length ];
	    int ct = 0;
	    try {
	        ct=  cbs.calcErrors(ctxt, population, residuals, res);
	    }
	    catch(Exception e) {
	        return new double[0];
	    }
	    
	    if (ct != population.length) {  //done
	        return new double[0];
	    }
	
	    double[] fitness = new double[ population.length ];
	    for (int i=0; i < population.length; ++i) {
	        fitness[i] = -residuals[i];
	    }
	    return fitness;
	}
	
	// first half of vector v3 equals first half of vector v1 index by index
	// rest of v3 equals v2 index by index
	private static double[] crossOver(double[] v1, double[] v2) {
	    double[] v3 = new double[v1.length];
	
	    //take half of each vector
	    for (int i=0; i < v1.length/2; ++i) {
	        v3[i] = v1[i];
	    }
	    for (int i=v1.length/2; i < v3.length && i < v2.length; ++i) {
	        v3[i] = v2[i];
	    }
	    //average the two vectors
	    //for (int i=0; i < v1.length && i < v2.length; ++i)
	    //    v3[i] = 0.5* ( v1[i] + v2[i] );
	    return v3;
	}
	
	// Generate new parameters by taking a random step which is proportional
	// to stepsize*(max value - min value)*( a uniform random # between -0.5
	// and 0.5)
	private static double[] mutate(double[] x, double prob, double stepSize, 
	                               double[] minValues, double[] maxValues, RTContext ctxt) {
	    double[] v = new double[x.length];
	    int n = (int)(v.length * prob);
	    if (n < 1) {  //at least one mutation
	        n = 1;
	    }
	    int k = 0;
	
	    for (int i=0; i < n; ++i) {
	        k = (int)(ctxt.random() * v.length);
	        v[k] += stepSize * (maxValues[i] - minValues[i]) * (ctxt.random() - 0.5);// * v[k];
	    }
	
	    return v;
	}
	
	// check parameters inside parameter bounds, fix if not the case
	// try the new population collection of parameters according to
	// selectMethod
	private static double oneStep(double[][] population, int selectionMethod, double[] minValues, 
	                              double[] maxValues, double mutationProb, double mutationStep, 
	                              double crossoverProb, double elitismPercent, RTContext ctxt, 
	                              OptimResults res, OptimCallbacks cbs) {
	    if (population.length < 1 || population[0].length < 1) return -1; //no population
	    int vecsize = population[0].length;
	
	    //make sure that all solutions are within the min and max boundaries
	    for (int i=0; i < population.length; ++i) {
	        for (int j=0; j < vecsize; ++j) {
	            if (minValues.length > j && maxValues.length > j &&
	                (population[i][j] > maxValues[j] || population[i][j] < minValues[j])) {
	
	                //pick random values in the given interval
	                population[i][j] = minValues[j] + ctxt.random()*(maxValues[j] - minValues[j]);  
	            }
	        }
	    }
	    double[] fitness = calcFitness(population, ctxt, res, cbs);
	    if (fitness.length != population.length) {  //done
	        return -1;
	    }
	
	    //get the min and max fitness
	    double minfitness = fitness[0], maxfitness = fitness[0];
	    int best = 0;
	    for (int i=0; i < fitness.length; ++i) {
	        if (fitness[i] < minfitness) minfitness = fitness[i];
	        if (fitness[i] > maxfitness)
	        {
	            best = i;
	            maxfitness = fitness[i];
	        }
	    }
	
	    //adjust fitness so that min is at 0
	    maxfitness -= minfitness;
	    for (int i=0; i < fitness.length; ++i) {
	        fitness[i] -= minfitness;
	    }
	
	    //always keep the best
	    double[][] newPopulation = new double[ population.length ][ vecsize ];
	    newPopulation[0] = cloneArray(population[ best ]);
	    int popsz = 1;
	
	    //selection loop
	
	    switch (selectionMethod) {
	
	        case 0: { //roulette wheel
	            double r1, r2, total = 0;
	            for (int i=0; i < fitness.length; ++i) { //compute total fitness
	                total += fitness[i];
	            }
	            while (popsz < population.length) {
	                int k = 0;
	                r1 = ctxt.random()*total;
	                r2 = fitness[0];
	            
	                while (r2 < r1 && k < (fitness.length-1)) { //convert fitness -> probability
	                    r2 += fitness[k];
	                    ++k;
	                }
	                double[] child;
	                if (ctxt.random() < crossoverProb) { //do crossover?
	                    r1 = ctxt.random()*total;
	                    r2 = fitness[0];
	                    int k2 = 0;
	                    while (r2 < r1 && k2 < (fitness.length-1)) {
	                        r2 += fitness[k2];                            
	                        ++k2;
	                    }
	                    child = crossOver(population[k], population[k2]);
	                    child = mutate(child, mutationProb, mutationStep, minValues, maxValues, ctxt);
	                } else { //if no crossover, then mutate
	                    child = mutate(population[k], mutationProb, mutationStep, minValues, maxValues, ctxt);
	                }
	                newPopulation[popsz] = child;
	                ++popsz;
	            }
	            break;
	        }
	
	        case 1: { //tournament
	            int k1,k2,k3,k4,p1,p2;
	            while (popsz < population.length) {
	                double[] child;
	                k1 = (int)(population.length * ctxt.random());  //pick an individual
	                k2 = (int)(population.length * ctxt.random()); //pick an individual
	                if (fitness[k1] > fitness[k2]) {  //select best of two
	                    p1 = k1;
	                } else {
	                    p1 = k2;
	                }
	                if (ctxt.random() < crossoverProb){ //do crossover?
	                    k3 = (int)(population.length * ctxt.random());
	                    k4 = (int)(population.length * ctxt.random());
	                    if (fitness[k3] > fitness[k4]) {
	                        p2 = k3;
	                    } else {
	                        p2 = k4;
	                    }                
	                    child = crossOver(population[p1], population[ p2 ]);
	                    child = mutate(child, mutationProb, mutationStep, minValues, maxValues, ctxt);
	                } else { //if no crossover, then mutate
	                    child = mutate(population[p1], mutationProb, mutationStep, minValues, maxValues, ctxt);
	                }
	                newPopulation[popsz] = child;
	                ++popsz;
	            }
	            break;
	        }
	
	        case 2: { //elitism
	            if (elitismPercent >= 1 || elitismPercent <= 0) {
	                elitismPercent = 0.5;
	            }
	            double[] sortedFitness = new double[fitness.length];
	            for (int i=0; i < fitness.length; ++i) {
	                sortedFitness[i] = fitness[i];
	            }
	            Arrays.sort(sortedFitness);
	            double cutoff = sortedFitness[ (int)(fitness.length*(1 - elitismPercent)) ];
	            for (int i=0; i < population.length; ++i) {
	                if (fitness[i] > cutoff) {
	                    newPopulation[popsz] = cloneArray(population[i]);
	                    ++popsz;
	                }
	            }
	            popsz = 0;
	            while (popsz < population.length) {
	                double[] child;
	                if (ctxt.random() < crossoverProb) {
	                    child = crossOver(
	                            newPopulation[(int)(ctxt.random() * newPopulation.length)],
	                            newPopulation[(int)(ctxt.random() * newPopulation.length)]);
	                    child = mutate(child, mutationProb, mutationStep, minValues, maxValues, ctxt);
	                } else {
	                    child = mutate(
	                            newPopulation[(int)(ctxt.random() * newPopulation.length)],
	                            mutationProb,
	                            mutationStep,
	                            minValues, maxValues,
	                            ctxt);
	                }
	                newPopulation[popsz] = child;
	                ++popsz;
	            }
	        }
	    }
	
	    //       population = newPopulation;
	    for (int i=0; i < population.length && i < newPopulation.length; ++i) {
	        for (int j=0; j < population[i].length && j < newPopulation[i].length; ++j) {
	            population[i][j] = newPopulation[i][j];  
	        }
	    }
	    return maxfitness;
	}
	
	// initialize first guesses for this population of parameters uniform randomized
	// between the min and max values
	private static double[][] initialize(int populationSize, int vectorSize, 
	                                     double[] xstart, double[] minValues, 
	                                     double[] maxValues , RTContext ctxt) {
	    double[][] population = new double[ populationSize ][ vectorSize ];
	    for (int j=0; j<vectorSize; j++) {
	        population[0][j] = xstart[j];
	    }
	    for (int i=1; i < populationSize; ++i) {
	        for (int j=0; j < vectorSize; ++j) {
	            if (minValues.length > j && maxValues.length > j) {
	
	                //pick random values in the given interval
	                population[i][j] = minValues[j] + ctxt.random()*(maxValues[j] - minValues[j]);
	            } else {
	                population[i][j] = ctxt.random(); //between 0 and 1
	            }
	        }
	    }
	    return population;
	}
}
