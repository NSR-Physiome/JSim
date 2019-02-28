/*NSRCOPYRIGHT
	Copyright (C) 1999-2019 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/


// Particle swarm algorithm translated from c++. Originally written by:

//--------------------------------------------------------------------------------------//
//                     University of California San Diego                               //
//                         Dept. of Bioengineering                                      //
//                      Cardiac Mechanics Research Group"                               //
//                         PI: Prof. Andrew McCulloch                                   //
//--------------------------------------------------------------------------------------//
// Authors: Yasser Aboelkassem & Andrew McCulloch                                       //
// Year  :  08/2017                                                                     //
//-----------------------------                                                         //
//          Originally a part of the "Markov Chain Monte Carlo" algorithm to solve      //
//                  cardiac thin filament activation problem.                           //



package JSim.nml.opt;
import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;
import java.lang.Math;
import Jama.*;
import java.util.Random;
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

public class PSwarm extends Optimizer {

	// static class initialization
	public static OptimAlg.Info algInfo() {
		OptimAlg.Info algInfo = new OptimAlg.Info();
		algInfo.name = "pswarm";
		algInfo.boundsNeeded = true;
		algInfo.sensMatNeeded = false;
		algInfo.parsNeeded = new String[] { "numParticles","velCoeff","minInertia",
											"maxInertia", "cogLearn", "socLearn"  };
		algInfo.optimClassName = PSwarm.class.getName();
		return algInfo;
	}

	// constructor
	public PSwarm() {	}

      // run optimizer
   public void optimize(RTContext ctxt, OptimResults res,
						OptimCallbacks cbs) throws Xcept {
   OptimArgs args = res.args;
   int nx = args.nx();        // # of pars to optimize
   double[] xmin = args.xmin; // parameter minimum value
   double[] xmax = args.xmax; // parameter maximum value
   if (Util.hasNaNs(xmin) ||
	   Util.hasNaNs(xmax)) throw new Xcept(this,
                "xmin & xmax required");
   int n_particles_PSO = args.numParticles;   // Number of particles in swarm
   if ( n_particles_PSO > args.maxCalls) throw new Xcept(this,
                "# of particles must be smaller than max # of runs");
   if (n_particles_PSO < 2) throw new Xcept(this,
                "# of particles in swarm must be greater than 1.");
   int max_iter = (int)(args.maxCalls/n_particles_PSO);  // Number of iterations
   double vel_coeff = args.velCoeff;  // Particle velocity coefficient
   if ( (vel_coeff > 1.0) || (vel_coeff < 0)) throw new Xcept(this,
                "Particle velocity coefficient must be between 0 and 1.0");
   if( (args.minInertia <0.0) || (args.minInertia>1.0) ) throw new Xcept(this,
                "Minimum inertia for particle must be between 0 and 1.0");
   if( (args.maxInertia <args.minInertia) || (args.maxInertia >1.0) ) throw new Xcept(this,
                "Maximum inertia for particle must be between min inertia and 1.0");
   if( (args.cogLearn < 0) || (args.cogLearn >3.0) ) throw new Xcept(this,
                "Cognitive learn factor for particle (C1) must be between 0.0 and 3.0");
   if( (args.socLearn < 0) || (args.socLearn >3.0) ) throw new Xcept(this,
                "Social learn factor for particle (C2) must be between 0.0 and 3.0");

   int maxfn = args.maxCalls;  // Maximum number of simulation runs

   double []residual_cost_func = new double[n_particles_PSO]; // resid between model and exp (rms err)
   double   ssetl = args.errTol;    // mininum error value tolerance
   int ifn = 0;                  // counter for number of function calls

   //----------------------------------------------
   // Construct particle-parameter arrays
   //          Particles positions and velocities
   //----------------------------------------------

	 // particle-parameter arrays: Particles positions:
  double [][] x_PSO = initPosition( n_particles_PSO, xmin, xmax, nx );

  // particle-parameter arrays: Particle velocities:
  double[][] v_PSO = initVelocity( vel_coeff, n_particles_PSO, xmin, xmax, nx );

  //--------------------------------------------------------------------------
  // Solve for each particle-parameter sets to obtain initial residual array
  //---------------------------------------------------------------------------

	residual_cost_func = getCost(ctxt,n_particles_PSO, x_PSO,res,cbs);
	if (residual_cost_func.length < n_particles_PSO) {
		sendMsg(res,1);return;
	}
	ifn = ifn + n_particles_PSO;      // count model evaluations
	if (ifn>=maxfn) { sendMsg(res,2); return; }

 //------------------------------------------
 // Find Min of Residual (i.e., global best)
 //------------------------------------------
 double []Res_pbest = new double[n_particles_PSO];      // Best personal fit
 double Res_gbest = residual_cost_func [0];
 int i_Res_gbest = 0;
 for (int i = 0; i < n_particles_PSO; i++)
  {
      if (residual_cost_func [i] < Res_gbest)
            {
            Res_gbest    = residual_cost_func [i];
            i_Res_gbest  = i;
            }

      Res_pbest[i] = residual_cost_func [i];  // Residual personal best
  }

  //--------------------------------------------------------
  // obtain the parameters that give global (gbest)
  //--------------------------------------------------------
 double x_gbest[] = new double[nx];
 Arrays.fill(x_gbest, 0);
 for(int n =0; n<nx; n++) { x_gbest[n] = x_PSO[i_Res_gbest][n]; }

  //---------------------------------------------------------------------------------------
  // obtain the parameters that give personal best (pbest)
  // Note: this can be combined with one of the other loop but keep it like that for now
  //----------------------------------------------------------------------------------------
 double x_pbest[][] = new double[n_particles_PSO][nx];

 for(double[] row: x_pbest)
	  Arrays.fill(row, 0.0);

 for (int i = 0; i < n_particles_PSO; i++)
  {
	  for(int n =0; n<nx; n++) { x_pbest[i][n] = x_PSO[i][n]; }
  }

//----------------------------------------------------------------------------------
//
//                 Swarm Iteration Step over all particles
//
//------------------ --------------------------------------------------------------

  double w_max, w_min, dw;   // used for inertia calc
  double c1, c2;                // learning factors
  w_max = args.maxInertia;      // Default 1.0
  w_min = args.minInertia;      // Default 0.3
  dw = (w_max-w_min)/max_iter;
  c1 = args.cogLearn;    // cognitive learning factor, default 1.05
  c2 = args.socLearn;    // social learning factor, default 1.05
  for (int it = 0; it < max_iter+1; it++)
   {      // begin swarm iteration
	 	double w = w_min +it*dw;        // inertia weight, linearly increases
		for (int i = 0; i < n_particles_PSO; i++)
		   { // begin loop over all particles
			   // Send in just the x and v values for one particle.
			   double[] x = new double[nx];  // Contains parameter values for 1 particle
			   double[] v = new double[nx];  // Contains velo of parameters for 1 particle..
			   for(int n=0; n<nx; n++) {
				   x[n] = x_PSO[i][n];
				   v[n] = v_PSO[i][n];
			   }
			   // Update velocity:
			   v = updateVelo( x, v, nx, x_pbest, x_gbest, w, c1, c2, i );
			   // Update position:
			   x = updatePosition( x, v, xmin, xmax, nx, i );
			   for(int n=0; n<nx; n++) {
				   x_PSO[i][n] = x[n]; // Store updated positions
				   v_PSO[i][n] = v[n];
			   }
			}   // end looping over all particles to have new Residual vector

			// Now calculate costs for all particles:
			residual_cost_func = getCost(ctxt,n_particles_PSO, x_PSO, res, cbs);
			if (residual_cost_func.length < n_particles_PSO) {
				sendMsg(res,1);return;
			}
			ifn = ifn + n_particles_PSO;      // count model evaluations
			if (ifn>=maxfn) { sendMsg(res,2); }

		    //--------------------------------------------------
        // Find New Min of Residual (i.e., new global best)
        //--------------------------------------------------
		double min_Res = residual_cost_func [0];
		int i_min_Res = 0;
		for (int i = 0; i < n_particles_PSO; i++)
		   {
          if (residual_cost_func[i] < min_Res)
				  {
					  min_Res = residual_cost_func [i];
					  i_min_Res  = i;
          }
					if( residual_cost_func[i] <= ssetl ) { sendMsg(res,1);return; }
		   }
          //-------------------------------
          // Check for update min residual
          //-------------------------------
		if (min_Res <= Res_gbest)
        {
        	Res_gbest = min_Res;
          //-------------------------------------------------
          // obtain the parameters that give global (gbest)
          //--------------------------------------------------
			  	for(int n=0; n<nx; n++) {
				  	x_gbest[n] = x_PSO[i_min_Res][n];
			  	}
		  }

		  		//-------------------------------------------------
          // obtain the parameters that give personal (pbest)
          //--------------------------------------------------
		for (int i = 0; i < n_particles_PSO; i++)
          {
              if(residual_cost_func [i]  <= Res_pbest[i] )
              {
					  		for(int n=0; n<nx; n++) { // loop through all of the params being optimized.
						  		x_pbest[i][n] = x_PSO[i][n];
					  		}
					  		Res_pbest[i] = residual_cost_func [i];
					  		if( Res_pbest[i]<=ssetl ) { sendMsg(res,1);return; }
            	}
          }
		if (ifn + n_particles_PSO > maxfn) { sendMsg(res,2);return; }

   }// end swarm iteration

   sendMsg(res,2);  // Finished, maximum calls reached
  } // end of optimize



	private static double[][] initPosition( int n_particles_PSO, double[]xmin,
										   double[]xmax, int nx )
	{
		double[][] x_PSO = new double[n_particles_PSO][nx];// Initial particles position
		for (int i = 0; i < n_particles_PSO; i++)
        {   // init particle positions:
			for(int n=0; n<nx; n++) {
				// Generate a random number between [lower, upper] values:
				x_PSO[i][n] = xmin[n] + (xmax[n]-xmin[n])*ThreadLocalRandom.current().nextDouble();
			}
		}
		return x_PSO;
	}

	private static double[][] initVelocity(double vel_coeff, int n_particles_PSO,
                                           double[]xmin, double[]xmax, int nx)
	{
		double[][] v_PSO = new double[n_particles_PSO][nx];
		for (int i = 0; i < n_particles_PSO; i++)
		{   // Velocities : vel_coeff*(lower-upper)*rand : this is can be anything
			for(int n=0; n<nx; n++) {
				// Generate a random number between [lower, upper] values:
				v_PSO[i][n] = vel_coeff*(xmax[n]-xmin[n])*ThreadLocalRandom.current().nextDouble();
			}
    }
		return v_PSO;
	}

	// Update particle velocity
	private static double[] updateVelo(double[] x,double[] v, int nx, double[][] x_pbest,
                            double[] x_gbest, double w, double c1, double c2, int i )
	{
		for(int n=0; n<nx; n++) {
			v[n] = w*v[n] +
			c1*ThreadLocalRandom.current().nextDouble() * (x_pbest[i][n] - x[n]) +
			c2*ThreadLocalRandom.current().nextDouble() * (x_gbest[n] - x[n]);
		}
		return v;
	}

	// Update particle position
	private static double[] updatePosition(double []x, double[] v, double[] xmin,
										   double[] xmax, int nx,  int i )
	{
		for(int n=0; n<nx; n++) {
			x[n] = x[n] + v[n];
			if(x[n]>xmax[n])   // Make sure velocity does not put it out of range.
				x[n] = xmax[n];
			else if(x[n]<xmin[n])
				x[n] = xmin[n];
		}
		return x;
	}

	private static double[] getCost( RTContext ctxt, int particles, double[][] x, OptimResults res, OptimCallbacks cbs )
	throws Xcept {
		double[] residuals = new double[particles];
		int ct = 0;
		// Calculates all residuals, able to take advantage of multiprocessor (parallel processing)
		ct = cbs.calcErrors(ctxt,x,residuals, res);
		if (ct != particles) {  //done
	  	return new double[0];
	  }
		return residuals;
	}

	// error messages ... Update with relevant msgs
	public void sendMsg(OptimResults res, int istat) {
		switch(istat) {
                case 1:
                    term(res, "mean sqr error");
                    break;
                case 2:
                    term(res,"# calls");
                    break;
                case 3:
                    term(res,"# calls would have exceeded");
                    break;
                default:
                    termError(res,"Unspecified error in PSwarm opt.");
                    break;
                }
            return;
        }

	// update OptimResults term info OK
	private void term(OptimResults res, String crit) {
            res.status = OptimResults.NORMAL;
            res.termMsg = "Met " + crit + " stopping criterion";
	}

	// update OptimResults termEror info
	private void termError(OptimResults res, String crit) {
            res.status = OptimResults.ERROR;
            res.termMsg = crit ;
	}

  // query
  public String diagInfo() { return "Particle Swarm Optimizer"; }
	public boolean allowMP() { return true; }

}
