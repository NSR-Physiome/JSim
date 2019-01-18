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

public class PSwarm extends Optimizer {

	int maxfn;            // Maximum number of simulation runs
	int n_particles_PSO;  // Number of particles in swarm
	int max_iter;         // Number of iterations
	double vel_coeff;     // Particle velocity coefficient

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
	public PSwarm() { 
		n_particles_PSO = 0;
		max_iter = 0;
		vel_coeff = 0;
		maxfn = 0;
	}

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
   n_particles_PSO = args.numParticles;
   if ( n_particles_PSO > args.maxCalls) throw new Xcept(this,
                "# of particles must be smaller than max # of runs");
   if (n_particles_PSO < 2) throw new Xcept(this,
                "# of particles in swarm must be greater than 1.");
   max_iter = (int)(args.maxCalls/n_particles_PSO);
   vel_coeff = args.velCoeff;
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

   maxfn = args.maxCalls;

   double []residual_cost_func = new double[n_particles_PSO]; // residual between model and exp (rms err)
   double []Res_pbest = new double[n_particles_PSO];      // Best personal fit
  
   double[] x = new double[nx];  // Contains parameter values.
   Arrays.fill(x, 0.0);
   double   ssetl = args.errTol;    // mininum error value tolerance
   int ifn = 0;                  // counter for number of function calls
   
   //----------------------------------------------
   // Step 1: construct particle-parameter arrays
   //          Particles positions and velocities
   //----------------------------------------------
  Random generator = new Random(); // to repeat, pass a seed (long) to Random().

  double [][] x_PSO = new double[nx][n_particles_PSO]; // particle-parameter arrays: Particles positions
  double [][] v_PSO = new double[nx][n_particles_PSO]; // particle-parameter arrays: Particle velocities
  for(double[] row: x_PSO)
	  Arrays.fill(row, 0.0);
  for(double[] row: v_PSO)
	  Arrays.fill(row, 0.0); 

  for (int i = 0; i < n_particles_PSO; i++)
        {
	        //-----------
	        // positions
	        //-----------
			for(int n=0; n<nx; n++) {
				// Generate a random number between [lower, upper] values:
				x_PSO[n][i] = xmin[n] + (xmax[n]-xmin[n])*generator.nextDouble(); 
			}
            //------------------------------------------------------------------
            // Velocities : vel_coeff*(lower-upper)*rand : this is can be anything
	        //--------------------------------------------------------------------
			for(int n=0; n<nx; n++) {
				// Generate a random number between [lower, upper] values
				v_PSO[n][i] = vel_coeff*(xmax[n]-xmin[n])*generator.nextDouble(); 
			}
        }
  //--------------------------------------------------------------------------
  // Step 2: solve for each particle-parameter sets to obtain residual array
  //---------------------------------------------------------------------------
  for (int i = 0; i < n_particles_PSO; i++)
  {
	  for(int n =0; n< nx; n++)
		{
			x[n] = x_PSO[n][i];
		}
   //-----------------------------------------------------------------------------------------
   // Call the force_pCa_curve function to get the force as a function of Ca++ concentrations.
   // NB: this function implicitly calls the other functions.
   //-----------------------------------------------------------------------------------------

	 //Compute the sum of the squares of the weighted residuals.
	  residual_cost_func[i] = cbs.calcError(ctxt,x,res);

	  ifn++;      // count model evaluations
	  if (ifn>=maxfn) { sendMsg(res,2); return; }
   } // close loop of particle

 //------------------------------------------
 // Find Min of Residual (i.e., global best)
 //------------------------------------------
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
 for(int n =0; n<nx; n++) { x_gbest[n] = x_PSO[n][i_Res_gbest]; }

  //---------------------------------------------------------------------------------------
  // obtain the parameters that give personal best (pbest)
  // Note: this can be combined with one of the other loop but keep it like that for now
  //----------------------------------------------------------------------------------------
 double x_pbest[][] = new double[nx][n_particles_PSO];

 for(double[] row: x_pbest)
	  Arrays.fill(row, 0.0);

 for (int i = 0; i < n_particles_PSO; i++)
  {
	  for(int n =0; n<nx; n++) { x_pbest[n][i] = x_PSO[n][i]; }
  }

//----------------------------------------------------------------------------------
//
//                 Swarm Iteration Step over all particles
//
//------------------ --------------------------------------------------------------
 
  double w_max, w_min, dw, w;   // inertia
  double c1, c2;                // learning factors
  w_max = args.maxInertia;      // Default 1.0
  w_min = args.minInertia;      // Default 0.3
  dw = (w_max-w_min)/max_iter;               
  c1 = args.cogLearn;    // cognitive learning factor, default 1.05
  c2 = args.socLearn;    // social learning factor, default 1.05

  for (int it = 0; it < max_iter+1; it++)
   {      // begin swarm iteration
	   w = w_min +it*dw;        // inertia weight, linearly increases
           for (int i = 0; i < n_particles_PSO; i++)
           { // begin loop over all particles

           //-----------------
           // Velocity update
           //-----------------
		   for(int n=0; n<nx; n++) {
			   v_PSO[n][i] = w*v_PSO[n][i] + c1*generator.nextDouble() * (x_pbest[n][i] - x_PSO[n][i]) +
				   c2*generator.nextDouble() * (x_gbest[n] - x_PSO[n][i]);
			}
           //-----------------
           // position update
           //-----------------
		   for(int n=0; n<nx; n++) {
			   x_PSO[n][i] = x_PSO[n][i] + v_PSO[n][i];  
			   if(x_PSO[n][i]>xmax[n])   // Make sure velocity does not put it out of range.
				   x_PSO[n][i] = xmax[n];
			   else if(x_PSO[n][i]<xmin[n]) 
				   x_PSO[n][i] = xmin[n];
		   }

           //---------------------------
           // model parameter update
           //---------------------------
		   for(int n=0; n<nx; n++) {
			   x[n] = x_PSO[n][i];
		   }

           //-----------------------------------------------------
           // residual update using the new particles/parameters
           //----------------------------------------------------
		   residual_cost_func[i] = cbs.calcError(ctxt,x,res);
		   ifn++;      // count model evaluations
           if (ifn>=maxfn) { sendMsg(res,2); return; }

           } // end looping over all particles to have new Residual vector

         //--------------------------------------------------
         // Find New Min of Residual (i.e., new global best)
         //--------------------------------------------------
         double min_Res = residual_cost_func [0];
         int i_min_Res = 0;
         for (int i = 0; i < n_particles_PSO; i++)
          {
              if (residual_cost_func [i] < min_Res)
				  {
					  min_Res    = residual_cost_func [i];
					  i_min_Res  = i;
                  }
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
				  x_gbest[n] = x_PSO[n][i_min_Res];
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
						  x_pbest[n][i] = x_PSO[n][i];
					  }
                   Res_pbest[i] = residual_cost_func [i];
				   if(  Res_pbest[i]<=ssetl)   { sendMsg(res,1); return; }
                  }
          }

   }// end swarm iteration

   sendMsg(res,2);  // Finished, maximum calls reached
   } // end of optimize

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
	public boolean allowMP() { return true; }   // true?

}
