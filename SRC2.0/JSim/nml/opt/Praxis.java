/*NSRCOPYRIGHT
	Copyright (C) 1999-2019 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/


// Code translated from C++. 20 Feb 2019

// Source:

//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2016
//
//  Author:
//
//    Original FORTRAN77 version by Richard Brent.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Richard Brent,
//    Algorithms for Minimization without Derivatives,
//    Prentice Hall, 1973,
//    Reprinted by Dover, 2002.



// Unbounded optimizer... added flag to make bounded.

package JSim.nml.opt;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;

import java.lang.Math;
import java.util.Arrays;
import Jama.*;
import java.util.concurrent.ThreadLocalRandom;

public class Praxis extends Optimizer {

	double v[]; // used by minfit()
	boolean test;	// run optimizer testing, true = 'run'
	boolean illc; // Is problem ill-conditioned?
	boolean bounded; // Use bounds?
	double[] xmin;	// Min param values
	double[] xmax;	// Max param values
	double errTol; // Min RMS error before stopping
	int ktm; // # of iterations without improvement before the algorithm terminates.
	int nf;  // counter for number of function calls
	int maxfn; // Maximum number of function calls.

        // static class initialization
	public static OptimAlg.Info algInfo() {
		OptimAlg.Info algInfo = new OptimAlg.Info();
        algInfo.name = "praxis";
				algInfo.boundsNeeded = true;  // Let user decide.
        algInfo.sensMatNeeded = true;
        algInfo.parsNeeded = new String[] { "maxCalls", "errTol", "maxDist",
			 								"itrNoImprove", "bounds", "praxisTol"	};
        algInfo.optimClassName = Praxis.class.getName();
        return algInfo;
	}

       // constructor
	public Praxis() {
	this.test = false;
	this.illc = false;
	this.bounded = false;  // Default to unbounded optimizer
	this.ktm = 1;
	this.maxfn = 0;
	this.nf = 0;
	this.errTol = 1e-4;
	}

 // run optimizer
	public void optimize(RTContext ctxt, OptimResults res,
						 OptimCallbacks cbs) throws Xcept {
		OptimArgs args = res.args;
		int nx = args.nx();        // # of pars to optimize

		this.maxfn = args.maxCalls; // Maximum number of simulation runs
		this.nf = 0;
		this.errTol = args.errTol; // mininum error value calculated
		if (args.praxisTol < 0) throw new Xcept(this,
								 "Fit Tolerence must be > 0");
		double t0 = args.praxisTol; // practical tolerance between true and calc error.

		double[] x = args.xstart;   // Starting values for each parameter
		this.bounded = args.bounds;
		if(this.bounded == true){
			this.xmin = args.xmin; // parameter minimum value
			this.xmax = args.xmax; // parameter maximum value
			if (Util.hasNaNs(this.xmin) ||
					Util.hasNaNs(this.xmax)) throw new Xcept(this,
					"xmin & xmax required");
		}
		// Make sure between 0 and 1, default to 1.
		if ( (args.maxDist >1.0) || (args.maxDist < 0.01)) args.maxDist = 1;


		double avgParmDist = 0;
		for(int i=0; i<nx;i++) avgParmDist = avgParmDist + x[i];
		double h0 = (avgParmDist/nx)*args.maxDist; // Max step size calculated from Avg of all params.

		this.ktm = args.itrNoImprove;
		// Set ktm to default if outside of range. Not a show stopper.
		if ( (this.ktm <1.0) || (this.ktm > 4)) this.ktm = 1;

		int prin =0; // Use for debug. Print settings:
		// 0: nothing printed.
		// 1: F is printed after every n+1 or n+2 linear minimizations.
		//       final X is printed, but intermediate X is printed only
		//       if N is at most 4.
		// 2: the scale factors and the principal values of the approximating
		//       quadratic form are also printed.
		// 3: X is also printed after every few linear minimizations.
		// 4: the principal vectors of the approximating quadratic form are
		//       also printed.


		double results = -5.0; // less than zero value.
 		results = praxis ( maxfn, t0, h0,  nx, prin, x,
						   ctxt, res, cbs );
		if(results == 2) sendMsg(res,2); // Finished, maximum calls reached
		if(results == 1) sendMsg(res,1);  // Finished, error tolerance reached.

}

// *********************************
// Test run optimizer
 public double testRun(int n, double[] xparams, int maxItr, double errTol,
 			double maxStep, double t0, int prin) throws Xcept {
	 int nx = n;         // # of pars to optimize
	 this.maxfn = maxItr; // Maximum number of simulation runs
	 this.errTol = errTol; // mininum error value tolerance
	 this.test = true;			// testing
	 this.bounded = false;	// Use unbounded optimization:
	 this.xmin = new double[nx];
	 this.xmax = new double[nx];
	 for (int i=0; i<nx; i++) {
		 this.xmin[i] = Double.NaN;;
		 this.xmax[i] = Double.NaN;;
	 }
	 double[] x = xparams;  // Starting values for each parameter
 	 double h0 = maxStep;   // Maximum step size.

	 // NOT used:
	 RTContext ctxt = null;
	 OptimResults res = null;
	 OptimCallbacks cbs = null;

	 double results = praxis ( maxfn, t0, h0,  nx, prin, x,
							ctxt, res, cbs );

	return results;
}

//****************************************************************************


//****************************************************************************
//
//  Purpose:
//
//    FLIN is the function of one variable to be minimized by MINNY.
//
//  Discussion:
//
//    F(X) is a scalar function of a vector argument X.
//
//    A minimizer of F(X) is sought along a line or parabola.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2016
//
//  Author:
//
//    Original FORTRAN77 version by Richard Brent.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Richard Brent,
//    Algorithms for Minimization without Derivatives,
//    Prentice Hall, 1973,
//    Reprinted by Dover, 2002.
//
//  Parameters:
//
//    Input, int N, the number of variables.
//
//    Input, int JSEARCH, indicates the kind of search.
//    If JSEARCH is a legal column index, linear search along V(*,JSEARCH).
//    If JSEARCH is -1, then the search is parabolic, based on X, Q0 and Q1.
//
//    Input, double L, is the parameter determining the particular
//    point at which F is to be evaluated.
//    For a linear search, L is the step size.
//    For a quadratic search, L is a parameter which specifies
//    a point in the plane of X, Q0 and Q1.
//
//    Input, double F ( double X[], int N ), the function to be minimized.
//
//    Input, double X[N], the base point of the search.
//
//    Input/output, int &NF, the function evaluation counter.
//
//    Input, double V[N,N], a matrix whose columns constitute
//    search directions.
//
//    Input, double Q0[N], Q1[N], two auxiliary points used to
//    determine the plane when a quadratic search is performed.
//
//    Input, double &QD0, &QD1, values needed to compute the
//    coefficients QA, QB, QC.
//
//    Output, double &QA, &QB, &QC, coefficients used to combine
//    Q0, X, and A1 if a quadratic search is used.
//
//    Output, double FLIN, the value of the function at the
//    minimizing point.
//
/*private double flin ( int n, int jsearch, double l, double f ( double x[], int n ),
  double x[], int &nf, double v[], double q0[], double q1[], double &qd0,
  double &qd1, double &qa, double &qb, double &qc,) */

private double[] flin ( int n, int jsearch, double l, double x[],
			double v[],	double q0[], double q1[], double qd0,
		  double qd1, double qa, double qb, double qc,
			RTContext ctxt, OptimResults res, OptimCallbacks cbs)
throws Xcept {
	double[] flinOut = new double[6]; // array of values to be returned.
	Arrays.fill(flinOut,0);
  int i;
  double[] t = new double[n];
  double value;
//
//  The search is linear.
//
  if ( 0 <= jsearch )
  {
    for ( i = 0; i < n; i++ )
    {
      t[i] = x[i] + l * v[i+jsearch*n];
    }
  }
//
//  The search is along a parabolic space curve.
//
  else
  {
    qa =                 l * ( l - qd1 ) /       ( qd0 + qd1 ) / qd0;
    qb = - ( l + qd0 ) *     ( l - qd1 ) / qd1                 / qd0;
    qc =   ( l + qd0 ) * l               / qd1 / ( qd0 + qd1 );

    for ( i = 0; i < n; i++ )
    {
      t[i] = qa * q0[i] + qb * x[i] + qc * q1[i];
    }
  }
//
//  The function evaluation counter NF is incremented.
//
	this.nf++;
//
//  Evaluate the function.
//
	if(this.test == true) {
		value = this.testFunction(t, n);
	}
	else {
		if(this.bounded == true) {
			for(i=0;i<n;i++) {
				if(t[i] < this.xmin[i]) t[i] = this.xmin[i];
				else if (t[i] > this.xmax[i]) {

					t[i] = this.xmax[i];
				}
			}
		}
		value = cbs.calcError(ctxt,t,res);
	}

	flinOut[0] = qd0;
	flinOut[1] = qd1;
	flinOut[2] = qa;
	flinOut[3] = qb;
	flinOut[4] = qc;
	flinOut[5] = value;

	return flinOut;
}
//****************************************************************************80


//****************************************************************************80
//
//  Purpose:
//
//    MINFIT computes the singular value decomposition of an N by N array.
//
//  Discussion:
//
//    This is an improved version of the EISPACK routine MINFIT
//    restricted to the case M = N and P = 0.
//
//    The singular values of the array A are returned in Q.  A is
//    overwritten with the orthogonal matrix V such that U * diag(Q) = A * V,
//    where U is another orthogonal matrix.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2016
//
//  Author:
//
//    Original FORTRAN77 version by Richard Brent.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Richard Brent,
//    Algorithms for Minimization without Derivatives,
//    Prentice Hall, 1973,
//    Reprinted by Dover, 2002.
//
//    James Wilkinson, Christian Reinsch,
//    Handbook for Automatic Computation,
//    Volume II, Linear Algebra, Part 2,
//    Springer Verlag, 1971.
//
//    Brian Smith, James Boyle, Jack Dongarra, Burton Garbow, Yasuhiko Ikebe,
//    Virginia Klema, Cleve Moler,
//    Matrix Eigensystem Routines, EISPACK Guide,
//    Lecture Notes in Computer Science, Volume 6,
//    Springer Verlag, 1976,
//    ISBN13: 978-3540075462,
//    LC: QA193.M37.
//
//  Parameters:
//
//    Input, int N, the order of the matrix A.
//
//    Input, double TOL, a tolerance which determines when a vector
//    (a column or part of a column of the matrix) may be considered
//    "essentially" equal to zero.
//
//    Input/output, double A[N,N].  On input, an N by N array whose
//    singular value decomposition is desired.  On output, the
//    SVD orthogonal matrix factor V.
//
//    Input/output, double Q[N], the singular values.
//
//public void minfit ( int n, double tol, double a[], double q[] )
public double[] minfit ( int n, double tol, double q[],	OptimResults res )// a[] ->v[] return q[]
{
  double c;
  double[] e;
  double eps;
  double f = 0;
  double g;
  double h;
  int i;
  int ii;
  int j;
  int jj;
  int k;
  int kk;
  int kt;
  int kt_max = 30;
  int l;
  int l2;
  int ll2;
  double s;
	boolean skip = false;
  double temp;
  double x;
  double y;
  double z;
//
//  Householder's reduction to bidiagonal form.
//
  if ( n == 1 )
  {
    q[0] = this.v[0+0*n];//a[0+0*n];
    this.v[0+0*n] = 1.0;//a[0+0*n] = 1.0;
		return q;
  }

  e = new double[n];

  eps = r8_epsilon ( );
  g = 0.0;
  x = 0.0;

  for ( i = 1; i <= n; i++ )
  {
    e[i-1] = g;
    l = i + 1;
    s = 0.0;
    for ( ii = i; ii <= n; ii++ )
    {
//      s = s + a[ii-1+(i-1)*n] * a[ii-1+(i-1)*n];
		s = s + this.v[ii-1+(i-1)*n] * this.v[ii-1+(i-1)*n];
    }
    g = 0.0;
    if ( tol <= s )
    {
      //f = a[i-1+(i-1)*n];
	  f = this.v[i-1+(i-1)*n];
      g = Math.sqrt( s );
      if ( 0.0 <= f )
      {
        g = - g;
      }
      h = f * g - s;
      //a[i-1+(i-1)*n] = f - g;
			this.v[i-1+(i-1)*n] = f - g;
      for ( j = l; j <= n; j++ )
      {
        f = 0.0;
        for ( ii = i; ii <= n; ii++ )
        {
//          f = f + a[ii-1+(i-1)*n] * a[ii-1+(j-1)*n];
			f = f + this.v[ii-1+(i-1)*n] * this.v[ii-1+(j-1)*n];
        }
        f = f / h;
        for ( ii = i; ii <= n; ii++ )
        {
//          a[ii-1+(j-1)*n] = a[ii-1+(j-1)*n] + f * a[ii-1+(i-1)*n];
			this.v[ii-1+(j-1)*n] = this.v[ii-1+(j-1)*n] + f * this.v[ii-1+(i-1)*n];
        }
      }
    }

    q[i-1] = g;
    s = 0.0;
    for ( j = l; j <= n; j++ )
    {
  //    s = s + a[i-1+(j-1)*n] * a[i-1+(j-1)*n];
		s = s + this.v[i-1+(j-1)*n] * this.v[i-1+(j-1)*n];
    }
    g = 0.0;
    if ( tol <= s )
    {
      if ( i < n )
      {
//        f = a[i-1+i*n];
		  f = this.v[i-1+i*n];
      }
      g = Math.sqrt( s );
      if ( 0.0 <= f )
      {
        g = - g;
      }
      h = f * g - s;
      if ( i < n )
      {
//        a[i-1+i*n] = f - g;
		  this.v[i-1+i*n] = f - g;
        for ( jj = l; jj <= n; jj++ )
        {
//          e[jj-1] = a[i-1+(jj-1)*n] / h;
			e[jj-1] = this.v[i-1+(jj-1)*n] / h;
        }
        for ( j = l; j <= n; j++ )
        {
          s = 0.0;
          for ( jj = l; jj <= n; jj++ )
          {
//            s = s + a[j-1+(jj-1)*n] * a[i-1+(jj-1)*n];
			  s = s + this.v[j-1+(jj-1)*n] * this.v[i-1+(jj-1)*n];
          }
          for ( jj = l; jj <= n; jj++ )
          {
//            a[j-1+(jj-1)*n] = a[j-1+(jj-1)*n] + s * e[jj-1];
			  this.v[j-1+(jj-1)*n] = this.v[j-1+(jj-1)*n] + s * e[jj-1];
          }
        }
      }
    }

    y = Math.abs( q[i-1] ) + Math.abs( e[i-1] );
    x = r8_max ( x, y );
  }
//
//  Accumulation of right-hand transformations.
//
//  a[n-1+(n-1)*n] = 1.0;
	this.v[n-1+(n-1)*n] = 1.0;
  g = e[n-1];
  l = n;
  for ( i = n - 1; 1 <= i; i-- )
  {
    if ( g != 0.0 )
    {
  //    h = a[i-1+i*n] * g;
		h = this.v[i-1+i*n] * g;
      for ( ii = l; ii <= n; ii++ )
      {
//        a[ii-1+(i-1)*n] = a[i-1+(ii-1)*n] / h;
		  this.v[ii-1+(i-1)*n] = this.v[i-1+(ii-1)*n] / h;
      }
      for ( j = l; j <= n; j++ )
      {
        s = 0.0;
        for ( jj = l; jj <= n; jj++ )
        {
  //        s = s + a[i-1+(jj-1)*n] * a[jj-1+(j-1)*n];
			s = s + this.v[i-1+(jj-1)*n] * this.v[jj-1+(j-1)*n];
        }
        for ( ii = l; ii <= n; ii++ )
        {
  //        a[ii-1+(j-1)*n] = a[ii-1+(j-1)*n] + s * a[ii-1+(i-1)*n];
			this.v[ii-1+(j-1)*n] = this.v[ii-1+(j-1)*n] + s * this.v[ii-1+(i-1)*n];
        }
      }
    }

    for ( jj = l; jj <= n; jj++ )
    {
//      a[i-1+(jj-1)*n] = 0.0;
		this.v[i-1+(jj-1)*n] = 0.0;
    }

    for ( ii = l; ii <= n; ii++ )
    {
//      a[ii-1+(i-1)*n] = 0.0;
		this.v[ii-1+(i-1)*n] = 0.0;
    }
//    a[i-1+(i-1)*n] = 1.0;
	this.v[i-1+(i-1)*n] = 1.0;
    g = e[i-1];
    l = i;
  }
//
//  Diagonalization of the bidiagonal form.
//
  eps = eps * x;

  for ( k = n; 1 <= k; k-- )
  {
    kt = 0;
    for ( ; ; )
    {
      kt = kt + 1;
      if ( kt_max < kt )
      {
        e[k-1] = 0.0;
		String minfitErrMsg = "MINFIT - Error!	The QR algorithm failed to converge.";
        termError(res,minfitErrMsg);
      }
      skip = false;
      for ( l2 = k; 1 <= l2; l2-- )
      {
        l = l2;
        if ( Math.abs( e[l-1] ) <= eps )
        {
          skip = true;
          break;
        }

        if ( 1 < l )
        {
          if ( Math.abs( q[l-2] ) <= eps )
          {
            break;
          }
        }
      }
//
//  Cancellation of E(L) if 1 < L.
//
      if ( ! skip )
      {
        c = 0.0;
        s = 1.0;
        for ( i = l; i <= k; i++ )
        {
          f = s * e[i-1];
          e[i-1] = c * e[i-1];
          if ( Math.abs( f ) <= eps )
          {
            break;
          }
          g = q[i-1];
//
//  q(i) = h = sqrt(g*g + f*f).
//
          h = r8_hypot ( f, g );
          q[i-1] = h;
          if ( h == 0.0 )
          {
            g = 1.0;
            h = 1.0;
          }
          c =   g / h;
          s = - f / h;
        }
      }
//
//  Test for convergence for this index K.
//
      z = q[k-1];
      if ( l == k )
      {
        if ( z < 0.0 )
        {
          q[k-1] = - z;
          for ( i = 1; i <= n; i++ )
          {
//            a[i-1+(k-1)*n] = - a[i-1+(k-1)*n];
			  this.v[i-1+(k-1)*n] = - this.v[i-1+(k-1)*n];
          }
        }
        break;
      }
//
//  Shift from bottom 2*2 minor.
//
      x = q[l-1];
      y = q[k-2];
      g = e[k-2];
      h = e[k-1];
      f = ( ( y - z ) * ( y + z ) + ( g - h ) * ( g + h ) ) / ( 2.0 * h * y );
      g = r8_hypot ( f, 1.0 );
      if ( f < 0.0 )
      {
        temp = f - g;
      }
      else
      {
        temp = f + g;
      }
      f = ( ( x - z ) * ( x + z ) + h * ( y / temp - h ) ) / x;
//
//  Next QR transformation.
//
      c = 1.0;
      s = 1.0;
      for ( i = l + 1; i <= k; i++ )
      {
        g = e[i-1];
        y = q[i-1];
        h = s * g;
        g = g * c;
        z = r8_hypot ( f, h );
        e[i-2] = z;
        if ( z == 0.0 )
        {
          f = 1.0;
          z = 1.0;
        }
        c = f / z;
        s = h / z;
        f =   x * c + g * s;
        g = - x * s + g * c;
        h = y * s;
        y = y * c;

        for ( j = 1; j <= n; j++ )
        {
//          x = a[j-1+(i-2)*n];
			x = this.v[j-1+(i-2)*n];
//          z = a[j-1+(i-1)*n];
			z = this.v[j-1+(i-1)*n];
//          a[j-1+(i-2)*n] =   x * c + z * s;
			this.v[j-1+(i-2)*n] =   x * c + z * s;
//          a[j-1+(i-1)*n] = - x * s + z * c;
			this.v[j-1+(i-1)*n] = - x * s + z * c;
        }
        z = r8_hypot ( f, h );
        q[i-2] = z;
        if ( z == 0.0 )
        {
          f = 1.0;
          z = 1.0;
        }
        c = f / z;
        s = h / z;
        f =   c * g + s * y;
        x = - s * g + c * y;
      }

      e[l-1] = 0.0;
      e[k-1] = f;
      q[k-1] = x;
    }
  }

  return q;
}
//****************************************************************************80


//****************************************************************************80
//
//  Purpose:
//
//    MINNY minimizes a scalar function of N variables along a line.
//
//  Discussion:
//
//    MINNY minimizes F along the line from X in the direction V(*,JSEARCH)
//    or else using a quadratic search in the plane defined by Q0, Q1 and X.
//
//    If FK = true, then F1 is FLIN(X1).  Otherwise X1 and F1 are ignored
//    on entry unless final FX is greater than F1.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2016
//
//  Author:
//
//    Original FORTRAN77 version by Richard Brent.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Richard Brent,
//    Algorithms for Minimization without Derivatives,
//    Prentice Hall, 1973,
//    Reprinted by Dover, 2002.
//
//  Parameters:
//
//    Input, int N, the number of variables.
//
//    Input, int JSEARCH, indicates the kind of search.
//    If J is a legal columnindex, linear search in the direction of V(*,JSEARCH).
//    Otherwise, the search is parabolic, based on X, Q0 and Q1.
//
//    Input, int NITS, the maximum number of times the interval
//    may be halved to retry the calculation.
//
//    Input/output, double &D2, is either zero, or an approximation to
//    the value of (1/2) times the second derivative of F.
//
//    Input/output, double &X1, on entry, an estimate of the
//    distance from X to the minimum along V(*,JSEARCH), or a curve.
//    On output, the distance between X and the minimizer that was found.
//
//    Input/output, double &F1, ?
//
//    Input, bool FK; if FK is TRUE, then on input F1 contains
//    the value FLIN(X1).
//
//    Input, double F ( double X[], int N ), is the name of the function to
//    be minimized.
//
//    Input/output, double X[N], ?
//
//    Input, double T, ?
//
//    Input, double H, ?
//
//    Input, double V[N,N], a matrix whose columns are direction
//    vectors along which the function may be minimized.
//
//    ?, double Q0[N], ?
//
//    ?, double Q1[N], ?
//
//    Input/output, int &NL, the number of linear searches.
//
//    Input/output, int &NF, the number of function evaluations.
//
//    Input, double DMIN, an estimate for the smallest eigenvalue.
//
//    Input, double LDT, the length of the step.
//
//    Input/output, double &FX, the value of F(X,N).
//
//    Input/output, double &QA, &QB, &QC;
//
//    Input/output, double &QD0, &QD1, ?.
//

/*public void minny ( int n, int jsearch, int nits, double &d2, double &x1, double &f1,
  bool fk, double f ( double x[], int n ), double x[], double t, double h,
  double v[], double q0[], double q1[], int &nl, int &nf, double dmin,
  double ldt, double &fx, double &qa, double &qb, double &qc, double &qd0,
  double &qd1 ) */
public double[] minny ( int n, int jsearch, int nits, double d2, double x1, double f1,
  boolean fk, double x[], double t, double h,
  double v[], double q0[], double q1[], int nl, double dmin,
  double ldt, double fx, double qa, double qb, double qc, double qd0,
  double qd1, double t0, RTContext ctxt,	OptimResults res, OptimCallbacks cbs  )
throws Xcept {
  double d1;
	boolean dz;
  double f0;
  double f2;
  double fm;
  int i;
  int k;
  double m2;
  double m4;
  double machep;
	boolean ok;
  double s;
  double sf1;
  double small;
  double sx1;
  double t2;
  double temp;
  double x2;
  double xm;

	double[] returnArray = new double[10]; // array of values to be returned.
	Arrays.fill(returnArray,0);

  machep = r8_epsilon ( );
  small = machep * machep;
  m2 = Math.sqrt( machep );
  m4 = Math.sqrt( m2 );
  sf1 = f1;
  sx1 = x1;
  k = 0;
  xm = 0.0;
  fm = fx;
  f0 = fx;
  dz = ( d2 < machep );
//
//  Find the step size.
//
  s = r8vec_norm ( n, x );
  if ( dz )
  {
    temp = dmin;
  }
  else
  {
    temp = d2;
  }
  t2 = m4 * Math.sqrt( Math.abs( fx ) / temp + s * ldt ) + m2 * ldt;
  s = m4 * s + t;
  if ( dz && s < t2 )
  {
    t2 = s;
  }
  t2 = r8_max ( t2, small );
  t2 = r8_min ( t2, 0.01 * h );
  if ( fk && f1 <= fm )
  {
    xm = x1;
    fm = f1;
  }
  if ( ( ! fk ) || Math.abs( x1 ) < t2 )
  {
    if ( 0.0 <= x1 )
    {
      temp = 1.0;
    }
    else
    {
      temp = - 1.0;
    }
    x1 = temp * t2;
		double[] flinOut = new double[6]; // array of values to returned by flin.
		Arrays.fill(flinOut,0);
  //  f1 = flin ( n, jsearch, x1, f, x, nf, v, q0, q1, qd0, qd1, qa, qb, qc );
		flinOut = flin ( n, jsearch, x1, x, v, q0, q1, qd0, qd1, qa, qb, qc, ctxt, res, cbs );
		qd0 = flinOut[0];
		qd1 = flinOut[1];
		qa = flinOut[2];
		qb = flinOut[3];
		qc = flinOut[4];
		f1 = flinOut[5];
		if( this.nf > maxfn) {
			returnArray[0] = -1.0;
			sendMsg(res,2);
			return returnArray;
		}
		if( f1 < this.errTol) {
			returnArray[0] = -2.0;
			sendMsg(res,1);
			return returnArray;
		}
  }
  if ( f1 <= fm )
  {
    xm = x1;
    fm = f1;
  }
//
//  Evaluate FLIN at another point and estimate the second derivative.
//
  for ( ; ; )
  {
    if ( dz )
    {
      if ( f1 <= f0 )
      {
        x2 = 2.0 * x1;
      }
      else
      {
        x2 = - x1;
      }
			double[] flinOut = new double[6]; // array of values to be returned.
			Arrays.fill(flinOut,0);
//      f2 = flin ( n, jsearch, x2, f, x, nf, v, q0, q1, qd0, qd1, qa, qb, qc );
			flinOut = flin ( n, jsearch, x2, x, v, q0, q1, qd0, qd1, qa, qb, qc, ctxt, res, cbs );
			qd0 = flinOut[0];
			qd1 = flinOut[1];
			qa = flinOut[2];
			qb = flinOut[3];
			qc = flinOut[4];
			f2 = flinOut[5];

			if( this.nf > maxfn) {
				returnArray[0] = -1.0;
				sendMsg(res,2);
				return returnArray;
			}
			if( f2 < this.errTol) {		// error tol met
				returnArray[0] = -2.0;
				sendMsg(res,1);
				return returnArray;
			}
      if ( f2 <= fm )
      {
        xm = x2;
        fm = f2;
      }
      d2 = ( x2 * ( f1 - f0 ) - x1 * ( f2 - f0 ) )
        / ( ( x1 * x2 ) * ( x1 - x2 ) );
    }
//
//  Estimate the first derivative at 0.
//
    d1 = ( f1 - f0 ) / x1 - x1 * d2;
    dz = true;
//
//  Predict the minimum.
//
    if ( d2 <= small )
    {
      if ( 0.0 <= d1 )
      {
        x2 = - h;
      }
      else
      {
        x2 = h;
      }
    }
    else
    {
      x2 = ( - 0.5 * d1 ) / d2;
    }
    if ( h < Math.abs( x2 ) )
    {
      if ( x2 <= 0.0 )
      {
        x2 = - h;
      }
      else
      {
        x2 = h;
      }
    }
//
//  Evaluate Function at the predicted minimum.
//
		ok = true;
    for ( ; ; )
    {
			double[] flinOut = new double[6]; // array of values to be returned.
			Arrays.fill(flinOut,0);
//      f2 = flin ( n, jsearch, x2, f, x, nf, v, q0, q1, qd0, qd1, qa, qb, qc );
			flinOut = flin ( n, jsearch, x2, x, v, q0, q1, qd0, qd1, qa, qb, qc, ctxt, res, cbs );
			qd0 = flinOut[0];
			qd1 = flinOut[1];
			qa = flinOut[2];
			qb = flinOut[3];
			qc = flinOut[4];
			f2 = flinOut[5];
			if( this.nf > maxfn) {
				returnArray[0] = -1.0;
				sendMsg(res,2);
				return returnArray;
			}
			if( f2 < this.errTol) {
				returnArray[0] = -2.0;
				sendMsg(res,1);
				return returnArray;
			}
      if ( nits <= k || f2 <= f0 )
      {
		  	break; // out of for loop.
      }
      k = k + 1;
      if ( f0 < f1 && 0.0 < x1 * x2 )
      {
				ok = false;
				break; // out of for loop.
      }
      x2 = 0.5 * x2;
    }
    if ( ok )
    {
			break; // out of for loop.
    }
  }
//
//  Increment the one-dimensional search counter.
//
  nl = nl + 1; // the number of linear searches.
  if ( fm < f2 )
  {
    x2 = xm;
  }
  else
  {
    fm = f2;
  }
//
//  Get a new estimate of the second derivative.
//
  if ( small < Math.abs( x2 * ( x2 - x1 ) ) )
  {
    d2 = ( x2 * ( f1 - f0 ) - x1 * ( fm - f0 ) )
      / ( ( x1 * x2 ) * ( x1 - x2 ) );
  }
  else
  {
    if ( 0 < k )
    {
      d2 = 0.0;
    }
  }

  d2 = r8_max ( d2, small );
  x1 = x2;
  fx = fm;
  if ( sf1 < fx )
  {
    fx = sf1;
    x1 = sx1;
  }
//
//  Update X for linear search.
//
  if ( 0 <= jsearch )
  {
    for ( i = 0; i < n; i++ )
    {
      x[i] = x[i] + x1 * v[i+jsearch*n];
    }
  }
	// Fill up return array:
	returnArray[0] = d2;
	returnArray[1] = x1;
	returnArray[2] = f1;
	returnArray[3] = (double)nl;
	returnArray[4] = fx;
	returnArray[5] = qa;
	returnArray[6] = qb;
	returnArray[7] = qc;
	returnArray[8] = qd0;
	returnArray[9] = qd1;

  return returnArray;
}
//****************************************************************************80


//****************************************************************************80
//
//  Purpose:
//
//    PRAXIS seeks an N-dimensional minimizer X of a scalar function F(X).
//
//  Discussion:
//
//    PRAXIS returns the minimum of the function F(X,N) of N variables
//    using the principal axis method.  The gradient of the function is
//    not required.
//
//    The approximating quadratic form is
//
//      Q(x") = F(x,n) + (1/2) * (x"-x)" * A * (x"-x)
//
//    where X is the best estimate of the minimum and
//
//      A = inverse(V") * D * inverse(V)
//
//    V(*,*) is the matrix of search directions;
//    D(*) is the array of second differences.
//
//    If F(X) has continuous second derivatives near X0, then A will tend
//    to the hessian of F at X0 as X approaches X0.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2016
//
//  Author:
//
//    Original FORTRAN77 version by Richard Brent.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Richard Brent,
//    Algorithms for Minimization without Derivatives,
//    Prentice Hall, 1973,
//    Reprinted by Dover, 2002.
//
//  Parameters:
//
//    Input, double T0, is a tolerance.  PRAXIS attempts to return
//    praxis = f(x) such that if X0 is the true local minimum near X, then
//    norm ( x - x0 ) < T0 + sqrt ( EPSILON ( X ) ) * norm ( X ),
//    where EPSILON ( X ) is the machine precision for X.
//
//    Input, double H0, is the maximum step size.  H0 should be
//    set to about the maximum distance from the initial guess to the minimum.
//    If H0 is set too large or too small, the initial rate of
//    convergence may be slow.
//
//    Input, int N, the number of variables.
//
//    Input, int PRIN, controls printing intermediate results.
//    0, nothing is printed.
//    1, F is printed after every n+1 or n+2 linear minimizations.
//       final X is printed, but intermediate X is printed only
//       if N is at most 4.
//    2, the scale factors and the principal values of the approximating
//       quadratic form are also printed.
//    3, X is also printed after every few linear minimizations.
//    4, the principal vectors of the approximating quadratic form are
//       also printed.
//
//    Input/output, double X[N], is an array containing on entry a
//    guess of the point of minimum, on return the estimated point of minimum.
//
//    Input, double F ( double X[], int N ), is the name of the function to be
//    minimized.
//
//    Output, double PRAXIS, the function value at the minimizer.
//
//  Local parameters:
//
//    Local, double DMIN, an estimate for the smallest eigenvalue.
//
//    Local, double FX, the value of F(X,N).
//
//    Local, bool ILLC, is TRUE if the system is ill-conditioned.
//
//    Local, double LDT, the length of the step.
//
//    Local, int NF, the number of function evaluations.
//
//    Local, int NL, the number of linear searches.
//

// t0: tolerance
// h0: maximum step size.
// n: number of paramters to fit,
// prin: printing ...
// x[]: current guess for each paramter.
public double praxis ( int maxfn, double t0, double h0, int n, int prin, double x[],
								RTContext ctxt, OptimResults res, OptimCallbacks cbs )
throws Xcept {
	double d[] = new double[n];
	Arrays.fill(d, 0.0);
	double d2;
  double df;
  double dmin;
  double dn;
  double dni;
  double f1;
  boolean fk;
  double fx;
  double h;
  int i;
  int ii;
  int j;
  int jsearch;
  int k;
  int k2;
  int kl;
  int kt;
  double large;
  double ldfac;
  double lds;
  double ldt;
  double m2;
  double m4;
  double machep;
  int nits;
  int nl;
  double q0[] = new double[n];
  double q1[] = new double[n];
	double qa;
  double qb;
  double qc;
	double qd0;
  double qd1;
  double qf1;
  double r;
  double s;
  double scbd;
  int seed;
  double sf;
  double sl;
  double small;
  double t;
  double temp;
  double t2;
  double value;
  double vlarge;
  double vsmall;
  double y[] = new double[n];
  double z[] = new double[n];
  v = new double[n*n];
  Arrays.fill(v, 0.0);
//
//  Initialization.
//
  machep = r8_epsilon();
  small = machep * machep;
  vsmall = small * small;
  large = 1.0 / small;
  vlarge = 1.0 / vsmall;
  m2 = Math.sqrt( machep );
  m4 = Math.sqrt( m2 );
  seed = 123456789;
//
//  Heuristic numbers:
//
//  If the axes may be badly scaled (which is to be avoided if
//  possible), then set SCBD = 10.  Otherwise set SCBD = 1.
//
//  If the problem is known to be ill-conditioned, initialize ILLC = true.
//
//  KTM is the number of iterations without improvement before the
//  algorithm terminates.  KTM = 4 is very cautious; usually KTM = 1
//  is satisfactory.
//
  scbd = 1.0;
  if ( this.illc )
  {
    ldfac = 0.1;
  }
  else
  {
    ldfac = 0.01;
  }
  kt = 0;
  nl = 0;
	this.nf++ ;
	if(this.test == true) {
		fx = this.testFunction(x, n);
	}
	else {
		if(this.bounded == true) {
			for(i=0;i<n;i++) {
				if(x[i] < this.xmin[i]) x[i] = this.xmin[i];
				else if (x[i] > this.xmax[i])	x[i] = this.xmax[i];
			}
		}
  	fx = cbs.calcError(ctxt,x,res);  // x[] is param value array
	}
	if( this.nf > maxfn) {
		sendMsg(res,2);
		return 2.0;
	}
	if(fx < this.errTol) { 			// If error less than errTol then stop.
		sendMsg(res,1);
		return 1.0;
	}
  qf1 = fx;
  t = small + Math.abs( t0 );
  t2 = t;
  dmin = small;
  h = h0;
  h = r8_max( h, 100.0 * t );
  ldt = h;
//
//  The initial set of search directions V is the identity matrix.
//
  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < n; i++ )
    {
      v[i+j*n] = 0.0;
    }
    v[j+j*n] = 1.0;
  }
  for ( i = 0; i < n; i++ )
  {
    d[i] = 0.0;
  }
  qa = 0.0;
  qb = 0.0;
  qc = 0.0;
  qd0 = 0.0;
  qd1 = 0.0;
  q0 = r8vec_copy(n,x);
  q1 = r8vec_copy(n,x);
  if ( 0 < prin )
  {
    print2 ( n, x, prin, fx, nf, nl );
  }
//
//  The main loop starts here.
//
  for (int m=0 ; m< this.maxfn; m++ )  // <-- loop until someone complains . Use max number of runs check.
  {
    sf = d[0];
    d[0] = 0.0;
//
//  Minimize along the first direction V(*,1).
//
    jsearch = 0;
    nits = 2;
    d2 = d[0];
    s = 0.0;
    value = fx;
    fk = false;
		double[] minnyOut = new double[10]; // array of values to be returned.
		Arrays.fill(minnyOut,0);
	//   this.minny ( n, jsearch, nits, d2, s, value, fk, f, x, t,
    //  h, v, q0, q1, nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 );
   	minnyOut = minny ( n, jsearch, nits, d2, s, value, fk,/* f,*/ x, t,
      h, v, q0, q1, nl, dmin, ldt, fx, qa, qb, qc, qd0, qd1, t0, ctxt, res, cbs );
		if (minnyOut[0] == -1.0){
			String minfitExitMsg = "# of calls";
      term(res,minfitExitMsg);
			return 2;
		}
		if (minnyOut[0] == -2.0){
			String minfitExitMsg = "Min RMS error";
			term(res,minfitExitMsg);
			return 1;
		}
		d2 = minnyOut[0];
		s = minnyOut[1];
		value = minnyOut[2];
		nl = (int)minnyOut[3];
		fx = minnyOut[4];
		qa = minnyOut[5];
		qb = minnyOut[6];
		qc = minnyOut[7];
		qd0 = minnyOut[8];
		qd1 = minnyOut[9];
    d[0] = d2;
    if ( s <= 0.0 )
    {
      for ( i = 0; i < n; i++ )
      {
        v[i+0*n] = - v[i+0*n];
      }
    }
    if ( sf <= 0.9 * d[0] || d[0] <= 0.9 * sf )
    {
      for ( i = 1; i < n; i++ )
      {
        d[i] = 0.0;
      }
    }
//
//  The inner loop starts here.
//
    for ( k = 2; k <= n; k++ )
    {
  		y = this.r8vec_copy ( n, x );
      sf = fx;
      if ( 0 < kt )
      {
        this.illc = true;
      }
      for ( ; ; )
      {
        kl = k;
        df = 0.0;
//
//  A random step follows, to avoid resolution valleys.
//
        if ( this.illc )
        {
          for ( j = 0; j < n; j++ )
          {
      //      r = r8_uniform_01( seed, res );
						r = ThreadLocalRandom.current().nextDouble();
            s = ( 0.1 * ldt + t2 * Math.pow( 10.0, kt ) ) * ( r - 0.5 );
            z[j] = s;
            for ( i = 0; i < n; i++ )
            {
              x[i] = x[i] + s * v[i+j*n];
            }
          }
					this.nf++;
					if(this.test == true) {
						fx = this.testFunction(x, n);
					}
					else {
						if(this.bounded == true) {
							for(int q=0;q<n;q++) {
								if(x[q] < this.xmin[q]) x[q] = this.xmin[q];
								else if (x[q] > this.xmax[q]){
								x[q] = this.xmax[q];
								}
							}
						}
		  			fx = cbs.calcError(ctxt,x,res);  // x[] is param value array
						if(fx < this.errTol) { 		// stop optimization
							sendMsg(res,1);
							return 1.0;
						}
					}

				}
//
//  Minimize along the "non-conjugate" directions V(*,K),...,V(*,N).
//
        for ( k2 = k; k2 <= n; k2++ )
        {
          sl = fx;
          jsearch = k2 - 1;
          nits = 2;
          d2 = d[k2-1];
          s = 0.0;
          value = fx;
          fk = false;
					Arrays.fill(minnyOut,0);
          // minny ( n, jsearch, nits, d2, s, value, fk, f, x, t,
          //  h, v, q0, q1, nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 );
					minnyOut = minny ( n, jsearch, nits, d2, s, value, fk,/* f,*/ x, t,
							h, v, q0, q1, nl, dmin, ldt, fx, qa, qb, qc, qd0, qd1, t0, ctxt, res, cbs );
					if (minnyOut[0] == -1.0){
						String minfitExitMsg = "# of calls";
		        term(res,minfitExitMsg);
						return 2;
					}
					if (minnyOut[0] == -2.0){
						String minfitExitMsg = "Min RMS error";
						term(res,minfitExitMsg);
						return 1;
					}
					d2 = minnyOut[0];
					s = minnyOut[1];
					value = minnyOut[2];
					nl = (int)minnyOut[3];
					fx = minnyOut[4];
					qa = minnyOut[5];
					qb = minnyOut[6];
					qc = minnyOut[7];
					qd0 = minnyOut[8];
					qd1 = minnyOut[9];
          d[k2-1] = d2;
          if ( this.illc )
          {
            s = d[k2-1] * Math.pow( s + z[k2-1], 2 );
          }
          else
          {
            s = sl - fx;
          }
          if ( df <= s )
          {
            df = s;
            kl = k2;
          }
        }
//
//  If there was not much improvement on the first try, set
//  ILLC = true and start the inner loop again.
//
        if ( this.illc )
        {
          break;
        }
        if ( Math.abs( 100.0 * machep * fx ) <= df )
        {
          break;
        }
        this.illc = true;
      }

      if ( k == 2 && 1 < prin )
      {
        r8vec_print ( n, d, "  The second difference array:" );
      }
//
//  Minimize along the "conjugate" directions V(*,1),...,V(*,K-1).
//
      for ( k2 = 1; k2 < k; k2++ )
      {
        jsearch = k2 - 1;
        nits = 2;
        d2 = d[k2-1];
        s = 0.0;
        value = fx;
        fk = false;
				Arrays.fill(minnyOut,0);
  //      minny ( n, jsearch, nits, d2, s, value, fk, f, x, t,
//          h, v, q0, q1, nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 );
				minnyOut = minny ( n, jsearch, nits, d2, s, value, fk, /*f,*/ x, t,
					h, v, q0, q1, nl, dmin, ldt, fx, qa, qb, qc, qd0, qd1, t0,
					ctxt,	res, cbs  );
				if (minnyOut[0] == -1.0){
					String minfitExitMsg = "# of calls";
		      term(res,minfitExitMsg);
					return 2;
				}
				if (minnyOut[0] == -2.0){
					String minfitExitMsg = "Min RMS error";
					term(res,minfitExitMsg);
					return 1;
				}
				d2 = minnyOut[0];
				s = minnyOut[1];
				value = minnyOut[2];
				nl = (int)minnyOut[3];
				fx = minnyOut[4];
				qa = minnyOut[5];
				qb = minnyOut[6];
				qc = minnyOut[7];
				qd0 = minnyOut[8];
				qd1 = minnyOut[9];
        d[k2-1] = d2;
      }
      f1 = fx;
      fx = sf;
      for ( i = 0; i < n; i++ )
      {
        temp = x[i];
        x[i] = y[i];
        y[i] = temp - y[i];
      }
      lds = r8vec_norm ( n, y );
//
//  Discard direction V(*,kl).
//
//  If no random step was taken, V(*,KL) is the "non-conjugate"
//  direction along which the greatest improvement was made.
//
      if ( small < lds )
      {
        for ( j = kl - 1; k <= j; j-- )
        {
          for ( i = 1; i <= n; i++ )
          {
            v[i-1+j*n] = v[i-1+(j-1)*n];
          }
          d[j] = d[j-1];
        }
        d[k-1] = 0.0;
        for ( i = 1; i <= n; i++ )
        {
          v[i-1+(k-1)*n] = y[i-1] / lds;
        }
//
//  Minimize along the new "conjugate" direction V(*,k), which is
//  the normalized vector:  (new x) - (old x).
//
        jsearch = k - 1;
        nits = 4;
        d2 = d[k-1];
        value = f1;
        fk = true;
				Arrays.fill(minnyOut,0);
        minnyOut = minny ( n, jsearch, nits, d2, lds, value, fk,/* f,*/ x, t, h,	v,
				q0, q1, nl, dmin, ldt, fx, qa, qb, qc, qd0, qd1, t0, ctxt, res, cbs );
				if (minnyOut[0] == -1.0){
					String minfitExitMsg = "# of calls";
	        term(res,minfitExitMsg);
					return 2;
				}
				if (minnyOut[0] == -2.0){
					String minfitExitMsg = "Min RMS error";
					term(res,minfitExitMsg);
					return 1;
				}
				d2 = minnyOut[0];
				lds = minnyOut[1];
				value = minnyOut[2];
				nl = (int)minnyOut[3];
				fx = minnyOut[4];
				qa = minnyOut[5];
				qb = minnyOut[6];
				qc = minnyOut[7];
				qd0 = minnyOut[8];
				qd1 = minnyOut[9];
        d[k-1] = d2;
        if ( lds <= 0.0 )
        {
          lds = - lds;
          for ( i = 1; i <= n; i++ )
          {
            v[i-1+(k-1)*n] = - v[i-1+(k-1)*n];
          }
        }
      }

      ldt = ldfac * ldt;
      ldt = r8_max ( ldt, lds );
      if ( 0 < prin )
      {
        print2 ( n, x, prin, fx, nf, nl );
      }
      t2 = r8vec_norm ( n, x );
      t2 = m2 * t2 + t;
//
//  See whether the length of the step taken since starting the
//  inner loop exceeds half the tolerance.
//
      if ( 0.5 * t2 < ldt )
      {
        kt = - 1;
      }
      kt = kt + 1;
      if ( this.ktm < kt )
      {
        if ( 0 < prin )
        {
          r8vec_print ( n, x, "  X:" );
        }
        return fx;
      }
    }
//
//  The inner loop ends here.
//
//  Try quadratic extrapolation in case we are in a curved valley.
//
//	quad ( n, f, x, t, h, v, q0, q1, nl, nf, dmin, ldt, fx, qf1,
//	qa, qb, qc, qd0, qd1 );
		double[] quadReturn = new double[8]; // Holds return values...
		Arrays.fill(quadReturn,0);
    quadReturn = quad ( n, x, t, h, v, q0, q1, nl, dmin, ldt, fx, qf1,
      qa, qb, qc, qd0, qd1, t0, ctxt,	res, cbs );
		nl = (int)quadReturn[0];
		fx = quadReturn[1];
		qf1 = quadReturn[2];
		qa = quadReturn[3];
		qb = quadReturn[4];
		qc = quadReturn[5];
		qd0 = quadReturn[6];
		qd1 = quadReturn[7];

    for ( j = 0; j < n; j++ )
    {
      d[j] = 1.0 / Math.sqrt( d[j] );
    }
    dn = r8vec_max ( n, d );
    if ( 3 < prin )
    {
      r8mat_print ( n, n, v, "  The new direction vectors:" );
    }
    for ( j = 0; j < n; j++ )
    {
      for ( i = 0; i < n; i++ )
      {
        v[i+j*n] = ( d[j] / dn ) * v[i+j*n];
      }
    }
//
//  Scale the axes to try to reduce the condition number.
//
    if ( 1.0 < scbd )
    {
      for ( i = 0; i < n; i++ )
      {
        s = 0.0;
        for ( j = 0; j < n; j++ )
        {
          s = s + v[i+j*n] * v[i+j*n];
        }
        s = Math.sqrt( s );
        z[i] = r8_max ( m4, s );
      }
      s = r8vec_min ( n, z );
      for ( i = 0; i < n; i++ )
      {
        sl = s / z[i];
        z[i] = 1.0 / sl;
        if ( scbd < z[i] )
        {
          sl = 1.0 / scbd;
          z[i] = scbd;
        }
        for ( j = 0; j < n; j++ )
        {
          v[i+j*n] = sl * v[i+j*n];
        }
      }
    }
//
//  Calculate a new set of orthogonal directions before repeating
//  the main loop.
//
//  Transpose V for MINFIT:
//
    r8mat_transpose_in_place ( n, v );
//
//  MINFIT finds the singular value decomposition of V.
//
//  This gives the principal values and principal directions of the
//  approximating quadratic form without squaring the condition number.
//
    d = minfit ( n, vsmall, /*v,*/ d, res );
//
//  Unscale the axes.
//
    if ( 1.0 < scbd )
    {
      for ( i = 0; i < n; i++ )
      {
        for ( j = 0; j < n; j++ )
        {
          v[i+j*n] = z[i] * v[i+j*n];
        }
      }

      for ( j = 0; j < n; j++ )
      {
        s = 0.0;
        for ( i = 0; i < n; i++ )
        {
          s = s + v[i+j*n] * v[i+j*n];
        }
        s = Math.sqrt( s );
        d[j] = s * d[j];
        for ( i = 0; i < n; i++ )
        {
          v[i+j*n] = v[i+j*n] / s;
        }
      }
    }

    for ( i = 0; i < n; i++ )
    {
      dni = dn * d[i];
      if ( large < dni )
      {
        d[i] = vsmall;
      }
      else if ( dni < small )
      {
        d[i] = vlarge;
      }
      else
      {
        d[i] = 1.0 / dni / dni;
      }
    }
//
//  Sort the eigenvalues and eigenvectors.
//
		d = svsort ( n, d );
//
//  Determine the smallest eigenvalue.
//
    dmin = r8_max ( d[n-1], small );
//
//  The ratio of the smallest to largest eigenvalue determines whether
//  the system is ill conditioned.
//
    if ( dmin < m2 * d[0] )
    {
      this.illc = true;
    }
    else
    {
      this.illc = false;
    }
    if ( 1 < prin )
    {
      if ( 1.0 < scbd )
      {
        r8vec_print ( n, z, "  The scale factors:" );
      }
      r8vec_print ( n, d, "  Principal values of the quadratic form:" );
    }
    if ( 3 < prin )
    {
      r8mat_print ( n, n, v, "  The principal axes:" );
    }
//
//  The main loop ends here.
//
  }
  if ( 0 < prin )
  {
    r8vec_print ( n, x, "  X:" );
  }

  return fx;
}
//****************************************************************************80

void print2 ( int n, double x[], int prin, double fx, int nf, int nl )

//****************************************************************************80
//
//  Purpose:
//
//    PRINT2 prints certain data about the progress of the iteration.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2016
//
//  Author:
//
//    Original FORTRAN77 version by Richard Brent.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Richard Brent,
//    Algorithms for Minimization with Derivatives,
//    Prentice Hall, 1973,
//    Reprinted by Dover, 2002.
//
//  Parameters:
//
//    Input, int N, the number of variables.
//
//    Input, double X[N], the current estimate of the minimizer.
//
//    Input, int PRIN, the user-specifed print level.
//    0, nothing is printed.
//    1, F is printed after every n+1 or n+2 linear minimizations.
//       final X is printed, but intermediate X is printed only
//       if N is at most 4.
//    2, the scale factors and the principal values of the approximating
//       quadratic form are also printed.
//    3, X is also printed after every few linear minimizations.
//    4, the principal vectors of the approximating quadratic form are
//       also printed.
//
//    Input, double FX, the smallest value of F(X) found so far.
//
//    Input, int NF, the number of function evaluations.
//
//    Input, int NL, the number of linear searches.
//
{
	System.out.println();
	System.out.println( "  Linear searches = " + nl );
	System.out.println( "  Function evaluations " + nf );
	System.out.println( "  Function value FX = " + fx );

  if ( n <= 4 || 2 < prin )
  {
    r8vec_print ( n, x, "  X:" );
  }

  return;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    QUAD seeks to minimize the scalar function F along a particular curve.
//
//  Discussion:
//
//    The minimizer to be sought is required to lie on a curve defined
//    by Q0, Q1 and X.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2016
//
//  Author:
//
//    Original FORTRAN77 version by Richard Brent.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Richard Brent,
//    Algorithms for Minimization with Derivatives,
//    Prentice Hall, 1973,
//    Reprinted by Dover, 2002.
//
//  Parameters:
//
//    Input, int N, the number of variables.
//
//    Input, double F ( double X[], int N ), the name of the function to
//    be minimized.
//
//    Input/output, double X[N], ?
//
//    Input, double T, ?
//
//    Input, double H, ?
//
//    Input, double V[N,N], the matrix of search directions.
//
//    Input/output, double Q0[N], Q1[N], two auxiliary points used to define
//    a curve through X.
//
//    Input/output, int &NL, the number of linear searches.
//
//    Input/output, int &NF, the number of function evaluations.
//
//    Input, double DMIN, an estimate for the smallest eigenvalue.
//
//    Input, double LDT, the length of the step.
//
//    Input/output, double &FX, the value of F(X,N).
//
//    Input/output, double &QF1, &QA, &QB, &QC, &QD0, &QD1 ?
//
/*void quad ( int n, double f ( double x[], int n ), double x[], double t,
  double h, double v[], double q0[], double q1[], int &nl, int &nf, double dmin,
  double ldt, double &fx, double &qf1, double &qa, double &qb, double &qc,
  double &qd0, double &qd1 ) */
private double[] quad ( int n, double x[], double t, double h, double v[],
		double q0[], double q1[], int nl, double dmin, double ldt, double fx,
		double qf1, double qa, double qb, double qc, double qd0, double qd1,
		double t0, RTContext ctxt,	OptimResults res, OptimCallbacks cbs )
throws Xcept {
  boolean fk;
  int i;
  int jsearch;
  double l;
  int nits;
  double s;
  double temp;
  double value;

	double[] returnArray = new double[8]; // Holds return values...
	Arrays.fill(returnArray,0);
  temp = fx;
  fx   = qf1;
  qf1  = temp;
  for ( i = 0; i < n; i++ )
  {
    temp  = x[i];
    x[i]  = q1[i];
    q1[i] = temp;
  }

  qd1 = 0.0;
  for ( i = 0; i < n; i++ )
  {
    qd1 = qd1 + ( x[i] - q1[i] ) * ( x[i] - q1[i] );
  }
  qd1 = Math.sqrt( qd1 );

  if ( qd0 <= 0.0 || qd1 <= 0.0 || nl < 3 * n * n )
  {
    fx = qf1;
    qa = 0.0;
    qb = 0.0;
    qc = 1.0;
    s = 0.0;
  }
  else
  {
    jsearch = - 1;
    nits = 2;
    s = 0.0;
    l = qd1;
    value = qf1;
    fk = true;

		double[] minnyOut = new double[10];
		Arrays.fill(minnyOut,0);
		//minny ( n, jsearch, nits, s, l, value, fk, f, x, t,
    //  h, v, q0, q1, nl, nf, dmin, ldt, fx, qa, qb, qc, qd0, qd1 );
		minnyOut = minny ( n, jsearch, nits, s, l, value, fk,/* f,*/ x, t, h, v,
		q0, q1, nl, dmin, ldt, fx, qa, qb, qc, qd0, qd1, t0, ctxt, res, cbs );
		if (minnyOut[0] == -1.0){
			String minfitExitMsg = "# of calls";
			term(res,minfitExitMsg);
			returnArray[0] = -1.0;
			return returnArray;
		}
		if (minnyOut[0] == -2.0){
			String minfitExitMsg = "Min RMS error";
			term(res,minfitExitMsg);
			returnArray[0] = -2.0;
			return returnArray;
		}
		s = minnyOut[0];
		l = minnyOut[1];
		value = minnyOut[2];
		nl = (int)minnyOut[3];
		fx = minnyOut[4];
		qa = minnyOut[5];
		qb = minnyOut[6];
		qc = minnyOut[7];
		qd0 = minnyOut[8];
		qd1 = minnyOut[9];

    qa =                 l * ( l - qd1 )       / ( qd0 + qd1 ) / qd0;
    qb = - ( l + qd0 )     * ( l - qd1 ) / qd1                 / qd0;
    qc =   ( l + qd0 ) * l               / qd1 / ( qd0 + qd1 );
  }

  qd0 = qd1;
  for ( i = 0; i < n; i++ )
  {
    s = q0[i];
    q0[i] = x[i];
    x[i] = qa * s + qb * x[i] + qc * q1[i];
  }
	returnArray[0] = (double)nl;
	returnArray[1] = fx;
	returnArray[2] = qf1;
	returnArray[3] = qa;
	returnArray[4] = qb;
	returnArray[5] = qc;
	returnArray[6] = qd0;
	returnArray[7] = qd1;
  return returnArray;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8_EPSILON returns the R8 roundoff unit.
//
//  Discussion:
//
//    The roundoff unit is a number R which is a power of 2 with the
//    property that, to the precision of the computer's arithmetic,
//      1 < 1 + R
//    but
//      1 = ( 1 + R / 2 )
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 September 2012
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Output, double R8_EPSILON, the R8 round-off unit.
//
public double r8_epsilon ( )
{
  double value = 2.220446049250313E-016;

  return value;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8_HYPOT returns the value of sqrt ( X^2 + Y^2 ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 March 2012
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, Y, the arguments.
//
//    Output, double R8_HYPOT, the value of sqrt ( X^2 + Y^2 ).
//
public double r8_hypot ( double x, double y )
{
  double a;
  double b;
  double value;

  if ( Math.abs( x ) < Math.abs( y ) )
  {
    a = Math.abs( y );
    b = Math.abs( x );
  }
  else
  {
    a = Math.abs( x );
    b = Math.abs( y );
  }
//
//  A contains the larger value.
//
  if ( a == 0.0 )
  {
    value = 0.0;
  }
  else
  {
    value = a * Math.sqrt( 1.0 + ( b / a ) * ( b / a ) );
  }

  return value;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8_MAX returns the maximum of two R8's.
//
//  Discussion:
//
//    The C++ math library provides the function fmax() which is preferred.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    18 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, Y, the quantities to compare.
//
//    Output, double R8_MAX, the maximum of X and Y.
//
public double r8_max ( double x, double y )
{
  double value;
  if ( y < x )
  {
    value = x;
  }
  else
  {
    value = y;
  }
  return value;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8_MIN returns the minimum of two R8's.
//
//  Discussion:
//
//    The C++ math library provides the function fmin() which is preferred.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    31 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, double X, Y, the quantities to compare.
//
//    Output, double R8_MIN, the minimum of X and Y.
//
public double r8_min ( double x, double y )
{
  double value;
  if ( y < x )
  {
    value = y;
  }
  else
  {
    value = x;
  }
  return value;
}
//****************************************************************************80

//public double r8_uniform_01 ( int seed )

//****************************************************************************80
//
//  Purpose:
//
//    R8_UNIFORM_01 returns a unit pseudorandom R8.
//
//  Discussion:
//
//    This routine implements the recursion
//
//      seed = ( 16807 * seed ) mod ( 2^31 - 1 )
//      u = seed / ( 2^31 - 1 )
//
//    The integer arithmetic never requires more than 32 bits,
//    including a sign bit.
//
//    If the initial seed is 12345, then the first three computations are
//
//      Input     Output      R8_UNIFORM_01
//      SEED      SEED
//
//         12345   207482415  0.096616
//     207482415  1790989824  0.833995
//    1790989824  2035175616  0.947702
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    09 April 2012
//
//  Author:
//
//    John Burkardt
//
//  Reference:
//
//    Paul Bratley, Bennett Fox, Linus Schrage,
//    A Guide to Simulation,
//    Second Edition,
//    Springer, 1987,
//    ISBN: 0387964673,
//    LC: QA76.9.C65.B73.
//
//    Bennett Fox,
//    Algorithm 647:
//    Implementation and Relative Efficiency of Quasirandom
//    Sequence Generators,
//    ACM Transactions on Mathematical Software,
//    Volume 12, Number 4, December 1986, pages 362-376.
//
//    Pierre L'Ecuyer,
//    Random Number Generation,
//    in Handbook of Simulation,
//    edited by Jerry Banks,
//    Wiley, 1998,
//    ISBN: 0471134031,
//    LC: T57.62.H37.
//
//    Peter Lewis, Allen Goodman, James Miller,
//    A Pseudo-Random Number Generator for the System/360,
//    IBM Systems Journal,
//    Volume 8, Number 2, 1969, pages 136-143.
//
//  Parameters:
//
//    Input/output, int &SEED, the "seed" value.  Normally, this
//    value should not be 0.  On output, SEED has been updated.
//
//    Output, double R8_UNIFORM_01, a new pseudorandom variate,
//    strictly between 0 and 1.
//
public double r8_uniform_01 ( int seed, OptimResults res )
{
  int i4_huge = 2147483647;
  int k;
  double r;
  if ( seed == 0 )
  {
	  System.out.println( );
	  System.out.println("R8_UNIFORM_01 - Fatal error!\n");
	  System.out.println( "  Input value of SEED = 0.\n");
    //exit ( 1 ); throw Xcept ....
		String errMsg = "Input value of SEED = 0";
		termError(res,errMsg);
  }
  k = seed / 127773;
  seed = 16807 * ( seed - k * 127773 ) - k * 2836;
  if ( seed < 0 )
  {
    seed = seed + i4_huge;
  }
  r = ( double ) ( seed ) * 4.656612875E-10;

  return r;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_PRINT prints an R8MAT.
//
//  Discussion:
//
//    An R8MAT is a doubly dimensioned array of R8 values, stored as a vector
//    in column-major order.
//
//    Entry A(I,J) is stored as A[I+J*M]
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    10 September 2009
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows in A.
//
//    Input, int N, the number of columns in A.
//
//    Input, double A[M*N], the M by N matrix.
//
//    Input, String TITLE, a title.
//
public void r8mat_print ( int m, int n, double a[], String title )
{
  r8mat_print_some ( m, n, a, 1, 1, m, n, title );

  return;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_PRINT_SOME prints some of an R8MAT.
//
//  Discussion:
//
//    An R8MAT is a doubly dimensioned array of R8 values, stored as a vector
//    in column-major order.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 June 2013
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int M, the number of rows of the matrix.
//    M must be positive.
//
//    Input, int N, the number of columns of the matrix.
//    N must be positive.
//
//    Input, double A[M*N], the matrix.
//
//    Input, int ILO, JLO, IHI, JHI, designate the first row and
//    column, and the last row and column to be printed.
//
//    Input, String TITLE, a title.
//
public void r8mat_print_some ( int m, int n, double a[], int ilo, int jlo, int ihi,
  int jhi, String title )
{
	int INCX = 5;
  int i;
  int i2hi;
  int i2lo;
  int j;
  int j2hi;
  int j2lo;

  System.out.print( "\n");
  System.out.print( title+" \n");

  if ( m <= 0 || n <= 0 )
  {
	  System.out.print( "\n");
	  System.out.print( "  (None)\n");
    return;
  }
//
//  Print the columns of the matrix, in strips of 5.
//
  for ( j2lo = jlo; j2lo <= jhi; j2lo = j2lo + INCX )
  {
    j2hi = j2lo + INCX - 1;
    if ( n < j2hi )
    {
      j2hi = n;
    }
    if ( jhi < j2hi )
    {
      j2hi = jhi;
    }
    System.out.print( "\n");
//
//  For each column J in the current range...
//
//  Write the header.
//
    System.out.print( "  Col:    ");
    for ( j = j2lo; j <= j2hi; j++ )
    {
		System.out.print(  j - 1 + "       ");
    }
		  System.out.print( "\n");
					  System.out.print( "  Row\n");
					  System.out.print( "\n");
//
//  Determine the range of the rows in this strip.
//
    if ( 1 < ilo )
    {
      i2lo = ilo;
    }
    else
    {
      i2lo = 1;
    }
    if ( ihi < m )
    {
      i2hi = ihi;
    }
    else
    {
      i2hi = m;
    }

    for ( i = i2lo; i <= i2hi; i++ )
    {
//
//  Print out (up to) 5 entries in row I, that lie in the current strip.
//
		System.out.print( i - 1 + ": ");
    for ( j = j2lo; j <= j2hi; j++ )
      {
		  System.out.print(  a[i-1+(j-1)*m] + "  ");
      }
      System.out.print( "\n");
    }
  }

  return;
					  //# undef INCX
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8MAT_TRANSPOSE_IN_PLACE transposes a square R8MAT in place.
//
//  Discussion:
//
//    An R8MAT is a doubly dimensioned array of R8 values, stored as a vector
//    in column-major order.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    26 June 2008
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of rows and columns of the matrix A.
//
//    Input/output, double A[N*N], the matrix to be transposed.
//
//public void r8mat_transpose_in_place ( int n, double a[] )
public double[] r8mat_transpose_in_place ( int n, double a[] )
{
  int i;
  int j;
  double t;
  for ( j = 0; j < n; j++ )
  {
    for ( i = 0; i < j; i++ )
    {
      t        = a[i+j*n];
      a[i+j*n] = a[j+i*n];
      a[j+i*n] = t;
    }
  }
  return a;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_COPY copies an R8VEC.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    03 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the vectors.
//
//    Input, double A1[N], the vector to be copied.
//
//    Output, double A2[N], the copy of A1.
//
	public double[] r8vec_copy ( int n, double a1[] )
{

  return Arrays.copyOf(a1,a1.length);
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_MAX returns the value of the maximum element in an R8VEC.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    22 August 2010
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the array.
//
//    Input, double R8VEC[N], a pointer to the first entry of the array.
//
//    Output, double R8VEC_MAX, the value of the maximum element.  This
//    is set to 0.0 if N <= 0.
//
public double r8vec_max ( int n, double r8vec[] )
{
  int i;
  double value;
  value = r8vec[0];
  for ( i = 1; i < n; i++ )
  {
    if ( value < r8vec[i] )
    {
      value = r8vec[i];
    }
  }
  return value;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_MIN returns the value of the minimum element in an R8VEC.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    02 July 2005
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in the array.
//
//    Input, double R8VEC[N], the array to be checked.
//
//    Output, double R8VEC_MIN, the value of the minimum element.
//
public double r8vec_min ( int n, double r8vec[] )
{
  int i;
  double value;
  value = r8vec[0];
  for ( i = 1; i < n; i++ )
  {
    if ( r8vec[i] < value )
    {
      value = r8vec[i];
    }
  }
  return value;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_NORM returns the L2 norm of an R8VEC.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//    The vector L2 norm is defined as:
//
//      R8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    01 March 2003
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of entries in A.
//
//    Input, double A[N], the vector whose L2 norm is desired.
//
//    Output, double R8VEC_NORM, the L2 norm of A.
//
public double r8vec_norm ( int n, double a[] )
{
  int i;
  double v;
  v = 0.0;
  for ( i = 0; i < n; i++ )
  {
    v = v + a[i] * a[i];
  }
  v = Math.sqrt( v );

  return v;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    R8VEC_PRINT prints an R8VEC.
//
//  Discussion:
//
//    An R8VEC is a vector of R8's.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    16 August 2004
//
//  Author:
//
//    John Burkardt
//
//  Parameters:
//
//    Input, int N, the number of components of the vector.
//
//    Input, double A[N], the vector to be printed.
//
//    Input, String TITLE, a title.
//
public void r8vec_print ( int n, double a[], String title )
{
  int i;
  System.out.print( "\n");
  System.out.println( title );
  System.out.println( );
  for ( i = 0; i < n; i++ )
  {
    System.out.print( "  " + i
					  + ": " + a[i] + "\n");
  }

  return;
}
//****************************************************************************80



//****************************************************************************80
//
//  Purpose:
//
//    SVSORT descending sorts D and adjusts the corresponding columns of V.
//
//  Discussion:
//
//    A simple bubble sort is used on D.
//
//    In our application, D contains singular values, and the columns of V are
//    the corresponding right singular vectors.
//
//  Licensing:
//
//    This code is distributed under the GNU LGPL license.
//
//  Modified:
//
//    04 August 2016
//
//  Author:
//
//    Original FORTRAN77 version by Richard Brent.
//    C++ version by John Burkardt.
//
//  Reference:
//
//    Richard Brent,
//    Algorithms for Minimization with Derivatives,
//    Prentice Hall, 1973,
//    Reprinted by Dover, 2002.
//
//  Parameters:
//
//    Input, int N, the length of D, and the order of V.
//
//    Input/output, double D[N], the vector to be sorted.
//    On output, the entries of D are in descending order.
//
//    Input/output, double V[N,N], an N by N array to be adjusted
//    as D is sorted.  In particular, if the value that was in D(I) on input is
//    moved to D(J) on output, then the input column V(*,I) is moved to
//    the output column V(*,J).
//
//public void svsort ( int n, double d[], double v[] )
public double[] svsort ( int n, double d[] ) // return d[], v[] is a member variable.
{
  int i;
  int j1;
  int j2;
  int j3;
  double t;

  for ( j1 = 0; j1 < n - 1; j1++ )
  {
//
//  Find J3, the index of the largest entry in D[J1:N-1].
//  MAXLOC apparently requires its output to be an array.
//
    j3 = j1;
    for ( j2 = j1 + 1; j2 < n; j2++ )
    {
      if ( d[j3] < d[j2] )
      {
        j3 = j2;
      }
    }
//
//  If J1 != J3, swap D[J1] and D[J3], and columns J1 and J3 of V.
//
    if ( j1 != j3 )
    {
      t     = d[j1];
      d[j1] = d[j3];
      d[j3] = t;
      for ( i = 0; i < n; i++ )
      {
        t         = v[i+j1*n];
        this.v[i+j1*n] = v[i+j3*n];
        this.v[i+j3*n] = t;
      }
    }
  }

  return d;
}
//****************************************************************************80



// update OptimResults term info OK
	private void term(OptimResults res, String crit) {
		if(this.test == false){
			res.status = OptimResults.NORMAL;
			res.termMsg = "Met " + crit + " stopping criterion";
		}
	//	else System.out.println("Met " + crit + " stopping criterion");
	}

	// update OptimResults termEror info
	private void termError(OptimResults res, String crit) {
		if(this.test == false){
			res.status = OptimResults.ERROR;
			res.termMsg = crit;
		}
		//else System.out.println("Terminate with Error: " + crit );
	}

	// query
	public String diagInfo() { return "PRAXIS Optimizer"; }
	public boolean allowMP() { return false; } // ?

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
                    termError(res,"Unspecified error in PRAXIS opt.");
                    break;
                }
            return;
        }


	public double testFunction(double x[], int n) {
// Rosenbrock function:
		int j;
	  double value;
	  value = 0.0;
		if (x.length > 1) System.out.println("TestFunction Param 1: "+x[0]+", Param 2:"+x[1]);

	  for ( j = 0; j < n; j++ )
	  {
	    if ( ( j % 2 ) == 0 )
	    {
	      value = value + Math.pow ( 1.0 - x[j], 2 );
	    }
	    else
	    {
	      value = value + 100.0 * Math.pow ( x[j] - x[j-1] * x[j-1], 2 );
	    }
	  }
		if(this.test == true) System.out.println("TestFunction current value: "+value);
	  return value;
	}


// ***************************************************************
	public static void main(String [] args) throws Xcept{

		//TESTING code here
		double h0;
	  int i;
	  int n = 2; // number of parameters
	  double pr;	// minnimum error value
	  int prin;
		int maxItr = 10000000;
	  double t0;  // error tolerance
	  double[] x = { -1.2, 1.0 };
		double rmsErr = 1e-20;
	  System.out.println("ROSENBROCK function TEST");

	  t0 = 2e-24;
	  h0 = 1.0;
	  prin = 3;
		Praxis a_Optimize = new Praxis();
		pr = a_Optimize.testRun( n, x, maxItr, rmsErr, h0, t0, prin);
		System.out.println("testRun return value: "+ pr);
	}

}
