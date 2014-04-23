/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//  distribution subroutines: lagdis, gaudis, expdis, expf

package JSim.fgen; 

import JSim.util.*;
import JSim.expr.*; 

public class FgenDistrib {

	//--------------LAGDIS------------------------------------------
	public static void  lagdis( FgenInterp storage, 
	                        double area, double tmean, double rd, 
	                        double skewn, boolean upslope, double frpeak) {
	
	    //Calculations normally done in cinput
	    /*ndimen is taked to be the length of h, although nh is calculated
	      it is not returned.
	    */
	    int ndimen = storage.length;
            double tstart=storage.t[0];
            double tend=storage.t[storage.length-1];
            double dx;
            if(storage.length>1) {dx=storage.t[1]-storage.t[0];}
            else {dx=0.0;}
	    for (int i=0; i<ndimen; i++) { storage.ft[i]=0.0; }

	    double tshift = 0-tstart;
	    double tstarts= tstart+tshift;
	    double tmeans = tmean+tshift;
	    double tends  = tend+tshift;

	    double tau=0;
	    if(skewn >=0 )  { 
	        tau = Math.pow(  (skewn*0.5),(1.0/3.0) )*rd*tmeans ; 
	    }
	    else { 
	        tau =-1.*Math.pow( (-1.*skewn*0.5),(1.0/3.0) )*rd*tmeans ; 
	    }
	
	    double tcentr = tmeans - tau ;
	    double xc = tcentr ;
	    double sigma = (rd*tmeans)*(rd*tmeans) - tau*tau ;
	    if (sigma >=0 ) {
	        sigma = Math.pow(sigma,0.5) ;
	    }
	    else {
	        sigma = sigma; 
	    }
	    boolean ix=upslope;
	    // Calculations mirror those in lagndc.f
	    //
	    // parameters for xpoly and xpoly function in separate routine
	    //
	    double scaler= 0.2316419; 
	    double cutoff = Math.min(1.0,frpeak);
	    cutoff = Math.max(cutoff,1.0E-25);
	    double rlpeak = Math.log(cutoff);
	
	    int nh = -1;
	    if ((dx <=   0.)  ||  (xc  <=  tstart)  ||  (sigma  <=  0.)  || 
	        (tau  <   0.)  ||  (ndimen <= 1) ) return;
	    /*
	    B. If sqrt(sigma**2 + tau**2) < dx/4, a spike at (xc+tau) is
	    returned.
	    */ 
	    if ((sigma*sigma + tau*tau)  <   (0.0625*dx*dx)) {
	        nh = (int) ( (xc + tau)/ dx+0.5) ;
	        if (nh  >   (ndimen - 2)) {
	            nh = -1;
	            return;
	        }
	    //
	        if (area  >   0.) {   
	            storage.ft[nh] = area / dx; 
	        } 
	        else {
	            storage.ft[nh] = 1.0 / dx ; 
	        }
	        nh += 1;
	        storage.ft[nh]=0;
	        return;
	    }

	    if  (tau   <  0.025*sigma) {
	        gaudis(storage, area, tmean, rd, frpeak);
	        return; 
	    }
	    /*
	      D.  If sigma < tau/50, use an exponential density function.
	    */
	    if (sigma  <   0.02*tau) {
	        expdis(storage, area, tmean, rd, frpeak);
	        return; 
	    }
	/*
	I.  Calculate the constants used in the computations.
	Note: the calculation is not done in the most straightforward
	fashion that might be expected.  In the actual formula, there
	are a number of exponential terms which individually may
	exceed the largest floating point number available.  By
	combining the exponential terms before the exponential operator
	is used, this numerical problem can be avoided.
	*/
	    double scalin = 0.5*sigma*sigma/(tau*tau) + xc/tau;
	    double dxdeca = dx/tau;
	    double sigmdx = dx/sigma;
	    double basetx = -(xc/sigma + sigma/tau);
	    double expdec = expf(-dxdeca);
	    double exptot = 1.0;
	/*
	The cutoff is calculated when the lag normal density curve can be
	calculated by a simple multiplication because it has become
	monoexponential.  The cutoff occurs when 
	Pr(t/sigma -xc/sigma -sigma/tau) -> 1.0.  Since
	Pr(x) = 1-exp(-x*x/2)*xpoly(1/(1+p*x)), the cut off has been chosen
	when -x*x/2 = -18.0, or x=+6, at which point Pr(x) = 1. 
	ncut = sigma/dx*(6.+xc/sigma+sigma/tau)
	*/ 
	    int ncut = (int) ((6.-basetx)/sigmdx+4.0 );
	/*
	+4 is added to insure that the computation occurs after the maximum
	value of the curve.
	*/ 
	    int ndo = Math.min(ncut,ndimen);
	/*
	II.  Compute the weighted, zero error function value.
	*/
	    double holder = 1./(1. - scaler*basetx);
	    double x      = scalin - 0.5*basetx*basetx;
	    double zeroef = -expf(x)*xpoly(holder);
	/*
	III.  Main loop to compute points of the integral.
	*/
	    double areasu = 0.0;
	    double pkvalu = 0.0;
	    double pkcut  = 0.0;
	    storage.ft[0]   = 0.0;
//
// Code to handle  FORTRAN "GOTO 40" statements
	    int imax = 0;
	    boolean branch40=false;
	    int i=0;
	    while (!branch40 && i<ndo-1) { 
	        i++;
	        exptot = exptot*expdec;
	/*
	A.  Present value of decay times.
	*/
	        scalin = scalin - dxdeca;
	        basetx = basetx + sigmdx;
	        holder = 1./(1. + scaler*Math.abs(basetx));
	/*
	B.  Compute the weighted, modified Prf().
	*/
	        x = scalin - 0.5*basetx*basetx;
	        double prft = 0;
	        if (x  >   25.) {
	            nh = -2;
	            return;
	        }
	        else {
	            prft   = (expf(x)*xpoly(holder));
	        }
	/*
	C.  Take the appropriate difference between this Prf() 
	    and the base Prf().
	*/
	        if (basetx > 0.0) {
	            storage.ft[i] = (exptot*zeroef -prft   + expf(scalin))/tau;
	        }
		else {
	            storage.ft[i] = ( exptot*zeroef +prft )/tau;
	        }
	/*
	D.  Maintain the peak value while looking for the cutoff.
	*/
	        if (pkvalu < storage.ft[i]) {
	            imax = i;
	            pkvalu = storage.ft[i];
	            pkcut = pkvalu * cutoff;
	        }
	        else if (pkcut > storage.ft[i]) {
	            nh = i;
	            branch40=true;
	        }
	    }

	/*
	IV.  Loop for exponential tail
	*/
	    if (!branch40) {
	        i=ndo-2;
	        while(!branch40 && i<ndimen-1) {
	            i++;
	            storage.ft[i]=storage.ft[i-1]*expdec;

	            if (pkvalu <  storage.ft[i]) {
	                imax = i;
	                pkvalu = storage.ft[i];
	                pkcut = pkvalu * cutoff;
	            }
	            else if (pkcut >    storage.ft[i]) {
	                nh = i;
	                i=ndimen;
	                branch40=true;
	            }

	        }
	    }
	/*
	V.  Come here when cutoff has occurred or ndimen has been reached.
	*/
	    if(!branch40) nh = ndimen;
	// end of GOTO 40 problem   
	// 40 CONTINUE
	/*
	VI.  Check to see if the front end of the curve is to be linearized.
	*/
	    if (ix) {
	/*
	A.  Find the points through which to draw a straight line.
	*/
	        double htop = 0.80 * pkvalu;
	        double hbot = 0.40 * pkvalu;

	        boolean branch51=false;
	        imax =Math.max(1,imax);
	        i=imax;
	        while( !branch51 && i>0 ) {
 	            i--;
	            if (storage.ft[i] <= htop) branch51=true;
	        } 
	        htop = storage.ft[i];
	        int itop = i;

	        boolean branch53=false;
	        i=itop;
	        while(!branch53 && i>0 ) {
	            i--;
	            if (storage.ft[i] <= hbot) branch53= true;
	        }
	        hbot = storage.ft[i];
	        int ibot = i;
	/*
	B.  Calculate the change in h(i) per dx.
	*/
	        double decr = (htop - hbot) / ((double) (itop - ibot));
	/*
	C.  Extrapolate to the baseline and zero earlier points.
	*/
	        boolean branch60=false;
	        if (ibot > 0) {
	            i=ibot;
	            while(!branch60 && i>0) {
	                i--;
	                storage.ft[i] = storage.ft[i + 1] - decr;

	                if (storage.ft[i] < 0.0) {
	                    int j=i+1;
	                    while(!branch60 && j>0) {
	                        j--;
	                        storage.ft[j]=0;
	                    }
	                    branch60=true;
	                }
	            }
	        }
	    }

	/*
	VII.  Force the area to the area set in the call.
	*/
	    storage.normalize(area);
	    return;
	}

	//--------------LAGDIS------------------------------------------

	//--------------EXPF--------------------------------------------   
	public static double expf (double x) { 
	    // limit exp(x) to exp(-350.0)<=exp(x)<=exp(350.0)
	    return ( Math.exp(Math.max((Math.min(x,350.0)),-350.0)));
	}
	//--------------EXPF--------------------------------------------

	//--------------xpoly--------------------------------------------
	public static double xpoly (double s) {
	    double a1=0.1274147959;
	    double a2=-0.1422483683;
	    double a3=0.7107068705; 
	    double a4=-0.7265760129;
	    double a5=0.5307027141; 
	    return (s*(a1+s*(a2+s*(a3+s*(a4+s*a5)))) );
	} 
	//--------------xpoly--------------------------------------------

	//--------------GAUDIS-------------------------------------------
	public static void gaudis( FgenInterp storage, 
                               double area, double tmean, double rd,
                               double frpeak) {
	    int nh = storage.length;
	    double tstart=storage.t[0];
	    double tend=storage.t[nh-1];
            double dt;
            if(nh>1) {dt=storage.t[1]-storage.t[0];}
            else {dt=0.0;}
	    if (tend<=tstart ||  dt<+0) return;

	    double tmeanR =tmean-tstart;
	    double sd=tmeanR*rd;
	    if(tmeanR<0.0) return;
	    if(tmean>tend) return;
	    boolean done = false;
	    // SPIKE
	    if(tmean==tend) {
	        storage.ft[nh-1] = 1;
	        done = true;
	    }
	    if(tmeanR==0.0) {
	        storage.ft[0]=1; 
	        done= true;
	    }
	    if(sd<=0 && !done) {
	        int i = Math.min(nh-1,(int)((tmean-tstart)/dt));
	        storage.ft[i]=1;
	        done=true;
	    }
	    if( !done) {
	        double sd2=2*sd*sd;
	        double x = tstart;
	        double exparg = 0.0 ;
	        for (int i=0; i<nh; i++ ) {
	            x=tstart+i*dt;
	            exparg=-(x-tmean)*(x-tmean)/sd2;
	            if ( exparg>-24.5 ) {
	                storage.ft[i]=Math.exp(exparg);
	  	    }
	            else {
	                storage.ft[i]=0.0;
	            }
	        }
	    }
	    storage.truncTail(frpeak);
	    storage.normalize(area);
	}
	//--------------GAUDIS-------------------------------------------

        //--------------EXPDIS-------------------------------------------
        public static void expdis( FgenInterp storage, 
                               double area, double tmean, double rd,
	                       double frpeak) {
        //Calculations normally done in cinput
	    int ndimen = storage.length;
	    double tstart=storage.t[0];
	    double tend=storage.t[ndimen-1];
            double dt;
            if(ndimen>1) {dt=storage.t[1]-storage.t[0];}
            else {dt=0.0;}
	
	    if(tend<=tstart || dt<=0) return;
	    boolean done=false;
	    double tmeanR =tmean-tstart;
	    double tau =tmeanR*rd;
	    double xa = tmean-tau;
	    if(tmeanR<0.0 || xa<tstart || tau<=0 ) {return;}
	    if(tmeanR==0.0) {
		storage.ft[0]=1;
	        done=true;
	    }
	    if (tau<0.25*dt && !done) {
	        int nh = Math.min ((int) ( (xa+tau)/dt + 0.5 ), ndimen-1);
	        storage.ft[nh]=1;
	        done=true;
	    }
	    if (!done) {
	        double factor = Math.exp(-dt/tau);
	        double amp=1.0/factor;
	        double x = tstart;
	        for (int i=0; i<ndimen; i++ ) {
	            x=tstart+i*dt;
	            if ( (x>=xa) && (amp>1.0e-10) ) {
	                amp=amp*factor; 
	                storage.ft[i]=amp;
	            }
	            else {
	                 storage.ft[i]=0.0;
	            }
	        }
	    }
	    storage.truncTail(frpeak);
  	    storage.normalize(area);
	}
        //--------------EXPDIS-------------------------------------------

}

