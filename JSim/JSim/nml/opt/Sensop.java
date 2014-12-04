/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.nml.opt;
import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;
import java.lang.Math;
import Jama.*;

public class Sensop extends Optimizer {
     	
        // static class initialization 
        public static OptimAlg.Info algInfo() {
            OptimAlg.Info algInfo = new OptimAlg.Info();
            algInfo.name = "sensop";
            algInfo.boundsNeeded = true;
            algInfo.sensMatNeeded = true;
            algInfo.parsNeeded = new String[] { "stepTol","gradTol" };
            algInfo.optimClassName = Sensop.class.getName();
            return algInfo;
        }

        // constructor
        public Sensop() { }

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
            double[] x =  args.xstart; // parameter starting value
            double[] x0 = x.clone();
            double[] x1 = x.clone();
            double   stptl = args.stepTol;   // relative step tolerance
            double   grdtl = args.gradTol;   // minimum gradient tolerance
            double   ssetl = args.errTol;    // mininum error value tolerance
            int      maxfn = args.maxCalls;  // maximum # of function calls
    
            boolean bypassrest = false;  // bypassrest and doagain are used
            boolean doagain = true;      // to replace complicate GOTO structure
                                         // in original algorithm
    
            double epslon =  2.38418579E-07; // epslon=r1mach(4)
            double eps1 = 0.0004882812499;   // sqrt(epslon)
            double eps2 = 0.004882812499;    // 10*sqrt(epslon)
            double rmax = 1.70141183E+38;    // rmax=r1mach(4);
            
            double gamma = 0;
            double stpnew = rmax;
            double sumd0 = rmax;
            double sumd = 0;
            double factor = 0.025;
            double st2tl = stptl*stptl;
            double gradtl = rmax;
            double[][] s = null; // weighted by sqrt sensitivity array
            int nd = 1 ; // leading dimension of weighted sensitivity array
            double[] maxSen = null;
            double[] maxSenH = null;
            double[] wgtDev = null;

            int ifn = 0;   // counter for number of function calls
            double[] dx       = new double[nx];
            double[] delx     = new double[nx];
            double[] dxgrad   = new double[nx];
            double[] errs     = new double[nx+1];
            int[] inbounds    = new int[nx]; //0 inbounds, -1<xmin, 1>xmax
            int[] idx         = new int[nx];
            double temp=0;
            double[][] wk = new double[nx][nx];
            double[] scale = new double[nx];
            double hnm =0;
            int nxa=nx;
            boolean runagain=true;
            boolean[] recompute = new boolean[nx];

            for (int ix=0; ix<nx; ix++) {
                dx[ix]=0;
                delx[ix]=Math.abs(eps2*x[ix]);
                if(delx[ix]==0) {delx[ix]=eps2;}
            }
// MAIN  LOOP
            while(doagain) {
                // A. Find the acceptable step length, gamma, such that the
                //    sum of the squares of the weighted deviation will
                //    decrease. Update parameter x1.
                // Check x1 range and save inbounds
                for (int ix=0; ix<nx; ix++) {
                    inbounds[ix]=0;
                    x1[ix]=x0[ix]-gamma*dx[ix];
                    if(x1[ix]<=xmin[ix]) {
                        inbounds[ix]=-1;
                        x1[ix]=xmin[ix];
                    }
                    if(x1[ix]>=xmax[ix]) {
                        inbounds[ix]=1;
                        x1[ix]=xmax[ix];
                    }
                }
                // B. & C. Compute the sum of the squares of the weighted residuals.
                sumd = cbs.calcError(ctxt,x1,res);
                bypassrest=false;
                ifn++; // count model evaluations
                if (ifn>=maxfn) { sendMsg(res,4); return; }
                // E. If the sum of the squares of the weighted residuals
                //    did not decrease, cut gamma in half and retry while
                //    gamma > eps1.
                if(sumd>sumd0) {
                    gamma/=2.;
                    if(gamma>eps1) {
                        bypassrest=true;
                    }
                }
// BYPASS REST TO RETURN TO MAIN COMPUTING LOOP
// SECTION 1.
                if (!bypassrest) {
                    // G. If the Levenberg-Marquardt step can not decrease
                    //    the sum of the squares of the weighted residuals at
                    //    first trial, double the Levenberg-Marquardt
                    //    coefficient by 2, else halve it.
                    if( (gamma<=0.5) && (factor<=200) ) {factor*=2.0;}
                    if( (gamma==1)   && (factor>eps1) ) {factor*=0.5;}
                    // H. Check stopping criteria
                    if( sumd<=ssetl)   { sendMsg(res,1); return;}
                    if( gradtl<=grdtl) { sendMsg(res,2); return;}
                    if( stpnew<=st2tl) { sendMsg(res,3); return;}
// SECTION 2.
                    // J. Compute sensitivity functions
                    runagain=true;
                    double localfactor=eps1*0.1;
                    for (int ix=1; ix<nx; ix++) {recompute[ix]=true;}
                    int nloop=0;
                    while ( runagain && nloop<3) {
                        if( ifn+nx+1>=maxfn) { sendMsg(res,5); return;}
                        nloop=nloop+1;
                        // This section replaces sensdv.F
                        double smin = 0.0;
                        double[] sdx = new double[nx];
                        for (int ix=0; ix<nx; ix++) {
                            sdx[ix]=validdx(x1[ix],delx[ix],xmin[ix],xmax[ix]);
                            delx[ix]=Math.abs(sdx[ix]);
                        }    
                        SensMatrix smat = cbs.calcSensMatrix(ctxt,x1,sdx,errs,res);
                        ifn+=nx+1; 
                        s=smat.getWgtSensMatrix();
                        nd = s.length;
                        wgtDev=smat.getWgtDev();
                        maxSen=smat.getMaxSen();
                        maxSenH=smat.getMaxSenH();
                        runagain=false;
                        for (int ix=0; ix<nx; ix++) {
                            if(recompute[ix]) {
                                recompute[ix]=false;
                                smin=eps1*eps1/delx[ix];
                                if(Math.abs(maxSenH[ix])>0) {smin*=maxSenH[ix];}
                                if(Math.abs(maxSen[ix])<smin) {
                                    runagain=true;
                                    recompute[ix]=true;
                                    delx[ix]=Math.sqrt(10.0)*delx[ix];
                                }
                            }
                        }
                    } //End while
                    // Clean up
                    for ( int ix=0; ix<nx; ix++ ) {
                        double delxmx = Math.abs(x1[ix]);
                        if(delxmx==0) delxmx=1.0;
                        double delxmn = localfactor*delxmx;
                        if(maxSen[ix]!=0) {
                            delx[ix]=Math.max(delxmn,Math.abs(eps1*maxSenH[ix]/maxSen[ix]));
                            delx[ix]=Math.min(delx[ix],delxmx);
                        }
                    }
                    // This ends the code for sensdv.F
// SECTION 3.
                    if( ifn>=maxfn) { sendMsg(res,4); return; }
                    // K. Compute transpose of sensitivity matrix*sensitivity Matrix and
                    // L1-norm of Matrix
                    for (int ix=0; ix<nx; ix++) {
                        for (int jx=0; jx<nx; jx++) {
                            temp =0;
                            for (int k=0; k<s.length; k++) {
                                temp += s[k][ix]*s[k][jx]; 
                            }
                            wk[ix][jx]=temp;
                        }
                        double hnorm=0;
                        for (int jx=0; jx<nx; jx++) {
                            hnorm+=Math.abs(wk[ix][jx]); 
                        }
                        scale[ix]=1.0;
                        if(hnorm!=0) scale[ix]=1./hnorm;
                        hnm=Math.max(hnm,hnorm);
                    }                    
                    if (hnm!=0.0) hnm=2.0/hnm;
                    // L. Compute transpose (sensitivy matrix*weighted residual vector)
                    nxa = nx;
                    for (int ix=0; ix<nx; ix++) {
                        temp=0;
                        for ( int id=0; id<s.length; id++) {
                            temp+=s[id][ix]*wgtDev[id];
                        }
                        dx[ix]=temp;
                        dxgrad[ix]=temp*hnm;
                        idx[ix]=ix;
                        if( inbounds[ix]<0 && dx[ix]<0) inbounds[ix]=0;
                        if( inbounds[ix]>0 && dx[ix]>0) inbounds[ix]=0;
                        if( inbounds[ix]!=0) nxa-=1;
                    }
                    if (nxa<1) { sendMsg(res,6); return; }
// SECTION 4.
                    Matrix DX0 = new Matrix(nxa,1,0.0);
                    Matrix WK = new Matrix(nxa,nxa,0.0);
                    double theta =0;
                    for (int ix=0; ix<nx; ix++) {
                        theta+=dx[ix]*dx[ix];
                    }             
                    gradtl = Math.sqrt(theta);
                    theta=gradtl*factor;
                    // we do the next section even if nxa=nx because the matrix
                    // has to be loaded
                    int index=0;
                    for (int jx=0; jx<nx; jx++) {
                        if(inbounds[jx]==0) {
                            idx[index]=jx; 
                            DX0.set(index,0,dx[jx]);
                            index++;
                        }
                        dx[jx]=0; 
                     }
                    for (int jxa=0; jxa<nxa; jxa++) {
                        for (int ixa=0; ixa<nxa; ixa++) {
                            if(jxa==ixa) {wk[idx[ixa]][idx[jxa]]+=theta;}
                            wk[idx[ixa]][idx[jxa]]*=scale[idx[jxa]];
                            WK.set(ixa,jxa,wk[idx[ixa]][idx[jxa]]);
                        }
                    }
                    // Invert Matrix and get new dx
                    try {
                        DX0=WK.solve(DX0);
                        for (int jxa=0; jxa<nxa; jxa++) {
                            dx[idx[jxa]]=DX0.get(jxa,0)*scale[idx[jxa]];
                        }
                    }
                    // if Matrix inversion fails use gradient step
                    catch (Exception e) {
                        for (int ix=0; ix<nx; ix++) {
                            dx[ix]=dxgrad[ix];
                        }
                    }
                    // P. Calculate relative step change
                    stpnew=0.0;
                    for (int ix=0; ix<nx; ix++) {
                        temp=dx[ix]/Math.max(Math.abs(x0[ix]),epslon); 
                        temp*=temp;
                        stpnew += temp;
                    }
                    // Q. Reset variables and parameters
                    gamma = 1.0;
                    sumd0=sumd;
                    for (int ix=0; ix<nx; ix++) {
                        x0[ix] = x1[ix];
                    }
                } // End BYPASS
            } // Return to MAIN LOOP.
        } // End run optimizer

        // error messages
        public void sendMsg(OptimResults res, int istat) {
            switch(istat) {
                case 1:
                    term(res, "mean sqr error");
                    break; 
                 case 2:        
                    term(res,"gradient");
                    break;
                 case 3:
                    term(res,"parameter step size");
                    break;
                 case 4:       
                    term(res,"# calls");
                    break;
                 case 5:
                    term(res,"# calls would have exceeded");
                    break;
                 case 6:
                    term(res,"all parameters pinned at limits");
                    break;
                 default:
                    termError(res,"Unspecified error in Sensop.");
                    break;
                } 
            return;
        }

        // check for valid dx with in range xmin, xmax
        public double validdx (double x1, double del, double xmin, double xmax) {
            double deltx=del;
            double ddx;
            double DFACT=0.90;
            if( (x1-deltx)> xmin) {ddx=-deltx;} 
            else if ( (x1+deltx)<xmax) {ddx=deltx;}
            else if ( (x1<(xmin+xmax)/2. ) ) {ddx=DFACT*(xmax-x1);}
            else {ddx=DFACT*(xmin-x1);}
            return ddx;
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
    	public String diagInfo() { return "Sensop Optimizer"; }
    	public boolean allowMP() { return true; }
}
