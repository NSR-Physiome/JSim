/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// sensitivity/covariance matrix & confidence limit calculations

package JSim.data;
import JSim.util.*;
import java.lang.Object;
import java.lang.Math;
import Jama.*;

public class SensMatrix {
	private int nx;            // # parameters
	private int nr;            // # ref data curves
	private double[] xs;       // parm values
        private double[] xdeltas;  // parm sensitivity deltas
	private Data.List ys;      // ref data
	private Data.List hs;      // unperturbed model data
	private Data.List[] hks;   // perturbed model data 
	private Data.List pws;     // point weights (optional)
	private double[] cws;      // curve weights (optional)

	// calculated data
	private int nf;		    // degrees of freedom
        private double sswr;        // sum squares weighted residuals
        private int nd;             // sum number of data points in all curves
	private double[][] sensMat; // nd*nx sensitivity matrix
        private double condno;      // Condition number for
                                    // transpose(sensMat)*W*sensMat where
                                    // W is a matrix nd*nd containing the 
                                    // point weights on the diagonal
	private double[][] covMat;  // nx*nx covariance matrix
        private double[]   confLim; // nx confidence limits for parameters
        private double[]   wtnorm;  // normalization for point weights
        private double[][] wgtSensMat; // nd*nx sensitivity matrix
                                    // includes sqrt(weights) and may
                                    // contain NaN's
        private double[]   maxSen;  // nx maximum values of wgtSensMat(j,nx)
        private double[]   maxSenH; // and corresponding value of
                                    // h(j),j=0..nd-1
        private double[]   wgtDev;  // nd*1 weighted by sqrt(weights) 
                                    // deviations
        
	// constructor
	public SensMatrix(double[] x, double[] xdelta,
	Data.List refs, Data.List unperturb, Data.List[] perturb, 
	Data.List pwgts, double[] cwgts) throws Xcept {

	    // load private state
	    xs = x;
	    xdeltas = xdelta;
	    ys = refs;
	    hs = unperturb;
	    hks = perturb;
	    nx = x.length;
	    nr = ys.size();
	    pws = pwgts;
	    cws = cwgts;

	    // sanity check
	    if (hs.size() != nr) throw new Xcept(
	    	"Wrong # of model data curves");

	    if (hks.length != nx) throw new Xcept(
	    	"Wrong # of sensitivity data curve lists");

	    for (int k=0; k<nx; k++)
		if (hks[k].size() != nr) throw new Xcept(
	    	    "Wrong # of sensitivity data curves");		

	    if (pws != null && pws.size() != nr) throw new Xcept(
	    	"Wrong # of point weight curves");

	    if (cws != null && cws.length != nr) throw new Xcept(
	    	"Wrong # of curve weights");

            for (int r=0; r<nr; r++) {
                int ny = ys.data(r).nsamples();
                if (ny != hs.data(r).nsamples()) throw new Xcept(
                    "Unequal length of data and model curves");
                for (int ix=0; ix<nx; ix++) {
                    if (ny != hks[ix].data(r).nsamples())
                        throw new Xcept(
                        "Unequal length of data and perturbed model curves");
                }
                if (pws != null) {
		    int np = pws.data(r).nsamples();
                    if (np != 1 && np != ny) throw new Xcept(
                    	"Unequal length of data and defined weights");
                }
            }


            // Always calculate nd and sensMat
            nd = calcNumDataPts();
            sensMat = calcSensMat();
            wtnorm  = calcCurveNorm();
        }

        // count number of data points
        private int calcNumDataPts() throws Xcept {
            int ndp = 0;
            for (int r=0; r<nr; r++) {
                Data h=hs.data(r);
                int  nk = h.nsamples();
                for (int k=0; k<nk; k++) {
                   if (!Double.isNaN(h.realVal(k))) {
                       ndp +=1;
                   }
                }
            }
            return ndp;
        }
        private double[] calcCurveNorm() throws Xcept {
            double[] wtnorm = new double[nr];
            for (int ir=0; ir<nr; ir++) {
                Data h = hs.data(ir);
                Data p = (pws !=null) ? pws.data(ir) : null;
                int n = h.nsamples();
                double pwttot=0;
                for (int j=0; j<n; j++) {
                   if(!Double.isNaN(h.realVal(j))) {
                       double pwgt=1;
                       if(p != null) 
                           pwgt = (p.ndim() == 0) ?
                                p.realVal(0) : p.realVal(j);
                       pwttot += pwgt;
                   }
                }
                wtnorm[ir]= (pwttot<=0) ? 1 : pwttot;
            }
            return wtnorm;
        }
            
        // Calculate weights
        private double wgt(int r, int k) throws Xcept {
            double wgt = (cws == null) ? 1 :cws[r]; 
            
            if (pws == null) return wgt;
	    if (pws.data(r).nsamples() == 1) k=0;
            wgt *= pws.data(r).realVal(k);
            if(wgt<0) throw new Xcept(
               "Point weight multiplied by curve weight must be non-negative");
            return wgt/wtnorm[r];
        }

        // Calculate degrees of freedom
        private int calcDegFreedom() throws Xcept{
            int degFree = -nx; 
            int kw = 0;
            double tot=0.0;
            for (int r=0; r<nr; r++) {
                Data h = hs.data(r);
                int nk = h.nsamples();
                
                for (int k=0; k<nk; k++) 
                    if(wgt(r,k)>0 && !Double.isNaN(h.realVal(k))) degFree++;
            }
            if (degFree<1) throw new Xcept(
                 "Number of non-zero weights minus number of parameters less than 1");
            return degFree;
        }

	// calculate Sensitivity Matrix
        private double [][] calcSensMat() throws Xcept {
            double[][] s = new double[nd][nx];
            for (int ix=0; ix<nx; ix++) {
                int idataPts = 0;
                Data.List hksj = hks[ix];
                for (int r=0; r<nr; r++) {
                    Data h   = hs.data(r);
                    Data hj  = hksj.data(r);
                    for (int i=0; i<h.nsamples(); i++) {
                        if(!Double.isNaN(h.realVal(i))) {
                                s[idataPts][ix] = 
                                (hj.realVal(i)-h.realVal(i))/xdeltas[ix];
                                idataPts++;
                        }
                    }
                }
            }
            return s;
        }

	// calculate Weighted Sensitivity Matrix (for Sensop)
        private double [][] calcWgtSensMat() throws Xcept {
            double[][] s = new double[nd][nx];
            for (int ix=0; ix<nx; ix++) {
                int idataPts = 0;
                Data.List hksj = hks[ix];
                for (int r=0; r<nr; r++) {
                    Data h   = hs.data(r);
                    Data hj  = hksj.data(r);
                    for (int i=0; i<h.nsamples(); i++) {
                        if(!Double.isNaN(h.realVal(i))) {
                                s[idataPts][ix] = Math.sqrt(wgt(r,i))*
                                (hj.realVal(i)-h.realVal(i))/xdeltas[ix];
                                idataPts++;
                        }
                    }
                }
            }
            return s;
        }
        
        // calculated Weighted Deviation Matrix (for Sensop)
        private double [] calcWgtDev() throws Xcept {
            double[] s = new double[nd];
            int idataPts = 0;
            for (int r=0; r<nr; r++) {
                Data h   = hs.data(r);
                Data y   = ys.data(r);
                for (int i=0; i<h.nsamples(); i++) {
                    if(!Double.isNaN(h.realVal(i))) {
                        s[idataPts] = Math.sqrt(wgt(r,i))*
                        (h.realVal(i)-y.realVal(i));
                        idataPts++;
                    }
                }
            }
            return s;
        }



        // calculate maximum Sensitivity for each parameter,
        // corresponding value of h (for sensop optimizer)
        private void calcMaxWgtSenH() throws Xcept {
           maxSen = new double[nx];
           maxSenH = new double[nx];
           if(wgtSensMat==null) wgtSensMat=getWgtSensMatrix();
           for (int ix=0; ix<nx; ix++) {
               double maxtemp=0;
               int idataPts = 0;
               for (int r=0; r<nr; r++) {
                   Data h   = hs.data(r);
                   for (int i=0; i<h.nsamples(); i++) {
                       if(!Double.isNaN(h.realVal(i))) {
                           if(Math.abs(wgtSensMat[idataPts][ix])>=maxtemp) {
                               maxtemp=Math.abs(wgtSensMat[idataPts][ix]);
                               maxSen[ix]  =wgtSensMat[idataPts][ix];
                               maxSenH[ix] =h.realVal(i);
                               idataPts++;
                           }
                       }
                   }
               }
           }
        }


 
        // Calculate sum of the squares of the weighted residuals
        private double calcSswr() throws Xcept {
            double sswres = 0.0;
            for (int r=0; r<nr; r++) {
                Data y  = ys.data(r);
                int  nk = y.nsamples();
                Data h  = hs.data(r);
                for (int k=0; k<nk; k++) {
                    if(!Double.isNaN(h.realVal(k))) {
                        double d = (y.realVal(k)-h.realVal(k));
                        sswres += d*d*wgt(r,k);
                    }
                }
            }
            return sswres;
        }

        // calculate Covariance Matrix
        private double[][] calcCovMat() throws Xcept {
            Matrix inCv = new Matrix(nx,nx,0.0);
            if(sswr==0) sswr=calcSswr();
            if(nf==0) nf=calcDegFreedom();
            for (int i=0; i<nx; i++) {
                if(Double.isNaN(xdeltas[i])) throw new Xcept(
                   "NaN for at least one parameter increment"); 
                for (int j=i; j<nx; j++) {
                    int ks = 0;
                    double tot=0.;
                    for (int r=0; r<nr; r++) {
                        Data h = hs.data(r);
                        int nk= h.nsamples();
                        for (int k=0; k<nk; k++) {
                            if(!Double.isNaN(h.realVal(k))) {
                                tot += wgt(r,k)*sensMat[ks][i] * sensMat[ks][j];
                                ks++;
                            }
                        }
                    }
                    inCv.set(i,j,tot);
                    inCv.set(j,i,tot);
                }
            }
             
            // Calculate Covariance Matrix
            condno = inCv.cond();
//          System.out.println("Condition Number " +cond);
            inCv=(inCv.inverse()).times(sswr/nf);
            double[][] Cov= new double[nx][nx];
            for (int i=0; i<nx; i++) {
                for (int j=0; j<nx; j++) {
                    Cov[i][j]=inCv.get(i,j);
                }
            }
            return Cov;
        }

        // calculate Confidence Limits
        private double[] calcConfLims(double pct) throws Xcept{
            double[] cl = new double[nx];
            if(nf==0) nf=calcDegFreedom();
            if(covMat==null) covMat=calcCovMat();
            StudTDist answer = new StudTDist(nf);
            double t = answer.tValue(pct);
            for (int i=0; i<nx; i++) 
                cl[i] = t*Math.sqrt(covMat[i][i]);
            return cl;
        }

        // query covariance matrix
        public double[][] getCovMatrix() throws Xcept {
            if (covMat == null) covMat=calcCovMat();
            return covMat;
        }
  
	// create normalized covariance matrix
	public double[][] getNormCovMatrix() throws Xcept {
	    return normalize(getCovMatrix());
	}
                
        // query sensitivity matrix
        public double[][] getSensMatrix() throws Xcept {
            if (sensMat == null) sensMat=calcSensMat();
            return sensMat;
        }
        // query Weighted sensitivy matrix
        public double [][] getWgtSensMatrix() throws Xcept {
            if (wgtSensMat==null) wgtSensMat=calcWgtSensMat();
            return wgtSensMat;
        }
        // query Weighted deviation matrix
        public double[] getWgtDev() throws Xcept {
            if (wgtDev==null) wgtDev=calcWgtDev();
            return wgtDev;
        }
        // query maximum sensitivity, 
        public double [] getMaxSen() throws Xcept {
           if (maxSen == null) calcMaxWgtSenH();
           return maxSen;
        }
        // query h corresponding to maximum sensitivity
        public double [] getMaxSenH() throws Xcept {
           if(maxSenH == null) calcMaxWgtSenH();
           return maxSenH;
        }
      

        // query confidence Limits
        public double [] getConfLimits(double pct) throws Xcept {
            return    calcConfLims(pct);
        }

        // query number of data points
        private int getNDP() throws Xcept {
            if (nd == 0) nd = calcNumDataPts();
            return nd;
        }

        // query sum squares weighted residuals
        private double getSswr() throws Xcept {
            if (sswr == 0) sswr = calcSswr();
            return sswr;
        }	

	// normalize a square matrix
	public static double[][] normalize(double[][] mat) {
	    int nx = mat.length;
	    double[][] nmat = new double[nx][nx];
	    for (int i=0; i<nx; i++) 
	    	for (int j=0; j<nx; j++) 
		    nmat[i][j] = mat[i][j] /
		    	Math.sqrt(mat[i][i] * mat[j][j]);
	    return nmat;
	}
        // get condition number
        public double getCondNo() throws Xcept {
            if (covMat == null) covMat=calcCovMat();
            return condno;
        }
       
}
