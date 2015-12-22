/*NSRCOPYRIGHT
	Copyright (C) 1999-2015 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// data comparison calculations

package JSim.data;
import JSim.util.*;
import java.util.ArrayList;
import java.io.Serializable;

public class DataCompare implements DiagInfo {

	// state vars
	private Data.List data;  // model data to analyze
	private Data.List refs;  // reference data
	private Data.List pointWgts; // point weights
	private double[] curveWgts; // curve weights

	// constructors
	public DataCompare(Data.List r) {
	    refs = r;
	}
	public DataCompare(Info info) throws Xcept {
	    data = new Data.List(info.data);
	    if (info.refs != null)
	    	refs = new Data.List(info.refs);
	    if (info.pointWgts != null)
	    	pointWgts = new Data.List(info.pointWgts);
	    curveWgts = info.curveWgts;
	}

	// independent copy
	public DataCompare copy() throws Xcept {
	    DataCompare c = new DataCompare(refs.copy());
	    if (data != null) c.data = data.copy();
	    if (pointWgts != null) c.pointWgts = pointWgts.copy();
	    if (curveWgts != null) {
		c.curveWgts = new double[curveWgts.length];
		for (int i=0; i<curveWgts.length; i++)
		    c.curveWgts[i] = curveWgts[i];
	    }
	    return c;
	}

	// query
	public int n() { return refs.size(); }
	public String diagInfo() { 
	    return "Data Residual";
	}
	public Data.List data() { return data; }
	public Data.List refs() { return refs; }
	public Data.List pointWgts() { return pointWgts; }
	public double[] curveWgts() { return curveWgts; }
	public Info info() throws Xcept { return new Info(this); }

	// set internal data
	public void setData(Data.List list) throws Xcept {
	    data = matchRefs(list);
	}
	public void setPointWgts(Data.List list) throws Xcept {
	    pointWgts = matchRefs(list);
	}
	public void setCurveWgts(double[] w) throws Xcept {
	    if (w != null && w.length != n()) 
	    	throw new Xcept(this,
		"curve weights array mismatch"); 
	    curveWgts = w;
	}

	// interpolate olist onto refs grids (if needed)
	public Data.List matchRefs(Data.List olist) throws Xcept {
	    if (olist == null) return null;  
	    if (olist.size() != n()) throw new Xcept(this, 
		"mismatched data list length");
	    Data.List nlist = new Data.List(n());   	  
	    for (int i=0; i<n(); i++) {
		Data rdata = refs.data(i);
		Data odata = olist.data(i);
		Data ndata = odata;
	
		// check dims, 0D data OK (same for all points)
		if (ndata.ndim() == 0) {
		    nlist.add(ndata);
		    continue;
		}
	
		if (odata.ndim() != rdata.ndim()) 
		    throw new Xcept(this,
			"mismatched data dimension");
		int ndim = odata.ndim();

		// resample if necessary
		GridData[] ngrids = new GridData[ndim];
		boolean resample = false;
		for (int j=0; j<ndim; j++) {
		    ngrids[j] = rdata.grid(j);
		    if (! rdata.grid(j).sameSamplesAs(odata.grid(j)))
			resample = true;
		}
		if (resample) {
		    if (ndata instanceof RealNData)
			ndata = new RealNData(
			    (RealNData) odata, ngrids, true);
		    else 
			ndata = rdata.grid(0);
		}
		nlist.add(ndata);
	    }
	    return nlist;
	}

	// RMS error between data and refs
	public double rmsError() throws Xcept {
	    if (data == null) throw new Xcept(this,
		"data not specified");
	    double curveWgtTot = 0;
	    double tot = 0;
	    for (int i=0; i<data.size(); i++) {
		double itot = 0;
		Data d = data.data(i);
		Data r = refs.data(i);
		Data p = (pointWgts != null) ?
		    pointWgts.data(i) : null;
		int n = r.nsamples();
		double pointWgtTot= 0;
		for (int j=0; j<n; j++) {
		    double dval = (d.ndim() == 0) ?
			d.realVal(0) : d.realVal(j);
		    if (Double.isNaN(dval)) continue;
		    double del = dval - r.realVal(j);
		    del = del*del;
		    double pwgt = 1;
		    if (p != null) 
			pwgt = (p.ndim() == 0) ?
			    p.realVal(0) : p.realVal(j); 
		    del *= pwgt;
		    itot += del;
		    pointWgtTot += pwgt;
		}
		if (pointWgtTot == 0) throw new Xcept(
		    "rmsError: point weights total=0. Perhaps misaligned model/ref domains?");
		itot = itot / pointWgtTot;
	        double cwgt = (curveWgts != null) ?  
		    curveWgts[i] : 1;
		itot *= cwgt;
		tot += itot;
		curveWgtTot += cwgt;
	    }
	    if (curveWgtTot != 0)
		tot = tot / curveWgtTot;
	    return Math.sqrt(tot);
        }

	// residuals between data and refs
	public Data.List residuals(boolean weighted) throws Xcept {
	    Data.List list = new Data.List(refs.size());
	    for (int i=0; i<refs.size(); i++)
		list.add(residual(i, weighted));
	    return list;
	}

	// one residual
	public RealNData residual(int i, boolean weighted) throws Xcept {
	    if (data == null) throw new Xcept(this,
		"data not specified");
	    Data d = data.data(i);
	    Data r = refs.data(i);
	    Data p = (pointWgts != null && weighted) ?
		pointWgts.data(i) : null;
	    
	    double cwgt = (curveWgts != null && weighted) ?
		curveWgts[i] : 1;
	    GridData[] grids = new GridData[r.ndim()];
	    for (int j=0; j<grids.length; j++) 
		grids[j] = r.grid(j);
	    String desc = d.legend() + " - " + r.legend();
	    RealNData resid = new RealNData(desc, 
		r.unit(), grids);
	    int ct = r.nsamples();
	    for (int j=0; j<ct; j++) {
		double x = d.realVal(j) - r.realVal(j);
		if (p != null) {
		    double pwgt = (p.ndim() == 0) ?
			p.realVal(0) : p.realVal(j);
		    x *= pwgt;
		}
	        x *= cwgt;
	        resid.set(j, x);
	    }
	    return resid;
	}
    
    // Get rms error for individual curve fits (if more than one)
    public IndividCrvResults[] compareSingleCurve() throws Xcept {
	DataInfo[] curveData =  new DataInfo[this.refs.size()];
	double[] curveW = this.curveWgts;  // get # of curvewgts: curveW.length
	curveData = this.refs.info();
	IndividCrvResults[] compareResults = new IndividCrvResults[curveData.length];
	for(int i=0; i<curveData.length; i++) {	      
	    Data nCurve =this.refs.dataForName(curveData[i].name);  // get individ curve
	    if(nCurve != null) {
		   Data.List individC = new Data.List(1);
		   individC.add(nCurve);	
		   if (curveW.length == curveData.length) {      
		       DataCompare individComp = new DataCompare(individC);
	       // curve wgt:
		       double[] individCwgt = {curveW[i]};
		       individComp.setCurveWgts(individCwgt);		      
	       // point weights
		       Data.List individPwgts = new Data.List(1); // just one curve 
		       Data individPwgt = pointWgts.data(i);
		       individPwgts.add(individPwgt);		     
		       individComp.setPointWgts(individPwgts);
	       // get data from model <-- Need to get only model data associated w/ curve   
		       Data.List individDlist = new Data.List(1);   // just use one curve
		       Data d = this.data.data(i);
		       if(d!= null) individDlist.add(d);
		       else throw new Xcept(this,"Error: curve data for comparison is null.");
		       individComp.setData(individDlist); 
		       double individErr = individComp.rmsError();
		       double avg = 0;
		       double sd = 0;
		       Data refPts = individC.data(0);
		       avg = dataAvg(refPts, individPwgt);
		       sd = dataSD(refPts, individPwgt);
		       if(avg <= 0)      // do not want negative or zero data avg.
			   compareResults[i] = new IndividCrvResults(nCurve.name(),individErr, 0,0 );  
		       else compareResults[i] = new IndividCrvResults(nCurve.name(),individErr, (individErr/avg), (individErr/sd) );
		   }
		   else throw new Xcept(this,"Error: Number of curves different then number of curve weights.");  
	       }
	   }
	return compareResults;
    }
    // calc weighted avg:
    public double dataAvg(Data d,Data pwts ) throws Xcept {
	double sumPts = 0;
	double sumPwts = 0;
	int nSamples = d.nsamples();
	int nPwts = pwts.nsamples();
	if( !((nPwts == nSamples) ||(nPwts==1)) ) 
	    throw new Xcept(this,"Point weights mismatch with data points");
	double pwt = 0;         // point weight value for a given pt.
	for(int j=0; j<nSamples;j++) 
	   {   
	       if(nPwts == 1) pwt = 1;   // no point weighting
	       else { 
		    if(!Double.isNaN(pwts.realVal(j))) pwt = pwts.realVal(j); 
		    else pwt = 0;
	       }  
	       if(!Double.isNaN(d.realVal(j))) {
		   sumPwts += pwt;      // sum point weights
		   sumPts += pwt*d.realVal(j); // sum weighted data pts 
	       }
	   }
	if(sumPwts ==0) return 0;   // divide by zero
	return sumPts/sumPwts;
    }

    // calc weighted SD for data:
    public double dataSD(Data d, Data pwts) throws Xcept {
	double avg = this.dataAvg(d, pwts);
	double ptDiffs = 0;
	double sumPwts = 0;
	double nSamples = (double)d.nsamples();
	int nPwts = pwts.nsamples();
	if( !((nPwts == nSamples) ||(nPwts==1)) ) 
	    throw new Xcept(this,"Point weights mismatch with data points");
	double pwt = 0;         // point weight value for a given pt.
	for(int j=0; j<nSamples;j++) 
	   {   
	       if(nPwts == 1) pwt = 1;   // no point weighting
	       else  { 
		   if(!Double.isNaN(pwts.realVal(j))) pwt = pwts.realVal(j);  
		   else pwt =0; 
	       }
	       if(!Double.isNaN(d.realVal(j))) {
		   sumPwts += pwt;       // sum point weights
		   ptDiffs += pwt*(Math.pow( (d.realVal(j)-avg),2) ); // pwt* sqr of diff 
	       }
	   }
	return Math.sqrt( ptDiffs/((nSamples-1)*sumPwts/nSamples) );

    }


	// Serializable Info
	public static class Info implements Serializable {
	    public DataInfo[] data;  // data to analyze
	    public DataInfo[] refs;  // reference data
	    public DataInfo[] pointWgts; // point weights
	    public double[] curveWgts; // curve weights

	    // constructor
	    public Info(DataCompare c) throws Xcept {
		data = c.data.info();
		if (c.refs != null) 
		    refs = c.refs.info();
		if (c.pointWgts != null) 
		    pointWgts = c.pointWgts.info();
		curveWgts = c.curveWgts;
	    }
	}


        public class IndividCrvResults {
	    public String curveName;  // name of ref data curve
	    public double rmsErr;     // root mean sqr error
	    public double rrmsErrAvg; // relative rms (weighted mean <-- Not used currently
            public double rrmsErrSD;  // rel rms (weighted standard deviation)

	    public IndividCrvResults(String n, double rms, double rrmsAvg, double rrmsSD) throws Xcept {
		curveName = new String(n);
		rmsErr = rms;
		rrmsErrAvg = rrmsAvg;
		rrmsErrSD = rrmsSD;
	    }
	    public IndividCrvResults() throws Xcept {
		rmsErr = Double.NaN;
		rrmsErrAvg = Double.NaN;
		rrmsErrSD = Double.NaN;
	    }

	}
		
}
