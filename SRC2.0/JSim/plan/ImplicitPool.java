/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// search eqns for implicit blocks

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class ImplicitPool {
	private ToolBox box;
	private TModel model;
	private int maxBlock;
	private double maxSearch;
	private VarUsage[] gvus;
	private Hashtable<VarUsage, Integer> gvumap;
	private PoolEqn[] peqns;

	// loop work storage to reduce parm passing / realloc
	private int pm, pn; 	
	private PoolEqn[] pblock;
	private int[] pblockVuxs;
	
	// constructor
	public ImplicitPool(ToolBox box) {
	    this.box = box;
	    model = box.model;
	    maxBlock = box.plan.maxImplicitBlock();
	    maxSearch = box.plan.maxImplicitSearch();
	}
	
	// build for restricted subdomain,  return tool
	public ImplicitTool makeTool(TSubDom sd) throws Xcept {

	    // build list of eqns, varusages to consider
	    ArrayList<PoolEqn> plist = new ArrayList<PoolEqn>();
	    LinkedHashSet<VarUsage> vuset = new LinkedHashSet<VarUsage>();
	    for (int i=0; i<model.eqns.size(); i++) {
	    	TEqn eqn = model.eqns.get(i);
		if (box.isUsed(eqn, sd)) continue;
		VarUsages vus = eqn.usages();
		if (! sd.isEntire()) vus = vus.restrict(sd);
	  	vus = box.unknown(vus, maxBlock+1);
		if (vus.size() > maxBlock) continue;
		if (vus.size() < 2) throw new Xcept(
		    "ImplicitPool not cleaned: " + eqn);
		if (! vus.areSolvable()) continue;
		PoolEqn peqn = new PoolEqn(eqn, vus);
		for (int j=0; j<vus.size(); j++)
		    vuset.add(vus.get(j));
		plist.add(peqn);
	    }
	    
	    // sort eqns, load gvu indexes
	    Collections.sort(plist);
	    peqns = plist.toArray(new PoolEqn[0]);
	    gvus = vuset.toArray(new VarUsage[0]);

	    gvumap = new Hashtable<VarUsage, Integer>();
	    for (int i=0; i<gvus.length; i++) 
	    	gvumap.put(gvus[i], new Integer(i));
	    for (int i=0; i<peqns.length; i++)
	    	peqns[i].loadVuxs();

	    // try duples, triples, quads, etc.
	    ImplicitTool tool = null;
	    long t = System.currentTimeMillis();
	    for (int m=2; m<=maxBlock; m++) {
	        createBlock(m);
		if (pblock == null) continue;
		double ct = pascal(pn, pm);
	   	String msg = "  implicit block (" + pn + " " + pm + 
		    ")=" + ct;
	    	if (ct > maxSearch) 
		    msg = msg +" exceeds maxSearch=" + maxSearch;
		log(msg);
		if (ct > maxSearch) return null;
		tool = buildBlockLoop(0, 0);
		if (tool != null) break;
	    }

	    t = System.currentTimeMillis() - t;
//	    log("  implicit search elapsed time: " + t + " msec");
	    return tool;
	}
	
	// create pblock for block size m
	public void createBlock(int m) throws Xcept {

	    // count # peqns with nvus<=m
	    pblock = null;
	    int n = 0;
	    while (n<peqns.length && peqns[n].nvus <= m) n++;
	    if (n < m) return;

	    // loop over m-tuples
	    pblock = new PoolEqn[m];
	    pblockVuxs = new int[m+1]; 
	    pn = n;
	    pm = m;
       	}	       
	    	
	// build m-tuple
	private ImplicitTool buildBlockLoop(int mx, int nlo) throws Xcept {
	    int nhi = pn - pm + mx + 1;
	    for (int nx=nlo; nx<nhi; nx++) {
		pblock[mx] = peqns[nx];
		if (mx < pm-1) {
		    ImplicitTool tool = buildBlockLoop(mx+1, nx+1);
		    if (tool != null) return tool;
		} else if (testBlock())
		    return makeTool();
	    }
	    return null;
	}		    

	// test block
	private boolean testBlock() {
	    int ct = 0;
	    for (int i=0; i<pblock.length; i++) {
	    	PoolEqn peqn = pblock[i];
		for (int j=0; j<peqn.nvus; j++) {
		    int k = peqn.vuxs[j];
		    boolean add = true;
		    for (int l=0; l<ct; l++) {
		    	if (pblockVuxs[l] == k) {
			    add = false;
			    break;
			}
		    }
		    if (!add) continue;
		    pblockVuxs[ct++] = k;
		    if (ct > pm) return false;
		}
	    }
	    return true;
	}

	// make tool from pblock
	private ImplicitTool makeTool() throws Xcept {
	    VarUsages vus = new VarUsages(model);
	    ArrayList<TExpr> exprs = new ArrayList<TExpr>();
	    for (int i=0; i<pblock.length; i++) {
	    	PoolEqn peqn = pblock[i];
		vus.add(peqn.vus);
		exprs.add(peqn.eqn.expr());
	    }
	    ImplicitTool tool = new ImplicitTool(vus, exprs);
 	    for (int i=0; i<pblock.length; i++) 
	        tool.eqns.add(pblock[i].eqn);
	    return tool;
	}
		    
	// messages
	public void log(String s) { box.log(s); }

	// Pool Equation
	public class PoolEqn implements Comparable<PoolEqn> {
	    public TEqn eqn;
	    public VarUsages vus;
	    public int nvus;
	    public int[] vuxs;
  	    public PoolEqn(TEqn eqn, VarUsages vus) {
	    	this.eqn = eqn;
		this.vus = vus;
		nvus = vus.size();
	    }
	    public void loadVuxs() throws Xcept {
	    	vuxs = new int[nvus];
		for (int i=0; i<nvus; i++) {
		    VarUsage vu = vus.get(i);
		    Integer inx = gvumap.get(vu);
		    if (inx == null) throw new Xcept(
		    	"ImplicitPool: gvumap missing " + vu);
		    vuxs[i] = inx.intValue();
		}
	    }
	    public int compareTo(PoolEqn peqn) {
	    	return nvus - peqn.nvus;
	    }
	}

	// n items taken m at a time
	private static double pascal(int n, int m) {
	    if (m>n) m=n;
	    double s = 1;
	    for (int i=0; i<m; i++) {
	    	s = s*(n-i)/(i+1);
//		System.out.println("  i=" + i + " s=" + s);
	    }
	    return s;
	}

	// test nsearches(p,n)
	public static void main(String[] args) throws Exception {
	    int n = Util.toInt(args[0]);
	    for (int m=0; m<=n; m++) {
	    	double s = pascal(n, m);
	        System.out.print("  " + s);
	    }
	    System.out.println();
	}
}
