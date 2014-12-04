/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// XSim parameter file

package JSim.xsim;

import java.io.*;	// File and PrintStream
import java.util.*;	

import JSim.util.*;
import JSim.jruntime.*;
import JSim.aserver.*; import JSim.project.*;

public class XSParFile {
	// instance info
	private String fileName;
	private BufferedReader rdr;
	private int lineCt;
	private String currGroup;
	private Par.NList pars;	   // all pars
	private Par.NList mpars;   // model pars
	private StringList feeds;  // names of feeds 
	private StringList report; // from last assignModelPars	

	// constructor loads pars from file
	public XSParFile(File f) throws Xcept {
	    try {
		pars = readFile(f);
	    } catch (Exception e) {
		throw Xcept.wrap(e);
	    }
	    updateSlaveValues(pars);
	    mpars = makeModelPars(pars);
	    feeds = makeFeeds();
	    report = new StringList(32);
	}

	// read all Pars
	private Par.NList readFile(File f) throws Exception {

	    // open file
	    fileName = f.getAbsolutePath();
	    FileReader frdr = new FileReader(f);
	    rdr = new BufferedReader(frdr);
	    currGroup = null;
	    Par.NList pars = new Par.NList(64);

	    // check header
	    String line = getLine();
	    if (line == null || ! line.equals("xs01")) throw new Xcept(
		"First line of XSIM par file must be xs01");

	    // read it
	    Par par;
	    while((par = nextPar()) != null) {
		debug("--reading " + par.name() + "=" + par.value);
		pars.add(par);
	    }
	    return pars;
	}

	// read next par
	private Par nextPar() throws Exception {

	    // loop till find par declaration
	    while (true) {
	    	String line = getLine();
		if (line == null) return null;
		line = quotedSpacesToUnderbars(line);
		StringTokenizer stok = 
		    new StringTokenizer(line);
		int ct = stok.countTokens();
		if (ct<2) continue;
		String word1 = Util.stripQuotes(stok.nextToken());
		String word2 = stok.nextToken();

		// end/begin group?
		if (word2.equals("{")) {
		    currGroup = (currGroup == null) ? 
			word1 : currGroup + "." + word1;
		    continue;
		}
		if (word2.equals("}")) {
		    int inx = currGroup.lastIndexOf('.');
		    currGroup = (inx>=0) ?
			currGroup.substring(0, inx) : null;
		    continue;
		}		

		// is Par
		Par par = new Par(currGroup, word1, word2);
		if (ct>=4) {
		    stok.nextToken();
		    par.value = getRemainingTokens(stok);
		} else {
		    line = getLine();
		    if (line == null) return null;
		    stok = new StringTokenizer(line);
		    par.value = getRemainingTokens(stok);
		}
		par.value = Util.stripQuotes(par.value);
		par.value = hackIfElse(par.value);
		return par;
	    }
	}

	// get rest of token
	private String getRemainingTokens(StringTokenizer stok) {
	    String s = stok.nextToken();
	    if (s == null) return s;
	    while (stok.hasMoreTokens())
	    	s = s + " " + stok.nextToken();
	    return s;
	}
	
	// get a line from file
	private String getLine() throws Exception {
	    while (true) {
		lineCt++;
	        String s = rdr.readLine();
		if (s == null) return s;
		int i=0;
		while (i<s.length() 
		&& Character.isWhitespace(s.charAt(i))) 
		    i++;
		if (i>=s.length()) continue;
		char c = s.charAt(i);
		if (c == '-' || c == '.' || Character.isDigit(c))
		    continue;
		return s;
	    }    
	}

	// hack :? to if else
	private String hackIfElse(String s) {
	    if (s == null) return s;
	    int qinx = s.indexOf('?');
	    int cinx = s.lastIndexOf(':');
	    if (qinx < 0 || cinx < 0 || qinx >= cinx)
		return s;
	    String a = s.substring(0,qinx-1);
	    String b = s.substring(qinx+1, cinx);
	    String c = s.substring(cinx+1);
	    if (Util.isBlank(a)) return s;
	    if (Util.isBlank(b)) return s;
	    if (Util.isBlank(c)) return s;
	    s = "if (" + a.replace("==", "=") + ") (" + 
	    	b + ") else (" + c + ")";
	    return s;
	}

	// line with quoted spaced replaced by underbars
	private String quotedSpacesToUnderbars(String s) {
	    StringBuffer buf = new StringBuffer();
	    boolean quoted = false;
	    for (int i=0; i<s.length(); i++) {
		char c = s.charAt(i);
		if (c == '"') quoted = !quoted;
		if (c == ' ' && quoted) c = '_';
		buf.append(c);
	    }
	    return buf.toString();
	}

	// read error
	private void readError(String msg) throws Xcept {
	    throw new Xcept(msg + "; at line #" + lineCt 
		+ " of file " + fileName);
	}

	// if par has slave.eval=master+-N,  master.val <- slave.val
	private void updateSlaveValues(Par.NList pars) throws Xcept {
	    for (int i=0; i<pars.size(); i++) {
	    	Par peval = pars.par(i);
		if (! peval.group.startsWith("model")) continue;
		if (! peval.name.equals("eval")) continue;
		Par pslave = pars.par(peval.group + ".val");
		if (pslave == null) continue;
		debug(" slave=" + pslave.group + 
		    " val=" + pslave.value + " eval=" + peval.value);
		String nmaster = masterName(peval.value);
		if (nmaster == null) continue;
		nmaster = "model." + nmaster + ".val";
		Par pmaster = pars.par(nmaster);
		if (pmaster == null) continue;
		debug("  " + nmaster + 
		    ": " + pmaster.value + " <- " + pslave.value);
		pmaster.value = pslave.value;
	    }
	}    		

	// calc master name from eval
	private static String masterName(String eval) {
	    if (Util.isBlank(eval)) return null;
	    int n = eval.length();
	    while (n>0 && Character.isDigit(eval.charAt(n-1))) n--;
	    if (n==eval.length() || n<2) return null;
	    char op = eval.charAt(n-1);
	    if (op != '+' && op != '-') return null;
	    return eval.substring(0, n-1);
	}
		
	// print debug message
	private static void debug(String s) {
	    // System.err.println(s);
	}

	// create model pars
	private Par.NList makeModelPars(Par.NList pars)
	throws Xcept {
	    Par.NList mpars = new Par.NList(64);
	    for (int i=0; i<pars.size(); i++) {
		Par par = pars.par(i);
		if (! par.group.startsWith("model")) 
		    continue;
		String name = par.subgroup();
		name = badCharsToUnderbars(name);
		if (mpars.size() < 4) {
		    name = replaceSfx(name, "_start", ".min");
		    name = replaceSfx(name, "_stop", ".max");
		    name = replaceSfx(name, "_incr", ".delta");
		}
		if (mpars.par(name) != null) continue;

		// create par, fill in value
//		debug("--make model par " + name);
		Par mpar = new Par("", name, "4");
		Par xpar = pars.par(par.group + "." + "val");
		if (xpar != null) 
		    mpar.value = xpar.value;
		xpar = pars.par(par.group + "." + "eval");
		if (xpar != null) 
		    mpar.value = xpar.value;
		mpars.add(mpar);
	    }

	    return mpars;
	}

	// replace suffix if matches
	private String replaceSfx(String s, String sfx1, String sfx2) {
	    int l = s.length();
	    int l1 = sfx1.length();
	    if (l < l1+1) return s;
	    String send = s.substring(l-l1);
	    if (sfx1.equals(send))
		return s.substring(0,l-l1) + sfx2;
	    return s;
	}

	// replace non-alphanumerics with underbars
	private String badCharsToUnderbars(String s) {
	    StringBuffer buf = new StringBuffer();
	    boolean quoted = false;
	    for (int i=0; i<s.length(); i++) {
		char c = s.charAt(i);
		if (! Character.isLetterOrDigit(c))
		    c = '_';
		buf.append(c);
	    }
	    return buf.toString();
	}

	// find func generators
	public StringList makeFeeds() throws Xcept {
	    StringList feeds = new StringList(32);
	    for (int i=0; i<mpars.size(); i++) {
		Par mpar = mpars.par(i);
		String name = mpar.name;
		if (! name.endsWith("_p1tbegn")) continue;
		int ct = name.length();
		String s = name.substring(0, ct-8);
		feeds.addUniq(s);
	    }
	    return feeds;
	}    	    

	// assign to PModel
	public void assignModelPars(PModel pmodel) throws Xcept {
	    report = new StringList(32);
	    report.add("Imported " + fileName + " into model " + pmodel.name());
	    StringList missingPars = new StringList(16);
	    StringList setPars = new StringList(16);

	    // loop over pars,  figure which to skip
	    ASModel rt = pmodel.rt();
	    for (int i=0; i<mpars.size(); i++) {
		Par mpar = mpars.par(i);
		String name = mpar.name;
		String val = mpar.value;
		String skip = null;
		ASVar v = null;
		try { v = rt.getASVar(name); } 
		    catch (Xcept e) { }
		if (val == null) 
		    skip = "null value";
		else if (name.startsWith("solflg__"))
		    skip = "solver";
		else if (val.equals(name + "_sw-1"))
		    skip = "switch";
		else if (val.equals(name + "_int"))
		    skip = "switch";
		for (int j=0; j<feeds.size(); j++) {
		    String fname = feeds.str(j);
		    if (name.startsWith(fname + "_"))
			skip = "feed";
		}
		if (v != null && ! v.isInput()) skip = "output";
		if (name.indexOf(':')>=0) skip = "derivative";

		// messages for skipped pars
		if (v == null && skip == null) {
		    // report.add("    model is missing parameter " + name);
		    missingPars.add(name);
		    continue;
		}
		if (skip != null) {
		    // report.add("    skipping " + skip + " parameter " + name);
		    continue;
		}

		// OK to assign
		// report.add("    assigning " + name + "=" + val);
		StringControl cntl = 
		    (StringControl) pmodel.vars().control(name);
		cntl.setVal(val);
		if (! cntl.valid()) {
		    report.add("    Invalid parameter assignment: " + name + "=" +  val);
		}			
		setPars.add(name);
	    }

	    // which rtmodel inputs not set?
	    ASVar.List asvars = rt.getASVars();
	    StringList unsetPars = new StringList(16);
	    for (int i=0; i<asvars.size(); i++) {
		ASVar v = asvars.asvar(i);
		if (! v.isInput()) continue;
		String n = v.name();
		if (setPars.containSame(n)) continue;
		if (feeds.containSame(n)) continue;
		unsetPars.add(v.name());
	    }

	    // report summay
	    report.add("    Feed parameters not imported: " + 
		((feeds.size() == 0) ? "none" : feeds.toString()));
//	    report.add("    File parameters missing from model: " + 
//		((missingPars.size() == 0) ? "none" : missingPars.toString()));
	    report.add("    Model parameters missing from file: " + 
		((unsetPars.size() == 0) ? "none" : unsetPars.toString()));
	}

	// assign funcgen parms for one feed
	public void assignFgen(FuncGen f, String pbase)
	throws Xcept {
	    setCntl(f, "which", pbase + "fselect.val",
		new String[] {
		"1", "Pulse1",
		"2", "Pulse2",
		"3", "Pulse3",
		"4", "Sine",
		"5", "Sawtooth",
		// 6 is square wave train,  no equiv in JSim
		"7", "LagNormal",
		"8", "GammaVar",
		"9", "Poisson",
		"10", "Gaussian",
		"11", "Exponential",
		"12", "RandomWalk" });
	    setCntl(f, "Pulse1.startTime", pbase + "p1tbegn.val");
	    setCntl(f, "Pulse1.duration", pbase + "p1tdur.val");
	    setCntl(f, "Pulse1.amplitude", pbase + "p1amp.val");
	    setCntl(f, "Exponential.area", pbase + "dfarea.val");
	    setCntl(f, "Exponential.tMean", pbase + "dfmean.val");
	    setCntl(f, "Exponential.rd", pbase + "dfrd.val");
	    setCntl(f, "Gaussian.area", pbase + "dfarea.val");
	    setCntl(f, "Gaussian.tMean", pbase + "dfmean.val");
	    setCntl(f, "Gaussian.rd", pbase + "dfrd.val");
	    setCntl(f, "LagNormal.area", pbase + "dfarea.val");
	    setCntl(f, "LagNormal.tMean", pbase + "dfmean.val");
	    setCntl(f, "LagNormal.rd", pbase + "dfrd.val");
	    setCntl(f, "LagNormal.skewn", pbase + "dfskew.val");
	    setCntl(f, "LagNormal.frPeak", pbase + "dffrpeak.val");
	    setCntl(f, "GammaVar.area", pbase + "dfarea.val");
	    setCntl(f, "GammaVar.tMean", pbase + "dfmean.val");
	    setCntl(f, "GammaVar.rd", pbase + "dfrd.val");
	    setCntl(f, "GammaVar.skewn", pbase + "dfskew.val");
	    setCntl(f, "GammaVar.frPeak", pbase + "dffrpeak.val");
	    setCntl(f, "Poisson.area", pbase + "dfarea.val");
	    setCntl(f, "Poisson.tMean", pbase + "dfmean.val");
	    setCntl(f, "Poisson.rd", pbase + "dfrd.val");
	    setCntl(f, "Poisson.frPeak", pbase + "dffrpeak.val");
	    setCntl(f, "RandomWalk.area", pbase + "dfarea.val");
	    setCntl(f, "RandomWalk.tMean", pbase + "dfmean.val");
	    setCntl(f, "RandomWalk.rd", pbase + "dfrd.val");
	    setCntl(f, "RandomWalk.skewn", pbase + "dfskew.val");
	    setCntl(f, "RandomWalk.frPeak", pbase + "dffrpeak.val");
	}

	// one plot page
	public void addPlotPage(PModel pmodel, String pbase, String pname)
	throws Xcept {
	    Project proj = pmodel.project();
	    PlotPage page = new PlotPage(proj, pname);
	    Plot plot = page.plot(0);
	    int j=0;
	    for (int i=0; i<9; i++) {
		String sfx = (i==0) ? "" : "" + (i+1);
		String ydesc = parVal(pbase + "ydesc" + sfx);
		if (ydesc == null) continue;
		plot.nItems.setVal(j+1);
		PlotItem item = plot.item(j++);
		item.expr.setVal(ydesc);
		item.dataSrc.setVal(pmodel.name());
		setCntl(item, "xExpr", pbase + "xdesc");
		setCntl(item, "color", pbase + "ycolor" + sfx, 
		    new String[] { "1", "black", "2", "gray", 
			"3", "salmon", "4", "violet",
			"5", "brown", "6", "red",
			"7", "forest", "8", "blue" });
		setCntl(item, "size", pbase + "ysize" + sfx,
		    new String[] { "1", "small", 
			"2", "normal", "3", "big" });
		item.line.setVal("none");
	    }
	    setCntl(plot, "xaxis.min", pbase + "xmin");
	    setCntl(plot, "xaxis.max", pbase + "xmax");
	    setCntl(plot, "xaxis.autoscale", pbase + "xauto");
	    setCntl(plot, "xaxis.log", pbase + "xlinear",
		new String[] { "true", "linear", "false", "log" });
	    setCntl(plot, "yaxis.min", pbase + "ymin");
	    setCntl(plot, "yaxis.max", pbase + "ymax");
	    setCntl(plot, "yaxis.autoscale", pbase + "yauto");
	    setCntl(plot, "yaxis.log", pbase + "ylinear",
		new String[] { "true", "linear", "false", "log" });
	}

	// get par val,  or null if par missing
	public String parVal(String n) {
	    Par par = pars.par(n);
	    if (par == null) return null;
	    String v = par.value();
	    if (Util.isBlank(v)) return null;
	    return v;
	}   

	// set project var
	public void setCntl(PNamed base, String child, String parname)
	throws Xcept {
	    setCntl(base, child, parname, null);
	}

	// set project var
	public void setCntl(PNamed base, String child, String parname,
	String[] subs)	throws Xcept {
	    Control cntl = (Control) base.nestedChild(child);
	    if (cntl == null) 
		throw new Xcept(base, child + " unknown");
	    String v = parVal(parname);
	    if (v == null) return;
	    if (subs == null) {
		cntl.setVal(v);
		return;
	    }
	    for (int i=0; i<subs.length; i+=2) {
		if (! v.equals(subs[i])) continue;
		cntl.setVal(subs[i+1]);
		return;
	    }
	    report.add("  parm " + parname + "=" +
		v + " not translatable to JSim");	    
	}

	// query
	public Par.NList pars() { return pars; }
	public Par.NList mpars() { return mpars; }
	public StringList report() { return report; }

	// XSParFile.Par class
	public static class Par implements Named {
	    public String group;
	    public String name;
	    public int type;
	    public String value;

	    // constructor
	    public Par(String g, String n, String t) 
	    throws Xcept {
		group = g;
		name = n;
		type = Util.toInt(t);
	    }

	    // simple query
	    public String name() { 
		return group + "." + name; 
	    }
	    public String diagInfo() {
		return "XSPar " + name();
	    }
	    public double realVal() { 
		return Util.toDouble(value); 
	    }
	    public boolean boolVal() {
		if (value.equalsIgnoreCase("f")) return false;
		if (value.equalsIgnoreCase("false")) return false;
	        if (value.equals("0")) return false;
		return true;
	    }
	    public String subgroup() {
		int inx = group.lastIndexOf('.')+1;
		if (inx < 0 || inx>=group.length()) return "";
		return group.substring(inx);
	    }
	    public String value() {
		if (type != 1) return value;
		return boolVal() ? "true" : "false";
	    }
	
	    // Par.NList
	    public static class NList extends NamedList {
		public NList(int i) { super(i); }
		public Par par(int i) { return (Par) get(i); }
		public Par par(String n) { return (Par) getByName(n); }

		public void dump() {
		    for (int i=0; i<size(); i++) {
			Par par = par(i);
			debug(par.name() + "=" + par.value());
		    }
		}
	    }	    
	}

	// test masterName
	public static void main(String[] args) {
	    System.out.println("" + masterName(args[0]));
	}
}

