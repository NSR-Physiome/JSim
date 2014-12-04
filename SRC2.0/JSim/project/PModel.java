/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Single model in project

package JSim.project; import JSim.aserver.*;

import java.io.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

public class PModel extends PNamed {

	// controls
	public StringControl easelVariant; // blank=none
	public TextControl notes;
	public TextControl modelSource; // model source code
	public ChoiceControl sourceType; // type of source
	public StringControl parSetName; // name of parset loaded
	public BooleanControl built; // model built?

	// phase out these 2
	public TextControl customRTMLText; // custom RTML text (deprecated)
	public BooleanControl customBuilt; // is custom RTML text realized

	// permanent state
	private ASModel rt;  // run-time connection
	private PModelBuildOptions options; // build options
	private PModelVars vars; // model vars
	private ParSet lastParSet; // ParSet used when last built
	private PImageSet images; // Model's image set
	private PModelLoops loops; // Model loop control
	private PModelSens sens; // Model sensitivity control
	private PModelOptim optim; // Model optimizer control
	private PModelMonte monte; // Monte-carlo control
	private PModelBrowser browser; // Model browser
	private PModelRTML customRTML; // customized RTML
	   // updated before build or XML save
	protected long timeStamp; // time of last run or 0
	protected boolean isRunning; // set by PJobs
	private boolean parsModified; // in model since parset save
	private boolean tempParsModified; // used during importXML()

	// new (empty) model constructor
	public PModel(PNamed p, String n) throws Xcept {
	    super(p, safeName(n));
	    addDescCntl();
	    easelVariant = new StringControl(this, "easelVariant");
	    notes = new TextControl(this, "notes");
	    modelSource = new TextControl(this, "modelSource");
	    sourceType = new ChoiceControl(this, "sourceType", 1,
		new String[] { "xml", "mml", "flat",
		"java" });
	    sourceType.setVal(ASModel.TEXT_MML);
	    parSetName = new StringControl(this, "parSetName");
	    parSetName.setVal("");
	    parsModified = false;
	    built = new BooleanControl(this, "built", false);
	    customRTMLText = new TextControl(this, "customRTML");
	    customBuilt = new BooleanControl(this, "customBuilt", false);
	    options = new PModelBuildOptions(this, "buildOptions");
	    vars = new PModelVars(this, "vars");
	    lastParSet = new ParSet(this, "last", true);
	    images = new PImageSet(this, "images");
	    loops = new PModelLoops(this, "loops");
	    sens = new PModelSens(this, "sens");
	    optim = new PModelOptim(this, "optim");
	    monte = new PModelMonte(this, "montecarlo");
	    browser = new PModelBrowser(this, "browser");
	    customRTML = new PModelRTML(this, "rtml");
	    rt = server().newModelRT();
	}

	// create model from File or URL
	public PModel(Project p, JSReadable readable, 
	String src, boolean junk) throws Xcept {
	    this(p, p.loadName(readable));
	    setSource(readable, src);
	} 

	// load model source from File or URL
	public void setSource(JSReadable readable, String text) 
	throws Xcept {
	    modelSource.setVal(text);
	    int type = (readable.fileType() == JSReadable.JAVA) ?
	        ASModel.TEXT_JAVA : ASModel.TEXT_MML;
	    sourceType.setVal(type);
	}
	
	// get class name from .java file/URL
	private String getClassName(JSReadable readable) throws Xcept {
	    String hdr = "// JSim generated model ";
	    String line = readable.readFirstLine();
	    if (! line.startsWith(hdr))
	        throw new Xcept(readable,
		    ".java models must start with <" +
		    hdr + "classname>.");
	    String s = line.substring(hdr.length());
	    return safeName(s);
	}
	    
	// import external model formats
	private void importXMLModel(JSReadable readable, 
	String options) throws Xcept {
	    sourceType.setVal(ASModel.TEXT_MML);
	    modelSource.setVal("");
	    String xtext = readable.readText();
	    try {
		String mml = server().translateModelText(
		    ASModel.TEXT_XML, ASModel.TEXT_MML, xtext, options);
		modelSource.setVal(mml);
	    } catch (Xcept e) {
	    	e.dinfo1 = readable;
		throw e;
	    }
	}

	// query
	public PModelBuildOptions options() { return options; }
	public PModelVars vars() { return vars; }
	public PModelLoops loops() { return loops; }
	public PModelSens sens() { return sens; }
	public PModelOptim optim() { return optim; }
	public PModelMonte monte() { return monte; }
	public PModelBrowser browser() { return browser; }
	public PModelRTML customRTML() { return customRTML; }
	public ASModel rt() { return rt; } 
	public String diagInfo() { return "Model " + name; }
	public String xmlLabel() { return "model"; }
	public String parSetDesc() {
	    String s = parSetName.stringVal();
	    if (parsModified) s = s +  " (modified)";
	    return s;
	}
	public PImageSet images() { return images; }
	public long timeStamp() { return timeStamp; }
	public boolean isRunning() { return isRunning; }
	public ParSet lastParSet() { return lastParSet; }
	public boolean getParsModified() { return parsModified; }

	// # of user queriable stores
  	public int nQueryStores() {
	    if (! rt.isBuilt()) return 0;
	    if (rt.storeMode() != ASModel.LOOPS) return 1;
	    return rt.nstores();
	}

	// set run state (from PJobs)
	protected void setRunning(boolean b) { isRunning = b; }

	// import all PModel XML
	public void importXMLForce(Element e) {
	    tempParsModified = false; // ???
	    super.importXMLForce(e);
	    setParsModified(tempParsModified);
	}

	// import XML child
	public PNamed importXMLChild(Element c) {

	    // parsModified no longer a control in 2.05+
	    if (c.getNodeName().equals("control") &&
		c.getAttribute("name").equals("parSetModified")) {
		try {
		    String s = c.getAttribute("value");
		    tempParsModified = Util.toBoolean(s);
		} catch (Xcept e) {
		    importXMLMessage(e.cleanMessage());
		}
		return null;
	    }

	    // import "state" control (before v1.6.14) 
	    if (c.getNodeName().equals("control") &&
		c.getAttribute("name").equals("state")) {
		String s=c.getAttribute("value");
		boolean unbuilt = s.equals("editing") 
		    || s.equals("building");
		try {
		    built.setVal(!unbuilt);
	        } catch (Xcept e) {
		    importXMLMessage(e.cleanMessage());
	 	}
		return null;
	    } 

	    // support legacy "PNamed" optim tag (before 1.6.42)
	    if (c.getNodeName().equals("PNamed") &&
		c.getAttribute("name").equals("optim")) {
		optim.importXMLForce(c);
		return optim;
	    }

	    // otherwise normal
	    return super.importXMLChild(c);
	}

	// export XML
	public void exportExtraXML(Element base) throws Xcept {
	    Document doc = base.getOwnerDocument();
	    if (rt != null) 
		lastParSet.store(this);

	    // parsModified was control before 2.05
	    Element e = doc.createElement("control");
	    e.setAttribute("name", "parSetModified");
	    e.setAttribute("value", "" + parsModified);
	    base.appendChild(e);
	}

	// pre-build operations
	public void preBuild() throws Xcept {
	   
	    // release previous model stuff ???

	    // if really built (rt null check is for XML file read)
	    if (built.val() && rt.isBuilt())
		lastParSet.store(this);

	    // initialize
	    built.setVal(false);
	    vars.rollover(null);
	}

	// load ASInfo.Build
	public void loadBuildInfo(ASInfo.Build info) throws Xcept {
	    modelSource.setVal(info.modelSource);
	    sourceType.setVal(info.sourceType);
	    options().setControls(info.options);
	}    

	// create ASInfo.Build
	public ASInfo.Build createBuildInfo() throws Xcept {
	    ASInfo.Build info = new ASInfo.Build();
	    info.name = name();
	    info.modelSource = modelSource.val();
	    info.sourceType = sourceType.val();
	    info.options = new NamedVal.NList();
	    for (int i=0; i<options.nChild(); i++) {
	    	Control c = (Control) options.child(i);
	    	info.options.add(c.namedVal(c.name()));	    
	    }
	    for (int i=0; i<appl().buildOptions.size(); i++) 
	    	info.options.add(appl().buildOptions.get(i));
	    return info;
	}

	// post-build operations
	public void postBuild() throws Xcept {
	    built.setVal(true);
	    vars().memory().updateDomains();

	    // restore ctnls, old pars
	    boolean saveModified = parsModified;
	    vars.rollover(rt);	    
	    lastParSet.load(this);
	    setParsModified(saveModified);
	}

	// clear RT
	public void clearRT() throws Xcept {
	    built.setVal(false);
	    vars().memory().updateDomains();
	    if (rt.isBuilt()) rt.unbuildRT();
	    vars.rollover(null);
	}

	// store ParSet in project
	public ParSet storeParSet(String n) throws Xcept {

	    // find or create ParSet
	    PNamed pnamed = project().child(n);
	    if (pnamed != null && !(pnamed instanceof ParSet)) 
		throw new Xcept(this, n + " does not refer to ParSet");
	    ParSet pset = (pnamed == null) ?
		new ParSet(project(), n) : (ParSet) pnamed;

	    // store and update controls
	    pset.store(this);
	    parSetName.setVal(n);
	    setParsModified(false);
	    return pset;
	}

	// load ParSet
	public void loadParSet(String n) throws Xcept {
	    // find ParSet
	    PNamed pnamed = project().child(n);
	    if (pnamed == null) throw new Xcept(this,
		"ParSet " + n + " not found");
	    if (! (pnamed instanceof ParSet)) throw new Xcept(this,
		n + " does not refer to ParSet");
	    ParSet pset = (ParSet) pnamed;

	    // load and update controls
	    pset.load(this);
	    parSetName.setVal(n);
	    setParsModified(false);
	}

	// load default Pars
	public void defaultParSet() throws Xcept {
	    lastParSet.clearGroups();
	    lastParSet.load(this);
	    vars.setDefaults();
	    parSetName.setVal("");
	    setParsModified(false);
	}	    

	// 1st domain, or null if unavailable
	public ASVar firstDomain() throws Xcept {
	    if (! rt.isBuilt()) return null;
	    ASVar.List asvars = rt.getASVars();
	    for (int i=0; i<asvars.size(); i++) 
		if (asvars.asvar(i).isDomain())
		    return asvars.asvar(i);
	    return null;
	}

	// forwardable IC vars
	public ASVar.List forwardableICVars() throws Xcept {
	    if (! rt.isBuilt()) throw new Xcept(
		"Model is not compiled.");
	    if (rt.nstores() == 0) throw new Xcept(
		"Model has no stored run data.");

	    // find state vars for IC variables
	    ASVar.List asvars = rt.getASVars();
	    ASVar.List vfwd = new ASVar.List(16);
	    for (int i=0; i<asvars.size(); i++) {
	    	ASVar v0 = asvars.asvar(i);
		if (! v0.isInput()) continue;
		String n = v0.name();
		if (! n.endsWith("__init")) continue;
		n = n.substring(0, n.length()-6);
		ASVar v = rt.getASVar(n);
		if (v == null) continue;
		if (v.ndim() != 1) continue;
		vfwd.add(v);
	    }

	    if (vfwd.size() == 0) throw new Xcept(
		"No forwardable ICs found.");
	    return vfwd;
	}

	// return IC forwardable domains
	public Data.List forwardableICDomains() throws Xcept {
	    StringList xnames = new StringList();
	    Data.List grids = new Data.List(2);
	    ASVar.List vfwd = forwardableICVars();
	    for (int i=0; i<vfwd.size(); i++) {
	    	ASVar v = vfwd.asvar(i);
		ASVar x = v.domain(0);
		if (xnames.containSame(x.name())) continue;
		xnames.addUniq(x.name());
		Data xdata = rt.getData(0, x);
		if (xdata != null) grids.add(xdata);
	    }
	    return grids;
	}
	    
	// forward ODE ICs from last run
	public ASVar.List forwardICs(NamedVal.List nvals) throws Xcept {
	    ASVar.List vfwd = forwardableICVars();
	    ASVar.List vdone = new ASVar.List(vfwd.size());
	    if (nvals == null) nvals = new NamedVal.List();
	    for (int i=0; i<vfwd.size(); i++) {
	        ASVar v = vfwd.asvar(i);
		String v0name = "vars." + v.name() + "__init";
		Control c = (Control) vars.nestedChild(v0name);
		double val = forwardVal(v, nvals);
		if (c == null || Double.isNaN(val)) continue;
		c.setVal(Util.pretty(val, false));
		vdone.add(v);
	    }
	    return vdone;
	}

	// forwardable value
	private double forwardVal(ASVar v, NamedVal.List nvals)
	throws Xcept {
	    ASVar t = v.domain(0);
	    NamedVal nval = nvals.nval(t.name());
	    if (nval == null) return v.finalRealVal();
	    Data data = rt().getData(0, v);
	    GridData grid = data.grid(0);
	    double tval = nval.realVal();
	    if (Double.isNaN(tval)) throw new Xcept(
	    	"Illegal forward IC domain value");
	    return data.realVal(new double[] { tval });
	}	    

	// set parsModified in audiable way
	public void setParsModified(boolean b) {
//	    if (b != parsModified) {
//	    	System.err.println("Setting parsModified=" + b);
//		Thread.currentThread().dumpStack();
//	    }
	    parsModified = b;
	}

	// is this a control to empty-determining ParSet content
	public boolean isEmptyControl(PNamed c) {
	    if (c instanceof PNamedControl) return true;
	    if (c instanceof ModelParControl) return true;
	    if (c instanceof DataControl) return true;
	    return false;
	}
	
	// make job info for a single run
	public NamedVal.NList makeJobInfo() throws Xcept {
	    NamedVal.NList nvals = new NamedVal.NList();
	    vars.addJobInfo(nvals);
	    return nvals;
	}
}	
