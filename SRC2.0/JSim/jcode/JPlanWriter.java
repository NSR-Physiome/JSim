// Java output for plan2 compilation

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 

import java.io.*;
import java.util.*;

public class JPlanWriter extends JWriter {
	protected Plan plan;
	protected State state;  // write state
	protected LinkedHashSet<Var> rtvars; // vars for runtime
	protected Hashtable<SeqItem,Integer> loopPhases;
	protected Hashtable<Var,Integer> varPhases;
	protected LinkedHashSet<JCacheVar> cacheVars;
	protected JMainWriter mainWriter;
	protected JFPWriter fpWriter;
	protected ArrayList<JMethodWriter> subWriters;
	protected StringList vtraces; // vars to trace
	
	// constructor
	public JPlanWriter(String className, Plan plan) throws Xcept {
	    super();
	    this.plan = plan;
	    state = new State(className);
	    if (mainBlock() == null) throw new Xcept(
	    	"Can't write java code - no sequencing info");

	    // assign phases to mainphase loops & vars
	    loopPhases = new Hashtable<SeqItem,Integer>();
	    varPhases = new Hashtable<Var,Integer>();
	    ArrayList<SeqItem> items = mainBlock().items();
	    Integer phase = new Integer(2);
	    for (int i=0; i<items.size(); i++) {
	    	SeqItem item = items.get(i);
		if (item instanceof SeqBlock.Loop) {
		    phase = new Integer(phase.intValue()+1);
		    loopPhases.put(item, phase);
		    setPhase(item, phase);
		    phase = new Integer(phase.intValue()+1);
		} else {
		    setPhase(item, phase);
		}
	    }

	    // load main cache into cacheVars
	    // set phase=1 for inputs
	    rtvars = new LinkedHashSet<Var>();
	    cacheVars = new LinkedHashSet<JCacheVar>();
	    phase = new Integer(1);
	    for (int i=0; i<math().nVar(); i++) {
	    	Var v = math().var(i);
		Tool tool = mainTool(v);
		if (tool != null) rtvars.add(v);
		if (! box().isInput(tool)) continue;
		varPhases.put(v, phase);
		if (v.ndim() == 0)
		     cacheVars.add(new JCacheVar(v, 0));
	    }

	    // create writers
	    mainWriter = new JMainWriter(this, mainBlock());
	    fpWriter = new JFPWriter(this);
	    subWriters = new ArrayList<JMethodWriter>();
	    addMethodWriters(mainWriter);

	    // update cacheVars from subWriters
	    for (int i=0; i<subWriters.size(); i++) {
		JMethodWriter p = subWriters.get(i);
		p.addCacheVars(cacheVars);	
	    }
	}

	// set phases for all vsol in an item
	private void setPhase(SeqItem item, Integer phase) 
	throws Xcept {
	    if (item instanceof QueryTool) {
	    	return;
	    } else if (item instanceof MuBlock) {
	    	ArrayList<Var> vs = ((MuBlock) item).vstate();
		for (int i=0; i<vs.size(); i++) 
		    varPhases.put(vs.get(i), phase);
	    } else if (item instanceof Tool) {
	    	VarUsages vus = ((Tool) item).vsols;
		Iterator<Var> vs = vus.vset().iterator();
		while (vs.hasNext()) 
		    varPhases.put(vs.next(), phase);
	    } else if (item instanceof SeqBlock.Loop) {
	    	ArrayList<SeqItem> subitems = 
		    ((SeqBlock.Loop) item).items();
		for (int i=0; i<subitems.size(); i++) 
		    setPhase(subitems.get(i), phase);
	    }
	}

	// set source libraries to match jsbatch
	public void setSourceLibs() throws Xcept {
	    fpWriter.setSourceLibs();
	}

	// recursively add to subWriters
	private void addMethodWriters(JMethodWriter wrt) {
	    for (int i=0; i<wrt.subWriters.size(); i++) {
	    	JMethodWriter wsub = wrt.subWriters.get(i);
		subWriters.add(wsub);
		addMethodWriters(wsub);
	    }
	}

	// write Java
	public void writePlan() throws Xcept {
	    writeHdr();
	    indentIn();
	    writeVarDecl();
	    fpWriter.writeDecl();
	    writeSolverDecl();
	    writeConstrHdr();
	    writeUnits();
	    writeVarInsts();
	    writeInputDefaults();
	    fpWriter.writeInit();
	    writeSolverInst();
	    writeConstrFooter();
	    writeMainMethods();
	    fpWriter.writeJavaClasses();
	    writeSolverClasses();
	    writeCtxt();
	    indentOut();
	    println("}");
	    println("");
	    fpWriter.writeNativeFuncClasses();
	    fpWriter.writeExternJava();
	}
	
	// write class header
	private void writeHdr() throws Xcept {
	    println("// JSim generated model " + className());
	    println(S);
	    println("import java.lang.Math;");
	    println("import JSim.util.*;");
	    println("import JSim.expr.*;");
	    println("import JSim.data.*;");
	    println("import JSim.jruntime.*;");
	    println("import JSim.aserver.*;");
	    println("import JSim.nml.math.NatMath;");
	    println(S);
	    println("// " + className() + " model");
	    println("public class " + className() +
		" extends RTModel {");
	    println("");
	}

	// write var declarations
	private void writeVarDecl() throws Xcept {
	    println("// variable declarations");
	    for (int i=0; i<math().nVar(); i++) {
		Var v = math().var(i);
		if (! rtvars.contains(v)) continue;
		println("public " + rtClass(v) + S +
		    vstruct(v) + ";");
	    }
	    println(S);
	}

	// write Solver declarations
	private void writeSolverDecl() throws Xcept {
	    println("// solver declarations");
	    for (int i=0; i<subWriters.size(); i++) 
	    	if (subWriters.get(i) instanceof JProblemWriter)
	    	    ((JProblemWriter) subWriters.get(i)).writeDecl();
	    println(S);
	}

	// write constructor header
	private void writeConstrHdr() throws Xcept {
	    println("// RT model constructor");
	    startMethod(className(), "u,m", "UnitNList,ASServer.Messenger"); 
	    println("super(" + Q + className() + Q + ", u, m);");
	    println(S);
	}

	// write units
	private void writeUnits() throws Xcept {
 	    println("// units used in model");
	    UnitNList units = mmlModel().units;
	    for (int i=0; i<units.fund.size(); i++)
		println("units.addFund(\"" +
		    units.fund.str(i) + "\");");
	    for (int i=0; i<units.size(); i++) {
		Unit u = units.unit(i);
		println("units.add(\"" + u.name() +
		    "\", Unit.parse(this, \"" +
		u.fundStr(units) + "\"));");
		if (units.isPrefixable(u.name()))
		    println("units.setPrefixable(\"" + u.name() + "\");");
	    }
	    for (int i=0; i<units.prefixes.size(); i++) {
	    	UnitNList.Prefix pfx = units.prefixes.prefix(i);
		println("units.addPrefix(\"" + pfx.name() +
		    "\", " + Util.pretty(pfx.mult()) + ");");
	    }
	    println(S);
	}

	// write var instantiations
	private void writeVarInsts() throws Xcept {
	    println("// var instantiation");
	    for (int i=0; i<math().nVar(); i++) {
	    	Var v = math().var(i);
		if (! rtvars.contains(v)) continue;
		if (v.ndim() == 0) writeVarInst(v);
	    }
	    for (int i=0; i<math().nVar(); i++) {
	    	Var v = math().var(i);
		if (! rtvars.contains(v)) continue;
		if (v.isDomain()) writeVarInst(v);
	    }
	    for (int i=0; i<math().nVar(); i++) {
	    	Var v = math().var(i);
		if (! rtvars.contains(v)) continue;
		if (! v.isDomain() && v.ndim() > 0) writeVarInst(v);
	    }
	    println(S);
	}

	// write 1 var instantiation
	private void writeVarInst(Var v) throws Xcept {

	    // units parm
	    String sunit = Unit.same(v.unit(), Unit.scalar()) ? 
	        Unit.dimless : v.unit().name();
	    sunit = Q + sunit + Q;
		
	    // phase
	    int sphase = phase(v);
	    
	    // domain parm
	    String sdom;
	    if (v.isDomain()) {
		Domain x = (Domain) v;
		sdom = vstruct(x.vmin) + C +
		    vstruct(x.vmax) + C +
		    vstruct(x.vdelta) + C +
		    vstruct(x.vct);
	    } else if (v.ndim() == 0) {
		sdom = "null";
	    } else {
		sdom = "new RTRealDomain[] {";
		for (int j=0; j<v.ndim(); j++) {
		    Domain x = v.domain(j);
		    if (j>0) sdom = sdom + C;
		    sdom = sdom + vstruct(x);
		}
		sdom = sdom + "}";
	    }

	    // labels parm
	    String slabels = "";
	    if (v instanceof ChoiceNVar) {
		ChoiceNVar cv = (ChoiceNVar) v;
		slabels = C + 
		    " new String[] { " + cv.labels().toString(C, true) + "}" 
		    + C + 
		    " new String[] {" + cv.values().toString(C, true) + "}";
	    }

	    // print instantiation line
	    println(vstruct(v) +
		" = new " + rtClass(v) +
		"(this, " + Q + v + Q + C + sunit + C +
		sphase + C + sdom + slabels + ");");
		
	    // tag private var?
	    if (v.isPrivate())
		println(vstruct(v) + ".setPrivate();");

	    // store doms
	    DomainSet rdoms = varReqDoms().get(v);
	    if (!v.isDomain() && rdoms != null && rdoms.size()>0) {
	        Iterator<Domain> xiter = rdoms.iterator();
	    	while (xiter.hasNext()) {
		    Domain x = xiter.next();
		    println(vstruct(v) + 
		    	".addReqDom(" + vstruct(x) + ");");
		}
	    }
	}

	// write input defaults
	private void writeInputDefaults() throws Xcept {
	    println("// input defaults");
	    for (int i=0; i<math().nVar(); i++) {
	    	Var v = math().var(i);
		if (! (mainTool(v) instanceof ExprTool)) continue;
		if (! isInput(v)) continue;
		ExprTool tool = (ExprTool) mainTool(v);
		println(vstruct(v) + ".setDefault(" + Q
		    + tool.expr.expr() + Q + ");");
	    }
	    println("");
	}

	// write solver instantiation
	private void writeSolverInst() throws Xcept {
	    println("// solver instantiation");
	    for (int i=0; i<subWriters.size(); i++) 
	    	if (subWriters.get(i) instanceof JProblemWriter)
	    	    ((JProblemWriter) subWriters.get(i)).writeInst();
	    println("");
	}

	// write constructor footer
	private void writeConstrFooter() throws Xcept {
	    stopMethod();
	    println("");
	}

	// write main methods
	private void writeMainMethods() throws Xcept {
	    boolean unitCorr = mmlModel().unitControl == Model.ON;
	    boolean allowMP = true;

	    println("// simple main methods");
	    println("public boolean defaultUnitCorrect() "
		+ "{ return " + unitCorr + "; }");
	    println("public boolean allowMPRuns() " 
	        + "{ return " + allowMP + "; }");
	    println("public void mainline(RTContext ctxt) throws Xcept "
	        + "{ ((XContext) ctxt).mainline(); }");
	    println("public RTContext newRunContext(int threadInx, int nproc, RTDataStore s) "
	        + "{ return new XContext(this, threadInx, nproc, s); }");
	    
	    println("");
	}

	// write solver classes
	private void writeSolverClasses() throws Xcept {
	    for (int i=0; i<subWriters.size(); i++) 
	    	if (subWriters.get(i) instanceof JProblemWriter)
	    	    ((JProblemWriter) subWriters.get(i)).writeClass();
	}

	// write XContext
	private void writeCtxt() throws Xcept {
	    println("// model-specific context");
	    println("public class XContext extends RTContext {");
	    indentIn();
	    
	    writeGlobalCache();

	    // ctxt constructor
	    println("// context constructor");
	    println("public XContext(RTModel m, int threadInx, int nproc, RTDataStore s) {"
	        + " super(m, threadInx, nproc, s, false); }");
	    println("");
	   
	    // abortNaN
	    println("// trace NaN causes abort?");
	    println("public boolean abortNaN() { return " + 
	    	plan.abortNaN() + "; }");
	    println("");

	    // ctxt mainline
	    mainWriter.writeMethods();

	    // ctxt methods for all method writers
	    for (int i=0; i<subWriters.size(); i++) 
	    	subWriters.get(i).writeMethods();

	    // write ctxt F&P methods
	    fpWriter.writeMethods();

	    // done XContext sub-class
	    indentOut();
	    println("}");
	    println(S);
	}
	    

	// write var cache declarations
	private void writeGlobalCache() throws Xcept {
	    println("// global caches");
	    Iterator<JCacheVar> cvs = cacheVars.iterator();
	    while (cvs.hasNext()) {
	    	JCacheVar cv = cvs.next();
		println("private double " + vcache(cv.v, cv.level) + 
		    (plan.initNaN() ? " = Double.NaN;" : ";"));
	    }
	    println("");
	}

	// init global cache to NaNs, if requested
	protected void initCaches() throws Xcept {
	    if (! plan.initNaN()) return;
	    println("// initNaN=true: init cache vars to NaNs");
	    Iterator<JCacheVar> cvs = cacheVars.iterator();
	    while (cvs.hasNext()) {
	    	JCacheVar cv = cvs.next();
		println(vcache(cv.v, cv.level) + "=Double.NaN;");
	    }
	    println("");
	}

	// trace query
	protected boolean traceVar(Var v) throws Xcept {
	    if (vtraces == null) 
	    	vtraces = new StringList(plan.traceVars(), ", ");
	    return vtraces.containSame(v.toString());
	}

	// check NaNs after each calc?
	protected boolean checkNaN() throws Xcept {
	    return plan.traceNaN() || plan.abortNaN();
	}

	// simple query methods
	public State state() { return state; }
	public TModel model() { return plan.model(); }
	public ToolBox box() { return plan.box(); }
	public Model mmlModel() { return model().mmlModel(); }
	public MathSys math() { return model().math(); }
	public MainBlock mainBlock() { return plan.main(); }
	public Hashtable<Var, DomainSet> varReqDoms() {
	    return mainBlock().varReqDoms(); 
	}

	public Hashtable<VarUsage, Tool> vuTools() { 
	    return mainBlock().vuTools(); 
	}
 	protected Tool mainTool(Var v) throws Xcept {
	    VarUsage vu = new VarUsage(model(), v);
	    return vuTools().get(vu);
	}
	protected int phase(Var v) throws Xcept {
	    return varPhases.get(v).intValue();
	}
	protected boolean isInput(Var v) throws Xcept {
	    return phase(v) == 1;
	}

	// RT class for variable
	public String rtClass(Var v) {
	    String s1 = v.isInt() ? "Int" : "Real";
	    if (v instanceof ChoiceNVar) s1 = "Choice";
	    String s2 = v.isDomain() ? "Domain" : "NVar";
	    return "RT" + s1 + s2;
	}	

	// test mainline
	public static void main(String[] args) throws Exception {
	    if (args.length != 1) throw new Exception(
	    	"Usage: JPlanWriter mod-file");
	    File modFile = new File(args[0]);
	    Model mmlModel = new ModelReader(modFile);
	    mmlModel.flatten("testwriter.junk");
	    Plan plan = new Plan(mmlModel);
	    plan.process();
	    if (plan.logger.errors.size() > 0) throw new Exception(
	    	"Compilation error(s): " + plan.logger.errors);
	    JPlanWriter wrt = new JPlanWriter("JTest", plan);
	    wrt.writePlan();
	}
	    
}

	
