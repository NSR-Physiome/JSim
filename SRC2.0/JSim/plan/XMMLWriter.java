/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// XMML output (model, tools, sequence)

package JSim.plan;

import org.w3c.dom.*;
import java.util.*;
import java.io.*;
import java.net.URL;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.mml.*;
import JSim.mathml.*;

public class XMMLWriter {
	private Plan plan; 
	private boolean debug; // include debug messages?
	private Document doc;
	private UnitTypes unitTypes;
	private Hashtable<Integer,Integer> hashIDs;

	public static final String VERSION = "2.01";

	// VarUsage status names
	public static final String[] statNames = new String[] {
	    "QUERY", "CURR", "MIN", "MAX", "DELAY", "XEXPR", "EXPR"
	};

	// constructor
	public XMMLWriter(Plan plan) throws Xcept {
	    this.plan = plan;
 	    debug = true;
	    hashIDs = new Hashtable<Integer,Integer>();
	}

	// set properties
	public void setDebug(boolean b) { debug = b; }

	// query
	public Document getDocument() throws Xcept { 
	    if (doc == null) 
	    	build();
	    return doc; 
	}

	// write
	public void write(PrintStream out) throws Xcept {
	    XMLWriter wrt = new XMLWriter();
	    wrt.write(getDocument(), out);
	}

	// write String
	public String writeString() throws Xcept {
	    XMLWriter wrt = new XMLWriter();
	    return wrt.writeString(getDocument());
	}
	
	// build doc
	private void build() throws Xcept {
	    doc = UtilXML.createDoc("xmml");
	    Element root = doc.getDocumentElement();
	    root.setAttribute("xmmlVersion", VERSION);
	    root.setAttribute("jsimVersion", Util.version());
	    root.setAttribute("xmlns:xmml", 
	    	"http://www.physiome.org/jsim/standard/xmml/" + VERSION);
	    // load unit types
	    UnitNList units = plan.model().units();
	    unitTypes = new UnitTypes(units);
	    URL url = getClass().getResource("unittypes.txt");
	    if (url == null) throw new Xcept(
	    	"utittypes.txt not found");
	    String txt = UtilIO.readText(url);
	    if (units.size() > 7) // ignore empty units
	    	unitTypes.loadTypes(txt);

	    addModel(root);
	    addTools(root);
	    addSequence(root);
	}

	//// MODEL section

	// add model
	private void addModel(Element base) throws Xcept {
	    base = add(base, "model");
	    if (model() == null) return;
	    addUnits(base);
	    addVars(base);
	    addEqns(base);
	    addRelations(base);
	    addEvents(base);
	    addProcCalls(base);
	}

	// add units
	private void addUnits(Element base) throws Xcept {
	    base = add(base, "unitList");
	    StringList funds = units().fund;
	    for (int i=0; i<funds.size(); i++) {
	    	Element ue = add(base, "fundamentalUnit");
		setID(ue, funds.str(i));
		setUnitType(ue, units().byName(funds.str(i)));
	    }
	    
	    Hashtable<String, Unit> dunits = 
	    	new Hashtable<String, Unit>();
	    for (int i=0; i<math().nVar(); i++) {
	    	Unit u = math().var(i).unit();
		if (u == null) continue;
		String name = u.name();
		if (funds.containSame(name)) continue;
		dunits.put(name, u);
	    }
	    Iterator<Unit> cunits = model().constUnits.iterator();
	    while (cunits.hasNext()) {
	    	Unit u = cunits.next();
		String name = u.name();
		if (funds.containSame(name)) continue;
		dunits.put(name, u);
	    }

	    ArrayList<Unit> darr = new
	    	ArrayList<Unit>(dunits.values());
	    for (int i=0; i<darr.size(); i++) {
	        Unit u = darr.get(i);
	    	Element ue = add(base, "derivedUnit");
		setID(ue, u);
		setUnitType(ue, u);
		if (u.f != 1) {
		    Element e = add(ue, "realFactor");
		    e.setAttribute("multiplier", Util.pretty(u.f));
		}
		for (int j=0; j<u.dim.length; j++) {
		    if (u.dim[j] == 0) continue;
		    Element e = add(ue, "unitFactor");
		    setID(e, "unitID", funds.str(j));
		    e.setAttribute("exponent", Util.pretty(u.dim[j]));
		}
	    }
	}
	
	// add variables
	private void addVars(Element base) throws Xcept {
	    Element list = add(base, "variableList");
	    for (int i=0; i<math().nVar(); i++) {
	    	Var v = math().var(i);
		Element ve = add(list, "variable");
		setID(ve, v);
		String dataType = "real";
		if (v.isInt()) dataType = "int";
		if (v instanceof ChoiceNVar) dataType = "choice";
		set(ve, "dataType", dataType);
		set(ve, "isDomain", v.isDomain());
		set(ve, "isExtern", v.isExtern());
		set(ve, "isPrivate", v.isPrivate());
		set(ve, "isState", v.isState());
		Unit u = v.unit();
		if (u != null) 
		    setID(ve, "unitID", u);
		if (v.ndim() > 0) {
		    Element xse = add(ve, "domainList");
		    for (int j=0; j<v.ndim(); j++) {
		    	Element xe = add(xse, "domain");
		    	setID(xe, "domainID", v.domain(j));
		    }
		}
		if (v instanceof ChoiceNVar) 
		    addChoiceLabels(ve, (ChoiceNVar) v);		
		addVarProps(ve, v);
	    }
	}

	// add choice labels
	private void addChoiceLabels(Element ve, ChoiceNVar v) {
	    Element cle = add(ve, "choiceList");
	    int ct = Math.min(v.labels().size(), v.values().size());
	    for (int i=0; i<ct; i++) {
	        Element ce = add(cle, "choice");
		ce.setAttribute("label", v.labels().str(i));
		ce.setAttribute("value", v.values().str(i));
	    }
	}

	// add properties to variable element
	private void addVarProps(Element base, Var v) throws Xcept {
	    Element pse = null;
	    for (int j=0; j<v.props.size(); j++) {
		CompProp prop = v.props.prop(j);
		String value = prop.stringVal(null);
		if (Util.isBlank(value)) continue;
		if (pse == null)
		    pse = add(base, "propertyValueList");
		Element pe = add(pse, "propertyValue");
		pe.setAttribute("name", prop.name());
		String dataType = null;
		switch (prop.dataType()) {
		case Expr.BOOLEAN: dataType = "boolean"; break;
		case Expr.REAL: dataType = "real"; break;
		case Expr.STRING: dataType = "string"; break;
		}
		if (dataType != null) 
		    pe.setAttribute("dataType", dataType);
		addText(pe, value);
	    }
	}

	// add equations
	private void addEqns(Element base) throws Xcept {
	    base = add(base, "equationList");
	    for (int i=0; i<model().eqns.size(); i++) {
	    	TEqn eqn = model().eqns.get(i);
		Element e = add(base, "equation");
		setID(e, eqn);
		add(e, "expression", eqn.expr());
	    }
	}

	// add relations
	private void addRelations(Element base) throws Xcept {
	    base = add(base, "relationList");
	    for (int i=0; i<model().relations.size(); i++) {
	    	TRelation r = model().relations.get(i);
		Element e = add(base, "relation");
		setID(e, r);
		add(e, "expression", r.expr());
	    }
	}

	// add events
	private void addEvents(Element base) throws Xcept {
	    base = add(base, "eventList");
	    for (int i=0; i<model().events.size(); i++) {
	    	TEvent event = model().events.get(i);
		Element e = add(base, "event");
		setID(e, event);
		setID(e, "timeDomainID", event.t);
		Element trig = add(e, "trigger", event.trigger);
		set(trig, "type", "absolute");
		Element eacts = add(e, "actionList");
		for (int j=0; j<event.actions.size(); j++) {
		    TEvent.TAction act = event.actions.get(j);
		    Element eact = add(eacts, "action");
		    setID(eact, "variableID", act.v);
		    add(eact, "expression", act.vexpr);
		}
	    }
	}

	// add proc calls
	private void addProcCalls(Element base) throws Xcept {
	    base = add(base, "procedureCallList");
	    for (int i=0; i<model().procs.size(); i++) {
	    	XFuncCall xfc = model().procs.get(i);
		Element efc = add(base, "procedureCall");
		set(efc, "procedureID", xfc.func().name());
		for (int j=0; j<xfc.args().size(); j++) {
		    XFuncArg arg = xfc.args().arg(j);
		    Element earg = add(efc, "procedureCallArgument");
		    add(earg, "expression", arg.base());
		    Element edoms = add(earg, "loopDomainList");
		    for (int k=0; k<arg.argDoms().size(); k++) {
		    	Element edom = add(edoms, "loopDomain");
			edom.setAttribute("domainID",
			    arg.argDoms().get(k).toString());
		    }
		}
	    }
	}

	//// TOOLS section

	// add tools
	private void addTools(Element base) throws Xcept {
	    base = add(base, "toolList");
	    if (box() == null) return;
	    Iterator<Tool> tools = box().seqTools().iterator();
	    while (tools.hasNext()) {
	    	Tool tool = tools.next();
		Element e = add(base, tool.toolType() + "Tool");
		setID(e, tool);
		add(e, "solvedVariableList", tool.vsols);
		add(e, "requiredVariableList", tool.vreqs);
		add(e, "sequenceLoopList", tool.seqLoops());
		if (tool.t() != null)
		    setID(e, "timeDomainID", tool.t());
		if (tool.icTools != null) {
		    Element eics = add(e, "initialConditionList");
		    Iterator<Tool> ictools = tool.icTools.iterator();
		    while (ictools.hasNext()) {
		    	Element eic = add(eics, "initialCondition");
			setID(eic, "toolID", ictools.next());
		    }
		}
		if (tool instanceof DETool) 
		    update(e, (DETool) tool);
		else if (tool instanceof ExprTool) 
		    update(e, (ExprTool) tool);
		else if (tool instanceof ImplicitTool) 
		    update(e, (ImplicitTool) tool);
		else if (tool instanceof ReuseTool) 
		    update(e, (ReuseTool) tool);
	    }
	}

	// DETool update
	private void update(Element base, DETool tool) 
	throws Xcept {
	    set(base, "timeOrder", "" + tool.torder);
	    addCon(base, tool.state(), model().entire);
	    if (! tool.isPDE()) return;
	    Element exs = add(base, "spatialDomainList");
	    Element ebcs = add(base, "boundaryConditionList");
	    Iterator<Domain> xs = tool.xs.iterator();
	    while (xs.hasNext()) {
	    	Domain x = xs.next();
		Element ex = add(exs, "spatialDomain");
		ex.setAttribute("domainID", x.toString());
		TSubDom sd = model().lhbcs.get(x);
	    	addCon(ebcs, tool.bc(sd), sd);
		sd = model().rhbcs.get(x);
	    	addCon(ebcs, tool.bc(sd), sd);
	    }
	}

	// add DECon for TSubDom to ODE/PDE
	private void addCon(Element base, DECon con,
	TSubDom subdom) throws Xcept {
	    Element e = null;
	    if (subdom.isEntire()) {
	    	e = add(base, "stateEquation");
	    } else {
	    	e = add(base, "boundaryCondition");
		set(e, "domainID", subdom.domain());
		set(e, "side", subdom.isLH() ? "left" : "right");
	    }
	    if (con == null) return;
	    if (con.isTool()) 
	    	setID(e, "toolID", con.tool());
	    else
	    	setID(e, "equationID", con.eqn());
	} 

	// ExprTool update
	private void update(Element base, ExprTool tool) 
	throws Xcept {
	    add(base, "expression", tool.expr);
	}

	// ImplicitTool update
	private void update(Element base, ImplicitTool tool) 
	throws Xcept {
	    Element ze = add(base, "zeroExpressionList");
	    for (int i=0; i<tool.exprs.size(); i++) 
	    	add(ze, "expression", tool.exprs.get(i).zeroExpr());
	    Element bes = add(base, "boundList");
	    ArrayList<ImplicitBound> bounds = tool.getBounds();
	    if (bounds != null) {
	    	for (int i=0; i<bounds.size(); i++) {
		    Element be = add(bes, "bound");
		    ImplicitBound bound = bounds.get(i);
		    String stype = "unknown";
		    switch (bound.type) {
		    case ImplicitBound.MIN: stype = "MIN"; break;
		    case ImplicitBound.APPROX: stype = "APPROX"; break;
		    case ImplicitBound.MAX: stype = "MAX"; break;
		    }
		    be.setAttribute("type", stype);
		    add(be, bound.vu);
		    add(be, "expr", bound.expr());
		}
	    }    
	    if (! tool.isLinear()) return;
	    Element me = add(base, "linearFactorList");
	    int n = tool.vsols.size();
	    for (int i=0; i<n; i++) {
	    	for (int j=0; j<=n; j++) {
		    Element ce = add(me, "linearFactor",
		    	tool.linmat(i, j));
		    ce.setAttribute("row", "" + i);
		    ce.setAttribute("column", "" + j);
		}
	    }
	}

	// ReuseTool update
	private void update(Element base, ReuseTool tool) 
	throws Xcept {
	    // nothing yet
	}

	//// SEQUENCE section

	// add sequence
	private void addSequence(Element base) throws Xcept {
	    base = add(base, "sequence");
	    addVarReqDoms(base);
	    SeqBlock main = plan.main();
	    if (main != null)
	        addSeqBlock(base, "main", main);
	}

	// add var Req doms
	private void addVarReqDoms(Element base) throws Xcept {
	    Hashtable<Var, DomainSet> varReqDoms = 
	        plan.main().varReqDoms();
	    if (varReqDoms == null) return;
	    Iterator<Var> viter = varReqDoms.keySet().iterator();
	    ArrayList<String> vxs = new ArrayList<String>();
	    while (viter.hasNext()) {
	    	Var v = viter.next();
		DomainSet sdoms = varReqDoms.get(v);
		Iterator<Domain> xiter = sdoms.iterator();
		while (xiter.hasNext()) {
		    Domain x = xiter.next();
		    vxs.add(id(v) + "~" + id(x));
		}
	    }
	    Collections.sort(vxs);
	    for (int i=0; i<vxs.size(); i++) {
	    	String vx = vxs.get(i);
		int inx = vx.indexOf('~');
		String vx0 = vx.substring(0, inx);
		String vx1 = vx.substring(inx+1);
		Element e = add(base, "RequiredDomain");
		e.setAttribute("variableID", vx0);
		e.setAttribute("domainID", vx1);
	    }
	}

	// add SeqBlock (common)
	private Element addSeqBlock(Element base, String name, SeqBlock block)
	throws Xcept {
	    Element eblock = add(base, name);
	    if (! (block instanceof MuBlock))
	    	for (int i=0; i<block.items().size(); i++)
	    	    addSeqItem(eblock, block.items().get(i));
	    return eblock;
	} 

	// add SeqItem
	private void addSeqItem(Element base, SeqItem item) throws Xcept {
	    if (item instanceof SeqBlock.Loop) {
	    	SeqBlock.Loop loop = (SeqBlock.Loop) item;
		Element eblock = addSeqBlock(base, "loopBlock", loop);
		eblock.setAttribute("loopDomainID", "" + loop.x());

	    } else if (item instanceof ODEBlock) {
	    	ODEBlock oblock = (ODEBlock) item;
		Element eblock = addSeqBlock(base, "ODEBlock", oblock);
		eblock.setAttribute("timeDomainID", "" + oblock.t());
		addStateTools(eblock, oblock);
		addMicroSequence(eblock, oblock);

	    } else if (item instanceof PDEBlock) {
	    	addPDEBlock(base, (PDEBlock) item);
	
	    } else if (item instanceof VarMem) {
	    	addVarMem(base, (VarMem) item);
	
	    } else {
	    	Element ei = add(base, "item");
		ei.setAttribute("itemID", id(item));
		addText(ei, item.toString());
	    }
	}

	// add PDEBlock
	private void addPDEBlock(Element base, PDEBlock block)
	throws Xcept {
	    Element eblock = addSeqBlock(base, "PDEBlock", block);
	    eblock.setAttribute("timeDomainID", "" + block.t());
	    Element exs = add(eblock, "spatialDomainList");
	    Iterator<Domain> xs = block.xs().iterator();
	    while (xs.hasNext()) {
		Element ex = add(exs, "spatialDomain");
		ex.setAttribute("domainID", xs.next().toString());
	    }
	    addStateTools(eblock, block);
	    addMicroSequence(eblock, block);
	    Element ebdys = add(eblock, "PDEBlockBoundaryList");
	    Iterator<BCBlock> bcblocks = block.bcblocks.values().iterator();
	    while (bcblocks.hasNext()) 
	    	addPDEBlockBoundary(ebdys, bcblocks.next());
	}	    

	// add PDEBlockBoundary
	private void addPDEBlockBoundary(Element base, BCBlock block)
	throws Xcept {
	    Element ebdy = add(base, "PDEBlockBoundary");
	    setSubDom(ebdy, block.subdom());
	    addMicroSequence(ebdy, block);
	}	    

	// add state tools
	private void addStateTools(Element base, MuBlock block) 
	throws Xcept {
	    Element etools = add(base, "stateToolList");
	    for (int i=0; i<block.detools.size(); i++) {
	    	Element etool = add(etools, "stateTool");
		DETool detool = block.detools.get(i);
	 	etool.setAttribute("toolID", id(detool));
		addText(etool, detool.toString());
	    }
	}

	// add microSequence
	private void addMicroSequence(Element base, MuBlock block) 
	throws Xcept {
	    Element emu = add(base, "microSequence");
	    for (int i=0; i<block.items().size(); i++)
	    	addSeqItem(emu, block.items().get(i));	    	    
	}

	// add VarMem to main sequence
	private void addVarMem(Element base, VarMem vf) 
	throws Xcept {
	    Element evs = add(base, vf.isFree() ? "free" : "alloc");
	    for (int i=0; i<vf.vars().size(); i++) {
		Var v = vf.vars().get(i);
	    	Element ev = add(evs, "var");
		ev.setAttribute("variableID", id(v));
	    }
	}

	//// UTILITIES section

	// add plain element
	private Element add(Element base, String name) {
	    Element e = doc.createElement(name);
	    base.appendChild(e);
	    return e;
	}

	// add text node
	private void addText(Element base, String name, String txt) {
	    Element e = add(base, name);
	    addText(e, txt);
	}
	private void addText(Element base, String txt) {
	    Node ptxt = doc.createTextNode(txt);
	    base.appendChild(ptxt);
	}

	// add Variable element
	private void add(Element base, String name, Var v) 
	throws Xcept {
	    Element e = add(base, name);
	    setID(e, v);
	}

	// add VarUsage[s] element
	private void add(Element base, String name, VarUsages vus)
	throws Xcept {
	    Element e = add(base, name);
	    for (int i=0; i<vus.size(); i++) 
		add(e, vus.get(i));
	}
	private Element add(Element base, VarUsage vu)
	throws Xcept {
	    Element e = add(base, "variableUsage");
	    setID(e, vu);
	    Var v = vu.v();
	    setID(e, "variableID", v);
	    int stat = vu.stat();
	    set(e, "status", statNames[stat]);
	    for (int i=0; i<v.ndim(); i++) {
	    	Domain x = v.domain(i);
		int qstat = vu.qstat(x);
		if (qstat == VarUsage.CURR) continue;
	        Element xe = add(e, "domainUsage");
		setID(xe, "domainID", x);
		set(xe, "domainStatus", statNames[qstat]);
	    }
	    return e;
	}

	// add sequence loops
	private void add(Element base, String name, DomainSet doms)
	throws Xcept {
	    if (doms == null) return;
	    Element e = add(base, name);
	    Iterator<Domain> iter = doms.iterator();
	    while (iter.hasNext()) {
	    	Element xe = add(e, "sequenceLoop");
		setID(xe, "domainID", iter.next());
	    }
	}

	// add expr element (MathML)
	private Element add(Element base, String name, TExpr expr)
	throws Xcept {
	    Element e = add(base, name);
	    if (debug) addText(e, "debug", expr.toString());
	    TSubDom subdom = expr.subdom();
	    if (! subdom.isEntire()) {
	    	Element esd = add(e, "subdomain");
		setSubDom(esd, subdom);
	    }
	    MLWriter math = new MLWriter(e);
	    math.add(expr.expr());
	    return e;
	}
	private Element add(Element base, String name, Expr expr)
	throws Xcept {
	    Element e = add(base, name);
	    if (debug) addText(e, "debug", expr.toString());
	    MLWriter math = new MLWriter(e);
	    math.add(expr);
	    return e;
	}

	// add subdomain
	private void setSubDom(Element e, TSubDom subdom) throws Xcept {
	    if (! subdom.isEntire()) {
	    	setID(e, "domainID", subdom.domain());
	    	set(e, "side", 
	    	    subdom.isLH() ? "left" : "right");
	    }
	}

	// set attribute
	private void set(Element e, String name, boolean b) {
	    e.setAttribute(name, "" + b);
	}
	private void set(Element e, String name, String s) {
	    e.setAttribute(name, "" + s);
	}
	private void set(Element e, String name, Var v) {
	    set(e, name, v.toString());
	}
	

	// id for object
	private String id(Object o) throws Xcept {
	    if (o == null) return "null";
	    if (o instanceof Unit)
	    	return ((Unit) o).pubName();
	    if (o instanceof String)
	    	return o.toString();
	    if (o instanceof Var)
	    	return o.toString();
	    if (o instanceof VarUsage)
	    	return o.toString();
	    if (o instanceof TRelation)
	    	return "relation_" + hashID(o);
	    if (o instanceof TEqn)
	    	return "eqn_" + hashID(o);
	    if (o instanceof TEvent)
	    	return "event_" + hashID(o);
	    if (o instanceof XFuncCall)
	    	return "proc_" + hashID(o);
	    if (o instanceof Tool)
	    	return "tool_" + hashID(o);
	    throw new Xcept("No id defined for class " + 
	    	((o == null) ? "null" : o.getClass().getName()));
	} 

	// consistent ID based on object hashCode
	private int hashID(Object o) {
	    Integer ihash = new Integer(o.hashCode());
	    Integer id = hashIDs.get(ihash);
	    if (id == null) {
	    	id = new Integer(hashIDs.size()+1);
		hashIDs.put(ihash, id);
	    }
	    return id.intValue();
	}

	// set id into Element
	private void setID(Element e, Object o) throws Xcept {
	    setID(e, "id", id(o));
	}
	private void setID(Element e, String name, Object o) throws Xcept {
	    e.setAttribute(name, id(o));
	}
	
	// set unitType
	private void setUnitType(Element e, Unit u) throws Xcept {
	    String type = unitTypes.getType(u);
	    e.setAttribute("unitType", type);
	}

	//// QUERY section
	private TModel model() { return plan.model(); }
	private UnitNList units() { return model().units(); }
	private MathSys math() { return model().math; }
	private ToolBox box() { return plan.box(); }

}
