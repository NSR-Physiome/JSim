// write java code for a method

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*;

import java.io.*;
import java.util.*;

abstract public class JMethodWriter extends JExprWriter {
	protected String id;
	protected SeqItem item;
	protected ArrayList<JMethodWriter> subWriters;

	// constructor
	public JMethodWriter(JMethodWriter parent, SeqItem item) 
	throws Xcept {
	    super(parent);
	    this.item = item;
	    subWriters = new ArrayList<JMethodWriter>();
	}

	// add subwriter block
	public void addSubWriterBlock(SeqBlock block) throws Xcept {
	    addSubWriterBlock(block, "");
	}
	public void addSubWriterBlock(SeqBlock block, String idSfx) 
	throws Xcept {
	    for (int i=0; i<block.size(); i++) 
		addSubWriter(block.items().get(i), idSfx);
	}

	// add subwriters recursively
	public void addSubWriter(SeqItem item, String idSfx) throws Xcept {
	    JMethodWriter wsub = makeSubWriter(item, idSfx);
	    if (wsub != null) 
	    	subWriters.add(wsub);
	    else if (item instanceof SeqBlock) 
		addSubWriterBlock((SeqBlock) item, idSfx);
	}
	
	// make sub-writer for item
	public JMethodWriter makeSubWriter(SeqItem item, String idSfx) 
	throws Xcept {
	    if (item instanceof ODEBlock)	
	    	return new JODEWriter(this, (ODEBlock) item);
	    if (item instanceof PDEBlock)	
	    	return new JPDEWriter(this, (PDEBlock) item);
	    if (item instanceof ImplicitTool) 	
	    	return new JImplicitWriter(this, 
		    (ImplicitTool) item, idSfx);
	    if (item instanceof SeqBlock.Loop)	
	    	return new JLoopWriter(this, (SeqBlock.Loop) item,
		    nloopWriters());
	    return null;
	}

	// # loop writers in this method writer (for loop id)
	private int nloopWriters() {
	    int n = 0;
	    for (int i=0; i<subWriters.size(); i++)
	    	if (subWriters.get(i) instanceof JLoopWriter)
		    n++;
	    return n;
	}

	// get problem sub-writer 
	public JMethodWriter getSubWriter(SeqItem item) {
	    for (int i=0; i<subWriters.size(); i++) {
		JMethodWriter swrt = subWriters.get(i);
	    	if (swrt.item == item) 
		    return swrt;
		if (swrt instanceof JLoopWriter) {
		    JMethodWriter s = swrt.getSubWriter(item);
		    if (s != null) return s;
		}
	    }
	    return null;
	}

	// write ctxt methods
	abstract public void writeMethods() throws Xcept;

	// write block
	public void writeBlock(SeqBlock block) throws Xcept {
	    for (int i=0; i<block.items().size(); i++) 
	    	writeItem(block.items().get(i));

	    // interrupt checks
	    //    MainBlock is silly
	    //    ODEBlock is handled elsewhere
	    if (! (block instanceof MainBlock) 
	    && ! (block instanceof ODEBlock))
	    	writeInterruptCheck();
	}

	// write an item
	public void writeItem(SeqItem item) throws Xcept {
	    if (item instanceof SeqBlock.Loop)
	    	write((SeqBlock.Loop) item);
	    else if (item instanceof ODEBlock)
	    	writeSolverCall(item);
	    else if (item instanceof PDEBlock)
	    	writeSolverCall(item);
	    else if (item instanceof ImplicitTool)
	    	write((ImplicitTool) item);
	    else if (item instanceof ImplicitBound)
	    	write((ImplicitBound) item);
	    else if (item instanceof TEvent)
	    	write((TEvent) item);
	    else if (item instanceof TRelation)
	    	write((TRelation) item);
	    else if (item instanceof DETool)
	    	write((DETool) item);
	    else if (item instanceof DomainTool)
	    	write((DomainTool) item);
	    else if (item instanceof ExprTool)
	    	write((ExprTool) item);
	    else if (item instanceof ExternTool)
	    	write((ExternTool) item);
	    else if (item instanceof ProcTool)
	    	write((ProcTool) item);
	    else if (item instanceof QueryTool)
	    	write((QueryTool) item);
	    else if (item instanceof ReuseTool)
	    	write((ReuseTool) item);
	    else if (item instanceof StateTool)
	    	write((StateTool) item);
	    else if (item instanceof VarMem)
	    	write((VarMem) item);
	    else if (item == null)
	    	throw new Xcept("JBlockWriter: null SeqItem");
	    else throw new Xcept(
	    	"Unsupported SeqItem class " + item.getClass());
	}

	// write loop
	protected void write(SeqBlock.Loop block) throws Xcept {
	    JLoopWriter wrt = (JLoopWriter) getSubWriter(block);
	    if (wrt == null) throw new Xcept(
	    	"Missing subwriter for " + block);
	    wrt.writeLoopsCall();
	}

	// write ODE/PDE solver call 
	protected void writeSolverCall(SeqItem item) throws Xcept {
	    JProblemWriter wrt = (JProblemWriter) getSubWriter(item);
	    if (wrt == null) throw new Xcept(
	    	"Missing subwriter for " + item);
	    wrt.writeSolverCall();
	}

	// write Implicit solver call
	protected void write(ImplicitTool tool) throws Xcept {

	    // write when, if needed
	    for (int i=0; i<tool.exprs().size(); i++) {
	    	TSubDom sd = tool.exprs().get(i).subdom();
		writeWhen(sd);
	    }
	    	
	    // solver call
	    JProblemWriter wrt = (JProblemWriter) getSubWriter(tool);
	    if (wrt == null) throw new Xcept(
	    	"MethodWriter: " + getClass() + 
		" missing subwriter for " + tool.getClass());
	    wrt.writeSolverCall();
	}
	

	// write DETool
	protected void write(DETool item) throws Xcept {
	    throw new Xcept(
	    	"JMethodWriter: DETools don't below here");
	}

	// write DomainTool
	protected void write(DomainTool tool) throws Xcept {
	    Domain x = tool.x();
	    Expr mexpr = x.vmin.add(x.vdelta.mult(
	    	x.vct.sub(Expr.one)));
	    writeAssign(x.vmax, mexpr);
	    println("setDomain(" + vstruct(x) + ");");
	}

	// write ExprTool
	protected void write(ExprTool tool) throws Xcept {
	    Var v = tool.v();
	    String vcache = vcache(v);
	    
	    // input dim0s: load cache from struct
	    if (planWriter().isInput(v)) {
	    	if (vcache != null) 
	    	    println(vcache + " = " + vcurr(v) + ";");
		else 
		    println("// input parm: " + tool);
		return;
	    }

	    // write when, if needed
	    TSubDom subdom = tool.expr.subdom();
	    VarUsage vu = tool.vu;
	    if (! subdom.isEntire()) 
	    	writeWhen(subdom);
	    else if (vu.isBoundary())
		writeWhen(vu.domain(), vu.isMin());
	    	
	    // write v=expr
	    writeAssign(v, tool.expr.expr());
	}

	// write when statement
	protected void writeWhen(TSubDom sd) throws Xcept {
	    if (! sd.isEntire())
	    	writeWhen(sd.domain(), sd.isLH());
	}	    
	protected void writeWhen(Domain x, boolean isLH) 
	throws Xcept {
	    String sset = isLH ? "setLHBC" : "setRHBC";
	    println(sset + "(" + vstruct(x) + ");");
	}	

	// write v=expr
	protected void writeAssign(Var v, Expr expr) throws Xcept {
	    String sexpr = fmt(expr);
	    String vcache = vcache(v);
	    if (vcache != null) 
	    	sexpr = vcache + " = " + sexpr;
	    if (! isMu)
	    	sexpr = "set(" + vstruct(v) + C + sexpr + ")";
	    println(sexpr + ";");
	    writeTrace(v);
	}
	    
	// write trace code for 1 Var
	protected void writeTrace(Var v) throws Xcept {
	    boolean dotrace = 
	    	planWriter().traceVar(v) || planWriter().checkNaN();
	    if (! dotrace) return;

	    // format trace call
	    Domain t = null;
	    if (this instanceof JODEWriter) 
	    	t = ((JODEWriter) this).block.t();
	    if (this instanceof JPDEWriter)
	    	t = ((JPDEWriter) this).block.t();
	    String muargs = "null";
	    if (t != null) 
	    	muargs = Q + t + "=" + Q + 
		    "+Util.pretty(" + vcache(t) + ")";
	    String strace = "trace(" + vstruct(v) + "," + 
	        fmt(v) + "," + muargs + ");";

	    // print var trace, maybe NaN check
	    if (planWriter().traceVar(v)) 
	    	println(strace);
	    else
	    	println("if (Double.isNaN(" + fmt(v) + ")) " + strace);
	}   

	// write ExternTool
	protected void write(ExternTool tool) throws Xcept {
	    Var v = tool.v;
	    String vcache = vcache(v);

	    // extern param cache load
	    if (! isMu) {
	    	if (vcache != null) 
	    	    println(vcache + " = " + vcurr(v) + ";");
		else 
		    println("// debug only: " + tool);
		return;
	    }

	    // muStepped extern
	    String line = vcache + "=realVal(" + vstruct(v) +
	    	", new double[] {";
	    for (int i=0; i<v.ndim(); i++) {
	    	if (i>0) line = line + ",";
		line = line + fmt(v.domain(i)); //WAS vcache(v.domain(i));
	    }
	    line = line + "});";
	    println(line);
	}

	// write ProcTool
	protected void write(ProcTool tool) throws Xcept {
	    writeWhen(tool.sdom());
	    planWriter().fpWriter.writeCall(tool.fc());
	}

	// write QueryTool
	protected void write(QueryTool item) throws Xcept {
	    // println("//debug:  " + item);
	}

	// write ReuseTool
	protected void write(ReuseTool tool) throws Xcept {
	    writeWhen(tool.x, true);
	    writeItem(tool.tool);
	}

	// write StateTool
	protected void write(StateTool tool) throws Xcept {
	    Var v = tool.v;
	    Domain t = tool.t;
	    String sexpr = "setNextRight(" + vstruct(v) +
	    	"," + vstruct(t) + ");";
	    String vcache = vcache(v);
	    if (vcache != null)
	    	sexpr = vcache + "=" + sexpr;
	    println("if (! atRHBC(" + vstruct(t) + ")) " + sexpr);
	}

	// write ImplicitBound
	protected void write(ImplicitBound bound) throws Xcept {
	    write(bound.relation());
	}

	// write TEvent
	protected void write(TEvent event) throws Xcept {
	    String strig = fmt(event.trigger.expr());
	    println("if (" + strig + ") {");
	    setMethodBreakable(false);
	    indentIn();
	    for (int i=0; i<event.actions.size(); i++) {
	    	TEvent.TAction act = event.actions.get(i);
		writeAssign(act.v, act.vexpr.expr());
	    }
	    indentOut();
	    println("}");
	    setMethodBreakable(true);
	}

	// write TRelation
	protected void write(TRelation trel) throws Xcept {
	    if (trel.op() == IExpr.APPROX) {
	    	// println("//debug only: " + trel);
		return;
	    } 
	    TExpr expr = trel.expr();
	    TSubDom subdom = expr.subdom();
	    writeWhen(subdom);
	    String sexpr = fmt(expr.expr());
	    println("if (!(" + sexpr + ")) badRelation("
	        + Q + expr.expr() + Q + ");");
	}

	// write VarMem 
	// currently disabled due to bugs (feature not yet implemented)
	protected void write(VarMem vf) throws Xcept {
/*
	    StringBuffer buf = new StringBuffer(vf.isFree() ? 
	    	"varFree(" : "varAlloc(");
	    buf.append("new RTNVar[] {");
	    for (int i=0; i<vf.size(); i++) {
	    	Var v = vf.vars().get(i);
		if (i>0) buf.append(",");
		buf.append(vstruct(v));
	    }
	    buf.append("});");
	    println(buf.toString());
*/
	}

	// write interrupt check
	protected void writeInterruptCheck() throws Xcept {
	    println("interruptCheck();");
	}
}

