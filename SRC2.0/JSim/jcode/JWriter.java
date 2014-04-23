// common code for all Java code writers

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;
import JSim.plan.*; 

import java.io.*;
import java.util.*;

abstract public class JWriter {
	protected Lang jlang = JLang.lang;

 	// static constants
 	protected static int methodLineMax = 1000;
 	protected static int methodCharMax = 32000;
	protected static final String Q = "\"";
	protected static final String C = ",";
	protected static final String S = " ";

	// constructor
	public JWriter() {
	}

	// state
	abstract public State state();

	// set file output
	public void setFile(File f) throws Xcept {
	    try {
	    	state().out = new PrintWriter(new FileWriter(f), true);
	    } catch (Exception e) {
	    	throw Xcept.wrap(e);
	    }
	}

	// Var rep
	public String vstruct(Var v) { return "JS" + vname(v); }
	public String vstruct(ArrayList<Var> vs) {
	    StringBuffer buf = new StringBuffer();
	    for (int i=0; i<vs.size(); i++) {
	    	if (i > 0) buf.append(",");
		buf.append(vstruct(vs.get(i)));
	    }
	    return buf.toString();
	}
  	public String vcache(Var v, int level) {
	    return "js" + level + vname(v);
	}
	public String vcurr(Var v) {
	    return "realVal(" + vstruct(v) + ")";
	}
	public String vname(Var v) { 
	    StringBuffer buf = new StringBuffer();
	    String sv = v.toString();
	    for (int i=0; i<sv.length(); i++) {
	    	char c = sv.charAt(i);
		switch (c) {
		case ':': buf.append("__D"); break;
		case '.': buf.append("__"); break;
		default : buf.append(c); break;
	        }
	    }
	    return buf.toString();
	}
	

	// function string
	protected String fmt(String f, String[] args) {
	    StringBuffer buf = new StringBuffer(f + "(");
	    for (int i=0; i<args.length; i++) {
	    	if (i>0) buf.append(",");
		buf.append(args[i]);
	    }
	    buf.append(")");
	    return buf.toString();
	}

	// const string
	protected String fmt(RealConst c) {
	    double d = c.realVal(null);
	    if (Double.isNaN(d)) return "Double.NaN";
	    return c.toString();
	}

	//// method/line control

	// start state().method
	protected void startMethod(String name, String args, String argTypes) 
	throws Xcept {
	    state().methodName = name;
	    state().methodArgs = args;
	    state().methodArgTypes = argTypes;
	    state().methodSfx = 0;
	    writeMethodHdr();
	}

	// write state().method (or continuation)
	protected void writeMethodHdr() throws Xcept {
	    state().methodLineCt = 0;
	    state().methodCharCt = 0;
	    StringTokenizer argToks = new StringTokenizer(state().methodArgs, ",");
	    StringTokenizer typeToks = new StringTokenizer(state().methodArgTypes, ",");
	    if (argToks.countTokens() != typeToks.countTokens()) throw new Xcept(
	    	"Mismatched state().method args (" + state().methodArgs + ") and types (" 
		+ state().methodArgTypes + ")");
	    int n = argToks.countTokens();
	    StringBuffer buf = new StringBuffer("public ");
	    if (state().methodSfx > 0 
	    || !state().methodName.equals(state().className))
	    	buf.append("void ");
	    buf.append(state().methodName);
	    if (state().methodSfx > 0) 
	    	buf.append("__" + state().methodSfx);
	    buf.append("(");
	    for (int i=0; i<n; i++) {
	    	if (i>0) buf.append(",");
		buf.append(typeToks.nextToken() + S + argToks.nextToken());
	    }
	    buf.append(") throws Xcept {");
	    println(buf.toString());
	    indentIn();
	}

	// stop state().method
	protected void stopMethod() throws Xcept {
	    state().methodName = null;
	    indentOut();
	    println("}");
	}

	// write line of output
	protected void println(String s) throws Xcept { 
	    if (isMethodTooBig()) {
		state().methodLineCt = 0;
		state().methodCharCt = 0;
		state().methodSfx++;
		println(state().methodName + "__" + 
		    state().methodSfx + "(" + 
		    state().methodArgs + ");");
	    	indentOut();
		println("}");
	    	writeMethodHdr();
	    }
	    state().out.println(state().indent + s);
	    state().methodLineCt++;
	    state().methodCharCt += s.length();
	}

	// is state().method too big
	protected boolean isMethodTooBig() {
	    if (state().methodName == null) return false;
	    if (! state().breakable) return false;
	    return state().methodLineCt > methodLineMax
	    	|| state().methodCharCt > methodCharMax;
	}

	// indents
	protected void indentIn() { 
	    state().indent = "    " + state().indent; 
	}
	protected void indentOut() { 
	    state().indent = state().indent.substring(4);
	}

	// set method breakable
	protected void setMethodBreakable(boolean b) {
	    state().breakable = b;
	}

	// other query
	public String className() { return state().className; }

	// state().of writer
	public static class State {
	    protected String className;
	    protected File outFile;
	    protected PrintWriter out;
	    protected String indent;
	    protected boolean breakable;

	    // method/line control fields
	    protected String methodName;	
 	    protected String methodArgs;	
 	    protected String methodArgTypes;	
   	    protected int methodLineCt;
   	    protected int methodCharCt;
   	    protected int methodSfx;

	    // constructor
	    public State(String className) {
	    	this.className = className;
		out = new PrintWriter(System.out, true);
		indent = "";
		breakable = true;
	    }
	}
 
}
		
	    	
