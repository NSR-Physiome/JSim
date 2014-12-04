/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// name and value associated

package JSim.data;

import JSim.util.*;
import JSim.expr.*;
import java.io.Serializable;
import java.util.ArrayList;

public abstract class NamedVal implements Named, DiagInfo, Serializable {

	// pseudo-constructors
	public static NamedVal create(String n, double d) {
	    return new Real(n, d);
	}
	public static NamedVal create(String n, int i) {
	    return new Int(n, i);
	}
	public static NamedVal create(String n, boolean b) {
	    return new Bool(n, b);
	}
	public static NamedVal create(String n, String s) {
	    return new Str(n, s);
	}
	public static NamedVal create(String n, String[] s) {
	    return new Text(n, s);
	}
	public static NamedVal create(String n, Data d) {
	    return new Dat(n, d);
	}

	// guess constructor: UNSAFE, test harnesses only!!!)
	public static NamedVal guess(String n, String val) throws
	Xcept {
	    if (val.equals("true"))
	    	return create(n, true);
	    if (val.equals("false"))
	    	return create(n, false);
	    try {
	    	int i = Integer.parseInt(val);
		return create(n, i);
	    } catch (NumberFormatException e) {
	    	try {
		    double d = Double.parseDouble(val);
		    return create(n, d);
		} catch (NumberFormatException e2) {
		    return create(n, val);
		}
	    }
	}

	// instance variable
	private String name;

	// constructors
	private NamedVal(String n) {
	    name = n;
	}
	protected NamedVal() { } // for serialization only

	// query
	abstract public String stringVal();
	public String name() { return name; }
	public String toString() { 
	    String s = stringVal();
	    if (s != null) s = s.replace('\n',',');
	    return name + "=" + s; 
	}
	public String diagInfo() { return toString(); }
	public double realVal() throws Xcept {
	    throw new Xcept(this, "realVal() not implemented");
	}
	public int intVal() throws Xcept {
	    throw new Xcept(this, "intVal() not implemented");
	}
	public boolean boolVal() throws Xcept {
	    throw new Xcept(this, "boolVal() not implemented");
	}
	public Data dataVal() throws Xcept {
	    throw new Xcept(this, "dataVal() not implemented");
	}
	public String[] textVal() throws Xcept {
	    throw new Xcept(this, "textVal() not implemented");
	}
	abstract public int dataType();

	// real data sub-class
	public static class Real extends NamedVal implements Serializable {
	    protected double realVal;
	    public Real(String n, double r) {
		super(n);
		realVal = r;
	    }
	    protected Real() { super(); } // for serialization only
	    public double realVal() { return realVal; }
	    public int intVal() { return (int) realVal; }
	    public String stringVal() { return "" + realVal; }
	    public int dataType() { return Expr.REAL; }
	}

	// int data sub-class
	public static class Int extends NamedVal implements Serializable {
	    protected int intVal;
	    public Int(String n, int i) {
		super(n);
		intVal = i;
	    }
	    protected Int() { super(); } // for serialization only
	    public double realVal() { return (double) intVal; }
	    public int intVal() { return intVal; }
	    public String stringVal() { return "" + intVal; }
	    public int dataType() { return Expr.REAL; }
	}

	// boolean data sub-class
	public static class Bool extends NamedVal implements Serializable {
	    protected boolean boolVal;
	    private Bool(String n, boolean b) {
		super(n);
		boolVal = b;
	    }
	    protected Bool() { super(); } // for serialization only
	    public boolean boolVal() { return boolVal; }
	    public String stringVal() { return "" + boolVal; }
	    public int dataType() { return Expr.BOOLEAN; }
	}

	// string data sub-class
	public static class Str extends NamedVal implements Serializable {
	    protected String stringVal;
	    private Str(String n, String s) {
		super(n);
		stringVal = s;
	    }
	    protected Str() { super(); } // for serialization only
	    public String stringVal() { return stringVal; }
	    public int dataType() { return Expr.STRING; }
	}

	// text data sub-class
	protected static class Text extends NamedVal implements Serializable {
	    protected String[] text;
	    private Text(String n, String[] t) {
		super(n);
		text = t;
	    }
	    protected Text() { super(); } // for serialization only
	    public String stringVal() { 
	    	if (text == null) return null;
		StringBuffer buf = new StringBuffer(text[0]);
		for (int i=1; i<text.length; i++) {
		    buf.append("\n");
		    buf.append(text[i]);
		}
		return buf.toString();
	    }
	    public String[] textVal() {
		return text;
	    }
	    public int dataType() { return Expr.UNDEFINED; }
	}

	// data sub-class
	protected static class Dat extends NamedVal {
	    protected Data dataVal;
	    private Dat(String n, Data d) {
		super(n);
		dataVal = d;
	    }
	    private Dat(DatInfo dinfo) throws Xcept {
		super(dinfo.name());
		if (dinfo.info != null) 
		    dataVal = Data.makeData(dinfo.info);
	    }
	    public String stringVal() { 
	        if (dataVal == null) return null;
		return dataVal.name(); 
	    }
	    public Data dataVal() { return dataVal; }
	    public int dataType() { return Expr.UNDEFINED; }
	}

	// data info sub-class
	protected static class DatInfo extends NamedVal implements Serializable {
	    protected DataInfo info;
	    private DatInfo(Dat data) throws Xcept {
		super(data.name());
		if (data.dataVal() != null) 
		    info = data.dataVal().info();
	    }
	    public String stringVal() { return info.name; }
	    public int dataType() { return Expr.UNDEFINED; }
	}

	// simple list
	public static class List extends ArrayList<NamedVal> {
	    public List() { super(); }
	    public NamedVal nval(int i) { 
		return (NamedVal) get(i);
	    }
	    public NamedVal nval(String n) {
	    	for (int i=0; i<size(); i++) 
		    if (n.equals(nval(i).name))
		    	return nval(i);
		return null;
	    }
	    public NamedVal[] info() throws Xcept {
		NamedVal[] nvals = new NamedVal[size()];
		for (int i=0; i<nvals.length; i++) {
		    NamedVal nval = nval(i);
		    if (nval instanceof Dat)
			nval = new DatInfo((Dat) nval);
		    nvals[i] = nval;
		}
		return nvals;
	    }
	}

	// name-queriable list
	public static class NList extends NamedList implements Query {
	    public NList() { super(); }
	    public NList(NamedVal[] nvals) throws Xcept {
		super();
		for (int i=0; i<nvals.length; i++) {
		    NamedVal nval = nvals[i];
		    if (nval instanceof DatInfo) 
			nval = new Dat((DatInfo) nval);
		    add(nval);
		}
	    }
	    public NamedVal nval(int i) { 
		return (NamedVal) get(i); 
	    }
	    public NamedVal namedVal(String n) {
		return (NamedVal) getByName(n); 
	    }
	    public NamedVal nval(String n) { return namedVal(n); }
	    public NamedVal[] info() throws Xcept {
		NamedVal[] nvals = new NamedVal[size()];
		for (int i=0; i<nvals.length; i++) {
		    NamedVal nval = nval(i);
		    if (nval instanceof Dat)
			nval = new DatInfo((Dat) nval);
		    nvals[i] = nval;
		}
		return nvals;
	    }

	    // default query
	    public boolean boolVal(String name, boolean def) {
		try {
	    	    NamedVal nval = nval(name);
		    return nval.boolVal();
		} catch (Exception e) {
		    return def;
		}
	    }
	    public int intVal(String name, int def) {
		try {
	    	    NamedVal nval = nval(name);
		    return nval.intVal();
		} catch (Exception e) {
		    return def;
		}
	    }
	    public double realVal(String name, double def) {
		try {
	    	    NamedVal nval = nval(name);
		    return nval.realVal();
		} catch (Exception e) {
		    return def;
		}
	    }
	    public String stringVal(String name, String def) {
		try {
	    	    NamedVal nval = nval(name);
		    return nval.stringVal();
		} catch (Exception e) {
		    return def;
		}
	    }

	    // set values
	    public void setVal(String name, boolean val) throws Xcept {
	    	set(NamedVal.create(name, val));
	    }
	    public void setVal(String name, int val) throws Xcept {
	    	set(NamedVal.create(name, val));
	    }
	    public void setVal(String name, double val) throws Xcept {
	    	set(NamedVal.create(name, val));
	    }
	    public void setVal(String name, String val) throws Xcept {
	    	set(NamedVal.create(name, val));
	    }

	}

	// name-queryiable interface
	public static interface Query {
	    public NamedVal namedVal(String n) throws Xcept;
	}

	// test program
	public static void main(String[] args) throws Exception {
	    NamedVal.NList overrides = new NamedVal.NList();
	    NamedVal tmax = NamedVal.create("t.min", 5);
	    System.err.println("addstart");
	    overrides.add(tmax);
	    System.err.println("addOK");
	}
}

