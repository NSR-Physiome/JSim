/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// One axis in Nested graph

package JSim.project;

import JSim.util.*;
import org.w3c.dom.Element;

public abstract class PNestedAxis extends PNamed {

	// constants
	public static final String X1 = "x1";
	public static final String Y1 = "y1";
	public static final String X2 = "x2";
	public static final String Y2 = "y2";
	public static final String Z = "z";
	public static final String LINE = "line";
	public static final String COLOR = "color";
	public static final String SIZE = "size";
	public static final String SHAPE = "shape";
	public static final String THICKNESS = "thickness";
	public static final String[] ALLIDS = new String[] { 
	    X1, Y1, X2, Y2, Z,
	    LINE, COLOR, SIZE, SHAPE, THICKNESS };
	public static final String[] ATTRIDS = new String[] { 
	    LINE, COLOR, SIZE, SHAPE, THICKNESS };
	public static final String AXIS_SUFFIX = "axis";

	// instance fields
	protected String id;
	public BooleanControl autoscale;
	public RealControl dataMin; // if not autoscale
	public RealControl dataMax; // if not autoscale
	
	// constructor
	public PNestedAxis(PNamed p, String id) throws Xcept {
	    super(p, id + AXIS_SUFFIX);
	    this.id = id;	    
	    autoscale = new BooleanControl(this, "autoscale", true);
	    dataMin = new RealControl(this, "dataMin", 0);
	    dataMax = new RealControl(this, "dataMax", 1);
	}

	// abstract query
	abstract public PNested pnested();
	abstract public boolean isGlobal(); 
	abstract public PNestedAxis.Global globalAxis();

	// simple query
	public String id() { return id; }
	public String diagInfo() { return "NestedAxis " + name; }
	public String xmlLabel() { return "nestedaxis"; }
	public boolean isLocal() { return ! isGlobal(); }
  	public boolean isInner() {
	    if (id.equals(X1)) return true;
	    if (id.equals(Y1)) return true;
	    if (id.equals(Z)) return true;
	    return false;
	}
	public boolean isOuter() {
	    if (id.equals(X2)) return true;
	    if (id.equals(Y2)) return true;
	    return false;
	}
	public boolean isAttr() {
	    if (id.equals(LINE)) return true;
	    if (id.equals(COLOR)) return true;
	    if (id.equals(SIZE)) return true;
	    if (id.equals(SHAPE)) return true;
	    if (id.equals(THICKNESS)) return true;
	    return false;
	}	    
	public boolean isSparse() {
	    return isOuter() || isAttr();
	}
	public boolean isDense() {
	    return ! isSparse();
	}
	public boolean isDependent() {
	    return id.equals(pnested().dependentAxisID());
	}
	public boolean isIndependent() {
	    return ! isDependent();
	}
	public boolean isActive() {
	    if (id.equals(X1)) return true;
	    if (id.equals(Y1)) return true;
	    if (id.equals(X2)) return pnested().xnesting.val();
	    if (id.equals(Y2)) return pnested().ynesting.val();
	    boolean isXY = (pnested().style.val() == PNested.XY_PLOT);
	    if (id.equals(Z)) return !isXY;
	    if (isAttr()) {
	    	if (! isActivatable()) return false;
	    	return globalAxis().show.val();
	    }
	    return false;
	}
	public boolean isActivatable() {
	    if (id.equals(COLOR)) return true;
	    return pnested().style.val() == PNested.XY_PLOT;		
	}
	public int maxSparseSamples() throws Xcept {
	    if (isOuter())
	        return globalAxis().nsubAxes.val();
	    if (id.equals(LINE)) return 10;
	    if (id.equals(COLOR)) return 13;
	    if (id.equals(SIZE)) return 4;
	    if (id.equals(SHAPE)) return 5;
	    if (id.equals(THICKNESS)) return 3;
	    throw new Xcept("maxSparseSamples(" + id + 
	        ") not supported for dense axis");
	} 

	// Global axis subclass
	public static class Global extends PNestedAxis {
	    public BooleanControl log;
	    public IntControl nsubAxes; // if isOuter()
	    public BooleanControl show; // visible? if isAttr()	   

	    // constructor
	    public Global(PNested p, String id) throws Xcept {
	    	super(p, id);
	    	log = new BooleanControl(this, "log", false,
		    new String[] { "log", "linear" });
	    	if (isOuter()) 
		    nsubAxes = new IntControl(this, "nsubAxes", 5);
		if (isAttr()) 
		    show = new BooleanControl(this, "show", false);
	    }

	    // query
	    public PNested pnested() { return (PNested) parent(); }
	    public boolean isGlobal() { return true; }
	    public PNestedAxis.Global globalAxis() { return this; }
	}

	// Local axis subclass
	public static class Local extends PNestedAxis {
	    public PNestedDomainControl domain;

	    public static final int LINEAR = 0;
	    public static final int LOG = 1;
	    public static final int LIST = 2;

	    // controls if isSparse()
	    public ChoiceControl sampleMethod; // LINEAR, LOG or LIST
	    public StringControl sampleList;  // if method = LIST
	    
	    public Local(PNestedItem p, String id) throws Xcept {
	    	super(p, id);
		domain = new PNestedDomainControl(this, "domain");
		if (isSparse()) {
		    sampleMethod = new ChoiceControl(this, "sampleMethod",
		        0, new String[] { "linear", "log", "list" });
		    sampleList = new StringControl(this, "sampleList");
		}
	    }

	    // query
	    public PNested pnested() { return (PNested) parent().parent(); }
	    public PNestedItem item() { return (PNestedItem) parent(); }
	    public boolean isGlobal() { return false; }
	    public PNestedAxis.Global globalAxis() {
	    	return pnested().globalAxis(id);
	    }
	}
}

