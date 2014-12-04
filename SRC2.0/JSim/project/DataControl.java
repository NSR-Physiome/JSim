/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// control representing Data from PDataSet or PModel

package JSim.project; import JSim.aserver.*;

import java.util.*;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*;

public class DataControl extends StringControl {
	protected PNamed metaBase;  // PModel, PPDataSet or PNamedControl
	protected Data data;	// data item if base is PDataSet
	protected ASQuery expr;	// expr if base is PModel 
  	protected Data sourceData; // data specified externally	
	protected StringList domainNames; // for DataSets only

	// values for pickName()
	public static final int NO_NAME = 0;
	public static final int SHORT_NAME = 1;
	public static final int LONG_NAME = 2;

	// constructors
	public DataControl(PNamed p, String n, PNamed b, String d) 
	throws Xcept {
	    super(p, n, d);
	    metaBase = b;
	    valid = false;
	    if (metaBase instanceof PNamedControl) 
	    	((PNamedControl) metaBase).slaves.add(this);
	}
	public DataControl(PNamed p, String n, PNamed b) 
	throws Xcept {
	    this(p, n, b, null);
	}
	
	// base for control, may be null
	public PNamed base() {
	    if (! (metaBase instanceof PNamedControl))
		return metaBase;
	    return ((PNamedControl) metaBase).pnamed();
	}

	// rename Xref
	public void renameXref(PNamed p, String newname)
	throws Xcept {
	    if (! (metaBase instanceof PNamedControl)) return;
	    PNamedControl mb = (PNamedControl) metaBase;
	    if (mb.pnamed() != p)  return;
	    mb.setVal(newname);
	}   

	// set data externally
	public void setSourceData(Data data) {
	    sourceData = data;
	}

	// recalculate valid flag
	public void revalidate() {
	    valid = false;
	    domainNames = null;
	    expr = null;
	    if (sourceData != null) {
	        data = sourceData;
		valid = true;
		return;
	    }
	    if (isBlank()) return;
	    PNamed base = base();
	    if (base == null) {
		validMsg = "invalid dataset";
		return;
	    }

	    // different checks
	    if (base instanceof PModel) {
		try {
		    PModel pmodel = (PModel) base;
		    expr = pmodel.rt().parseQuery(value);
		    valid = true;
		} catch (Xcept e) {
		    validMsg = e.cleanMessage();
	        }
	    } else if (base instanceof PDataSet) {
		data = ((PDataSet) base).data(value);
		if (data == null) 
		    validMsg = value + " not found";
		else
		    valid = true;
	    }
	}	    

	// get domain names
	public StringList getDomainNames() throws Xcept {
	    if (! valid) return null;
	    if (expr != null) 
	        return expr.getDomainNames();
	    if (domainNames == null)
	    	loadDomainNames(getData(0));
	    return domainNames;
	}

	// load domainNames
	private void loadDomainNames(Data data) throws Xcept {
	    domainNames = new StringList();
	    for (int i=0; i<data.ndim(); i++) {
	    	GridData g = data.grid(i);
		String n = g.desc();
		if (Util.isBlank(n))
		    n = "grid " + (i+1);
		domainNames.add(n);
	    }
	}
	
	// get model expr
	public ASQuery getExpr() { return expr; }  

	// get data,  or null if unavailble
	public Data getData(int run) {
	    if (!valid) return null;
	    PNamed base = base();
	    if (base == null) return null;
	    if (base instanceof PDataSet) return data;
	    try {
		ASModel rt = ((PModel) base).rt();
		Data d = rt.getData(run, expr);
		return d;
	    } catch (Xcept e) {
		return null;
	    }
	}

	// plot legend label for this item's ith store
	public String label(int inx) {
	    String label = stringVal();
	    if (!valid) return label;
	    
	    // load unit, group
	    String unitString = null;
	    String group = null;
	    PNamed base = base();
	    if (base == null) return label;
	    if (base instanceof PDataSet) {
	    	if (data != null) {
		    Unit unit = data.unit();
		    if (unit != null) 
		        unitString = unit.pubName();
		    group = data.group();
		}
	    } else if (base instanceof PModel) {
	    	unitString = expr.unitString();
		group = ((PModel) base).rt().getStoreName(inx);
	    }
	    
	    // augment label & return
	    if (unitString != null && !unitString.equals(Unit.dimless))
	    	label = label + " " + unitString;
	    if (group != null)
	    	label = label + " {" + group + "}";
	    return label; 
	}   

	// pick data dimension
	protected int pickName(int ndim) {  return pickName(1, ndim); }
	protected int pickName(int pickDim, int ndim) {
	    if (ndim < pickDim) return NO_NAME;
	    if (ndim == pickDim) return SHORT_NAME;
	    return LONG_NAME;
	}

	// pick list
	public StringList pickList() {
	    StringList list = new StringList(8);
	    PNamed base = base();
	    if (base == null) return null;
	    if (base instanceof PDataSet) {
		PDataSet dataset = (PDataSet) base;
		for (int i=0; i<dataset.nData(); i++) {
		    Data data = dataset.data(i);
		    if (pickName(data.ndim()) == SHORT_NAME) 
		    	list.add(data.name());
		}
	    } else if (base instanceof PModel) try {
		PModel pmodel = (PModel) base;
		ASVar.List asvars = pmodel.rt().getASVars();
		for (int i=0; i<asvars.size(); i++) {
		    ASVar asvar = asvars.asvar(i);
		    if (asvar.isDomain()) continue;
		    int pickName = pickName(asvar.ndim());
		    if (pickName == NO_NAME) continue;
		    String n = (pickName == LONG_NAME) ? 
			asvar.fullName() : asvar.name();
		    list.add(n);
		}
	    } catch (Xcept x) { 
		// DEBUG ONLY System.err.println(x.cleanMessage());
	    }
 
	    return list;
	}

	// NamedVal with given name n
  	public NamedVal namedVal(String n) throws Xcept {
	    return NamedVal.create(n, data);
	}

	// accumulate model save exprs
	public void addModelExprs(PModel pmodel, StringList exprs) {
	    if (base() != pmodel) return;
	    String expr = stringVal();
	    if (! Util.isBlank(expr)) 
	    	exprs.add(expr);
	}
}

