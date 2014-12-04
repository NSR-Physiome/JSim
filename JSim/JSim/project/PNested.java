/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Nested graph

package JSim.project;

import JSim.util.*;
import JSim.data.*;
import java.util.*;

public class PNested extends PNamed {

	// constants
	public static final int XY_PLOT = 0; // for style below
	public static final int CONTOUR = 1; 
	public static final int SURFACE = 2; // not yet implemented

	public static final int GLOBAL = 0; // for scaling below
	public static final int PAGE = 1; 
	public static final int LOCAL = 2; 

	public static String TITLE_DEFAULT = "[Title]";
	public static String FOOTER_DEFAULT = "[Footer]";

	// show controls
	public BooleanControl showTitle;
	public StringControl title; // title text
	public BooleanControl showFooter;
	public StringControl footer; // footer text
	public BooleanControl showConf;

	// layout control
	public ChoiceControl style; // XY_PLOT/CONTOUR/SURFACE
	public ChoiceControl scaling; // GLOBAL/PAGE/LOCAL
	public BooleanControl xnesting; // level of nesting 1/2 for now
	public BooleanControl ynesting; // level of nesting 1/2 for now
	public IntControl nItems; // # data items to plot
	public IntControl confItem; // item displayed in GUI
	
	// items, axes
	public ArrayList<PNestedItem> items; // data items	
	private Hashtable<String, PNestedAxis.Global> globalAxes; 

	// constructor
	public PNested(PNamed p, String n) throws Xcept {
	    super(p, n);

	    // init controls
	    nItems = new IntControl(this, "nItems", 1) {
		public void updateOther() throws Xcept {
		    reconfig();
		}
	    };
	    showTitle = new BooleanControl(this, "showTitle", true);
	    title = new StringControl(this, "title", TITLE_DEFAULT);
	    showFooter = new BooleanControl(this, "showFooter", true);
	    footer = new StringControl(this, "footer", FOOTER_DEFAULT);
	    showConf = new BooleanControl(this, "showConf", true);
	    style = new ChoiceControl(this, "style", XY_PLOT,
		new String[] { "XY plot", "contour" }) {
	    };
	    scaling = new ChoiceControl(this, "scaling", GLOBAL,
		new String[] { "global", "page", "local" }) {
	    };
	    xnesting = new BooleanControl(this, "xnesting", false,
	    	new String[] { "2", "1" });
	    ynesting = new BooleanControl(this, "ynesting", false,
	    	new String[] { "2", "1" });
	    confItem = new IntControl(this, "confItem", 0) {
		public void updateOther() throws Xcept {
		    if (val()>=nItems.val()) 
			nItems.setVal(val()+1);
		}
	    };		
	    items = new ArrayList<PNestedItem>();
	    globalAxes = new Hashtable<String, PNestedAxis.Global>();
	    for (int i=0; i<PNestedAxis.ALLIDS.length; i++) {
	        String id = PNestedAxis.ALLIDS[i];
	    	globalAxes.put(id, new PNestedAxis.Global(this, id));
	    }

	    reconfig();
	}

	// change nitems
	public void reconfig() throws Xcept {
	    int ct = nItems.val();
	    if (ct<1) ct=1;
	    int ct0 = items.size();

	    // add missing plot children
	    for (int i=ct0; i<ct; i++) 
		items.add(new PNestedItem(this, "item" + i, i));

	    // remove extra plot children
	    for (int i=ct0-1; i>=ct; i++) {
		PNestedItem item = item(i);
		int inx = items.indexOf(item);
		if (inx <= 0) throw new Xcept(
		    "Internal error removing plot item " + i);
		items.remove(item);
		remove(item);
	    }

	    if (confItem.val() >= ct)
		confItem.setVal(0);
	}

	// query
	public String diagInfo() { return "Nested " + name; }
	public String xmlLabel() { return "nested"; }
	public PNestedAxis.Global globalAxis(String id) { 
	    return globalAxes.get(id); }
	public PNestedItem item(int i) { return items.get(i); }

	public PNestedItem confItem() { return item(confItem.val()); }
	 
	public boolean hasBlankItems() {
	    for (int i=0; i<items.size(); i++) 
		if (item(i).expr.isBlank()) return true;
	    return false;
	}
	public String dependentAxisID() {
	    if (style.val() == XY_PLOT) 
	        return PNestedAxis.Y1;
	    else
	        return PNestedAxis.Z;
	}
	
}
