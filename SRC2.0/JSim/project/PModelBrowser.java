/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// model browser

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.data.*;

import org.w3c.dom.*;

public class PModelBrowser extends PNamed {
	public StringControl which; // which data: "variables", "sequence", various debug

	// variable graph rendering
	public BooleanControl varCollapse;
	public StringControl varNodeTextColor;
	public StringControl varNodeFillColor;
	public StringControl varNodeBorderColor;
	public StringControl varNodeShape;

	// sequence graph rendering
	public StringControl seqNodeTextColor;
	public StringControl seqNodeFillColor;
	public StringControl seqNodeBorderColor;
	public StringControl seqNodeShape;

	// graph XY layouts
	private PModelBrowserLayout layout;

	// constructor
	public PModelBrowser(PModel p, String n) throws Xcept {
	    super(p, n);
	    which = new StringControl(this, "which", "variables");
	    varCollapse = new BooleanControl(this, "varCollapse", true);	
	    varNodeTextColor = new StringControl(this, "varNodeTextColor", "disabled");	
	    varNodeFillColor = new StringControl(this, "varNodeFillColor", "unitType");	
	    varNodeBorderColor = new StringControl(this, "varNodeBorderColor", "toolType");	
	    varNodeShape = new StringControl(this, "varNodeShape", "isInput");
	    seqNodeTextColor = new StringControl(this, "seqNodeTextColor", "disabled");	
	    seqNodeFillColor = new StringControl(this, "seqNodeFillColor", "itemType");	
	    seqNodeBorderColor = new StringControl(this, "seqNodeBorderColor", "phase");	
	    seqNodeShape = new StringControl(this, "seqNodeShape", "hasDeT");
	    layout = new PModelBrowserLayout(this);
	}

	// query
	public String diagInfo() { return "ModelBrowser " + parent().name(); }
	public String xmlLabel() { return "browser"; }
	public PModelBrowserLayout layout() { return layout; }

}
