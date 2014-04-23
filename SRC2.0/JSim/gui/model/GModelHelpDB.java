/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// model variable help database

package JSim.gui.model;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import JSim.text.*;
import JSim.aserver.*; import JSim.project.*;
import JSim.gui.*;

public class GModelHelpDB extends GHelpDB {
	private GModel gmodel;
	private VarQuery vquery;	

	// constructor
	public GModelHelpDB(GModel g) {
	    super(null);
	    gmodel = g;
	}

	// refresh DB (s/b compile only)
	public void refresh() {
	    vquery = null;
	}

	// get fixed text
	public String getFixedText(String key) {
	    PModel pmodel = gmodel.pmodel();
	    StringTokenizer stok = new StringTokenizer(
		key, " ()+-*/^");
	    StringList vnames = new StringList(stok);

	    // init vquery, if needed
	    if (vquery == null) 
	    	vquery = new VarQuery(pmodel.rt());
	    return vquery.getText(vnames);
	}
}

