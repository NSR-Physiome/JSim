/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// button removes item:  for now supports only FuncGen removal

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

public class GRTRemoveButton extends GRTButton {

	PNamed prm;	// PNamed to remove

	// constructor
	public GRTRemoveButton(GRTNode p, Element e) {
	    super(p, e);
	    prm = gmodel().pmodel().vars().child(name());
	    action = new GAction(this, "remove") {
		public void doit() throws Xcept {
		    if (prm == null) throw new Xcept(
			"Can't find " + name());
		    prm.parent().remove(prm);
	    	    prm.project().revalidate();
System.err.println("kludge " + gnode.jcomp().getClass());
		    gnode.jcomp().setVisible(false);
		    gnode.gmodel().gpars().removePage(page());
		}
	    };
	}

}

