/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// plug-in graphic tab

package JSim.gui.plugin;

import javax.swing.JComponent;
import org.w3c.dom.Element;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;
import java.awt.Component;

public abstract class GGraphic implements GProjectListener {
	private Callbacks callbacks;

	// constructor
	public GGraphic(Callbacks c) throws Exception {
	    callbacks = c;
	}

	// get callbacks to environment
	public final Callbacks callbacks() { return callbacks; }

	// get custom graphics JComponent
	public abstract JComponent getJComponent() throws Exception;

	// import state via XML
	public abstract void importXML(Element e) throws Exception;
	
	// export state via XML
	public abstract void exportXML(Element e) throws Exception;

	// Callbacks to environment
	public static interface Callbacks {

	    // which Project is this GGraphic in?
	    Project getProject();

	    // call GUI to select a model 
  	    PModel userModelSelect(Component parent);

	    // call GUI to select a dataset 
  	    PDataSet userDataSetSelect(Component parent);
	}
}
	
