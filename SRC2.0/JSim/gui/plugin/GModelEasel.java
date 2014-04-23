/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Plugin drag-n-drop model builder

package JSim.gui.plugin;

import javax.swing.JComponent;
import org.w3c.dom.Element;
import JSim.util.*;
import JSim.data.*;

public abstract class GModelEasel {
	protected Callbacks callbacks;

	// internal Easel state change constants
	public static final int EASEL = 1; // any saveable changes
	public static final int MML = 2; // changes to MML
	public static final int NAMEDVALS = 4; // changes to params
	public static final int RTML = 8; // changes to RTML

	// create new easel object
	public GModelEasel(Callbacks cb) throws Exception {
	    callbacks = cb;
	}
	
	// get JComponent for this easel
	public abstract JComponent getJComponent();

	// get MML for current GModelEasel state
	public abstract String getMML() throws Exception;

	// get named parameter values for current GModelEasel state(if any)
	public NamedVal[] getNamedVals() throws Exception {
	    // not yet implemented
	    return null;
	}

	// get RTML for current GModelEasel state
	public Element getRTML() throws Exception {
	    // not yet implemented
	    return null;
	}

	// import state via XML
	public void importXML(Element e) throws Exception {
	    // not yet implemented
	}

	// export state via XML
	public void exportXML(Element e) throws Exception {
	    // not yet implemented
	}

	// GModelEasel.Callbacks: callbacks to JSim application
	public static interface Callbacks {

	    // tell JSim to recompile the model
	    public void easelCompile();

	    // tell JSim that easel internal state has changed
	    // state values numerical ORs of EASEL, MML, NAMEDVALS, RTML
	    public void easelStateChanged(int state);
	}
}
	    
