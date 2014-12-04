/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Project server,  may be local or remote

package JSim.aserver;

import JSim.util.*;
import JSim.data.*;
import JSim.fgen.*;
import JSim.lserver.*;

public abstract class ASServer {

	// create server
	public static ASServer create(NamedVal.NList options,
	ASServer.Messenger msgr, ASInfo.Sandbox sandbox)
	throws Xcept {
	    NamedVal nval = options.namedVal("server");

	    // stmt below needed due to Plugin.newInstance
	    //   failure in Applet brower Java (RCCLient class not found)
	    if (nval != null)
	        return new JSim.rclient.RCClient(
		    options, msgr, sandbox);

	    String clssName = (nval == null) ?
	    	"JSim.lserver.LSServer" : "JSim.rclient.RCClient";
	    return (ASServer) Plugin.newInstance(
	    	ClassLoader.getSystemClassLoader(),
	    	clssName,
		new Class[] { 
		    NamedVal.NList.class, ASServer.Messenger.class, ASInfo.Sandbox.class },
		new Object[] { options, msgr, sandbox });
	}	    		        

	// add plugin
	abstract public void addPlugin(Plugin plugin) throws Xcept;

	// query common texts
	abstract public String getCommonText(String fileName) throws Xcept;

	// query available optimizer algorithms
	abstract public OptimAlg.NList optimAlgs() throws Xcept;
	abstract public OptimAlg.Info[] optimAlgsInfo() throws Xcept;

	// get/set dynamic server properties
	abstract public void setProperties(NamedVal[] nvals) throws Xcept;
	abstract public void setProperty(NamedVal nval) throws Xcept;
	abstract public NamedVal getProperty(String n) throws Xcept;

	// get solver/func gen default settings
	abstract public NamedVal.NList getSolverDefaults() throws Xcept;
	public NamedVal.NList getFuncGenDefaults() throws Xcept {
	    return FgenMaster.getDefaults();
	}

	// translate model text
	abstract public String translateModelText(int srcType, 
	int destType, String srcText, String options) 
	throws Xcept;

	// get tranlator warnings
	abstract public String[] getTranslatorWarnings() 
	throws Xcept;

	// create new model run-time
	abstract public ASModel newModelRT() 
	throws Xcept;

	// disconnect from server
	abstract public void disconnect();
	
	// memory message
	abstract public String memoryMessage() throws Xcept;

	// messenger class
	public static interface Messenger {
	    public void message(ASInfo.Message msg);
	}
}
