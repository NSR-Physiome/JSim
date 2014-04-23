/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// translate between various model text formats

package JSim.text;

import org.w3c.dom.*;
import java.util.*;
import java.io.*;
import java.net.URL;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.mml.*;
import JSim.mathml.*;
import JSim.plan.*;
import JSim.sbml.*;
import JSim.cellml.*;
import JSim.aserver.*;

public class XMMLUtil {
		
	// test harness
	public static final void main(String[] args) throws Xcept {

	    // parse command line
	    String jsimHome = null;
	    String options = null;
	    String buildDir = null;
	    int i = 0;
	    while (i<args.length && args[i].startsWith("-")) {
	    	if (args[i].equals("-home"))
		    jsimHome = args[++i];
		else if (args[i].equals("-bd"))
		    buildDir = args[++i];
		else if (args[i].equals("-o"))
		    options = args[++i];
		else throw new Xcept(
		    "Unknown switch: " + args[i]);
		i++;  
	    }
	    if (args.length - i != 1) throw new Xcept(
	    	"Usage: XMMLUtil [-home JSIMHOME] [-bd buildDir] [-o options] file");	 
	    String fname = args[i++];
	    
	    // read text, figure text type
	    File f = new File(fname);
	    String srcText = UtilIO.readText(f);
	    int srcType = -1;
	    if (fname.endsWith(".mod")) srcType = ASModel.TEXT_MML;
	    if (fname.endsWith(".xml")) srcType = ASModel.TEXT_XML;
	    if (fname.endsWith(".sbml")) srcType = ASModel.TEXT_SBML;
	    if (fname.endsWith(".cellml")) srcType = ASModel.TEXT_CELLML;
	    if (srcType < 0) throw new Xcept("Unknown file suffix: " + fname);
	    int destType = ASModel.TEXT_XMML;
	    
	    // create server
	    if (jsimHome != null)
	    	System.setProperty("jsim.home", jsimHome);
	    NamedVal.NList soptions = new NamedVal.NList();
	    if (buildDir != null)
	    	soptions.setVal("buildDir", buildDir);
	    ASServer server = ASServer.create(soptions, null, null);	    

	    // translate
	    try {
	    	String out = server.translateModelText(srcType, destType, srcText, options);
	    	System.out.println(out);
	    } catch (Exception e) {
	    	e.printStackTrace();
	    }

	    // cleanup
	    server.disconnect();
	}
}
	

