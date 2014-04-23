/*NSRCOPYRIGHT
	Copyright (C) 1999-2012 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Parameters for GBGraph rendering

package JSim.gui.browser; 

import JSim.util.*;
import JSim.project.*;

import javax.swing.*;
import java.util.*;
import java.awt.Dimension;
import java.io.*;
import java.awt.BorderLayout;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.event.WindowAdapter;
import org.w3c.dom.*;
import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.*;
import javax.xml.transform.stream.*;

import prefuse.Constants;


public class GBParms {
  public      String  graphID;       // graph data to render
  public      int     graphStyle;     // VARIABLES or SEQUENCE
  public      boolean collapseVars;  // combine all nodes for same variable
  public      String  nodeText;      // property determining text value (default to 'name'
  public      String  nodeTextColor; // property determining node text color (NOT in Use)
  public      String  nodeFillColor; // property determining node bkgd fill
  public      String  nodeBorderColor; // property determing node border color
  public      String  nodeShape;     // bool property determining shape for node
  public      String  nodePopup;     // property to determine what info in node mouseover popup
  public      String  groupBy;       // property to determine how/if to group nodes together- Only for variables graph

  // constants
  public static final int VARIABLES = 1;
  public static final int SEQUENCE = 2;

  public static final String NAME = "name";
  public static final String ITEM_TYPE = "itemType" ;  // sequence graph
  public static final String ITEM_TEXT = "itemText";   //    "
  public static final String SEQUENCES = "sequence";   //    "     
  public static final String PHASE = "phase";          //    "
  public static final String UNIT_TYPE = "unitType" ;  // variables graph
  public static final String UNIT = "unit";            //     "     graph
  public static final String VARIABLE ="variable";     //     "
  public static final String DATA_TYPE = "dataType";    //     "
  public static final String TOOL_TYPE ="toolType";     //     "
  public static final String TOOL_TEXT = "toolText";    //     "
  public static final String EVENT_TEXT = "eventText";  //     "
  public static final String DOMAINS = "domains";
  public static final String IS_INPUT = "isInput";      //     "
  public static final String IS_PRIVATE = "isPrivate";  //     "

  public static final String DISABLED = "disabled";   //  All graphs

  public static final String TESTLAYOUT = "graphLayout.txt"; // test graph layout file name

	// constructors
	public GBParms() {
	    graphID = "variables";
	    graphStyle = VARIABLES;  // default to variables graph Not needed?
	    collapseVars = true;
	    nodeText = NAME;
	    nodeTextColor = null;
	    nodeFillColor = null;  //"unitType";
	    nodeBorderColor= null;
	    nodeShape = null;    //"isInput";
	    nodePopup=null;      //"toolText";	    
	}
	
	// update parms with text, throw Xcept if invalid
	public void setParm(String txt) throws Xcept {

	    // shorthand substitution
	    if (txt.startsWith("-"))
	    	txt = txt.substring(1);
	    if (txt.equals("c"))
	    	txt = "collapseVars=true";
	    else if (txt.equals("nc"))
	    	txt = "collapseVars=false";
	    else if (txt.startsWith("f="))
	    	txt = "nodeFillColor=" + txt.substring(2);
	    else if (txt.startsWith("b="))
	    	txt = "nodeBorderColor=" + txt.substring(2);
	    else if (txt.startsWith("s="))
	    	txt = "nodeShape=" + txt.substring(2);
	    else if (txt.startsWith("p="))
	    	txt = "nodePopup=" + txt.substring(2);
	    else if (txt.startsWith("a="))		// aggregate by
	    	txt = "groupBy=" + txt.substring(2);
	    else if (txt.startsWith("g="))
	    	txt = "graphStyle=" + txt.substring(2);

	    // update
	    int ce = txt.indexOf('=');
	    String key = txt.substring(0, ce);
	    String val = txt.substring(ce+1);
	    System.out.println("Value of val: "+val);

	    if (Util.isBlank(val)) val = null;

	    if (key.equals("graphID"))
	    	graphID = val;
	    else if (key.equals("collapseVars"))
	    	collapseVars = Util.toBoolean(val);
	    else if (key.equals("nodeTextColor"))
		nodeTextColor = val;
	    else if (key.equals("nodeFillColor"))
		nodeFillColor = val;
	    else if (key.equals("nodeBorderColor"))
		nodeBorderColor = val;
	    else if (key.equals("nodeShape"))
		nodeShape = val;
	    else if (key.equals("nodePopup"))
		nodePopup = val;
	    else if (key.equals("groupBy"))
		groupBy = val;
	    else if (key.equals("graphStyle"))
		graphStyle = Util.toInt(val);
		
	    else throw new Xcept(
	    	"Unrecognized MDG render parameter=" + key);
	}

	// String rep
	public String toString() {
	    String s00 = "graphID=" + graphID;
	    String s0 = "style=" + graphStyle;
	    String s1 = collapseVars ? "c" : "nc";
	    String s2 = "t=" + nodeText;
	    String s3 = "f=" + nodeFillColor;
	    String s4 = "b=" + nodeBorderColor;
	    String s5 = "s=" + nodeShape;
	    String s6 = "p=" + nodePopup;
	    return s00 + " " + s0 + " " + s1 + " " + s2 + " " + s3 + " " + s4 + " " + s5 + " " + s6;
	}

   
    // ***********************************************************
    //    TEST harness, Syntax:
    //    GBParms graphMLfile param1 param2 param#
    //   ex: 'java GBParms SeveringO2.xml nc f=variable b=dataType'
    // ***********************************************************
	public static void main(String args[]) throws Exception {
	    GBParms parms = new GBParms();
	    // Set the Display size:
	    int xWidth = 800;
	    int yHeight = 600;

	    File f = new File(args[0]);
	    for (int i=1; i<args.length; i++)
	    	parms.setParm(args[i]);
	    if(parms.graphStyle == 2)
		parms.graphID = SEQUENCES;
	    System.out.println(parms.toString());

	    // create a new window to hold the visualization
	     JFrame frame = new JFrame("Perfuse graph test");
	    // ensure application exits when window is closed
	    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	    frame.setSize(xWidth, yHeight);	    
	    JPanel contentPane = new JPanel(new BorderLayout());  
	    contentPane.setSize(xWidth, yHeight); 	   
	    frame.setContentPane(contentPane);
	    PModelBrowserLayout layout = new PModelBrowserLayout(null);
	    GBGraph graphParam = new GBGraph(contentPane, layout);
	    Document doc = UtilXML.parse(f);
	    graphParam.update(doc, parms, true);
	    	   
	    frame.pack();
	    frame.setVisible(true);	        
       }

} 
