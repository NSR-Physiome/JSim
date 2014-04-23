/*NSRCOPYRIGHT
	Copyright (C) 1999-2012 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Graphical browser test harness

package JSim.tests;
import JSim.util.*;
import JSim.gui.browser.*;
import JSim.project.*;

import javax.swing.*;
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
public class GBTest {

    public static final String TESTLAYOUT = "graphLayout.txt"; // test graph layout file name
    // Set the Display size:
    int xWidth = 800;
    int yHeight = 600;
    PModelBrowserLayout layout ; // layout of current rendering
    PModelBrowserLayout savedLayout ;  // previously saved layout


    public GBTest() throws Xcept {
	this.layout = new PModelBrowserLayout(null); // layout of current rendering
	this.savedLayout = new PModelBrowserLayout(null);  // previously saved layout
    }

    private static String readFileAsString(String filePath)
	throws java.io.IOException
	{
	    File file = new File(filePath);
	    boolean exists = file.exists();
	    if(exists)
		{
		    StringBuffer fileData = new StringBuffer(1000);
		    BufferedReader reader = new BufferedReader(new FileReader(filePath));
		    char[] buf = new char[1024];
		    int numRead=0;
		    while((numRead=reader.read(buf)) != -1){
			String readData = String.valueOf(buf, 0, numRead);
			fileData.append(readData);
			buf = new char[1024];
		    }
		    reader.close();
		    return fileData.toString();
		}
	    return "";
	}

    // For testing, takes String represent from file and puts it into PModelBrowserLayout format. 
    // Assume String representing layout is sequential (format is graphID, node and its coordinates).
    private static PModelBrowserLayout getSavedLayout(String strLoadedLayout) throws Xcept
    {
	PModelBrowserLayout savedLayout = new PModelBrowserLayout(null);
	String eol = System.getProperty("line.separator");
	String[] lines = strLoadedLayout.split(eol);
	for(int i=0; i < lines.length;i++)
	    {
		// add graph - graphID
		if(lines[i].contains("GraphID"))
		    {
			String[] graphID = lines[i].split(":");
			if(graphID.length == 2)
			    {
				PModelBrowserLayout.Graph newGraph = savedLayout.addNewGraph(graphID[1].trim());
				i++;
				while( (i <lines.length) && (!lines[i].contains("GraphID")) )
				    { // Assume node line format:' nodename:123.333,-2.1233'
					String[] node = lines[i].split(":");
					String[] coord = node[1].split(",");
					if(coord.length == 2)
					    {
						Float x = new Float(coord[0].trim());
						Float y = new Float(coord[1].trim());	
						newGraph.putXY(node[0].trim(),x.floatValue(), y.floatValue());
					    }
					i++;
				    }
				i--;  // if another graph then need to push i back one so can get graphID.
			    }
			else System.out.println("Saved layout wrong format (graphID)");
		    }
	    }
	return savedLayout;
    }

	// Convert specific graph to String
    public String toString(PModelBrowserLayout layout, String graphID) {
	String eol = System.getProperty("line.separator");
	PModelBrowserLayout.Graph graphToPrint = layout.getGraph(graphID);
	String gString = new String();
	if(graphToPrint != null)
	    {
		gString+="GraphID:"+graphID+eol;
		for (Enumeration e =graphToPrint.keys(); e.hasMoreElements();)
		    {
			String nodeN = (String)e.nextElement();
			gString+=nodeN+":"+graphToPrint.getX(nodeN)+","
			    +graphToPrint.getY(nodeN)+eol;
		    }
	    }
	else { System.err.println("**  "+graphID+": does not exist!" );}
	return gString;
	}

	// Convert specific graph to XML DOM
    public Document toXMLDoc(PModelBrowserLayout layout, String graphID)  {
	    try {
	  	DocumentBuilderFactory dbfac = DocumentBuilderFactory.newInstance();
		DocumentBuilder docBuilder = dbfac.newDocumentBuilder();
		Document doc = docBuilder.newDocument();	    
		PModelBrowserLayout.Graph graphToPrint = layout.getGraph(graphID);
		String gString = new String();
		if(graphToPrint != null)
		    {
			Element root = doc.createElement("graph");
			root.setAttribute("graphID",graphID);
			doc.appendChild(root);		  
			for (Enumeration enumN =graphToPrint.keys(); enumN.hasMoreElements();)
			    {
				String nodeN = (String)enumN.nextElement();
				Element child = doc.createElement("node");
				root.appendChild(child);
				child.setAttribute("name",nodeN);
				Float ftemp = graphToPrint.getX(nodeN); 
				Element xcoord = doc.createElement("x");
				xcoord.appendChild(doc.createTextNode(ftemp.toString()));
				child.appendChild(xcoord);
				ftemp = graphToPrint.getY(nodeN);
				Element ycoord = doc.createElement("y");
				ycoord.appendChild(doc.createTextNode(ftemp.toString()));
				child.appendChild(ycoord);
	  		    }
		    }
		else { System.err.println("**  "+graphID+": does not exist!" );}
		return doc;
	    }
		catch (javax.xml.parsers.ParserConfigurationException e) {
		    System.err.println("Parser error: "+e.toString() );
		}
	    return null; // Something wrong happened.
	}

    // ***********************************************************
    //    TEST harness, Syntax:
    //    GBTest? graphMLfile param1 param2 param#
    //   ex: 'java GBTest SeveringO2.xml nc f=variable b=dataType'
    // ***********************************************************
    public static void main(String args[]) throws Exception {
	GBParms parms = new GBParms();
 	GBTest testing = new GBTest();
	File f = new File(args[0]);
	for (int i=1; i<args.length; i++)
	    parms.setParm(args[i]);
	if(parms.graphStyle == 2)
	    parms.graphID = GBParms.SEQUENCES;
	System.out.println(parms.toString());

	 // create a new window to hold the visualization
	 JFrame frame = new JFrame("Perfuse graph test");
	 // ensure application exits when window is closed
	 frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	 frame.setSize(testing.xWidth, testing.yHeight);	    
	 JPanel contentPane = new JPanel(new BorderLayout());  
	 contentPane.setSize(testing.xWidth, testing.yHeight); 	   
	 frame.setContentPane(contentPane);
	 String strLoadedLayout = "";
	 strLoadedLayout = readFileAsString(GBParms.TESTLAYOUT);
	 if(strLoadedLayout !="") {
	     testing.savedLayout =getSavedLayout(strLoadedLayout);
	     //graphParam.setLayout(testing.savedLayout);
	 }
	 GBGraph graphParam = new GBGraph(contentPane, testing.savedLayout);
	 Document doc = UtilXML.parse(f);
	 graphParam.update(doc, parms, true);
	 frame.addWindowListener(new GBWindowAdapter (testing, graphParam, parms.graphID) );
	 testing.layout = graphParam.getLayout();	   
	
	 frame.pack();
	 frame.setVisible(true);	   

	}  


}


// Used only for testing. Gets updated graphLayout on Window closing.
// Simulate JSim user 'saving' graph layout through GUI.
  class GBWindowAdapter extends WindowAdapter {
      GBTest tester;
      GBGraph graph;
      String graphID;

      GBWindowAdapter( GBTest tester, GBGraph graph, String graphID){
	  this.tester = tester;	    
	  this.graph=graph;
	  this.graphID = graphID;
	}

	public void windowClosing(WindowEvent arg0) {
	try {
		    PModelBrowserLayout layout = new PModelBrowserLayout(null);
		    graph.updateLayout();
		    layout = graph.getLayout();
		    BufferedWriter out = new BufferedWriter(new FileWriter(GBParms.TESTLAYOUT));
		    System.out.println("Window closing");
		    for(Enumeration e=layout.keys(); e.hasMoreElements();)
			{
			    String graphName = (String)e.nextElement();
			    System.out.println("Found graph: "+graphName);
			    out.write(this.tester.toString(layout, graphName));
			}
		    out.close();
	} 
	catch (IOException e) {
	    System.err.println("IO Exception: "+e.getMessage());
	    }
	catch (Exception e){
		   System.err.println("Caught Exception: " + e.getMessage()) ;
	    }	
	}  
    }
