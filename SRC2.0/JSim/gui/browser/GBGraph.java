/*NSRCOPYRIGHT
	Copyright (C) 1999-2012 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.gui.browser;
import JSim.util.*;
import JSim.data.*;
import JSim.project.*;

import java.io.ByteArrayInputStream;
import javax.swing.*;
import java.util.*;
import java.awt.*;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import org.w3c.dom.*;

import prefuse.Constants;
import prefuse.data.Graph;
import prefuse.data.io.DataIOException;
import prefuse.data.io.GraphMLReader;
import prefuse.data.Table;
import prefuse.data.tuple.TableNode;
import prefuse.data.util.TableIterator;

// Reads xml file and sends the graph object to class GBRenderData

public class GBGraph {
    private JPanel panel; // graph parent
    private Document graphDoc; // GraphML for all graphs
    private GBRenderData renderGraph;
   
    protected GBParms userGraphParams; // parameters to render
    protected PModelBrowserLayout layout; // node positions 

    // constants
    public static final String X_COORD = "x_coord";
    public static final String Y_COORD = "y_coord";
    public static final String NAME = "name";    // Column for node name
    public static final float RATIO_NODES_SAME = 0.35f; // If ratio too low then generate a new layout.
    // constructor
    public  GBGraph(JPanel newPanel, PModelBrowserLayout layout) {
	this.panel = newPanel;
 	this.layout = layout;
	this.renderGraph = null;  // New graph, no rendering yet.
    }

    // update graphics 
    // if preserveLayout, keep previous node positions if possible
    //    otherwise use default layout mechanism
    public void update(Document graphDoc, GBParms params, 
    boolean preserveLayout) throws Xcept {
	try {
	    GraphMLWriter wrt = new GraphMLWriter();
	    String graphMLStr = wrt.writeString(graphDoc, params.graphID);
	    boolean newDoc = (graphDoc == this.graphDoc);
	    this.graphDoc = graphDoc;
	    this.userGraphParams = params;
	    ByteArrayInputStream graphMLBytes = 
	    	new ByteArrayInputStream(graphMLStr.getBytes());
	    Graph graph = new GraphMLReader().readGraph(graphMLBytes);
	    // Check if graph currently rendered:
	    if (renderGraph !=null)   
		this.updateLayout();
	    // Check if saved layout for graph:
	    if(!layout.isEmpty() && preserveLayout==true)
		{ 
		    if(layout.containsKey(params.graphID)) {
			addCoordToGraph(graph);
		    }  
		} 	    
	    panel.removeAll();
	    renderGraph=new GBRenderData(graph, userGraphParams, panel);
	    renderGraph.DrawData();
	} catch (Exception e) {
	    throw Xcept.wrap(e);
	} 
    }

    // load current XY positions for graph(s) into layout
    public void updateLayout() throws Xcept {
	if (renderGraph != null) // may not have been rendered!
	    renderGraph.getNodeCoordinates(layout);  // Get current coord of displayed graph nodes.
    }

    // get layout (called externally from application)
    public PModelBrowserLayout getLayout() throws Xcept {
        return layout;
    }

    // Assumes that nodes are in sequential order (ie no skipping of numbers : 1,2,3,6,7,9,etc)
    private boolean addCoordToGraph(Graph graph)
    {
	float xWidth =0f;
	float yHeight =0f;
	Dimension panelSize = new Dimension();
	panelSize = this.panel.getSize();
	xWidth =(float)panelSize.getWidth();
	yHeight=(float)panelSize.getHeight();
	Table nodeTable = graph.getNodeTable();
	Hashtable<Integer,String> nodesList = new Hashtable<Integer,String>();
	if(isGraphSame(nodeTable, nodesList)) 
	    {
		// add x and y coord columns.
		nodeTable.addColumn(X_COORD,float.class,0f);
		nodeTable.addColumn(Y_COORD,float.class,0f);
		if(nodeTable.canSetFloat(X_COORD))
		    { 
			PModelBrowserLayout.Graph g = this.layout.getGraph(userGraphParams.graphID);
			for (Integer i=0;i<nodesList.size();i++)
			    {
				String nodeName = nodesList.get(i);			
				Float x =0f;
				x = g.getX(nodeName);
				if(x.isNaN())
				    x=i.floatValue()/(2*nodesList.size()); // Normalize unsaved nodes
				Float y = 0f;
				y = g.getY(nodeName);
				if(y.isNaN())
				    y=i.floatValue()/(2*nodesList.size()); // Normalize unsaved nodes
				nodeTable.setFloat(i,X_COORD,x*xWidth);
				nodeTable.setFloat(i,Y_COORD,y*yHeight);
			    }
			return true;
		    }
	      	return false; // Could not add X_COORD to table
	    }
	else return false; //saved layout too different from current graph.
    }

    private boolean isGraphSame(Table nodeTable, Hashtable<Integer,String> nodeList)
    {
	PModelBrowserLayout.Graph g = this.layout.getGraph(userGraphParams.graphID);
	Integer found = 0;
	Integer totalNumberNodes = (Integer)nodeTable.getRowCount();
	TableIterator iter = nodeTable.iterator();
	while( iter.hasNext() )
	    {
		int nodeIndex = iter.nextInt();
		if(iter.canGetString(NAME))
		    {
			String nodeName = iter.getString(NAME);
			if(g.containsKey(nodeName))
			    {
				found++;
			    }
			// want list of all nodes, regardless of previously saved: 
			nodeList.put(nodeIndex,nodeName); 
		    }
	    }
	if( (found.floatValue()/totalNumberNodes.floatValue()) > RATIO_NODES_SAME)  //  otherwise new layout
	    {
		return true;
	    }
	else return false; // saved layout very different.     	
    }

}

 
