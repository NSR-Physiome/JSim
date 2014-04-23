/*NSRCOPYRIGHT
	Copyright (C) 1999-2012 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.gui.browser; 
import JSim.util.*;
import JSim.project.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.*;
import javax.swing.SwingUtilities;

import prefuse.Constants;
import prefuse.Display;
import prefuse.Visualization;
import prefuse.action.ActionList;
import prefuse.action.RepaintAction;
import prefuse.action.assignment.*;
import prefuse.action.layout.graph.*;
import prefuse.action.layout.Layout;
import prefuse.action.layout.SpecifiedLayout;
import prefuse.activity.Activity;
import prefuse.controls.*;
import prefuse.data.Graph;
import prefuse.data.Table;
import prefuse.data.Schema;
import prefuse.data.util.TableIterator;
import prefuse.data.io.DataIOException;
import prefuse.data.io.GraphMLReader;
import prefuse.render.*;
import prefuse.render.Renderer;
import prefuse.data.expression.Predicate;
import prefuse.data.expression.parser.ExpressionParser;

import prefuse.util.*;
import prefuse.util.ui.*;
import prefuse.visual.*;
import prefuse.visual.expression.InGroupPredicate;


// *****************************************************************
// class RenderData(Graph graph, MDParms graphParams, JFrame frame)
// Takes a Graph object and renders it graphically. 
// Calls TransformData, if needed, to modify nodes/edges before rendering.
// *****************************************************************

public class GBRenderData {

    Graph graph;
    JPanel panel;
    Visualization vis;
    VisualGraph vg;
    String groupType;   // What Type to aggregate data by : Currently only GBParms.PHASE
    double zoomMagnitude;    // default zoom used by jpanel to display graphs.
    int perferredNodeCount;  // Max node count where default zoom used. More nodes, less zoom.
    boolean useSavedLayout;
    GBParms userGraphParams;
   
    // constants
    public static final String GRAPH = "graph";
    public static final String AGGR = "aggregates";
    public static final String AGGR_DECORATORS = "aggrDeco";  
    public static final String CIRC = "circular";
    // Default frame sizes for testing browser w/out JSim:
    public static final int MIN_WIDTH=600;
    public static final int MIN_HEIGHT=400;
    public static final int PERFERRED_NODE_COUNT=75;
    public static final float DEFAULT_ZOOM_MAGNITUDE=1.5f;

    private static final Schema DECORATOR_SCHEMA = PrefuseLib.getVisualItemSchema(); 
    static { 
    	DECORATOR_SCHEMA.setDefault(VisualItem.INTERACTIVE, false); 
    	DECORATOR_SCHEMA.setDefault(VisualItem.TEXTCOLOR, ColorLib.gray(128)); 
    	DECORATOR_SCHEMA.setDefault(VisualItem.FONT, FontLib.getFont("Tahoma",16));
    }
 
    // Constructor
    GBRenderData ( Graph graph, GBParms graphParams, JPanel panel ) throws Xcept
    {

	this.userGraphParams = graphParams;
	this.zoomMagnitude = DEFAULT_ZOOM_MAGNITUDE;
	this.perferredNodeCount = PERFERRED_NODE_COUNT;
	this.useSavedLayout = false;
        // the visualization --------------------------------------------    
        // add the graph to the visualization as the data group "graph"
        // nodes and edges are accessible as "graph.nodes" and "graph.edges"
        this.vis = new Visualization();	
	this.graph = graph;
 	this.panel = panel; 
    }

    public void DrawData() throws Xcept
    {
	Table nTable = graph.getNodeTable();
	useSavedLayout = nTable.canSetFloat(GBGraph.X_COORD);
	this.vg = vis.addGraph(GRAPH, graph);   
	this.vis.setInteractive("graph.edges", null, false);
	if( userGraphParams.collapseVars == true ) //currently only collapse same 'variable'
	    { 
		if( userGraphParams.graphStyle == GBParms.VARIABLES ) // Transform graph if a VARIABLES graph
		    {
			GBTransformData modifyData = new GBTransformData(graph, true);
		    }
	    }
 	// Add column for circular dependencies:
	Table edgeVTable= graph.getEdgeTable();
	if( edgeVTable.getColumnNumber(CIRC) == -1  )
	    {
		edgeVTable.addColumn(CIRC,String.class, "false");
	    }
	// Only check for circular depend if not collapsing variables. 
	if( (userGraphParams.collapseVars == false) || (userGraphParams.graphStyle == GBParms.SEQUENCE ))
	    edgeVTable = findCircularDependencies(edgeVTable);
	// ***************************************************
	// Aggregate stuff:
	Table v_nTable=vg.getNodeTable();    
        GBAggregateTable setAggTable = new GBAggregateTable(vis, AGGR, userGraphParams.groupBy);  
	setAggTable.setUpAggregateTable( this.graph.getNodeTable(),v_nTable, this.vg);

	// -- The renderers and renderer factory ---------------------------
	DefaultRendererFactory rf= new DefaultRendererFactory() ;
	// Set edges:
        EdgeRenderer newEdge = new EdgeRenderer(Constants.EDGE_TYPE_LINE, Constants.EDGE_ARROW_FORWARD); 
	newEdge.setArrowHeadSize(8,8);    // Set arrow size (pixels h x w)
  
	// Renderer used for Aggregate grouping (to display 'nested graphs')
	Renderer polyR=new PolygonRenderer(Constants.POLY_TYPE_CURVE);
	((PolygonRenderer)polyR).setCurveSlack(0.15f);

        // Two different renderers based on bool values of graphParams.nodeShape
	LabelRenderer lRound = new LabelRenderer( userGraphParams.nodeText);
        lRound.setRoundedCorner(20, 100); // round the corners 
	lRound.setHorizontalPadding(5);

	LabelRenderer lSqr = new LabelRenderer(userGraphParams.nodeText);
	lSqr.setHorizontalPadding(5);

	TableIterator nodeRowchk = new TableIterator(v_nTable,v_nTable.rows()); // Used to check if data field really exists.

	if(  userGraphParams.nodeShape == null )
	    {
		rf.setDefaultRenderer(lRound);
	    }
	else{
	    if(nodeRowchk.canGetString(userGraphParams.nodeShape)==false)
		    {
			rf.setDefaultRenderer(lRound);
		    }
	    else {
		    rf.add("ingroup('graph.nodes') and "+userGraphParams.nodeShape+"=TRUE", lRound);
		    rf.add("ingroup('graph.nodes') and "+userGraphParams.nodeShape+"=FALSE", lSqr);  
	    }
	}    
               
	rf.add("ingroup('aggregates')",polyR);
	rf.setDefaultEdgeRenderer(newEdge);
	rf.add(new InGroupPredicate(AGGR_DECORATORS), new LabelRenderer("aggName"));
	vis.setRendererFactory(rf);
        
        DECORATOR_SCHEMA.setDefault(VisualItem.TEXTCOLOR, ColorLib.gray(50, 128));
        DECORATOR_SCHEMA.setDefault(VisualItem.FONT, FontLib.getFont("Tahoma", Font.BOLD, 16));
        vis.addDecorators(AGGR_DECORATORS, AGGR, DECORATOR_SCHEMA);

        // The processing actions ---------------------------------------         
	int transparency = 75;    // Set alpha, 255 is opaque (transparency for aggregate coloring).
	int[] paletteAGGR = new int[] {
            ColorLib.rgba(255,100,100,transparency),
            ColorLib.rgba(100,255,100,transparency),
            ColorLib.rgba(100,100,255,transparency),
	    ColorLib.rgba(100,200,255,transparency),
	    ColorLib.rgba(255,100,200,transparency),
	    ColorLib.rgba(200,255,100,transparency),
	    ColorLib.rgba(200,10,100,transparency),
	    ColorLib.rgba(10,100,200,transparency),
	    ColorLib.rgba(100,200,10,transparency)
        };

	int[] paletteDefault = new int[] { ColorLib.rgb(255,100,100)};
        // map nominal data values to colors using our provided palette
        // Primary coloring:
        // Chose color based on 'nodeFillColor'. Colors used are default:
	DataColorAction fill;   // Node fill color
	if( userGraphParams.nodeFillColor !=null)
	    {
		if(nodeRowchk.canGetString( userGraphParams.nodeFillColor)==false)
		    {
			fill = new DataColorAction("graph.nodes", GBParms.NAME,
						   Constants.NOMINAL, VisualItem.FILLCOLOR, paletteDefault);	
		    }
		else {
		      fill = new DataColorAction("graph.nodes",  userGraphParams.nodeFillColor,
				       Constants.NOMINAL, VisualItem.FILLCOLOR);
		}
	    }
	else {  // DEFAULTS
	   
		 fill = new DataColorAction("graph.nodes", GBParms.NAME,
					       Constants.NOMINAL, VisualItem.FILLCOLOR, paletteDefault);
	    }	  
	// Border (stroke) coloring: 
	DataColorAction stroke;
	if( (userGraphParams.nodeBorderColor !=null) && (nodeRowchk.canGetString( userGraphParams.nodeBorderColor)==true) )
	    {		
		stroke = new DataColorAction("graph.nodes",  userGraphParams.nodeBorderColor,
					     Constants.NOMINAL, VisualItem.STROKECOLOR);
	    }
	else{  // Use same color as fill if no border specified:
	    if(nodeRowchk.canGetString( userGraphParams.nodeFillColor)==false)  {
			stroke = new DataColorAction("graph.nodes", GBParms.NAME,
						   Constants.NOMINAL, VisualItem.STROKECOLOR, paletteDefault);	
		    }
	    else {
		        stroke = new DataColorAction("graph.nodes", userGraphParams.nodeFillColor,
						   Constants.NOMINAL, VisualItem.STROKECOLOR);
	         }
	}	
        // use black for node text
        ColorAction text = new ColorAction("graph.nodes",
                VisualItem.TEXTCOLOR, ColorLib.gray(0));
	
        // use grey with transparency for edges, Red if 'circular' edge:
        ColorAction edges = new ColorAction("graph.edges", VisualItem.STROKECOLOR);
	edges.setDefaultColor(ColorLib.gray(0,100));
	edges.add("ingroup('graph.edges') and "+CIRC+" ='true'",ColorLib.rgb(255,0,0));
       
	// use grey for edge fill (needed for arrows):
	ColorAction edgeArrow = new ColorAction("graph.edges", VisualItem.FILLCOLOR);
	edgeArrow.setDefaultColor(ColorLib.gray(0,100));
	edgeArrow.add("ingroup('graph.edges') and "+CIRC+" ='true'",ColorLib.rgb(255,0,0));
	         
	// Aggregate colors:
	ColorAction aStroke = new ColorAction(AGGR, VisualItem.STROKECOLOR);
        aStroke.setDefaultColor(ColorLib.gray(200));
              	
	ColorAction aFill = new DataColorAction(AGGR, "id",
                Constants.NOMINAL, VisualItem.FILLCOLOR, paletteAGGR);

	ActionList color =new ActionList();
	color.add(fill);
	color.add(stroke);  
	color.add(new StrokeAction("graph.nodes", new BasicStroke(2))); // double width of node border
        color.add(edges);
	color.add(edgeArrow);
        color.add(text);
	color.add(aStroke);
	color.add(aFill);
	color.add(new RepaintAction());
       
	//ActionList layout = new ActionList(1000,2);
	ActionList layoutList = new ActionList();
	Layout graphLayout;
	if( useSavedLayout )
	    { 
		//	System.out.println("Generateing SPecified Layout.");
		graphLayout = new SpecifiedLayout( "graph",GBGraph.X_COORD,GBGraph.Y_COORD);
	    }
	else {
	    graphLayout = new FruchtermanReingoldLayout("graph",20);
	} 
	layoutList.add(graphLayout);

	GBAggregateLayout aggLayout = new GBAggregateLayout(AGGR);
	GBLabelLayout aggLabel = new GBLabelLayout(AGGR_DECORATORS);
	layoutList.add(aggLayout);
	layoutList.add(aggLabel);
	layoutList.add(new RepaintAction());
        
	// Update aggregate based on mouse dragging a node
	ActionList updateAgg = new ActionList();
	updateAgg.add(aggLayout);
	updateAgg.add(aggLabel);
	updateAgg.add(new RepaintAction());

        // add the actions to the visualization:
	vis.putAction("color", color);
        vis.putAction("layout", layoutList);
	vis.putAction("updateAgg",updateAgg);
       
        // -- display and interactive controls ------------------
        // create a new window to hold the visualization
       	// Set the Display size:

	Dimension panelSize = new Dimension();
	panelSize = panel.getSize();

	int xWidth =0;
	int yHeight =0;
	xWidth =(int)panelSize.getWidth();
	yHeight=(int)panelSize.getHeight();
	Display d = new Display(vis);
	if(xWidth ==0)
	    {
		xWidth = MIN_WIDTH;
	    }
	if(yHeight ==0)
	    {
		yHeight = MIN_HEIGHT;
		d.setSize(xWidth, yHeight);
	    }
	else {
	    d.setSize(panelSize); // set display size
	}
	Point anchorCoor = new Point(-xWidth,-yHeight);  // Center the graph in the display    

	double zoomMagnitude = 1.5;
	int perferredNodeCount = 75;
	int nodeCount=graph.getNodeCount();
        if (nodeCount > perferredNodeCount)
	    {
		zoomMagnitude = zoomMagnitude*perferredNodeCount/nodeCount;
	    }
	// Scale zoom based on number of nodes and panel size
        d.zoom(anchorCoor,zoomMagnitude);  // Set default zoom- the anchor point for the zoom, in screen coord 
        // pan with left-click drag on background
        d.addControlListener(new PanControl()); 
        // zoom with right-click drag
        d.addControlListener(new ZoomControl());
	// drag individual items around
	d.addControlListener(new GBAggregateDragControl("updateAgg"));
	// Add tooltip popup for nodes:
        d.addControlListener(new GBToolTipControl(userGraphParams.graphStyle));
        // -- launch the visualization --     
        // Use panel window to hold the visualization
        panel.add(d);                
        // assign the colors
	vis.run("color"); 
        // start up the animated layout
        vis.run("layout");     
    }

 
   // Preserve current coordinates of nodes from visual table to PModelBrowserLayout object
    public void getNodeCoordinates(PModelBrowserLayout currentGraphLayout)throws Xcept {
	// Need delay before get XY data:??
	String s ="";
	float xWidth =0f;
	float yHeight =0f;
	Dimension panelSize = new Dimension();
	panelSize = this.panel.getSize();
	xWidth =(float)panelSize.getWidth();
	yHeight=(float)panelSize.getHeight();
	for (int i = 0; i< 10000; i++){ s+= i; }

	// TODO: CHeck if graphID already exists, if so then update.
	PModelBrowserLayout.Graph newNodeList = currentGraphLayout.addNewGraph(userGraphParams.graphID);  // Is it variable, sequence, etc....
	//System.out.println("** GraphID: "+userGraphParams.graphID );
        Visualization vis = vg.getVisualization();
	Iterator iter = vis.items("graph.nodes");
	    while( iter.hasNext() )
		{
		    String nodeName = "";
		    int rowNumber =0;
		    // Point2D.Float nodeCoord = new Point2D.Float(0,0);
		    VisualItem nodeItem = (VisualItem)iter.next();
		 	  
		    if(nodeItem.canGet("name",nodeName.getClass()))
			{
			    nodeName=(String)nodeItem.get("name");
			    float xCoord = (float)nodeItem.getX();
			    // System.out.println("xWIdth:"+xWidth+" xCoord:"+xCoord);
			    xCoord = xCoord/xWidth;
			    // System.out.println(" xCoord:"+xCoord);
			    newNodeList.putXY(nodeName,xCoord,((float)nodeItem.getY()/yHeight));
			    //System.out.println("FOUND node name column: "+nodeName);
			}
		    else {
			System.out.println("Cannot find node name column! ");
		    }		   

		}
    }

   

    // *****************************************************
    // Find all edges that create a circular dependencies between nodes.
    // Store this list of edges in HashSet and then go set the CIRC datafield 
    // for each edge to 'true'
    // *****************************************************
    private Table findCircularDependencies(Table edgeTable)
    {
	HashSet <Object> circEdges = new HashSet <Object>();
	int totalEdges = edgeTable.getRowCount();
	for( TableIterator edgeRow = new TableIterator(edgeTable,edgeTable.rows()); edgeRow.hasNext(); )
	    {
		int homeNode =0;
		int edgeIndex =0;
		int numberEdges =0;
		HashSet <Object> nodesWalked = new HashSet <Object>();
		edgeIndex = edgeRow.nextInt();
		if(edgeRow.canGetInt("source"))
		    {
			int targetNode =0;
			homeNode = edgeRow.getInt("source");
			if(edgeRow.canGetInt("target"))
			    {
				targetNode = edgeRow.getInt("target");			
				if(targetNode !=homeNode)
				    {
					if( findNode(homeNode, edgeIndex, edgeTable, circEdges,nodesWalked)== true)
					    {
						// set this edge circular value to 'true'
						circEdges.add(edgeIndex);					
					    }
				    }
			    }
		    }
	    }
	setCircularDependencies(edgeTable, circEdges);
	return edgeTable;
    }


    // ***********************************************
    // Recursive function. Finds target node of given edge
    // and checks to see if it is home node of current
    // search for circular dependencies.
    // ***********************************************
    private Boolean findNode(int homeNode, int edge, Table edgeTable, HashSet <Object> hs, HashSet <Object> nodesWalked) 
    {
	int oldSrcNode =0;
	int targetNode =0;		
	oldSrcNode = edgeTable.getInt(edge,"source");
	targetNode = edgeTable.getInt(edge,"target");
	if(nodesWalked.contains(targetNode))
	    {
		return false;  
	    }
	else {
	    nodesWalked.add(targetNode);
	}
	for( TableIterator edgeRow = new TableIterator(edgeTable,edgeTable.rows()); edgeRow.hasNext(); )
	    {
		int newTarget =0;
		int edgeIndex = edgeRow.nextInt();	
		if(edgeRow.canGetInt("source"))
		    {		
			if(targetNode == edgeRow.getInt("source"))
			    {				
			// WHat happens if newTarget is the same as Target?
				newTarget = edgeRow.getInt("target");						
				if(newTarget == targetNode)  
				    {
					if( !hs.contains(edgeIndex) )
					    hs.add(edgeIndex);  
					return false; 
				    }
				// Case where nodes are each others target
				if(newTarget == oldSrcNode)
				    {
					if( !hs.contains(edgeIndex) )
					    hs.add(edgeIndex);
					if( !hs.contains(edge) )
					    hs.add(edge);
					return false;   
				    }
				if(newTarget == homeNode)
				    {
					// make node circular column 'true' for node
					if( !hs.contains(edgeIndex) )
					    hs.add(edgeIndex);			
					return true;
				    }
				else {
				    if(findNode(homeNode, edgeIndex, edgeTable, hs, nodesWalked) == true) 
					{
				   // make node circular column 'true' for node
					    if( !hs.contains(edgeIndex) )
						hs.add(edgeIndex);	     
					    return true;
					}				     
				}
			    }

		    }
	    }
	return false;

    }
   

    // **********************************************************************
    // Takes a HashSet containing all edges that reveal circular dependencies
    // and notes this in the edge table associated with current graph.
    // **********************************************************************
    private void setCircularDependencies(Table eTable, HashSet <Object> edgeSet)
    {
	int count = eTable.getRowCount();
	//System.out.println("Number of circular edges: "+edgeSet.size());
	for(int i=0; i< count; i++)
	    {
		if( edgeSet.contains(i))
		    {
			if(eTable.canSetString(CIRC))
			    {
				eTable.setString(i ,CIRC, "true");
			    }
		    }
	    }
    }
 

    // **********************************************************
    // Get row number, row name pairs for later use in changing source and target nodes in edge table.
    // (node name, node#) pairs
    // **********************************************************
    private HashMap get_TableMap(Table table)
	{
	    HashMap <Object,Object> hm = new HashMap <Object,Object>();
	    int rIndex =0;
	    for( TableIterator nodeRow = new TableIterator(table,table.rows()); nodeRow.hasNext(); )
	    {		   	   
		rIndex = nodeRow.nextInt();
		if(nodeRow.canGetString("name") )
		{// Get model variable name .
		    hm.put( nodeRow.getString("name"),rIndex);
		}		   
	    }	   
	    return hm;
	}


    // ****************************************************************
    // Set label positions. Labels are assumed to be DecoratorItem instances,
    // decorating their respective nodes. The layout simply gets the bounds
    // of the decorated node and assigns the label coordinates to the center
    // of those bounds.
    // ****************************************************************
private class GBLabelLayout extends Layout {
    String group;
    public GBLabelLayout(String groupLayout) {
        super(groupLayout);
	group = groupLayout;
    }
    public void run(double frac) {
        Iterator iter = vis.items(group);
        while ( iter.hasNext() ) {
            DecoratorItem decorator = (DecoratorItem)iter.next();
            VisualItem decoratedItem = decorator.getDecoratedItem();
            Rectangle2D bounds = decoratedItem.getBounds();
            double x = bounds.getCenterX();
            double y = bounds.getCenterY();          
            setX(decorator, null, x);
            setY(decorator, null, y);
        }
    }
} // end

}  // END of Class RenderData()










	
