/*NSRCOPYRIGHT
	Copyright (C) 1999-2010 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.gui.browser;
 
import JSim.util.*;
import java.util.*;

import prefuse.data.Graph;
import prefuse.data.Table;
import prefuse.data.event.EventConstants;
import prefuse.data.io.DataIOException;
import prefuse.data.io.GraphMLReader;
import prefuse.util.collections.*;
import prefuse.data.util.TableIterator;
import prefuse.data.tuple.TableNode;
import prefuse.data.tuple.TableTuple;
import prefuse.data.tuple.TableEdge;


// Currently takes a Graph object and gets rid of 'redundant' nodes 
// and edges
// 'redundant' is defined as node that has key 'variable' indicating that
// it is directly related to another variable (node)

public class GBTransformData {

    private Graph m_transformGraph =null;
    
    private TableNode m_nodeCheck;
    private String m_dataKey ="variable";
    private String m_nodeName ="name";
    private HashMap <Object,Object> m_nTableMap;
    private HashMap <Object,Object> m_nodeReplaceList;
   
    // List of node data key types (assume node id is same as data key type "name"):
  
    // Assume edges have just 'source' and 'target'
    // Assume datafield of node to collapse depends on dat key 'variable' existing. 

    GBTransformData( Graph graph, boolean collapseVars )throws Xcept
    {
     
	m_transformGraph = transformGraph(graph);
    }


    // ******************************************

    // ******************************************
    private Graph transformGraph(Graph graph) throws Xcept
    {

	Table nTable = graph.getNodeTable();
	Table eTable = graph.getEdgeTable();
	m_nTableMap = get_TableMap(nTable);
	m_nodeReplaceList = new HashMap<Object,Object>();

	int nodeRemoveIndex =0;    // Used to get correct edge deleted....
	try {
	        
            
		    for( TableIterator nodeRow = new TableIterator(nTable,nTable.rows());nodeRow.hasNext();)
		    {
		   
			String varDup = "";
			String varRemove = "";
		   	   
			nodeRemoveIndex = nodeRow.nextInt();

			if(nodeRow.canGetString(m_dataKey) )
			    {
				// Get model variable name to remove:
				varDup=nodeRow.getString(m_dataKey);
				varRemove = nodeRow.getString(m_nodeName); 	  
			   
				if(varDup != null)
				{//  pair: (node# to remove, node# to replace) 
				    m_nodeReplaceList.put(nodeRemoveIndex,m_nTableMap.get(varDup));
				 
				}
			    
			    }
		   
		    }
	   
		    

	} catch(ArrayIndexOutOfBoundsException e){  // need better exception handling.
		     System.out.println("Exception!: "+e );
			throw new Xcept("Node iterator problem");
		  }
       
	eTable = editEdgeSrcDest( eTable );

	nTable = removeNodes(nTable);
	eTable = removeExtraEdges(eTable);
	return graph;
    }

    public Graph getGraph()
    {
	return m_transformGraph;
    }

  

    // **********************************************************
    // Get row number, row name pairs for later use in changing source and target nodes in edge table.
    // (node name, node#) pairs
    // **********************************************************
    private HashMap <Object,Object> get_TableMap(Table table)
	{
	    HashMap <Object,Object> hm = new HashMap<Object,Object>();
	    int rIndex =0;
	    for( TableIterator nodeRow = new TableIterator(table,table.rows()); nodeRow.hasNext(); )
	    {		   	   
		rIndex = nodeRow.nextInt();
		if(nodeRow.canGetString(m_nodeName) )
		{// Get model variable name .
		    hm.put( nodeRow.getString(m_nodeName),rIndex);
		}
		   
	    }
	   
	    return hm;
	}


    // *****************************************

    // *****************************************
    private Table editEdgeSrcDest(Table eTable)
    {
	int edge_row = 0;
	int rowCount = eTable.getRowCount();
        
	TableIterator edgeRow = new TableIterator(eTable,eTable.rows());
	while( edgeRow.hasNext() )
	{
	    edge_row = edgeRow.nextInt();
	   
	    if(edgeRow.canGetInt("source"))
	    { 
		int edgeSource = edgeRow.getInt("source");
		//	System.out.println("edge SRC:"+edgeSource);
       
		if(m_nodeReplaceList.containsKey(edgeSource))
		{
		    if(edgeRow.canSetInt("source"))
		       {   
				Integer c = (Integer)m_nodeReplaceList.get(edgeSource);
				//	System.out.println("Row: "+edge_row+", Going to Replace EDGE SRC! with:"+c);
				int chk_val = eTable.getInt(edge_row,"source");					
				edgeRow.setInt("source",c);  
		    	}				    
	  				     
						    
		}
	    }

	    if (edgeRow.canGetInt("target") )
	    {
		int edgeTarget = edgeRow.getInt("target");
    
		if(m_nodeReplaceList.containsKey(edgeTarget))
		{
			if(edgeRow.canSetInt("target"))
			    {
				Integer c = (Integer)m_nodeReplaceList.get(edgeTarget);
			
				edgeRow.setInt("target",c);
				  
			    }    
						    
		}
	     }
	    else { System.out.println("edge - Cannot get Target !" );}
			
	}	
	
	return eTable;
    }


    // **********************************************
    // Remove nodes as specified in m_nodeReplaceList
    // **********************************************
    private Table removeNodes( Table nodeTable)
    {
	int nodeRemoveIndex =0;    // 
	int rowCount = nodeTable.getRowCount();

		try {
	        
            
		    for( TableIterator nodeRow = new TableIterator(nodeTable,nodeTable.rows());nodeRow.hasNext();)
		    
		    {
			nodeRemoveIndex = nodeRow.nextInt();
		
			if(m_nodeReplaceList.containsKey(nodeRemoveIndex))
			    {
				nodeRow.remove(); 			 
			    
			    }
		   
		    }
	   
		    

		    // *********************
	} catch(ArrayIndexOutOfBoundsException e){  
		     System.out.println("Exception!: "+e );
			System.exit(1);
			} 

	return nodeTable;
    }


    // **********************************************
    // Removes edges where the 'source' and 'target' are the same node.
    // **********************************************
    private Table removeExtraEdges(Table edgeTable)
    {
	int edgeRemoveIndex =0;    // 
	int rowCount = edgeTable.getRowCount();
	String sourceN = "source";
	String targetN = "target";

		try {
	        
            
		    for( TableIterator edgeRow = new TableIterator(edgeTable,edgeTable.rows());edgeRow.hasNext();) 
		    {
			int srcNode = 0;
			int targetNode =0;
	
			edgeRemoveIndex = edgeRow.nextInt();

			if(edgeRow.canGetString(sourceN))
			    {
				srcNode = edgeRow.getInt(sourceN);
				targetNode = edgeRow.getInt(targetN);
				//System.out.println("SRC #:"+ srcNode + "TARGET #: "+targetNode);
				if(srcNode == targetNode)
				    {
					edgeRow.remove(); 			 
				    }	
			    
			    }
		   
		    }
	   
		    

		    // *********************
		} catch(ArrayIndexOutOfBoundsException e){  
		     System.out.println("Exception!: "+e );
			System.exit(1);
			} 

		return edgeTable;
    } 


    // ********************************************************
    //  Utility: Useful check for debugging removal of rows from table....
    // ********************************************************
    private void checkTable(Table tableChk)
    {
	int index =0;
	for( TableIterator check = new TableIterator(tableChk,tableChk.rows());check.hasNext();)
	{
	    index = check.nextInt();
	    System.out.println("THIS TABLE Row #:"+ index );
        }

    }


    // ***********************************
    // Utility: Print out HashMap pairs
    // ***********************************
    private void printHashMap(HashMap hm)
    {

	// Print out has table
	    Set set = hm.entrySet();
	    System.out.println("Hash Map:");
	    Iterator i = set.iterator();
	    while(i.hasNext()){
		Map.Entry me = (Map.Entry)i.next();
		System.out.println(me.getKey() + " : " + me.getValue() );
	    }

    }

   

}