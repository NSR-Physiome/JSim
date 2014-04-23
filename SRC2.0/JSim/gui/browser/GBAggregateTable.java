/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.gui.browser; 
import JSim.util.*;

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


   // ***********************************************************
    // Divide up all nodes into groups and add to a aggregate table
    // for displaying together w/in an individual color field.
    // Some nodes may not be a part of any group.
    //
    // Breakdown phase name into components based on delimiter '+'
    // Then add nodes to more than one phase as needed.
    // Ex: node 2 phaseName is t#1+x#1 then put in the t#1 and x#1 agg groups.
    // TODO: make this code work for any grouping user wants. Currently only
    //     aggregates according to phases in a sequence graph.
    // ***********************************************************

public class GBAggregateTable {

	AggregateTable at;
	java.lang.String groupType;

	GBAggregateTable( Visualization vis, java.lang.String group, java.lang.String groupType  ) throws Xcept
	{
		this.groupType = groupType;		
		this.at = vis.addAggregates(group)	;  // AGGR
 		this.at.addColumn(VisualItem.POLYGON, float[].class);
        	this.at.addColumn("id", int.class);
        	this.at.addColumn("aggName", String.class);    // Used for labeling Groups/Phases


	}	

	public AggregateTable getAggTable()
	{
		return this.at;
	}

// TODO: CHange 'phase' to 'group' for more generic instance.
    protected void setUpAggregateTable( Table nTable, Table v_nTable, VisualGraph vg )
    {
	int nodeIndex =0;
	int groupNumber =0;
        String delimiter ="\\+";
  
	//Table nTable = graph.getNodeTable();
	if( groupType == null || groupType == GBParms.PHASE)
	    {
		groupType = GBParms.PHASE;  // Aggregate grouping by phases
	    }

	HashMap <Object,Object> groupNames = new HashMap <Object,Object>(); // Holds groups and their names (groupName)

        int groupID =0; 
	for( TableIterator nodeRow = new TableIterator(nTable,nTable.rows());nodeRow.hasNext();)
	    {	   
		String groupName = "";	   	   
		nodeIndex = nodeRow.nextInt();

		if(nodeRow.canGetString(groupType) )
		    {		       
			groupName=nodeRow.getString(groupType);
				   
			if(groupName != null)
			{
			    // split groupName on '+'
			    // add to list the different groupNames for the single node.
			    String[] group_List = null;
			    if(groupType == GBParms.PHASE)   // Phases may have multiple names
				{
				   group_List =groupName.split(delimiter);
				}
			    else group_List = groupName.split(delimiter,0);     // Only one group name

			    String previousPhase = "";
			  
			    for(int i =0; i< group_List.length; i++)
				{// now just concatenate them together to create phases, excluding last one.
				    String newPhase = "";
				    if( previousPhase.equals("") )
					{
					    if( !groupNames.containsValue(group_List[i]))
						{
						    groupNames.put(groupID,group_List[i]);
						    groupID++;
						}
					    previousPhase = group_List[i];
					}
					
				    else 
					{
					    if(groupType == GBParms.PHASE)
					    {
					       newPhase = previousPhase + "+" +group_List[i];
					    }
					    else {
					       newPhase = previousPhase;
				  	    }
					    
					    if( !groupNames.containsValue(newPhase))
						{
						    groupNames.put(groupID,newPhase);
						    groupID++;
						}
					    //groupID++;
					    previousPhase = newPhase;
					}

				}
			    
			}
			    
		    }
		   
	    }

	nodeIndex =0;

	// Now add nodes to appropriate group names w/in aggregate table:
	for( groupNumber  =0; groupNumber< groupNames.size(); groupNumber++ )
	    {
		AggregateItem aitem = (AggregateItem)at.addItem();
		aitem.setInt("id", groupNumber);
		String newName = (String)groupNames.get(groupNumber);
		String pName = new String(newName);
		pName  = pName.replaceAll("#","");
		aitem.setString("aggName",pName);  // Keep track of name of group

		// Next add all nodes with the associated phase name to the group:
		for( TableIterator nodeRow = new TableIterator(v_nTable,v_nTable.rows());nodeRow.hasNext();)
		    {
			nodeIndex =nodeRow.nextInt();

			// If group name is in node info then add it to aggregate item:
			if( nodeRow.canGetString(groupType) )
			    {
				String nodegroupName = nodeRow.getString(groupType);
				if( (nodegroupName!=null) && (isInGroupName(nodegroupName, newName)) )
				    {
					// then add node to aggregate item:
					aitem.addItem((VisualItem)vg.getNode(nodeIndex));		   
				    }
			    }
		    }


	    }


    }



   // ********************************
    // Takes searchName and checks to see if groupName is contained w/in it.
    // Special: Names to search for are left precedent only.
    // Example: searchName = t1+x2+y1
    // Look for groupNames that match t1+x2+y1, t1, t1+x2 only.
    // *********************************
    protected Boolean isInGroupName(String searchName, String groupName)
    {
	String delimiter = "\\+";
	if( searchName.equals(groupName) )
	    {
		return true;
	    }
	if( searchName.contains(groupName) )
	    {
		String [] name_List;
	
		String test_Name="";
		name_List = searchName.split(delimiter);
		for (int i=0;i<name_List.length;i++)
		    {
			test_Name=test_Name+name_List[i];
			if(test_Name.equals(groupName))
			    {
				return true;
			    }
			test_Name=test_Name+"+"; // groupName has '+' in it
		    }

	    }
	return false;

    }


}
