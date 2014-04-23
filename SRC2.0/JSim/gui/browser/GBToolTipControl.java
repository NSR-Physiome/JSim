/*NSRCOPYRIGHT
	Copyright (C) 1999-2010 University of Washington
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

import prefuse.Constants;
import prefuse.Display;
import prefuse.controls.*;
import prefuse.util.*;
import prefuse.util.ui.*;
import prefuse.visual.*;

// ****************************************************
// * Tooltip Control"
// ****************************************************
class GBToolTipControl extends ControlAdapter {
 
    int graphStyle;

    // constants
    public static final  int LINE_WIDTH = 40;
    public static final String HTML_BREAK = "<br>";
    public static final String REAL_DOMAIN = "realDomain";
    public static final String DOMAIN = "domain";
    public static final String PROPERTY = "property";

    // constructor
    public GBToolTipControl(int gStyle)
    {
	graphStyle = gStyle;
    }

    public void itemEntered(VisualItem item, MouseEvent e)
	{   
	    String textList ="<html>";   // Make OS independent by using HTML.

	    // All the things we may want to display in tool tip (order in list important):
	    //String [] tipList ={GBParms.NAME, GBParms.ITEM_TYPE, GBParms.PHASE,GBParms.ITEM_TEXT};
	    Display disp = (Display)e.getSource();


	    if( graphStyle == GBParms.VARIABLES )
		{
		    textList = getVarGraphInfo( item, textList );
		   
		}

	    else 
		{
		    if( graphStyle == GBParms.SEQUENCE )
			{
			    textList = getSeqGraphInfo( item, textList );
			}
		}

	    textList=textList+"</html>";
	    // TODO: check if nothing for tooltip to display (mouse over aggregate)
	   
	    disp.setToolTipText(textList);
	    
	}

    public void itemExited( VisualItem item, MouseEvent e)
	{
	    Display disp = (Display)e.getSource();
	    disp.setToolTipText(null);
	}
	   


    // ******************************************************
    // Get node data from variable graph and display in tooltip
    // ******************************************************
    private String getVarGraphInfo( VisualItem item, String varText )
    {
	String newLine = "";   // First line
	if(item.canGetString(GBParms.IS_PRIVATE))
	    {
		if(item.getString(GBParms.IS_PRIVATE) == "true")
			newLine = newLine + "private ";
	    }
	if(item.canGetString(GBParms.IS_INPUT))
	    {
		if(item.getString(GBParms.IS_INPUT) == "true")
			newLine = newLine + "Input ";
		else {
			newLine = newLine + "Output ";
		    }
	    }
	 if(item.canGetString(GBParms.DOMAINS))
	     {
		 if(item.canGetString(GBParms.DATA_TYPE))
		     {
			 if(item.getString(GBParms.DATA_TYPE)!=REAL_DOMAIN)
			     {
				 String newDomain = null;
				 newDomain = item.getString(GBParms.DOMAINS);
				 if( newDomain != null)
				     {
					 newLine=newLine + newDomain +" ";
				     }
			     }
		     }
	     }

	 if(item.canGetString(GBParms.NAME))
	     {
		 newLine = newLine + item.getString(GBParms.NAME)+ " ";
	     }
	 if(item.canGetString(GBParms.UNIT))
	     {
		 String newUnit = null;
		 newUnit = item.getString(GBParms.UNIT);
		 if( newUnit != null)
		     {
			 newLine = newLine + newUnit;
		     }
	     }

	 varText= varText+ checkLineLength(newLine) + HTML_BREAK;

	 newLine ="";    // Start second line
	 //  Put in hasProperty.name Property here:	
	 newLine = getPropertiesText( item, newLine);
	 varText= varText + newLine;

	 newLine = "";   // Start third line
	 if(item.canGetString(GBParms.EVENT_TEXT))
	     {
		 String newEvent = null;
		 newEvent = item.getString(GBParms.EVENT_TEXT);
		 if(newEvent != null)
		     {
			 newLine = newLine+ "Event: "+ newEvent;
			 varText= varText+ checkLineLength(newLine) + HTML_BREAK;
		     }
	     }

	 newLine = "";   // Start fourth line
	 if(item.canGetString(GBParms.TOOL_TYPE))
	     {
		 if(item.canGetString(GBParms.TOOL_TEXT))
		     {
			 String newText = null;
			 newText = item.getString(GBParms.TOOL_TEXT);
			 if( newText != null )
			     {
				 newLine = newText;
				 newLine= checkLineLength(newLine);
				 newLine= newLine.replaceAll(";", HTML_BREAK);
			     }
		     }

		 varText = varText + newLine;
	     }


	return varText;
    }


    // ******************************************************
    // Check node data for 'property.##' field and get text for display in tooltip
    // Each node may have more than one property field associated with it.
    // ******************************************************
    private String getPropertiesText( VisualItem item, String varText )
    {
	int totalColumns = item.getColumnCount();
	for( int i =0; i<totalColumns; i++ )
	    {
		String columnName = item.getColumnName(i);
		if( columnName.contains(PROPERTY) )
		    {
			int propIndex = columnName.indexOf('.');
			String propertyName = null;
			propertyName = columnName.substring(propIndex +1);
			if( propertyName !=null)
			    {
				//	varText = varText + propertyName+ ": ";
				if( item.canGetString(columnName) )
				    {
					String propDesc =null;
					propDesc = item.getString(columnName);
					if( propDesc != null)
					    {
						varText = varText+ propertyName+ ": "+ checkLineLength(propDesc);
						varText = varText + HTML_BREAK;
					    }
					//System.out.println("Property: "+varText);
				
				    }

			    }


		    }



	    }
	return varText;
    }

    // ******************************************************
    // Get node data from Sequence graph and display in tooltip
    // ******************************************************
    private String getSeqGraphInfo( VisualItem item, String varText )
    {
	String newLine = "";   // First line
	if(item.canGetString(GBParms.NAME))
	     {
		 newLine = newLine + item.getString(GBParms.NAME)+ " ";
	     }
	
	if(item.canGetString(GBParms.PHASE))
	    {
		String newText = null;
		newText = item.getString(GBParms.PHASE);
		if( newText != null)
		    {
			newText = newText.replaceAll("#","");
			newLine = newLine + "Phase: "+newText;
		    }
		
	    }

	varText = varText + newLine + HTML_BREAK;

	newLine = "";         // Second Line
	if(item.canGetString(GBParms.ITEM_TYPE))
	     {
		 String iType = null;
		 iType = item.getString(GBParms.ITEM_TYPE); 
		 if( !iType.equals(DOMAIN) )
		     {
			 newLine = iType +" ";
		     }
		 if(item.canGetString(GBParms.ITEM_TEXT))
		     {
			 String newText = null;
			 newText = item.getString(GBParms.ITEM_TEXT);
			 if( newText != null )
			     {
				 newLine = newLine + newText;
				 newLine= checkLineLength(newLine);
				 newLine= newLine.replaceAll(";", HTML_BREAK);
			     }
		     }

		 varText = varText + newLine;
	     }

	return varText;
    }

    // **********************************************
    // 
    private String checkLineLength( String line )
    {
	if(line.length() > LINE_WIDTH)
		{
		    int strLength = line.length();
		    int totalLines = (strLength/LINE_WIDTH)+1; // div rounds down
		    String textWidth = "";     // holds string with breaks every 'LINE_WIDTH'
		    for(int z=0;z<totalLines;z++)
			{
			    if(z == totalLines-1)
				{
				    textWidth=textWidth + line.substring(z*LINE_WIDTH);
				}
			    else {
				textWidth = textWidth + line.substring(z*LINE_WIDTH,((z+1)*LINE_WIDTH))+ HTML_BREAK;
			    }
			}
		    line = textWidth;
		}

	return line;
    }


}  // END Class ToolTipControl

