/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// one variable in a page

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;
import JSim.project.*;
import JSim.gui.*;

abstract public class GRTVar extends GRTPageItem {

	// state
	private String name;
	private Control cntl;  // control if input
	private PModel pmodel;
	private GControl gcntl;  // gcontrol if input
	private ASQuery expr;	// input or output expr
	private boolean isArray; // is multi dim (for output)
	private JComponent jvalue; // value component
	private JTextField junit; // units component, if any

	// constructor
	public GRTVar(GRTNode n, Element e) {
	    super(n, e);
	    name = e.getAttribute("name");
	    addXMLChildren(e);
	    pmodel = gmodel().pmodel();
	}

	// connect and make widgets
	public void makeStuff(boolean units) {
	    cntl = pmodel.vars().control(name);
	    ASModel rt = pmodel.rt();
	    try {
		expr = rt.parseQuery(name);
	    } catch (Xcept e) {
		expr = null;
   	    }

	    // create jval value component
	    jvalue = null;
	    if (isInput()) {
		GRTInputVar ivar = new GRTInputVar(this, expr);
		gcntl = ivar.gcntl();
		gcntl.addAuxNode(page());
		jvalue = ivar.jvalue();
	    } else if (isOutput()) {
	    	JTextField text = new JTextField();
		text.setBackground(glook().dark());
		text.setEditable(false);
	    	jvalue = text;
		if (expr instanceof ASVar)
		    setHelp(jvalue, this);
		GControl.cursorTraversal(jvalue);
	    } else {
	    	JTextField text = new JTextField();
		text.setBackground(glook().dark());
		text.setEditable(false);
		text.setText("[missing]");
	    	jvalue = text;
		GControl.cursorTraversal(jvalue);
	    }	    

	    // unit component
	    if (units) {
		String u = unitString();
		if (u == null) u="";
	        junit = new JTextField(u);
		junit.setBackground(glook().dark());
		junit.setEditable(false);
		junit.setCaretPosition(0);
	    }
	}

	// refresh page
	public void refresh() {
	    super.refresh();
	    if (! isOutput()) return;
	    if (pmodel.rt().nstores()>0 && !pmodel.isRunning())
		refreshOutput();
	}
		
	// refresh output
	public void refreshOutput() {

	    // is output scalar with runtime?
	    String sval = null;
	    try {
		ASModel rt = gmodel().pmodel().rt();
		ASVar v = ((ASVar) expr);	
		double val = v.finalRealVal();
//		sval = Util.pretty(val, false);
		sval = PrettyFormat.sformat(val, 5);
	    } catch (Exception e) {
		sval = "???";
   	    }	
	    
	    // show data in field
	    JTextField text = (JTextField) jvalue;
	    text.setText(sval);
	    text.setCaretPosition(0);
	}

	// query
	public String name() { return name; }
	public String label() { 
	    if (text() != null) return text(); 
	    if (expr != null) {
		if (expr instanceof ASVar) 
		    return parenName((ASVar) expr);
		else
		    return expr.toString();
	    }
	    if (cntl != null) 
		return cntl.name();
	    return name;
	}
	public boolean isInput() { return cntl != null; }
	public boolean isOutput() { 
	    return !isInput() && expr != null; 
	}
	public JComponent jvalue() { return jvalue; }
	public JTextField junit() { return junit; }
	public String unitString() {
	    if (expr == null) return null;
	    return expr.unitString();
	}
	protected Control cntl() { return cntl; }

	// key to help database (outputs only, input use GControl)
	public String helpKey() {
	    if (! (expr instanceof ASVar))
		return null;
	    ASVar v = (ASVar) expr;
	    return "model" + GHelp.sep +
		gmodel().pmodel().name() + 
		GHelp.sep + v.name();
	}
}

