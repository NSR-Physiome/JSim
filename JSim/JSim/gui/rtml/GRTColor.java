/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// interface color

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

public class GRTColor extends GRTNode {

	// state
	private String name;
	private String rgb;

	// constructor
	public GRTColor(GRTNode p, Element e) {
	    super(p);
	    name = e.getAttribute("name");
	    rgb = e.getAttribute("rgb255");
	    addXMLChildren(e);
	}
}

