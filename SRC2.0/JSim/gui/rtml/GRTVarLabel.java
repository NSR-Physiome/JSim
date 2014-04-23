/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// clickable variable label

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
import JSim.project.*;
import JSim.gui.*;

public class GRTVarLabel extends JLabel implements MouseListener {
	private GRTVar v;	// variable

	// constructor
	public GRTVarLabel(GRTVar vv) {
	    super(vv.label());
	    v = vv;
	    addMouseListener(this);
	}

	// MouseListener methods
	public void mouseClicked(MouseEvent e) { }
	public void mouseEntered(MouseEvent e) { }
	public void mouseExited(MouseEvent e) { }
	public void mousePressed(MouseEvent e) { 
	    setBackground(v.glook().bright());
	}
	public void mouseReleased(MouseEvent e) { }
}
