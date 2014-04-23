/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// button pops up a Var box

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

public class GRTVarButton extends GRTVar {

	// state
	public boolean visible;
	protected GAction action;
	public JPanel jaux;	// value/unit popup

	// constructor
	public GRTVarButton(GRTNode p, Element e) {
	    super(p, e);
	    visible = false;
	    addXMLChildren(e);
	    action = new GAction(this, label()) {
		public void doit() throws Xcept {
		    visible = !visible;
		    show();
		}
		public String helpKey() {
		    GRTVar btn = (GRTVar) gnode;
		    return "model" + GHelp.sep + 
			gnode.gmodel().pmodel().name() +
			GHelp.sep + btn.name();
		}
	    };
	}

	// make JComponent
	public void makeJComp() {
	    JButton button = action.button();
	    setJComp(button);
	    Insets insets = new Insets(1,1,1,1);
	    button.setMargin(insets);
	    setFont();
	    setBounds();
	}
	
	// show colors/widget
	public void show() {
	    JButton button = (JButton) jcomp();
	    Color color = glook().bg();
	    if (visible) {
		if (jaux == null) createAux();
		refresh();
		button.setBackground(Color.black);
		button.setForeground(color);
	    } else {
		button.setBackground(color);
		button.setForeground(Color.black);
	    }
	    jaux.setVisible(visible);
  	} 
	    
	// create jaux
	private void createAux() {
	    boolean units = false;
	    units = gmodel().pmodel().rt().getFlags().needsUnits;
	    makeStuff(units);
	    jvalue().setPreferredSize(
		new Dimension(pix(7),pix(1.5)));
	    LayoutManager layout = new GridLayout(
		junit() == null ? 1 : 2, 1);
	    jaux = new JPanel(layout);
	    int x = jcomp().getX();
	    int y = jcomp().getY() + jcomp().getHeight();
	    jaux.add(jvalue());
	    if (junit() != null) {
		junit().setPreferredSize(
		    new Dimension(pix(9),pix(1.5)));
		jaux.add(junit());
	    }
	    jaux.setLocation(x, y);
	    jaux.setSize(jaux.getPreferredSize());
	    page().addTop(jaux);
	}
}

