/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// forward IC dialog

package JSim.gui.model;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;
import JSim.aserver.*;

public class GModelForwardIC extends GNode {

	// state
	private PModel pmodel;
	private JDialog popup;
	private JLabel[] xlabels;
	private JTextField[] xfields;
	private GAction ok, cancel;

	// constructor
	public GModelForwardIC(GModel g, PModel p) {
	    super(g, p);
	    pmodel = p;
	    popup = new JDialog(g.frame(),
	    	"Forward ICs from last run");

	    // OK/Cancel
	    ok = new GAction(this, "OK") {
	       	public void doit() throws Xcept {
		    doForward();
		    popup.setVisible(false);
	        }
	    };
	    cancel = new GAction(this, "Cancel") {
	       	public void doit() {
		    popup.setVisible(false);
	        }
	    };
	}

	// update content and show the dialog
	public void show() throws Xcept {
	    PModel pmodel = gmodel().pmodel();
	    Data.List doms = pmodel.forwardableICDomains();
 	    if (doms.size() == 0) throw new Xcept(
		"Model has no forwardable ICs");
	    JPanel panel = new JPanel(new GridLayout(doms.size()+2,2));
//	    ghelp().registerKeystrokes(panel);

	    JLabel l = new JLabel("Domain");
	    l.setHorizontalAlignment(JLabel.CENTER);
	    panel.add(l);
	    l = new JLabel("Value");
	    l.setHorizontalAlignment(JLabel.CENTER);
	    panel.add(l);

	    xlabels = new JLabel[doms.size()];
	    xfields = new JTextField[doms.size()];
	    for (int i=0; i<doms.size(); i++) {
	    	String xname = doms.data(i).desc();
		xlabels[i] = new JLabel(xname);
	    	xlabels[i].setFont(glook().bigFont());
	    	xlabels[i].setHorizontalAlignment(JLabel.CENTER);
		panel.add(xlabels[i]);	
		xfields[i] = new JTextField(15);
		double xval = doms.data(i).max();
		xfields[i].setText(Util.pretty(xval));
		panel.add(xfields[i]);
	    }

	    JButton b = new JButton(ok);
	    panel.add(b);
	    popup.setContentPane(panel);
	    b = new JButton(cancel);
	    panel.add(b);

	    panel.setSize(panel.getPreferredSize());
	    popup.pack();
	    popup.setLocationRelativeTo(parent().jcomp());
	    popup.setVisible(true);
	}

	// forward the ICs
	private void doForward() throws Xcept {
	    NamedVal.List nvals = new NamedVal.List();
	    for (int i=0; i<xfields.length; i++) {
	    	String n = xlabels[i].getText();
		double val = Util.toDouble(xfields[i].getText());
		NamedVal nval = NamedVal.create(n, val);
		nvals.add(nval);
	    }
	    ASVar.List vfwd = pmodel.forwardICs(nvals);
	    gmodel().refresh();
	    gproject().message("Forwarded ICs for " + vfwd);
	}	    
}
