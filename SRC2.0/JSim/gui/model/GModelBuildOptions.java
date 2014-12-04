/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Model source editor

package JSim.gui.model;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

public class GModelBuildOptions extends GNode {

	// perm state
	private PModel pmodel;
	private PModelBuildOptions poptions;
	
	// actions
	private GAction a_compile, a_dismiss;
	
	// sub-components
	private JPanel panel;
	private GVertTable gopts;
	private JButton b_compile, b_dismiss;
	private JDialog dialog;

	// constructor
	public GModelBuildOptions(GNode g, PModel pm) {
	    super(g, pm);
	    pmodel = pm;
	    poptions = pmodel.options();
	    panel = new JPanel(new BorderLayout());
	    
	    a_compile = new GAction(this, "Compile") {
		public void doit() throws Xcept {
		    hide();
		    gmodel().gsrc().compile.doit();
		}
	    };
	    a_dismiss = new GAction(this, "Dismiss") {
		public void doit() throws Xcept {
		    hide();	
		}
	    };

	    // control panel
	    PNamed.List clist = poptions.children(Control.class);
	    Control[] cntls = new Control[clist.size()];
	    for (int i=0; i<cntls.length; i++)
	    	cntls[i] = clist.control(i);
	    gopts = new GVertTable(this, cntls, 10, null);
	    panel.add(gopts.jcomp(), BorderLayout.CENTER);

	    // control buttons
	    JPanel buttons = new JPanel(new GridLayout(1, 2));
	    b_compile = new JButton(a_compile);
	    b_dismiss = new JButton(a_dismiss);
	    buttons.add(b_compile);
	    buttons.add(b_dismiss);
	    panel.add(buttons, BorderLayout.SOUTH);

	    ghelp().registerKeystrokes(panel);	    

	    // dialog
	    Window pwin =
		SwingUtilities.getWindowAncestor(gproject().jcomp());
	    JFrame frame = (pwin instanceof JFrame) ?
	    	((JFrame) pwin) : null;
	    dialog = new JDialog(frame, "Compiler Options");
	    dialog.setContentPane(panel);
	}
	
	// refresh
	
	// show dialog
	public void show() {
	    gopts.refresh();
	    Component pcomp = parent().jcomp();
	    Point p = GUtil.getLocationOnScreen(pcomp);
	    Dimension dim = panel.getPreferredSize();
	    p.x += pcomp.getWidth()/4 - dim.width/2;
	    p.y += pcomp.getHeight()/2 - dim.height/2;
	    dialog.setLocation(p.x, p.y);
	    dialog.setSize(dim.width + 10, dim.height + 30);
	    dialog.setVisible(true);
	}

	// hide window
	private void hide() {
	    dialog.setVisible(false);
	}

	// query
}

