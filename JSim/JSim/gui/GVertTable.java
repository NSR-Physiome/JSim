/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// fixed vertical control table

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GVertTable extends GNode {

	// state
	private int nrows;
	private JPanel panel;
	private Control[] cntls;
	private JLabel[] jlabels;
	private GControl[] gcntls;

	// constructor
	public GVertTable(GNode p, Control[] controls,
	int labWidth, String[] labs) {
	    super(p, null);
	    cntls = controls;
	    nrows = cntls.length;
	    jlabels = new JLabel[nrows];
	    gcntls = new GControl[nrows];

	    // create widgets
	    panel = new JPanel(null);
	    setJComp(panel);
	    int xcntl = (int) (labWidth * glook().fontSize());
	    int ypos = 0;
	    for (int i=0; i<nrows; i++) {
		String lab = cntls[i].name();
		if (labs != null && labs[i] != null)
		    lab = labs[i];
		lab = pad(lab, labWidth);
		JLabel jlabel = new JLabel(lab);
		jlabel.setLocation(0, ypos);
		jlabel.setSize(jlabel.getPreferredSize());
		panel.add(jlabel);
		jlabels[i] = jlabel;

		GControl gcntl = GControl.create(this, cntls[i]);
		JComponent jcntl = gcntl.jcomp();
		jcntl.setLocation(xcntl, ypos);
		jcntl.setSize(jcntl.getPreferredSize());
		panel.add(jcntl);
		ypos += jcntl.getSize().height;
		gcntls[i] = gcntl;
	    }
	    int xmax = xcntl*2;
	    panel.setPreferredSize(new Dimension(xmax, ypos));
	}

	// pad end of string
	public String pad(String s, int w) {
	    int p = w - s.length();
	    for (int i=0; i<p; i++)
		s = s + "  ";
	    return s;
	}

	// set row visible
	public void setRowVisible(int r, boolean b) {
	    jlabels[r].setVisible(b);
	    gcntls[r].jcomp().setVisible(b);
	}
	public void setRowVisible(String name, boolean b) {
	    for (int r=0; r<nrows; r++)
	    	if (cntls[r].name().equals(name))
		    setRowVisible(r, b);
	}
	
	// query
	public int nrows() { return nrows; }
	public GControl gcntl(int i) { 
	    return (GControl) child(i); 
	}

}
