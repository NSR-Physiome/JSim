/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Memory monitor

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;

public class GMemory extends GNode {
	public int maxMem;
	public int totMem;
	private JDialog popup;
	private JProgressBar bar;
	private GAction refresh, gc, dismiss;
	private final long MB = 1024*1024;
	
	// constructor
	public GMemory(GMain main) throws Xcept {
	    super(main, null);
	    maxMem = (int) (Runtime.getRuntime().maxMemory() / MB);
	}
	
	// show
	public void show(GProject gproj) throws Xcept {
	    if (popup == null)
	    	createPopup(gproj);
	    refresh();
	    popup.setLocationRelativeTo(gproj.jcomp());
	    popup.setVisible(true);	    
	}

	// create popup
	private void createPopup(GProject gproj) throws Xcept {

	    // actions
	    refresh = new GAction(this, "Refresh") {
	    	public void doit() throws Xcept {
		    gnode.refresh();
		}
	    };
	    dismiss = new GAction(this, "Dismiss") {
	    	public void doit() throws Xcept {
		    ((GMemory) gnode).popup.setVisible(false);
		}
	    };
	    gc = new GAction(this, "Compact") {
	    	public void doit() throws Xcept {
		    System.gc();
		    gnode.refresh();	    
		}
	    };

	    // dialog widgets	    
	    if (gproj.frame() == null) throw new Xcept(
	    	"Memory monitor not available in embedded applets");
	    popup = new JDialog(gproj.frame());
	    popup.setTitle("JSim memory monitor");
	    JPanel panel = new JPanel(new GridLayout(4,1));
	    JLabel hdr = new JLabel("Current memory usage", JLabel.CENTER);
	    panel.add(hdr);
	    bar = new JProgressBar(0, maxMem);
	    bar.setStringPainted(true);
	    panel.add(bar);
	    JPanel mpanel = new JPanel(new GridLayout(1,2));
	    mpanel.add(new JLabel("0", JLabel.LEFT));
	    mpanel.add(new JLabel("" + maxMem, JLabel.RIGHT));
	    panel.add(mpanel);
//	    JPanel bpanel = new JPanel(new GridLayout(1,2));
	    JPanel bpanel = new JPanel(new GridLayout(1,3));
	    bpanel.add(new JButton(refresh));
//	    bpanel.add(new JButton(gc));
	    bpanel.add(new JButton(dismiss));
	    panel.add(bpanel);
	    popup.setContentPane(panel);
	    popup.pack();
	}


	// refresh
	public void refresh() {
  	    if (bar == null) return;
	    totMem = (int) (Runtime.getRuntime().totalMemory() / MB);
	    bar.setValue(totMem);
	    bar.setString("" + totMem + " MB");
	    // box.repaint();
	}
}
