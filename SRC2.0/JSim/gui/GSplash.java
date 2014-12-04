/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// splash screen for project startup,  file load

package JSim.gui;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.*;

public class GSplash extends Window {
	static GSplash gsplash;

	// show splash window
	public static void showSplash(Window w) {
	    if (gsplash != null) return;
	    System.err.println("Loading JSim file(s). Please wait...");
	    gsplash = new GSplash(w);
	    gsplash.setVisible(true);
	}

	// hide splash window
	public static void hideSplash() {
	    if (gsplash == null) return;
	    System.err.println("File load completed.");
	    gsplash.setVisible(false); 
	    gsplash = null;
	}

	// constructor
	public GSplash(Window w) {
	    super(w);
	    JPanel panel = new JPanel(new GridLayout(1, 1));
	    JLabel label = new JLabel(
		"Loading JSim file(s). Please wait...");
	    panel.add(label);
	    panel.setBorder(new EtchedBorder());
	    add(panel);
	    Dimension sdim = getToolkit().getScreenSize();
	    Dimension wdim = new Dimension(sdim.width/4, sdim.height/8);
	    int x = (sdim.width - wdim.width) / 2;
	    int y = (sdim.height - wdim.height) / 2;
	    setLocation(x,y);
	    setSize(wdim);
	}
}
