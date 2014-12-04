/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file figure graphic

package JSim.xsim;

import java.io.*;
import JSim.util.*;
import javax.swing.*;

public class CFFigure extends CFGroupItem {
	public String name;
	public File file;  // file containing image
	public int wimg, himg;

	// constructor
	public CFFigure(CFGroup g, String f) {
	    super(g);
	    f = f.replaceAll(".xbm", ".gif");
	    file = new File("GIF/" + f);
	    name = file.getName();
	    name = name.substring(0, name.length()-4);
	    ImageIcon icon = new ImageIcon(file.getPath());
	    wimg = icon.getIconWidth();
	    himg = icon.getIconHeight();
	}

	// set dimension,  update group scaling
	public void setDim(String[] d) {
	    super.setDim(d);
	    group.pix_cfx = (wimg + 0.0) / w;
	    group.pix_cfy = (himg + 0.0) / h;
	}	    
	
	// set attribute
	protected void setItem(String key, String value) {
	    // nothing yet
	}

	// write RTML
	public void writeRTML() {
	    println("\t<image " + pos() + " " + size() +
		" name=\"" + name + "\"/>");
	}
}
