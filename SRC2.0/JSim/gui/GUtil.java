/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// General-purpose GUI utilities

package JSim.gui;

import java.awt.*;
import JSim.util.*;

public class GUtil {

	// window warning banner height,  if any
	public static int windowWarningBannerHeight(Window w) {
	    try {
		SecurityManager mgr = System.getSecurityManager();
		if (mgr == null) return 0;
		AWTPermission perm = new AWTPermission(
		    "showWindowWithoutWarningBanner");
		mgr.checkPermission(perm);
	        return 0;
	    } catch (Exception e) {
		Font font = w.getFont();
		int h = 20;
		if (font != null) 
		    h = w.getFontMetrics(font).getHeight();
//		System.err.println("banner height=" + h);
		return h;
	    }
	}

	// "safe" location on screen
	public static Point getLocationOnScreen(Component c) {
	    try {
	    	return (Point) c.getLocationOnScreen().clone();
	    } catch (IllegalComponentStateException e) {
//		System.err.println(
//		    "Warning: getLocationOnScreen() failed");
	    	Dimension sz = c.getToolkit().getScreenSize();
	    	return new Point(sz.width/2, sz.height/2);
	    }
	}
}
