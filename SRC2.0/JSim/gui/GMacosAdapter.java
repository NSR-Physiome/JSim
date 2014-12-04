/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// interaction between Mac OS X and JSim application
//    com.apple.eawt classes are built into Mac OS X JRE
//    non-Mac platforms instantiate non-functional
//    placeholder classes from
//    $JSIMSRC/$JSIMOS/lib/ext/AppleJavaExtensions.jar

package JSim.gui;

import java.io.*;
import com.apple.eawt.*;
import JSim.util.*;

public class GMacosAdapter extends ApplicationAdapter {

	// connect JSim application to OS X
	public static void connect(GMain g) {
	    Application appl = new Application();
	    appl.setEnabledPreferencesMenu(false);
	    GMacosAdapter adapter = new GMacosAdapter(g);
	    appl.addApplicationListener(adapter);
	}

	// instance fields
	private GMain gmain;

	// constructor
	private GMacosAdapter(GMain g) { 
	    super(); 
	    gmain = g;
	}

	// .proj .mod .tac etc. file opened from finder
	public void handleOpenFile(ApplicationEvent e) {
	    File f = new File(e.getFilename());
	    gmain.loadFile(f);
	}

	// quit application from finder
        public void handleQuit(ApplicationEvent e) {
	    try {
	    	gmain.exitRequest();
	    } catch (Xcept x) {
	    	System.err.println(x.cleanMessage());
	    }
	}
}

