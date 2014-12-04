/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// XSim configuration file menu

package JSim.xsim;

import java.util.ArrayList;
import JSim.util.*;

public class CFMenu extends CFItem {
	public boolean top;
	public String text;
	public CFItem.List items;

	// constructor
	public CFMenu(CF c, String n) {
	    super(c);
	    text = n;
	    top = (text == null);
	    items = new CFItem.List();
	}

	// has content
	public boolean hasContent() {
	    for (int i=0; i<items.size(); i++) {
		CFItem item = items.item(i);
		if (item instanceof CFMenu &&
		    ((CFMenu) item).hasContent())
		    return true;
		if (item instanceof CFMenuPage &&
		    ((CFMenuPage) item).hasContent())
		    return true;
	    }
	    return false;
	}

	// write RTML page
	public void writeRTML() {
	    writeRTML("    ");
	}
	public void writeRTML(String indent) {
	    if (! hasContent()) return;
	    String tlab = (text==null) ? "" : textAttr(text);
	    println(indent + "<menu" + tlab + ">");
	    for (int i=0; i<items.size(); i++) {
		CFItem item = items.item(i);
		if (item instanceof CFMenu) 
		    ((CFMenu) item).writeRTML(indent + "    ");
		if (item instanceof CFMenuPage)
		    ((CFMenuPage) item).writeRTML(indent + "    ");
	    }
	    println(indent + "</menu>");
	}

	// Menu stack
	public static class Stack extends ArrayList<CFMenu> {
	    // constructor
	    public Stack() { super(4); }

	    // push onto stack
	    public void push(CFMenu m) {
		add(m);
	    }

	    // item at top of stack, null if empty
	    public CFMenu top() {
	       	int n = size();
		if (n<1) return null;
		return (CFMenu) get(n-1);
	    }

	    // pop off stack
	    public void pop() {
		int n = size();
		if (n>0) remove(n-1);
	    }
	}
}
