/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Project messages area

package JSim.gui;

import java.text.*;
import java.util.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GMessage extends GEditor {

	// state
	private GAction showMem, runGC;
	private JTextArea text;
	private DateFormat dateFormat;

	// constructor
	public GMessage(GNode p) {
	    super(p, null);
	    dateFormat = new SimpleDateFormat("HH:mm:ss");

	    // create widgets
	    JRootPane root = new JRootPane();
	    text = new JTextArea();
	    text.setBackground(glook().dark());
	    text.setEditable(false);
	    text.setLineWrap(true);
	    setIndentedBorder(text);
	    JScrollPane scroll = new JScrollPane(text);
	    root.getContentPane().add(scroll);
	    setJComp(root);

	    // Startup message
	    message("Starting JSim version " + Util.version() + 
	    	" on " + Util.javaOSVersion() + "\n");

	    // create actions
	    showMem = new GAction(this, "Show memory stats") {
	    	public void doit() throws Xcept { 
		   showMemory(); 
		}
	    };
	    runGC = new GAction(this, "Run garbage collector") {
	    	public void doit() throws Xcept { 
		   message("GC running...");
		   Runtime.getRuntime().gc();
		   showMemory();
		}
	    };

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    JMenu menu;

	    menu = newMenu("File");
	    menu.add(storeNotes.item());
	    menu.add(exportText.item());
	    menu.add(printText.item());
	    menu.add(showMem.item());
//	    menu.add(runGC.item()); // debug only
	    mbar.add(menu);

	    menu = newMenu("Edit");
	    menu.add(copy.item());
	    menu.add(gotoLine.item());
	    menu.add(findText.item());
	    menu.add(findAgain.item());
	    menu.addSeparator();
	    menu.add(erase.item());
	    mbar.add(menu);

	    root.setJMenuBar(mbar);
	}	    

	// query
	public JTextArea text() { return text; }

	// message
	public void message(String s) {
	    if (s == null) 
	        s = "mysterious error";
	    text.append(time() + " " + s);
	    if (s.charAt(s.length()-1) != '\n') 
		text.append("\n");
	}
	public String time() {
	    Date date = new Date(System.currentTimeMillis());
	    String s = dateFormat.format(date);
	    return s;
	}
	public String title() { 
	    return gproject().title() + " Messages";
	}

	// memory message
	public void showMemory() throws Xcept {
	    message(server().memoryMessage());
	}
}
