/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Help system central control

package JSim.gui;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.CharacterData;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.model.*;

public class GHelp 
implements MouseListener, MouseMotionListener, ActionListener  {
	public static final String sep = "$"; // key separator

	// modes
	public static final int HIDE = 0; 
	public static final int HOVER = 1; 
	public static final int FAST = 2; 
	public static final int EDIT = 3; 
	private int permMode; // set via keys (HIDE, HOVER, FAST)
	private int currMode; // current popup status

	// perm info
	private static GHelpDB jsimDB; // JSim help DB
	private GProject gproj;
	private GHelpPopup popup;
	private GAction hoverKey, fastKey, editKey;

	// curr status
	private Object currObj;  // object to show help for
	private JComponent currJComp;  // component to show help for 
	private Rectangle currRect;  // rectangle within component, or null 

	// hover timer
	private static int HOVER_CLICK = 400; // ms per click
	private static int HOVER_DELAY = 4; // popup at 4 clicks
	private javax.swing.Timer hoverTimer;
	private int hoverCt;

	// constructor
	public GHelp(GProject p) {
	    gproj = p;
	    popup = new GHelpPopup(gproj);
	    permMode = HIDE; // was HOVER in v1.6
	    currMode = HIDE;

	    // setup, register keystrokes
	    hoverKey = new GAction(gproj, "Hover Help Key") {
		public void doit() {  helpKeyTyped(HOVER); }
	    };
	    fastKey = new GAction(gproj, "Fast Help Key") {
		public void doit() {  helpKeyTyped(FAST); }
	    };
	    editKey = new GAction(gproj, "Help Edit Key") {
		public void doit() {  helpKeyTyped(EDIT); }
	    };    
	    registerKeystrokes(gproj.jcomp());

	    // start timer
	    hoverTimer = new javax.swing.Timer(HOVER_CLICK, this);
	    hoverCt = 0;
	    hoverTimer.start();
	}

	// register keystrokes
	public void registerKeystrokes(JComponent jcomp) {
	    ActionMap amap = jcomp.getActionMap();
	    InputMap imap = jcomp.getInputMap(
		jcomp.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
	    KeyStroke f1 = KeyStroke.getKeyStroke(
		KeyEvent.VK_F1, 0);
	    KeyStroke sf1 = KeyStroke.getKeyStroke(
		KeyEvent.VK_F1, Event.SHIFT_MASK);
	    KeyStroke cf1 = KeyStroke.getKeyStroke(
		KeyEvent.VK_F1, 
		gproj.toolkit().getMenuShortcutKeyMask());
	    amap.put("f1", hoverKey);
	    imap.put(f1, "f1");
	    amap.put("sf1", fastKey);
	    imap.put(sf1, "sf1");
	    amap.put("cf1", editKey);
	    imap.put(cf1, "cf1");
	}

	// MouseListener
	public void mouseEntered(MouseEvent e) {
	    hoverCt = 0;
	    if (! (e.getComponent() instanceof JComponent)) 
		return;
	    JComponent jcomp = (JComponent) e.getComponent();
	    Object o = jcomp.getClientProperty("help");
	    showHelp(jcomp, o);
	}
	public void mouseExited(MouseEvent e) {
	    hoverCt = 0;
	    if (currMode == HOVER) showHelp(HIDE);
	    showHelp(null, null);
	}
	public void mouseClicked(MouseEvent e) { }
	public void mousePressed(MouseEvent e) { }
	public void mouseReleased(MouseEvent e) { }

	// MouseMotionListener
	public final void mouseMoved(MouseEvent e) { 
	    hoverCt = 0; 
	    if (currMode == HOVER) showHelp(HIDE); 
	}
	public final void mouseDragged(MouseEvent e) { 
	    mouseMoved(e);
	}

	// ActionListener (for hoverTimer)
	public void actionPerformed(ActionEvent e) {
	    if (currJComp == null) {
	    	if (currMode == HOVER) showHelp(HIDE);
		return;
	    }
	    if (permMode != HOVER) return;	     
	    hoverCt++;
	    if (hoverCt != HOVER_DELAY) return;
	    showHelp(HOVER);
	}

	// help key 
	private void helpKeyTyped(int whichKey) {    
	    int mode = HIDE;
	    String msg = null;
	    switch (whichKey) {
	    case HOVER:
	    	mode = permMode = (permMode == HIDE) ? HOVER : HIDE;
		msg = (mode == HIDE) ?
		    "Help disabled. Type F1 to enable hover help" +
		        ", shift-F1 to enable mouse-over help." :
		    "Hover help enabled. Type F1 or Shift-F1 to disable.";
		break;
	    case FAST:
	    	mode = permMode = (permMode == HIDE) ? FAST : HIDE;
		msg = (mode == HIDE) ?
		    "Help disabled. Type F1 to enable hover help" +
		        ", shift-F1 to enable mouse-over help." :
		    "Fast help enabled, type F1 or Shift-F1 to disable.";
		break;
	    case EDIT:
	    	mode = (currMode == EDIT) ? HIDE : EDIT;
		break;
	    }	    
	    showHelp(mode);
	    if (msg != null) gproj.message(msg);
	}

	// set help refs and show
	public void showHelp(JComponent j, Object o) {
	    Rectangle r = null;
	    if (o instanceof GTabs && (!(j instanceof JMenu))) {
		r = ((GTabs) o).tabBounds();
		if (r != null) 
		    r.width += 10;
	    }
	    showHelp(j, r, o);
	}
	public void showHelp(JComponent j, Rectangle r, Object o) {
	    currJComp = j;
	    currRect = r;
	    currObj = o;
	    if (permMode == FAST) showHelp(FAST);
	}

	// show/hide popup, update currMode
	private void showHelp(int mode) {
	    if (showHelpAux(mode)) {
	    	currMode = mode;
	    } else {
		if (currMode != HIDE)
		    popup.hide();
		currMode = HIDE;
	    	hoverCt = 0; // fix win32 focusLost problem 3/29/07
	    }
	}

	// show help or return false
	private boolean showHelpAux(int mode) {
	    if (mode == HIDE) return false;
	    if (currJComp == null) return false;
	    String key = currKey();
	    if (key == null) return false;

	    // which DB
	    GHelpDB db = jsimDB();
	    if (key.startsWith("model$")) {
		key = key.substring(6);
		int inx = key.indexOf('$');
		if (inx < 0) return false;
		String model = key.substring(0,inx);
		key = key.substring(inx+1);
		db = modelDB(model);
	    }
	    if (db == null) return false;

	    // valid location? (HACK!!!, extra getLocation call)
	    try {
	    	currJComp.getLocationOnScreen();
	    } catch (IllegalComponentStateException e) {
		return false;
	    }

	    // show popup
	    popup.show(mode, currJComp, currRect, db, key);
	    return true;
	}

	// calculate current key
	private String currKey() { 
	    if (currObj == null) return null;
	    Object obj = currObj;
	    if (obj instanceof JComponent) 
	    	obj = ((JComponent) obj).getClientProperty("help");
	    String key = null;
	    if (obj instanceof GAction) {
		key = ((GAction) obj).helpKey();
	    } else if (currJComp instanceof JMenu) {
		key = "menu" + sep + shortClassName(obj) + sep +
		    ((JMenu) currJComp).getText();
	    } else if (currJComp instanceof JMenuItem && obj instanceof GControl) {
		GControl c = (GControl) obj;
		JMenuItem m = (JMenuItem) currJComp;
		key = c.helpKey(m.getText());
	    } else if (obj instanceof GNode) {
	    	key = ((GNode) obj).helpKey();
	    } else if (obj instanceof String) {
		key = (String) obj;
	    }
	    return key;
	}

	// jsim help DB
	private GHelpDB jsimDB() {
	    if (jsimDB != null) return jsimDB;
	    try {
		URL url = getClass().getResource("helpDB.xml");
		jsimDB = GHelpDB.read(url);
	    } catch (Xcept e) {
		gproj.message(e);
	    }
	    return jsimDB;
	}

	// model help DB
	private GHelpDB modelDB(String model) {
	    PNamed pnamed = gproj.project().child(model);
	    if (! (pnamed instanceof PModel)) {
		System.err.println("modelDB " + model + "=" + pnamed);
		return null;
	    }
	    GModel gmodel = (GModel) gproj.lefttabs().child(pnamed);
	    return (gmodel == null) ? null : gmodel.helpDB();
	}

	// short class name
	public static String shortClassName(Object obj) {
	    if (obj == null) return "null";
	    return shortClassName(obj.getClass());
	}
	public static String shortClassName(Class c) {
	    String clss = c.getName();
	    int inx = clss.lastIndexOf('.');
	    if (inx >= 0) 
		clss = clss.substring(inx+1);
	    return clss;
	}	

	// help text for action;
	public String helpText(GAction a) {
	    String key = a.helpKey();
	    return jsimDB().getEditableText(key);
	}

	// is help active? s/b phased out
	public boolean helpActive() { return currMode == FAST; }
	public int permMode() { return permMode; }

	// called project exit
	public void exit() {
	    hoverTimer.stop();
	    showHelp(HIDE);
	}

}

