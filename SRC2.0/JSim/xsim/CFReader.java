/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// Parser callbacks for reading a CF
 
package JSim.xsim;
 
import java.io.*;       // File 
import java.util.*;
 
import JSim.util.*;
import JSim.data.*;
import JSim.mml.*;
import JSim.project.*;
import org.w3c.dom.Document;

public class CFReader extends CF {
	public CFScanner scanner;
	public CFItem parseItem;
	public CFGroup parseGroup;
	public CFGroupItem parseGItem;
	public CFBox parseBox;
	public CFMenu.Stack menuStack; 

	// constructor from file
	public CFReader(File f, Model u) throws Xcept {
	    super();
	    cfName = f.getAbsolutePath();
	    modName = f.getName();
	    int lx = modName.lastIndexOf('.');
	    if (lx > 0) modName = modName.substring(0, lx);
	    unitModel = u;
	    out = new PrintWriter(System.out, true);
	    err = new PrintWriter(System.err, true);
	    maxloc = 2001;
	    renamedPars = new CFItem.List();
	    dims = new StringList(4);
	    menuStack = new CFMenu.Stack();

	    // run parser
	    try {
	    	FileReader rdr = new FileReader(f);
	    	scanner = new CFScanner(rdr);
	    	CFParser parser = new CFParser(scanner);
	    	parser.cfr = this;
		parser.parse();
	    } catch (Exception e) {
		e.printStackTrace();
		error("parser failed one " + scanPos() + 
		    "  " + e);
	    }

	    // post-processing
	    maxloc += 1000;
	    ivar.pstart.rename(ivar.name() + ".min");
	    ivar.pstop.rename(ivar.name() + ".max");
	    ivar.pincr.rename(ivar.name() + ".delta");
	    for (int i=0; i<items.size(); i++) 
		items.item(i).post();
	}

	// create item
	public void startItem(String s, String n) {
	    s = s.toLowerCase();
	    if (s.equals("model")) 
		parseItem = new CFModel(this);
	    else if (s.equals("ivar")) 
		parseItem = new CFIvar(this, n);
	    else if (s.equals("feed")) 
		parseItem = new CFFeed(this, n);
	    else if (s.equals("logical")) 
		parseItem = new CFPar(this, CFPar.LOGICAL, n);
	    else if (s.equals("real")) 
		parseItem = new CFPar(this, CFPar.REAL, n);
	    else if (s.equals("choice")) 
		parseItem = new CFPar(this, CFPar.CHOICE, n);
	    else if (s.equals("int") || s.equals("integer")) 
		parseItem = new CFPar(this, CFPar.INT, n);
	    else if (s.equals("string")) 
		parseItem = new CFPar(this, CFPar.STRING, n);
	    else if (s.equals("parinfo")) 
		parseItem = par(n);
	    else 
		error("illegal top-level item: \"" + s + "\"");
	}

	// start a menu
	public void startMenu(String n) {
	    CFMenu m = new CFMenu(this, n);
	    CFMenu parent = menuStack.top();
	    if (parent != null) 
		parent.items.add(m);
	    menuStack.push(m);
	}

	// add page to menu
	public void addMenuPage(String n) {
	    CFMenuPage p = new CFMenuPage(this, n);
	    menuStack.top().items.add(p);
	}

	// done with menu
	public void doneMenu() {
	    menuStack.pop();
	}

	// start a group
	public void startGroup(String n) {
	    parseGroup = new CFGroup(this, n);
	    parseItem = parseGroup;
	}

	// create group item
	public void startGroupItem(String s, String n) {
	    if (parseGroup == null) {
		error("group item \"" + n + "\"lacks parent group");
		return;	
	    }

	    s = s.toLowerCase();
	    if (s.equals("figure")) 
		parseItem = new CFFigure(parseGroup, n);
	    else if (s.equals("text")) 
		parseItem = new CFText(parseGroup, n);
	    else if (s.equals("parbtn") || s.equals("parbutton")) 
		parseItem = new CFButton(parseGroup, CFButton.PAR, n);
	    else if (s.equals("groupbtn") || s.equals("groupbutton")) 
		parseItem = new CFButton(parseGroup, CFButton.GROUP, n);
	    else if (s.equals("feedbtn") || s.equals("feedbutton")) 
		parseItem = new CFButton(parseGroup, CFButton.FEED, n);
	    else if (s.equals("box")) 
		parseItem = new CFBox(parseGroup);
	    else 
		error("illegal group item: \"" + s + "\"");
	}

	// set dimensions of group item
	public void setDim(String[] dims) {
	    if (! (parseItem instanceof CFGroupItem)) {
		error("setDim without group item");
		return;
	    }
	    ((CFGroupItem) parseItem).setDim(dims);
	}

	// done with group item
	public void doneGroupItem() {
	    parseItem = parseGroup;
	}

	// start a box
	public void startBox(String[] dims) {
	    if (parseGroup == null) {
		error("box lacks parent group");
		return;	
	    }
	    parseItem = parseBox = new CFBox(parseGroup);
	    setDim(dims);
	}

	// add boxpar
	public void startBoxPar(String n) {
	    parseItem = new CFBoxPar(parseBox, n);
	}

	// done boxpar
	public void doneBoxPar() {
	    parseItem = parseBox;
	}
	
	// done with box
	public void doneBox() {
	    parseItem = parseGroup;
	}

	// set parse attribute
	public void setParseAttr(String key, String value) {
	    if (parseItem == null) return;
	    parseItem.set(key, value);
	}
	public void setParseAttrList(String key, StringList value) {
	    if (parseItem == null) return;
	    parseItem.setList(key, value);
	}

	// show position in scanner
	public String scanPos() {
	    return cfName + 
		": near line #" + scanner.getLineNo() +
		" char #" + scanner.getCharNo() + 
		" token \"" + scanner.tokText() + "\"";
	}

	
}

