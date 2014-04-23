/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Model Parameter display

package JSim.gui.model;

import java.io.*;
import java.util.ArrayList;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; import JSim.project.*;
import JSim.xsim.*;
import JSim.gui.*;
import JSim.gui.rtml.*;

public class GModelPars extends GNode {

	// state
	private JPanel panel;
	private JMenuBar mbar;
	private JLabel l_hdr;
	private JMenu pageMenu;
	private GRTDoc grtAuto, grtCustom;
	private ArrayList<JComponent> menuItems;
	private ArrayList<JComponent> pageItems;
	private int nCustomPageItems;
	private CardLayout cards;
	private StringList cardNames;
	private GRTPage currPage;
	private String currPageName;
	private int currPageItemInx; 
	private StringList pageStack;

	// actions
	public GAction useStd, useCustom, back, next, newFunc;
	public GAction importRTML, deleteRTML,
	    exportDefaultRTML, exportCustomRTML;

	// constructor
	public GModelPars(GModel gmodel) {
	    super(gmodel, gmodel.pmodel());

	    // create widgets
	    JRootPane root = new JRootPane();
	    setJComp(root);
	    cards = new CardLayout();
	    panel = new JPanel(cards);
	    root.getContentPane().add(panel);

	    // back to last page action
	    back = new GAction(this, "Back") {
		public void doit() {
		    int ct = pageStack.size();
		    if (ct<2) return;
		    String name = pageStack.str(ct-2);
		    pageStack.remove(ct-2);
		    pageStack.remove(ct-2);
		    setPage(name);
		}
	    };
	    back.setAccel('B');

	    // next page action
	    next = new GAction(this, "Next") {
		public void doit() {
		    setPage(currPageItemInx + 1);
		}
	    };
	    next.setAccel('N');

	    // create new function generator action
	    newFunc = new GAction(this, "New function generator") {
		public void doit() throws Xcept {
		    PModelVars vars = gmodel().pmodel().vars();
		    String msg = "Enter name for new function generator";
		    String n = vars.newChildName("fgen", true);
		    String nname = (String) JOptionPane.showInputDialog(
			    jcomp(), msg, "Enter name", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().funcgenIcon(),
			    null, n);
		    if (nname == null) return;
		    new FuncGen(vars, nname);
		    connectRT();
		    gproject().project().revalidate();
		    gproject().refresh();
		    setPage(pageItems.size()-1);
		}
	    };

	    // import custom RTML
	    importRTML = new GAction(this, "Import custom RTML...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, true, GFileChooser.RTML);
		    if (info == null) return;
		    File f = info.file;
	   	    gmodel().pmodel().customRTML().importRTML(f);
		    reloadCustom(gmodel().pmodel().customRTML().rtml());
		}
	    };
	
	    // delete custom RTML
	    deleteRTML = new GAction(this, "Delete custom RTML...") {
		public void doit() throws Xcept {
		    String msg = "Delete " + nCustomPages() + " custom RTML pages "
		        + " and " + nCustomMenuItems() + " menu items?";
		    int stat = JOptionPane.showConfirmDialog(jcomp(), msg);
		    if (stat != JOptionPane.YES_OPTION) return;
	   	    gmodel().pmodel().customRTML().deleteRTML();
		    reloadCustom(gmodel().pmodel().customRTML().rtml());
		}
		public boolean sbEnabled() { return hasCustomRTML(); }
	    };
	
	    // export custom RTML
	    exportCustomRTML = new GAction(this, "Export custom RTML...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, false, GFileChooser.RTML);
		    if (info == null) return;
		    File f = info.file;
	   	    gmodel().pmodel().customRTML().exportRTML(f);
		}
	    };
	
	    // export default RTML
	    exportDefaultRTML = new GAction(this, "Export default RTML...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, false, GFileChooser.RTML);
		    if (info == null) return;
		    try {
		    	FileWriter fwrt =  new FileWriter(info.file);
		    	XMLWriter xwrt = new XMLWriter();
		    	GRTAuto grtAuto = (GRTAuto) gnode.gmodel().gpars().grtAuto();
		    	if (grtAuto == null) throw new Xcept(
			    "Model is not compiled");
		    	Document doc = grtAuto.doc();
		    	xwrt.write(doc, fwrt);
		    } catch (Exception e) {
			throw Xcept.wrap(e);
		    }
		}
	    };
	
	    // menubar
	    mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

 	    gmodel().addParSetMenu(mbar);

	    // pages menu and Next button
	    pageMenu = newMenu("Pages");
	    mbar.add(pageMenu);
	    mbar.add(back.button(glook().prevIcon()));
	    mbar.add(next.button(glook().nextIcon()));

	    // run button, All-Runs menu
	    mbar.add(gmodel().singleRun.button("Run"));
	    gmodel().addRunsMenu(mbar);

	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);

	    // null initial configuration
	    reloadPages();
	}

	// connect to RT model after compile
	public void connectRT() {
	    if (grtCustom != null) {
		grtCustom.destroy();
		grtCustom = null;
	    }
	    if (grtAuto != null) {
		grtAuto.destroy();
		grtAuto = null;
	    }
	    if (gmodel().pmodel().rt().isBuilt())
	    	grtAuto = new GRTAuto(this);
 	    gmodel().realizeCustom();
	    reloadPages();
	}

	// reloadCustom() 
	public void reloadCustom(Element e) {
	    if (e == null) 
	    	grtCustom = null;
	    else
		grtCustom = new GRTDoc(this, e);
	    reloadPages();
	}

	// reload pages menu and current page
	private void reloadPages() {

	    // clear panel / pages
	    panel.removeAll();
	    pageMenu.removeAll();	    
	    cardNames = new StringList(4);
	    pageStack = new StringList(32);

 	    // run-time unavailable
	    if (grtAuto == null) {
	        panel.add(new JLabel(
		   "Model is not compiled."),
		   "zorkFest");
		return;
	    }

	    // load menuItems,  pageItems, nCustomPageItems
	    menuItems = new ArrayList<JComponent>();
	    pageItems = new ArrayList<JComponent>();
	    nCustomPageItems = 0;
	    if (grtCustom != null) {
		menuItems.addAll(grtCustom.menuItems());
		pageItems.addAll(grtCustom.pageItems());
		nCustomPageItems = pageItems.size();
	    }
	    if (grtAuto != null) {
		menuItems.addAll(grtAuto.menuItems());
		pageItems.addAll(grtAuto.pageItems());
	    }

	    // set accelerators for auto Items
	    boolean[] accel = new boolean[128];
	    String[] starts = new String[] {
		"Input", "Output", "Solver", "Func" };
	    for (int i=nCustomPageItems; i<pageItems.size(); i++) {
		JMenuItem item = (JMenuItem) pageItems.get(i);
	        String s = item.getText();
		boolean addAccel = false;
		for (int j=0; j<starts.length; j++)
		    if (s.startsWith(starts[j]))
			addAccel = true;
		char c = Util.isBlank(s) ? ' ' : s.charAt(0);
		int cinx = c - ' ';
		if (cinx<0 || cinx>=accel.length || accel[cinx])
		    addAccel = false;
		if (addAccel) {
		    KeyStroke key = KeyStroke.getKeyStroke(c, 
			toolkit().getMenuShortcutKeyMask(), 
			false);
		    item.setAccelerator(key);
		    accel[cinx] = true;
		}
	    }

	    // create menu from menuItems
	    for (int i=0; i<menuItems.size(); i++) {
		JMenuItem item = (JMenuItem) menuItems.get(i);
		pageMenu.add(item);
	    }

	    // next, new func gen
	    pageMenu.addSeparator();
	    pageMenu.add(back.item());
	    pageMenu.add(next.item());
	    pageMenu.add(newFunc.item());
	    pageMenu.addSeparator();
	    pageMenu.add(importRTML.item());
	    pageMenu.add(exportCustomRTML.item());
	    pageMenu.add(exportDefaultRTML.item());
	    pageMenu.add(deleteRTML.item());

	    // show 1st page
	    setPage(0);
	}

	// set page by pageItem index
	private void setPage(int n) {
	    if (n >= pageItems.size()) n=0;
	    currPageItemInx = n;
	    currPageName = pageName(n);
	    if (currPageName == null) 
		currPageName = "missing page #" + n;
	    currPage = page(currPageName);
	    showPage();
	    pageStack.add(currPageName);
	}

	// set page by name,  then show
	public void setPage(String name) {
	    currPageName = name;
	    currPageItemInx = pageItemInx(name);
	    currPage = page(name);
	    showPage();
	    pageStack.add(currPageName);
	}

	// push page card to front,  create if needed
	private void showPage() {

	    // update Pages Menu highlight icon
	    for (int i=0; i<pageItems.size(); i++) {
		JMenuItem item = (JMenuItem) pageItems.get(i);
	    	Icon icon = (i == currPageItemInx) ?
		    glook().selectIcon() : null;
		item.setIcon(icon);
	    }

	    // create new card?
	    if (! cardNames.containSame(currPageName)) {
		JComponent c = null;
		if (currPage == null) 
		    c = new JLabel("No such page \"" + 
			currPageName + "\"");
		else
		    c = currPage.jcomp();
		panel.add(c, currPageName);
		cardNames.add(currPageName);
	    }

	    // push appropriate card forward and refresh
	    cards.show(panel, currPageName);
	    if (currPage != null) {
		currPage.refresh();
	    	currPage.setFocusTop();
	    }
	}

	// theme updated: reload current page
	public void lookUpdated() {
	    super.lookUpdated();
	    cardNames = new StringList(4);
	    if (pageItems != null)
	    	showPage();
	}

	// refresh current card page
	public void refresh() {
	    setTabLabel(l_hdr);
	    gmodel().refreshParSetMenu(mbar);
	    if (currPage != null) 
		currPage.refresh();
	}

	// remove a page
	public void removePage(GRTPage page) {
	    page.destroy();
	    connectRT();
	    gproject().refresh();
	}    

	// query
	public GRTDoc grtAuto() { return grtAuto; }

	// get page by name,  null if not found
	private GRTPage page(String name) {
	    GRTPage page = null;
	    if (grtCustom != null)
		page = grtCustom.page(name);	    
	    if (grtAuto != null && page == null) 
		page = grtAuto.page(name);
	    return page;
	}

	// get page item index via name
	private int pageItemInx(String name) {
	    for (int i=0; i<pageItems.size(); i++) {
		String pname = pageName(i);
		if (pname == null) continue;
		if (pname.equals(name)) return i;
	    }
	    return -1;
	}

	// get page name by page item index
	private String pageName(int n) {
	    JMenuItem item = (JMenuItem) pageItems.get(n);
	    String s = (String) item.getClientProperty("page");
	    if (s == null) s = item.getText();
	    return s;
	}

	// local query
	private int nCustomPages() {
	    return (grtCustom == null) ? 0 : grtCustom.npages();
	}
	private int nCustomMenuItems() {
	    return (grtCustom == null) ? 0 : grtCustom.nmenuItems();
	}
	private boolean hasCustomRTML() {
	    return nCustomPages() > 0 || nCustomMenuItems() > 0;
	}
}
