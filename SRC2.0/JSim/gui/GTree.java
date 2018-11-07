/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Project tree

package JSim.gui;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.model.*;

public class GTree extends GNode 
implements ListCellRenderer, MouseListener {

	// state
	private JList jlist;
	private Hashtable<GNode,Item> items; // item map
	private Vector<Item> vitems; // ordered items curr showing

	// actions
	public GAction blue, lightBlue, aqua, green, pink, violet, peach, gray, 
	    customColor, maxProc, bkupProj,
	    biggerFont, smallerFont, savePrefs,
	    sbwregister, sbwenable, sbwshow, sbwchange;
	public JMenuItem sbwtarget;

	// constants
	public final static Class[] itemClasses = {
	    PModel.class, ParSet.class, PDataSet.class, PImageSet.class, 
	    PlotPage.class, PNested.class, PGraphic.class, PNotes.class, 
	    PSemSim.class, Project.class };

	// constructor
	public GTree(GNode g) {
	    super(g, null);
	    items = new Hashtable<GNode,Item>();

	    // actions
	    maxProc = new GAction(this, "Multiprocessor configuration ...") {
	    	public void doit() throws Xcept {
		    maxProcDialog();
		}
	    };

		bkupProj = new GAction(this, "Auto backup project file" ) {
	    	public void doit() throws Xcept {
		    bkupProjDialog();
		}
	    };

	    blue = new GAction(this, "Blue background") {
		public void doit() { 
		    glook().resetColor(GLook.DEFAULT_BACKGROUND_RGB);
		}
	    };
	    lightBlue = new GAction(this, "Light blue background") {
		public void doit() { 
		    glook().resetColor(new int[] { 160, 210, 250 });
		}
	    };
	    aqua = new GAction(this, "Aqua background") {
		public void doit() { 
		    glook().resetColor(new int[] { 160, 220, 220 });
		}
	    };
	    green = new GAction(this, "Green background") {
		public void doit() { 
		    glook().resetColor(new int[] { 160, 210, 160 });
		}
	    };
	    pink = new GAction(this, "Pink background") {
		public void doit() { 
		    glook().resetColor(new int[] { 240, 200, 200 });
		}
	    };
	    violet = new GAction(this, "Violet background") {
		public void doit() { 
		    glook().resetColor(new int[] { 240, 200, 240 });
		}
	    };
	    peach = new GAction(this, "Peach background") {
		public void doit() { 
		    glook().resetColor(new int[] { 240, 200, 180 });
		}
	    };
	    gray = new GAction(this, "Gray background") {
		public void doit() { 
		    glook().resetColor(new int[] { 220, 220, 220 });
		}
	    };
	    customColor = new GAction(this, "Custom background color...") {
		public void doit() {
		    Color c = JColorChooser.showDialog(
			gproject().jcomp(), "Choose custom background color",
			glook().bg());
		    if (c == null) return;
		    int[] rgb = new int[] { c.getRed(), c.getGreen(), c.getBlue() };
		    glook().resetColor(rgb);
		}
	    };
	    biggerFont = new GAction(this, "Bigger font") {
		public void doit() {
		    int fs = glook().fontSize()+1;
		    glook().resetFont(fs); 
		}
	    };
	    biggerFont.setAccel(KeyEvent.VK_PLUS);
	    smallerFont = new GAction(this, "Smaller font") {
		public void doit() {
		    int fs = glook().fontSize()-1;
		    glook().resetFont(fs); 
		}
	    };
	    smallerFont.setAccel('-');
	    savePrefs = new GAction(this, "Save preferences") {
		public void doit() throws Xcept {
		    glook().setWinSize(gproject());
		    GPrefs p = new GPrefs(glook(),gappl().bkupProj);
		    File f = gappl().preferencesFile();
		    if (f == null) 
			gproject().securityXcept();
		    p.write(f);
		    gproject().message(
			"Saved user preferences in " + 
			f.getPath());
		}
	    };
	    sbwregister = new GAction(this, "Register with broker") {
	    	public void doit() throws Xcept {
		    gproject().message("Registering with SBW broker...");
		    gnode.gsbw().doRegister();
		}
	    };
	    sbwenable = new GAction(this, "Enable module") {
	    	public void doit() throws Xcept {
		    gproject().message("Enabling SBW module...");
		    gnode.gsbw().doModule();
		}
	    };
	    sbwshow = new GAction(this, "Target model") {
	    	public void doit() throws Xcept {
		    GModel target = gnode.gsbw().target();
		    if (target == null) return;
		    GProject gproj = target.gproject();
		    gproj.lefttabs().pushNode(target);
		    gproj.message("SBW target model is " +
		    	target.pmodel().name());
		}
		public boolean sbEnabled() {
		    return gnode.gsbw().target() != null;
		}
	    };
	    sbwchange = new GAction(this, "Set target model ...") {
	    	public void doit() throws Xcept {
		    GNode.List gmodels = 
		    	gproject().lefttabs().children(GModel.class);
		    StringList items = new StringList();
		    String knew = "<new model>";
		    items.add(knew);
		    for (int i=0; i<gmodels.size(); i++) 
		    	items.add(gmodels.gnode(i).pnamed().name());
		    String ret = (String) JOptionPane.showInputDialog(
		        gnode.jcomp(),
		    	"Select SBW model target", "SBW model target",
			JOptionPane.QUESTION_MESSAGE, null,
			items.array(), null);
		    int inx = items.indexOf(ret); 
		    GModel target = (inx == 0) ? 
		        gnode.gsbw().createNewTarget(gproject()) :
			(GModel) gmodels.gnode(inx-1);
		    gnode.gsbw().setTarget(target);
		    gproject().message("SBW model target set to " + 
		        target.pmodel().name());
		    gproject().refresh();
		}
		public boolean sbEnabled() {
		    return gnode.gsbw().isRunning();
		}
	    };
	    sbwtarget = sbwshow.item();

	    // create widgets
	    JRootPane root = new JRootPane();
	    root.setJMenuBar(createMenuBar());
	    Project proj = g.gproject().project();
	    JPanel jpanel = new JPanel(new BorderLayout());
	    jpanel.setBackground(Color.white);
	    JLabel jhdr = new JLabel("Project File Contents", 
	    	glook().projectIcon(), JLabel.CENTER);
	    jpanel.add(jhdr, BorderLayout.NORTH);
	    jlist = new JList();
	    jlist.addMouseListener(this);
	    jlist.setCellRenderer(this);
	    JScrollPane jscroll = new JScrollPane(jlist);
	    jpanel.add(jscroll, BorderLayout.CENTER);
//	    root.getContentPane().add(jpanel);
	    root.setContentPane(jpanel);
	    setJComp(root);

	    // universal font resizing keystrokes
	    JComponent pj = gproject().jcomp();
	    ActionMap amap = pj.getActionMap();
	    InputMap imap = pj.getInputMap(
		pj.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
	    KeyStroke bigger = KeyStroke.getKeyStroke(
		KeyEvent.VK_EQUALS,
		gproject().toolkit().getMenuShortcutKeyMask());
	    KeyStroke smaller = KeyStroke.getKeyStroke(
		KeyEvent.VK_MINUS,
		gproject().toolkit().getMenuShortcutKeyMask());
	    amap.put("biggerFont", biggerFont);
	    imap.put(bigger, "biggerFont");
	    amap.put("smallerFont", smallerFont);
	    imap.put(smaller, "smallerFont");

	}

	// create menubar
	private JMenuBar createMenuBar() {
	    JMenuBar mbar = new JMenuBar();
	    JMenu menu;

	    menu = newMenu("File");
	    menu.add(gproject().openProj.item());
	    menu.add(gproject().newProj.item());
	    menu.addSeparator();
	    menu.add(gproject().saveProj.item());
	    menu.add(gproject().saveProjAs.item());
	    menu.add(gproject().printTree.item());
	    menu.add(gproject().showMem.item());
	    menu.addSeparator();
	    menu.add(gproject().closeProj.item());
	    menu.add(gproject().exitProg.item());
	    mbar.add(menu);

	    menu = newMenu("Edit");
	    menu.add(gproject().cutNodes.item());
	    menu.add(gproject().copyNodes.item());
	    menu.add(gproject().pasteNodes.item());
	    menu.addSeparator();
	    menu.add(gproject().renameNodes.item());
	    menu.add(gproject().editDesc.item());
//	    menu.add(gproject().prefEdit.item());
	    mbar.add(menu);

	    menu = newMenu("Add");
	    menu.add(gproject().importModel.item());
	    menu.add(gproject().importPDataSet.item());
	    menu.add(gproject().importParSet.item());
	    menu.addSeparator();
	    menu.add(gproject().newModel.item());
	    menu.add(gproject().newPDataSet.item());
	    menu.add(gproject().newPlot.item());
	    menu.add(gproject().newNested.item());
	    menu.add(gproject().newGraphic.item());
	    menu.add(gproject().newNotes.item());
	    mbar.add(menu);

	    if (gsbw().isAvailable()) {
	    	menu = newMenu("SBW");
		menu.add(sbwenable.item());
		menu.add(sbwchange.item());
		menu.add(sbwtarget);
	    	menu.addSeparator();
		menu.add(sbwregister.item());
		mbar.add(menu);
	    }

	    menu = newMenu("Preferences");
	    menu.add(biggerFont.item());
	    menu.add(smallerFont.item());
	    menu.addSeparator();
	    menu.add(maxProc.item());
	    menu.addSeparator();
		menu.add(bkupProj.item());
		menu.addSeparator();
	    menu.add(blue.item());
	    menu.add(lightBlue.item());
	    menu.add(aqua.item());
	    menu.add(green.item());
	    menu.add(pink.item());
	    menu.add(violet.item());
	    menu.add(peach.item());
	    menu.add(gray.item());
	    menu.add(customColor.item());
	    menu.addSeparator();
	    menu.add(savePrefs.item());
	    mbar.add(menu);

	    // help menu
	    helpLinks().addHelpMenu(this, mbar);

	    return mbar;
	}

	// show maxProc dialog
	public void maxProcDialog() throws Xcept {
	    if (gmain().jobRunning()) throw new Xcept(
	    	"Can't configure multiprocessing while jobs are running");
	    NamedVal nval = server().getProperty("maxProc");
	    if (nval == null) throw new Xcept(
	    	"Remote server does not support multi-processing");
	    int nproc = nval.intVal();
	    int sproc = Runtime.getRuntime().availableProcessors();
	    if (sproc < 2) throw new Xcept(
	    	"This computer does not have multiple processors");
	    String[] msg = { "JSim currently using " + nproc +
		"compute thread(s)",
	    	"     on " + sproc + " system processors.",
		"Enter # threads to use:" };
	    String s = (String) JOptionPane.showInputDialog(
	    	jcomp(),  msg, "Multiprocessor configuration",
		JOptionPane.QUESTION_MESSAGE,
		glook().modelIcon(),
		null, "" + nproc);
	    if (s == null) return;
	    nproc = Util.toInt(s);
	    nval = NamedVal.create("maxProc", nproc);
	    server().setProperty(nval);
	    nval = server().getProperty("maxProc");
	    nproc = nval.intVal();
	    gproject().message("JSim now using " + nproc +
	    	" compute thread(s) on " + sproc + " system processors");
	}

	public void bkupProjDialog() throws Xcept {

		String bkupValue = "";
		if(gmain().getBkupProj() == true) bkupValue = "Yes";
		else  bkupValue = "No";
		String bkupMSG = "Always backup project file after successful run? (Currently: "+bkupValue+")";
		int a =JOptionPane.showConfirmDialog(jcomp(),bkupMSG,"Backup project file",
				 JOptionPane.YES_NO_OPTION,0, glook().modelIcon());
		if(a == 0) {
			gmain().setBkupProj(true); // Ideally should only set in one place. Why can't we?
			gappl().bkupProj = true;
		}
		else {
			gmain().setBkupProj(false);
			gappl().bkupProj = false;
		}
	}

	// get list of selected GNodes in tree
	public PNamed.List getSelectedNodes() throws Xcept {
	    Object[] vals = jlist.getSelectedValues();
	    PNamed.List list = new PNamed.List(4);
	    for (int i=0; i<vals.length; i++) 
		list.add(((Item) vals[i]).pnamed);
	    if (list.size() == 0) throw new Xcept(
		"No content is selected within project tree");
	    return list;
	}

	// look update: JList doesn't propagate to list items jcomps
	public void lookUpdated() {
	    for (int i=0; i<vitems.size(); i++) 
	    	SwingUtilities.updateComponentTreeUI(vitems.get(i).box);
	    super.lookUpdated();
	}

	// refresh
	public void refresh() {
	    vitems = new Vector<Item>(items.values());
	    Collections.sort(vitems);
	    jlist.setListData(vitems);
	    for (int i=0; i<vitems.size(); i++)
	    	vitems.get(i).refresh();
	    GModel target = gsbw().target();
	    String s = (target == null) ? 
	    	"<none>" : target.pmodel().name();
	    sbwtarget.setText("Model target: " + s);
	    super.refresh();
	}

	// add item
	public void add(GNode gnode) {
	    Item item = new Item(gnode);
	    items.put(gnode, item);
	    refresh();
	}
	
	// remove item
	public void remove(GNode gnode) {
	    items.remove(gnode);
	    refresh();
	}

	// item renamed
	public void renamed(GNode gnode) {
	    Item item = items.get(gnode);
	    if (item != null)
	    	item.refresh();
	    refresh();
	}

	// get GNode for PNamed
	public GNode gnode(PNamed pnamed) {
	    for (int i=0; i<vitems.size(); i++) {
	    	Item item = vitems.get(i);
	    	if (item.pnamed == pnamed)
		    return item.gnode;
	    }
	    return null;
 	}

	// description for PNamed
	public String desc(PNamed pnamed) {
	    String desc = pnamed.desc();
	    if (! Util.isBlank(desc)) return desc;
	    Class c = pnamed.getClass();
	    if (c == PModel.class) return "model";
	    if (c == ParSet.class) return "parameter set";
	    if (c == PDataSet.class) return "data set";
	    if (c == PImageSet.class) return "image set";
	    if (c == PNotes.class) return "notes";
	    if (c == PSemSim.class) return "semsim";
	    if (c == PGraphic.class) return "graphic plugin";
	    if (c == PlotPage.class) return "plot page";
	    if (c == PNested.class) return "nested plot";
	    return "???";
	}
	     
	// cell renderer
	public JComponent getListCellRendererComponent(JList list, 
	Object oitem, int inx, boolean isSelected, 
	boolean cellHasFocus) {
	    Item item = (Item) oitem;
	    Color c = isSelected ? Color.gray : Color.black;
	    item.jname.setForeground(c);
	    item.jdesc.setForeground(c);
 	    item.box.setBorder(isSelected ? item.border : null);
	    return item.box;
	}


	// double-click shows tab
	public void mouseClicked(MouseEvent e) {
	    if (e.getClickCount() != 2) return;
	    int inx = jlist.locationToIndex(e.getPoint());
	    if (vitems == null || inx >= vitems.size()) return;
	    gproject().pushTab(vitems.get(inx).gnode);
	} 

	// unused mouse listener methods
	public void mouseEntered(MouseEvent e) { } 
	public void mouseExited(MouseEvent e) { } 
	public void mousePressed(MouseEvent e) { } 
	public void mouseReleased(MouseEvent e) { } 

	// Item data
	public class Item implements Comparable<Item> {
	    public GNode gnode;   // tab node
	    public PNamed pnamed; // proj content
	    public Box box;       // container
	    public JLabel jname; // name & icon
	    public JLabel jdesc;  // desc
	    public Border border; // border, if selected

	    // constructor
	    public Item(GNode gnode) {
	        this.gnode = gnode;
		pnamed = gnode.pnamed();
		Icon icon = glook().tabIcon(pnamed.getClass());
		box = new Box(BoxLayout.X_AXIS);
		jname = new JLabel(pnamed.name(), icon, JLabel.LEFT);
		jdesc = new JLabel(desc(pnamed));
		box.add(jname);
		box.add(jdesc);
		border = new EtchedBorder();
		refresh();
	    }

	    // list order
	    public int compareTo(Item item) {
	     	PNamed p = pnamed;
		PNamed q = item.pnamed;
	    	int pc = compareOrder(p.getClass());
	    	int qc = compareOrder(q.getClass());
	    	if (pc != qc) return pc-qc;
	    	return p.name().compareTo(q.name());
	    }
	    private int compareOrder(Class c) {
	    	for (int i=0; i<itemClasses.length; i++) 
	    	    if (c == itemClasses[i]) return i;
	    	return itemClasses.length;
	    }

	    // refresh appearance
	    public void refresh() {
		jname.setText(pnamed.name());
		jname.setSize(jname.getPreferredSize());
		jdesc.setText("  (" + desc(pnamed) + ")");
		jdesc.setSize(jdesc.getPreferredSize());
	    }

	}

}
