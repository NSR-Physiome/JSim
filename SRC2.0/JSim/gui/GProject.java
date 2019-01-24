/*NSRCOPYRIGHT
	Copyright (C) 1999-2019 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Project content

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import org.w3c.dom.Element;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.model.*;
import JSim.aserver.*;

public class GProject extends GNode implements MouseListener {

	private static int lastSeq = 0;
	private static long lastBeep = 0;

	// state info
	private Project project;
	private int seq; // sequence # for window
	private JSReadable file; // load/save file, or null
	private JRootPane root;
	private JFrame frame; // created frame,  if any
	private JLabel editline; // display editor line # 
	private JLabel statline; // status line
	private boolean statlineCleared; // is statline cleared
	private GAbstractTabs lefttabs, righttabs;
	private GTree gtree;
	private GMessage gmessage;
	private GHelp ghelp;
	private int currEditLine;

	// actions
	public GAction openProj, newProj, 
	    importModel, importPDataSet, importParSet,
	    newModel, newPDataSet, newPlot, newNested, newNotes, newGraphic,
	    saveProj, saveProjAs, printTree, 
	    closeProj, showMem, exitProg;
	public GAction cutNodes, copyNodes, pasteNodes, renameNodes, editDesc;

	// constructor,  if new frame rootPane == null
	public GProject(GMain m, JRootPane r, boolean splash) throws Xcept {
	    super(m, new Project(m.newProjectName(), m.gappl()));
	    project = (Project) pnamed();
	    seq = ++lastSeq;
	    frame = null;
	    root = r;
	    currEditLine = -1;

	    // create new Frame, if required
	    Dimension winDim = glook().winDim();
	    if (root == null) {
		frame = new JFrame("JSim GUI") {
		    public void processWindowEvent(WindowEvent e) {
			if (e.getID() == WindowEvent.WINDOW_CLOSING)
			    windowClosing();
			else
			    super.processWindowEvent(e);
		    }
		};
		setFile(null);
		root = frame.getRootPane();
		root.setPreferredSize(winDim);
	    } else {
		winDim = root.getSize();
	    }
	    setJComp(root);
	    Container content = root.getContentPane();

	    // applet bg grey without this
	    content.setBackground(glook().bg()); 

	    // create 
	    ghelp = new GHelp(this); 
 	    switch (gappl().tabType) {
	    case 1:
  	    	lefttabs = new GLeftProj(this, project);
		righttabs = new GTabs(this, project);
		break;
	    case 2:
		String[] leftNames = new String[] {
		    "Project", "Models", "ParSets", "DataSets", "Notes"
		};
		String[] rightNames = new String[] {
		    "Message", "Plot pages", "Nested Plots", "Graphics"
		};
		Class[] leftClasses = new Class[] {
		    GTree.class, GModel.class, GParSet.class,
		    GDataSet.class, GNotes.class
		};
		Class[] rightClasses = new Class[] {
		    GMessage.class, GPlotPage.class, GNested.class, GGraphicTab.class
		};
  	    	lefttabs = new GButtonTabs(this, project,
		    leftNames, leftClasses);
		righttabs = new GButtonTabs(this, project,
		    rightNames, rightClasses);
		break;
	    default:
	    	lefttabs = GCompactTabs.newProjectLeft(this, project);
  	    	righttabs = GCompactTabs.newProjectRight(this, project);
		break;
	    }

	    // left/right side sizing and split
	    JComponent jleft = lefttabs.jcomp();
	    jleft.setFont(glook().bigFont());
     	    jleft.setPreferredSize(
		new Dimension(winDim.width/2, winDim.height));
	    jleft.setMinimumSize(new Dimension(100, 100));
	    JComponent jright = righttabs.jcomp();
	    jright.setFont(glook().bigFont());
	    jright.setMinimumSize(new Dimension(100, 100));
	    JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, 
	    	jleft, jright);
	    content.add(split, BorderLayout.CENTER);

	    // status line
	    Box statbox = new Box(BoxLayout.X_AXIS);
	    editline = new JLabel();
	    setEditLine(0);
	    editline.setFont(glook().textFont());
	    statbox.add(editline);
	    statline = new JLabel(
		"Use the F1 key for context-sensitive help. " +
		"See any Help menu...");
	    statline.setOpaque(true);
	    statline.addMouseListener(this);
	    statbox.add(statline);
	    content.add(statbox, BorderLayout.SOUTH);
	    statlineCleared = false;

	    // open local project action
	    openProj = new GAction(this, "Open project file ...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, true, GFileChooser.PROJ);
		    if (info == null) return;
		    loadFile(info.file);
		}
	    };	
	    openProj.setAccel('O');

	    // new project action
	    newProj = new GAction(this, "New project ...") {
		public void doit() throws Xcept {
		    GProject gproj = new GProject(gmain(), null, false);
		    gproj.addDefaultTabs();
		    gproj.load(false);
		}
	    };

	    // show memory monitor
	    showMem = new GAction(this, "Show memory usage ...") {
	        public void doit() throws Xcept {
		    gmain().gmem().show(gproject());
		}
	    };

	    // import model file into project action
	    importModel = new GAction(this, "Import model file (.mod +)") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, true, GFileChooser.MODEL);
		    if (info == null) return;
		    loadFile(info.file);
		}
		public boolean sbEnabled() { return !gnode.gmain().jobRunning(); }
	    };		
	    importModel.setAccel('M');

	    // import data file into project action
	    importPDataSet = new GAction(this, "Import data file (.tac +)") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, true, GFileChooser.DATASET);
		    if (info == null) return;
		    loadFile(info.file);
		}
		public boolean sbEnabled() { return !gnode.gmain().jobRunning(); }
	    };		
	    importPDataSet.setAccel('D');

	    // import par file into project action
	    importParSet = new GAction(this, "Import parameter file (.par)") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, true, GFileChooser.PARSET);
		    if (info == null) return;
		    loadFile(info.file);
		}
		public boolean sbEnabled() { return !gnode.gmain().jobRunning(); }
	    };		

	    // create new model action
	    newModel = new GAction(this, "New model") {
		public void doit() throws Xcept {

		    // pick model name
		    String msg = "Enter name for new model";
		    String n = project.newChildName("model", true);
		    String nname = (String) JOptionPane.showInputDialog(
			    gtree().jcomp(), msg, "Enter name", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().modelIcon(),
			    null, n);
		    if (nname == null) return;

		    // pick easel plugin or text,  if easel available
		    String pname = "text";
		    Plugin.List plugins = project.appl().plugins();
		    plugins = plugins.plugins("ModelEasel");
		    String[] names = new String[plugins.size()+1];
		    names[0] = pname;
		    for (int i=1; i<names.length; i++) 
		        names[i] = plugins.plugin(i-1).variant();
		    if (plugins.size() > 0) {
		    	pname = (String) JOptionPane.showInputDialog(
                            jcomp(), 
			    "Choose easel plugin variant",
			    "Create plugin easel",
                            JOptionPane.QUESTION_MESSAGE,
                            glook().graphicIcon(),
                            names, null);
                    	if (pname == null) return;
		    }

		    // create model
		    PModel pmodel = new PModel(project, nname);
		    if (! pname.equals("text"))
		    	pmodel.easelVariant.setVal(pname);
		    add(pmodel);
		    refresh();
		}
	    };

	    // create new dataset action
	    newPDataSet = new GAction(this, "New data set") {
		public void doit() throws Xcept {
		    String msg = "Enter name for new data set";
		    String n = project.newChildName("data", true);
		    String nname = (String) JOptionPane.showInputDialog(
			    gtree().jcomp(), msg, "Enter name", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().datasetIcon(),
			    null, n);
		    if (nname == null) return;
		    add(new PDataSet(project, nname));
		    refresh();
		}
	    };

	    // create new plot page action,  push forward
	    newPlot = new GAction(this, "New plot page") {
		public void doit() throws Xcept {
		    String n = project.newChildName("plotpage", true);
		    add(new PlotPage(project, n));
		    righttabs.pushLast();
		}
	    };
	    newPlot.setAccel('P');

	    // create new nested plot action,  push forward
	    newNested = new GAction(this, "New nested plot") {
		public void doit() throws Xcept {
		    String n = project.newChildName("nested", true);
		    add(new PNested(project, n));
		    righttabs.pushLast();
		}
	    };

	    // create new graphic plugin
	    newGraphic = new GAction(this, "New graphic (plugin)") {
	    	public void doit() throws Xcept {
		    Plugin.List plugins = project.appl().plugins();
		    plugins = plugins.plugins("Graphic");
		    if (plugins.size() < 1) throw new Xcept(
		    	"No Graphic plugins are currently available");
		    String[] names = new String[plugins.size()];
		    for (int i=0; i<names.length; i++) 
		        names[i] = plugins.plugin(i).variant();
		    String name = (String) JOptionPane.showInputDialog(
                            jcomp(), 
			    "Choose Graphic plugin variant",
			    "Create plugin graphic",
                            JOptionPane.QUESTION_MESSAGE,
                            glook().graphicIcon(),
                            names, null);
                    if (name == null) return;
		    String n = project.newChildName("graphic", true);
		    PGraphic pgraphic = new PGraphic(project, n);
		    pgraphic.variant.setVal(name);
		    add(pgraphic);
		    righttabs.pushLast();
		}
	    };

	    // create new notes action
	    newNotes = new GAction(this, "New notes page") {
		public void doit() throws Xcept {
		    String n = project.newChildName("notes", true);
		    add(new PNotes(project, n));
		}
	    };

	    // save project action	
	    saveProj = new GAction(this, "Save") {
		public void doit() throws Xcept {
		    if (file != null && file.isFile()) 
		    	saveTo(file.file());
		    else saveProjAs.doit();
		}
		public String revisedText() {
		    return project.changed ?
			"Save (needed)" : "Save (not needed)";
		}
	    };
	    saveProj.setAccel('S');

	    // save project as ... action
	    saveProjAs = new GAction(this, "Save as ...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, false, GFileChooser.PROJ);
		    if (info == null) 
			throw new Xcept("Save cancelled");
		    File f = info.file;
		    if (! UtilIO.fileSuffix(f).equals("proj")) 
			f = new File(f.getPath() + ".proj");
		    saveTo(f);
		}
	    };
//	    saveProjAs.setAccel('A');

	    printTree = new GAction(this, "Print project tree ...") {
		public boolean sbEnabled() { return false; }
	    };

	    // close/exit program action
	    closeProj = new GAction(this, "Close project") {
		public void doit() throws Xcept { closeRequest(); }
	    };
	    exitProg = new GAction(this, "Exit program") {
		public void doit() throws Xcept {
		    gmain().exitRequest();
		}
	    };

	    // cut and copy to paste buffer
	    cutNodes = new GAction(this, "cut") {
		public void doit() throws Xcept {
		    PNamed.List list = gtree().getSelectedNodes(); 
		    for (int i=0; i<list.size(); i++) {
			PNamed pnamed = list.pnamed(i);
			if (ok2Remove(pnamed))
			    removeChild(pnamed);
		    }
		    gmain().setEditBuffer(list);
		    project().revalidate();
		    refresh();
		}
		public boolean sbEnabled() { return !gnode.gmain().jobRunning(); }		
	    };

	    // copy into paste buffer
	    copyNodes = new GAction(this, "copy") {
		public void doit() throws Xcept {
		    PNamed.List list = gtree().getSelectedNodes(); 
		    gmain().setEditBuffer(list);
		}
		public boolean sbEnabled() { return !gnode.gmain().jobRunning(); }
	    };

	    // copy paste buffer into this project
	    pasteNodes = new GAction(this, "paste") {
		public void doit() throws Xcept {
		    if (gmain().xmlBuffer == null) return;

		    for (int i=0; i<gmain().xmlBuffer.length; i++) {
			Element e = gmain().xmlBuffer[i];
			if (e == null) continue;
			PNamed p = project.importXMLChild(e);
		    	if (p instanceof PModel)
			    ((PModel) p).clearRT();
			add(p);
		    }
	    	    lefttabs.pushFirst();
		    project().revalidate();
		    refresh();
		}
		public boolean sbEnabled() { return !gnode.gmain().jobRunning(); }
	    };

	    // rename nodes
	    renameNodes = new GAction(this, "rename ...") {
		public void doit() throws Xcept {
		    PNamed.List list = gtree().getSelectedNodes();
		    for (int i=0; i<list.size(); i++) {
			PNamed pchild = list.pnamed(i);
			String msg = "Rename node " + pchild.name();
			String nname = (String) JOptionPane.showInputDialog(
			    gtree().jcomp(), msg, "Rename", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().userIcon(),
			    null, pchild.name());
			if (nname == null) return;
			if (nname.equals(pchild.name())) return;
			rename(pchild, nname);
		    	refresh();
		    }
		}
		public boolean sbEnabled() { return !gnode.gmain().jobRunning(); }
	    };
	    renameNodes.setAccel('R');

	    // edit node desc
	    editDesc = new GAction(this, "edit description ...") {
		public void doit() throws Xcept {
		    PNamed.List list = gtree().getSelectedNodes();
		    for (int i=0; i<list.size(); i++) {
			PNamed pchild = list.pnamed(i);
			String msg = "Edit description of " + pchild.name();
			String desc = gtree().desc(pchild);
			desc = (String) JOptionPane.showInputDialog(
			    gtree().jcomp(), msg, "Edit descritpion", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().userIcon(),
			    null, desc);
			if (desc == null) return;
			StringControl pdesc = pchild.descCntl;
			if (pdesc != null) pdesc.setVal(desc);
			gtree().refresh();
		    	refresh();
		    }
		}
		public boolean sbEnabled() { return !gnode.gmain().jobRunning(); }
	    };
	    editDesc.setAccel('Q');

	    // project help & tabs
	    gtree = new GTree(lefttabs);
	    addTab("Project", gtree);
	    gmessage = new GMessage(righttabs);
	    addTab("Message", gmessage);

	    // other top-level tabs
	    pushTree();
	    pushMessages();

	    // refresh content, set visible
	    refresh();
	    if (frame != null) {
	    	frame.pack();
	    	frame.setVisible(true);
	    } 
	}

	// add default project tabs
	public void addDefaultTabs() throws Xcept {
	    new PlotPage(project, "plotpage_1");
	}

	// load .proj or subsidiary file
    	protected void loadFile(File f) throws Xcept {
	    String sfx = UtilIO.fileSuffix(f);
	    // load project
	    if (sfx.equals("proj")) {
		loadProjFile(new JSReadable(f));
		return;
	    }
	    
	    // load subsidiary file
	    JSReadable r = new JSReadable(f);
	    if (skipDubious(r)) {
	    	message("Canceled loading file " + f + " at user request");
		return;
 	    }
	    PNamed pnamed = project.load(r);
	    project.revalidate();
	    add(pnamed);
	    refresh();
	    message("Imported " + f + " as " + pnamed.diagInfo());
	}

	// skip JSReadable with dubious content
	public boolean skipDubious(JSReadable r) throws Xcept {
	    if (! r.isContentDubious()) return false;
	    int stat = JOptionPane.showConfirmDialog(
	    	jcomp(), 
		"Content of " + r + " doesn't look like Antimony.  Load anyway?",
		"Dubious content warning",
		JOptionPane.YES_NO_OPTION,
		JOptionPane.WARNING_MESSAGE);
	    return stat != JOptionPane.YES_OPTION;
	}

	// load .proj file
	private void loadProjFile(JSReadable readable) throws Xcept {

	    // blank current GProject or create new
	    GProject gproj = null;
	    if (project.nChild()== 1 && 
		(project.child(0) instanceof PlotPage)) {
		removeChild(project.child(0));
	   	project = new Project(
		    gmain().newProjectName(), gmain().gappl());
		resetPNamed(project);
	        gproj = this;
	    } else {
		gproj = new GProject(gmain(), null, true);
	    }

	    // initialize with file
	    gproj.setFile(readable);
	    gproj.project().importXML(readable);
	    gproj.project().revalidate();
	    gproj.load(false);
	}

	// load project children
	public void load(boolean demo) throws Xcept {
	    for (int i=0; i<project.nChild(); i++) 
		add(project.child(i));
	    projectUnchanged();

	    // restore build state of models
	    GModel.List gmodels = lefttabs.children(GModel.class);
	    for (int i=0; i<gmodels.size(); i++) {
		GModel gmodel = (GModel) gmodels.gnode(i);
		PModel pmodel = gmodel.pmodel();
		boolean dmodel = (demo && i==0);
		if (pmodel.built.val() || dmodel) {
	    	    gmain().runJob(
			new GModelJob.Build(gmodel, dmodel), true);
		}
	    }
	    if (demo && gmodels.size() == 0) demo(null);
	}

	// demo mode
	public void demo(GModel gmodel) {
	    // push model
	    if (gmodel != null) lefttabs.pushNode(gmodel);
	    PModel pmodel = 
	        (gmodel == null) ? null : gmodel.pmodel();
	
	    // plot exprs adjustment?
	    try {
	    	StringList exprs = demoExprs(gmodel);
	    	GNode.List gpages = 
		    righttabs.children(GPlotPage.class);
		GPlotPage gpage = null;
		boolean addExprs = true;
		for (int i=0; i<gpages.size(); i++) {
		    GPlotPage igpage = (GPlotPage) gpages.gnode(i);
		    StringList istrs = new StringList();
		    igpage.page().addExprs(pmodel, istrs);
		    if (istrs.size() > 0 && !istrs.containSame(exprs)) 
		        continue;
		    gpage = igpage;
		    if (istrs.size() > 0) addExprs = false;
		    break;
		}
		if (gpage == null) {
		    String n = project.newChildName("plotpage", true);
		    gpage = (GPlotPage) add(new PlotPage(project, n));
	    	}
		if (addExprs) {
		    Plot plot = gpage.page().plot(0);
		    plot.insertExprs(pmodel, exprs);		    
		}
	    	righttabs.pushNode(gpage);
	    } catch (Xcept e) {
	    	message(e);
   	    }
	}

	// get plot expressions
	private StringList demoExprs(GModel gmodel) throws Xcept {   
	    StringList exprs = gappl().demoExprs;
	    if (gmodel == null) return exprs;
	    if (exprs.size() != 1) return exprs;
	    
	    // see if numeric
	    try { 
	    	int ct = Util.toInt(exprs.str(0));
		ASVar.List vars = gmodel.pmodel().rt().getASVars();
		exprs = new StringList();
		int tot = 0;
		for (int i=0; tot<ct && i<vars.size(); i++) {
		    ASVar v = vars.asvar(i);
		    if (v.ndim() != 1) continue;
		    if (v.isInput()) continue;
		    if (v.isDomain()) continue;
		    exprs.add(v.toString());
		    tot++;
		}
	    } catch (Xcept e) { /* nothing to do */ }
	    return exprs;
	}

	// set project changed=false
	public void projectUnchanged() {
	    project().changed = false;
	}

	// tabs management
	public void addTab(GNode gnode) {
	    PNamed p = gnode.pnamed();
	    String n = gnode.pnamed().name();
	    addTab(n, gnode);
	}
	public GAbstractTabs whichTabs(GNode gnode) {
	    if (gnode instanceof GMessage) return righttabs;
	    if (gnode instanceof GPlotPage) return righttabs;
	    if (gnode instanceof GNested) return righttabs;
	    if (gnode instanceof GGraphicTab) return righttabs;
	    return lefttabs;
	}
	public void addTab(String name, GNode gnode) {
	    whichTabs(gnode).addTab(name, gnode);
	}
	public void removeTab(GNode gnode) {
	    whichTabs(gnode).removeTab(gnode);
	}
	public void renameTab(GNode gnode, String n) {
	    whichTabs(gnode).renameTab(gnode, n);
	}
	public void pushTree() { lefttabs.pushFirst(); }
	public void popTree() { lefttabs.pop(); }
	public void pushMessages() { righttabs.pushFirst(); }
	public void popMessages() { righttabs.pop(); }

	// push arbitrary tab
	public void pushTab(GNode gnode) {
	    whichTabs(gnode).pushNode(gnode);
	}

	// clone a node and refresh
	public void cloneNode(GNode gnode) throws Xcept {
	    Element e = gnode.pnamed().exportXML();
	    PNamed p = project().importXMLChild(e);
	    add(p);
	    project().revalidate();
	    // set tab forward
	   refresh();
	}

	// remove a node and refresh
	public void removeNode(GNode gnode) throws Xcept {
	    if (! ok2Remove(gnode.pnamed())) return;
	    removeChild(gnode.pnamed());
	    project().revalidate();
	    refresh();
	}    

	// dialog to remove node
	public boolean ok2Remove(PNamed pnamed) {
	    int stat = JOptionPane.showOptionDialog(jcomp(), 
		"Are your sure you want to remove " + pnamed.diagInfo() + "?",
		"Remove from project", JOptionPane.YES_NO_OPTION,
		JOptionPane.WARNING_MESSAGE, null,
		new String[] { "YES", "NO" }, "NO");
	    return stat == JOptionPane.OK_OPTION;
	}

	// window manager requested close
	public void windowClosing() {
	    statlineClear();
	    try {
		closeRequest();
	    } catch (Xcept e) {
		warning(e.cleanMessage());
	    }
	}

	// close request
	public void closeRequest() throws Xcept {

	    // ask about save if project changed
	    if (project.changed) {
		if (frame != null) 
		    frame.setVisible(true); // bring forward
		String[] options = {"Save", "Don't Save", "Cancel"};
		int which = JOptionPane.showOptionDialog(root,
		    "Some project data not yet saved.  Save before closing?", 
		    "Closing " + title(), 
		    JOptionPane.YES_NO_CANCEL_OPTION,
		    JOptionPane.QUESTION_MESSAGE,
 		    glook().userIcon(),
			options,options[0]);
		boolean cancel = 
		    (which == JOptionPane.CANCEL_OPTION);
		if (which == JOptionPane.YES_OPTION) {
		    saveProj.doit();
		    cancel = project.changed;
		}
		if (cancel)
		    throw new Xcept("Project close canceled");
	    }

	    // go ahead with project close
	    ghelp.exit();
	    gsbw().projectClosing(this);
	    gappl().removeProject(project);
	    gproject().destroy(); // remove from main
	    frame.setVisible(false);
	    frame.dispose();
	    
	    // if last close, exit
	    if (gmain().ngprojs()<1) 
	    	gmain().exitNow();
	}

	// save to a file
	private void saveTo(File file) throws Xcept {
	    try {
		// save graphic-editable states (if any)
		exportGraphicState();
/*		GNode.List graphics = 
		     gproject().righttabs().children(GGraphicTab.class);
		for (int i=0; i<graphics.size(); i++) {
		    GGraphicTab g = (GGraphicTab) graphics.gnode(i);
		    System.err.println("saving " + g.pnamed().name());
		    g.exportGraphicState();
		}
*/
		// write project file
	    	OutputStream out = new FileOutputStream(file);
	    	PrintStream pout = new PrintStream(out);
	    	project.writeXML(pout);
	    	pout.close();
		
		// notify user
	    	message("Project saved to file " + file);
		setFile(new JSReadable(file));
		projectUnchanged();
	    } catch (FileNotFoundException e) {
		throw new Xcept(e.getMessage());
	    } catch (SecurityException e) {
		securityXcept();
	    }
	}

	// job is starting stopping
	public void jobStartStop(PJob job, boolean start) {
	    GNode.List gnodes = righttabs.children(GGraphicTab.class);
	    	for (int i=0; i<gnodes.size(); i++) 
		    ((GGraphicTab) gnodes.gnode(i)).jobStartStop(
		   	job, start);
	}

	// live data update during run
	public void liveUpdate(PJob job) {
	    if (! gappl().liveUpdate) return;
	    if (! (job instanceof PModelRunJob) &&
	  	! (job instanceof PModelLoopsJob))
	    	return;
	    GNode gnode = righttabs.selectedNode();
	    if (gnode instanceof GPlotPage)
		((GPlotPage) gnode).liveUpdate();
	}

	// query
	public JSReadable file() { return file; }
	public Project project() { return project; }
	public JFrame frame() { return frame; }
	public GTree gtree() { return gtree; }
	public GHelp ghelp() { return ghelp; }
	public String title() {
	    return "Project " +  ((file == null) ? 
		("<untitled-" + seq + ">") : file.truncString(80));
	}
	public Dimension rootSize() {
	    if (frame == null) return null; // if applet, no preference info
	    return root.getSize();
	}
	public GAbstractTabs lefttabs() { return lefttabs; }
	public GAbstractTabs righttabs() { return righttabs; }

	// set properties
	public void setFile(JSReadable readable) {
	    file = (readable == null) ? null : readable; 
	    if (frame == null) return;
	    frame.setTitle("JSim version " +
		Util.version() + ": " + title());
	}

	// show frame (if any)
	public void pushFrameFront() {
	    if (frame == null) return;
	    frame.setExtendedState(Frame.NORMAL);
	    frame.setVisible(true);
	    frame.toFront();
	}

	// set edit line no
	public void setEditLine(int i) {
	    if (i == currEditLine) return;
	    currEditLine = i;
	    editline.setText("line " + currEditLine + "  ");
	}

	// warning message in this window
	public void warning(Exception e) {
	    String msg = (e instanceof Xcept) ?
	    	((Xcept) e).cleanMessage() : e.getMessage();
	    if (Util.isBlank(msg)) msg = "" + e;
	    warning(msg);
	    if (gappl().stackTrace) e.printStackTrace();
	}
	public void warning(String s) {
	    statline.setText(s);
	    statlineCleared = false;
	    gmessage.message(s);
	    beepBright();
	}

	// beep and set background bright
	private void beepBright() {
	    statline.setBackground(glook().bright());

	    // beep no more frequently than every 2 seconds
	    long t = System.currentTimeMillis();
	    if (t-lastBeep > 2000) toolkit().beep();
	    lastBeep = t;
	}

	// general message in this window
	public void message(Exception e) {
	    String msg = (e instanceof Xcept) ?
	    	((Xcept) e).cleanMessage() : e.getMessage();
	    if (Util.isBlank(msg)) msg = "" + e;
	    message(msg);
	    if (gappl().stackTrace) e.printStackTrace();
	    beepBright();
	}
	public void message(String s) {
	    statline.setText(s);
	    statlineCleared = false;
	    statline.setBackground(glook().bg());
	    gmessage.message(s);
	}
	public void messages(String[] ss) {
	    if (ss == null) return;
	    for (int i=0; i<ss.length; i++)
	    	message(ss[i]);
	}
	public void messages(boolean hasWarn, String[] ss) {
	    if (ss == null || ss.length == 0) return;
	    for (int i=0; i<ss.length; i++)
	    	message(ss[i]);
	    if (hasWarn) beepBright(); 
	}

	// statline clearing, MouseListener
	public void statlineClear() {
	    // test boolean first for speed
	    if (statlineCleared) return;
	    statline.setText("");
	    statlineCleared = true;
	}
	public void mouseClicked(MouseEvent e) {
	    statlineClear();
	}
	public void mouseEntered(MouseEvent e) { }
	public void mouseExited(MouseEvent e) { }
	public void mousePressed(MouseEvent e) { }
	public void mouseReleased(MouseEvent e) { }

	// security message
	public void securityXcept() throws Xcept {
	    String where = isApplet() ? 
		"untrusted WWW applet" : "JSim sandbox";
	    throw new Xcept(
		"Operation not allowed in " + where + ".");
	}

	// add a PNamed to gproject
	public GNode add(PNamed p) throws Xcept {
	    project.revalidate();
	    GNode gnode = null;
	    if (p instanceof PModel) 
		gnode = new GModel(lefttabs, (PModel) p);
	    else if (p instanceof PDataSet) 
		gnode = new GDataSet(lefttabs, (PDataSet) p);
	    else if (p instanceof ParSet) 
		gnode = new GParSet(lefttabs, (ParSet) p);
	    else if (p instanceof PlotPage) 
		gnode = new GPlotPage(righttabs, (PlotPage) p);
	    else if (p instanceof PNested) 
		gnode = new GNested(righttabs, (PNested) p);
	    else if (p instanceof PGraphic) 
		gnode = new GGraphicTab(righttabs, (PGraphic) p);
	    else if (p instanceof PNotes) 
		gnode = new GNotes(lefttabs, (PNotes) p);
	    else if (p instanceof PSemSim) 
		gnode = new GSemSim(lefttabs, (PSemSim) p);
	    else if (p instanceof PImageSet) {
		gnode = new GImageSet(lefttabs, (PImageSet) p);
		addTab(gnode);
	    }
	    gtree().add(gnode);
	    return gnode;
	}

	// rename a project child
	public void rename(PNamed pchild, String n) throws Xcept {
	    String safe = PNamed.safeName(n);
	    if (! n.equals(safe)) throw new Xcept(
		"Rename failed:  name \"" + n + "\" contains illegal characters");
	    if (pchild.parent().child(n) != null) throw new Xcept(
		"Rename failed: name \"" + n + "\" already exists in project");
	    message(pchild.diagInfo() + " will be renamed " + n);

	    String oldn = pchild.name();
	    GNode gchild = gtree().gnode(pchild);
	    if (gchild != null) renameTab(gchild, n);
	    pchild.rename(n);
	    gtree().renamed(gchild);
	    project.revalidate();
	}

	// remove project child
	public void removeChild(PNamed p) throws Xcept {

	    // destroy any associated widgets
	    GNode gnode = gtree().gnode(p);
	    if (gnode != null) {
	    	gnode.destroy();
	    	removeTab(gnode);
	        if (gnode == gsbw().target())
	    	    gsbw().setTarget(null);
	    }
	    gtree().remove(gnode);
	    if (! project.remove(p)) warning(
		p.name() + " not removed from project");
	}	    

}
