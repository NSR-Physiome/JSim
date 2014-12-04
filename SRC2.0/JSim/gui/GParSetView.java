/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// ParSet viewer

package JSim.gui;

import java.io.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;

public class GParSetView extends GEditor {

	// state
	private GParSetSelect gselect;
	private GBooleanControl glocked;
	private JTextArea text;
	private ParSet parset;
	private PrintWriter writer;
	private JLabel l_hdr;
	private JLabel l_lock;
	private GAction writeFile, importPars;

	// constructor from PNotes
	public GParSetView(GNode p, ParSet ps) {
	    super(p, ps);
	    parset = ps;
	    gselect = new GParSetSelect(this);

	    // actions
	    glocked = new GBooleanControl(this, parset.locked, "Locked");
	    glocked.setVerifyUncheckText(
	    	"Unlocking will allow overwriting this parset. Are you sure?");
	    glocked.addAuxNode(gproject());

	    // import selected pars from another parset
	    importPars = new GAction(this, "Import selected pars...") {
		public void doit() throws Xcept {
		    gselect.show();
		}
	    };

	    // par file export
	    writeFile = new GAction(this, "Export par file...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, false, GFileChooser.PARSET);
		    if (info == null) return;
		    File f = info.file;
		    if (! UtilIO.fileSuffix(f).equals("par")) 
			f = new File(f.getPath() + ".par");
		    UtilIO.writeText(f, text().getText());
		}
	    };

	    // create widgets
	    JRootPane root = new JRootPane();
	    text = new JTextArea();
	    text.setBackground(glook().dark());
	    text.setEditable(false);
	    setIndentedBorder(text);
	    JScrollPane scroll = new JScrollPane(text);
	    JPanel jcontent = new JPanel(new BorderLayout());

	    JPanel jlockLine = new JPanel(new BorderLayout());
	    JPanel jlockGroup = new JPanel(new GridLayout(1,2));
	    jlockGroup.add(glocked.jcomp());
	    l_lock = new JLabel(glook().lockIcon());
	    jlockGroup.add(l_lock);
	    jlockLine.add(jlockGroup, BorderLayout.WEST);

	    jcontent.add(jlockLine, BorderLayout.NORTH);
	    jcontent.add(scroll, BorderLayout.CENTER);
	    root.setContentPane(jcontent);
	    setJComp(root);

	    // menubar
	    JMenuBar mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);
	    JMenu menu;

	    menu = newMenu("File");
	    menu.add(importPars.item());
	    menu.add(writeFile.item());
	    menu.add(printText.item());
	    mbar.add(menu);

	    menu = newMenu("Edit");
	    menu.add(copy.item());
	    menu.add(gotoLine.item());
	    menu.add(findText.item());
	    menu.add(findAgain.item());
	    mbar.add(menu);

	    helpLinks().addHelpMenu(this, mbar);

	    root.setJMenuBar(mbar);

	}

	// refresh
	public void refresh() {
	    refreshText();
	    text().setCaretPosition(0);
	    setTabLabel(l_hdr);
	    Icon icon = parset.locked.val() ? glook().lockIcon() : glook().unlockIcon();
	    l_lock.setIcon(icon);
	    super.refresh();
	}
	    
	// write contents to Text component
	protected void refreshText() {
	    StringWriter swriter = new StringWriter();
	    writer = new PrintWriter(swriter);
	    try {
	        writeAll();
	    } catch (Xcept e) {
		println("\nError while writing data text: ");
		println("\t" + e.cleanMessage());
	    }
	    String s = swriter.toString();
	    text().setText(s);
	}

	// import selected par
	private void importSelectedPars() throws Xcept {
	    System.err.println("importSelectedPars");
	}    

	// query
	public JTextArea text() {  return text; }
	public String title() { 
	    return gproject().title() + 
		" Parameter Set " + parset.name();
	}
	public ParSet parset() { return parset; }

	// write all content to some Writer
	public void writeAll() throws Xcept {
	    parset.writeFile(writer);
	}

	// write preformatted
	public void println(String s) {
	    writer.println(s);
	}
}
