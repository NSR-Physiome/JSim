/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Help system popup

package JSim.gui;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.model.*;

public class GHelpPopup  implements MouseListener {
	private GProject gproj;
	private Dimension screenDim; // screen dimension
	private Window window;	// plain help
	private boolean visible; // plain help visible?

	// editable dialog
	private JDialog jdialog;
	private JPanel jpanel;
	private JScrollPane jscroll;
	private JTextArea jtext;
	private JList jfixed;
	private GHelpDB currDB;
	private String currKey;

	// constructor
	public GHelpPopup(GProject g) {
	    gproj = g;
	    screenDim = gproj.toolkit().getScreenSize();
	}

	// MouseListener: s/b activated only if help confused
	public void mouseEntered(MouseEvent e) { 
	    hide();
	    gproj.ghelp().showHelp(null,null);
	}
	public void mouseClicked(MouseEvent e) {  }
	public void mouseExited(MouseEvent e) { }
	public void mousePressed(MouseEvent e) {  }
	public void mouseReleased(MouseEvent e) {  }

	// hide popup
	public void hide() {
	    if (visible) 
		window.setVisible(false);
	    visible = false;
	}

	// show popup
	public void show(int mode, JComponent jcomp,
	Rectangle rect, GHelpDB db, String key) {
	    currDB = null;
	    currKey = null;
	    if (mode == GHelp.EDIT)
		showEdit(jcomp, rect, db, key);
	    else 
		showPlain(mode, jcomp, rect, db, key);
	}

	// show plain
	private void showPlain(int mode, JComponent jcomp,
	Rectangle rect, GHelpDB db, String key) {
	
	    // create window first time
	    if (window == null) {
	    	Window pwin = SwingUtilities.getWindowAncestor(
		    gproj.jcomp());
	    	window = new Window(pwin);
		window.addMouseListener(this);
	    }

	    // prepare text
	    String text = "";
	    String ftext = db.getFixedText(key);
	    if (ftext != null) text = text + ftext;
	    String etext = db.getEditableText(key);
	    if (etext != null) text = text + etext;
	    if (Util.isBlank(text)) return;
	    StringList slist = new StringList(text, "\n", 80);
	    slist.add(">> Type F1 or shift-F1 to disable help <<");

	    // load window
	    window.removeAll();
	    JPanel jpanel = new JPanel(
		new GridLayout(slist.size(), 1));
	    jpanel.setBackground(gproj.glook().bright());
	    for (int i=0; i<slist.size(); i++) 
		jpanel.add(new JLabel(slist.str(i)));
	    jpanel.setBorder(new EtchedBorder());
	    window.add(jpanel);
	    Dimension dim = jpanel.getPreferredSize();
	    dim.height += GUtil.windowWarningBannerHeight(window);
	    window.setSize(dim);

	    // set location & visible
	    Point p = location(jcomp, rect, dim);
	    window.setLocation(p.x, p.y);
	    window.setVisible(true);
	    visible = true;	    
	}

	// show editable dialog
	private void showEdit(JComponent jcomp,
	Rectangle rect, GHelpDB db, String key) {
	    hide(); // hide plain window, if visible
	    currDB = db;
	    currKey = key;
	    if (jdialog == null) createDialog();
	    String ftext = db.getFixedText(key);
	    String etext = db.getEditableText(key); 
	    refresh(key, ftext, etext);
	    Dimension dim = jdialog.getSize();
	    Point p = location(jcomp, rect, dim);
	    jdialog.setLocation(p.x, p.y);
	    jdialog.setVisible(true);
	}	    

	// create editable dialog
	private void createDialog() {
	    jdialog = new JDialog(gproj.frame(), "Help Editor", true);
	    jpanel = new JPanel(new BorderLayout());
	    jdialog.setContentPane(jpanel);
	    jfixed = new JList();
	    jfixed.setBackground(gproj.glook().dark());
	    jpanel.add(jfixed, BorderLayout.NORTH);
	    jtext = new JTextArea();
	    jtext.setBackground(gproj.glook().light());
	    jscroll = new JScrollPane(jtext);
	    jpanel.add(jscroll, BorderLayout.CENTER);
	    JPanel jbuttons = new JPanel(new GridLayout(1, 3));
	    jpanel.add(jbuttons, BorderLayout.SOUTH);

	    // OK button: don't use GAction (recursive help calls!)
	    AbstractAction aok = new AbstractAction("OK") {
		public void actionPerformed(ActionEvent ev) { ok(); }
	    };
	    JButton jok = new JButton(aok);
	    jok.setBackground(Color.white);
	    jbuttons.add(jok);

	    // Cancel button
	    AbstractAction acancel = new AbstractAction("Cancel") {
		public void actionPerformed(ActionEvent ev) { cancel(); }
	    };
	    JButton jcancel = new JButton(acancel);
	    jcancel.setBackground(Color.white);
	    jbuttons.add(jcancel);

	    // Export button
	    AbstractAction aexport = new AbstractAction("Export") {
		public void actionPerformed(ActionEvent ev) { export(); }
	    };
	    JButton jexport = new JButton(aexport);
	    jexport.setBackground(Color.white);
	    jbuttons.add(jexport);

	    // size
	    Dimension dim = new Dimension();
	    dim.width = 25 * gproj.glook().fontSize(); // bogus
	    dim.height = dim.width;
	    jdialog.setSize(dim);
	}

	// OK to change
	public void ok() {
	    jdialog.setVisible(false);
	    String text = jtext.getText();
	    if (currDB != null)
		currDB.setEditableText(currKey, text);
	}

	// cancel
	public void cancel() {
	    jdialog.setVisible(false);
	}

	// export database
	public void export() {
	    ok();
	    if (currDB == null) return;
	    try {
	    	GFileChooser.Info info = GFileChooser.select(
		    gproj, false, GFileChooser.XML);
	    	if (info == null) return;
	    	currDB.save(info.file);
	    } catch (Xcept e) {
		gproj.warning(e.cleanMessage());
	    }
	}

	// refresh dialog contents, size
	private void refresh(String key, String ftext, String etext) {

	    // dialog title
	    jdialog.setTitle("Edit Help: " + key);

	    // refresh fixed text
	    if (Util.isBlank(ftext)) {
		jfixed.setVisible(false);
		jfixed.setListData(new String[] { "" });
	    } else {
		StringList slist = new StringList(ftext, "\n");
		jfixed.setVisible(true);
		jfixed.setListData(slist.array());
	    }

	    // editable text
	    jtext.setText(etext);
	    jtext.setCaretPosition(0);
	}	    

	// calculation location for popup 
	private Point location(JComponent jcomp, Rectangle rect, Dimension dim) {
	    Point p = GUtil.getLocationOnScreen(jcomp);
	    if (rect == null) {
		p.x += jcomp.getWidth();
	    } else {
		p.x += rect.x + rect.width;
		p.y += rect.y;
	    }
	    if (p.x + dim.width > screenDim.width) {
		p.x -= dim.width;
		p.y += (rect == null) ? 
		    jcomp.getHeight() : rect.height;
	    }
	    return p;
	}
}

