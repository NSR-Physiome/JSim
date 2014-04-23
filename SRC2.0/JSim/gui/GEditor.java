/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// common routines for JTextArea display & edit

package JSim.gui;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.undo.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.print.*;
import java.io.*;

import JSim.util.*;
import JSim.project.*;

abstract public class GEditor extends GNode 
implements CaretListener, UndoableEditListener {

	// actions
	public GAction importClearText, importInsertText, 
	    printText, cut, paste,
	    exportText, copy, gotoLine, findText, findAgain,
	    undo, redo, erase, storeNotes;
	private String lastFind; // text from last find
	protected UndoManager undoMgr;
	private Document doc;  

	// constructor from Editor
	public GEditor(GNode n, PNamed p) {
	    super(n, p);

	    // import text
	    importClearText = new GAction(this, "Import text (clear)...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, true, GFileChooser.TEXT);
		    if (info == null) return;
		    File f = info.file;
		    String s = UtilIO.readText(f);
		    text().setText(s);	
		    // may occur without focus,  so update now!
		    if (gcntl() != null) gcntl().update();
		}
	    };

	    // import text
	    importInsertText = new GAction(this, "Import text (insert)...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, true, GFileChooser.TEXT);
		    if (info == null) return;
		    File f = info.file;
		    String s = UtilIO.readText(f);
		    text().insert(s, text().getCaretPosition());
		    if (gcntl() != null) gcntl().update();
		}
	    };

	    // export text
	    exportText = new GAction(this, "Export text...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, false, GFileChooser.TEXT);
		    if (info == null) return;
		    File f = info.file;
		    UtilIO.writeText(f, text().getText());
		}
	    };

	    // print 
	    printText = new GAction(this, "Print text...") {
		public void doit() throws Xcept {
		    gnode.printText(title(), text().getText());
		}
	    };

	    // cut/paste/goto actions
            cut = new GAction(this, "cut"){
                public void doit() {text().cut();}
            };
            paste = new GAction(this, "paste") {
                public void doit() { text().paste();}
            };
            copy = new GAction(this, "copy") {
                public void doit() {
		    text().copy();
		}
            };
	    gotoLine = new GAction(this, "go to line#") {
		public void doit() throws Xcept {
		    String msg = "Go to line#";
		    String sline = (String) JOptionPane.showInputDialog(
			    jcomp(), msg, msg, 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().userIcon(),
			    null, null);
		    text().requestFocus();
		    if (Util.isBlank(sline)) return;
		    int line = Util.toInt(sline) - 1;
		    if (line<0) line = 0;
		    int max = text().getLineCount();
		    if (line>max) line = max;
		    try {
			int ofs = text().getLineStartOffset(line);
			int ofs1 = text().getLineStartOffset(line+1);
			text().setCaretPosition(ofs);
			if (ofs1 > ofs) {
			    text().setSelectionStart(ofs);
			    text().setSelectionEnd(ofs1);
			}
		    } catch (Exception e) { }
		}
	    };
	    findText = new GAction(this, "find") {
		public void doit() throws Xcept {
		    String msg = "Find text";
		    lastFind = (String) JOptionPane.showInputDialog(
			    jcomp(), msg, msg, 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().userIcon(),
			    null, lastFind);
		    text().requestFocus();
		    if (Util.isBlank(lastFind)) return;
		    findAgain.doit();
		}
	    };
	    findAgain = new GAction(this, "find again") {
		public void doit() throws Xcept {
		    String alltext = text().getText();
		    int cpos = text().getCaretPosition();
		    int npos = alltext.indexOf(lastFind, cpos);
		    if (npos <= cpos) npos = alltext.indexOf(lastFind);
		    if (npos<0) throw new Xcept("Can't find \"" + 
			lastFind + "\" in current editor window");
		    text().setCaretPosition(npos); 
		    text().setSelectionStart(npos);
		    text().setSelectionEnd(npos + lastFind.length());
		}
	    };
	    undo = new GAction(this, "undo") {
	    	public void doit() throws Xcept {
		    if (undoMgr == null) return;
		    try {
		    	undoMgr.undo();
			updateUndoAccels();
		    } catch (Exception e) {
		    	throw Xcept.wrap(e);
		    }
		}
		public boolean sbEnabled() {
		    UndoManager mgr = ((GEditor) gnode).undoMgr;
		    if (mgr == null) return false;
		    return mgr.canUndo();
		}
	    };
	    redo = new GAction(this, "redo") {
	    	public void doit() throws Xcept {
		    if (undoMgr == null) return;
		    try {
		    	undoMgr.redo();
			updateUndoAccels();
		    } catch (Exception e) {
		    	throw Xcept.wrap(e);
		    }
		}
		public boolean sbEnabled() {
		    UndoManager mgr = ((GEditor) gnode).undoMgr;
		    if (mgr == null) return false;
		    return mgr.canRedo();
		}
	    };

	    // don't set accels for right tabs components
	    //    because they interfere with left tabs
	    if (!(this instanceof GMessage)) {
		cut.setAccel('X');
	    	copy.setAccel('C');
	    	paste.setAccel('V');
	    	gotoLine.setAccel('G');
	    	findText.setAccel('F');
	    	findAgain.setAccel('N');
	    	undo.setAccel('Z', false);
	    	redo.setAccel('Z', true);
	    }

	    // erase text
	    erase = new GAction(this, "erase") {
		public void doit() {
		    text().setText("");
		}
	    };

	    // store text in project notes
	    storeNotes = new GAction(this, "Store as project notes ...") {
		public void doit() throws Xcept {
		    gproject().pushTree();
		    String msg = "Store text under name";
		    Project proj = gproject().project();
		    String n = proj.newChildName("notes", true);
		    String nname = (String) JOptionPane.showInputDialog(
			    jcomp(), msg, "Enter name", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().fileIcon(),
			    null, n);
		    gproject().popTree();
		    if (nname == null) return;
		    PNotes pnotes = new PNotes(proj, nname);
		    String s = text().getText();
		    pnotes.text.setVal(new String(s));
		    gproject().add(pnotes);
		    gproject().refresh();
		}
	    };		

	}

	// enable line# update
	protected void enableLineUpdate() {
	    text().addCaretListener(this);
	}

	// caret moved
	public void caretUpdate(CaretEvent e) {
	    try {
	    	int line = text().getLineOfOffset(e.getDot());
	    	gproject().setEditLine(line+1);
	    } catch (Exception ex) { 
		// no biggie if fails
	    }
	}

	// enable undo
	private void enableUndo() {
	    undoMgr = new UndoManager();
	    doc = text().getDocument();
	    doc.addUndoableEditListener(this);
 	}
 
	// load undo doc, can't in constructor since text() null
	protected void resetUndo() {
	    if (undoMgr == null) enableUndo();
	    undoMgr.discardAllEdits();
	}

	// undoable edit happened
	public void undoableEditHappened(UndoableEditEvent e) {
	    UndoableEdit edit = e.getEdit();
	    undoMgr.addEdit(edit);
	    updateUndoAccels();
	}

	// update undo accelerators
	private void updateUndoAccels() {
	    undo.setEnabled(undo.sbEnabled());
	    redo.setEnabled(redo.sbEnabled());
	}

	// get text area / control if appropriate
	abstract public JTextArea text();
	public GTextControl gcntl() { return null; }
	abstract public String title();
}
