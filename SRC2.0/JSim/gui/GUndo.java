/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.undo.*;
import javax.swing.event.*;

public class GUndo extends JTextArea implements UndoableEditListener {
	private UndoManager undo;
	private Document doc;
	private Action redoAction, undoAction;
	
	// constructor
	public GUndo() {
	    super();
	    setText("abc\ndef\n");
	    setPreferredSize(new Dimension(200,200));
	    undo = new UndoManager();
	    doc = getDocument();
	    doc.addUndoableEditListener(this);

	    undoAction = new AbstractAction("Undo") {
	    	public void actionPerformed(ActionEvent e) {
		    try {
		    	if (undo.canUndo())
			    undo.undo();
			else
			    System.err.println("Can't undo");
		    } catch (Exception x) {
		    	System.err.println("" + x);
		    }
		}
	    };
	    getActionMap().put("Undo", undoAction);
	    getInputMap().put(KeyStroke.getKeyStroke("control Z"), "Undo");

	    redoAction = new AbstractAction("Redo") {
	    	public void actionPerformed(ActionEvent e) {
		    try {
		    	if (undo.canRedo())
			    undo.redo();
			else
			    System.err.println("Can't redo");
		    } catch (Exception x) {
		    	System.err.println("" + x);
		    }
		}
	    };
	    getActionMap().put("Redo", redoAction);
	    getInputMap().put(KeyStroke.getKeyStroke("control Y"),"Redo");
	}

	// undoable edit happened
	public void undoableEditHappened(UndoableEditEvent e) {
	    undo.addEdit(e.getEdit());
	}

	// mainline
	public static void main(String[] args) throws Exception {
	    JFrame frame = new JFrame("GUndo");
	    frame.getRootPane().setContentPane(new GUndo());
	    frame.pack();
	    frame.setVisible(true);
	}
}
