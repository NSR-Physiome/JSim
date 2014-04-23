/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// 1-line or multi-line text widget for any project Control

package JSim.gui;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GStringControl extends GControl 
implements FocusListener, KeyListener, MouseListener {

	// state
	protected JScrollPane scroll;  // only if Text
	protected JTextComponent text;
	protected boolean changed; // text changed by user
	protected boolean validBlack; // turn black if valid
	protected GStringControl slaveBlack; // slave blackness
	protected boolean blankNaN; // NaN appear blank

	// constructor
	public GStringControl(GNode p, Control c, int ncols) {
	    super(p, c);
	    if (ncols == 0) {
	        text = new JTextArea();
		setIndentedBorder((JTextArea) text);
		scroll = new JScrollPane(text);
		setJComp(scroll);
	    } else {
		text = new JTextField(ncols+1); // needs 1 col xtra space
	        setJComp(text);
	    }
	    text.setBackground(glook().light());
	    text.addKeyListener(this);
	    text.addFocusListener(this);
	    text.addMouseListener(this);
	    
	    changed = false;
	    validBlack = false;
	    blankNaN = false;
	}

	// colors
	public void setValidBlack(boolean b) {
	    validBlack = b;
	    updateColors();
	}
	public void setValidBlack(GStringControl c) {
	    slaveBlack = c;
	    setValidBlack(true);
	}
	protected void updateColors() {
	    GStringControl c = slaveBlack;
	    if (c == null) c = this; 
	    boolean black = c.validBlack;
	    if (black && c.cntl() != null && 
	    c.cntl() instanceof StringControl)
		black = ((StringControl) c.cntl()).valid();
	    updateColors(black);
	}
	protected void updateColors(boolean valid) {
	    Color fg = valid ? glook().light() : Color.black;
	    Color bg = valid ? Color.black : glook().light();
	    text.setForeground(fg);
	    text.setBackground(bg);
	    text.setCaretColor(fg);
	}

	// set other properties
	public void setBlankNaN(boolean b) { blankNaN = b; }

	// valid notify
	private void validMessage() {
	    if (cntl() == null) return;
	    if (! (cntl() instanceof StringControl)) return;
	    StringControl c = (StringControl) cntl();  
	    if (c.valid() || c.isBlank()) return;
	    warning(c.validMsg());
	}

	// event processing
	public void mouseEntered(MouseEvent e) { }
  	public void mouseExited(MouseEvent e) { 
	    focusLost(null);
	}
	public void mouseClicked(MouseEvent e) { }
 	public void mousePressed(MouseEvent e) { }
 	public void mouseReleased(MouseEvent e) { }
        public void focusGained(FocusEvent e) { }
        public void focusLost(FocusEvent e) {
            if (! changed) return;
            userUpdate();
        }
	public void keyTyped(KeyEvent e) {
	    if (e.getKeyChar() != '\n') {
//		(e.getKeyCode() != KeyEvent.VK_ENTER)); doesn't work
		if (! changed && validBlack)
		    updateColors(false);
		changed=true;
	    } else if (changed) {
		userUpdate();
	    }
	}
	public void keyPressed(KeyEvent e) { }
	public void keyReleased(KeyEvent e) { }

	// user initiated update
	private void userUpdate() {
	    gproject().statlineClear();
	    update();
	    validMessage();
	}

	// update
	public void update() {
	    try {
		if (cntl() == null) return;
		if (cntl().stringVal().equals(text.getText())) 
		    return;
	    	cntl().setVal(text.getText());
	        if (validBlack) updateColors(); // needed, really!
		refresh(); // update numerics, validBlack
		refreshAux();
	    } catch (Xcept e) {
		warning("could not update control " + cntl().name() + 
		    ": " + e.cleanMessage());
	    }
	    changed = false;
	}	

	// refresh
	public void refresh() {
//	    line below commented to allow validBlack refresh on model load
//	    if (cntl().stringVal().equals(text.getText())) return;

	    if (!refreshing) {
		int pos = text.getCaretPosition();
		Control c = cntl();
		String s = (c == null) ? "" : c.stringVal();
		if (blankNaN && s.equals("NaN")) s = "";
		setText(s);
		if (s != null && pos>s.length()) pos = s.length();
	    	text.setCaretPosition(pos);
		text.setEditable(editable());
	    }
	    if (validBlack) updateColors();
	    super.refresh();
	}

	// set text, override in GTextControl
	public void setText(String s) { text.setText(s); }

	// query
	public JTextComponent text() { return text; }
}
