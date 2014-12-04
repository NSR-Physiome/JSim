/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// JPopupMenu for editing ChoiceControl. BooleanControl

package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.project.*;

public class GMenuControl extends GControl 
implements ActionListener, PopupMenuListener {

	// state
	protected static boolean menuWaiting = false;
	protected static final String showBad = "???";
	protected GAction action;
	protected JButton button;
	protected GPopupMenu menu;
	protected String[] labels; // user visible labels
	protected String[] labelValues; // Cntl stored vals
	protected Icon[] icons;
	protected boolean scrollable;
	protected StringList pickList; // override control pickList

	// hack for PlotIconControl
	protected int attr;

	// constructors
	public GMenuControl(GNode p, Control c) { 
	    this(p, c, false, -1);
	}
	public GMenuControl(GNode p, Control c, int attr) {
	    this(p, c, false, attr);
	}	    
	public GMenuControl(GNode p, Control c, boolean s) {
	    this(p, c, s, -1);
	}
	public GMenuControl(GNode p, Control c, boolean s, int attr) {
	    super(p, c);
	    scrollable = s;
	    this.attr = attr;
	    action = new GAction(this, "") {
		public void doit() {
		    if (menuWaiting || menu.isVisible()) {
			menu.setVisible(false);
			menuWaiting = false;
		    } else {
			int n = cntlVal();
			// can't setSelectedIndex properly ???
		        menu.show(button, 0, button.getHeight());
			menuWaiting = true;
		    }
		}
	    };
	    button = new JButton(action);
	    button.setMargin(new Insets(0,0,0,0));
	    setJComp(button);
	    resetLabels();
	    refresh();
	}

	// make labels
	public String[] makeLabels() {
	    String[] labels;
	    if (cntl() instanceof ChoiceControl) {
	    	ChoiceControl c = (ChoiceControl) cntl();
	    	labels = new String[c.nLabels()];
	    	for (int i=0; i<labels.length; i++)
		    labels[i] = c.stringVal(i);
	    } else if (cntl() instanceof BooleanControl) {
	    	BooleanControl c = (BooleanControl) cntl();
	    	labels = new String[2];
	    	labels[0] = c.stringVal(true);
	    	labels[1] = c.stringVal(false);
	    } else if (cntl() instanceof StringControl) {
	    	StringControl c = (StringControl) cntl();
		StringList slist = pickList();
		int ct = (slist==null) ? 0 : slist.size();
		if (ct<1) 
		    labels = new String[] { showBad };
		else {
		    labels = new String[ct];
		    for (int i=0; i<ct; i++)
		    	labels[i] = slist.str(i);
		}
	    } else 
		labels = new String[] { showBad };
	    return labels;
	}

	// pickList
	public StringList pickList() {
	    if (pickList != null) return pickList;
	    if (cntl() instanceof StringControl)
	    	return ((StringControl) cntl()).pickList();
	    return null;
	}
	public void setPickList(StringList p) { pickList = p; }
	
	// make icons
	public Icon[] makeIcons() { return null; }

	// colors
	public Color bg(int i) {  return null; }

	// create or recreate menu
	public void resetLabels() {
	    labels = makeLabels();
	    icons = makeIcons();

	    if (menu != null) menu.setVisible(false);
	    menuWaiting = false;
	    menu = GPopupMenu.create(this, scrollable);
	    // menu.addPopupMenuListener(this);
	    Dimension maxDim = new Dimension(10, 10);
	    int ct = (icons != null) ? icons.length : labels.length;
	    for (int i=0; i<ct; i++) {
		Icon icon = (icons == null) ? null : icons[i];
		Item item = (icon == null) ?
		     new Item(this, labels[i], i) : new Item(icon, i);
		if (bg(i) != null) {
		    item.setBackground(bg(i));
		    item.setForeground(bg(i));
		}
		item.addActionListener(this);
		menu.add(item);
		maxDim = max(maxDim, item.buttonDim());
	    }
	    buttonSize(maxDim);
	}

	// set associated labelValues
	public void setLabelValues(String[] l) {
	    labelValues = l;
	}

	// set button size
	protected void buttonSize(Dimension maxDim) {
	    button.setPreferredSize(maxDim);
	}

	// menu item was selected
	public void actionPerformed(ActionEvent event) {
	    menuWaiting = false;
	    if (! (event.getSource() instanceof Item)) return;
	    int n = ((Item) event.getSource()).n;
	    if (cntlVal() == n) return;

	    // update 
	    try {
		Control c = cntl();
	    	if (c instanceof ChoiceControl) 
		    ((ChoiceControl) c).setVal(n);
	    	else if (c instanceof IntControl)
		    ((IntControl) c).setVal(n);
	    	else if (c instanceof BooleanControl) 
		    ((BooleanControl) c).setVal(n==0);
		else if (c instanceof StringControl)
		    ((StringControl) c).setVal(labelValue(n));
	    	refresh();
	        refreshAux();
	    } catch (Xcept e) {
		warning(e.cleanMessage());
	    }
	}

	// refresh
	public void refresh() {
	    if (! refreshing) {
		if (pickList() != null) 
		    resetLabels();
	 	int i = cntlVal();
		if (i<0) {
		    String bad = showBad;
		    if (cntl() instanceof PNamedControl) {
			PNamedControl pcntl = (PNamedControl) cntl();
			if (pcntl.isBlank()) {
			    bad = pcntl.singlePick();
			    if (Util.isBlank(bad)) bad = showBad;
			}
		    }
		    button.setText(bad);
		    button.setIcon(null);
		} else if (icons == null || icons[i] == null) {
	    	    button.setText(labels[i]);
		    button.setIcon(null);
		} else {
		    button.setText(null);
		    button.setIcon(icons[i]);
		}
		if (bg(i) != null) {
		    button.setBackground(bg(i));
		    button.setForeground(bg(i));
		}
		button.setEnabled(editable());
	    }
	    super.refresh();
	}

	// hidePopups
	public void hidePopups() {
	    if (menu == null) return;
	    menuWaiting = false;
	    menu.setVisible(false);
	}

	// internal control & jcomp values
	public int cntlVal() {
	    int n=0;
	    Control c = cntl();
	    if (c instanceof ChoiceControl) 
		n = ((ChoiceControl) c).val();
	    else if (c instanceof IntControl) {
		n = ((IntControl) c).val();
		if (n<0) n=1;
	    } else if (c instanceof BooleanControl) 
		n = ((BooleanControl) c).val() ? 0 : 1;
	    else if (c instanceof StringControl) {
		n = -1;
		for (int i=0; i<labels.length; i++) 
		    if (c.stringVal().equals(labelValue(i)))
			n = i;
	    }
		
	    return n;
	}

	// internal value associated with label
	private String labelValue(int i) {
	    if (labelValues == null) return labels[i];
	    return labelValues[i];
	}

	// PopupMenuListener methods
	public void popupMenuCanceled(PopupMenuEvent e) { 
//	    menuWaiting = false;
	}
	public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
//	    menuWaiting = false;
	}
	public void popupMenuWillBecomeVisible(PopupMenuEvent e) { }

	// menu item
	public class Item extends JMenuItem {
	    public int n;
	    public Dimension buttonDim;

	    public Item(GControl gcntl, String s, int i) { 
		super(s); 
		n = i;
		setMargin(new Insets(0,0,0,0));
		setFont(glook().baseFont());
		buttonDim = getPreferredSize();
		setHelp(this, gcntl);
	    }
	    public Item(Icon icon, int i) { 
		super(icon); 
		n = i;
		setMargin(new Insets(0,0,0,0));
		setContentAreaFilled(true);
		Dimension dim = getPreferredSize();
		buttonDim = new Dimension(
		    4+icon.getIconWidth(), dim.height);
	    }

	    public Dimension buttonDim() { return buttonDim; }
	}
	
}

