/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// compact tabs

package JSim.gui.jcomp;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.net.*;
import java.util.*;

import JSim.util.*;

public class JSCompactTabs extends JComponent
implements MouseListener, MouseMotionListener {

	// private internals
	private ArrayList<Listener> listeners;
	private int ngroups; // # groups
	private Group[] groups; // groups
	private static final Color selectShade = Color.white;
	private static final Color deselectShade = new Color(220,220,220);

	// properties
	private int selectedGroup; // which group is selected
	private String selectedItem; // which item in group is selected
	private Color unselectedColor; // unselected tab color
	private Font tabFont; // tab text font
	private Font itemFont; // menu item font
	private Icon selectIcon; // show selected item in menu
	private Icon forcedIcon; // forced icon, if any
	private int forcedIconGroup; // group for forced icon
	private String forcedIconItem; // item for forced icon

	// recalc() fields
	private boolean needsRecalc; // needs tabs recalc?
	private JSCompactTabs jcomp; // reqd for paintIcon() call
 	private Tab[] compactTabs; // tabs to draw
 	private Tab[] uncompactTabs; // tabs to draw
	private int maxCompactW; // compact width threshold 
	private Font font;	// tab font to use
	private FontMetrics fontM; // tab font metrics
	private int fontA;	// max font ascent
	private int yspace; 	// y spacer
	private int ytop; 	// top of tabs area
	private int ytab; 	// top line of tabs
	private int ybevel; 	// start of tab bevel
	private int ytext;	// text drawing pos
	private int ybottom; 	// bottom of 
	private int xspace;	// x spacer
	private int uncompactClip;  // clipping for uncompact tabs
	private int maxCompactClip;  // maximum clipping for compact tabs

	// paint-time parameters
	private boolean compact; // use compactTabs ?
	private int xleft; 	// tabs area left bounds  
	private int xright;	// tabs area right bounds
	private int compactClip; // current clipping for compact tabs

	// event control
	private JPopupMenu visibleMenu; // visible menu, if any
	private Tab mousedTab; // mouse in this tab

	// constructor
	public JSCompactTabs(String[] names, Icon[] icons) {
	    super();

	    // build groups
	    if (names == null) names = new String[0];
	    if (icons == null) icons = new Icon[0];
	    ngroups = Math.max(names.length, icons.length);
	    groups = new Group[ngroups];
	    for (int i=0; i<ngroups; i++) {
		String name = (i<names.length) ? names[i] : null;
		Icon icon = (i<icons.length) ? icons[i] : null;
		groups[i] = new Group(i, name, icon);
	    }
	    needsRecalc = true;

	    // listeners
	    listeners = new ArrayList<Listener>();
	    addMouseListener(this);
	    addMouseMotionListener(this);
	}

	// get properties
	public Color getUnselectedColor() { return unselectedColor; }
	public int getSelectedGroup() { return selectedGroup; }
	public String getSelectedItem() { return selectedItem; }
	public Icon getSelectIcon() { return selectIcon; }
	public int getPreferredHeight() {
	    loadFontM();
	    return fontA*2 + 1;
	}

	// set properties
	public void setUnselectedColor(Color c) { unselectedColor = c; }
	public void setFont(Font f) {
	    super.setFont(f);
	    needsRecalc = true;
	    repaint();
	}
	public void setSelection(int gx, String s) {
	    selectedGroup = gx; 
	    selectedItem = s;
	    needsRecalc = true;
	    repaint();
	}
	public void setSelectIcon(Icon icon) {
	    selectIcon = icon;
	    needsRecalc = true;
	    repaint();
	}

	// force icon
	public void forceIcon(int gx, String item, Icon icon) {
	    forcedIconGroup = gx;
	    forcedIconItem = item;
	    forcedIcon = icon;
	    repaint();
	}
	public void unforceIcon() {
	    forcedIcon = null;
	    repaint();
	}

	// add group item
	public void addGroupItem(int gx, String item) {
	    Group group = groups[gx];
	    group.items.addUniq(item);
	    needsRecalc = true;
	    repaint();
	}

	// remove group item
	public void removeGroupItem(int gx, String item) {
	    Group group = groups[gx];
	    group.items.remove(item);
	    needsRecalc = true;
	    repaint();
	}

	// add listener
	public void addCompactTabsListener(Listener l) {
	    listeners.add(l);
	}

	// moused query
	public int getMousedGroup() {
	    return (mousedTab == null) ? 
		-1 : mousedTab.group.inx;
	}
	public String getMousedItem() {
	    return (mousedTab == null) ? 
		null : mousedTab.item;
	}
	public Rectangle getMousedRectangle() {
	    if (mousedTab == null) return null;
	    return mousedTab.bounds();
	}

	// paint the component
	public void paint(Graphics g) {

	    // recalc needed?
	    if (needsRecalc) recalc();

	    // bounds adjusted by insets
	    Rectangle b = getBounds();
	    Insets insets = getInsets();
	    b.x += insets.left;
	    b.y += insets.bottom;
	    b.width -= insets.left + insets.right;
	    b.height -= insets.top + insets.bottom;
	    xleft = b.x;
	    xright = b.x + b.width;

	    // clear tabs area
	    g.setColor(getBackground());
	    g.fillRect(b.x, b.y, b.width, b.height);
	    g.setFont(font);

	    // draw uncompactTabs if enough space
	    compact = b.width < maxCompactW;
	    if (!compact) {
		paintTabs(g, uncompactTabs, uncompactClip);
		return;
	    }
		
	    // tweak compactClip?
	    while (compactClip > 0 
	    && tabsWidth(compactTabs, compactClip) > xright)
		compactClip--;
	    while (compactClip < maxCompactClip
	    && tabsWidth(compactTabs, compactClip+1) <= xright)
		compactClip++;
	    setTabPositions(compactTabs, compactClip);

	    // draw compactTabs
	    paintTabs(g, compactTabs, compactClip);
	}

	// paint a tab array
	public void paintTabs(Graphics g, Tab[] tabs, int maxc) {
	    Tab selTab = null; // selected tab,  if any

	    // paint each tab,  note selected
	    for (int i=0; i<tabs.length; i++) {
		Tab tab = tabs[i];
		tab.paint(g, maxc);
		if (tab.selected) selTab = tab;
	    }

	    // draw bottom line
	    g.setColor(selectShade);
	    g.drawLine(xleft, ybottom, xright, ybottom);
	    if (selTab == null) return;
	    g.setColor(getBackground());
	    g.drawLine(selTab.xleft, ybottom, 
		selTab.xright(), ybottom);
	}

	// recalculate tabs
	public void recalc() {
	    needsRecalc = false;

	    // tab-rendering font
	    jcomp = this;
	    loadFontM();

	    // group-independent positions
	    yspace = fontA/4;
	    ytop = 0;
	    ytab = ytop + yspace;
	    ybevel = ytop + 3*yspace;
	    ytext = ytop + fontA + fontA/2;
	    ybottom = ytop + 2*fontA;
	    xspace = 2*yspace;

	    // uncompact tabs 
	    ArrayList<Tab> tabs = new ArrayList<Tab>();
	    for (int i=0; i<ngroups; i++) {
		Group group = groups[i];
		for (int j=0; j<group.items.size(); j++) {
		    String item = group.items.str(j);
		    tabs.add(new Tab(group, item));
		}
	    }
	    uncompactTabs = tabsArray(tabs);
	    uncompactClip = maxText(uncompactTabs);
	    setTabPositions(uncompactTabs, uncompactClip);
	    maxCompactW = tabsWidth(uncompactTabs, uncompactClip);

	    // compact tabs
	    tabs = new ArrayList<Tab>();
	    for (int i=0; i<ngroups; i++) {
		Group group = groups[i];
	        int ct = group.items.size();
		if (ct == 0) continue;
		Tab tab = (ct==1) ?
		    new Tab(group, group.items.str(0)) :
		    new Tab(group);
		tabs.add(tab);
	    }
	    compactTabs = tabsArray(tabs);
	    maxCompactClip = maxText(compactTabs);
	    compactClip = maxCompactClip;
	    setTabPositions(compactTabs, compactClip);
	}

	// load font metrics
	private void loadFontM() {
	    font = getFont();
	    if (font == null) {
		font = new Font("Monospaced", Font.BOLD, 8);
		setFont(font);
	    }
	    fontM = getFontMetrics(font);
	    fontA = fontM.getMaxAscent();
	}

	// max chars in tab text
	private int maxText(Tab[] tabs) {
	    int max = 0;
	    for (int i=0; i<tabs.length; i++) {
		String s = tabs[i].text;
		if (s == null) continue;
		if (s.length() > max) max = s.length();
	    }
	    return max;
	}

	// total tabs size given #chars
	private int tabsWidth(Tab[] tabs, int maxc) {
	    int tot = 0;
	    for (int i=0; i<tabs.length; i++) 
		tot += tabs[i].calcXSize(maxc);
	    return tot;
	}

	// set tabs size given #chars
	private void setTabPositions(Tab[] tabs, int maxc) {
	    int xpos = xleft + xspace;
	    for (int i=0; i<tabs.length; i++) {
		Tab tab = tabs[i];
		tab.xleft = xpos;
		tab.xsize = tab.calcXSize(maxc);
		xpos += tab.xsize;
	    }
	}

	// is group/item selected
	private boolean isSelected(Group group) {
	    return group.inx == selectedGroup;
	}
	private boolean isSelected(Group group, String item) {
	    return group.inx == selectedGroup
	    && selectedItem != null
	    && item != null
	    && item.equals(selectedItem);
	}

	// is item icon forced?
	private boolean isIconForced(Group group, String item) {
	    return forcedIcon != null
	    && item != null
	    && group.inx == forcedIconGroup
	    && item.equals(forcedIconItem);
	}
	
	// create Tab[] from ArrayList
	private Tab[] tabsArray(ArrayList list) {
	    Tab[] tabs = new Tab[list.size()];
	    for (int i=0; i<tabs.length; i++)
		tabs[i] = (Tab) list.get(i);
	    return tabs;
	}

 	// mouse button pressed
        public void mousePressed(MouseEvent e) {
	    if (visibleMenu != null) {
		hideMenu();
	    	return;
	    }
	    Tab tab = tabForPoint(e.getPoint());
	    if (tab == null) return;
	    if (tab.item == null)
		tab.group.showMenu(tab.bounds());
	    else 
	    	tab.group.selectItem(tab.item);   
	}
	
	// which tab corresponds to point
	public Tab tabForPoint(Point p) {
	    Tab[] tabs = compact ? compactTabs : uncompactTabs;
	    if (tabs == null || tabs.length == 0) return null;
	    if (p.x < tabs[0].xleft) return null;
	    for (int i=0; i<tabs.length; i++)
		if (p.x<tabs[i].xright()) 
		   return tabs[i];
	    return null;
	}

	// hide menu
	public void hideMenu() {
	    if (visibleMenu == null) return;
	    visibleMenu.setVisible(false);
	    visibleMenu = null;
	}

	// mouse moved
	public void mouseMoved(MouseEvent e) { 
	    Tab tab = tabForPoint(e.getPoint());
	    if (tab == mousedTab) return;
	    mousedTab = tab;
	    sendMotionEvents();
	}

	// exit window
        public void mouseExited(MouseEvent e) {	
	    if (e.getSource() != this) return;
	    if (mousedTab == null) return;
	    mousedTab = null;
	    sendMotionEvents();
	}

	// send motion events to listeners
	private void sendMotionEvents() {
	    for (int i=0; i<listeners.size(); i++) {
		Listener l = (Listener) listeners.get(i);
		l.tabMoused(this);
	    }
	}

	// worthless mouse event processing
	public void mouseEntered(MouseEvent e) { }
        public void mouseClicked(MouseEvent e) { }
        public void mouseReleased(MouseEvent e) { }
	public void mouseDragged(MouseEvent e) { }

	// one Group
	private class Group {
	    int inx; // index of group
	    String name; // name of group
	    Icon icon; // icon for group
	    StringList items; // items in this group

	    // constructor
	    public Group(int i, String n, Icon icn) {
		inx = i;
		name = n;
		icon = icn;
		items = new StringList();
	    }

	    // select an item
	    public void selectItem(String item) {
		if (isSelected(this, item)) return;
		selectedGroup = inx;
		selectedItem = item;
		needsRecalc = true;
		repaint();
		for (int i=0; i<listeners.size(); i++) {
		    Listener l = (Listener) listeners.get(i);
		    l.tabItemSelected(jcomp);
		}
	    }

	    // show the menu
	    private void showMenu(Rectangle bounds) {
	        JPopupMenu menu = new JPopupMenu();

		// add header / separator
		menu.setBackground(getBackground());
// hdr below JMenuItem rather that JLabel due to 
// bug in MacOS applet JSIM/171
		JMenuItem hdr = new JMenuItem(name, icon);
		hdr.setHorizontalAlignment(SwingConstants.CENTER);
// OLD VERSION	JLabel hdr = new JLabel(name, icon, JLabel.CENTER);
		hdr.setFont(font);
		hdr.addMouseListener(jcomp);
		menu.add(hdr);
		menu.addSeparator();

		// add items
		for (int i=0; i<items.size(); i++) {
		    String item = items.str(i);
		    Action a = new SelectAction(this, item);
		    JMenuItem jitem = new JMenuItem(a);
		    jitem.setFont(font);
		    jitem.setBackground(getBackground());
		    if (isSelected(this, item))
			jitem.setIcon(selectIcon);
		    else if (isIconForced(this, item))
			jitem.setIcon(forcedIcon);
		    menu.add(jitem);
		}

		// set selection icon and show
		menu.show(jcomp, bounds.x, bounds.y + bounds.height);
		visibleMenu = menu;
	    }
	}

	// selection action
	private class SelectAction extends AbstractAction {
	    Group group;
	    String item;
	    public SelectAction(Group g, String i) {
		super(i);
		group = g;
		item = i;
	    }
	    public void actionPerformed(ActionEvent e) {
		group.selectItem(item);
		visibleMenu = null;
	    }
	}
		
  	// one tab to draw
	private class Tab {
	    Group group; 	// attached group
	    String item; 	// null if group menu
	    String text; 	// untruncated text
	    Icon icon; 		// from group
	    boolean selected; 	// item selected?
	    int xleft; 		// left position
	    int xsize; 		// size
	    
	    // group constructor
	    public Tab(Group g) {
		group = g;
		item = null;
		text = group.name;
		if (text != null) 
		    text = text + " (" + group.items.size() + ")";
		icon = group.icon;
		selected = isSelected(group);
	    }

	    // item constructor
	    private Tab(Group g, String itm) {
		group = g;
		item = itm;
		text = itm;
		icon = group.icon;
		selected = isSelected(group, item);
	    }

	    // calc X size given max #chars
	    public int calcXSize(int maxc) {
		int size = 2*xspace;
		if (icon != null) 
		    size += icon.getIconWidth();
		String txt = clippedText(maxc);
		if (txt != null) {
		    size += fontM.stringWidth(txt);
		    if (icon != null) 
			size += xspace;
		}
		return size;
	    }

	    // paint one tab
	    public void paint(Graphics g, int maxc) {

	        // tab-delimiting boundaries
		int xbevel = xleft + xspace;
		int xright = xright();
		int[] xp = new int[] { xleft, xleft, xbevel, xright, xright };
		int[] yp = new int[] { ybottom-1, ybevel, ytab, ytab, ybottom-1 };

		// tab fill & line boundary
		if (!selected && unselectedColor != null) {
		    g.setColor(unselectedColor);
		    g.fillPolygon(xp, yp, 5);
		}
		g.setColor(getForeground());
		g.drawPolyline(xp, yp, 5);

		// icon
		Icon wicon = isIconForced(group, item) ?
		    forcedIcon : icon;
	 	int xtext = xleft + xspace;
		if (wicon != null) {
		    int yicon = ytab+1;
		    wicon.paintIcon(jcomp, g, xtext, yicon);
		    xtext += wicon.getIconWidth() + xspace;
		}

		// text
		String txt = clippedText(maxc);
		if (txt != null) 
		    g.drawString(txt, xtext, ytext);

		// shading
		g.setColor(selected ? selectShade : deselectShade);
		g.drawPolyline(
		    new int[] { xleft+1, xleft+1, xbevel, xright },
		    new int[] { ybottom-1, ybevel, ytab+1, ytab+1 },
		    4);
	    }

	    // clipped text
	    private String clippedText(int maxc) {
		if (text == null) return text;
		if (maxc < 1) return null;
		if (text.length() > maxc)
		    return text.substring(0, maxc);
		return text;
	    }

	    // simple query
	    public String toString() { return text; }
	    private int xright() { return xleft + xsize; }
	    private Rectangle bounds() {
		return new Rectangle(xleft, ybottom, xsize, ytab-ybottom);
	    }
	}

	// debug Action for Keystroke

	// JSCompactTab.Listener
	public static interface Listener {
	    void tabItemSelected(JSCompactTabs jcomp);
	    void tabMoused(JSCompactTabs jcomp);
	}

	//// test routine

	// test mainline
	public static void main(String[] args) throws Exception {

	    // load names and icons
	    String[] names = new String[] { 
		"Projects", "Models", "ParSets", "DataSets", "Notes" };
	    Icon[] icons = new Icon[4];
	    icons[0] = new ImageIcon(
		JSCompactTabs.class.getResource("../icons/project.gif"));
	    icons[1] = new ImageIcon(
		JSCompactTabs.class.getResource("../icons/model.gif"));
	    icons[2] = new ImageIcon(
		JSCompactTabs.class.getResource("../icons/parset.gif"));
	    icons[3] = new ImageIcon(
		JSCompactTabs.class.getResource("../icons/dataset.gif"));
	    Icon sicon = new ImageIcon(
		JSCompactTabs.class.getResource("../icons/select.gif"));
	    Icon picon = new ImageIcon(
		JSCompactTabs.class.getResource("../icons/parsetg.gif"));

	    // create tabs
	    JSCompactTabs tabs = new JSCompactTabs(names, icons);
	    tabs.setBackground(new Color(160,210,250));
	    tabs.setForeground(Color.black);
	    tabs.setUnselectedColor(new Color(135,185,250));
	    tabs.setFont(new Font("Lucida", Font.BOLD, 14));
	    tabs.setPreferredSize(new Dimension(500,100));
	    tabs.setSelectIcon(sicon);
	    tabs.addCompactTabsListener(new mainListener());
	    
	    //add group items, set selection
	    tabs.addGroupItem(0, "Project");
	    tabs.addGroupItem(1, "model_1234567890");
	    tabs.addGroupItem(2, "pars_1");
	    tabs.addGroupItem(2, "pars_2");
	    tabs.addGroupItem(3, "dataset_1");
	    tabs.addGroupItem(3, "dataset_2");
	    tabs.addGroupItem(3, "dataset_3");
	    tabs.setSelection(0, "Project");
	    tabs.forceIcon(2, "pars_2", picon);

	    // containing panel & frame
	    JPanel panel = new JPanel(new GridLayout(1,1));
	    panel.add(tabs);
	    JFrame frame = new JFrame("JSCompactTabs");
	    frame.getRootPane().setContentPane(panel);
	    frame.pack();
	    frame.setVisible(true);
	}

	// test listener
	private static class mainListener implements Listener {
	    public void tabItemSelected(JSCompactTabs jcomp) {
		System.err.println(
		    "selected group=" + jcomp.getSelectedGroup() +
		    " item=" + jcomp.getSelectedItem());
	    }
	    public void tabMoused(JSCompactTabs jcomp) {
	        System.err.println(
		    "mouse tab group=" + jcomp.getMousedGroup() + 
		    " item=" + jcomp.getMousedItem());
	    }
	}
}
