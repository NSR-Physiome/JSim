/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Image set

package JSim.gui;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.io.*;

import JSim.util.*;
import JSim.project.*;

public class GImageSet extends GNode 
implements ListSelectionListener {

	// state info
	private PImageSet images;
	private JRootPane root;
	private JPanel jpanel;
	private Dimension jpanelDim;
	private JLabel l_hdr;
	private JLabel jhdr;
	private JList jlist;
	private JScrollPane jscroll;
	private JLabel jlab;
	private JLabel jimage;
	
	// actions
	public GAction importFile, exportFile, rename, delete;

	// constructor
	public GImageSet(GNode p, PImageSet iset) {
	    super(p, iset);
	    images = iset;
	    root = new JRootPane();
	    jpanel = new JPanel(null) {
		public void doLayout() { reconfig(true); } 
	    };
	    root.getContentPane().add(jpanel);
	    setJComp(root);
	}
	
	// make content
	public void makeContent() {
	    needsContent = false;

	    // import file action
	    importFile = new GAction(this, "Import image file...") {
		public void doit() throws Xcept {
		    GFileChooser.Info info = GFileChooser.select(
			gnode, true, GFileChooser.IMAGE);
		    if (info == null) return;
	            String name = UtilIO.fileBaseName(info.file);
		    name = (String) JOptionPane.showInputDialog(
			    jcomp(), "Enter name for imported image", "Image name", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().imagesetIcon(),
			    null, name);
		    if (name == null) return;
		    String safe = PNamed.safeName(name);
		    if (! name.equals(safe)) throw new Xcept(
		    	"Image name contains illegal characters");
		    PImage pimage = images.pimage(name);
		    if (pimage == null)
		    	pimage = new PImage(images, name);
		    pimage.importFile(info.file);
		    reconfig(false);
		    jlist.setSelectedValue(name, true);
		}
	    };
	    
	    // export file action
	    exportFile = new GAction(this, "Export image file...") {
		public void doit() throws Xcept {
	    	    String name = (String) jlist.getSelectedValue();
	            if (Util.isBlank(name)) throw new Xcept(
		    	"No image selected");
		    PImage pimage = images.pimage(name);
		    if (pimage == null) throw new Xcept(
		        "Can't find selected image");
		    GFileChooser.Info info = GFileChooser.select(
		    	gnode, false, GFileChooser.IMAGE);
		    if (info == null) return;
		    String oext = pimage.origFileSuffix();
		    String fext = UtilIO.fileSuffix(info.file);
		    if (Util.isBlank(fext) && !Util.isBlank(oext)) 
			info.file = new File(info.file.getPath() +
			    "." + oext);
		    UtilIO.writeBytes(info.file, pimage.imageData());
	            gproject().message("Image " + name + " exported as "
		    	+ info.file);
	    	}
	    };
	    
	    // rename image
	    rename = new GAction(this, "Rename image...") {
		public void doit() throws Xcept {
	    	    String name = (String) jlist.getSelectedValue();
	            if (Util.isBlank(name)) throw new Xcept(
		    	"No image selected");
		    PImage pimage = images.pimage(name);
		    if (pimage == null) throw new Xcept(
		    	"Selected image not found");
		    name = (String) JOptionPane.showInputDialog(
			    jcomp(), "Enter name for imported image", "Image name", 
			    JOptionPane.QUESTION_MESSAGE, 
			    glook().imagesetIcon(),
			    null, name);
		    if (name == null) return;
		    String safe = PNamed.safeName(name);
		    if (! name.equals(safe)) throw new Xcept(
		    	"Image name contains illegal characters");
		    pimage.rename(name);
		    reconfig(false);
		    jlist.setSelectedValue(name, true);
		}
	    };
	    
	    // delete image
	    delete = new GAction(this, "Delete image...") {
		public void doit() throws Xcept {
	    	    String name = (String) jlist.getSelectedValue();
	            if (Util.isBlank(name)) throw new Xcept(
		    	"No image selected");
		    images.remove(name);
		    reconfig(false);
		}
	    };

	    // MenuBar & label
	    JMenuBar mbar = new JMenuBar();
	    l_hdr = new JLabel();
	    setTabLabel(l_hdr);
	    mbar.add(l_hdr);

	    // menu
	    JMenu menu = newMenu("File");
	    menu.add(importFile.item());
	    menu.add(exportFile.item());
	    mbar.add(menu);
	    menu = newMenu("Edit");
	    menu.add(rename.item());
	    menu.add(delete.item());
	    mbar.add(menu);
	    root.setJMenuBar(mbar);	    

	    // empty content
	    jhdr = new JLabel("Nothing yet");
	    jpanel.add(jhdr);
	    jlist = new JList();
	    jlist.setBackground(glook().dark());
	    jlist.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	    jlist.addListSelectionListener(this);
	    jscroll = new JScrollPane(jlist);
	    jpanel.add(jscroll);
	    jlab = new JLabel("Nothing yet");
	    jpanel.add(jlab);
	    jimage = new JLabel();
	    jpanel.add(jimage);
	}

	// reconfigure jpanel
	private void reconfig(boolean checkSize) {
	    if (jhdr == null) return; // skip if invisible
	    String[] imageNames = images.imageNames();

	    // no action if same size
	    Dimension dim = jpanel.getSize();
	    if (checkSize
	    && jpanelDim != null 
	    && jpanelDim.width == dim.width
	    && jpanelDim.height == dim.height)
		return;
	    jpanelDim = dim;

	    // spacing parameters
	    int sp = glook().fontSize()*3/2;
	    int y = 0;

	    // header
	    jhdr.setText("This image set contains " + 
	    	 imageNames.length + " image(s).");
	    jhdr.setFont(glook().bigFont());
	    jhdr.setLocation(0,y);
	    setPref(jhdr);
	    y += sp;

	    // list content
	    jlist.setListData(imageNames);

	    // scolled list
	    jscroll.setLocation(sp, y);
	    int w = Math.max(2*sp, dim.width-2*sp);
	    jscroll.setSize(w, sp*7);
	    y += sp*8;

	    // image label
	    jlab.setLocation(sp, y);
	    jlab.setVisible(imageNames.length > 0);
	    y += sp;

	    // selected image
	    jimage.setLocation(sp,y);
	    jimage.setPreferredSize(new Dimension(w, sp*10));
	    y += sp*10;
	}

	// set size to preferred size
	private void setPref(JComponent j) {
	    j.setSize(j.getPreferredSize());
	}

	// refresh
	public void refresh() {
	    if (refreshing) return;
	    super.refresh();

	    // refresh image label
	    setTabLabel(l_hdr);
	    String name = (String) jlist.getSelectedValue();
	    if (Util.isBlank(name)) {
	    	jimage.setVisible(false);
		jlab.setText("No image currently selected");
		setPref(jlab);
		return;
	    }
	    PImage pimage = images.pimage(name);
	    Dimension orig = pimage.origDim();
	    String sfx = pimage.origFileSuffix();
	    jlab.setText(name + ": original suffix=" + sfx + 
	    	" size=(" + orig.width + " " + orig.height + ")");
	    setPref(jlab);

	    // selected image icon
	    Dimension avail = jimage.getPreferredSize();
	    double xfact = (avail.width + 0.0) / orig.width;
	    double yfact = (avail.height + 0.0) / orig.height;
	    double fact = Math.min(xfact, yfact);
	    if (fact <= 0) fact = 1;
	    int w = (int) (fact * orig.width);	    
	    int h = (int) (fact * orig.height);	    
	    Icon icon = icon(name, w, h);
	    jimage.setSize(new Dimension(w, h));
	    jimage.setIcon(icon);
	    jimage.setVisible(true);
	}	    	

	// selection list changed
	public void valueChanged(ListSelectionEvent e) {
	    if (e.getValueIsAdjusting()) return;
	    refresh(); 
	}

	// theme updated
	public void lookUpdated() { reconfig(false); } 

	// icon
	public Icon icon(String name) {
	    return icon(name, 0, 0);
	}
	public Icon icon(String name, int w, int h) {
	    PImage pimage = images.pimage(name);
	    if (pimage == null || pimage.imageData() == null)
		return null;
	    Image image = toolkit().createImage(pimage.imageData());
	    if (w>0 && h>0) 
		image = image.getScaledInstance(w, h, 
		   Image.SCALE_SMOOTH);
	    return new ImageIcon(image);
	}
}

