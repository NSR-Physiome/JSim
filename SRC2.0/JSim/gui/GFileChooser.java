/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSIM-specific JFileChooser

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;

public class GFileChooser {

	// static constants, filters
	public static final int PROJ = 0;
	public static final int TEXT = 1;
	public static final int MODEL = 2;
	public static final int DATASET = 3;
	public static final int EPS = 4;
	public static final int XSIMPAR = 5;
	public static final int XML = 6;
	public static final int RTML = 7;
	public static final int IMAGE = 8;
	public static final int PARSET = 9;
	private static DataFormat.List fmts;
	private static Filter projFilter;
	private static Filter textFilter;
	private static Filter modelFilter;
	private static Filter[] dataFilters;
	private static Filter importDataFilter;
	private static Filter exportDataFilter;
	private static Filter exportEPSFilter;
	private static Filter xsimParFilter;
	private static Filter xmlFilter;
	private static Filter rtmlFilter;
	private static Filter imageFilter;
	private static Filter parSetFilter;

	// HACK to communicate with inner classes
	private static GLook glook;

	// create static filters
	private static void makeFilters(DataFormat.List f) {
	    fmts = f;
	    projFilter = new Filter("JSim project files", "proj");
	    modelFilter = new Filter("JSim model files", 
		new String[] { "mod", "flat", "xml", "cellml", "sbml",
		"txt" });
	    textFilter = new Filter("JSim text files", 
		new String[] { "txt", "mod", "flat",
		   "tac", "rtml", "xml", "cellml", "sbml",
		   "java", "c", "h", "f" });
	    exportEPSFilter = new Filter(
		"Encapsulated Postscript files", "eps");
	    xsimParFilter = new Filter(
		"XSim parameter files", "par");
	    parSetFilter = new Filter(
		"Parameter files", "par");
	    xmlFilter = new Filter("XML files", "xml");
	    rtmlFilter = new Filter("RTML files", "rtml");
	    imageFilter = new Filter("Image files",
	        new String[] { "gif", "jpeg", "png" });
		
	    dataFilters = new Filter[fmts.size()];
	    StringList esfx = new StringList(fmts.size());
	    StringList isfx = new StringList(fmts.size());
	    for (int i=0; i<fmts.size(); i++) {
		DataFormat fmt = fmts.format(i);
		dataFilters[i] = new Filter(fmt);
		String[] sfxs = fmt.suffixes();
		for (int j=0; j<sfxs.length; j++) {
		    String sfx = sfxs[j];
		    esfx.add(sfx);
		    if (fmt.readSupported(true)) 
		    	isfx.add(sfx);
		}
	    }
	    importDataFilter = new Filter(
		"Importable data files", isfx.array());
	    exportDataFilter = new Filter(
		"Exportable data files", esfx.array());
	}	 

	// show select file dialog,  return File or  null
	public static Info select(GNode gnode,
	boolean isOpen, int which) throws Xcept {
	    glook = gnode.glook();
	    if (projFilter == null) 
		makeFilters(gnode.gappl().dataFormats());

	    try { 
		// JFileChooser with local icon
		JFileChooser fc = new JFileChooser(
		    gnode.gappl().userDir()) {
		    public Icon getIcon(File f) {
			if (f.isDirectory())
			    return super.getIcon(f);
			if (modelFilter.accept(f))
		  	    return glook.modelIcon();
	   	    	if (importDataFilter.accept(f))
			    return glook.datasetIcon();
	    		if (projFilter.accept(f))
			    return glook.projectIcon();
	    		return super.getIcon(f);
		    }
		};

		// set filter
		switch (which) {
		case PROJ: 
		    fc.setFileFilter(projFilter);
		    break;
		case TEXT: 
		    fc.setFileFilter(textFilter);
		    break;
		case MODEL: 
		    fc.setFileFilter(modelFilter);
		    break;
		case DATASET: 
		    if (isOpen) {
		    	for (int i=0; i<dataFilters.length; i++)
			    if (fmts.format(i).readSupported(true))
			    	fc.addChoosableFileFilter(dataFilters[i]);
		    	fc.setFileFilter(importDataFilter);
		    } else {
		    	for (int i=0; i<dataFilters.length; i++)
			    fc.addChoosableFileFilter(dataFilters[i]);
		    	fc.setFileFilter(exportDataFilter);
			fc.setAccessory(dataAccessory());
		    }
		    break;
		case EPS: 
		    fc.setFileFilter(exportEPSFilter);
		    break;
		case XSIMPAR: 
		    fc.setFileFilter(xsimParFilter);
		    break;
		case XML: 
		    fc.setFileFilter(xmlFilter);
		    break;
		case RTML: 
		    fc.setFileFilter(rtmlFilter);
		    break;
		case IMAGE: 
		    fc.setFileFilter(imageFilter);
		    break;
		case PARSET: 
		    fc.setFileFilter(parSetFilter);
		    break;
		}

		// show dialog,  get info   
		int stat = isOpen ?
		    fc.showOpenDialog(gnode.jcomp()) :
		    fc.showSaveDialog(gnode.jcomp());
		if (stat != JFileChooser.APPROVE_OPTION) 
		    return null;
		File f = fc.getSelectedFile();
	        gnode.gappl().setUserDir(f);
		if (f == null) return null;
		Info info = new Info(f);
		if (which != DATASET || isOpen) return info;
		String sfx = UtilIO.fileSuffix(f);
		info.doublePrecision = true;
		info.dataFormat = fmts.forSuffix(sfx);
		if (info.dataFormat == null) throw new Xcept(
		    "Export file suffix must be " +
		    ".tac .jsml .pdata .csv .cdata or .ldata");
		return info;

	    } catch (SecurityException e) {
		gnode.gproject().securityXcept();
		return null; // no reached,  needed for javac
	    }
	}

	// create help accessory
	public static JComponent dataAccessory() {
	    String[] text = new String[] {
		"Export data format is determined by",
		"file suffix.  Supported suffixes are:",
		"   .tac .jsml .csv .cdata .pdata .ldata",
		"See 'Files of Type' below for details."
	    };
	    JPanel jpanel = new JPanel(new GridLayout(text.length,1));
	    for (int i=0; i<text.length; i++) 
		jpanel.add(new JLabel(text[i]));
	    return jpanel;
	}

	// query
	public static Filter modelFilter() { return modelFilter; }

	// GFileChooser.Filter sub-class
	public static class Filter extends javax.swing.filechooser.FileFilter {
	    private String desc;
	    private String[] sfxs;

	    // constructors
	    public Filter(String d, String[] s) {
	    	super();
	    	sfxs = s;
	    	desc = d + " (";
		for (int i=0; i<s.length; i++) {
		    if (i!=0) desc = desc + " ";
		    desc = desc + "." + sfxs[i];
		}
		desc = desc + ")";
		
	    }	
	    public Filter(String d, String s) {
	    	this(d, new String[] { s });
	    }
	    public Filter(DataFormat fmt) {
	    	this(fmt.longName(), fmt.suffixes());
	    }

	    // accept file?
	    public boolean accept(File f) {
	    	if (f.isDirectory()) return true;
	    	String s = UtilIO.fileSuffix(f);
	    	for (int i=0; i<sfxs.length; i++) 
		    if (s.equals(sfxs[i])) return true;
	    	return false;
	    }

	    public String getDescription() { return desc; }
	}


	// GFileChooser.Info
	public static class Info {
	    public File file;
	    public boolean doublePrecision;
	    public DataFormat dataFormat;
	    public Info(File f) {
		file = f;
	    }
	}
}



