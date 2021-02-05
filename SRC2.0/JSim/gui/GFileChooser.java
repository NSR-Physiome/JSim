/*NSRCOPYRIGHT
	Copyright (C) 1999-2021 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSIM-specific JFileChooser
// and macOS FileDialog

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

	private static final String textExt[]= { ".txt", ".mod", ".flat", ".tac", ".rtml", 
                                   ".xml", ".cellml", ".sbml", ".java", ".c", ".h", ".f" };
	private static final String modelExt[]= { ".mod", ".flat", ".xml", ".cellml", ".sbml",
											  ".txt" }; 
	private static final String imageExt[]= { ".gif", ".jpeg", ".png" };
	private static final String dataExt[]= {".tac", ".jsml", ".pdata", ".csv", ".cdata", ".ldata"};
	private static boolean isMacOS; 

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
		FileDialog fd = null;
		JFileChooser fc = null;
	    glook = gnode.glook();
	    if (projFilter == null) 
		makeFilters(gnode.gappl().dataFormats());
		if(Util.isMacos()) isMacOS = true;
		else isMacOS = false;
	    try { 
			if(isMacOS) {
				Frame currFr = gnode.frame();
				fd = new FileDialog(currFr) ;
			}
			else {
				// JFileChooser with local icon
				fc = new JFileChooser(
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
			}
		
		// set filter
  		switch (which) {
		case PROJ: 
			if (isMacOS) {		
				if(isOpen) fd.setTitle("Open .proj (project) file");
				else { fd.setTitle("Save .proj (project) file");
				fd.setFilenameFilter(new FilenameFilter() {
				    @Override
					public boolean accept(File dir, String name) {
						return name.toLowerCase().endsWith(".proj");		
					}}); 
				}
			}
			else fc.setFileFilter(projFilter);
		    break;
		case TEXT:
			if(isMacOS) {
				if (isOpen)	fd.setTitle("Open a text (.txt, .mod, .flat, .tac, .rtml, .xml, .cellml, .sbml, .java, .c, .h, .f) file");
				else fd.setTitle("Save text file");
				fd.setFilenameFilter(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					for(int i=0; i< textExt.length; i++) {
						if(name.toLowerCase().endsWith(textExt[i]))
						   return true;
					}		
					return false;
				}}); 
			}
			else fc.setFileFilter(textFilter);
		    break;
		case MODEL: 
			if(isMacOS) {
				if (isOpen)	fd.setTitle("Open a model file");
				else fd.setTitle("Save model file");
				fd.setFilenameFilter(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					for(int i=0; i< modelExt.length; i++) {
						if(name.toLowerCase().endsWith(modelExt[i]))
						   return true;
					}
					return false;
				}});
			} 
			else fc.setFileFilter(modelFilter);
		    break;
		case DATASET:
			if(isMacOS) {
				if (isOpen) {
					fd.setTitle("Open a data file. (.tac .pdata .csv .cdata .ldata)");
					for (int i=0; i<dataFilters.length; i++)
						if (fmts.format(i).readSupported(true)) {
							fd.setFilenameFilter(new FilenameFilter() {
								@Override
								public boolean accept(File dir, String name) {
									for(int i=0; i< dataExt.length; i++) {
										if(name.toLowerCase().endsWith(dataExt[i]))
											return true;
									}		
									return false;
								}});
						}
				} else {
					fd.setTitle("Save data file. (.tac .pdata .csv .cdata .ldata)");
					fd.setFilenameFilter(new FilenameFilter() {
							@Override
							public boolean accept(File dir, String name) {
								for(int i=0; i< dataExt.length; i++) {
									if(name.toLowerCase().endsWith(dataExt[i]))
										return true;
								}		
								return false;
							}});
				}
			}
			else {
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
			}
		    break;
		case EPS: 
			if(isMacOS) {
				fd.setTitle("Export .eps (encapsulated postscript) file");
				fd.setFilenameFilter(new FilenameFilter() {
				    @Override
					public boolean accept(File dir, String name) {
						return name.toLowerCase().endsWith(".eps");
					}}); 
			}
			else fc.setFileFilter(exportEPSFilter);
			
		    break;
		case XSIMPAR: 
			if(isMacOS) {
				fd.setTitle("Export .par (XSIM parameter) file");
				fd.setFilenameFilter(new FilenameFilter() {
				    @Override
					public boolean accept(File dir, String name) {
						return name.toLowerCase().endsWith(".par");
					}}); 
			}
			else fc.setFileFilter(xsimParFilter);
		    break;
		case XML:
			if(isMacOS) {
				fd.setTitle("Export .xml file");
				fd.setFilenameFilter(new FilenameFilter() {
				    @Override
					public boolean accept(File dir, String name) {
						return name.toLowerCase().endsWith(".xml");
					}}); 
			} 
		    else fc.setFileFilter(xmlFilter);
		    break;
		case RTML: 
			if(isMacOS) {
				fd.setTitle("Export .rtml file");
				fd.setFilenameFilter(new FilenameFilter() {
				    @Override
					public boolean accept(File dir, String name) {
						return name.toLowerCase().endsWith(".rtml");
					}}); 
			}
		    else fc.setFileFilter(rtmlFilter);
		    break;
		case IMAGE: 
			if(isMacOS) {
				fd.setTitle("Image (.png, .jpeg, .gif) file");
				fd.setFilenameFilter(new FilenameFilter() {
				@Override
				public boolean accept(File dir, String name) {
					System.out.println("In image filter...");
					for(int i=0; i< modelExt.length; i++) {
						if(name.toLowerCase().endsWith(imageExt[i]))
						   return true;
					}
					return false;
				}});
			}
			else fc.setFileFilter(imageFilter);
		    break;
		case PARSET: 
			if(isMacOS) {
				fd.setTitle("Export .par (parameter set) file");
				fd.setFilenameFilter(new FilenameFilter() {
				    @Override
					public boolean accept(File dir, String name) {
						return name.toLowerCase().endsWith(".par");
					}}); 
			}
		    else fc.setFileFilter(parSetFilter);
		    break;
		}
  
		File f = null;
		// show dialog,  get info   
		if(isMacOS) {
			if(isOpen) fd.setMode(FileDialog.LOAD);
			else fd.setMode(FileDialog.SAVE); 
			fd.setAlwaysOnTop(true);
			fd.setVisible(true);
			File[] files = fd.getFiles();
			if(files.length == 0) return null; // no files selected.
			f = files[0];
			gnode.gappl().setUserDir(f.getParentFile());
		}
		else {
	   		int stat = isOpen ?
				fc.showOpenDialog(gnode.jcomp()) :
				fc.showSaveDialog(gnode.jcomp());
			if (stat != JFileChooser.APPROVE_OPTION) 
				return null;
			f = fc.getSelectedFile();
			gnode.gappl().setUserDir(f);
		}
		
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
			return null; // not reached,  needed for javac
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



