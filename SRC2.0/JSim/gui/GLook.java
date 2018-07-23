/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// JSim LookAndFeel management

package JSim.gui;

import java.awt.*;
import java.awt.geom.*;
import javax.swing.*;
import javax.swing.plaf.*;
import javax.swing.plaf.metal.*;
import java.net.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.model.*;

public class GLook {
	private GMain gmain;  // used to update UI components
	private String name;  // current LookAndFeel name
	private int[] rgb;    // JSim base color
	private int fontSize; // base font size
	private int[] winSize;// JSim project window size

	// fixed colors
	private Color[] plotColors;
	private ColorUIResource disabled;
	private final Color light, dark;
	private final ColorUIResource bright;
	protected static final int[] DEFAULT_BACKGROUND_RGB 
	    = new int[] { 135, 200, 255 };
		
	// calculated properties
	private ColorUIResource bg, deep, bg_lighter, bg_darker;  
	private FontUIResource baseFont, textFont, bigFont;
	private Font[] baseFontScaled;
	private GLookMetal gmetal;

	// icons
	private Icon jsimIcon, projectIcon, unknownIcon, 
	    modelIcon, parsetIcon, parsetSelectIcon,
	    datasetIcon, plotpageIcon, nestedIcon, graphicIcon, 
	    notesIcon, imagesetIcon,
	    selectIcon, funcgenIcon,
	    userIcon, runningIcon, closeIcon,
	    nextIcon, prevIcon, lockIcon, unlockIcon;

	private static String metalLookName = 
	    "javax.swing.plaf.metal.MetalLookAndFeel";
	private String baseLookName;
	private UIManager.LookAndFeelInfo[] looks; // available look
	private LookAndFeel currLook; // current installed look

	// constructor
	public GLook(GMain main, String n, int[] rgbx, int fs, int[] ws) {
	    gmain = main;

	    // load default/available looks, display if verbose mode
	    LookAndFeel baseLook = UIManager.getLookAndFeel();
	    baseLookName = (baseLook == null) ? 
	    	metalLookName : baseLook.getClass().getName();
	    looks = UIManager.getInstalledLookAndFeels();
	    Util.verbose("Available LookAndFeels:");
	    for (int i=0; i<looks.length; i++)
	    	Util.verbose("\t" + looks[i].getName() + 
		    ":  " + looks[i].getClassName());
	    Util.verbose("Default LookAndFeel: " + 
	       ((baseLook == null) ? "null" : baseLook.getName()));

	    // fixed color initialization
	    light = new Color(0.975f, 0.96f, 0.96f);
	    dark = new Color(0.925f, 0.92f, 0.92f);
	    bright = new ColorUIResource(250,200,120);
	    disabled = new ColorUIResource(140,140,140); // gray
	    plotColors = new Color[13];
	    int k=0;
	    plotColors[k++] = Color.black; // black
	    plotColors[k++] = Color.red; // red
	    plotColors[k++] = Color.green; // green
	    plotColors[k++] = new Color(255,127,0); // orange
	    plotColors[k++] = Color.blue;  // blue
	    plotColors[k++] = new Color(0,64,127); // indigo
	    plotColors[k++] = new Color(127,0,127); // violet2
	    plotColors[k++] = new Color(140,140,140); // gray
	    plotColors[k++] = new Color(46,139,87); // forest
	    plotColors[k++] = new Color(233,150,122); // salmon
	    plotColors[k++] = new Color(139,35,35); // brown
	    plotColors[k++] = new Color(160,32,240); // violet
	    plotColors[k++] = new Color(255,204,51); // dark yellow .. Added

	    // read icons
	    jsimIcon = readIcon("jsim.gif");
	    modelIcon = readIcon("model.gif");
	    parsetIcon = readIcon("parset.gif");
	    parsetSelectIcon = readIcon("parsetg.gif");
	    datasetIcon = readIcon("dataset.gif");
	    plotpageIcon = readIcon("plotpage.gif");
	    nestedIcon = readIcon("nested.gif");
	    graphicIcon = readIcon("graphic.gif");
	    notesIcon = readIcon("notes.gif");
	    imagesetIcon = readIcon("imageset.gif");
	    projectIcon = readIcon("project.gif");
	    unknownIcon = readIcon("unknown.gif");
	    selectIcon = readIcon("select.gif");
	    funcgenIcon = readIcon("funcgen.gif");
	    userIcon = readIcon("user.gif");
	    runningIcon = readIcon("running.gif");
	    closeIcon = readIcon("close.gif");
	    nextIcon = readIcon("next.png");
	    prevIcon = readIcon("prev.png");
	    lockIcon = readIcon("lock.png");
	    unlockIcon = readIcon("unlock.png");

	    // create theme
	    name = n;
	    rgb = rgbx;
	    fontSize = fs;
	    winSize = ws;
	    update();
	}
	
	// update the look
	public void resetColor(int[] rgbx) {
	    rgb = rgbx;
	    update();
	}
	public void resetFont(int fs) {
	    fontSize = fs;
	    update();
	}

	// update internals
	private void update() {

	    // bullet proof parameters
	    if (rgb != null) rgb = (int[]) rgb.clone();
	    if (rgb == null || rgb.length != 3) 
		rgb = DEFAULT_BACKGROUND_RGB;
	    if (rgb[0] < 100 && rgb[1] < 100 && rgb[2] < 100)
		rgb = DEFAULT_BACKGROUND_RGB;
	    if (fontSize == 0) fontSize = 12;
	    if (fontSize < 5) fontSize = 5;
	    if (fontSize > 26) fontSize = 26;
	    if (winSize != null) winSize = (int[]) winSize.clone();
	    if (winSize == null || winSize.length != 2)
		winSize = new int[] { 1000, 800 };
	    if (winSize[0] < 100 || winSize[1] < 100)
		winSize = new int[] { 1000, 800 };		

	    // color init
	    bg = color(rgb);
	    int mx = 2;
	    if (rgb[1] > rgb[2]) mx = 1;
	    if (rgb[0] > rgb[1] && rgb[0] > rgb[2]) mx = 0;
	    int[] deepx = new int[3];
	    deepx[mx] = 64;
	    deep = color(deepx);
	    int[] vec = new int[]{ 60, 60, 60 };
	    vec[mx] = 0;
	    bg_lighter = color(rgb, vec);
	    vec[0] *= -1;
	    vec[1] *= -1;
	    vec[2] *= -1;
	    bg_darker = color(rgb, vec);

	    // font init
	    baseFont = new FontUIResource(family(fontSize), Font.BOLD, fontSize);
	    textFont = new FontUIResource("Monospaced", Font.PLAIN, fontSize);
	    bigFont = new FontUIResource(family(fontSize+2), Font.BOLD, fontSize+2);
	    baseFontScaled = new Font[64];

	    // always create metal while still has JSim dependencies 
	    gmetal = new GLookMetal(this);

	    // create/install look and feel
	    try {
	    	install();
	    } catch (Exception e) {
	    	System.err.println("Error installing LookAndFeel:");
		e.printStackTrace();
	    }

	    // update JSim component UIs
	    gmain.updateUI();
	}

	// instantiate and install the LookAndFeel
	private void install() throws Exception {
	    String className = null;
	    for (int i=0; i<looks.length; i++)
	    	if (lookMatches(looks[i])) 
		    className = looks[i].getClassName();
	    if (className == null) {
		className = baseLookName;
	    	System.err.println("LookAndFeel " 
		    + name + " not found.  Using Metal instead.");
	    } else {
	        Util.verbose("Installing " + className);
	    }
	    Class<?> clss = Class.forName(className);
	    currLook = (LookAndFeel)
	    	clss.getConstructor().newInstance();
	    UIManager.setLookAndFeel(currLook);
	}

	// does look info match current name?
	public boolean lookMatches(UIManager.LookAndFeelInfo info) {
	    if (name.equalsIgnoreCase(info.getName()))
	    	return true;
	    String cname = info.getClassName();
	    if (name.equalsIgnoreCase(cname))
	    	return true;
	    int inx = cname.lastIndexOf(".");
	    if (inx > 0) {
	    	cname = cname.substring(inx+1);
		if (name.equalsIgnoreCase(cname))
		    return true;
	    }
	    return false;
	}

	// set winSize from Project window
	public void setWinSize(GProject gproj) {
	    Dimension d = gproj.rootSize();
	    if (d == null) return;
	    winSize[0] = d.width;
	    winSize[1] = d.height;
	}

	//// QUERY METHODS

	// LookAndFeel queries
	public boolean isMetal() {
	    if (currLook == null) return true;
	    return currLook instanceof MetalLookAndFeel;
	}

	// color queries
	public ColorUIResource bg() { return bg; }
	public ColorUIResource bg_lighter() { return bg_lighter; }
	public ColorUIResource bg_darker() { return bg_darker; }
	public ColorUIResource deep() { return deep; }
	public ColorUIResource disabledColor() { return disabled; }
	public ColorUIResource bright() { return bright; }
	public Color light() { return light; }
	public Color dark() { return dark; }
	public Color plotColor(int i) { 
	    if (i<0) i *= -1;
	    i = i%plotColors.length;
	    return plotColors[i];
	}
	public Color goodPlotColor(int i) {
	    i = i%7;
	    if (i==2) 
		i += 3;
	    else if (i>2) 
		i += 4;
	    return plotColor(i);
	}
	
	// font queries
	public FontUIResource baseFont() { return baseFont; }
	public FontUIResource textFont() { return textFont; }
	public FontUIResource bigFont() { return bigFont; }
	public Font font(double mult) { // Scaled font
	    int i = (int) (fontSize*mult);
	    if (baseFontScaled[i] == null)
//	        baseFontScaled[i] = baseFont.deriveFont((float) i);
		baseFontScaled[i] = new Font(
		    family(i), Font.BOLD, i);
	    return baseFontScaled[i];
	}

	// icon queries
	public Icon jsimIcon() { return jsimIcon; }
	public Icon modelIcon() { return modelIcon; }
	public Icon parsetIcon() { return parsetIcon; }
	public Icon datasetIcon() { return datasetIcon; }
	public Icon plotpageIcon() { return plotpageIcon; }
	public Icon nestedIcon() { return nestedIcon; }
	public Icon graphicIcon() { return graphicIcon; }
	public Icon notesIcon() { return notesIcon; }
	public Icon imagesetIcon() { return imagesetIcon; }
	public Icon projectIcon() { return projectIcon; }
	public Icon selectIcon() { return selectIcon; }
	public Icon funcgenIcon() { return funcgenIcon; }
	public Icon fileIcon() { return projectIcon; }
	public Icon runningIcon() { return runningIcon; }
	public Icon closeIcon() { return closeIcon; }
	public Icon parsetIcon(boolean select) {
	    return select ? parsetSelectIcon : parsetIcon;
	}
	public Icon userIcon() { return userIcon; }
	public Icon nextIcon() { return nextIcon; }
	public Icon prevIcon() { return prevIcon; }
	public Icon lockIcon() { return lockIcon; }
	public Icon unlockIcon() { return unlockIcon; }

	// icons for Project tabs
	public Icon tabIcon(GNode gnode) {
	    if (gnode == null) return null;
	    return tabIcon(gnode.getClass());
	}

	// icons for Project tabs
	public Icon tabIcon(Class gclass) {
	    if (gclass == GModel.class) return modelIcon;
	    if (gclass == GParSet.class) return parsetIcon;
	    if (gclass == GDataSet.class) return datasetIcon;
	    if (gclass == GPlotPage.class) return plotpageIcon;
	    if (gclass == GNested.class) return nestedIcon;
	    if (gclass == GGraphicTab.class) return graphicIcon;
	    if (gclass == GNotes.class) return notesIcon;
	    if (gclass == GSemSim.class) return notesIcon;
	    if (gclass == GImageSet.class) return imagesetIcon;
	    if (gclass == GTree.class) return projectIcon;

	    if (gclass == PModel.class) return modelIcon;
	    if (gclass == ParSet.class) return parsetIcon;
	    if (gclass == PDataSet.class) return datasetIcon;
	    if (gclass == PlotPage.class) return plotpageIcon;
	    if (gclass == PNested.class) return nestedIcon;
	    if (gclass == PGraphic.class) return graphicIcon;
	    if (gclass == PNotes.class) return notesIcon;
		if (gclass == PSemSim.class) return notesIcon;
	    if (gclass == PImageSet.class) return imagesetIcon;
	    if (gclass == Project.class) return projectIcon;
	    return null;
	}

	// other query
	public int[] rgb() { return rgb; }
	public int fontSize() { return fontSize; }
	public int[] winSize() { return winSize; }
	public Dimension winDim() { return new Dimension(
	    winSize[0], winSize[1]); }

	// read icon
	private Icon readIcon(String fileName) {
	    URL url = getClass().getResource(
		"icons/" + fileName);
	    if (url == null) return null;
	    return new ImageIcon(url);
	}

	// font family
	private String family(int fs) {
	    return (fs<13) ? "Helvetica" : "Lucida";
	}

	// color utilities
	private ColorUIResource color(int[] rgb) {
	    return new ColorUIResource(rgb[0], rgb[1], rgb[2]);
	}
	private ColorUIResource color(int r, int g, int b) {
	    return new ColorUIResource(r, g, b);
	}
	private ColorUIResource color(int[] rgb, int[] vec) {
	    int r = rgb[0] + vec[0];
	    int g = rgb[1] + vec[1];
	    int b = rgb[2] + vec[2];
	    if (r>255) r = 255; if (r<0) r = 0;
	    if (g>255) g = 255; if (g<0) g = 0;
	    if (b>255) b = 255; if (b<0) b = 0;
	    return color(r,g,b);
	}
}

