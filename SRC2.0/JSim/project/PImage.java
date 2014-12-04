/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// one image in project

package JSim.project;

import java.io.*;

import org.w3c.dom.Element;
import org.w3c.dom.Document;
import org.w3c.dom.Text;

import JSim.util.*;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Image;
import javax.swing.ImageIcon;

public class PImage extends PNamed {

	// state
	public StringControl origFileSuffix;
	private Dimension origDim;
	private byte[] imageData;

	// constructors
	public PImage(PNamed p, String n) throws Xcept {
	    super(p, n);
	    origFileSuffix = new StringControl(this, "origFileSuffix");	    	
	}

	// load image
	public void load(File f) throws Xcept {
	    imageData = null;
	    imageData = UtilIO.readBytes(f); 
	}

	// XML export
	public void exportExtraXML(Element e) {
	    if (imageData == null) return;
	    Document doc = e.getOwnerDocument();
	    Text text = doc.createTextNode(Hex.encode4(imageData));
	    e.appendChild(text);
	}   

	// XML import
	public void importXML(Element e) {
	    super.importXML(e);
	    String s = allText(e);
	    imageData = null;
	    try { 
		imageData = Hex.decode4(s);
	    } catch (Xcept x) {
		importXMLMessage(x.cleanMessage());
	    }
	}   

	// import an image file
	public void importFile(File f) throws Xcept {
	    origDim = null;
	    imageData = UtilIO.readBytes(f);
	    origFileSuffix.setVal(UtilIO.fileSuffix(f));
//	    childChanged(this);
	}

	// query original dimension
	public Dimension origDim() {
	    if (origDim != null) return origDim;
	    if (imageData == null) return null;
	    Image image = Toolkit.getDefaultToolkit().createImage(imageData());
	    ImageIcon icon = new ImageIcon(image);
	    int w = icon.getIconWidth();
	    int h = icon.getIconHeight();
	    origDim = new Dimension(w, h);
	    return origDim;
	}

	// query original file extension
	public String origFileSuffix() {
	    return origFileSuffix.val();
	}
		
	// query
	public String diagInfo() { return "Image " + name; }
	public String xmlLabel() { return "image"; }
	public byte[] imageData() { return imageData; }
}

