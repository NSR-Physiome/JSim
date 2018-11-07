/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// preferences

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.*;
import java.util.*;
import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.model.*;

public class GPrefs {
	public int[] rgb;
	public int fontSize;
	public int[] winSize;
	public Boolean bkupProj;

	// create preferences from theme
	public GPrefs(GLook glook, Boolean bkup) {
	    rgb = glook.rgb();
	    fontSize = glook.fontSize();
	    winSize = glook.winSize();
		bkupProj = new Boolean(bkup);
	}

	// load preferences from file
	public GPrefs(GAppl gappl, File f) throws Xcept {
	    String txt = UtilIO.readText(f);
	    Document doc = UtilXML.parse(txt);
	    Element root = doc.getDocumentElement();
	    NodeList list = root.getChildNodes();
	    Element pref = null;
	    for (int i=0; i<list.getLength(); i++) {
		Node node = list.item(i);
		if (! (node instanceof Element)) continue;
		String n = node.getNodeName();
		if (! n.equals("preferences")) continue;
		pref = (Element) node;
		break;
	    }
	    list = pref.getChildNodes();
	    for (int i=0; i<list.getLength(); i++) {
		Node node = list.item(i);
		if (! (node instanceof Element)) continue;
		String n = node.getNodeName();
		if (! n.equals("preference")) continue;
		Element e = (Element) node;
		String name = e.getAttribute("name");
		String value = e.getAttribute("value");
		if (name.equals("color")) {
		    rgb = intArrVal(value);
		    if (rgb.length != 3) throw new Xcept(
			"illegal format for background color preference in file "
			+ f.getPath());
		}
		if (name.equals("fontSize"))
		    fontSize = Util.toInt(value);
		if (name.equals("windowSize")) {
		    winSize = intArrVal(value);
		    if (winSize.length != 2) throw new Xcept(
			"illegal format for window size preference in file " 
			+ f.getPath());
		}

		if(name.equals("bkupProj")) {
			bkupProj = new Boolean(value);
		}
	    }
	}

	// String representation
	public String toString() {
	    return "color=" + rgb[0] + "x" + rgb[1] + "x" +
		rgb[2] + " fontSize=" + fontSize + 
		" windowSize=" + winSize[0] + "x" + winSize[1];
	}

	// write pref file
	public void write(File f) throws Xcept {
	    PrintWriter wrt = null;
	    try {
	    	wrt = new PrintWriter(new FileWriter(f), true);
	    } catch (IOException e) {
		throw Xcept.wrap(e);
	    }
	    wrt.println(XMLWriter.XMLHDR);
	    wrt.println("<JSim version=\"" + Util.version() + "\">");
	    wrt.println("  <preferences>");
	    wrt.println("    <preference name=\"color\" value=\"" +
		rgb[0] + "x" + rgb[1] + "x" + rgb[2] + "\"/>");
	    wrt.println("    <preference name=\"fontSize\" value=\"" +
		fontSize + "\"/>");
	    wrt.println("    <preference name=\"windowSize\" value=\"" +
		winSize[0] + "x" + winSize[1]+ "\"/>");
	    wrt.println("    <preference name=\"bkupProj\" value=\"" +
					bkupProj.toString() + "\"/>");
	    wrt.println("  </preferences>");
	    wrt.println("</JSim>");
	    wrt.close();
	}

	// int array value
	public static int[] intArrVal(String s) throws Xcept {
	    String[] xarr = xArray(s);
	    int[] arr = new int[xarr.length];
	    for (int i=0; i<arr.length; i++) 
		arr[i] = Util.toInt(xarr[i]);
	    return arr;
	}

	// separate sub-values
	private static String[] xArray(String s) {
	    if (s == null) return new String[0];
	    StringTokenizer stok = new StringTokenizer(s, "x");
	    String[] sarr = new String[stok.countTokens()];
	    int i=0;
	    while(stok.hasMoreTokens()) 
		sarr[i++] = stok.nextToken();
	    return sarr;
	}

}

