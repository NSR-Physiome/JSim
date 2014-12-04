/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// text printer

package JSim.gui;

import java.io.*;
import java.util.*;
import java.text.*;
import java.awt.*;
import java.awt.print.*;
import java.util.ArrayList;

import JSim.util.*;

public class GTextPrintable implements Printable {
	private String title;
	private String content;
	private String date;  // date of print

	// formatting characteristics
	private int fontSize = 8;
	private double space = fontSize*1.2;
	private int hlines = 2; // # header lines
	private int lmargin = 25;
	private int rmargin = 25;
	private int tmargin = 50;
	private int bmargin = 50;
	private ArrayList<Page> pages;

	// constructor
	public GTextPrintable(String t, String c) throws Xcept {
	    title = t;
	    content = c;
	    pages = null;  // loaded during print()

	    // format date
	    DateFormat dateFmt = 
		new SimpleDateFormat("ddMMMyy, HH:mm");
	    date = dateFmt.format(new Date());
	}

	// set pages
	private void setPages(PageFormat pf) throws PrinterException {

	    // content lines per page?
	    double avail = pf.getImageableHeight() - tmargin - bmargin;
	    int lpp = (int) (avail / space) - 2;
	    if (lpp < 1) throw new PrinterException(
		"no space available on printer page");

	    // initialize pages
	    pages = new ArrayList<Page>();
	    Page page = new Page();
	    pages.add(page);

	    // read content into pages
	    if (Util.isBlank(content)) 
		content = "<no text available>";
	    BufferedReader rdr = new BufferedReader(
		new StringReader(content));
	    String line = null;
	    try {
	    	while((line = rdr.readLine()) != null) {
		    if (page.nLines() >= lpp) {
		    	page = new Page();
		    	pages.add(page);
		    }
		    page.addLine(line);
		}
	    } catch (IOException e) {
		throw new PrinterException(e.toString());
	    }
	}

	// print a page
	public int print(Graphics g, PageFormat pf,
	int inx) throws PrinterException {
	    if (pages == null) setPages(pf);
	    if (inx < 0 || inx >= pages.size())
		return NO_SUCH_PAGE;
	    Page page = (Page) pages.get(inx);

	    // setup graphics
	    g.setColor(Color.black);
            g.setClip((int) pf.getImageableX(),
		(int) pf.getImageableY(),
		(int) pf.getImageableWidth(),
		(int) pf.getImageableHeight());
	    Rectangle r = g.getClipBounds();
	    g.translate(r.x + lmargin, r.y + tmargin);

	    // content
	    Font f = new Font("Monospaced", Font.BOLD, fontSize);
	    g.setFont(f);
	    
	    String s = title + "     " + date + "  page " + 
		(inx+1) + "/" + pages.size();
	    g.drawString(s, 0, 0);
	    f = new Font("Monospaced", Font.PLAIN, fontSize);
	    g.setFont(f);
	    for (int i=0; i<page.nLines(); i++) {
		double ypos = space*(i+hlines);
		String line = page.line(i);
		// trailing tabs mess up some HP printers
		line = Util.removeTrailingWhitespace(line);
		if (line.length() == 0) continue;
		g.drawString(line, 0, (int) ypos);
	    }
	    return PAGE_EXISTS;
	}	    

	// GTextPrintable.Page class
	public static class Page {
	    private StringList lines;
	    
	    public Page() {
		lines = new StringList(40);
	    }

	    // add a line to this page
	    public void addLine(String s) {
		lines.add(s);
	    }

	    // query
	    public int nLines() { return lines.size(); }

	    // one line of content
	    public String line(int i) {
		return lines.str(i);
	    }
	}
}


	    
	
