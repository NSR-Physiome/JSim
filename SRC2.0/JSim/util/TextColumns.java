/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.util;

// create String of columns

public class TextColumns {
	private int[] widths;  	// column widths
	private StringBuffer buf; // string buffer
	private int col; 	// current column

	// constructors
	public TextColumns() { this(null); }
	public TextColumns(int[] w) {
	    widths = w;
	    if (widths == null) widths = new int[0];
	    clear();
	}

	// add one column
	public void addColumn(int w) {
	    int[] old = widths;
	    widths = new int[ncols() + 1];
	    for (int i=0; i<old.length; i++)
		widths[i] = old[i];
	    widths[old.length] = w;
	}

	// add n identical columns
	public void addColumns(int n, int w) {
	    for (int i=0; i<n; i++) addColumn(w);
	}

	// add column,  write header
	public void addColumn(int w, String s) {
	    addColumn(w);
	    print(s);
	}

	// query
	public int ncols() { return widths.length; }
	public String toString() { return buf.toString(); }

	// clear text
	public void clear() {
	    buf = new StringBuffer();
	    col = 0;
	}

	// new line
	public void println() { 
	    buf.append("\n");
	    col = 0;
	}

	// print centered text into column
	public void print(String s) {
	    int n = col;
	    if (n>=ncols()) n = ncols()-1;
	    int w = (n>=0) ? widths[n] : 15;
	    while(s.length() < w)
		s = " " + s + " ";
	    if (s.length() > w)
		s = s.substring(0, w);
	    buf.append(s);
	    col++;
	}

	// print numbers
	public void print(float f) { 
	    print(PrettyFormat.sformat(f)); 
	}
	public void print(double d) { 
	    print(PrettyFormat.sformat(d)); 
	}
	public void print(double d, int p) { 
	    print(PrettyFormat.sformat(d, p)); 
	}
	public void print(int f) { print("" + f); }

}

