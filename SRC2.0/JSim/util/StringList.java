/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// String list

package JSim.util;

import java.io.*;
import java.util.*;

public class StringList extends ArrayList<String> implements Serializable{

	// constructors
	public StringList() { super(); }
	public StringList(int n) { super(n); }
	public StringList(String s) {
	    super(4);
	    add(s);
	}
	public StringList(String[] s) {
	    super(s.length);
	    for (int i=0; i<s.length; i++) 
	        if (s[i] != null)
		    add(s[i]);
	}
	public StringList(StringList list) {
	    super(list.size());
	    addAll(list);
	}
	public StringList(StringTokenizer stok) {
	    this(stok.countTokens());
	    while (stok.hasMoreTokens())
		add(stok.nextToken());
	}
	public StringList(String path, String sep) {
	    this(path, sep, 0);
	}
	    
	public StringList(String path, String sep, int wrap) {
	    this();
	    addPath(path, sep, wrap);
	}

	public StringList(int[] is) {
	    this();
	    for (int i=0; i<is.length; i++) 
	    	add("" + is[i]);
	}

	// add path
	public void addPath(String path, String sep, int wrap) {
	    if (path == null) return;
	    StringTokenizer stok = new StringTokenizer(
		path, sep);
	    while (stok.hasMoreElements()) {
		String s = stok.nextToken();
		if (! Util.isBlank(s)) add(s, wrap);
	    }
	}

	// add string with wrap
	public void add(String s, int wrap) {
	    while (wrap > 0 && s.length() > wrap) {
	    	add(s.substring(0, wrap));
		s = s.substring(wrap);
	    }
	    add(s);
	}

	// query
	public String str(int i) { return (String) get(i); }
	public int indexOf(String s) {
	    for (int i=0; i<size(); i++) 
		if (s.equals(str(i))) return i;
	    return -1;
	}
	public boolean containSame(String s) {
	    return indexOf(s)>=0;
	}
	public boolean containSame(StringList list) {
	    for (int i=0; i<list.size(); i++) 
	    	if (! containSame(list.str(i))) 
		    return false;
	    return true;
	}
	public void remove(String s) {
	    int i = indexOf(s);
	    if (i>=0) remove(i);
	}
	public void remove(StringList s) {
	    for (int i=0; i<s.size(); i++)
	        remove(s.str(i));
	}
	public void addUniq(String s) {
	    if (containSame(s)) return;
	    add(s);
	}
	public void addUniq(String[] s) {
	    if (s == null) return;
	    for (int i=0; i<s.length; i++) addUniq(s[i]);
	}
	public String[] array() {
	    String[] arr = new String[size()];
	    for (int i=0; i<size(); i++) arr[i] = str(i);
	    return arr;
	}
	public StringList separateLines() {
	    StringList list = new StringList(size());
	    for (int i=0; i<size(); i++) {
		StringTokenizer stok = new StringTokenizer(
		    str(i), "\n");
		while (stok.hasMoreTokens()) {
		    String s = stok.nextToken();
		    if (! Util.isBlank(s))
			list.add(s);
		}
	    }
	    return list;
	}
	public StringList javaEncode() {
	    StringList list = new StringList(size());
	    for (int i=0; i<size(); i++) 
		list.add(Util.javaEncode(str(i)));
	    return list;
	}

	// separated list
	public String toString(String sep, boolean quoted) {
	    StringBuffer buf = new StringBuffer();
	    for (int i=0; i<size(); i++) {
		if (i>0) buf.append(sep);
		String s = str(i);
		if (quoted) s = "\"" + s + "\"";
		buf.append(s);
	    }
	    return buf.toString();
	}

	// String is in array?
	public static boolean containSame(String s, String[] arr) {
	    if (arr == null || s == null) return false;
	    for (int i=0; i<arr.length; i++) 
		if (s.equals(arr[i])) return true;
	    return false;
	}

	// String base is in array?
	public static boolean containStart(String s, String[] arr) {
	    if (arr == null || s == null) return false;
	    for (int i=0; i<arr.length; i++) 
		if (arr[i].startsWith(s)) return true;
	    return false;
	}

}
