/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Uniq content list

package JSim.util;

import java.util.ArrayList;

public class UniqList<E> extends ArrayList<E> implements DiagInfo {

	// constructor
	public UniqList() { super(); }

	// add, ensuring uniqness
	public boolean add(E e) {
	    if (contains(e))
	    	return false;
	    else 
	    	return super.add(e);
	}
	public boolean addAll(ArrayList<E> list) {
	    boolean ret = false;
	    for (int i=0; i<list.size(); i++)
	        ret = ret || add(list.get(i));
	    return ret;
	}
	
	// add quickly, but unsafe, no duplicate protection
	public boolean addUnsafe(E e) {
	    return super.add(e);
	} 

	// diag info
	public String diagInfo() { return super.toString(); }

	// test harness
	public static void main(String[] args) {
	    UniqList<String> l1 = new UniqList<String>();
	    l1.add("aaa");
	    l1.add("bbb");
	    l1.add("aaa");
	    System.err.println("l1=" + l1);
	    UniqList<String> l2 = new UniqList<String>();
	    l2.add("aaa");
	    l2.add("ccc");
	    l2.add("ccc");
	    System.err.println("l2=" + l2);
	    l1.addAll(l2);
	    System.err.println("addAll=" + l1);
	}
}
