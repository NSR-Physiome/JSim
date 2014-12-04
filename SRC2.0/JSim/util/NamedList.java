/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.util;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;

// growable array with named items
public class NamedList extends ArrayList<Named> {
	
	private ArrayList<Item> namelist;

	// constructors
	public NamedList() { 
	    super(); 
	    namelist = new ArrayList<Item>();
	}
	public NamedList(int i) { 
	    super(i);
	    namelist = new ArrayList<Item>(i);
	}

	// add element,  true on all,  false on dup or no name
	public boolean add(Named o) {
	    String n = o.name();
	    if (n == null) return false;

	    // look for name
	    int i = Collections.binarySearch(namelist, n);
	    if (i>=0) return false;
	    super.add(o);
	    Item item = new Item(n, size()-1);
	    namelist.add(-(i+1), item);
	    return true;
	}

	// already know name ordering
	public void add(int i, Named o) { addNamed(i, o); }
	public boolean addNamed(int i, Named o) {
	    String n = o.name();
	    if (n == null) return false;
	    super.add(o);
	    namelist.add(i, new Item(o.name(), size()-1));
	    return true;
	}

	// rename
	public boolean rename(String oldname, String newname) throws Xcept {
	    if (oldname.equals(newname)) return true;
	    if (getByName(newname) != null) return false;
	    int i = Collections.binarySearch(namelist, oldname);
	    if (i<0) return false;
	    Item olditem = (Item) namelist.get(i);
	    namelist.remove(i);
	    i = Collections.binarySearch(namelist, newname);
	    if (i>=0) throw new Xcept(
		"Internal corruption error renaming " + 
		oldname + " to " + newname);
	    namelist.add(-(i+1), new Item(newname, olditem.inx));
	    return true;
	}

	// replace existing item
	public void replace(Named named) throws Xcept {
	    int inx = inx(named.name());
	    if (inx<0) throw new Xcept("NamedList.replace() failed");
	    remove(inx);
	    super.add(inx, named);
	}

	// set or replace existing item
	public void set(Named named) throws Xcept {
	    if (inx(named.name()) >= 0)
	    	replace(named);
	    add(named);
	}

	// return var inx or -1-insertion_index
	public int inx(String n) {
	    int i =  Collections.binarySearch(namelist, n);
	    if (i>=0) {
		Item item = (Item) namelist.get(i);
		return item.inx;
	    } 
	    return i;   
	}

	// return object by name
	public Object getByName(String n) {
	    int i = Collections.binarySearch(namelist, n);
	    if (i<0) return null;
	    Item item = (Item) namelist.get(i);
	    if (item.inx >= size()) return null; // NEW EB Oct 6 2004 ???
	    return get(item.inx);
	}

	// remove item, return success
	public boolean remove(String name) {
	    int ninx = Collections.binarySearch(namelist, name);
//System.out.println("name " + name + " ninx=" + ninx);
	    if (ninx<0) return false;
	    Item item = (Item) namelist.get(ninx);
//System.out.println("remove " + ninx + " " + item.name + " " + item.inx);
	    remove(item.inx);
	    namelist.remove(ninx);
	    for (int i=0; i<namelist.size(); i++) {
		Item item1 = (Item) namelist.get(i);
		if (item1.inx>item.inx) item1.inx--;
//System.out.println("decrement " + i + " " + item1.name + " " + item1.inx);
	    }
	    return true;
	}
	    
	// dump diagnostic info
	public void dumpname(PrintStream out) {
	    for (int i=0; i<namelist.size(); i++) {
		Item item = (Item) namelist.get(i);
		out.println("Name " + i + " " +  
		    item.name + " " + item.inx + " " + get(item.inx));
	    }
	    for (int i=0; i<size(); i++) {
		out.println("Array " + i + " " + get(i)); 
	    }
	}

	// name lookup list item
	public class Item implements Comparable<String> {
	    String name;
	    int inx;

	    public Item(String s, int i) {
		name = s;
		inx = i;
	    }
	    public String toString() { return name; }
	    public int compareTo(String s) {
//		String s = o.toString();
		return name.compareTo(s);
	    }
	}
	    
}
