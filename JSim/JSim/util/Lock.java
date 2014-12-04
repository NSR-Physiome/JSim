/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// thread lock flag

package JSim.util;

public class Lock {
	private String name; // of this lock
	private Thread owner;	// or null
	private int waiting;
	private Thread debug; // interesting thread
	private static String debugName = "query thread";

	// constructor
	public Lock(String n) {
	    name = n;
	    owner = null;
	    waiting = 0;
	    debug = null;
	}

	// debug interesting situations
	private synchronized void debug(String msg) {
	Thread curr = Thread.currentThread();
	    if (debug == null && curr.getName().equals(debugName)) 
		debug = curr;
	    boolean show = 
		owner == null || owner == debug || curr == debug;
	    if (!show) return;
	    System.err.println(curr.getName() + 
		": (lock owner=" + 
		((owner==null) ? "null" : owner.getName()) + 
		") " + msg);
	}
	    
	// grab lock
	synchronized public void grab(long timeout) throws Xcept {
	    waiting++;
	    long begin = System.currentTimeMillis();
//	    debug("attempted grab");
	    while (! setOwner()) {
		try { 
		    wait(timeout); 
		} catch (Exception e) {
		    throw new Xcept(id() + e.toString());
		}
		long elapsed = System.currentTimeMillis() - begin;
		if (elapsed>timeout) throw new Xcept(
		    id() + " timed out after " +
		    (timeout/1000.0) + " seconds");
	    }
//	    debug("successful grab");
	}

	// set owner
	synchronized private boolean setOwner() {
//	    debug("setOwner attempt1");
	    Thread thread = Thread.currentThread();
	    if (owner == null) owner = thread;
//	    debug("setOwner attempt2");
	    if (owner == thread) {
		waiting = 0;
		return true;
	    } else {
//		debug("setOwner = false, still waiting to grab lock" );
		return false;
	    }
	}

	// free lock
	synchronized public void free() throws Xcept {
	    Thread thread = Thread.currentThread();
	    if (owner == thread) {
//		debug("freeing lock OK");
		owner = null;
		try {
	    	    notify();
		} catch (Exception e) {
		    throw new Xcept(id() + e.toString());
		}
	    } else {
//		debug("freeing lock failed (not owner)");
		throw new Xcept(
		    "Lock free attempt by non-owner" + 
		    " (internal error, consult programmer)");
	    }
	}

	// get owner
	private synchronized Thread getOwner() { 
	    return owner;
	}

	// allow other process access,  if waiting
	public void regrab(long timeout) throws Xcept {
//	    debug("regrab");
	    if (getOwner() != Thread.currentThread()) {
		grab(timeout);
	    } else if (waiting>0) {
		free();
		// sleep below seems reqd to allow
		//    waiting process to grab after free
		try { Thread.sleep(0);	} 
		    catch (Exception e) { }
		grab(timeout);
	    }
	}

	// ID message
	private String id() {
	    return name + ": " + Thread.currentThread().getName();
	}
}

 