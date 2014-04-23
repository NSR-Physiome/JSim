/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test thread lock flag

package JSim.tests; import JSim.util.*;

public class TestLock {
	public static final void main(String[] args) 
	throws Exception {

	    // parse command args
	    if (args.length != 4) throw new Exception(
		"Usage: TestLock comp-time q1-time q2-time timeout");
	    long compTime = Util.toInt(args[0]);
	    long q1Time = Util.toInt(args[1]);
	    long q2Time = Util.toInt(args[2]);
	    long timeout = Util.toInt(args[3]);

	    // create lock, threads
	    Lock lock = new Lock("TestLock");
	    CompThread cthread = new CompThread(lock, compTime, timeout);
	    cthread.start();
	    QueryThread qthread = new QueryThread(lock, q1Time, q2Time, timeout);
	    qthread.start();
	}	    

	// comp thread
	public static class CompThread extends Thread {
	    Lock lock;
	    long compTime, timeout;

	    // constructor
	    public CompThread(Lock l, long c, long t) throws Exception {
		super("Comp thread");
		lock = l;
		compTime = c;
		timeout = t;
	    }

	    // initialize
	    public void init() {
		try {
		    lock.grab(timeout); // beginning of run
		} catch (Exception e) {
		    System.out.println(getName() + e);
		}
	    }

	    // run computation
	    public void run() {
		try {
		    while(true) {
			long t = (long) (compTime * Math.random());
//			System.out.println(getName() + " msec=" + t);
			Thread.sleep(t);
			lock.regrab(timeout); // cancelCheck()
		    }
		} catch (Exception e) {
		    System.out.println(getName() + e);
		}
	    }
	} 

	// query thread
	public static class QueryThread extends Thread {
	    Lock lock;
	    long q1Time;
	    long q2Time;
	    long timeout;

	    // constructor
	    public QueryThread(Lock l, long q1, long q2, long t) 
	    throws Exception {
		super("Query thread");
		lock = l;
		q1Time = q1;
		q2Time = q2;
		timeout = t;
	    }

	    // run computation
	    public void run() {
		try {
		    while(true) {
			long t = (long) (q1Time * Math.random());
			System.out.println(getName() + " pausing msec=" + t);
			Thread.sleep(t); // sleep between queries
			lock.grab(timeout);
			t = (long) (q2Time * Math.random());
			System.out.println(getName() + " working msec=" + t);
			Thread.sleep(t); // working time for query
			lock.free();
		    }
		} catch (Exception e) {
		    System.out.println(getName() + e);
		}
	    }
	} 
}

 
