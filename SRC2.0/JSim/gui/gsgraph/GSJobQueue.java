/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
 
// FIFO queue for GSJobs
 
package JSim.gui.gsgraph;
 
import java.io.*;
import java.awt.*;
import java.awt.print.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.ArrayList;

import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.util.*;
import gov.noaa.pmel.sgt.dm.*;

import JSim.util.*;
import JSim.gui.graph.*;

public class GSJobQueue {
	private GSJob[] queue;
	private Lock lock;
	private int id;
	private static int nqueues = 0;

	// constructor
	public GSJobQueue() {
	    queue = new GSJob[GSJob.NCODES+1];
	    lock = new Lock("GSJobQueue lock");
	    id = nqueues++;
	}

	// enqueue a job,  return if 1st job
	public void enqueue(GSJob job) {
	    grabLock();

	    // start job if queue empty
	    if (queue[0] == null) {
		queue[0] = job;
		job.start();
		freeLock();
		return;
	    }

	    // queue non-empty, add/replace queued job
	    for (int i=1; i<queue.length; i++) {
		if (queue[i] == null 
		|| queue[i].code() == job.code()) {
		    queue[i] = job;
		    freeLock();
		    return;
		}
	    }

	    // should never happen,  but...
	    System.err.println("GSJobQueue.enqueue() failed");
	    freeLock();
	    return;
	}

	// dequeue current job,  start next job
	public void startNext() {
	    grabLock();
	    GSJob job = queue[0];
	    for (int i=1; i<queue.length; i++) 
		queue[i-1] = queue[i];
	    queue[queue.length-1] = null;

	    // start waiting job
	    if (queue[0] != null) 
		queue[0].start();
	    freeLock();
	}

	// grab the lock
	private void grabLock() {
	    try {
		lock.grab(1000);
	    } catch (Xcept e) {
		System.err.println(e.cleanMessage());
	    }
	}

	// free the lock
	private void freeLock() {
	    try {
		lock.free();
	    } catch (Xcept e) {
		System.err.println(e.cleanMessage());
	    }
	}

	// String rep
	public String toString() {
	    String s = "queue" + id;
	    for (int i=0; i<queue.length; i++) 
		s = s + " " + queue[i];
	    return s;
	}
}

