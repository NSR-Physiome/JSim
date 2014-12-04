/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Dispatch a set of parallel jobs to multiple threads/processors

package JSim.util;

import java.util.*;

public class MPDispatch {
	private String name; // name for this dispatcher
	private Job[] jobs; // jobs needing work
	private Monitor monitor; // to track job progress
	private Worker[] workers; // worker threads
	private int nstarted; // # jobs started so far
	private boolean canceled; // run canceled
	private Exception xcept; // exception from any job?
	private HashSet<Job> jobsDone; // jobs completed or skipped
	private HashSet<Job> jobsSkipped; // jobs completed or skipped
	
	// constructor
	public MPDispatch(String n, Job[] j, Monitor m) {
	    name = n;
	    jobs = j;
	    monitor = m;
	    jobsDone = new HashSet<Job>();
	    jobsSkipped = new HashSet<Job>();
	}
	
	// run parallel
	public void run(int nworkers) throws Xcept {
	    if (nworkers > njobs()) nworkers = njobs();

	    // create worker threads
	    workers = new Worker[nworkers];
	    for (int i=0; i<nworkers; i++) {
	        workers[i] = new Worker(i);
		workers[i].start();
	    }
	    
	    // wait till all workers done
	    for (int i=0; i<nworkers; i++) {
	        try {
	 	    workers[i].join();
		} catch (InterruptedException e) { }
	    }

	    // throw Xcept?
	    if (canceled) 
	    	throw new Xcept("Run cancelled by user");	    
	    if (xcept != null) 
	    	throw Xcept.wrap(xcept);
	}

	// cancel all workers
	public void cancel() { 
	   canceled = true; 
	   for (int i=0; i<nworkers(); i++)
	       workers[i].cancel();
	}

	// skip one job
	public void skipOneJob() {
	    Job job = getJobToSkip();
	    if (job == null) return;
	    job.jobSkip();
	}
	    
	// get job to skip, or null if none available
	protected synchronized Job getJobToSkip() {
	    for (int i=0; i<njobs(); i++) {
	    	Job job = jobs[i];
		if (jobsDone.contains(job)) continue;
		if (jobsSkipped.contains(job)) continue;
		jobsSkipped.add(job);
		return job;
	    }
	    return null;
	}
	    
	// get next available job, null if done
	protected synchronized Job getNextJob() {
	    if (canceled) return null;
	    if (xcept != null) return null;
	    if (nstarted >= njobs()) return null;
	    Job job = jobs[nstarted++];
	    if (monitor != null) monitor.jobStarted(job);
	    return job;
	}
	    
	// job completed
	protected synchronized void jobCompleted(Job job) {
	    jobsDone.add(job);	    
	    if (monitor != null) monitor.jobCompleted(job);
	}

	// exception noted in some worker thread
	protected synchronized void setXcept(Exception e) {
	    xcept = e;
	}

	// query
	public String name() { return name; }
	public int njobs() { return jobs.length; }
	public int nworkers() { return workers.length; }
//	public int nstarted() { return nstarted; }
//	public int ndone() { return jobsDone.size(); }
	
	//// MPDispatch.Worker - one worker thread
	public class Worker extends Thread {
	    private int inx; // index of this worker
	    private Job currJob; // current job, if any
	    
	    // constructor
	    public Worker(int i) {
	    	super(name() + " worker #" + i);
		inx = i;
	    }

	    // run jobs till no more available
	    public void run() {
		while(true) {
		    currJob = getNextJob();
		    if (currJob == null) return;
		    try {
		    	currJob.jobRunX(inx);
		    } catch (Exception e) {
		    	setXcept(e);
		    }
		    jobCompleted(currJob);
		}
	    }
	    
	    // cancel this worker
	    public void cancel() {
	    	if (currJob != null) currJob.jobCancel();
	    }
	}
	
	//// MPDispatch.Job - one job to dispatch
	public static interface Job {
	    public String jobName();
	    public void jobRunX(int workerInx) throws Xcept;
	    public void jobCancel();
	    public void jobSkip();
	}

	//// MPDispatch.Monitor - outside tracking of progress
	public static interface Monitor {
	    public void jobStarted(Job job);
	    public void jobCompleted(Job job);
	}

	//// MPDispatch.SimpleMonitor - monitor to stderr
	public static class SimpleMonitor implements Monitor {
	    public void jobStarted(Job job) {
	    	System.err.println("MPDispatch: started " + 
		    job.jobName());
	    }
	    public void jobCompleted(Job job) {
	    	System.err.println("MPDispatch: completed " + 
		    job.jobName());
	    }
	}

}
