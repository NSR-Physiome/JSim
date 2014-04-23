/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// synchronous monitor for a batch job

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.io.*;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.model.*;

public class GJobSyncMonitor extends Thread {

	// state
	private GJob gjob;
	private PJob pjob;
	private GProject gproj;
	private JDialog dialog;
	private JOptionPane optionPane;
	private int msecs;	// # milli-seconds elapsed
	private static final int NONE = 0;
	private static final int CANCEL = 1;
	private static final int KILL = 2;
	private static final int POLLFREQ = 200; // milli-sec
	private static final int KILLGRACE = 3000; // milli-sec
	private JComponent[] jmsg;
	private JLabel jsecs;
	private JProgressBar jprogress;

	// constructor
	public GJobSyncMonitor(GJob g) {
	    super();
	    gjob = g;
	    pjob = gjob.pjob();
	    gproj = gjob.gproject();
	    setName("Job Monitor for " + pjob.getName());
	    msecs = 0;
	    jmsg = new JComponent[3];
	    jmsg[0] = new JLabel(pjob.getName() + 
		" in progress...");
	    jsecs = new JLabel(secMsg());
	    jmsg[1] = jsecs;
	    jprogress = new JProgressBar();
	    jprogress.setVisible(false);
	    jmsg[2] = jprogress;

	    // create message area for monitor
	    String[] opts = new String[] { "Cancel", "Kill" };
	    optionPane = new JOptionPane(
		jmsg,
		JOptionPane.INFORMATION_MESSAGE, 
		JOptionPane.DEFAULT_OPTION, 
		gproj.glook().modelIcon(),
		opts);
	    dialog = optionPane.createDialog(gproj.jcomp(),
		"Job Monitor");
	}

	// query
	public PJob pjob() { return pjob; }
	public JDialog dialog() { return dialog; }
	public String secMsg() {
	    return pjob.phaseDesc() + "  " + 
		(msecs/1000) + " seconds elapsed";
	}

	// user pressed button
	public int userRequest() {
	    Object o = optionPane.getValue();
	    if (! (o instanceof String)) 
		return NONE;
	    String s = (String) o;
	    if (s.equals("Cancel")) return CANCEL;
	    if (s.equals("Kill")) return KILL;
	    return NONE;
	}

	// show dialog
	public void dialogShow() {
	    dialog.setVisible(true);
	}


	// dialog has finished,  any action?
	public void dialogDone() {
	    switch (userRequest()) {
	    case NONE:  
		return;
	    case CANCEL: 
		pjob.cancel(); 
		gproj.message("Waiting for job \"" + 
		    pjob.getName() + "\" termination");
		break;
	    case KILL:
		int ct = KILLGRACE / POLLFREQ;
		int i=0;
		while (!pjob.startKill() && i<ct) {
		    try { Thread.sleep(POLLFREQ); }
		        catch (Exception e) { }
		    i++;
		} 
		while (i<ct && pjob.isAlive()) {
		    try { Thread.sleep(POLLFREQ); }
		        catch (Exception e) { }
		    i++;
		} 
		if (pjob.isAlive()) pjob.stop(); 
		break;
	    }

	    // wait for pjob
	    try {
	        pjob.join();
	    } catch (Exception e) {
		System.err.println(pjob.getName() + 
		    ": termination exception " + e);
	    }
	}

	// run
	public void run() {


	    // redirect compiler error stream
	    //   to prevent ugly console messages
	    if (pjob instanceof PModelBuildJob)
	    	gproj.gmain().nullErr(); // obsolete

	    // run the pjob
	    try {
		// start pjob
		gproj.message("Job \"" + pjob.getName() + 
		    "\" in progress...\n");
		pjob.start();

		// popup dialog,  update each sec
		while (pjob.isAlive()) {
		    jsecs.setText(secMsg());
		    int pct = (int) (100 * pjob.phaseFrac());
		    if (pjob.phaseFrac() != 0 && pct<100)
			jprogress.setVisible(true);
		    jprogress.setValue(pct);
		    pjob.join(POLLFREQ);
		    msecs += POLLFREQ;
		    gproj.liveUpdate(pjob);
		}

		// if normal termination, popdown dialog
	    	if (userRequest() == NONE)
		    dialog.dispose();

		// postRun
		pjob.postRun();
		String msg = pjob.getName() + 
		    ": " + pjob.termMessage() + "\n";
		if (pjob.stat() == PJob.NORMAL) 
		    gproj.message(msg);
		else {
		    gproj.warning(msg);
		    if (pjob instanceof PModelBuildJob)
		    	GModelSrc.setCaret((GModelJob) gjob);
		}

	    } catch (Xcept e) {
		gproj.warning(e.cleanMessage());
	    } catch (Exception e) {
		System.err.println(getName() + " exception " +
		    e.toString());
	    }

	    gproj.gmain().origErr(); // OBSOLETE restore error stream
	}

}

