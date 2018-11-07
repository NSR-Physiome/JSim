/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// asynchronous monitor for a batch job

package JSim.gui;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.ArrayList;

import JSim.util.*;
import JSim.aserver.*; import JSim.project.*;
import JSim.gui.model.*;

public class GJobAsyncMonitor implements ActionListener {
	// static stuff
	private static final long POLLFREQ = 1000;
	private static final long POPUPDELAY = 1000;
	private static final long KILLDELAY = 5000;

	// perm state
	private GMain gmain;
	private ArrayList<GJob> queue;  // all GJobs


	// current internal state
	private GJob gjob;
	private PJob pjob;
	private GProject gproj;
	private int nrunsLast; // last run with complete refresh
	private long startTime;	// time of start of job
	private long eventTime; // time of last time event
	private long cancelTime; // time user first requested cancel
	private boolean popupShown; // is popup visible
	private boolean killText; // jcancel text now "Kill..."?
	private boolean posted; // post() processing initiated
	private Timer timer;

	// swing popup components
	private JDialog popup;
	private JPanel jpanel;
	private JLabel jhdr, jtext, jactionMsg;
	private JProgressBar jbar;
	private JButton jcancel;
	private JButton jnextLoop;
	private Action cancel, nextLoop;

	// constructor
	public GJobAsyncMonitor(GMain gm) {
	    super();
	    gmain = gm;
	    queue = new ArrayList<GJob>(4);
	    timer = new javax.swing.Timer((int) POLLFREQ, this);
	    cancel = new AbstractAction("CancelKill") {
		public void actionPerformed(ActionEvent e) {
		    cancel();
		}
	    };
	    nextLoop = new AbstractAction("NextLoop") {
		public void actionPerformed(ActionEvent e) {
		    nextLoop();
		}
	    };
	}

	// enqueue a GJob or reject if appropriate
	public void enqueue(GJob gjob, boolean wait) 
	throws Xcept {
	    if (!wait && queue.size() != 0) 
		throw new Xcept ("Could not start \"" +
		    gjob.pjob().getName() + 
		    "\" because another job was in progress.");
	    queue.add(gjob);
	    if (queue.size() == 1) start(gjob);
	}


	// start a job
	private void start(GJob g) {
	    gjob = g;
	    pjob = gjob.pjob();
	    gproj = gjob.gproject();

	    // reinitialize and start the timer
	    nrunsLast = 0;
	    startTime = System.currentTimeMillis();
	    eventTime = cancelTime = 0;
	    popupShown = killText = posted = false;
	    popup = null;
	    timer.start();

	    // start pjob thread
	    gproj.message("Job \"" + pjob.getName() + 
		"\" in progress...\n");
	    gjob.gpre();
	    pjob.start();
	}

	// process timer event, handle AWT exceptions
	public void actionPerformed(ActionEvent e) {
	    try {
		actionPerformedCaptive(e);
	    } catch (Exception x) {
		gproj.message(x);
	    }
	}

	// timer event 
	private void actionPerformedCaptive(ActionEvent e) 
	throws Exception {

	    // reject piled up events
	    long etime = System.currentTimeMillis();
	    if (etime - eventTime < POLLFREQ/2) return;
	    eventTime = etime;

	    // update cancel button text?
	    if (cancelTime != 0  
	    && eventTime - cancelTime >= KILLDELAY 
	    && !killText) {
		jcancel.setEnabled(true);
		jactionMsg.setText("Still trying to cancel ...");
		jcancel.setText("Kill immediately");
	    	killText = true; 
	    }

	    // check for pjob completion
	    if (! pjob.isAlive()) {
		post();
		return;
	    }

	    // update graphs
	    if (cancelTime == 0) {
	        ASInfo.Status jstat = pjob.getJobStatus();
	    	boolean refreshAll = false;
	    	boolean refreshLive = false;
	    	if (jstat != null) {
	    	    int nrunsCurr = jstat.nrunsDone;
	    	    refreshAll = nrunsCurr > nrunsLast;
	    	    nrunsLast = nrunsCurr;
		    refreshLive = !refreshAll
		    	&& jstat.mode != ASModel.OPTIM;
	    	}
	    	if (refreshAll) {
		    if (pjob instanceof PModelOptimJob) 
		        ((PModelOptimJob) pjob).updateBest();
		    gproj.refresh();
		}
	    	if (refreshLive) gproj.liveUpdate(pjob);
	    }

	    // popup update
	    if (eventTime - startTime < POPUPDELAY) return;
	    if (!popupShown) popupShow();
	    popupUpdate();
	}
	    
	// post-processing
	private void post() {
	    if (posted) return;	// do this only once
	    posted = true;
	    timer.stop();	// cease generating events
	    if (popupShown) popupHide(); // hide popup, if any

	    // PJob post-processing
	    boolean normal; 
	    String msg;  
	    try {
		pjob.join();
		pjob.postRun();
		normal = (pjob.stat() == PJob.NORMAL);
		msg = pjob.termMessage();
        // Do not backup if Applet or user does not want to:
		if( (! gmain.isApplet()) && (gmain.getBkupProj()==true) ) {
		// See ASModel for jobCodes
			if((pjob.jobCode()>=0) && (pjob.jobCode()< 5)) {
				pjob.backup();
			}
		}
	    } catch (Xcept e) {
		normal = false;
	        msg = e.cleanMessage() + ": " + pjob.termMessage();
	    } catch (Exception e) {
		normal = false;
	        msg = e.getMessage();
	    }

	    // messages
	    if (normal) {		
		gproj.message(msg);
	    } else {
		gproj.warning(msg);
		if (pjob instanceof PModelBuildJob)
		    GModelSrc.setCaret((GModelJob) gjob);
	    }	

	    // remove from queue, then do GUI post-processing
	    queue.remove(0);
	    gjob.gpost();

	    // check for additional jobs in queue
	    if (queue.size() > 0)
		start((GJob) queue.get(0));
	}

	// show popup
	private void popupShow() {

	    // create popup components
	    Icon icon = gmain.glook().runningIcon();
	    if (pjob instanceof PModelBuildJob) icon = null;
	    jhdr = new JLabel(pjob.getName() + 
		" in progress...", icon, JLabel.CENTER);
	    jtext = new JLabel(jtextMsg(), JLabel.CENTER);
	    jbar = new JProgressBar();
	    jbar.setVisible(false);
	    jactionMsg = new JLabel(" ", JLabel.CENTER);
	    jcancel = new JButton(cancel);
	    jcancel.setText("Cancel");
	    jnextLoop = new JButton(nextLoop);
	    jnextLoop.setText("Next Loop");
	    jpanel = new JPanel(new GridLayout(5, 1));
	    jpanel.add(jhdr);
	    jpanel.add(jtext);
	    jpanel.add(jbar);
	    jpanel.add(jactionMsg);
	    if (showNextLoop()) {
	    	JPanel bpanel = new JPanel(new GridLayout(1, 2));
		bpanel.add(jnextLoop);
		bpanel.add(jcancel);
		jpanel.add(bpanel);
	    } else {
	    	jpanel.add(jcancel);
	    }
	    jpanel.setBorder(new EtchedBorder());
	    Dimension dim = jpanel.getPreferredSize();
	    dim.width += 5*gproj.glook().fontSize();

	    // create popup
	    Component pcomp = gproj.jcomp();
	    Window pwin = 
		SwingUtilities.getWindowAncestor(pcomp);
	    JFrame pframe = (pwin instanceof JFrame) ?
	        ((JFrame) pwin) : null; 
	    popup = new JDialog(pframe, "Job Monitor") {
		public void processWindowEvent(WindowEvent e) {
		    if (e.getID() != WindowEvent.WINDOW_CLOSING)
			super.processWindowEvent(e);
		}
	    };
	    Point p = GUtil.getLocationOnScreen(pcomp);
	    p.x += pcomp.getWidth()/4 - dim.width/2;
	    p.y += pcomp.getHeight()/2 - dim.height/2;
	    popup.setLocation(p.x, p.y);
	    popup.setContentPane(jpanel);
	    popup.setSize(dim.width, dim.height);
	    popup.setVisible(true);
	    popupShown = true;	    
	}

	// progress text message
	private String jtextMsg() {
	    long secs = (eventTime - startTime)/1000;
	    String msg = pjob.phaseDesc();
	    if (msg == null) msg = "";
	    msg = msg + "  " + secs + " seconds elapsed"; 
	    return msg; 
	}

	// update popup progress
	private void popupUpdate() {
	    jtext.setText(jtextMsg());
	    int pct = (int) (100 * pjob.phaseFrac());
	    if (pjob.phaseFrac() != 0 && pct<100)
		jbar.setVisible(true);
	    jbar.setValue(pct);
	}

	// hide popup
	private void popupHide() {
	    if (popup == null) return;
	    popup.setVisible(false);
	    popup.dispose();
	}

	// cancel action
	private void cancel() {

	    // first time,  try safe cancel
	    if (cancelTime == 0) {
		cancelTime = eventTime;
		gproj.message("Attempting controlled cancel of \"" + 
		    pjob.getName() + "\"");
		jactionMsg.setText("Starting controlled cancel ...");
		jcancel.setEnabled(false);
	    	pjob.cancel();
		return;
	    }

	    // if long enough,  start kill
	    if (eventTime - cancelTime >= KILLDELAY) {
		gproj.message("Killing  \"" + pjob.getName() + "\" now.");
		jactionMsg.setText("Killing job now ...");
		pjob.stop();
	    }
	}

	// show next loop button?
	private boolean showNextLoop() {
	    if (pjob == null) return false;
	    return pjob instanceof PModelLoopsJob;
	}

	// next loop action
	private void nextLoop() {
	    jactionMsg.setText("Skipping to next loop ...");
	    pjob.nextLoop();
	}

	// job queued?
	public boolean jobQueued() { return queue.size() > 0; }	

}

