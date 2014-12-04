/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// definitions for client connection to remote server

package JSim.rclient;

import java.io.*;
import java.rmi.*;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*; 

public interface RCClientIF extends Remote {

	// translate model text
	String translateModelText(int srcType, int destType, 
	String srcText, String options) throws RemoteException;

	// get model tranlator warnings
	String[] getTranslatorWarnings() throws RemoteException;

	// start model build,  return jobID
	RCInfo.JobStatus startBuild(String modelID, 
	ASInfo.BuildInfo binfo) throws RemoteException;

	// start single run, return jobID
	RCInfo.JobStatus startSingleRun(String modelID, RCInfo.RunInput rinfo) 
	throws RemoteException;

	// start loops run, return jobID
	RCInfo.JobStatus startLoopsRun(String modelID, RCInfo.LoopsInput rinfo) 
	throws RemoteException;

	// start sensitivity run, return jobID
	RCInfo.JobStatus startSensRun(String modelID, RCInfo.SensInput rinfo) 
	throws RemoteException;

	// start optimization run, return jobID
	RCInfo.JobStatus startOptimRun(String modelID, RCInfo.OptimInput rinfo) 
	throws RemoteException;

	// start multi-optimization run, return jobID
	RCInfo.JobStatus startMoptRun(String modelID, RCInfo.MoptInput rinfo) 
	throws RemoteException;

	// get latest job status,  null if no jobs run
	RCInfo.JobStatus getJobStatus() throws RemoteException;

	// skip to next loop (if loops job)
	RCInfo.JobStatus nextLoop() throws RemoteException;

	// cancel whatever job is running
	RCInfo.JobStatus cancelJob() throws RemoteException;

	// get job data
	Serializable getJobOutput(String jobID) throws RemoteException;

	// get model text
	String getText(String modelID, int type, String variant) 
	throws RemoteException;

	// get model build warnings
	String[] getBuildAlerts(String modelID) 
	throws RemoteException;

	// get model text warnings
	String[] getTextWarnings(String modelID, int type, String variant) 
	throws RemoteException;

	// get common texts
	String getCommonText(String name) throws RemoteException;

	// get Solver defaults
	NamedVal[] getSolverDefaults() throws RemoteException;

	// set func gen names
	public void setFuncGenNames(String modelID, String[] names)
	throws RemoteException;

	// set assign
	public void setAssign(String modelID, String var, String
	expr) throws RemoteException;

	// parse query expr
	public RCInfo.Query parseQuery(String modelID, String query)
	throws RemoteException;

	// get job output
	DataInfo getData(String modelID, 
	int run, String expr) throws RemoteException;

	// get profile info
	ProfileData getProfile(String modelID) throws RemoteException;

	// get memory message
	String getMemoryMessage() throws RemoteException;

	// ping client,  used to stay alive
	void ping() throws RemoteException;

	// disconnect from RSClient
	void disconnect() throws RemoteException;
}

