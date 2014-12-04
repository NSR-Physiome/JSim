/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// find all phase exit->entry paths

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 

import java.util.*;
import java.io.*;


public class SeqPullFinder extends SeqGraph {
	private Plan plan;
	private LinkedHashSet<SeqNode> entryNodes, exitNodes;
	private LinkedHashSet<SeqEdge> exitEdges;
	private LinkedHashSet<SeqNode> fftNodes;
	private LinkedHashSet<String> ffts;
	private Hashtable<SeqNode, LinkedHashSet<SeqNode>> fftEntries;
	private Hashtable<SeqNode, LinkedHashSet<XEdge>> entryXEdges, entry2Exits;
	private ArrayList<SeqPath> pullPaths;
	private ArrayList<SeqPath> circPaths;
	private Hashtable<SeqNode,SeqPhase> pullMap;
	private Hashtable<SeqNode,String> pullMapTs;

	// constructor
	public SeqPullFinder(SeqGraph graph, Plan plan) throws Xcept {
	    super(graph);
	    this.plan = plan;
//System.err.println("SeqPullFinder: ");
	    removeMainPhaseDeadEnds();
	    buildNofftables();
	    buildXEdges1(); // entry -> exit or ffts
	    Iterator<String> xs = ffts.iterator();
	    while (xs.hasNext()) 
		buildXEdges2(xs.next());
	    if (plan.pullDisjoint()) buildXEdges3();
//System.err.println("  entryXEdges=" + entryXEdges);
//System.err.println("  entry2Exits=" + entry2Exits);
	    buildPullPaths();
	    buildPullMap();
	}

	// remove main phase node dead ends
	private void removeMainPhaseDeadEnds() throws Xcept {
	    int ict = nnodes();
	    boolean working = true;
	    while (working) {
	    	working = false;
		ArrayList<SeqNode> ns = getPhaseNodeArray(mainPhase());
		for (int i=0; i<ns.size(); i++) {
		    SeqNode n = ns.get(i);
		    if (nedgesFrom(n) > 0 && nedgesTo(n) > 0)
		    	continue;
		    remove(n);
		    working = true;
		}
	    }
	}

	// build entryNodes, exitNodes, fftNodes
	private void buildNofftables() throws Xcept {
	    entryNodes = new LinkedHashSet<SeqNode>();
	    exitNodes = new LinkedHashSet<SeqNode>();
	    exitEdges = new LinkedHashSet<SeqEdge>();
	    Iterator<SeqEdge> es = getEdges();
	    while (es.hasNext()) {
	    	SeqEdge e = es.next();
		if (getPhase(e.src()) == getPhase(e.dest())) 
		    continue;
		exitNodes.add(e.src());
		entryNodes.add(e.dest());
		exitEdges.add(e);
/*
SeqPhase p1 = getPhase(e.src());
SeqPhase p2 = getPhase(e.dest());
if (! p1.contains(p2))
System.err.println("Exit edge: " + e + " p1=" + p1 + " p2=" + p2);
*/
	    }
//System.err.println("  exitEdges=" + exitEdges);
//System.err.println("  exitNodes=" + exitNodes);
//System.err.println("  entryNodes=" + entryNodes);
//SeqIO.write(this, System.err);
	}

	// build no-integrator xedges & fftEntries
	private void buildXEdges1() throws Xcept {
	    entryXEdges = new Hashtable<SeqNode, LinkedHashSet<XEdge>>();
	    entry2Exits = new Hashtable<SeqNode, LinkedHashSet<XEdge>>();
	    ffts = new LinkedHashSet<String>();
	    fftNodes = new LinkedHashSet<SeqNode>();
	    fftEntries = new Hashtable<SeqNode, LinkedHashSet<SeqNode>>();
	    Hashtable<SeqNode, LinkedHashSet<SeqNode>> nodeEntries = 
	    	new Hashtable<SeqNode, LinkedHashSet<SeqNode>>();

	    // init nodeEntries with entry nodes
	    Iterator<SeqNode> ens = entryNodes.iterator();
	    while (ens.hasNext()) {
	    	SeqNode en = ens.next();
		LinkedHashSet<SeqNode> entries = new LinkedHashSet<SeqNode>();
		entries.add(en);
		nodeEntries.put(en, entries);
	    }
//System.err.println("  nodeEntries=" + nodeEntries);

	    // propage entryNodes until phase exit or fft encountered
	    LinkedHashSet<SeqNode> dirtyNodes = new LinkedHashSet<SeqNode>(entryNodes);
	    while (dirtyNodes.size() > 0) {
//System.err.println("  dirtyNodes1 =" + dirtyNodes);
	    	Iterator<SeqNode> dns = dirtyNodes.iterator();
		dirtyNodes = new LinkedHashSet<SeqNode>();
		while (dns.hasNext()) {
		    SeqNode dn = dns.next();
		    LinkedHashSet<SeqNode> dnentries = nodeEntries.get(dn);
//System.err.println("  dn=" + dn + " dnentries=" + dnentries);
		    if (dn.fft() != null) {
		    	if (fftNodes.add(dn)) {
			    ffts.add(dn.fft());
			    fftEntries.put(dn, dnentries);
			}
			continue;
		    }
		    SeqPhase dp = getPhase(dn);
		    Iterator<SeqEdge> es = getEdgesFrom(dn);
		    while (es.hasNext()) {
		    	SeqNode n = es.next().dest();
			SeqPhase p = getPhase(n);
			if (p != dp) {
			    addXEdges(entryXEdges, dnentries, n, null, false);
			    continue;
			}
			if (exitNodes.contains(n)) 
			    addXEdges(entry2Exits, dnentries, n, null, false);
			LinkedHashSet<SeqNode> nentries = nodeEntries.get(n);
			if (nentries == null) {
			    nentries = new LinkedHashSet<SeqNode>();
			    nodeEntries.put(n, nentries);
			}
			if (nentries.addAll(dnentries))
			    dirtyNodes.add(n);
		    }
		}
	    }
//System.err.println("  nodeEntries()=" + nodeEntries);
//System.err.println("  fftNodes=" + fftNodes);
	}

	// build integrator xedges
	private void buildXEdges2(String x) throws Xcept {

	    // init nodeEntrys2 with x integrators
	    Hashtable<SeqNode, LinkedHashSet<SeqNode>> nodeEntries = 
	    	new Hashtable<SeqNode, LinkedHashSet<SeqNode>>();
	    LinkedHashSet<SeqNode> dirtyNodes = 
	    	new LinkedHashSet<SeqNode>();
	    Iterator<SeqNode> ens = fftNodes.iterator();
	    while (ens.hasNext()) {
	    	SeqNode en = ens.next();
		if (! en.fft().equals(x)) continue;
		dirtyNodes.add(en);
		LinkedHashSet<SeqNode> entries = fftEntries.get(en);
		entries.add(en);
		nodeEntries.put(en, entries);
	    }

	    // augment nodeEntries until no more dirtyNodes
	    while (dirtyNodes.size() > 0) {
//System.err.println("  dirtyNodes2 =" + dirtyNodes);
	    	Iterator<SeqNode> dns = dirtyNodes.iterator();
		dirtyNodes = new LinkedHashSet<SeqNode>();
		while (dns.hasNext()) {
		    SeqNode dn = dns.next();
		    String dnx = dn.fft();
		    if (dnx != null && ! dnx.equals(x)) continue;
		    SeqPhase dp = getPhase(dn);
		    LinkedHashSet<SeqNode> dnentries = nodeEntries.get(dn);
		    Iterator<SeqEdge> es = getEdgesFrom(dn);
		    while (es.hasNext()) {
		    	SeqNode n = es.next().dest();
			SeqPhase p = getPhase(n);
			if (p != dp) {
			    addXEdges(entryXEdges, dnentries, n, x, false);
			    continue;
			}
			if (exitNodes.contains(n)) 
			    addXEdges(entry2Exits, dnentries, n, x, false);
			LinkedHashSet<SeqNode> nentries = nodeEntries.get(n);
			if (nentries == null) {
			    nentries = new LinkedHashSet<SeqNode>();
			    nodeEntries.put(n, nentries);
			}
			if (nentries.addAll(dnentries))
			    dirtyNodes.add(n);
		    }
		}
	    }
//System.err.println("  nodeEntries(" + x + ")=" + nodeEntries);

	}

	// build XEdges for dead-end entry paths
	private void buildXEdges3() throws Xcept {

	    // accum exit edge dest nodes by src phase
	    Hashtable<SeqPhase, LinkedHashSet<SeqNode>> phaseDests = 
	    	new Hashtable<SeqPhase, LinkedHashSet<SeqNode>>();
	    Iterator<SeqEdge> es = exitEdges.iterator();
	    while (es.hasNext()) {
	    	SeqEdge e = es.next();
		SeqPhase psrc = getPhase(e.src());
		SeqPhase pdest = getPhase(e.dest());
		if (psrc.contains(pdest)) continue;
		LinkedHashSet<SeqNode> pdests = phaseDests.get(psrc);
		if (pdests == null) {
		    pdests = new LinkedHashSet<SeqNode>();
		    phaseDests.put(psrc, pdests);
		}
		pdests.add(e.dest());
	    }
//System.err.println("  phaseDests=" + phaseDests);				    

	    // make XEdges
	    Iterator<SeqNode> n1s = entryNodes.iterator();
	    while (n1s.hasNext()) {
	    	SeqNode n1 = n1s.next();
		SeqPhase p = getPhase(n1);
		if (hasXEdge(p)) continue;
		LinkedHashSet<SeqNode> n2s = phaseDests.get(p);
		if (n2s == null) continue;
		ArrayList<SeqNode> ln1 = new ArrayList<SeqNode>();
		ln1.add(n1);
		Iterator<SeqNode> n2iter = n2s.iterator();
		while (n2iter.hasNext()) {
		    SeqNode n2 = n2iter.next();

		    // test code
//		    if (! sameFFT(n1.fft(), n2.fft()))
//		    	continue;


		    String fft = n1.fft();
		    if (fft == null) fft = n2.fft();
		    addXEdges(entryXEdges, ln1, n2, fft, true);
//System.err.println("  addXEdges3: " + ln1 + " -> " + n2 + " [" + fft + "]");
		}
	    }
	}

	// matching fft?
	private boolean sameFFT(String t1, String t2) {
	    if (t1 == null && t2 == null) return true;
	    if (t1 == null || t2 == null) return false;
	    return t1.equals(t2);
	}

	// is there an XEdge from this phase?
	private boolean hasXEdge(SeqPhase p) throws Xcept {
	    Iterator<SeqNode> ns = getPhaseNodes(p);
	    while (ns.hasNext()) {
	    	SeqNode n = ns.next();
		if (entryXEdges.get(n) != null) return true;
	    }
	    return false;
	}

	// add XEdges to entry node list
	private void addXEdges(Hashtable<SeqNode, LinkedHashSet<XEdge>> map,
	Collection<SeqNode> n1s, SeqNode n2, String x, boolean disjoint) throws Xcept {
//System.err.println("  addXEdges: " + n1s + " -> " + n2 + " [" + x + "]");
	    Iterator<SeqNode> in1s = n1s.iterator();
	    while (in1s.hasNext()) {
	    	SeqNode n1 = in1s.next();
		if (! entryNodes.contains(n1)) continue;
		if (n1 == n2) continue;
		XEdge xedge = new XEdge(n1, n2, x);
		if (disjoint) xedge.isDisjoint = true;
// System.err.println("    xedge=" + xedge);
		LinkedHashSet<XEdge> xedges = map.get(n1);
		if (xedges == null) {
		    xedges = new LinkedHashSet<XEdge>();
		    map.put(n1, xedges);
		}
		xedges.add(xedge);
	    }
	}

	// build exit->entry paths,  accumulate pull paths
	private void buildPullPaths() throws Xcept {
	    pullPaths = new ArrayList<SeqPath>();
	    Iterator<SeqNode> ns = exitNodes.iterator();
	    while (ns.hasNext()) {
	    	SeqNode n = ns.next();
		SeqPhase p = getPhase(n);
		if (p == mainPhase()) continue;
		Iterator<SeqEdge> es = getEdgesFrom(n);
		while (es.hasNext()) {
		    SeqEdge e = es.next();
		    SeqPhase p1 = getPhase(e.dest());
		    if (p == p1) continue;
		    if (p.contains(p1)) continue;
		    SeqPhase pr = reentryPhase(e);

//System.err.println("  tracing outgoing edge " + e + " to reentry phase " + pr);
		    SeqPath path = new SeqPath();
		    path.add(e);
		    path.addPhase(p1);
		    growReentryPath(pr, path);
		}
	    }
	}
	
	// reentry phase for pull path search
	private SeqPhase reentryPhase(SeqEdge e) throws Xcept {
	    SeqPhase p1 = getPhase(e.src());
	    SeqPhase p2 = getPhase(e.dest());
	    SeqPhase pca = p1.commonAncestor(p2);
	    while (p1.parent() != pca)
	    	p1 = p1.parent();
	    return p1;
	}

	// grow reentry paths from given path till reenter phase
	private void growReentryPath(SeqPhase phase, SeqPath path)
	throws Xcept {
	    Collection<XEdge> cxes = entryXEdges.get(path.lastNode());
	    if (cxes == null) return;
	    Iterator<XEdge> xes = cxes.iterator();
	    while (xes.hasNext()) {
	    	XEdge xe = xes.next();
		SeqNode n1 = xe.dest();
		SeqPhase phase1 = getPhase(n1);
//System.err.println("   testing " + xe + " to phase " + phase1);
//		if (phase1 != phase && path.contains(phase1)) continue;
//		if (phase1 != mainPhase() && phase1 != phase && path.contains(phase1)) continue;
		if (! phase1.contains(phase) && path.contains(phase1)) continue;
// last version more general, but takes too long for GR/2474
//System.err.println("   growing " + path + " to phase " + phase1);
		SeqPath path1 = new SeqPath(path, xe);
		if (xe.isDisjoint) path1.setDisjoint();
		path1.addPhase(phase1);
		path1.addFFTs(xe.ffts);
		path1.addPhaseCrosses(xe.phaseCrosses);
		if (phase.contains(phase1)) {
//System.err.println("  reentry " + phase + ": " + path1.ustring());
		    if (path1.hasFFTWithoutPhaseCross()) 
		    	continue;
//System.err.println("  pullable " + phase + ": " + path1.ustring());
		    pullPaths.add(path1);
		    continue;
		}
		growReentryPath(phase, path1);
	    }
	}

	// build pullMap
	private void buildPullMap() throws Xcept {
	    pullMap = new Hashtable<SeqNode, SeqPhase>();
	    pullMapTs = new Hashtable<SeqNode, String>();
	    circPaths = new ArrayList<SeqPath>();
	    if (pullPaths.size() == 0) return;
	    for (int i=0; i<pullPaths.size(); i++) {
	    	SeqPath path = pullPaths.get(i);
		buildPullMap(path);
	    }
//System.err.println("SeqPullFinder:" + pullMap);
//System.err.println("              :" + pullMapTs);
	}


	// collect nodes to be pulled from reentry path
	//    circ xcept if includes exit node
	private void buildPullMap(SeqPath path)
	throws Xcept {
	    SeqNode xnode = path.firstNode();
	    SeqNode enode = path.lastNode();
	    SeqPhase xphase = getPhase(xnode);
	    SeqPhase ephase = getPhase(enode);
	    SeqPhase phase = xphase.commonAncestor(ephase);
	    String t = phase.x();
	    if (t == null) throw new Xcept(
	    	"Reentry path missing fft: " + path);
	    LinkedHashSet<SeqNode> pnodes = 
	    	new LinkedHashSet<SeqNode>();
	    LinkedHashSet<SeqNode> dirtys = 
	    	new LinkedHashSet<SeqNode>();
	    pnodes.add(enode);
	    dirtys.add(enode);
	    
	    // augment pnodes
	    while (dirtys.size() > 0) {
	    	Iterator<SeqNode> dns = dirtys.iterator();
		dirtys = new LinkedHashSet<SeqNode>();
		while (dns.hasNext()) {
		    SeqNode dn = dns.next();
		    Iterator<SeqEdge> es = getEdgesFrom(dn);
		    while (es.hasNext()) {
		    	SeqNode n = es.next().dest();
			if (pnodes.contains(n)) continue;
			if (! phase.contains(getPhase(n)))
			    continue;
			if (n.fft() != null && !n.fft().equals(t))
			    continue;
			if (n == xnode) {
			    SeqPath cpath = extendPath(path, xnode);
//System.err.println("  pull circpath: " + cpath 
//+ " phaseCrosses=" + cpath.phaseCrosses()
//+ " nodeFFTs=" + cpath.nodeFFTs()
//+ " hasFFTWithoutPhaseCross=" + cpath.hasFFTWithoutPhaseCross()
//+ " hasFFTWithoutPhaseCross2=" + cpath.hasFFTWithoutPhaseCross2());
			    if (! cpath.hasFFTWithoutPhaseCross()) {
			        if (cpath.hasDisjointEdge()) 
				    plan.logger.log("Disjoint circpath ignored:" + cpath);
			        else 
				    circPaths.add(cpath);
			    }
			    continue;
			}
			pnodes.add(n);
			dirtys.add(n);
		    }
		}
	    }
	
	    // update pullMap
	    LinkedHashSet<SeqNode> conflicts = new LinkedHashSet<SeqNode>();
	    SeqPhase pullPhase = getPhase(path.nextToLastNode());
// System.err.println("  pull " + path + ": " + pullPhase + "<-" + pnodes);	    
	    Iterator<SeqNode> pnodei = pnodes.iterator();
	    while (pnodei.hasNext()) {
	    	SeqNode n = pnodei.next();

		// update pullMap
		SeqPhase p = pullMap.get(n);
		if (p == null) p = pullPhase;
		if (p != pullPhase) {
//System.err.println("  " + n + " pullPhase conflict: " 
//+ p + " vs " + pullPhase);
		    p = p.commonAncestor(pullPhase);
		}
		pullMap.put(n, p);

		// update pullMapTs
		String nt = pullMapTs.get(n);
		if (nt == null) nt = t;
		if (! nt.equals(t)) {
		    plan.logger.warn("pullMapT conflict: node=" + 
		    	n + " ffts=" + t + "," + nt);
		    conflicts.add(n);
		}
		pullMapTs.put(n, nt);
	    }

	    if (conflicts.size() > 0 && ! plan.pullDisjoint()) 
	    	throw new Xcept("pullMapT conflicts for " + conflicts);
	}

	// extend path to exit
	private SeqPath extendPath(SeqPath path, SeqNode xnode) throws Xcept {
	    SeqNode enode = path.lastNode();
	    if (enode == xnode) return path;
	    XEdge xeadd = null;
	    if (entry2Exits.get(enode) != null) {
	    	Iterator<XEdge> xes = entry2Exits.get(enode).iterator();
		while (xes.hasNext()) {
		    XEdge xe = xes.next();
		    if (xe.dest() != xnode) continue;
		    xeadd = xe;
		    break;
		}
	    }
	    if (xeadd == null) xeadd = new XEdge(enode, xnode, null);
	    SeqPath cpath = new SeqPath(path, xeadd);
	    if (xeadd.isDisjoint) cpath.setDisjoint();
	    cpath.addFFTs(xeadd.ffts);
	    return cpath;
	}
	    	
	// extend path to circ path, without exiting phase  
	private SeqPath extendPath2Circ(SeqPath path) throws Xcept {
	    SeqNode n1 = path.firstNode();
	    SeqNode n2 = path.lastNode();
	    if (n1 == n2) return path;
	    SeqPhase p1 = getPhase(n1);
	    SeqPhase p2 = getPhase(n2);
	    if (p1 != p2) return null;
	    Iterator<SeqEdge> es = getEdgesFrom(n2);
	    while (es.hasNext()) {
	    	SeqEdge e = es.next();
		SeqNode n3 = e.dest();
		if (getPhase(n3) != p1) continue;
	    	SeqPath cpath = new SeqPath(path, e);
		if (n3.fft() != null)
		    cpath.addFFTs(new StringList(n3.fft()));
		cpath = extendPath2Circ(cpath);
		if (cpath != null) return cpath;
	    }
	    return null;
	}

	// query
	protected Hashtable<SeqNode,SeqPhase> getPullMap() {
	    return pullMap;
	}
	protected Hashtable<SeqNode,String> getPullMapTs() {
	    return pullMapTs;
	}
	protected SeqPath getCircPath() {
	    if (circPaths.size() == 0) return null;
	    return circPaths.get(0);
	}


	// phase-cross edge
	public class XEdge extends SeqEdge {
	    private LinkedHashSet<String> phaseCrosses, ffts;
	    private boolean isDisjoint;
	    
	    // constructor
	    public XEdge(SeqNode src, SeqNode dest, String fft) throws Xcept {
	    	super(src, dest);
		ffts = new LinkedHashSet<String>();
		if (fft != null) ffts.add(fft);
		phaseCrosses = domainCrosses(getPhase(src), getPhase(dest()));
	    }

	    // query
	    public String toString() {
		return super.toString() + ffts + "-" + phaseCrosses + 
		    (isOutgoing() ? "_out" : "_in");
	    }
	    public boolean equals(Object o) {
	    	if (! (o instanceof XEdge)) return false;
		XEdge xe = (XEdge) o;
            	if (! src().equals(xe.src())) return false;
            	if (! dest().equals(xe.dest())) return false;
		if (! ffts.equals(xe.ffts)) return false;
		return true;
	    }
	    public int hashCode() {
	    	return super.hashCode() + ffts.hashCode();
	    }
	    public boolean isOutgoing() {
	    	return ! getPhase(src()).contains(getPhase(dest()));
	    }
	}	    

	// domain crosses between 2 phases
	private LinkedHashSet<String> domainCrosses(SeqPhase p1, SeqPhase p2)
	throws Xcept {
	    LinkedHashSet<String> xs = new LinkedHashSet<String>();
	    SeqPhase p0 = p1.commonAncestor(p2);
	    while (p1 != p0) {
	    	xs.add(p1.x());
		p1 = p1.parent();
	    }
	    while (p2 != p0) {
	    	xs.add(p2.x());
		p2 = p2.parent();
	    }
	    return xs;
	}
}
