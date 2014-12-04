/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// stored data,  accessible from jsim

package JSim.data;
import JSim.util.*;
import JSim.expr.*;
import java.util.ArrayList;
import java.io.Serializable;

public abstract class Data implements Named, DiagInfo {
	protected String name;	// name, follows JSim name conventions
	protected String desc;	// description of data, free form
	protected Unit unit;	// unit (may be null)
	protected String group; // group (e.g. loop) or null
	public Subset subset;   // if non-null, delimits NaNs

	// constructors
	public Data(String d, Unit u) throws Xcept {
	    this(null, d, u);
	}
	public Data(String n, String d, Unit u) throws Xcept {
	    name = n;
	    desc = d;
	    unit = u;
	}

	// query
	public String name() { return name; }
	public String desc() { return desc; }
	public String group() { return group; }
	public Unit unit() { return unit; }
	public String toString() { return desc; }
	public String diagInfo() { return "Data " + desc; } 
	abstract public int ndim();
	public String legend() {
	    if (Util.isBlank(name)) 
		return (desc == null) ? "" : desc;
	    if (Util.isBlank(desc))
		return (name == null) ? "" : name;
	    if (name.equals(desc))
		return name;
	    return name + " \"" + desc + "\"";
	}
	public String glegend() {
	    if (group == null) return legend();
	    return legend() + "{" + group + "}";
	}
	public DataInfo info() throws Xcept { return info(null); } // serializable info
	abstract public DataInfo info(Subset sset) throws Xcept;
	abstract public double min();	// minimum value
	abstract public double minpos(); // minumum pos value
	abstract public double max(); // max value
	abstract public int nsamples();
	abstract public double[] samples() throws Xcept;
	abstract public double realVal(int i) throws Xcept;
	abstract public double realVal(double[] vals) throws Xcept;
	abstract public GridData grid(int i) throws Xcept;
	abstract public String legendGrids();

	// modify data
	public void setName(String n) { name = n; } // use carefully
	public void setDesc(String d) { desc = d; }
	public void setGroup(String g) { group = g; }
	abstract public void calcExtrema();

	// are domain array values out of range, within fudge factor?
  	public boolean outOfRange(double[] gvals) throws Xcept {
	    int n = ndim();
	    if (gvals == null || gvals.length != ndim())
	        throw new Xcept(this, "outOfRange dimension mismatch");
	    for (int i=0; i<n; i++) 
	    	if (grid(i).outOfRange(gvals[i])) return true;
	    return false;
	}

	// calc index for grid pos
	public int inx(int[] pos) throws Xcept {
	    int[] ct = new int[ndim()];
	    for (int i=0; i<ct.length; i++) 
		ct[i] = grid(i).ct();
	    int inx = inx(pos, ct);
	    if (inx < 0) throw new Xcept(this,  
		"Grid positioning error");
	    return inx;
	}

	// calc index for grid pos, ct[]
	public static int inx(int[] pos, int[] ct) throws Xcept {
	    int inx = 0;
	    for (int i=ct.length-1; i>=0; i--) {
		if (pos[i]<0 || pos[i]>=ct[i]) 
		    return -1;
		if (i<ct.length-1) inx *= ct[i];
		inx += pos[i];
	    }
	    return inx;
	}

	// calc grid pos for index
	public int[] gridPos(int inx) throws Xcept {
	    int[] gpos = new int[ndim()];
	    for (int i=0; i<ndim(); i++) {
		GridData grid = grid(i);
		gpos[i] = inx % grid.ct();
		inx = inx / grid.ct();
	    }
	    return gpos;
	}

	// grid values for index
	public double[] gridVals(int inx) throws Xcept {
	    double[] gvals = new double[ndim()];
	    int[] gpos = gridPos(inx);
	    for (int i=0; i<ndim(); i++) {
		GridData grid = grid(i);
		gvals[i] = grid.realVal(gpos[i]);
	    }
	    return gvals;
	}

	// nearest inx for grid values
	public int nearestInx(double[] gvals) throws Xcept {
	    int[] gpos = new int[ndim()];
	    for (int i=0; i<ndim(); i++)
	    	gpos[i] = grid(i).nearestInx(gvals[i]);
	    return inx(gpos);
	}

	// reconstitute Data from DataInfo
	public static Data makeData(DataInfo info) throws Xcept {
	    Data data = null;
	    Unit u = (info.unit == null) ? 
		Unit.scalar() : info.unit.unit();

	    // decode various types
	    if (info instanceof RegularGridData.Info) {
		RegularGridData.Info ginfo = (RegularGridData.Info) info;
		data = new RegularGridData(info.desc, u,
		    ginfo.min, ginfo.max, ginfo.ct);

	    } else if (info instanceof IrregularGridData.Info) {
		IrregularGridData.Info iinfo = (IrregularGridData.Info) info;
		data = new IrregularGridData(info.desc, u, 
		    iinfo.samples);

	    } else if (info instanceof RealNData.Info) {
		RealNData.Info rinfo = (RealNData.Info) info;
		GridData[] grids = null;
		if (rinfo.grids != null) {
		    int n = rinfo.grids.length;
		    grids = new GridData[n];
		    for (int i=0; i<n; i++)
			grids[i] = (GridData) makeData(rinfo.grids[i]);
		}
		RealNData ndata = new RealNData(info.desc, u, grids);
		ndata.set(info.subset, rinfo.samples);
		data = ndata;

	    } else throw new Xcept(
		"Data.makeData(): Illegal DataInfo subclass");

	    data.name = info.name;
	    data.setGroup(info.group);
	    data.subset = info.subset;
	    return data;
	}

	// independent identical copy of data
	public Data copy() throws Xcept { 
	    return makeData(info()); 
	}

	// all samples NaN?
	public boolean allNaN() throws Xcept {
	    for (int i=0; i<nsamples(); i++) 
		if (! Double.isNaN(realVal(i))) return false;
	    return true;
	}

	// has same grids in same order
	public boolean hasSameGridOrder(Data data) throws Xcept {
	    if (ndim() != data.ndim()) return false;
	    for (int i=0; i<ndim(); i++) 
		if (! grid(i).sameSamplesAs(data.grid(i)))
		    return false;
	    return true;
	}

/* MAYBE USEFUL LATER, BUT BUGGY
	// same grids as another Data
	public boolean hasSameGrids(Data data) throws Xcept {
	    if (ndim() != data.ndim()) return false;
	    for (int i=0; i<ndim(); i++) 
		if (! data.hasSameGrid(grid(i)))
		    return false;
	    return true;
	}
*/
	// has same Grid
	public boolean hasSameGrid(GridData g) throws Xcept {
	    boolean b = false;
	    for (int i=0; i<ndim(); i++) 
		if (grid(i).sameSamplesAs(g)) b = true;
	    return b;
	}

	// Data.Subset
	public static class Subset implements Serializable {
	    public int gridInx; // index of grid()
	    public int lox; // low non-NaN data index inclusive
	    public int hix; // high non-NaN data index exclusive
	    // valid data is x>=lox and x<hix

	    // constructor
	    public Subset() {
	    	gridInx = -1;  // never valid
	    	lox = 0;
	    	hix = 0; // no data
	    }
	    public Subset(Subset sset) {
		this();
		if (sset != null) {
		    gridInx = sset.gridInx;
		    lox = sset.lox;
		    hix = sset.hix;
		}
	    }

	    // query
	    public String toString() {
		return "inx=" + gridInx + 
		    " lox=" + lox + " hix=" + hix;
	    }
	}

	// Data.List
	public static class List extends ArrayList<Data> {
	    public List() { super(); }
	    public List(int n) { super(n); }
	    public List(Data[] data) {
	    	super(data.length);
		for (int i=0; i<data.length; i++)
		    add(data[i]);
	    }
	    public List(Data[][] data) {
	    	super(data.length*data[0].length);
		for (int i=0; i<data.length; i++) 
		    for (int j=0; j<data[i].length; j++) 
		   	add(data[i][j]);
	    }
	    public List(DataInfo[] info) throws Xcept { 
		super(info.length); 
		for (int i=0; i<info.length; i++) 
		    add(makeData(info[i]));		
	    }
	    public Data data(int i) { return (Data) get(i); }
	    public Data data(String n) {
		for (int i=0; i<size(); i++) {
		    Data data = data(i);
		    String dn = data.name();
		    if (Util.isBlank(dn)) continue;
		    if (n.equals(dn)) return data;
		}
		return null;
	    }
	    public boolean allNaN() throws Xcept {
	    	for (int i=0; i<size(); i++) 
		    if (! data(i).allNaN()) return false;
		return true;
	    }
	    public Data.List copy() throws Xcept {
		Data.List list = new List(size());
		for (int i=0; i<size(); i++) 
		    list.add(data(i).copy());
		return list;
	    }
	    public DataInfo[] info() throws Xcept {
		DataInfo[] info = new DataInfo[size()];
		for (int i=0; i<info.length; i++) 
		    info[i] = data(i).info();
		return info;
	    }
	    public Data dataForName(String name) throws Xcept {
	    	Data data = null;
		for (int i=0; i<size(); i++) {
		    Data d = data(i);
		    if (d.name == null) continue;
		    if (! d.name.equals(name)) continue;
		    if (data != null) throw new Xcept(
		    	"Multiple data match name \"" + 
			name + "\"");
		    data = d;
		}
		return data;
	    }
	    public Data dataForDesc(String desc) throws Xcept {
	    	Data data = null;
		for (int i=0; i<size(); i++) {
		    Data d = data(i);
		    if (d.desc == null) continue;
		    if (! d.desc.equals(desc)) continue;
		    if (data != null) throw new Xcept(
		    	"Multiple data match description \"" + 
			desc + "\"");
		    data = d;
		}
		return data;
	    }
	}

	// Data.NList
	public static class NList extends NamedList {
	    public NList (int i) { super(i); }
	    public Data data(int i) { return (Data) get(i); } 
	    public Data data(String n) { return (Data) getByName(n); } 
	    public Data.List list() {
		Data.List list = new Data.List(size());
		for (int i=0; i<size(); i++)
		    list.add(data(i));
		return list;
	    }
	    public Data dataForDesc(String desc) throws Xcept {
	    	Data data = null;
		for (int i=0; i<size(); i++) {
		    Data d = data(i);
		    if (d.desc == null) continue;
		    if (! d.desc.equals(desc)) continue;
		    if (data != null) throw new Xcept(
		    	"Multiple data match description \"" + 
			desc + "\"");
		    data = d;
		}
		return data;
	    }
	}
}
