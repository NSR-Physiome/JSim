// one cached var

package JSim.jcode; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*; 

public class JCacheVar {
	public Var v;
	public int level;

	// constructor
	public JCacheVar(Var v, int level) {
	    this.v = v;
	    this.level = level;
	}

	// hash code
	public int hashCode() { return v.hashCode() + level; }

	// equality for collections
	public boolean equals(Object o) {
	    if (! (o instanceof JCacheVar)) return false;
	    JCacheVar cv = (JCacheVar) o;
	    return v == cv.v && level == cv.level;
	}
}
