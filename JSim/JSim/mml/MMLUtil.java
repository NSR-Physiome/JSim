// MML language utilities

package JSim.mml;

import JSim.util.*;
import java.util.HashSet;

public class MMLUtil {

	// reserved words
	private static String[] rwarr = new String[] {
	     "and",  "assign",  "class",  "conversion",  "else",  
	     "event",  "extern",  "false",  "function",  
	     "fundamental",  "if",  "import",  "integral",  
	     "JSim",  "or",  "override",  "PI",  "private",  
	     "procedure",  "public",  "solver",  "source",  
	     "sum",  "template",  "to",  "true",  "unit",  
	     "when"
	};
	public static HashSet<String> reservedWords;

	static {
	    reservedWords = new HashSet<String>();
	    for (int i=0; i<rwarr.length; i++)
	    	reservedWords.add(rwarr[i]);
	}
	
	// is legal MML identifier name? (unit, component, template)
	public static boolean isLegalName(String n) {
	    if (n.length() == 0) return false;
	    if (! Character.isLetter(n.charAt(0))) return false;
	    for (int i=0; i<n.length(); i++) {
	    	char c = n.charAt(i);
		if (Character.isLetterOrDigit(c)) continue;
		if (c == '_') continue;
		if (c == '.') continue;
		return false;
	    }
	    if (n.length() > 1) {
	    	String n2 = n.substring(0,2);
	    	if (n2.equalsIgnoreCase("JS")) return false;
	    }
	    if (n.indexOf("__") >= 0) return false;
	    if (reservedWords.contains(n)) return false;
	    return true;
	}	     
	
	// test harness
	public static void main(String[] args) throws Exception {
	    for (int i=0; i<args.length; i++) {
	        String w = args[i];
		boolean b = isLegalName(w);
		System.out.println(w + ": " + 
		    (b ? "legal" : "illegal"));
	    }
	}
}
