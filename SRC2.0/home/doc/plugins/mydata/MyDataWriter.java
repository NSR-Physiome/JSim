import java.io.*;
import JSim.util.*;
import JSim.data.*;


public class MyDataWriter extends DataWriter {

	// constructor
	public MyDataWriter(DataFormat fmt) {
	    super(fmt);
	}
	
	// write the data
	public void writeData(Writer wrt, Data.List dlist)
	throws Xcept {
	    PrintWriter writer = new PrintWriter(wrt);
	    for (int i=0; i<dlist.size(); i++) {
	    	Data data = dlist.data(i);
		writer.print(data.legend() + " = [");
		for (int j=0; j<data.nsamples(); j++) {
		    double d = data.realVal(j);
		    writer.print(" " + pretty(d));
		}
		writer.println(" ]");
	    }
	} 
}
