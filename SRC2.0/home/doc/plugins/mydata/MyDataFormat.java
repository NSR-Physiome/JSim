import java.net.URL;
import JSim.util.*;
import JSim.data.*;

public class MyDataFormat extends DataFormat {

    	// constructor
    	public MyDataFormat() {
            super();
	}
	
	// simple query
	public String shortName() { return "mine"; }
	public String longName() { return "Erik's Data Format"; }
	public String[] suffixes() { 
	    return new String[] { "mydata" };
	}
	public URL url() { return null; }
	public boolean isText() { return true; }
	
	// create writer
	public DataWriter createWriter() { 
	    return new MyDataWriter(this);
	}
}

