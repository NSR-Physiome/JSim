/*NSRCOPYRIGHT
	Copyright (C) 1999-2018 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Curve editor

// Currently only generates CSV format for editing.
package JSim.gui;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import java.awt.*;
import java.awt.event.*;
import java.util.List;
import java.util.ArrayList;
import java.net.*;

import JSim.util.*;
import JSim.project.*;

public class GModifyData extends JFrame implements ActionListener {

	private String curve_text;
	private String curve_name;
	private String lastFind;  // text from last find
	private JTextArea mod_curve;
	private List<GDataUpdateListener> listeners;

	// constructor 
	public GModifyData( String dataCurve, String dataTitle  ) {
		listeners = new ArrayList<GDataUpdateListener>();
   
		if(dataCurve == null) { 
			curve_text = "";
			curve_name = "New 1D Curve";
		}
		else  {	
			curve_text = dataCurve;
			curve_name = dataTitle + "_mod";  // prepend curve name on this.
		}
		common();
	}

	// common constructor code
	private void common() {

		JFrame frame = new JFrame(curve_name);
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
 		
	    // create widgets
		mod_curve = new JTextArea(curve_text);
		mod_curve.setLineWrap(true);

	    // menubar
	    JMenuBar mbar = new JMenuBar();   
	    JMenu menu;

	   	menu = new JMenu("Curve");
		JMenuItem mi_save = new JMenuItem("save");
		KeyStroke stroke = this.setAccel('S');
		mi_save.setAccelerator(stroke);

		mi_save.addActionListener(this);
		menu.add(mi_save);
	    mbar.add(menu);
 		
	    menu = new JMenu("Edit");

		JMenuItem mi_cut = new JMenuItem("cut");
		stroke = this.setAccel('X');
		mi_cut.setAccelerator(stroke);

	 	JMenuItem mi_copy = new JMenuItem("copy");
		stroke = this.setAccel('C');
		mi_copy.setAccelerator(stroke);

		JMenuItem mi_paste = new JMenuItem("paste"); 
		stroke = this.setAccel('V');
		mi_paste.setAccelerator(stroke);  

		JMenuItem mi_goto = new JMenuItem("goto line#"); 
		stroke = this.setAccel('G');
		mi_goto.setAccelerator(stroke);

		JMenuItem mi_find = new JMenuItem("find"); 
		stroke = this.setAccel('F');
		mi_find.setAccelerator(stroke);

		JMenuItem mi_findagain = new JMenuItem("find again"); 
		stroke = this.setAccel('N');
		mi_findagain.setAccelerator(stroke);

		mi_cut.addActionListener(this);
		mi_copy.addActionListener(this);
		mi_paste.addActionListener(this);
		mi_goto.addActionListener(this);
		mi_find.addActionListener(this);
		mi_findagain.addActionListener(this);

		menu.add(mi_cut);
	    menu.add(mi_copy);
	    menu.add(mi_paste);
		menu.addSeparator();
		menu.add(mi_goto);
		menu.add(mi_find);
		menu.add(mi_findagain);
	    mbar.add(menu);
  
		// Look of window (JFrame):	
		frame.setLocation(100,10);
		frame.setVisible(true);
		frame.setJMenuBar(mbar);
		JScrollPane scrollPane = new JScrollPane(mod_curve);
		Border thickBorder = new LineBorder(Color.WHITE, 10);
		scrollPane.setBorder(thickBorder);
		frame.add(scrollPane, BorderLayout.CENTER);
		frame.setSize(400, 500);
		frame.show();
	}

	public void addListener(GDataUpdateListener toAdd) {
		listeners.add(toAdd);
	}

    public void actionPerformed(ActionEvent e) 
    { 
        String s = e.getActionCommand(); 
  
        if (s.equals("cut")) { 
            mod_curve.cut(); 
        } 
        else if (s.equals("copy")) { 
            mod_curve.copy(); 
        } 
        else if (s.equals("paste")) { 
            mod_curve.paste(); 
        } 
	    else if (s.equals("save")) { 
            curve_text=mod_curve.getText(); 
			try {
				for(GDataUpdateListener gdul: listeners) {
					gdul.updatedCSVData(curve_text, curve_name);
				}
			} catch (Xcept err) { 
			//System.err.println(err.getMessage());	
			}
        } 
		else if (s.equals("goto line#")) {
			String msg = "Go to line#";
		    String sline = (String) JOptionPane.showInputDialog(
				null, msg, msg, 
			    JOptionPane.QUESTION_MESSAGE, 
			    this.readIcon("user.gif"),
			    null, null);
		    text().requestFocus();
		    if (Util.isBlank(sline)) return;
			int line = 0;
			try { line = Util.toInt(sline) - 1; }
			catch (Xcept err) {	
				//System.err.println(err.getMessage());
			}
		    if (line<0) line = 0;
		    int max = text().getLineCount();
		    if (line>max) line = max;
		    try {
				int ofs = text().getLineStartOffset(line);
				int ofs1 = text().getLineStartOffset(line+1);
				text().setCaretPosition(ofs);
				if (ofs1 > ofs) {
					text().setSelectionStart(ofs);
					text().setSelectionEnd(ofs1);
				}
		    } catch (Exception err) { }
		}

		else if (s.equals("find")) {
		    String msg = "Find text";
		    lastFind = (String) JOptionPane.showInputDialog(
			    null, msg, msg, 
			    JOptionPane.QUESTION_MESSAGE, 
			     this.readIcon("user.gif"),
			    null, lastFind);
		    text().requestFocus();
		    if (Util.isBlank(lastFind)) return;
		    this.findAgain();
		}
		else if (s.equals("find again")) {
		    if (Util.isBlank(lastFind)) return;
			this.findAgain();
			}
	}

	private KeyStroke setAccel(int keyChar) {
		KeyStroke stroke = KeyStroke.getKeyStroke(keyChar, 
							  Toolkit.getDefaultToolkit().getMenuShortcutKeyMask());
		return stroke;
	}

		private void findAgain() {
		    String alltext = text().getText();
		    int cpos = text().getCaretPosition();
		    int npos = alltext.indexOf(lastFind, cpos);
		    if (npos <= cpos) npos = alltext.indexOf(lastFind);
		    if (npos<0) { 
				String message = "Can't find \"" + 
					lastFind + "\" in current editor window";
				JOptionPane.showMessageDialog(new JFrame(), message, "Find failed!",
										  JOptionPane.ERROR_MESSAGE);
			}
			else {
				text().setCaretPosition(npos); 
				text().setSelectionStart(npos);
				text().setSelectionEnd(npos + lastFind.length());
			}
		}

	// read icon ... Same method as in GLook
	private Icon readIcon(String fileName) {
	    URL url = getClass().getResource(
		"icons/" + fileName);
	    if (url == null) return null;
	    return new ImageIcon(url);
	}

	// refresh
	public void refresh() {
	
	}
	    
	// query
	public JTextArea text() {
	    return mod_curve ;
	}
	
	public String title() { 
	    return " Modify data";
	}
	public boolean editable() { return true; }
}
