/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// class to make symbol icons for GUI

package JSim.gui; 

import javax.swing.*;
import java.awt.*;
import JSim.gui.graph.GraphData;

public class GSymbolIcon implements Icon {
        private int width, height, size, shape;
        private Color fg, bg;
	private BasicStroke stroke;
        private int x1, y1;
	private Graphics2D g2;

        // constructor
        public GSymbolIcon(int w, int h, Color f, Color b, int sz, int sh) {
            width = w;
            height = h;
            fg = f;
            bg = b;
            size = sz;
	    shape = sh;
	    stroke = new BasicStroke(1.5f);
	    switch (shape) {
	    case GraphData.SHAPE_ASTERISK:
		size = size-2;
		break;	    
 	    case GraphData.SHAPE_DIAMOND:
		size = size+2;
		break;
	    }	    
        }

	// dimension query
        public int getIconWidth() { return width; }
        public int getIconHeight() { return height; }

        // paint 
        public void paintIcon (Component c, Graphics g, 
                               int x, int y) {
            g.setColor(bg);
            g.fillRect(x, y, width, height); // EB changed from clearRect
            g.setColor(fg);
            x1 = x + ((width / 2) - (size / 2));
            y1 = y + ((height / 2) - (size / 2));
            g2 = (Graphics2D) g;
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                RenderingHints.VALUE_ANTIALIAS_ON);
	    g2.setStroke(stroke);
	    switch (shape) {
	    case GraphData.SHAPE_TRIANGLE: paintTriangle(); break;
	    case GraphData.SHAPE_CIRCLE: paintOval(); break;
	    case GraphData.SHAPE_STAR: paintStar(); break;
	    case GraphData.SHAPE_SQUARE: paintSquare(); break;
	    case GraphData.SHAPE_DIAMOND: paintDiamond(); break;
	    case GraphData.SHAPE_ASTERISK: paintBlackOval(); break;
	    }
        }

	// paint oval
	private void paintOval() {
            g2.drawOval(x1, y1, size, size);
 	}

	// paint star
	private void paintStar() {
            int[] cx = { x1, 
                         x1 + size, 
                         x1 + (size / 5), 
                         x1 + (size / 2), 
                         x1 + size - (size / 5) };
            int[] cy = { y1 + (size / 3), 
                         y1 + (size / 3), 
                         y1 + size, 
                         y1, 
                         y1 + size };
            g2.drawPolygon(cx, cy, 5);
  	}

	// paint square
	private void paintSquare() {
            int[] cx = { x1, 
                         x1 + size, 
                         x1 + size, 
                         x1 };
            int[] cy = { y1, 
                         y1, 
                         y1 + size, 
                         y1 + size };
            g2.drawPolygon(cx, cy, 4);
  	}

	// paint asterisk
	private void paintAsterisk() {
            g2.drawLine(x1, y1 + (size / 2),
                        x1 + size, y1 + (size / 2));
            g2.drawLine(x1 + (size / 2), y1,
                        x1 + (size / 2), y1 + size);
            g2.drawLine(x1 + (size / 8), y1 + (size / 7),
                        x1 + ((size / 8) * 7), y1 + ((size / 8) * 7));
            g2.drawLine(x1 + (size / 8), y1 + ((size / 8) * 7),
                        x1 + ((size / 8) * 7), y1 + (size / 8));
  	}

	// paint triangle
	private void paintTriangle() {
            int[] cx = { x1 + (size / 2), x1 + size, x1 };
            int[] cy = { y1, y1 + size, y1 + size };
            g2.drawPolygon(cx, cy, 3);
  	}

	// paint diamond
	private void paintDiamond() {
            int[] cx = { x1 + (size / 2), 
                         x1 + size, 
                         x1 + (size / 2), 
                         x1 };
            int[] cy = { y1, 
                         y1 + (size / 2), 
                         y1 + size, 
                         y1 + (size / 2) };
            g2.drawPolygon(cx, cy, 4);
    	}

	// paint black oval
	private void paintBlackOval() {
            g2.fillOval(x1, y1, size, size);
  	}

}
