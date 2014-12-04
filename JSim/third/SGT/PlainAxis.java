/*
 * $Id: PlainAxis.java,v 1.2 2014/01/15 17:27:43 butterw Exp $
 *
 * This software is provided by NOAA for full, free and open release.  It is
 * understood by the recipient/user that NOAA assumes no liability for any
 * errors contained in the code.  Although this software is released without
 * conditions or restrictions in its use, it is expected that appropriate
 * credit be given to its author and to the National Oceanic and Atmospheric
 * Administration should the software be included by the recipient as an
 * element in other product development.
 */

package  gov.noaa.pmel.sgt;

import gov.noaa.pmel.util.Point2D;

import java.util.Vector;
import java.awt.Graphics;
import java.awt.Color;
import java.awt.Rectangle;
import java.net.URL;
import java.net.MalformedURLException;
// jdk1.2
//import java.awt.geom.Point2D;

/**
 * Axis class for creation of standard "plain" linear axes. An
 * {@link SpaceAxis example} is available demonstrating
 * <code>PlainAxis</code> use.
 *
 * @author Donald Denbo
 * @version $Revision: 1.2 $, $Date: 2014/01/15 17:27:43 $
 * @since 1.0
 */
public class PlainAxis extends SpaceAxis implements Cloneable {
  static final double TIC_GAP = 0.05;
  static final double TIC_RATIO = 1.3;
  static final double LABEL_RATIO = 1.1;
  ////
  String[] ticlbls;      // strings for tic mark labels
  double[] ticlocs;      // doubles for tic locations (user space)
  String zmat = "\\A\\b0\\b\\Z";      // zero alone ("^0$") regex
  String dmat = "\\d[.]\\d";          // digit-dot-digit regex
  protected boolean nsrLogScale_; // use log scale to position title
  ////
  /**
   * Default constructor for PlainAxis.
   **/
  public PlainAxis() {
    this("");
  }
  /**
   * Constructor for Axis. Sets the axis identifier and initializes
   * the defaults.
   *
   * @param id axis identification
   **/
  public PlainAxis(String id) {
    super(id);
    nsrLogScale_ = false;
  }
  public Axis copy() {
    PlainAxis newAxis;
    try {
      newAxis = (PlainAxis)clone();
    } catch (CloneNotSupportedException e) {
      newAxis = new PlainAxis();
    }
    //
    // remove registered axes and transforms
    //
    newAxis.registeredAxes_ = new Vector(2,2);
    newAxis.registeredTransforms_ = new Vector(2,2);
    //
    return newAxis;
  }
  ////
  public void setTicArray(String[] ticlbls, double[] ticlocs) {
    this.ticlbls = ticlbls;
    this.ticlocs = ticlocs;
  }
  ////
  //
  void draw(Graphics g) {
    int xloc, yloc, xend, yend;
    int istop, i;
    double xt, yt, dir, x, y, xp, yp;
    double xtitle, ytitle;
    double delta = uRange_.delta;
    Format format;
    String labelText;
    SGLabel title = getTitle();
    if(!visible_) return;
    if(Double.isNaN(delta)) delta = (uRange_.end - uRange_.start)/10.0;
    if(title != null) title.setLayer(graph_.getLayer());
    //
    if(lineColor_ == null) {
      g.setColor(graph_.getLayer().getPane().getComponent().getForeground());
    } else {
      g.setColor(lineColor_);
    }
    //
    if(labelFormat_.length() <= 0) {
      format = new Format(Format.computeFormat(uRange_.start, uRange_.end, sigDigits_));
    } else {
      format = new Format(labelFormat_);
    }
    if(orientation_ == Axis.HORIZONTAL) {
      if(uLocation_ == null) {
        yloc = graph_.getYUtoD(tLocation_.t);
        yp = graph_.getYUtoP(tLocation_.t);
      } else {
        yloc = graph_.getYUtoD(uLocation_.y);
        yp = graph_.getYUtoP(uLocation_.y);
      }
      xloc = graph_.getXUtoD(uRange_.start);
      xend = graph_.getXUtoD(uRange_.end);
      g.drawLine(xloc, yloc, xend, yloc);
      //
      dir = delta > 0? 1.0: -1.0;
      xt = (int)((uRange_.start/delta + (dir*uRange_.start > 0? 1.0: -1.0)*0.00001))*delta;
      if(dir*xt < dir*uRange_.start) xt += delta;
      istop = (int)((uRange_.end - xt)/delta + 0.00001);
      x = xt;
      xp = graph_.getXUtoP(x);
      //// 
      //// start of new tic mark drawing routine (x axis)
      if (ticlocs != null) {

        for(int j=0; j < ticlocs.length; j++) {
            xp = graph_.getXUtoP(ticlocs[j]);
            drawXTic(g, xp, yp, largeTicHeight_);
        }

      } else {
      //// end of new tic mark drawing routine (x axis)
      //// start of orig x tic mark drawing routine
      drawSmallXTics(g, x, uRange_.start, -delta, yp);
      drawXTic(g, xp, yp, largeTicHeight_);
      for(i=0; i < istop; i++) {
        drawSmallXTics(g, x, uRange_.end, delta, yp);
        x += delta;
        xp = graph_.getXUtoP(x);
        drawXTic(g, xp, yp, largeTicHeight_);
      }
      drawSmallXTics(g, x, uRange_.end, delta, yp);
      //// end of orig x tic mark drawing routine
      }
      ////
      //
      if(labelInterval_ <= 0 || labelPosition_ == NO_LABEL) return;
      //
      SGLabel label;
      int vertalign;
      if(labelPosition_ == POSITIVE_SIDE) {
        vertalign = SGLabel.BOTTOM;
        if(ticPosition_ == BOTH_SIDES || ticPosition_ == POSITIVE_SIDE) {
          yt = yp + TIC_RATIO*largeTicHeight_;
        } else {
          yt = yp + TIC_GAP;
        }
        ytitle = yt + LABEL_RATIO*labelHeight_;
      } else {
        vertalign = SGLabel.TOP;
        if(ticPosition_ == BOTH_SIDES || ticPosition_ == NEGATIVE_SIDE) {
          yt = yp - TIC_RATIO*largeTicHeight_;
        } else {
          yt = yp - TIC_GAP;
        }
        ytitle = yt - LABEL_RATIO*labelHeight_;
      }
      if(dir*uRange_.start <= 0 && dir*uRange_.end >= 0) {
        x = ((int)(uRange_.start/(delta*labelInterval_) - 0.00001))*delta*labelInterval_;
      } else {
        x = xt;
      }
      //// start of new tic label drawing routine (x axis)
      if (ticlbls != null && ticlocs != null) {

        for(int j=0; j < ticlbls.length; j++) {
            xt = graph_.getXUtoP(ticlocs[j]);
            String tls = ticlbls[j].trim();     // trim ticlbl

            // move labels left one or two pixels. 
            // adjusts for tic mark width?
            xt = (tls.matches(zmat) || tls.matches(dmat)) ? xt - 0.01 :
                                                            xt - 0.02;
            label = new SGLabel("coordinate", tls, 
                                new Point2D.Double(xt, yt));
            label.setAlign(vertalign, SGLabel.MIDDLE);
            label.setOrientation(SGLabel.HORIZONTAL);
            label.setFont(labelFont_);
            label.setColor(labelColor_);
            label.setHeightP(labelHeight_);
            label.setLayer(graph_.getLayer());
            try {
                label.draw(g);
            } catch (LayerNotFoundException e) { }
        }

      } else {
      //// end of new tic label drawing routine (x axis)
      //// start of orig tic label drawing routine (x axis)
      istop = (int)((uRange_.end - x)/(delta*labelInterval_) + 0.00001);
      for(i=0; i <= istop; i++) {
        xt = graph_.getXUtoP(x);
        labelText = format.form(x);
        label = new SGLabel("coordinate", labelText, new Point2D.Double(xt, yt));
        label.setAlign(vertalign, SGLabel.MIDDLE);
        label.setOrientation(SGLabel.HORIZONTAL);
        label.setFont(labelFont_);
        label.setColor(labelColor_);
        label.setHeightP(labelHeight_);
        label.setLayer(graph_.getLayer());
        try {
          label.draw(g);
        } catch (LayerNotFoundException e) {}
        x = x + delta*labelInterval_;
      }
      //// end of orig tic label drawing routine (x axis)
      }
      ////
      if(title_ != null) {
        xtitle = nsrLogScale_ ? 
	    Math.sqrt(uRange_.end * uRange_.start) :
	    (uRange_.end + uRange_.start)*0.5;
        yt = ytitle;
        xt = graph_.getXUtoP(xtitle);
        title.setLocationP(new Point2D.Double(xt, yt));
        title.setAlign(vertalign, SGLabel.CENTER);
        title.setOrientation(SGLabel.HORIZONTAL);
        try {
          title.draw(g);
        } catch (LayerNotFoundException e) {}
      }
    } else {                               // orientation is vertical
      if(uLocation_ == null) {
        xloc = graph_.getXUtoD(tLocation_.t);
        xp = graph_.getXUtoP(tLocation_.t);
      } else {
        xloc = graph_.getXUtoD(uLocation_.x);
        xp = graph_.getXUtoP(uLocation_.x);
      }
      yloc = graph_.getYUtoD(uRange_.start);
      yend = graph_.getYUtoD(uRange_.end);
      g.drawLine(xloc, yloc, xloc, yend);
      //
      dir = delta > 0? 1.0: -1.0;
      yt = (int)((uRange_.start/delta) + (dir*uRange_.start > 0? 1.0: -1.0)*0.00001)*delta;
      if(dir*yt < dir*uRange_.start) yt += delta;
      istop = (int)((uRange_.end - yt)/delta + 0.00001);
      y = yt;
      yp = graph_.getYUtoP(y);
      //// 
      //// start of new tic mark drawing routine (y axis)
      if (ticlocs != null) {

        for(int j=0; j < ticlocs.length; j++) {
            yp = graph_.getYUtoP(ticlocs[j]);
            drawYTic(g, xp, yp, largeTicHeight_);
        }

      } else {
      //// end of new tic mark drawing routine (y axis)
      //// start of orig y tic mark drawing routine
      drawSmallYTics(g, xp, y, uRange_.start, -delta);
      drawYTic(g, xp, yp, largeTicHeight_);
      for(i=0; i < istop; i++) {
        drawSmallYTics(g, xp, y, uRange_.end, delta);
        y += delta;
        yp = graph_.getYUtoP(y);
        drawYTic(g, xp, yp, largeTicHeight_);
      }
      drawSmallYTics(g, xp, y, uRange_.end, delta);
      //// end of orig y tic mark drawing routine
      }
      ////
      //
      if(labelInterval_ <= 0 || labelPosition_ == NO_LABEL) return;
      //
      SGLabel label;
      int vertalign;
      if(labelPosition_ == NEGATIVE_SIDE) {
        vertalign = SGLabel.BOTTOM;
        if(ticPosition_ == BOTH_SIDES || ticPosition_ == NEGATIVE_SIDE) {
          xt = xp - TIC_RATIO*largeTicHeight_;
        } else {
          xt = xp - TIC_GAP;
        }
        xtitle = xt - LABEL_RATIO*labelHeight_;
      } else {
        vertalign = SGLabel.TOP;
        if(ticPosition_ == BOTH_SIDES || ticPosition_ == POSITIVE_SIDE) {
          xt = xp + TIC_RATIO*largeTicHeight_;
        } else {
          xt = xp + TIC_GAP;
        }
        xtitle = xt + LABEL_RATIO*labelHeight_;
      }
      if(dir*uRange_.start <= 0 && dir*uRange_.end >= 0) {
        y = ((int)(uRange_.start/(delta*labelInterval_) - 0.00001))*delta*labelInterval_;
      } else {
        y = yt;
      }
      //// start of new tic label drawing routine (y axis)
      if (ticlbls != null && ticlocs != null) {

        for(int j=0; j < ticlbls.length; j++) {
            yt = graph_.getYUtoP(ticlocs[j]);
            String tls = ticlbls[j].trim();     // trim ticlbl

            // move labels down one or two pixels. 
            // adjusts for tic mark width?
            yt = (tls.matches(zmat) || tls.matches(dmat)) ? yt - 0.01 :
                                                            yt - 0.02;

            // also adjust all y labels left two pixels
            label = new SGLabel("coordinate", tls, 
                                new Point2D.Double(xt - 0.02, yt));
            label.setAlign(vertalign, SGLabel.CENTER);
            label.setOrientation(SGLabel.VERTICAL);
            label.setFont(labelFont_);
            label.setColor(labelColor_);
            label.setHeightP(labelHeight_);
            label.setLayer(graph_.getLayer());

            try {
                label.draw(g);
            } catch (LayerNotFoundException e) { }

        }

      } else {
      //// end of new tic label drawing routine (y axis)
      //// start of orig tic label drawing routine (y axis)
      istop = (int)((uRange_.end - y)/(delta*labelInterval_) + 0.00001);
      for(i=0; i <= istop; i++) {
        yt = graph_.getYUtoP(y);
        labelText = format.form(y);
        label = new SGLabel("coordinate", labelText, new Point2D.Double(xt, yt));
        label.setAlign(vertalign, SGLabel.CENTER);
        label.setOrientation(SGLabel.VERTICAL);
        label.setFont(labelFont_);
        label.setColor(labelColor_);
        label.setHeightP(labelHeight_);
        label.setLayer(graph_.getLayer());
        try {
          label.draw(g);
        } catch (LayerNotFoundException e) {}
        y = y + delta*labelInterval_;
      }
      //// end of orig tic label drawing routine (y axis)
      }
      ////
      if(title_ != null) {
        ytitle = nsrLogScale_ ? 
	   Math.sqrt(uRange_.end * uRange_.start) :
	   (uRange_.end + uRange_.start)*0.5;
        yt = graph_.getYUtoP(ytitle);
        xt = xtitle;
        ////title.setLocationP(new Point2D.Double(xt, yt));
        title.setLocationP(new Point2D.Double(xt - 0.02, yt));
        title.setAlign(vertalign, SGLabel.CENTER);
        title.setOrientation(SGLabel.VERTICAL);
        try {
          title.draw(g);
        } catch (LayerNotFoundException e) {}
      }
    }
  }
  /**
   * Get the bounding box for the axis in device units.
   *
   * @return bounding box
   * @see Rectangle
   **/
  public Rectangle getBounds() {
    double xp, yp, ymin, ymax, xmin, xmax;
    int xd, yd, width, height, x, y;
    if(orientation_ == Axis.HORIZONTAL) {
      xd = graph_.getXUtoD(uRange_.start);
      if(uLocation_ == null) {
        yp = graph_.getYUtoP(tLocation_.t);
      } else {
        yp = graph_.getYUtoP(uLocation_.y);
      }
      width = graph_.getXUtoD(uRange_.end) - xd;
      x = xd;
      ymin = yp;
      ymax = yp;
      if(ticPosition_ == BOTH_SIDES || ticPosition_ == POSITIVE_SIDE) {
        ymax = ymax + largeTicHeight_;
      }
      if(ticPosition_ == BOTH_SIDES || ticPosition_ == NEGATIVE_SIDE) {
        ymin = ymin - largeTicHeight_;
      }
      if(labelPosition_ == POSITIVE_SIDE) {
        if(ticPosition_ == BOTH_SIDES || ticPosition_ == POSITIVE_SIDE) {
          ymax = ymax + (1.0 -TIC_RATIO)*largeTicHeight_ + labelHeight_;
        } else {
          ymax = ymax + TIC_GAP + labelHeight_;
        }
      } else if(labelPosition_ == NEGATIVE_SIDE) {
        if(ticPosition_ == BOTH_SIDES || ticPosition_ == NEGATIVE_SIDE) {
          ymin = ymin - (1.0 - TIC_RATIO)*largeTicHeight_ - labelHeight_;
        } else {
          ymin = ymin - TIC_GAP - labelHeight_;
        }
      }
      y = graph_.getLayer().getYPtoD(ymax);
      height = graph_.getLayer().getYPtoD(ymin) - y;
    } else {
      yd = graph_.getYUtoD(uRange_.start);
      if(uLocation_ == null) {
        xp = graph_.getXUtoP(tLocation_.t);
      } else {
        xp = graph_.getXUtoP(uLocation_.x);
      }
      y = graph_.getYUtoD(uRange_.end);
      height = yd - y;
      xmin = xp;
      xmax = xp;
      if(ticPosition_ == BOTH_SIDES || ticPosition_ == POSITIVE_SIDE) {
        xmax = xmax + largeTicHeight_;
      }
      if(ticPosition_ == BOTH_SIDES || ticPosition_ == NEGATIVE_SIDE) {
        xmin = xmin - largeTicHeight_;
      }
      if(labelPosition_ == POSITIVE_SIDE) {
        if(ticPosition_ == BOTH_SIDES || ticPosition_ == POSITIVE_SIDE) {
          xmax = xmax + (1.0 -TIC_RATIO)*largeTicHeight_ + labelHeight_;
        } else {
          xmax = xmax + TIC_GAP + labelHeight_;
        }
      } else if(labelPosition_ == NEGATIVE_SIDE) {
        if(ticPosition_ == BOTH_SIDES || ticPosition_ == NEGATIVE_SIDE) {
          xmin = xmin - (1.0 - TIC_RATIO)*largeTicHeight_ - labelHeight_;
        } else {
          xmin = xmin - TIC_GAP - labelHeight_;
        }
      }
      x = graph_.getLayer().getXPtoD(xmin);
      width = graph_.getLayer().getXPtoD(xmax) - x;
    }
    return new Rectangle(x, y, width, height);
  }
  /**
   * NSR mod for log scale for positioning axis label (title) correctly
   */
  public void setNSRLogScale(boolean b) {
    nsrLogScale_ = b;
  }
  /**
   * Not implemented.
   */
  public void setBounds(Rectangle r) {
    setBounds(r.x, r.y, r.width, r.height);
  }
  /**
   * Not implemented.
   */
  public void setBounds(int x, int y, int width, int height) {
  }
}
