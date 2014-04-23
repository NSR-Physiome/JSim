      SUBROUTINE flohet(model, hrd,  hskew, hkurt,  nin, fin, wdin,
     +                  ilimf, fmin, fmax,  ftype,
     +                  nout,  fout, wdout, foutmx, istat)
c
c Compute a frequency histogram of regional relative flows to model
c flow heterogeneity
c
c File flohet.f (Version 1.2).  Last modified at 14:20:14 on 07/19/94.
c
c.......................................................................
c
c From:   National Simulation Resource
c         Center for Bioengineering (WD-12)
c         University of Washington
c         Seattle, WA 98195
c
c         Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c Copyright (C) 1994 by National Simulation Resource.
c All rights reserved.
c
c This software may be copied so long as this copyright notice is
c included.
c
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c.......................................................................
c
c DESCRIPTION
c 
c flohet generates a set of relative flows and associated weighting
c factors that describe a probability density function (PDF) of regional
c relative flows. These flows and weights are normalized to a mean and
c area of 1.0 so that they are suitable for setting the flows and
c weights of the flow paths in a multipath model while conserving mass
c and transit time. In the discussion below, the term "flow PDF" will be
c used to refer to the density function of relative flows, while the
c term "flow histogram" will be used for the flows and weights calculated
c by flohet.
c 
c The goals of flohet are: (1) to allow several choices for selecting a
c flow PDF that represents the distribution of regional flows in the
c tissue being modeled, (2) to allow flexibility in the selection of the
c relative flow values used in constructing the flow histogram, and
c (3) to select weights for the flows of the flow histogram that, faithfully
c represent the flow PDF.
c In some instances, goal 3 and the requirement that unity area and
c mean be maintained may conflict. In these cases conservation of mass
c and transit time take precedence, and the weights are adjusted. This
c results in a flow histogram that does not have the same statistical
c properties (dispersion, skewness, and kurtosis) as the PDF, but every
c attempt is made to match these as closely as possible.
c
c (See LIMITATIONS/WARNINGS)
c 
c Note that the weights returned are normalized so they can be used
c directly as weights for the flow paths. In the terminology used by
c Bassingthwaighte (1982), they are wd(i)'s:
c 
c      wd(i) = w(i) * delta f(i)
c 
c where delta f(i) is the width of the flow interval for path i. If the
c user specifies a flow PDF, the weights specified are also wd(i)'s.
c 
c More information about the calculation of relative flow PDF's is given
c in Bassingthwaighte (1982), and examples of experimentally measured
c PDF's are given in King et al. (1985) and Bassingthwaighte et al. (1990).
c 
c.......................................................................
c
c USAGE
c
c    PARAMETER  (nout=NNN)
c    INTEGER    model, nin, ilimf, nout, istat
c    REAL       hrd, hskew, hkurt, fin(1000), wdin(1000), fmin, fmax, ftype
c    REAL       fout(nout), wdout(nout), foutmx
c      .
c      .
c      .
c    CALL flohet (model, hrd, hskew, hkurt, nin, fin, wdin, ilimf,
c                 fmin, fmax, ftype, nout, fout, wdout, foutmx, istat)

c Formal Parameters
c 
c The formal parameters of flohet are described below. Note that some
c formal parameters may be used for either input or output depending on
c the method used for specifying the flow PDF, selected by parameter model,
c and the method used for selecting the flows of the histogram, selected
c by parameter ftype.
c
c Parameter  Type    Function
c _________  ______  __________________________________________________
c model      input   Select the model used for the flow PDF.
c                    0 = User specifies relative flows and frequencies
c                        using parameters nin, fin, and wdin.
c                    1 = Use a lagged-normal density function (LNDC).
c                        User specifies the relative dispersion and
c                        skewness using parameters hrd and hskew.
c                    2 = Use random walk density function (RANWOK).
c                        User specifies the relative dispersion and
c                        skewness using parameters hrd and hskew.
c hrd        input   When model = 1 or 2, the relative dispersion of
c                    the flow PDF. (Minimum value = 0.03; typical
c                    value 0.3 to 0.6.)
c hskew      input   When model = 1 or 2, the skewness of the flow PDF.
c                    (For LNDC, minimum value = 0.0, maximum value =
c                    1.99; for RANWOK, minimum value >= 3*hrd.)
c hkurt      input   The kurtosis of the flow PDF. (This parameter is
c                    currently unused and is included for future
c                    enhancements.)
c nin        input   When model = 0, the number of points in the user
c                    defined flow PDF. (Minimum value = 1.)
c            output  When model = 1 or 2, the number of points in the
c                    distribution function used as the flow PDF.
c                    (Minimum value = 1, maximum value = 1000.)
c fin        input/  When model = 0, an array of relative flows for the
c            output  flow PDF. (nin values in ascending order. The
c                    first value must be > 0.0.)
c                    NOTE: These values are overwritten with the flow
c                    values that result from the normalization of the
c                    flow PDF.
c            output  When model = 1 or 2, the relative flows of the
c                    generated flow PDF. (nin values. The first value
c                    is 0.005, and the increment is 0.01)
c 
c wdin       input/  When model = 0, the weight for each user specified
c            output  relative flow. (nin values >= 0.0.)
c                    NOTE: These values are overwritten with the
c                    weights that result from the normalization of the
c                    flow PDF.
c            output  When model = 1 or 2, the weight for each of the
c                    relative flows of the generated flow PDF. (nin
c                    values.)
c ilimf      input   Select the method used for clipping the tails of
c                    the flow PDF.
c                    = 1, use fmin and fmax.
c                    Not equal 1, use default limits (i.e, where wd(i)
c                    < 0.01%).
c fmin       input   When ilimf = 1, the minimum flow used from the
c                    PDF. (Minimum value = 0.0; maximum value = 0.5.
c                    If set > 0.5, a value of 0.5 is used.)
c fmax       input   When ilimf = 1, the maximum flow used from the
c                    PDF. (Minimum value = 1.5. If set < 1.5, a value
c                    of 1.5 is used.)
c ftype      input   Select the method used for selecting relative
c                    flows for the flow histogram.
c                    < 0.0 = User specifies relative flows using
c                            parameters nout and fout.
c                      0.0 = Use relative flows equally spaced in the
c                            flow domain.
c                      1.0 = Use relative flows spaced equally in the
c                            transit time domain.
c                      0.0 < ftype < 1.0 = Use relative flows linearly
c                            interpolated between equal spacing in the
c                            flow and transit time domains.
c                      1.0 < ftype, low flows weighted toward equal
c                            spacing in transit time domain, high 
c                            flows weighted toward equal spacing in 
c                            flow domain.
c nout       input   The number of relative flows in the flow histogram.
c                    (Minimum value 1, maximum value 1000.)
c fout       input/  When ftype < 0.0, an array of relative flows to be
c            output  used for the flow histogram. (nout values in
c                    ascending order. The first value must be > 0.0.)
c                    NOTE: These values are overwritten with the flow
c                    values that result from the normalization of the
c                    flow histogram.
c            output  When ftype >= 0.0, the relative flows for the
c                    flow histogram.
c wdout      output  The weight for each relative flow of the flow
c                    histogram.
c foutmx     output  The upper boundary of the flow interval for
c                    fout(nout). (See The Regional Flow Histogram for
c                    more information about this parameter.)
c istat      output  Completion status code. (See DIAGNOSTICS.)
c                      0 = Normal completion.
c                    > 0 = Parameter error detected and corrective
c                          action taken.
c                    < 0 = Fatal error detected.
c
c Parameter Limits and Default Actions
c 
c The table below gives the acceptable ranges of the input parameters.
c Also listed are the corrective actions taken when any of the
c parameters are outside the allowable range. If any corrective action
c is taken, istat is set to an appropriate value.
c 
c Parameter  Range               Corrective Action
c _________  __________________  _________________________________
c model      0, 1, 2             Use model = 1
c hrd        >= 0.03             Use hrd   = 0.5
c hskew      model=1, 0 - 1.99;  Use hskew = 0.0
c            model=2, > 3*hrd    Use hskew = 3.01*hrd
c hkurt      NONE                NONE
c nin        1 - 1000            Use LNDC (hrd = 0.5, hskew = 0.0)
c fin        ascending order,    Use LNDC (hrd = 0.5, hskew = 0.0)
c            fin(1) > 0.0
c wdin       All >= 0.0,         Use LNDC (hrd = 0.5, hskew = 0.0)
c            at least 1 > 0.0
c fmin       0.0 - <= 0.5        Use fmin = 0.5
c fmax       >= 1.5              Use fmax = 1.5
c fout       ascending order,    Use ftype = 1.01
c            fout(1) > 0.0
c 
c The Probability Density Function (PDF) of Regional Flows
c 
c Specifying the flow PDF: There are two methods for specifying the
c flow PDF: (1) the user can explicitly specify the relative flows and
c the weights, or (2) a mathematical model can be used. Selection
c between these methods is controlled by the parameter model.
c 
c User specified flow PDF (model = 0): The user-specified PDF is a
c vehicle for using experimentally determined data to set the flow
c heterogeneity. To use this method, the user must supply: (1) the
c number of relative flows in the PDF (nin), (2) the relative flow
c values (array fin containing nin values), and (3) the weight
c for each relative flow (array wdin containing nin values). No more
c than 1000 flow values can be used. The flow values must be in
c ascending order, and the lowest flow must be < 0.0. Each weight
c must be >= 0.0, and at least one weight must be > 0.0. The PDF
c specified need not be normalized to mean and area of 1.0; it will
c be normalized by flohet and the results stored in fin and wdin.
c 
c Using a mathematical model for the flow PDF (model > 0): Two
c mathematical models are available to generate the shape of the flow
c PDF: (1) a lagged normal density function (LNDC, model = 1), and
c (2) a random walk with first traversal (RANWOK, model = 2). To use
c this method, the user must supply: (1) the relative dispersion of
c the distribution (hrd), and (2) its skewness (hrd). For either
c function, the relative dispersion must be > 0.3. For a LNDC, the
c skewness must be in the range 0.0 to 1.99, and for a RANWOK it must
c be greater than or equal to  3*hrd.
c 
c When a mathematical model is used, the flow PDF generated is
c normalized and written into arrays fin and wdin. The number of
c points in the PDF is written into nin.
c 
c While both of these functions can generate right skewed curves
c (skewness > 0) neither can generate curves that are left skewed, nor
c can the kurtosis of the distribution be specified. The kurtosis
c parameter (hkurt) is included for future enhancements.
c 
c Clipping the tails of the flow PDF: In order to avoid having a
c number of relative flows at the extremes of the PDF that have very
c low weights, the tails of the PDF are clipped. The user can specify
c the limits at which the PDF is clipped, or the default limits can be
c used. The selection is controlled by parameter ilimf. User-specified
c limits (ilimf = 1) are contained in parameters fmin and fmax. In
c order to avoid severe deformation of the flow PDF, fmin must be
c <= 0.5, and fmax must be 1.5. If the specified value of either
c parameter violates its limit, the limit is used in the calculations.
c 
c Any value of ilimf other than 1 results in default clipping which
c removes 0.1% of the area from each end of the PDF. This gives a PDF
c with an area of 0.998, but the normalization of the flow histogram
c insures that the results still conserve mass and transit time.
c 
c The Regional Flow Histogram
c 
c The flows used for the flow histogram are returned in the array
c fout. The flow PDF is interpolated at these flows, and weights are
c calculated that are returned in the array wdout. Two methods are
c available for selecting flows for the histogram: (1) flows can be
c specified by the user, or (2) flows can be calculated by flohet in such
c a manner that they are distributed across the range of the PDF.
c flohet also returns the upper boundary for the highest flow interval
c in the parameter foutmx. Using the flow values and foutmx, it is
c possible to calculate the boundaries of each interval if required.
c See flo2in for details of the calculations.
c 
c User-specified histogram flows (ftype < 0.0): To use this method of
c selecting histogram flows, the user must supply: (1) the number of
c relative flows (nout), and (2) the relative flow values (array fout
c containing nout values). No more than 1000 flow values can be used.
c The flow values be in ascending order, and the lowest flow must be
c > 0.0.
c 
c Calculated histogram flows (0.0 <= ftype): When histogram flows
c are calculated, they can be distributed over the the range of the flow
c PDF so that they are equally spaced in the flow domain (ftype = 0.0),
c equally spaced in the transit time domain (ftype = 1.0), linearly
c interpolated between these two extremes (0.0 < ftype < 1.0), or chosen 
c so that the low flows are weighted toward the transit time domain 
c and the high flows are weighted toward the flow domain (ftype > 1.0).
c Examples of calculated flows for four values of ftype are given in 
c the table below. For each example, the number of paths (nout) is seven, 
c the minimum flow (fmin) is 0.2, and the maximum flow (fmax) is 1.8. As
c ftype increases toward 1.0, more low flows are selected, and the 
c highest relative flow decreases.
c
c When ftype > 1.0, the histogram flows are calculated so that the
c lower flows are distributed toward being equally spaced in the
c transit time domain, and the higher flows are distributed toward
c being equally spaced in the flow domain.  This has the effect of
c having many of the slower flow paths which affect the tail of the
c outflow curves, as well as preserving the statistics (RD and
c skewness) of the flow PDF. (RECOMMENDED CHOICE).
c
c 
c ftype                   Relative flows
c _____   ______________________________________________
c  0.0    0.31   0.54   0.77   1.00   1.23   1.46   1.69 
c  0.6    0.25   0.37   0.49   0.62   0.78   1.00   1.47
c  1.0    0.21   0.25   0.30   0.36   0.48   0.69   1.32
c  >1     0.23   0.32   0.47   0.69   0.96   1.29   1.63
c 
c 
c The optimum value of ftype depends on the data being modeled. In the
c case of indicator dilution curves, the high flow pathways determine
c the arrival time of the tracer at the outflow and, usually, the
c shape of the peak of the curve, while the low flow pathways determine
c the the shape of the tail of the curve.  Choosing equally spaced flows
c undersamples the long transit time paths and usually gives poor
c resolution on the tail of the curve. Choosing flows that are equally
c spaced in transit time undersamples the high flow pathways and may
c deform the peak and give an overestimate of the arrival time. Equally
c spaced flows give a flow histogram with statistics that most closely
c match those of the flow PDF. Thus, the value chosen for ftype is a
c compromise. For most combinations of parameters, the histogram is an
c adequate representation of the PDF when the distribution of flows is
c intermediate between equal spacing in the flow and transit time domains.
c For indicator dilution studies, a value of 0.6 is commonly used for ftype.
c For better statistical representation of the PDF and more low flow
c paths, a value greater than 1 for ftype is preferable.
c
c.......................................................................
c 
c LIMITATIONS/WARNINGS
c 
c nout: The value of nout should not exceed the dimension of fout and
c wdout. If it does, this error will not be detected by flohet and will
c lead to erroneous results or to a program crash.
c 
c nout must be in the range 1 - 1000. An invalid value of nout is the
c only error for which flohet takes no corrective action and returns
c no flow histogram.
c 
c Using the largest possible value of nout will result in a flow histogram
c that most faithfully represents the PDF. As ftype approaches 1.0,
c a large value of nout is required to obtain a good distribution of
c relative flows.
c 
c fin, wdin, and fout: When these arrays are used as input, their
c values will be overwritten with the normalized values of the flow PDF
c or histogram. Thus, temporary variables should be used in the calling
c routine if the entering values are to be preserved.
c 
c Flow histogram: Some parameter combinations can result in a flow
c histogram that is quite different than the specified PDF. The user
c should ensure that the histogram adequately represents the PDF. One
c way to do so is to plot both the PDF and histogram and compare them
c visually. The routines fcont and fhist are useful for obtaining
c values to be plotted.
c 
c.......................................................................
c
c DIAGNOSTICS
c
c The exit status of flohet is returned in istat. The values of istat
c and their meanings are given in the table below. A positive value
c indicates that a parameter error was detected, and a corrective action
c was taken. (See Parameter Limits and Default Actions.) Note that if
c more than one correctable error is detected, the values are summed
c and returned in istat. For example, a value of 133 would indicate
c that errors were detected in model, hskew, and fmin.
c 
c Value   Meaning
c  _____  ____________________________________________
c   -4    Fatal error (Histogram normalization failed)
c   -3    Fatal error (PDF normalization failed)
c   -2    Fatal error (PDF generation failed)
c   -1    Fatal error (nout out or range)
c    0    Normal return
c    1    Invalid value of model
c    2    Invalid value of hrd
c    4    Invalid value of hskew
c    8    Invalid value of hkurt
c   16    Invalid value of nin
c   32    Invalid value of fin
c   64    Invalid value of wdin
c  128    Invalid value of fmin
c  256    Invalid value of fmax
c  512    Invalid value of fout
c 
c.......................................................................
c 
c REFERENCES
c 
c Bassingthwaighte, J.B. Calculation of transorgan transport functions
c for a multipath capillary-tissue system: Multipath transorgan
c transport. Report PB82-2295 35 (UW/BIOENG-82/1) National Technical
c Information Services, Springfield, VA, 1982.
c 
c Bassingthwaighte, J.B., M.A. Malone, T.C. Moffett, R.B. King,
c S.E. Little, J.M. Link, and K.A. Krohn. Validity of microsphere
c depositions for regional myocardial flows.
c Am. J. Physiol. 253(Heart Circ. Physiol. 22): H184-H193, 1987.
c 
c Bassingthwaighte, J.B., M.A. Malone, T.C. Moffett, R.B. King,
c I.S. Chan, J.M. Link, and K.A. Krohn. Molecular and particulate
c depositions for regional myocardial flows in sheep.
c Circ. Res. 66: 1328-1344, 1990.
c 
c King, R.B., J.B. Bassingthwaighte, J.R.S. Hales, and L.B. Rowell.
c Stability of heterogeneity of myocardial blood flow in normal
c awake baboons. Circ. Res. 57: 285-295, 1985
c 
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED
c 
c NSR math library:
c normwf - Normalize wd and f arrays
c lagndc - Generate a lagged normal density function
c ranwok - Generate a random walk density function
c fgen   - Linear interpolation
c flo2in - Calculate flow intervals from flows
c flo2md - Calculate flows from flow intervals
c 
c.......................................................................
c
c SEE ALSO
c 
c fcont(3), fhist(3)
c.......................................................................
c
c HISTORY:
c
c Written:  G.M. Raymond and R.B. King (JAN94)
c
c Modified: G.M. Raymond (16JUN94)
c (1) Upper limit on hskew for LAGNDC changed in documentation to be 
c 1.99, consistent with program.  (2) Added new distribution of flows 
c for ftype > 1.0 (low flows weighted toward transit time, high flows
c weighted to flow domain. (3)  Documentation on lower limit of hrd
c changed to >= to 0.03 (was > 0.03) to conform with program.  (4)
c Changed values of flows in table in documentation. (5) Removed
c error code for invalid value of ftype. (6) Changed value of error
c code on invalid value of fout from 1024 to 512.  (6) Changed
c lower limit of hrd for RANWOK to be >= 3*hskew (previously, 3*
c hskew would cause it to be reset to 3.01*hskew). (7) Changed
c default value of ftype to 1.01 to invoke new distribution in (2.).
c (8) Added setting foutmx=2 for degenerate case of single flow.
c 
c-----------------------------------------------------------------------
c
      INTEGER      NSHAP
      PARAMETER   (NSHAP=1000)
      REAL         WDCUT1,       WDCUT2
      PARAMETER   (WDCUT1=0.001, WDCUT2=1.0-WDCUT1)
      REAL         HTMEAN,       DELTA,      AREADF,     FRPEAK
      PARAMETER   (HTMEAN=1.0,   DELTA=0.01, AREADF=1.0, FRPEAK=0.0)
      INTEGER      IX
      PARAMETER   (IX=0)
c
      INTEGER      model,  nin,     ilimf,   nout,  istat
      REAL         hrd,    hskew,   hkurt,   fmin,  fmax, ftype, foutmx
      REAL         fin(*), wdin(*), fout(*), wdout(*)
c
      CHARACTER*63 sid1, sid2
      INTEGER      lmodel, lstat, ilasto, i
      LOGICAL      degen, dftpdf, dftfty
      REAL         finti(0:NSHAP), finto(0:NSHAP)
      REAL         ftemp(0:NSHAP), cumin(0:NSHAP), win(0:NSHAP)
      REAL         flo, fhi, rlo, rhi, rtemp
      REAL         sigma, tau, tc, ta, tbar, tkappa
      REAL         lhrd, lhskew, lfmin, lfmax, lftype
c
      REAL         fgen
      EXTERNAL     fgen
c
      EQUIVALENCE (sigma, ta), (tau, tbar), (tc, tkappa)
      EQUIVALENCE (ftemp, win), (ftemp, cumin)
c
c Source Code Control Data
c
      DATA         sid1
     + /'@(#)flohet.f	1.2 created on 07/19/94 at 14:20:14.'/
      DATA         sid2
     + /'@(#)    Retrieved on 03/31/00 at 22:20:59.'/
c
c-----------------------------------------------------------------------
c
c  I. Check the input parameters.
c
      degen  = .FALSE.
      dftpdf = .FALSE.
      dftfty = .FALSE.
      istat  = 0
c
      IF (model.LT.0 .OR. model.GT.2) THEN
c
c      A. Invalid PDF selection.
c
          dftpdf = .TRUE.
          istat  = istat+1
c
      ELSE IF (model .EQ. 0) THEN
c
c      B. User specified PDF.
c
c         Check the number of flows.
c
          IF (nin.LT.1 .OR. nin.GT.1000) THEN
              dftpdf = .TRUE.
              istat  = istat+16
          END IF
c
c         Check the flows.
c
          IF (fin(1) .LT. 0.0) THEN
              dftpdf = .TRUE.
              istat  = istat+32
          ELSE
              DO 10 i = 1, nin-1
                  IF (fin(i) .GE. fin(i+1)) THEN
                      dftpdf = .TRUE.
                      istat  = istat+32
                      GO TO 11
                  END IF
   10         CONTINUE
          END IF
c
c         Check the weights.
c
   11     CONTINUE
          rtemp = 0.0
          DO 12 i = 1, nin
              IF (wdin(i) .GE. rtemp) rtemp = wdin(i)
              IF (wdin(i) .LT. 0.0) THEN
                  dftpdf = .TRUE.
                  istat  = istat+64
                  GO TO 13
              END IF
   12     CONTINUE
   13     CONTINUE
          IF (istat.EQ.0 .AND. rtemp.EQ.0.0) THEN
              dftpdf = .TRUE.
              istat  = istat+64
          END IF
c
      ELSE
c
c      C. Mathematical PDF.
c
c         Check the RD.
c
          IF (hrd .LT. 0.03) THEN
              istat = istat+2
              lhrd  = 0.5
          ELSE
              lhrd  = hrd
          END IF
c
c             Check the skewness.
c
          IF (model .EQ. 1) THEN
c
c             LNDC
c
              IF (hskew.LT.0.0 .OR. hskew.GT.1.99) THEN
                  istat  = istat+4
                  lhskew = 0.0
              ELSE
                  lhskew = hskew
              END IF
          ELSE
c
c             RANWOK
c
              IF (hskew .LT. 3.0*hrd) THEN
                  istat  = istat+4
                  lhskew = 3.01*hrd
              ELSE
                  lhskew = hskew
              END IF
          END IF
      END IF
c
c      D. PDF clipping parameters.
c
      IF (ilimf .EQ. 1) THEN
          IF (fmin.LT.0.0 .OR. fmin.GT.0.5) THEN
              istat = istat+128
              lfmin = 0.5
          ELSE
              lfmin = fmin
          END IF
c
          IF (fmax .LT. 1.5) THEN
              istat = istat+256
              lfmax = 1.5
          ELSE
              lfmax = fmax
          END IF
      END IF
c
c      E. Flow histogram parameters.
c
      IF(nout.LT.1 .OR. nout.GT.1000) THEN
          istat = -1
      ELSE IF (ftype .LT. 0.0) THEN
          IF (fout(1) .LT. 0.0) THEN
              istat  = istat+512
              dftfty = .TRUE.
          ELSE
              DO 14 i = 1, nout-1
                  IF (fout(i) .GE. fout(i+1)) THEN
                      istat  = istat+512
                      dftfty = .TRUE.
                      GO TO 15
                  END IF
   14         CONTINUE
   15         CONTINUE
          END IF
      END IF
c
c      F. Check that status and default flags.
c
      IF (istat .LT. 0) THEN
          RETURN
      ELSE
          IF (dftpdf) THEN
              lmodel = 1
              lhrd   = 0.5
              lhskew = 0.0
          ELSE
              lmodel = model
          END IF
          IF (dftfty) THEN
              lftype = 1.01
          ELSE
              lftype = ftype
          END IF
      END IF
c
c II. Handle degenerate case(s).
c 
      IF (nout .EQ. 1) THEN
c
c II.A.   Only one output flow.
c
          fout(1)  = 1.0
          wdout(1) = 1.0
          foutmx   = 2.0
          degen    = .TRUE.
      END IF
c
      IF (degen) RETURN
c
c III. Handle the flow PDF. (Phase 1)
c
      IF (lmodel .GT. 0) THEN
c
c III.A.  Calculate mathematical PDF's.
c
          IF (lmodel .EQ. 1) THEN
c
c             LNDC
c
              tau   = lhrd * (0.5*lhskew)**(1./3.)
              sigma = SQRT((lhrd+tau)*(lhrd-tau))
              tc    = HTMEAN - tau
              CALL lagndc(win(1), nin, DELTA, FRPEAK, NSHAP, AREADF,
     +                    sigma, tau, tc, IX)
              IF (nin .LT. 2) istat = -2
          ELSE
c
c             RANWOK
c
              tkappa = lhskew*SQRT(2.0)/3.0
              tbar   = HTMEAN*lhrd*3.0/lhskew
              ta     = HTMEAN - tbar
              CALL ranwok(win(1), nin, DELTA, FRPEAK, NSHAP, AREADF,
     +                    ta, tbar, tkappa)
              IF (nin .LT. 2) istat = -2
          END IF
c	
          IF (istat .LT. 0) RETURN
c
c         Convert from w to wd, and generate fin.
c
          DO 20 i = 1, nin
              fin(i)  = REAL(i-1)*DELTA + DELTA/2.
              wdin(i) = win(i)*DELTA
   20     CONTINUE
c
      END IF
c
c III.B.  Normalize the PDF.
c
c         Calculate the flow intervals and their midpoints.
c
      CALL flo2in(fin, finti, nin, lstat)
      IF (lstat .LT. 0) THEN
          istat = -3
          RETURN
      END IF
      DO 30 i = 1, nin
          ftemp(i) = 0.5*(finti(i-1)+finti(i))
   30 CONTINUE
c
c         Do the normalization (normwf) and rescale the intervals.
c
      rtemp   = ftemp(1)
      CALL normwf(wdin, ftemp(1), nin, lstat)
      IF (lstat .LT. 0) THEN
          istat = -3
          RETURN
      END IF
      rtemp    = ftemp(1)/rtemp
      finti(0) = finti(0)*rtemp
      DO 31 i = 1, nin
          finti(i) = rtemp*finti(i)
          fin(i)   = rtemp*fin(i)
   31 CONTINUE
c
c  III.C.  Clip the PDF.
c
c          Generate the cumulative density function.
c
      cumin(0) = 0.0
      DO 40 i = 1, nin
          cumin(i) = cumin(i-1) + wdin(i)
   40 CONTINUE
c
      IF(cumin(nin) .LE. 0.0) cumin(nin) = 1.0
      rtemp = 1.0/cumin(nin)
      DO 41 i = 0, nin
          cumin(i) = rtemp*cumin(i)
   41 CONTINUE
c
c          Locate the limits for default clipping.
c
      flo = finti(0)
      DO 42 i = 1, nin
          IF(cumin(i-1).LE.WDCUT1 .AND. WDCUT1.LT.cumin(i)) THEN
              flo = finti(i) + (finti(i-1)-finti(i))
     +                          *(cumin(i)-WDCUT1)/(cumin(i)-cumin(i-1))
              GO TO 43
          END IF
   42 CONTINUE
c
   43 CONTINUE
      fhi = finti(nin)
      DO 44 i = nin, 1, -1
          IF(cumin(i-1).LE.WDCUT2 .AND. WDCUT2.LT.cumin(i)) THEN
              fhi = finti(i) + (finti(i-1)-finti(i))
     +                          *(cumin(i)-WDCUT2)/(cumin(i)-cumin(i-1))
              GO TO 45
          END IF
   44 CONTINUE
c
c          Handle user-defined clipping.
c
   45 CONTINUE
      IF (ilimf .EQ. 1) THEN
          flo = MAX(lfmin, flo)
          fhi = MIN(lfmax, fhi)
      END IF
c
c  IV. Handle the flow histogram. (Phase 2)
c
c  IV.A.  Get the flow intervals.
c
      IF (lftype .GE. 0) THEN
c
c         Generate the intervals.
c
          rhi = 1.0/fhi
          rlo = 1.0/flo
          DO 50 i = 0, nout
              rtemp    = REAL(nout-i)/REAL(nout)
              IF(lftype.LE.1.0) THEN
                  finto(i) = (1.0-lftype)*(fhi+rtemp*(flo-fhi))
     +                          + lftype /(rhi+rtemp*(rlo-rhi))
              ELSE
                  finto(i) =      rtemp /(rhi+rtemp*(rlo-rhi))
     +                      +(1.0-rtemp)*(fhi+rtemp*(flo-fhi))
              ENDIF
   50     CONTINUE
      ELSE
c
c         Use user-specified flows.
c
          CALL flo2in(fout, finto, nout, lstat)
          IF (lstat .LT. 0) THEN
              istat = -4
              RETURN
          END IF
          IF(flo.GT.fout(1) .OR. fhi.LT.fout(nout)) THEN
              rtemp  = (fhi-flo)/(finto(nout)-finto(0))
              DO 51 i = nout, 0, -1
                  finto(i) = flo + rtemp*(finto(i)-finto(0))
   51         CONTINUE
          END IF
      END IF
c
c  IV.B.  Get the weights.
c
      ilasto = 1
      DO 60 i = 1, nout
          wdout(i) =   fgen(finti, nin+1, ilasto, cumin, finto(i))
     +               - fgen(finti, nin+1, ilasto, cumin, finto(i-1))
   60 CONTINUE
c
c  IV.C.  Normalize the histogram.
c
c         Get the midpoints.
c
      DO 70 i = 1, nout
          ftemp(i) = 0.5*(finto(i-1)+finto(i))
   70 CONTINUE
c
c         Normalize and rescale.
c
      rtemp = ftemp(1)
      CALL normwf(wdout, ftemp(1), nout, lstat)
      IF (lstat .LT. 0) THEN
          istat = -4
          RETURN
      END IF
      rtemp = ftemp(1)/rtemp
      DO 71 i = 0, nout
          finto(i) = rtemp*finto(i)
   71 CONTINUE
c
c  IV.D.  Calculate the flows from the intervals.
c
      CALL flo2md(fout, finto, nout, lftype, lstat)
      IF (lstat .LT. 0) THEN
          istat = -4
          RETURN
      END IF
      foutmx = finto(nout)
c
      RETURN
      END
