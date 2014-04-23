c File optdcn.F (Version 1.3, local).  Last modified at 14:05:32 on 2/3/98.
c
c Deconvolve output data from transfer function to produce an input
c function.
c
c.......................................................................
c
c From:  National Simulation Resource 
c        Department of  Bioengineering
c        University of Washington
c        Box 357962
c        Seattle, WA 98195-7962
c
c        Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1996 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c 
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c  
c HISTORY:
c
c Written:  G.M. Raymond      (AUG96)
c
c Modified: See SCCS archive.
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnopt(lopt,
     +                  tcout, cout, ncout, delth, nhhmax, hh,
     +                  smcoef, lexptl, endtim, tfit,
     +                  tcin, cin, ncin, cv, ev, aweght, istat)
c
      LOGICAL    lopt
      REAL       tcout(*), cout(*), hh(*), tcin(*), cin(*)
      INTEGER    ncout, nhhmax, ncin, istat
      REAL       delth
      REAL       smcoef
      INTEGER    lexptl
      REAL       endtim, tfit
      REAL       cv, ev, aweght
c
      INTEGER    NMAX
      PARAMETER (NMAX=200)
c
c Common block optdcm for routine dcnwvr used by ggopt.
c
      INTEGER    IPR,       MAXIT,     NX
      PARAMETER (IPR=-1,    MAXIT=2,   NX=1)
      REAL       GRDTL,     STPTL,     FMIN,     EPS
      PARAMETER (GRDTL=0.0, STPTL=0.0, FMIN=0.0, EPS=0.0001)
c
      REAL       tct(NMAX),ct(NMAX),dlth,h(NMAX),smcf,endtm,tft
      REAL       tcn(3*NMAX),cn(3*NMAX),cvar,evar,awt
      INTEGER    nct, nh, lxptl, ncn, istt
      COMMON    /optdcm/
     +           tct,ct,dlth,h,smcf,endtm,tft,tcn,cn,cvar,evar,awt,
     +           nct,nh,lxptl,ncn,istt
c      
c Variables for ggopt
c
      REAL       x(1), wk(9)

      EXTERNAL   ggopt
      EXTERNAL   dcnwvr
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)optdcn.F	1.3 (local) created on 2/3/98 at 14:05:32.\n'/
c
c This section is for optimization of the smoothing coefficient.
c
      IF (lopt) THEN
c
c         Copy the parameters to the common block.
c
          dlth   = delth
          endtm  = endtim
          tft    = tfit
          lxptl  = lexptl
          nct    = MIN(ncout, NMAX)
          awt    = aweght
          IF((awt.GT.1.0) .OR. (awt.LT.0.0)) awt = 0.5
c
c         Copy the output data to the common block.
c
          DO 10 i = 1, nct
              tct(i) = tcout(i)
              ct(i)  = cout(i)
   10     CONTINUE
c 
c         Parameter being optimized is log(base10) of smcoef.
c
          IF((smcoef.LT.1.0e-05) .OR. (smcoef.GT.1.0e05)) THEN
              x(1) = 0.0
          ELSE
              x(1) = LOG(smcoef)/LOG(10.)
          ENDIF
c 
c         Use ggopt to get minimum for weighted variation.
c
          CALL ggopt(dcnwvr, NX,X, MAXIT,GRDTL,STPTL, IPR,
     +               FMIN, EPS, wk, istop)
          IF (istop .EQ. 0) THEN
	     CALL scwmsg('Optimization of the smoothing coef failed.')
	  ENDIF
c
          smcoef = 10.0**x(1)
c 
      END IF
c
c Perform the deconvolution to get results (tcin, cin).
c
      CALL dcnper(tcout, cout, ncout, delth, nhhmax, hh,
     +            smcoef, lexptl, endtim, tfit,
     +            tcin, cin, ncin, cv, ev, istat)
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      REAL FUNCTION dcnwvr(x)
c
c Subroutine for ggopt for convolution and weighted variation
c
c DESCRIPTION:
c
c    This function is used by subroutine ggopt to find the minimum
c of the weighted variation, which is defined to be the weighted
c sum of the coefficient of variation, cvar, between the output data 
c for the deconvolution and the output from convolving the derived
c input function with the transfer function, and the excess
c variation, evar, of the derived input function.  The weighted sum 
c is given by
c
c     dcnwvr = awt*cvar + (1.0-awt)*evar
c
c where awt is between 0 and 1, cvar is the coefficient of
c variation, and evar is the excess variation of the input
c function.
c
c A quadratic penalty function has been added to constrain the
c smoothing coefficient to lie between 1.0e-05 and 1.0e+05.
c
c.......................................................................
c
      REAL       x(*)
c
      INTEGER     NMAX
      PARAMETER  (NMAX=200)
      REAL        tct(NMAX),ct(NMAX),dlth,h(NMAX),smcf,endtm,tft
      REAL        tcn(3*NMAX),cn(3*NMAX),cvar,evar,awt
      INTEGER     nct, nh, lxptl, ncn, istt
      COMMON     /optdcm/
     +            tct,ct,dlth,h,smcf,endtm,tft,tcn,cn,cvar,evar,awt,
     +            nct,nh,lxptl,ncn,istt
c
      REAL        smin, smax
      DATA        smin /-5.0/
      DATA        smax / 5.0/
c
      smcf = 10.0**MIN(MAX(x(1),smin),smax)
c
c Calculate deconvolution using the variables in the common block.
c
      CALL dcnper(tct, ct, nct, dlth, NMAX, h, smcf, lxptl,
     +            endtm, tft, tcn, cn, ncn, cvar, evar, istt)
c
c Compute the weighted variation.
c
      dcnwvr = awt*cvar+(1.0-awt)*evar
c
c Add in relative quadratic penalty function for ggopt.
c
      IF (x(1) .LT. smin) dcnwvr = dcnwvr*(1.+ABS(smin-x(1))**2.0)
      IF (x(1) .GT. smax) dcnwvr = dcnwvr*(1.+ABS(smax-x(1))**2.0)
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnper(tcout, cout, ncout, delth, nhhmax, hh,
     +                  smcoef, lexptl, endtim, tfit,
     +                  tcin, cin, ncin, cv, ev, istat)
c
c Deconvolve output data from transfer function to produce an input
c function.
c
c.......................................................................
c
      REAL       tcout(*), cout(*)
      INTEGER    ncout
      INTEGER    nhhmax
      REAL       delth, hh(*)
      REAL       smcoef
      INTEGER    lexptl
      REAL       endtim, tfit
      INTEGER    ncin
      REAL       tcin(*), cin(*)
      REAL       cv, ev
      INTEGER    istat
      REAL       smcof, tmfit
c
      INTEGER    NDIM
      PARAMETER (NDIM=200)
c
      REAL       tfin(NDIM), cfin(NDIM)
      INTEGER    nfin
      REAL       ttrnc(2*NDIM), ctrnc(2*NDIM)
      INTEGER    ntrnc
      REAL       tcrs(NDIM), ccrs(NDIM), cors(NDIM)
      INTEGER    ncrs
      REAL       wk(NDIM*NDIM), wkv(2*NDIM)
      REAL       texi(3*NDIM), cexi(3*NDIM)
      INTEGER    nexi
      REAL       t3(10*NDIM),   c3(10*NDIM)
c
      REAL       frmach, deltc
      INTEGER    n3, nmax, i, nc, ncmax, icmax, ncors, ierr
      INTEGER    iskip
     
      INTEGER    isamax
      REAL       dcncvr, dcnevr
      EXTERNAL   isamax, dcncvr, dcnevr
c
c.......................................................................
c
c Set up section.
c
c     Check for invalid input values.
c
      IF ((ncout.LT.1) .OR. (delth.LE.0.0)) THEN
          istat = -1
          RETURN
      END IF
c
c     Set local parameters.
c
      istat = 0
      smcof = MIN(MAX(smcoef,1.0e-5),1.0e5)
c
c     Copy the output data adding a point at time=0 if necessary.
c
      IF (tcout(1) .GT. 0.0) THEN
          ntrnc    = MIN(ncout+1,NDIM+1)
          iskip    = 1
          ttrnc(1) = 0.0
          ctrnc(1) = 0.0
      ELSE
          ntrnc    = MIN(ncout,NDIM+1)
          iskip    = 0
      END IF
c
      DO 10 i = 1,ntrnc
          ttrnc(i+iskip) = tcout(i)
          ctrnc(i+iskip) = cout(i)
   10 CONTINUE
c
c.......................................................................
c
c Perform deconvolution at the resolution specified by delth.
c
c     Interpolate the output curve.
c
      DO 110 i = 1,NDIM
          tcin(i) = REAL(i-1)*delth
          tfin(i) = tcin(i)
  110 CONTINUE
      nfin = MIN(NDIM, NINT(ttrnc(ntrnc)/delth)+1)
      CALL dcnint(ttrnc,ctrnc,ntrnc,tcin,cfin,nfin,wkv)
c
c     Get the transfer function.
c
      CALL dcnvh(delth, endtim, nhhmax, nhh, hh)
      IF (nhh .LT. 2) THEN
          istat = -1
      END IF
c
c     Do the deconvolution.
c
      CALL dconv2(hh,nhh,cfin,nfin,delth,smcof,wk,cin,ncin,ierr)
c
c.......................................................................
c
c Extrapolate the data if necessary.
c 
      IF ( endtim.GT.ttrnc(ntrnc)       .AND.
     +    (lexptl.EQ.1 .OR. lexptl.EQ.2)     ) THEN
          nmax  = 2*NDIM
          icmax = isamax(ntrnc,ctrnc,1)
          tmfit = MAX(tfit,ttrnc(icmax))
c
          CALL dcnext(ttrnc,ctrnc,ntrnc,nmax,lexptl,tmfit,endtim,
     +                                      texi,cexi,nexi,istat)
      ELSE
c
c         No extrapolation is required, load the extrapolation arrays
c         the interpolated data.
c
          nexi = ntrnc
          DO 210 i = 1, nexi
              texi(i) = ttrnc(i)
              cexi(i) = ctrnc(i)
  210     CONTINUE
      ENDIF
c
c.......................................................................
c
c Do the low resolution deconvolution if necessary.
c
      IF (texi(nexi) .GT. tfin(nfin)) THEN
c
c         Calculate the new time step.
c
          deltc = MAX(texi(nexi)/REAL(NDIM-1), delth)
c
c         Interpolate the output data.
c         
          ncrs  = MIN(NDIM, NINT(texi(nexi)/deltc)+1)
          DO 230 i = 1, ncrs
              tcrs(i) = REAL(i-1)*deltc
  230     CONTINUE
          CALL dcnint(texi,cexi,nexi,tcrs,ccrs,ncrs,wkv)
c
c         Get the low resolution transfer function.
c
          CALL dcnvh(deltc, texi(nexi), nhhmax, nhh, hh)
c
c         Perform the deconvolution.
c
          CALL dconv2(hh,nhh,ccrs,ncrs,deltc,smcof,wk,
     +                cors,ncors,ierr)
c
c        Put together both deconvolutions using curve matching.
c        Joining of curves will be done after the peak, beginning
c        at the level of frmach*cmax.
c
          frmach = 1.0
          CALL dcnmch(tcin,cin,ncin,tcrs,cors,ncors,frmach,
     +                t3,c3,n3,istat)
          DO 240 i = 1, n3
              tcin(i) = t3(i)
              cin(i)  = c3(i)
  240     CONTINUE
          ncin = n3
c
      END IF
c
c.......................................................................
c
c Wrap-up with the calculation of some statistics.
c
c     Calculate the CV between the covolution of cin and the high
c     resolution transfer funcion and cout.
c
c     Use the work array wk to set the variance for each data point 
c     in (cout(j), tcout(j),j=1,ncout) to be equal to 1.0.
c
      ncmax = 10*NDIM
      CALL dcnvh(delth, endtim, nhhmax, nhh, hh)
      CALL dcncnv(cin,tcin,ncin,hh,tfin,nhh,c3,t3,nc,ncmax,wk,
     +            tcout(ncout))
c
      DO 300 i = 1, ncout
          wk(i) = 1.0
  300 CONTINUE
c
      cv = dcncvr(c3,t3,nc,cout,tcout,ncout,wk)
c
c     Calculate the excess variation of cin.
c
      ev = dcnevr(cin,ncin)
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      REAL FUNCTION dcncvr(vx,tx,nx,vy,ty,ny,varvy)
c
c Calculate coefficient of variation between two series
c
c DESCRIPTION:
c    Interpolate (vx(i),tx(i),i=1,nx) for each (ty(j),j=1,ny).
c Calculate weighted coefficient of variation between vy(j), and
c vx(interpolated at ty(j)), j=1,ny).
c Coefficient of variation is only calculated for only where the
c series overlap in their independent variable.
c
c.......................................................................
c
      REAL     vx(*),tx(*),vy(*),ty(*), varvy(*)
      INTEGER  nx, ny
      INTEGER  i, j
      LOGICAL  sumwt
      REAL     xj, wt, sumc, sumw, sumwy, sumwy2
      INTEGER  ilast
      REAL     fgen
      EXTERNAL fgen
c
      dcncvr=0.0
      IF(ny.LT.2) RETURN
c
c IF any variances are all positive, weights are 1/varvy(i)
c ELSE make all weights = 1.0
c 
      sumwt=.TRUE.
      DO 10 i=1,ny
          IF(varvy(i).LE.0.0) THEN
              sumwt=.FALSE.
          ENDIF
   10 CONTINUE
c 
      ilast  = 0
      sumc   = 0.0
      sumw   = 0.0
      sumwy  = 0.0
      sumwy2 = 0.0
      DO 20 j=1,ny
          IF( (ty(j).GE.tx(1)).AND.( ty(j).LE.tx(nx)) ) THEN
              xj=fgen(tx,nx,ilast,vx,ty(j))
              IF(sumwt) THEN
                  wt=1./varvy(j)
              ELSE
                  wt=1.0
              ENDIF
              sumc   = sumc + 1.
              sumw   = sumw + wt
              sumwy  = sumwy + wt*ABS(vy(j))
              sumwy2 = sumwy2 + wt*(xj-vy(j))**2.
          ENDIF
   20 CONTINUE
      IF( (sumwy.GT.0).AND.(sumc.GT.1.) ) THEN
          dcncvr= SQRT(sumwy2/((1.-1./sumc)*sumw))/(sumwy/sumw)
      ELSE
         dcncvr= 0.0
      ENDIF
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcncnv(vi,ti,ni,vh,th,nh,vo,to,no,nomax,wk,timend)
c
c Interpolate two curves and convolve
c
c DESCRIPTION
c
c Assume (vi(j),ti(j),j=1,ni) is the input function and 
c (vh(j),th(j),j=1,nh) is the transfer function.  Assume both
c functions are unequally spaced in time.  Interpolate both
c functions to the smallest time step from either and convol
c to a maximum of nomax-1 points.
c
c.......................................................................
c
      REAL     vi(*),ti(*),vh(*),th(*)
      REAL     vo(nomax),to(nomax),wk(2*nomax)
      REAL     timend
      INTEGER  ni, nh, no, nomax
      INTEGER  iwk, i, ilast, inh
      SAVE     iwk
      REAL     vii, dt, t
      REAL     fgen, gconv
      EXTERNAL fgen, gconv
      no=0
      IF( (ni.LT.2).OR.(nh.LT.2) ) RETURN
c
      dt=ABS(ti(2)-ti(1))
      DO 10 i=2,ni-1,1
          dt=MIN(dt,ABS(ti(i+1)-ti(i)))
   10 CONTINUE
      DO 20 i=1,nh-1,1
          dt=MIN(dt,ABS(th(i+1)-th(i)))
   20 CONTINUE
c
      IF( dt.LE.0.0) RETURN
c
c Interpolate the transfer function and store in first half of wk 
c array
c
      ilast=0
      inh=MIN(INT(th(nh)/dt)+1,nomax-1)
      DO 30 i=1, inh
          t=(i-1)*dt
          wk(i)=fgen(th,nh,ilast,vh,t)
   30 CONTINUE
c
c Interpolate input function and convol with transfer function 
c to produce (vo(j),to(j),j=1,no, no<=nomax-1)
c
      ilast=0
      no=MIN(INT(ti(ni)/dt)+1,nomax-1,INT(timend/dt)+1)
      iwk=0
      DO 40 i=1, no
          to(i)=REAL(i-1)*dt
          vii=fgen(ti,ni,ilast,vi,to(i))
          vo(i)=gconv(vii,wk(1),inh,dt,wk(nomax+1),iwk)
   40 CONTINUE
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      REAL FUNCTION dcnevr(x,n)
c
c Calculate excess variation of a curve
c
c DESCRIPTION
c
c The excess variation measures how much a curve which
c is everywhere non-negative departs from being unimodal.  
c The excess variation of a unimodal curve is 0.0.
c
c.......................................................................
c
      REAL    x(*)
      INTEGER n
      REAL    xmax
      INTEGER i
      dcnevr=0.0
      xmax=-1.e35
c
      DO 10 i=1,n
          xmax=MAX(xmax,x(i))
  10  CONTINUE
      IF(xmax.LE.0) RETURN
c
      dcnevr=-2.*xmax
      DO 20 i=1,n-1,1
      dcnevr=dcnevr+ABS(x(i+1)-x(i))
  20  CONTINUE
c
c The next line is equivalent to having a zero value at
c both ends of the curve
c
      dcnevr=dcnevr+ABS(x(1))+ABS(x(n))
      dcnevr=MAX(dcnevr,0.0)/xmax
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnint(ti,ci,ni,to,co,no,wk)
c
c Interpolate a curve producing another curve
c
c Interpolate ((ti(j),ci(j)),j=1,ni) using array to(k) to produce 
c ( co(k), k=1,no).  Dimension of work array wk is >=ni. 
c
c.......................................................................
c
      REAL ti(*), ci(*), to(*), co(*), wk(*)
      INTEGER ni, no, i, ierr
      LOGICAL skip
      DATA skip/.TRUE./
      CALL pchim(ni,ti,ci,wk,1,ierr)
      DO 10 i=1,no
          CALL pchfe(ni,ti,ci,wk,1,skip,1,to(i),co(i),ierr)
   10 CONTINUE
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      INTEGER FUNCTION dcnix0(x,nx,x0)
c
c Return location of where in ascending array x, x0 is closest
c Return 0 if x0 < x(1), or nx+1 if x(nx)<x0
c
c.......................................................................
c
      REAL    x(*), x0
      INTEGER nx
      REAL    xmin, xc
      INTEGER i
c
c Return dcnix0=0 if x0<x(1)
c
      dcnix0=0
      IF(x0.LT.x(1) ) RETURN
c
      xmin=ABS(x(1)-x0)
      DO 10 i=1,nx
          xc=ABS(x(i)-x0)
          xmin=MIN(xmin,xc)
          IF(xmin.EQ.xc) dcnix0=i
   10 CONTINUE
c
c Return dcnix0=nx+1 if x0>x(nx)
c
      IF(x0.GT.x(nx)) dcnix0=nx+1
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnext(ti,ci,ni, nmax,lexptl,tfit,tend, to,co,no,istat)
c
c Extrapolate a curve with decaying tail.
c
c DESCRIPTION:
c
c The input  data is (ci(k),ti(k),k=1,ni).  It is a 'complete'
c curve (rise, peak, tail).  tfit is where on the tail to begin
c the extrapolation.  It will be defaulted to at least the last
c eight points of the curve if it is set for fewer than the last
c eight points.  tend is how far out in time to extrapolate the
c curve.  If tend is less than the last time value of the input
c curve, no extrapolation is done.
c
c The output curve contains most of the input curve.  Points in
c the extrapolation data can replace some of the data from
c tfit on to assure a smooth joining of the input data with the
c extrapolated data.
c
c The output data is (co(k),to(k),k=1,no)
c
c Assumptions:  The input curve contains 201 or fewer points.
c The output curve contains a maximum of 400 points.  The input
c curve need not contain equally spaced data.  Extrapolated data
c after the last time value in ti will be equally spaced out to
c tend.
c
c nmax will be 400 (2*NDIM)
c
c tfit is where to began the fitting.  It should be
c after the peak.  If there are fewer than 8 data points after the
c peak, the  extrapolation will probably be poor.
c 
c.......................................................................
c 
      REAL    ti(*), ci(*), to(*), co(*)
      REAL    tfit, tend
      INTEGER ni, nmax, lexptl, no, istat
      INTEGER jexptl
c
      INTEGER NDIM
      PARAMETER (NDIM=200)
      REAL    timfit(3*NDIM), cimfit(3*NDIM)
      REAL    wt(NDIM)
c
      REAL    fcoef(2)
c
      REAL    wk1exp(NDIM*2*8), wk2exp(NDIM*2), parexp(2*8)
      INTEGER ilbegn, nfit, n3, i, ibegin, nexpo, ipr
      REAL    frmach, tbegin, dat1, rwkvfp(4*NDIM)
      INTEGER dcnix0
      EXTERNAL dcnix0
c
c     tfit where the fitting of data begins, data used will be
c     from tfit(ifit) to ti(ni).  If number of data points less
c     than 2, an error.
c
c     lexptl =0, no extrapolation, extension of curve becomes 0
c            =1, power tail 
c            =2, multi-exponential tail
c
c     Return status
c     istat=0, OK,
c     istat=1, OK, tend <= ti(ni), no extrapolation necessary
c     
c.......................................................................
c
c If desired extrapolation time less than input data, return the
c     ci(k),ti(k),k=1,ni)
c
c
      istat=0
      IF( (ni.LT.8).OR.(nmax.LE.ni).OR.(tend.LE.ti(ni))) THEN
          no=MAX(1,MIN(nmax,ni))
          istat=1
          DO 10 i=1,no 
              to(i)=ti(i)
              co(i)=ci(i)
   10     CONTINUE
          RETURN
      ENDIF
c
c.......................................................................
c
      jexptl=MIN(MAX(lexptl,0),2)
c
c
      no=nmax
c
c Find where to began data fitting, use minimum of tfit and ti(ni-7)
c
      tbegin=MIN(tfit,ti(ni-7))
      ibegin=dcnix0(ti,ni,tbegin)
    
      ilbegn=ni-ibegin+1
c
c.......................................................................
c
c Generate the time points for the fits
c
      DO 100 i=1,ni
          to(i)=ti(i)
          co(i)=ci(i)
  100 CONTINUE
      nfit=nmax-ibegin
      DO 110 i=ni+1, nmax
          to(i)=ti(ni) + (tend-ti(ni))*REAL(i-ni)/REAL(nmax-ni)
  110 CONTINUE
c
c.......................................................................
c
c No printed output from curve extrapolations
      ipr=-1
c
c Only requesting zeros added to end of data
c
      IF(jexptl.EQ.0) THEN
          DO 210 i=1, ni
              co(i)=ci(i)
  210     CONTINUE
          DO 220 i=ni+1,nmax
              co(i)=0.0
  220     CONTINUE
          RETURN
c
c.......................................................................
c
      ELSEIF(jexptl.EQ.1) THEN
c
c Extrapolate with fitting of power tail function
c
          CALL fitpow(ilbegn,ti(ibegin),ci(ibegin),nfit,to(ibegin+1),
     +            co(ibegin+1),fcoef,rwkvfp,ierr)
          IF (ierr .EQ. -2)
     +        CALL scwmsg("Tail extension in deconvolution failed.")
c
c
c Extrapolate with multi-exponentials
c
      ELSEIF(jexptl.EQ.2) THEN
c     Fit a maximum of eight exponentials
          nexpo=MIN(8,ilbegn/2)
c     No constraint on first extrapolation point
          dat1=-1.0
c     Minimize the sum of the square of the residuals
          DO 400 i=1, ni
              wt(i)= 1.0
  400     CONTINUE
          CALL fitexp(ilbegn,ti(ibegin),ci(ibegin),nfit,to(ibegin+1),
     +            co(ibegin+1), wk1exp, wk2exp, wt(ibegin), nexpo, 
     +            dat1, parexp, ipr)
      ENDIF
c
c Join extrapolated curve with input curve
c
      frmach=1.0
      CALL dcnmch(ti,ci,ni,to(ibegin+1),co(ibegin+1),nfit,frmach,
     +            timfit,cimfit,n3,istat)
      no=n3
      DO 500 i=1,no
          to(i)=timfit(i)
          co(i)=cimfit(i)
  500 CONTINUE
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnmch(t1,c1,n1,t2,c2,n2,frmach,t3,c3,n3,istat)
c
c Match and join two curves
c
c SYNOPSIS
c
c    REAL t1(*),c1(*),t2(*),c2(*),t3(*),c3(*),frmach
c    INTEGER n1, n2, n3,istat
c
c    CALL dcnmch(t1,c1,n1,t2,c2,n2,frmach,t3,c3,n3,istat)
c
c DESCRIPTION
c    Curve one is given by (t1(j),c1(j),j=1,n1); curve two is given 
c by (t2(j),c2(j),j=1,n2).  It is assumed that each curve
c has a peak, and that the curves match over some portion of their 
c tails.  Beginning at a level of frmach*c1(imax) for curve one where 
c c1(i1max) is the peak of curve one and frmach is the fraction of the 
c peak of curve 1 after which matching will occur, create curve 3, 
c (t3(j),c3(j),j=1,n3) where the third curve contains curve 1 up to 
c the matching point followed by the tailing portion of curve 2. 
c
c ASSUMPTIONS
c              Both curves everywhere non-negative if matched in
c              a logarithmic sense.
c
c              There is a region where they match after their
c              peaks
c            
c LIMITATIONS/WARNINGS
c    If the curves overlap where they both have areas of zero
c values, there is a very good chance that this is where they
c will match best.
c
c.......................................................................
c
      REAL     t1(*), c1(*)
      REAL     t2(*), c2(*)
      REAL     t3(*), c3(*)
      REAL     frmach
      INTEGER  n1, n2, n3, istat
      LOGICAL  llgli
c
      REAL     frmat
      REAL     t
      INTEGER  i, ic1max, imatch, im1, it1, it2
      INTEGER  isamax, dcnix0
      EXTERNAL isamax, dcnix0, dcnmcv
c
c    I.  llgil=.TRUE., match curves in a logarithmic sense
c
      IF( (n1.LE.0).OR.(n2.LE.0) ) THEN
          istat=-1
          n3=0
          RETURN
      END IF
      istat=0
      frmat=MIN(frmach,1.0)
c
c   II.  Determine where on curve 1 after the peak to begin matching 
c        with curve 2
c
      imatch=n1
      ic1max=isamax(n1,c1,1)
      DO 10 i=n1,ic1max,-1
          IF(c1(i).LT.frmat*c1(ic1max) ) THEN
             imatch=i
          ENDIF
   10 CONTINUE
      im1=n1-imatch+1
      llgli=.TRUE.
      CALL dcnmcv(t1(imatch),c1(imatch),im1,t2,c2,n2,llgli,t,istat)
c
c  III.  Join curve 1 and curve 2 where they match best
c        Part of the code in this section is to avoid
c        having the same time value for two points sequentially
c        in the third curve (t3(j),c3(j),j=1,n3); i.e.,
c        t3(j) never should equal t3(j+1) for j=1 to n3-1. 
c        As a precaution we require that at the joining point
c        the difference in time between the end of the first
c        curve and the beginning of the second curve be greater
c        than the minimum spacing of data in either curve.
c
      it1=dcnix0(t1,n1,t)
      it2=dcnix0(t2,n2,t)
c
c        Special handling for t1(it1)+sdt<=t2(it2)
c
      sdt=ABS(t1(2)-t1(1))
      DO 15 i=2,n1-1,1
          sdt=MIN(sdt,ABS(t1(i+1)-t1(i)))
   15 CONTINUE
      DO 17 i=1,n2-1,1
          sdt=MIN(sdt,ABS(t2(i+1)-t2(i)))
   17 CONTINUE
      IF(t2(it2)-t1(it1).LT.sdt) THEN
          IF(it2.LT.n2) THEN
             it2=it2+1
          ELSE
             it1=it1-1
          ENDIF
      ENDIF
c
c        Repeat test
c
      IF(t2(it2)-t1(it1).LT.sdt) THEN
          IF(it2.LT.n2) THEN
             it2=it2+1
          ELSE
             it1=it1-1
          ENDIF
      ENDIF
c
c  IV.  Make third curve from the two curves
c
      DO 20 i=1,it1
          t3(i)=t1(i)
          c3(i)=c1(i)
   20 CONTINUE
      DO 30 i=it2,n2
          t3(it1+i+1-it2)=t2(i)
          c3(it1+i+1-it2)=c2(i)
   30 CONTINUE
      n3=it1+n2+1-it2
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnmcv(t1,c1,n1,t2,c2,n2,llgli,t,istat)
c
c Match curve 1 to curve 2, t is the time of the best match.
c
c DESCRIPTION
c
c The point in time where two curves match best is determined.
c The region of time overlap between the two curves is calculated.
c Over the region of time overlap, each curve is 
c linearly interpolated over 1024 points.  The absolute value
c of their differences or differences of their logs is calculated.
c Find the minimum of this new curve over different window sizes,
c i.e., the minimum of single points, the minimum of adjacent
c 2 points, ... the minimum of adjacent n points.  In a set of bins
c corresponding to the interpolated differences, add -1
c for each time a point belongs to a minimum.
c Where the bin counts are most negative, the interpolated point
c belongs to the most minimums of different numbers of adjacent points.
c This is the point where both curves match the best.
c 
c istat=1 means no overlap in time.
c Assumptions: both curves positive everywhere, and have a region of
c near overlap. The best fit is determined in either a logarithmic
c sense or a linear sense.  The time, t, is where the two curves
c match best.  
c
c.......................................................................
c
      INTEGER NDIM
      PARAMETER (NDIM=1024)
      REAL     t1(*), c1(*), t2(*), c2(*), t
      INTEGER  n1, n2, istat
      LOGICAL  llgli
c Set llgli to TRUE for logaritmic matching
c Set llgli to FALSE for linear matching
      REAL     timstr, timend, dt, aldif(NDIM), ovlap(NDIM)
      INTEGER  ismin(NDIM), ilast1, ilast2, istart, iend, ilen
      REAL     c1v, c2v, fgen, r1mach
      INTEGER  dcnilm
      EXTERNAL fgen, r1mach, dcnilm
      INTRINSIC MAX
      CALL f7init()
c
      t=0.0
c
c Are either series a single point?  
c
      istat=0
      IF( (n1.LT.2).OR.(n2.LT.2) ) THEN
          t=MIN(t1(1),t2(1))
          istat=1
      ENDIF
c
c If series do not overlap, return the best fit as halfway 
c between where one ends and the next series starts.
c
      IF (t1(n1).LE.t2(1)) THEN
          istat=-1
          t=(t1(n1)+t2(1))/2.
          RETURN
      ELSE IF (t2(n2).LE.t1(1) ) THEN
          istat=-1
          t=(t2(n2)+t1(1))/2.
          RETURN
      ENDIF
c
c Find the overlap
c
      timend=MIN(t1(n1),t2(n2))
      timstr=MAX(t1(1) ,t2(1) )
      dt=(timend-timstr)/(NDIM-1)
c      
c Compute either ABS(LOG(difference)) or ABS(difference)
c from timstr to timend depending on llgli
c
      ilast1=0
      ilast2=0
      DO 20 i=1,NDIM
          t=timstr+REAL(i-1)*dt
          c1v=fgen(t1,n1,ilast1,c1,t)
          c2v=fgen(t2,n2,ilast2,c2,t)
          IF(llgli) THEN
              IF(c1v.LT.1.e-20) c1v=1.e-20
              IF(c2v.LT.1.e-20) c2v=1.e-20
              aldif(i)=ABS(log(c1v)-log(c2v))
          ELSE
              aldif(i)=ABS(c1v-c2v)
          ENDIF
   20 CONTINUE
c Find the minimum for different lengths
      DO 30 ilen=1,NDIM/16
          ismin(ilen)=dcnilm(aldif,NDIM,ilen)
   30 CONTINUE
c
c Find where most of these segments overlap
c
      DO 40 i=1,NDIM
          ovlap(i)=0
   40 CONTINUE
      DO 50 ilen=1,NDIM/16
          istart=ismin(ilen)
          iend  = istart+ilen-1
          DO 60 i=istart,iend
              ovlap(I)=ovlap(i)-1
   60     CONTINUE
   50 CONTINUE
      it=dcnilm(ovlap,NDIM,1)
      t=timstr+REAL(it-1)*dt
c 
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      INTEGER FUNCTION dcnilm(x,n,ilen)
c
c Find j such that sum of (x(k)), k=j,j+ilen-1) is a minimum
c
c DESCRIPTION 
c
c Find j such that sum of (x(k)), k=j,j+ilen-1) is a minimum.
c If there are more than one value of j which produces the 
c same minimum, the largest j is returned.
c
c.......................................................................
c
      REAL     x(*)
      INTEGER  n, ilen
      INTEGER  i, m, jlen
      REAL     sum, summin
c
c    I.  Assume at least one data point in the series.
      m=MAX(1,n)
c
c   II.  Assume that the length of the window for the computation
c        is between 1 and n.
      jlen=MIN(MAX(1,ilen),m)
c
c  III.  Begin with first sum
c
      sum = 0.0
      DO 10 i=1,jlen
          sum=sum+x(i)
   10 CONTINUE
      summin=sum
      dcnilm=1
c
c   IV.  On subsequent sums, subtract out the previous first point
c        and add in the next end point
      DO 20 i=2,m-jlen+1,1
          sum=sum-x(i-1)+x(i+jlen-1)
          summin=MIN(sum,summin)
          IF(summin.EQ.sum) dcnilm=i
   20 CONTINUE
c
      RETURN
      END
