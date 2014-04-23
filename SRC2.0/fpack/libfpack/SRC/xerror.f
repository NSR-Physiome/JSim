      subroutine xerror(messg,nmessg,nerr,level)
c***begin prologue  xerror
c***date written   790801   (yymmdd)
c***revision date  851111   (yymmdd)
c***category no.  r3c
c***keywords  error,xerror package
c***author  jones, r. e., (snla)
c***purpose  process an error (diagnostic) message.
c***description
c
c     abstract
c        xerror processes a diagnostic message, in a manner
c        determined by the value of level and the current value
c        of the library error control flag, kontrl.
c        (see subroutine xsetf for details.)
c
c     description of parameters
c      --input--
c        messg - the hollerith message to be processed, containing
c                no more than 72 characters.
c        nmessg- the actual number of characters in messg.
c        nerr  - the error number associated with this message.
c                nerr must not be zero.
c        level - error category.
c                =2 means this is an unconditionally fatal error.
c                =1 means this is a recoverable error.  (i.e., it is
c                   non-fatal if xsetf has been appropriately called.)
c                =0 means this is a warning message only.
c                =-1 means this is a warning message which is to be
c                   printed at most once, regardless of how many
c                   times this call is executed.
c
c     examples
c        call xerror('smooth -- num was zero.',23,1,2)
c        call xerror('integ  -- less than full accuracy achieved.',
c    1                43,2,1)
c        call xerror('rooter -- actual zero of f found before interval f
c    1ully collapsed.',65,3,0)
c        call xerror('exp    -- underflows being set to zero.',39,1,-1)
c
c     written by ron jones, with slatec common math library subcommittee
c***references  jones r.e., kahaner d.k., 'xerror, the slatec error-
c                 handling package', sand82-0800, sandia laboratories,
c                 1982.
c***routines called  xerrwv
c***end prologue  xerror
      character*(*) messg
c***first executable statement  xerror
      call xerrwv(messg,nmessg,nerr,level,0,0,0,0,0.,0.)
      return
      end
