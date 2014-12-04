import nsrunit;
unit conversion on;

math BesselErf {

/* This programs tests besseli0(x), besseli1(x), erf(x), erfc(x),
   besseljn(n,x), besselkn(n,x), besselyn(n,x) from the
   cern colt library. The Bessel classes are found in
   colt/src/cern/jet/math/Bessel.java and the erf and erfc
   classes are found in colt/src/jet/stat/Probability.java

   There are tested with the value of b = x^3 = 1.331, and also
   with  the derivatives. n=3 is used for BesselJn, BesselKn,
   and BesselYn. Answers may be checked with Maple (TM) program
   in comments at end.
*/

// INDEPENDENT VARIABLES
real eps = 0.0001;
realDomain x; x.min=1.1; x.max=x.min+eps; x.delta = eps;
real dx=x.delta;

real b(x)=x*x*x; //Necessary do test derivatives
real n = 3;      //Bessel order for besseljn, besselkn, and besselyn

//test functions
real i0(x), i1(x), jn(x), kn(x), yn(x), er(x), ec(x);

//derivatives
real di0(x), di1(x), djn(x), dkn(x), dyn(x), der(x), dec(x);


// calculations
i0  = besseli0(b); 
di0 = besseli0(b):x;


i1  = besseli1(b); 
di1 = besseli1(b):x;


jn  = besseljn(n,b); 
djn = besseljn(n,b):x;

kn  = besselkn(n,b); 
dkn = besselkn(n,b):x;

yn  = besselyn(n,b); 
dyn = besselyn(n,b):x;

er  = erf(b); 
der = erf(b):x;


ec  = erfc(b); 
dec = erfc(b):x;

/* CHECK ANSWERS WITH Maple(TM)
x:=t^3;
n:=3;
besseli0:=BesselI(0,x);
besseli1:=BesselI(1,x);
besseljn:=BesselJ(n,x);
besselkn:=BesselK(n,x);
besselyn:=BesselY(n,x);
ERF:=erf(x);
ERFC:=erfc(x);
db_i_0dt:=diff(besseli0,t);
db_i_1dt:=diff(besseli1,t);
db_j_ndt:=diff(besseljn,t);
db_k_ndt:=diff(besselkn,t);
db_y_ndt:=diff(besselyn,t);
dERFdt:=diff(ERF,t);
dERFCdt:=diff(ERFC,t);
t:=1.1;
evalf(besseli0);
evalf(besseli1);
evalf(besseljn);
evalf(besselkn);
evalf(besselyn);
evalf(ERF);
evalf(ERFC);
evalf(db_i_0dt);
evalf(db_i_1dt);
evalf(db_j_ndt);
evalf(db_k_ndt);
evalf(db_y_ndt);
evalf(dERFdt);
evalf(dERFCdt);

evalf(besseli0);   1.494409348
evalf(besseli1);   0.8241604938
evalf(besseljn);   0.04391984410
evalf(besselkn);   2.763274609
evalf(besselyn);  -2.762988740
evalf(ERF);        0.9402071821

evalf(ERFC);       0.05979281785
evalf(db_i_0dt);   2.991702592
evalf(db_i_1dt);   3.176995497
evalf(db_j_ndt);   0.3322091799
evalf(db_k_ndt); -25.51689483
evalf(db_y_ndt);  18.63270547

evalf(dERFdt);     0.6965984718
evalf(dERFCdt);   -0.6965984718
*/


}


