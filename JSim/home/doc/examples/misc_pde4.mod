// PDE blood-tissue exchange

math pde_example4 {
  realDomain t; t.min=0; t.max=30; t.delta=0.3;
  realDomain x; real L, Ngrid; x.min=0; x.max=L; x.ct=Ngrid;    
  private x.ct;

  real Fp=0.016666667,     // flow in mL/s/g
       Vp=0.07,            // volume in mL/g
       Wp=0.94,            // water fraction, unitless
       Dp=0,               // axial dispersion in cm^2/s
       a=1,
       b=1,
       PSg(x)=if (x>L/2) a else b,
       Visfp=0.35,         // volume in mL/g
       Disf=0,             // axial dispersion in cm^2/s
       Ngrid=5,
       L=0.1;

  extern real C1in(t);

  real C1(t,x), C2(t,x);
  real C1out(t);

  // B.C.
  when  (x=x.min) {
    C1 = C1in;
    C2:x = 0;
  }
  when (x=x.max) {
    C1:x = 0;
    C2:x = 0;
    C1out = C1;
  }

  // I.C.
  when (t=t.min) {
    C1 = if (x=x.min) C1in else 0;
    C2 = 0;
  }

  C1:t = -Fp*L/Vp*C1:x - PSg/Vp*(C1/Wp-C2) + Dp*C1:x:x;
  C2:t = PSg/Visfp*(C1/Wp-C2) + Disf*C2:x:x;
}

//
// This is a linear, constant-parameter model:
//
//             _________________
// Fp          |Vp,Wp     C1(t)|
// Cin(t) ---> |               |---> Cout(t)
//             |       ^ PSg   |
//             --------|--------
//             |       v       |
//             |          C2(t)|                    
//             |Visfp          |
//             _________________
//
//             |--> x
//             |<------------->|
//                     L
//
// PDEs:
//    dC1(t,x)/dt = -Fp*L/Vp*dC1(t,x)/dx - PSg/Vp*(C1/Wp-C2) 
//                + Dp*d^2(C1)/dx^2
//    dC2(t,x)/dt = PSg/Visfp*(C1/Wp-C2) + Disf*d^2(C1)/dx^2
//
// B.C.: C1(t,x)|x=0 = Cin(t)
//       C2(t,x)|x=0 = 0
//
//       dC1(t,x)/dx|x=0 = 0
//       dC2(t,x)/dx|x=0 = 0
//       dC1(t,x)/dx|x=L = 0
//       dC2(t,x)/dx|x=L = 0
//
// I.C.: C1(t,x)|t=0 = 0
//       C2(t,x)|t=0 = 0
//
// Model Parameters:
//    Fp    - Flow, 0.01667 ml/sec/g
//    Vp    - Plasma volume, 0.07 ml/g
//    Wp    - Water content for plasma, 0.94, unitless
//    Vp*Wp - Volume of distribution of plasma region, ml/g
//    Visfp - Volume of distribution of ISF region, 0.35 ml/g
//    PSg   - Permeability-surface area product between plasma to ISF, ml/sec/g
//
//    time unit for PDE solutions - second
//
// Model input function:
//    C1in - inflow concentration, mmol/ml of plasma
//           (external and provided by XSIM)
//
//          Note: The model is implemented such that the input function
//                is taken as constant value for each time step t to t+dt.
//                However, C1 and C2 are the concentrations at the end of
//                the time step.
//
// Model output function:
//    C1   - Concentration in plasma (Vp1), mmol/ml of plasma
//    C2   - Concentration in ISF (Visfp1), mmol/ml of volume of distribution
//           in interstitial region.
//    Cout - outflow concentration, mmol/ml of plasma
