// PDE blood-tissue exchange

math pde_example3 {
  realDomain t; t.min=0; t.max=30; t.delta=0.3;
  realDomain x; real L, Ngrid; x.min=0; x.max=L; x.ct=Ngrid;
  realDomain x2; real L2, Ngrid2; x2.min=0; x2.max=L; x2.ct=Ngrid2;
  private x.ct;
  private x2.ct;

  real L=0.1,               // capillary length in cm
       Ngrid=21,
       L2=0.1,              // capillary length in cm
       Ngrid2=21,
		
       Fp1=0.016666667,     // flow in mL/s/g
       Vp1=0.07,            // volume in mL/g
       Wp1=0.94,            // water fraction, unitless
       Dp1=0,               // axial dispersion in cm^2/s
       PSg1=0.05,           // PS product in mL/s/g
       Visfp1=0.35,         // volume in mL/g
       Disf1=0,             // axial dispersion in cm^2/s

       Fp2=-0.016666667,    // flow in mL/s/g
       Vp2=0.07,            // volume in mL/g
       Wp2=0.94,            // water fraction, unitless
       Dp2=0,               // axial dispersion in cm^2/s
       PSg2=0.05,           // PS product in mL/s/g
       Visfp2=0.35,         // volume in mL/g
       Disf2=0;             // axial dispersion in cm^2/s

  extern real C1in(t), C3in(t);

  real C1(t,x), C2(t,x), C1out(t);
  real C3(t,x2), C4(t,x2), C3out(t);

  // B.C.
  when (x=x.min) {
    C1 = C1in;
    C2:x = 0;
  }
  when (x=x.max) {
    C1:x = 0;
    C2:x = 0;
    C1out = C1;
  }
  when (x2=x2.min) {
    C3:x2 = 0;
    C4:x2 = 0;
    C3out = C3;
  }
  when (x2=x2.max) {
    C3 = C1out;
    C4:x2 = 0;
  }

  // I.C.
  when (t=t.min) {
    C1 = if (x=x.min) C1in else 0;
    C2 = 0;
    C3 = if (x2=x2.max) C1out else 0;
    C4 = 0;
  }

  C1:t = -Fp1*L/Vp1*C1:x - PSg1/Vp1*(C1/Wp1-C2) + Dp1*C1:x:x;
  C2:t = PSg1/Visfp1*(C1/Wp1-C2) + Disf1*C2:x:x;
  C3:t = -Fp2*L/Vp2*C3:x2 - PSg2/Vp2*(C3/Wp2-C4) + Dp2*C3:x2:x2;
  C4:t = PSg2/Visfp2*(C3/Wp2-C4) + Disf2*C4:x2:x2;
}

//
// This is a linear, constant-parameter model with 2 units in series:
//
//             _________________
// Fp1         |Vp1,Wp1   C1(t)|
// C1in(t) --->|               |-----> C1(t)
//             |       ^ PSg1  |     |
//             --------|--------     |
//             |       v       |     |
//             |          C2(t)|     |           
//             |Visfp1         |     |
//             |               |     |
//             -----------------     |          
//             |--> x                |
//             |<------------->|     |
//                     L             |
//                                   |
//                                   |
//             -----------------     |
//             |               |     |
//             |               |     |
//             |Visfp2  C4(t)  |     |
//             |               |     |
//             |       ^ PSg2  |     |
//             --------|--------     |
//             |       v       |     |
// C3out(t)<---|               |<----- C3in(t)
//             |Vp2,Wp2   C3(t)|       Fp2
//             _________________
//
//             |--> x2
//             |<------------->|
//                     L
//
// PDEs:
//    dC1(t,x)/dt = -Fp1*L/Vp1*dC1(t,x)/dx - PSg1/Vp1*(C1/Wp1-C2) 
//                + Dp1*d^2(C1)/dx^2
//    dC2(t,x)/dt = PSg1/Visfp1*(C1/Wp1-C2) + Disf1*d^2(C1)/dx^2
//    dC3(t,x)/dt = -Fp2*L/Vp2*dC3(t,x)/dx - PSg2/Vp2*(C1/Wp2-C2)
//                + Dp2*d^2(C3)/dx^2
//    dC4(t,x)/dt = PSg2/Visfp2*(C1/Wp2-C2) + Disf2*d^2(C3)/dx^2
//
// B.C.: C1(t,x)|x=0 = C1in(t)
//       C2(t,x)|x=0 = 0
//       C3(t,x)|x=L = C3in(t)
//       C4(t,x)|x=L = 0
//
//       dC1(t,x)/dx|x=0 = 0
//       dC2(t,x)/dx|x=0 = 0
//       dC3(t,x)/dx|x=0 = 0
//       dC4(t,x)/dx|x=0 = 0
//       dC1(t,x)/dx|x=L = 0
//       dC2(t,x)/dx|x=L = 0
//       dC3(t,x)/dx|x=L = 0
//       dC4(t,x)/dx|x=L = 0
//
// I.C.: C1(t,x)|t=0 = 0
//       C2(t,x)|t=0 = 0
//       C3(t,x)|t=0 = 0
//       C4(t,x)|t=0 = 0
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
//    C3in - inflow concentration, mmol/ml of plasma
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
//    C3   - Concentration in plasma (Vp2), mmol/ml of plasma
//    C4   - Concentration in ISF (Visfp2), mmol/ml of volume of distribution
//           in interstitial region.
//    C1out - outflow concentration, mmol/ml of plasma
//    C3out - inflow concentration, mmol/ml of plasma
