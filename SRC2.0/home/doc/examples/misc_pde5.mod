// PDE blood-tissue exchange

import nsrunit;
unit conversion on;

math pde_example5 {
  realDomain t sec; t.min=0; t.max=30; t.delta=0.3;
  realDomain x cm; real L, Ngrid; x.min=0; x.max=L; x.ct=Ngrid;
  private x.ct;

  real Fp    = 1     ml/(min*g),     
       Vp    = 0.07  ml/g,
       Wp    = 0.94,
       Dp    = 0     cm^2/sec,
       Vmax  = 0.1   mmol/(sec*g),
       Km    = 0.01  Molar,
       Visfp = 0.35  ml/g,
       Disf  = 0     cm^2/sec,
       L     = 0.1   cm,
       Ngrid  = 21;

  extern real C1in(t) Molar;

  real C1(t,x)   Molar, 
       C2(t,x)   Molar,
       C1out(t)  Molar,
       PSg1(t,x) ml/(min*g), 
       PSg2(t,x) ml/(min*g);

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

  PSg1 = Vmax/(C1+Km);
  PSg2 = Vmax/(C2+Km);
  C1:t = -Fp*L/Vp*C1:x - (PSg1*C1/Wp- PSg2*C2)/Vp + Dp*C1:x:x;
  C2:t = (PSg1*C1/Wp-PSg2*C2)/Visfp + Disf*C2:x:x;
}

//
// This is a nonlinear, constant-parameter PDE model:
//
//             _________________
//             |               |
// Fp          |Vp,Wp     C1(t)|
// Cin(t) ---> |               |---> Cout(t)
//             |               |
//             |PSg2 ^  |      |
//             ------|--|-------
//             |     |  v PSg1 |
//             |               |
//             |               |
//             |          C2(t)|                    
//             |Visfp          |
//             _________________
//
//             |--> x
//             |<------------->|
//                     L
//
// PDEs:
//    dC1(t,x)/dt = -Fp*L/Vp*dC1(t,x)/dx - (PSg1*C1/Wp-PSg2*C2)/Vp 
//                + Dp*d^2(C1)/dx^2
//    dC2(t,x)/dt = (PSg1*C1/Wp-PSg2*C2)/Visfp + Disf*d^2(C1)/dx^2
//
//    where PSg1 = Vmax/(Km+C1) and PSg2 = Vmax/(Km+C2)
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
//    PSg1  - Permeability-surface area product from plasma to ISF, ml/sec/g
//    PSg2  - Permeability-surface area product from ISF to plasma, ml/sec/g
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
