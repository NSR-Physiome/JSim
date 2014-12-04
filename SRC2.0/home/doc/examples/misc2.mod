// Michaelis-Menten transport with non-linear PSg

math example2 {                 

  realDomain t; 
  t.min   = 0; 
  t.max   = 30; 
  t.delta = 0.1;

  real  Fp=0.016666667, 
        Vp=0.07, 
        Wp=0.94, 
        PSg1(t), 
        PSg2(t), 
        Visfp=0.35,	
        Km=1,
        Vmax=1;

  extern real Cin(t);

  real C1(t), C2(t);

  when(t=t.min) {
    C1 = 0;
    C2 = 0;
  }

  PSg1 = Vmax/(C1+Km);
  PSg2 = Vmax/(C2+Km);
  C1:t = Fp/Vp*(Cin-C1) - PSg1/Vp*(C1/Wp) + PSg2/Vp*C2;
  C2:t = PSg1/Visfp*(C1/Wp) - PSg2/Visfp*C2;
}

//
// This is a nonlinear, constant-parameter, two-region model: 
// 
//            _________________
// Fp         |Vp,Wp   C1(t)  |
// Cin(t) --->|               |---> C1(t)
//            |  PSg1      ^  |
//            ---|---------|---
//            |  v       PSg2 |
//            |               |
//            |Visfp   C2(t)  |
//            -----------------
// ODEs:
//    dC1(t)/dt = Fp/Vp*(Cin-C1) - PSg/Vp*(C1/Wp-C2)
//    dC2(t)/dt = PSg/Visfp*(C1/Wp-C2)
// Algebraic Equations:
//    PSg1 = Vmax/(C1+Km)
//    PSg2 = Vmax/(C2+Km)
//
// I.C.: C1(t)|t=0 = 0
//       C2(t)|t=0 = 0
//
// Model Parameters:
//    Fp    - Flow, 0.01667 ml/sec/g
//    Vp    - Plasma volume, 0.07 ml/g
//    Wp    - Water content for plasma, 0.94, unitless
//    Vp*Wp - Volume of distribution of plasma region, ml/g
//    Visfp - Volume of distribution of ISF region, 0.35 ml/g
//    Vmax  - Maximal transport flux, mmol/sec/g
//    Km    - Michaelis-Menten constant, mmol/ml (Molar)
//
//    time unit for ODE solutions - second
//
// Model input function:
//    Cin - inflow concentration, mmol/ml of plasma 
//          (external and provided by XSIM)
//
//          Note: The model is implemented such that the input function
//                is taken as constant value for each time step t to t+dt.
//                However, C1 and C2 are the concentrations at the end of 
//                the time step.
//
// Model output function:
//    C1   - Concentration in plasma (Vp), mmol/ml of plasma
//    C2   - Concentration in ISF (Visfp), mmol/ml of volume of distribution 
//           in interstitial region.
//    PSg1 - Permeability-surface area product from plasma to ISF, ml/sec/g
//    PSg2 - Permeability-surface area product from ISF to plasma, ml/sec/g
