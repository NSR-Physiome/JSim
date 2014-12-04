// conditional equations for PSg
math example4 {                 
  realDomain t; 
  t.min=0; 
  t.max=30; 
  t.delta=0.1;

  real  Fp=0.016666667,  // static variables
        Vp=0.07, 
        Wp=0.94, 
        flux1(t), 
        flux2(t), 
        Visfp=0.35,
        Km=0.0001,
        Vmax=0.00001;

  extern real Cin(t);	// externally calculated variable	

  real C1(t), C2(t);	// concentrations in 2 regions
		
  // initial conditions
  when(t=t.min) {
    C1 = 0;
    C2 = 0;
  }

  // ODEs and algebraic eqns
  flux1 = if (C1<10*Km) Vmax/(Km+C1)*C1/Wp    // Michaelis-Menten transport
          else Vmax;                          // zero-order transport
  flux2 = if (C2<10*Km) Vmax/(Km+C2)*C2
          else Vmax;
  C1:t  = Fp/Vp*(Cin-C1) - flux1/Vp + flux2/Vp;
  C2:t  = flux1/Visfp - flux2/Visfp;
}

//
// This is a conditional nonlinear, constant-parameter, two-region model: 
// 
//            _________________
// Fp         |Vp,Wp   C1(t)  |
// Cin(t) --->|               |---> C1(t)
//            |  flux1     ^  |
//            ---|---------|---
//            |  v      flux2 |
//            |               |
//            |Visfp   C2(t)  |
//            -----------------
//
// ODEs:
//    dC1(t)/dt = Fp/Vp*(Cin-C1) - PSg/Vp*(C1/Wp-C2)
//    dC2(t)/dt = PSg/Visfp*(C1/Wp-C2)
// Algebraic Equations:
//    flux1 = Vmax,             if C1 >= Km  (constant transport flux)
//          = Vmax/Km*(C1/Wp),  if C1 <  Km  (linear transport)
//    flux2 = Vmax,             if C2 >= Km  (constant transport flux)
//          = Vmax/Km*C2,       if C2 <  Km  (linear transport)
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
//    C1 - Concentration in plasma (Vp), mmol/ml of plasma
//    C2 - Concentration in ISF (Visfp), mmol/ml of volume of distribution 
//         in interstitial region.
//    flux1 - transport flux from plasma to ISF, mmol/sec/g
//    flux2 - transport flux from ISF to plasma, mmol/sec/g
