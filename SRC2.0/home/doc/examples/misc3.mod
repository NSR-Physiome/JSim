// time-varying parameter, Fp, with constant infusion

math example3 {               
  realDomain t; 
  t.min=0; 
  t.max=30; 
  t.delta=0.1;

  real Vp=0.035, 
       Wp=0.94, 
       PSg=0.1, 
       Visfp=0.15;	

  extern real Fp(t), Qin(t);

  real C1(t), C2(t), Cin(t);
  real Qfick(t), Q(t);

  when(t=t.min) {
    C1 = 0;
    C2 = 0;
    Qfick = C1*Vp + C2*Visfp;
  }

  Cin  = Qin / Fp;
  C1:t = Fp/Vp*(Cin-C1) - PSg/Vp*(C1/Wp-C2);
  C2:t = PSg/Visfp*(C1/Wp-C2);
  Qfick:t = Fp*(Cin-C1);
  Q = C1*Vp + C2*Visfp;
}

//
// This is a time-varying, two-region model: 
// 
//            _________________
// Fp(t)      |Vp,Wp          |
// Cin(t) --->|       C1(t)   |---> C1(t)
//            |  ^PSg         |
//            ---|-------------
//            |  v            |
//            |       C2(t)   |
//            |Visfp          |
//            -----------------
// ODEs:
//    dC1(t)/dt = Fp/Vp*(Cin-C1) - PSg/Vp*(C1/Wp-C2)
//    dC2(t)/dt = PSg/Visfp*(C1/Wp-C2)
//
// I.C.: C1(t)|t=0 = 0
//       C2(t)|t=0 = 0
//
// Model Parameters:
//    Vp    - Plasma volume, 0.07 ml/g
//    Wp    - Water content for plasma, 0.94, unitless
//    Vp*Wp - Volume of distribution of plasma region, ml/g
//    Visfp - Volume of distribution of ISF region, 0.35 ml/g
//    PSg   - Permeability-surface area product between plasma and ISF,
//            0.05 ml/sec/g
//
//    time unit for ODE solutions - second
//
// Model input function:
//    Fp  - Flow, a function of time, ml/sec/g
//          (external and provided by XSIM), 
//    Qin - inflow flux, mmol/s of plasma 
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
//    Qfick - Amount in the exchange unit calculated by Fick principle:
//            Qfick = Fp * (Cin - C1)
//    Q     - Amount in the exchange unit calculated by adding the amount
//            in plasma and in ISF: Q = C1*Vp + C2*Visfp
