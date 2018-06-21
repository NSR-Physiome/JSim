// MODEL NUMBER: 0382
// MODEL NAME: Osm.CoupledKK.1sol
// SHORT DESCRIPTION: Transport across a membrane between 2 stirred tanks, V1 and V2 of solute 1 and solvent water. Water fluxes 
// induce volume changes and pressure changes. Solute 1 can also permeate the membrane independently of the pore, Pmemb11, 
// and so can water PmembW. This uses linearized thermodynamics of irreversible processes from Kedem and Katchalsky 1958. 
// TEST MML comments export to SBML.

 import nsrunit; unit conversion on;

 math  Osm.CoupledKK1sol{
  realDomain t sec; t.min=0; t.max=100; t.delta=0.1;
  
  // PARAMETERS:
  real Pmemb1 = 0 cm/s,                    // solute 1 could permeate membrane independent of aqueous pore
       Ppore1 = 1e-5 cm/s,                 // solute 1 can permeate aqueous pore
       PmembW = 0 cm/(s*mmHg),             // Water could permeate membrane independent of aqueous pore
       Am    = 1 cm^2,                     // Surface area of membrane
       sig1  = 0.9,                        // reflection coeff for solute 1, independent of direction
       Lp    = 0.02 cm/(s*mmHg),           // Hydraulic conductivity of porous memb for water (solvent)
           // which translates to Pf,cm/s, by multiplying Lp by RT/Vw = 1.02*1e6 mmHg at 20 degC
	   // so Pf =2.031e4 cm/s. 
	   // Mlekody, Moore Levitt JGP 81: 213, 1983 give Pf = 2e-2 cm/s  at 23C in RBC.
           // Here Pf = 2.15*1e4 cm/s @37C. Note the the surface to volume ratio here is 1 cm^2 / 1 ml
           // whereas that for RBC is much higher
       C11init = 2 mM,                        // Init concn of 1  on side 1
       C12init = 1 mM,                        // Init concn of 1  on side 2
 	V1init = 1 mL,                        // Init volume of side 1
	V2init = 1 mL,                        // Init volume of side 2
	p1zero = 0 mmHg,                      // Init pressure of side 1
	p2zero = 0 mmHg,                      // Init pressure of side 2
      Q1tot = C11init*V1init + C12init*V2init,// conservation check
      TempC = 37 degK,                        // deg Centigrade
     // R     = 0.062363 mmHg/(mM*degK),      // Gas Constant in mmHg/(mM*degK)
     // T     = 273.16 + TempC,               // T degK - convert to degK
      RT = 19.3425*10^6  mmHg*cm^3/mol,       // RT = 19.3425*10^6 mmHg*cm^3*mol^(-1) at 37C
      Base1 = 0.001 cm^2,             // area of base of volume 1, to calc p1.
      Base2 = 0.001 cm^2,             // area of base of volume 2, to calc p2.
      rho   = 1.0 g/ml,               // water density -> cm H2O height to mmHg (rhoHg = 13.59508 g/ml)
      grav  = 980 cm/sec^2;           // press p = rho*grav*h = rho*grav*VolumeChange/Base 
 
  real C11(t) mM,   C12(t) mM,               // Concn, Cij of solute i and side j 
       Q11(t) umol, Q12(t) umol,             // Quantities Qij of solute i and side j       
       V1(t)  mL,   V2(t) mL,                // Volumes on sides 1 and 2
       p1(t) mmHg,  p2(t) mmHg,              // Hydrostatic pressures on sides 1 and 2
                
       Jv(t) cm/s,                           // Total Volume flux from V1 to V2, per unit area of membrane
       Jvpore(t) cm/s,                       // Volume flux via pore from V1 to V2, per unit area of membrane
       Jvmemb(t) cm/s,                       // Volume flux via memb from V1 to V2, per unit area of membrane      
       Jc1(t)     umol/(cm^2 * sec),         // Solute 1 total flux from 1 to 2 
       Jc1memb(t) umol/(cm^2 * sec),         // Solute 1 flux from 1 to 2 via transmemb diffusion
       Jc1conv(t) umol/(cm^2 * sec),         // Solute 1 flux from 1 to 2 via convection with Jv
       Jc1diff(t) umol/(cm^2 * sec);         // Solute 1 flux from 1 to 2 via intrapore diffusion                             // (Cannot use Js in JSim because it is used in the Java)
                                                        
  real OP11(t) mmHg, OP12(t) mmHg,           // dOP(1,j) for sol 1, side j
       dOP1(t) mmHg, dp(t) mmHg;             // OP diffs for solute 1,  and hydrostatic press diff, dp

// INITIAL CONDITIONS:
   when (t=t.min) { V1 = V1init;  V2 = V2init;
        Q11 = C11init * V1init;    Q12 = C12init * V2init; }
  
 real avgC1(t) mM;                           // average concn solute 1 and solute 2 within pore
  avgC1 = (C11 + C12)/2;                     // linear avg solute 1, but could take logs

  Jvpore = Lp * (dp - sig1*dOP1);            // water flux, V1 to V2 via pore  Eq 10-21 of Katchalsky and Curran
  Jvmemb = - PmembW*(OP11 - OP12);           // water flux, V1 to V2 via memb permeation, parallel path to pore
  Jv     = Jvpore + Jvmemb;
  Jc1    = Pmemb1*dOP1/RT + avgC1*(1-sig1)*Jvpore + Ppore1*(1-sig1)*(C11-C12);  // solute 1 flux
  Jc1memb= Pmemb1*dOP1/RT;
  Jc1conv= avgC1*(1-sig1)*Jvpore;
  Jc1diff= Ppore1*(1-sig1)*(C11-C12);


// ODEs:  volume and solute flux equations: fluxes are from 1 to 2
  V1:t  = -Jv * Am;
  V2:t  = Jv * Am;          // assumes conservation, and negligible solute volume  
 
  Q11:t = - Am*Jc1;   C11 = Q11/V1;   OP11 = sig1*RT*C11; // osmotic pressures due to solute 1 in V1               
  Q12:t = Am*Jc1;     C12 = Q12/V2;   OP12 = sig1*RT*C12; // osmotic pressures due to solute 1 in V2 
  dOP1 = OP11 - OP12;       // Osmotic pressure difference 
   
// Assume P-V relationship, put V1 and V2 in columns of water over an area Base1 or Base2
  p1 = p1zero + rho*grav * (V1-V1init)/(Base1);  // p1 is pressure in mmHg due to height 1
  p2 = p2zero + rho*grav * (V2-V2init)/(Base2);  // p2 is pressure in mmHg due to height 2 
  dp   = p1 - p2;                                // hydrostatic pressure diff

// Normalized variables for plots:
real V1p(t), V2p(t), C11p(t), C12p(t), Q1tp(t); //C21p(t),C22p(t),Q2tp(t);
  V1p = V1/V1init; 
  V2p = V2/V2init;
  C11p= C11/C11init;
  C12p= C12/C12init;
  Q1tp = (Q11+Q12)/Q1tot;   // Conservation check     
}
/* 
 DETAILED DESCRIPTION:  
 Model for coupled fluxes of water and one solute through an aqueous channel, with permability
 Ppore via the pore (aqueous channel). Diffusion with or against water flow affects solute flux. 
 Solute diffusion is retarded by friction with channel wall, by a fraction 1-sigma.
 Permeability Ppore cm/s = Dw * AreaPore* (1-sigma) / (PoreLength *AreaMemb), where
 Dw is the free diffusion coeff in water. Sigma (approx) = 1- (1 - rs/rp)^2, but in this same project
 file is another program, PoreTransp.P.Sig, which can be use to set P and sig1 in accord with the
 hydrodynamics of a right cylindrical pore.

 COPYRIGHT AND REQUEST FOR ACKNOWLEDGMENT OF USE:   
  Copyright (C) 1999-2016 University of Washington. From the National Simulation Resource,  
  Director J. B. Bassingthwaighte, Department of Bioengineering, University of Washington, Seattle WA 98195-5061. 
  Academic use is unrestricted. Software may be copied so long as this copyright notice is included.

  When citing JSim please use this reference: Butterworth E, Jardine BE, Raymond GM, Neal ML, Bassingthwaighte JB. 
  JSim, an open-source modeling system for data analysis [v3; ref status: indexed, http://f1000r.es/3n0] 
  F1000Research 2014, 2:288 (doi: 10.12688/f1000research.2-288.v3)  

  This software was developed with support from NIH grants HL088516 and HL073598, NIBIB grant BE08417,
  the Cardiac Energy Grid HL199122 (PI: J.B. Bassingthwaighte), and the Virtual Physiological Rat program 
  GM094503 (PI: D.A.Beard). Please cite these grants in any publication for which this software is used and 
  send an email with the citation and, if possible, a PDF file of the paper to: staff@physiome.org. 


*/

