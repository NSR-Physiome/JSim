JSim v1.1

import  nsrunit;
unit conversion on;

math btex50_pde {

realDomain t sec ; t.min=0; t.max=30; t.delta=0.1;
realDomain x cm; real L=0.1 cm, Ngrid=41; x.min=0; x.max=L; x.ct=Ngrid;    
private x.min, x.max, x.ct;

/* P    = PLASMA 
   ISF  = INTERSTITIAL FLUID REGION
   EC   = ENDOTHELIAL CELL
   PC   = PARENCHYMAL CELL 
   MITO = MITOCHONDRIA */

real Fp= 1 ml/(g*min);     // Plasma flow

real Vp=0.05 ml/g;         // Plasma volume  
real Visfp = 0.15 ml/g;    // ISF volume of distribution
real Vecp = 0.02 ml/g;     // EC volume of distribution
real Vpcp = 0.6 ml/g;      // PC volume of distribution
real Vmitop = 0.01 ml/g;   // MITO volume of distribution

// the hVolumes protect against zero divides
private real hVp       =if(Vp>0)     Vp     else (1e-6 ml/g);
private real hVisfp    =if(Visfp>0)  Visfp  else (1e-6 ml/g);
private real hVecp     =if(Vecp>0)   Vecp   else (1e-6 ml/g);
private real hVpcp     =if(Vpcp>0)   Vpcp   else (1e-6 ml/g);
private real hVmitop   =if(Vmitop>0) Vmitop else (1e-6 ml/g);


real PSg=1 ml/(g*min);     // Permeability-surface area product between
                           //    plasma and interstitial fluid region (ISF)
real PSecl=1 ml/(g*min);   // Permeability-surface area product exchange
                           //    coefficient between EC and plasma
real PSeca=1 ml/(g*min);   // Permeability-surface area product exchange
                           //    coefficient between EC and ISF   
real PSpc = 3 ml/(g*min);  // Permeability-surface area product exchange
                           //    coefficient between ISF and PC
real PSmito= 1 ml/(g*min); // Permeability-surface area product exchange
                           //    coefficient between PC and MITO
                           
real Gp=0 ml/(g*min);      // Plasma consumption rate for metabolite 
real Gisf  = 0 ml/(g*min); // ISF consumption rate for metabolite
real Gec  = 0.0 ml/(g*min);// EC consumption rate for metabolite
real Gpc  = 0.0 ml/(g*min);// PC consumption rate for metabolite
real Gmito= 1.0 ml/(g*min);// MITO consumption rate for metabolite

real Dp=1.0e-05 cm^2/sec;        // Plasma axial diffusion coefficient
real Disf  = 0.0 cm^2/sec; // ISF axial diffusion coefficient
real Dec  = 0.0 cm^2/sec;  // EC axial diffusion coefficient
real Dpc  = 0.0 cm^2/sec;  // PC axial diffusion coefficient
real Dmito  = 0.0 cm^2/sec;// MITO axial diffusion coefficient

real Cin(t) mmol/ml;
Cin = exp(-t/(1 sec)) * (1 mmol/ml);

real Cp(t,x) mmol/ml;
real Cisf(t,x) mmol/ml;
real Cec(t,x) mmol/ml;
real Cpc(t,x) mmol/ml;
real Cmito(t,x) mmol/ml;
real Cout(t) mmol/ml;

// Boundary Conditions
when  (x=x.min) { Cp = Cin; }
when  (x=x.max) { Cp:x = 0; Cout = Cp; }
when  (x=x.min) { Cisf:x = 0;}
when  (x=x.max) { Cisf:x = 0;}
when  (x=x.min) { Cec:x = 0;}
when  (x=x.max) { Cec:x = 0;}
when  (x=x.min) { Cpc:x = 0;}
when  (x=x.max) { Cpc:x = 0;}
when  (x=x.min) { Cmito:x = 0;}
when  (x=x.max) { Cmito:x = 0;}

// I.C.
when (t=t.min) { Cp   = if(x=x.min) Cin else 0; }
when (t=t.min) { Cisf = 0; }
when (t=t.min) { Cec  = 0; }
when (t=t.min) { Cpc  = 0; }
when (t=t.min) { Cmito  = 0; }

// Partial differential equation
Cp:t   = -Fp*L/hVp*Cp:x -Gp/hVp*Cp+ Dp*Cp:x:x 
        + PSg/hVp*(Cisf-Cp) + PSecl/hVp*(Cec-Cp);
Cisf:t =               -Gisf/hVisfp*Cisf + Disf*Cisf:x:x 
        + PSg/hVisfp*(Cp-Cisf) + PSeca/hVisfp*(Cec-Cisf) 
        + PSpc/hVisfp*(Cpc-Cisf);
Cec:t  =               -Gec/hVecp*Cec +Dec*Cec:x:x
        +PSecl/hVecp*(Cp-Cec) +PSeca/hVecp*(Cisf-Cec);
Cpc:t  =               -Gpc/hVpcp*Cpc +Dpc*Cpc:x:x
        + PSpc/hVpcp*(Cisf-Cpc) + PSmito/hVpcp*(Cmito-Cpc);
Cmito:t  =             -Gmito/hVmitop*Cmito +Dmito*Cmito:x:x
        + PSmito/hVmitop*(Cpc-Cmito);
}
/*
 Fp          ________________________________________
 Cin(t) ---> |Vp                             Cp(t,x)|---> Cout(t)
             |Gp    ^                               |              
             |Dp    |                         PLASMA|
             ______PSecl_____   ^  _________________|
             |Vecp  |        \  |  \        Cec(t,x)|
             |Gec   V    ^    \ PSg \    ENDOTHELIAL|
             |Dec        |     \  |  \          CELL|
             __________PSeca____\ v   \_____________|
             |Visfp      |                 Cisf(t,x)|
             |Gisf       V            ^ INTERSTITIAL|
             |Disf                    | FLUID REGION|
             _______________________Pspc_____________
             |Vpcp                    |     Cpc(t,x)|
             |Gpc            ^        V  PARENCHYMAL|
             |Dpc            |                  CELL|
             |  ----------PSmito-----------------   |
             |  |Vmitop      |          Cmito(t)|   |
             |  |Gmito       V                  |   |
             |  |Dmito              MITOCHONDRIA|   |
             |  ---------------------------------   |
             ________________________________________
             |<----------------L------------------->|
             |--> x
            
 Fp : Plasma Flow Rate, (ml/g)/min
 Vp : Plasma Volume,  ml/g 
 Vecp, Visfp, Vpcp, Vmitop: Volumes of Distribution,  ml/g  
 PSg, Psecl, Pseca, Pspc, PSmito: Permeability-surface area product exchange
                          coefficients, (ml/g)/min
 Gp, Gec, Gisf, Gpc, Gmito: Consumption rates for metabolite, (ml/g)/min
 Dp, Disf, Dec, Dpc, Dmito: Axial Diffusion Rate, cm^2/sec
 Cin: Plasma metabolite inflow, mmol/ml
 Cout: Plasma metabolite outflow, mmol/ml
 Cp, Cec, Cisf, Cpc, Cmito: metabolite concentration, mmol/ml
*/
