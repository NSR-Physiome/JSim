// Unclear error msg
/*
1:05:45 Job "Compile model OneAlvLung.ExchBody" in progress...
11:05:46 aborted:  (internal?) MainBlock: No tool for ctrach:x

11:05:46 ERROR:  (internal?) MainBlock: No tool for ctrach:x
11:06:17 Project saved to file units_bug_possible.proj

Instrumented:
13:42:47 aborted:  (internal?) MainBlock:buildQueryTools(): No tool for ctrach:x
13:42:47 ERROR:  (internal?) MainBlock:buildQueryTools(): No tool for ctrach:x

Update message to this:
09:34:22 ERROR:  (internal?) MainBlock: Cannot sequence variable/expression: 'ctrach:x'. Please look at dependencies.

*/

import nsrunit; unit conversion on;

math Lung_RC_Air { realDomain t s; t.min=0; t.max=15; t.delta= 0.01;

// PARAMETERS: (Values chosen to approximate an adult human)
real 
 RQ		= 1		dimensionless,
 R		= 62.36		L*mmHg/(mol*K),	// Gas constant
 T		= 310		K,		// Temperature in body
 VFRC		= 3000		ml,		// Functional Residual Capacity (FRC) 
 visc		= 50		1/s,
 ScalPvent	= 10		dimensionless,	// Scalar of amplitude of Pvent (Ventilator)  :: for convenience
 ScalPchest	= 10		dimensionless,	// Scalar of amplitude of Pchest ( +ve to expand Chest)  :: for convenience
 ScalPexhaust	= 10		dimensionless,	// Scalar of amplitude of Pexhaust ( -ve to expand Chest)  :: for convenience
 ComLung0	= 100		ml/mmHg,	// Compliance of the lung, linear
 ComLungslope	= 0.01		dimensionless,	// slope of Compliance of lung times Vlung/VFRC
 ComChest	= 100		ml/mmHg,	// Compliance of Chest
 Res		= 0.01		mmHg*sec/ml,	// Resistance of airway
 Patmos		= 760		mmHg,		// Reference Pressure external to lung
 Vintrapl0	= 100		ml,		// Arbitrary amount air in pleura to allow Vintrapl changes 
 Vchest0	= 4000		ml,		// Chest volume at FRC (incl heart,etc. of about 1000 ml)
 Pintrapl0	= -15		mmHg,		// Intrapleural pressure at FRC
 PO2atmos	= 150		mmHg,		// External partial pressure of O2
 PCO2atmos	= 0		mmHg,		// External partial pressure of CO2
 PH2Oatmos	= 0		mmHg,
 PN2atmos	= Patmos - PO2atmos - PH2Oatmos - PCO2atmos,
 PO2lung0	= 0		mmHg,		// Pressure of O2 in lung at t=0
 PCO2lung0	= 0		mmHg,		// Pressure of CO2 in lung at t=0 
 PH2Olung0	= 0		mmHg,
 PN2lung0	= Patmos - PO2lung0 - PCO2lung0 - PH2Olung0,
 PSO2lung	= 500		ml/min,		// Permeability-surface area for O2 of lung alveolus, passive, symmetric
 PSCO2lung	= 500		ml/min,		// Permeability-surface area for CO2 of lung alveolus, passive, symmetric
 SolyO2		= 1.46e-6	mol/(L*mmHg),	// O2 solubility constant
 SolyCO2	= 3.27e-5	mol/(L*mmHg),	// CO2 solubility constant
 Vpulcap	= 1000		ml,		// Volume of pulmonary capillaries
 Fblood  	= 5000		ml/min,		// Flow is cardiac output, about 5 L/min in human adults
 PO2pulcap0	= 0		mmHg,		// Partial pressure of O2 in lung capilary blood at t = 0.
 PO2tisscap0	= 0		mmHg,		// Partial pressure of O2 in body blood at t = 0.
 PO2tiss0	= 0		mmHg,		// Partial pressure of O2 in body tissue at t = 0.
 PCO2pulcap0	= 0		mmHg,		// Partial pressure of CO2 in lung capilary blood at t = 0.
 PCO2tisscap0	= 0		mmHg,		// Partial pressure of CO2 in body blood at t = 0.
 PCO2tiss0	= 0		mmHg,		// Partial pressure of CO2 in body tissue at t = 0.
 Vtisscap     	= 4000		ml,		// Volume of circulating blood in body
 Vtiss		= 50000		ml,		// Volume of extravascular tissue in body
 PSO2tiss	= 500		ml/min,		// Perm.surf area of O2 for blood tissue exchange
 PSCO2tiss	= 500		ml/min,		// Perm.surf area of CO2 for blood tissue exchange
 Gtiss		= 500		ml/min,		// Consumption, G for Gulosity, in tissue

 Vtrach		= 50		ml,
 Ltrach		= 15		cm;
real Atrach	ml/cm;
 Atrach	= Vtrach/Ltrach;
real PSH2Otrach	= 500		ml/(min*cm),
 PH2Ovapor	= 50		mmHg;
 
realDomain x cm; x.min=0; x.max=Ltrach; x.delta= 0.01;

// VARIABLES

extern real
 Pvent(t)	mmHg,		// Ventilator Driving Press.: 2 sec at 10 mmHg   
 Pchest(t)	mmHg,		// Driving Press. due to chest muscles
 Pexhaust(t)	mmHg;		// Pressure in IronLung tank exhaust (Neg for inhale)

real 
 Pmouth(t)	mmHg,		// Pressure at the mouth
 Plung(t)	mmHg,		// Pressure in the lung, Non-Linear
 Pintrapl(t)	mmHg,		// Pressure in intrapleural space (Neg to inhale)
 Vintrapl(t)	ml,		// Volume of air in intrapleural space (regarded as compressible)
 Vchest(t)	ml,		// Chest volume
 Ptank(t)	mmHg,		// Pressure in iron lung tank
 ComLung(t)	ml/mmHg,	// Linear Compliance and PV curve
 Fair(t)	ml/sec,		// Flow at mouth
 Vlung(t)	ml,		// Volume of air in lung
 nlung(t)	mol,
 nFlung(t)	mol/s,
 nFtrach(t,x)	mol/sec,  // added domains
 //nFtrach	M/sec,    //    < ----- Changed ??????????????
 ctrach(t,x)    M,
 PO2trach(t,x)	mmHg,
 cO2trach(t,x)	M,
 yO2trach(t,x)	dimensionless,
 PCO2trach(t,x)	mmHg,
 cCO2trach(t,x)	M,
 yCO2trach(t,x)	dimensionless,
 PN2trach(t,x)	mmHg,
 cN2trach(t,x)	M,
 yN2trach(t,x)	dimensionless,
 PH2Otrach(t,x)	mmHg,
 cH2Otrach(t,x)	M,
 yH2Otrach(t,x)	dimensionless,
 //Ftrach(t,x)	mmHg,
 Ptrach(t,x)	mmHg,
// ntrach(t,x)	mol/cm,
 PO2lung(t)	mmHg,		// Partial pressure of O2 in lung
 nO2lung(t)	mol,
 PCO2lung(t)	mmHg,		// Partial pressure of O2 in lung
 nCO2lung(t)	mol,
 PN2lung(t)	mmHg,		// Partial pressure of O2 in lung
 nN2lung(t)	mol,
 PH2Olung(t)	mmHg,		// Partial pressure of O2 in lung
 nH2Olung(t)	mol,
 PO2pulcap(t)	mmHg,		// Partial pressure of O2 in pulmonary capillary blood and concn leaving it
 PO2tisscap(t)	mmHg,		// Partial pressure of O2 in body blood and outflow concn leaving it (stirred tank)
 PO2tiss(t)	mmHg,		// Partial pressure of O2 in body tissue exchanging with blood by PStiss
 O2pulcap(t)	nmol/ml,	// Concn O2 in pulmonary capillary blood and concn leaving it
 O2tisscap(t)	nmol/ml,	// Concn O2 in body blood and outflow concn leaving it (stirred tank)
 O2tiss(t)	nmol/ml,	// Concn O2 in body tissue exchanging with blood by PStiss
 PCO2pulcap(t)	mmHg,		// Partial pressure of CO2 in pulmonary capillary blood and concn leaving it
 PCO2tisscap(t)	mmHg,		// Partial pressure of CO2 in body blood and outflow concn leaving it (stirred tank)
 PCO2tiss(t)	mmHg,		// Partial pressure of CO2 in body tissue exchanging with blood by PStiss
 CO2pulcap(t)	nmol/ml,	// Concn CO2 in pulmonary capillary blood and concn leaving it
 CO2tisscap(t)	nmol/ml,	// Concn CO2 in body blood and outflow concn leaving it (stirred tank)
 CO2tiss(t)	nmol/ml;	// Concn CO2 in body tissue exchanging with blood by PStiss
 

// INITIAL CONDITIONS 
when(t=t.min) {
 Vlung = VFRC - Vintrapl0;
 nlung =  Patmos * Vlung / (R * T);
 nO2lung = PO2lung0 * Vlung / (R * T);
 O2pulcap = PO2pulcap0*SolyO2;
 O2tisscap = PO2tisscap0*SolyO2;
 O2tiss = PO2tiss0*SolyO2;
 nCO2lung = PCO2lung0 * Vlung / (R * T);
 CO2pulcap = PCO2pulcap0*SolyCO2;
 CO2tisscap = PCO2tisscap0*SolyCO2;
 CO2tiss = PCO2tiss0*SolyCO2;
 nN2lung = PN2lung0 * Vlung / (R * T);
 nH2Olung = PH2Olung0 * Vlung / (R * T);
 ctrach =  if (x = x.min) Patmos / (R * T) else Plung / (R * T);
 cO2trach = if (x = x.min) PO2atmos / (R * T) else PO2lung / (R * T);
 cCO2trach = if (x = x.min) PCO2atmos / (R * T) else PCO2lung / (R * T);
 cN2trach = if (x = x.min) PN2atmos / (R * T) else PN2lung / (R * T);
 cH2Otrach = if (x = x.min) PH2Oatmos / (R * T) else PH2Olung / (R * T);
}              

          
// ALGEBRAIC AND ODE EQUATIONS
 ComLung     = ComLung0*(1+ ComLungslope*(1-Vlung/VFRC)); // Compliance dependent on volume

 Pmouth = Patmos + ScalPvent * Pvent; 
 Ptank	 = ScalPexhaust * Pexhaust;
 Vlung:t = visc * ((Plung - Patmos - (Pintrapl - Pintrapl0)) * ComLung + (VFRC - Vintrapl0) - Vlung) ;
 Plung = nlung * R * T / Vlung;
 Fair = Vlung:t;
 nFlung = Plung * (Pmouth - Plung)/ (Res * R * T);
 nlung:t = nFlung 
	   -(PSO2lung * (PO2lung - PO2pulcap))/(R * T)
	   - (PSO2lung * (PCO2lung - PCO2pulcap))/(R * T); 


 Pintrapl = Pintrapl0 + (Vlung + Vintrapl0 - VFRC)/ComChest - ScalPchest*Pchest - Ptank;	
 Vchest  = Vchest0 + (Vlung - (VFRC - Vintrapl0) + (Vintrapl - Vintrapl0));
 Vintrapl = Vintrapl0* (Pintrapl0 + Patmos)/(Pintrapl + Patmos);

// Trachea
real constArea = 1 cm^2;
 nFtrach 	= Ptrach * (Ltrach / Res) * Ptrach:x / (R * T);  // mol/sec
 ctrach:t 	= -(1/constArea)*nFtrach:x 
		  + PSH2Otrach * (PH2Ovapor - PH2Otrach) / (R * T * Atrach); // <-- UNITS issue
	 
// cO2trach:t 	= yO2trach * (nFtrach:x) / Atrach ;   // <---- Units issue...
real const_Value(t,x) = 1 mol/(s*cm);  // replace 'nFtrach'

cO2trach:t 	= yO2trach * (const_Value) / Atrach ; 
 cCO2trach:t 	= yCO2trach *(const_Value) / Atrach ;
 cN2trach:t 	= yN2trach * (const_Value) / Atrach ;
 cH2Otrach:t 	= yH2Otrach * (const_Value) / Atrach  
		 + PSH2Otrach * (PH2Ovapor - PH2Otrach) / (R * T * Atrach);

 yO2trach	= cO2trach / ctrach;
 yCO2trach	= cCO2trach / ctrach;
 yN2trach	= cN2trach / ctrach;
 yH2Otrach	= cH2Otrach / ctrach;
 
 Ptrach 	= ctrach * R * T ;    //   ctrach dependency
 PO2trach 	= cO2trach * R * T ; 
 PCO2trach	= cCO2trach * R * T ;
 PN2trach 	= cN2trach * R * T ;
 PH2Otrach 	= cH2Otrach * R * T ;

// Lung 
 nO2lung:t = (if (nFlung >  0) nFlung * PO2atmos / Patmos else nFlung * PO2lung / Plung)
	     - (PSO2lung * (PO2lung - PO2pulcap))/(R * T);
 nCO2lung:t = (if (nFlung >  0) nFlung * PCO2atmos / Patmos else nFlung * PCO2lung / Plung)
	     - (PSO2lung * (PCO2lung - PCO2pulcap))/(R * T);
 nN2lung:t = (if (nFlung >  0) nFlung * PN2atmos / Patmos else nFlung * PN2lung / Plung);
 nH2Olung:t = (if (nFlung >  0) nFlung * PH2Oatmos / Patmos else nFlung * PH2Olung / Plung);

 PO2lung = nO2lung * R * T / Vlung;
 PCO2lung = nCO2lung * R * T / Vlung;
 PN2lung = nN2lung * R * T / Vlung;
 PH2Olung = nH2Olung * R * T / Vlung;

// Blood
 O2pulcap:t	= PSO2lung*(PO2lung - PO2pulcap)/(R*T)/Vpulcap + Fblood*(O2tisscap - O2pulcap)/Vpulcap; //Recirculation into Lung Cap
 O2tisscap:t	= PSO2tiss*(O2tiss - O2tisscap)/Vtisscap + Fblood*(O2pulcap - O2tisscap)/Vtisscap;
 O2tiss:t	= -PSO2tiss*(O2tiss - PO2tisscap*SolyO2)/Vtiss - Gtiss*O2tiss/Vtiss;
 PO2pulcap	= O2pulcap / SolyO2;
 PO2tisscap	= O2tisscap / SolyO2;
 PO2tiss	= O2tiss/SolyO2;

 
 CO2pulcap:t	= PSCO2lung*(PCO2lung - PCO2pulcap)/(R*T)/Vpulcap + Fblood*(CO2tisscap - CO2pulcap)/Vpulcap; //Recirculation into Lung Cap
 CO2tisscap:t	= PSCO2tiss*(CO2tiss - CO2tisscap)/Vtisscap + Fblood*(CO2pulcap - CO2tisscap)/Vtisscap;
 CO2tiss:t	= -PSCO2tiss*(CO2tiss - PCO2tisscap*SolyCO2)/Vtiss + RQ*Gtiss*O2tiss/Vtiss;
 PCO2pulcap	= CO2pulcap/SolyCO2;
 PCO2tisscap	= CO2tisscap/SolyCO2;
 PCO2tiss	= CO2tiss/SolyCO2;
 
//Calculation of O2 consumed
real O2consump(t)	mL/min;
O2consump = Gtiss*O2tiss*R*T/Patmos;
} // END OF MML CODE


