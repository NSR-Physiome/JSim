// MODEL NAME: MicroCircIllusoryCooperativity_1 
// SHORT DESCRIPTION: 
// A spatially distributed blood tissue exchange model which incorporates the regulation of coronary blood flow by adenosine acting through adenosine A2A receptors on the 
// surface of vascular smooth muscle cells. This simplified version of the model lumps all interstitial adenosine consuming/transporting processes into a single process with Michaelis Menten kinetics.


import nsrunit; unit conversion on;

math MicroCircIllusoryCooperativity_1{

// DOMAINS
realDomain t sec; t.min = 0; t.max = 100; t.delta=0.1; 	// Time domain
realDomain x cm; x.min = 0; x.max = 0.1; x.ct = 81;	// Spatial domain, along the capillary length

// //////////// Varying Adenosine Infusion Levels ////////////
// This domain essentially allows looping of model runs over differing inflowing
// adenosine levels.  Within each run, the adenosine infusion concentration is 
// fixed, but between runs, it is varied.  It is important to make sure that your 
// simulation time is sufficiently long for your slowest adjusting system response
// to reach steady state. This domain is set up as the log of the infusion concentration
// because we generally want equally spaced points in log space, since that is how
// we will plot it. 
realDomain logCin dimensionless; logCin.min = -1; logCin.max = 1; logCin.ct = 20; 

real Cin(logCin) uM;		// Inflowing adenosine concentration
real Cp(t,x,logCin) mM;		// Plasma adenosine concentration
real Cisf(t,x,logCin) mM;	// ISF adenosine concentration

real F(t,logCin) ml/min/g;	// Flow per gram tissue
real Fmin = 0.6 ml/min/g;	// Minimum value for flow (baseline with no infused adenosine)
real Fmax = 5.5 ml/min/g;	// Maximum value for flow (saturating value for high concentrations of infused adenosine)

real PSg(x) ml/min/g;		// Endothelial cleft permeability-surface area product per gram tissue
real PSg_max = 1 ml/min/g;	// Maximum capillary permeability. Approach to this value is exponental from 0.
real impermx = 0.02 cm; 	// The segment from x=0 to x=impermx is impermeable to adenosine (PS=0)
real PSg_relax = 0.01 cm;	// Controls rate of exponential approach to PSg_max after impermeable region

real Vp =0.05 ml/g; 		// Volume of plasma in capillary per gram tissue
real Visf = 0.15 ml/g;		// Volume of interstitial fluid per gram tissue
real L = x.max-x.min;		// Length of capillary

real Dp = 8.45e-5 cm^2/s;	// Diffusion coefficient for adenosine in plasma
real Disf = 8.45e-5 cm^2/s;	// Diffusion coefficient for adenosine in ISF

real xRecept = 0.01 cm; 	// Receptor x location
real Rkm = .1 uM;		// Receptor Km
real ReceptOcc(t,logCin) dimensionless;// Receptor fractional occupancy

// Lumped Consuming Process parameters
real G_Km = 1 uM;		// Michaelis Menten consumption Km
real G_Vmax = 1 uM/min;		// Michaelis Menten consumption Vmax


real C_atR(t,logCin) = Cisf(t,xRecept,logCin); // Concentration of adenosine at the receptor

ReceptOcc = C_atR/(Rkm+C_atR); 	// Michaelis-Menten type receptor binding

F = ReceptOcc*(Fmax-Fmin)+Fmin; // Flow depends linearly on receptor occupancy

PSg = if (x<impermx) 0 else PSg_max*(1-exp(-(x-impermx)/PSg_relax)); // Permeability along arteriole/capillary unit

Cin = 10^logCin; // Converts from logarithmic to linear value


// Initial Conditions
when (t=t.min) {Cp=0; Cisf=0;}

// Boundary Conditions
when (x=x.min) {F*L/Vp*(Cp-Cin)=Dp*Cp:x; Disf*Cisf:x=0;}
when (x=x.max) {Dp*Cp:x=0; Disf*Cisf:x=0;}

// Partial Differential Equations
Cp:t = -F*L/Vp*Cp:x + Dp*Cp:x:x - PSg/Vp*(Cp-Cisf);
Cisf:t = -G_Vmax*Cisf/(Cisf+G_Km)+Disf*Cisf:x:x - PSg/Visf*(Cisf-Cp);

}

/*

 DETAILED DESCRIPTION:

This is a spatially distributed blood tissue exchange model which incorporates 
the regulation of coronary blood flow by adenosine acting through adenosine A2A 
receptors on the surface of vascular smooth muscle cells. This simplified version 
of the model lumps all interstitial adenosine consuming/transporting processes 
into a single process with Michaelis Menten kinetics. This was done to reduce the
number of parameters when exploring the basic mechanisms underlying illusory 
cooperativity.

 SHORTCOMINGS/GENERAL COMMENTS:

Of course the combined dynamics of transporter uptake and deamination are not 
really adequately represented by lumping them together, so when examining these
processes, a more detailed model should be used. 

 
 KEY WORDS: illusory, cooperativity, adenosine, receptor, transport,

 
 REVISION HISTORY:
	Original Author : MB  Date: 04/12/13

	

 COPYRIGHT AND REQUEST FOR ACKNOWLEDGMENT OF USE:   
  Copyright (C) 1999-2012 University of Washington. From the National Simulation Resource,  
  Director J. B. Bassingthwaighte, Department of Bioengineering, University of Washington, Seattle WA 98195-5061. 
  Academic use is unrestricted. Software may be copied so long as this copyright notice is included.
  
  This software was developed with support from NIH grant HL073598. 
  Please cite this grant in any publication for which this software is used and send an email 
  with the citation and, if possible, a PDF file of the paper to: staff@physiome.org. 

*/

