JSim v1.1

// MODEL NUMBER: 0235
// MODEL NAME: HbCO
// SHORT DESCRIPTION: 
// Binding kinetics of carbon monoxide to hemoglobin using Adair's four site model. 
// Two-way binding for carbon monoxide and no interactions between binding sites.


import nsrunit; unit conversion on;

math HbCO {	
  
  realDomain t sec;t.min =0; t.max=30; t.delta=0.1;
  realDomain cCO uM; cCO.min = 0; cCO.max = 100; cCO.delta = 0.1;

// From Henry's partial pressure law: partialpress = kh * concentration
// kh for carbon monoxide = 1052.63 L*atm/mol at 25C

  private real kh = 1052.63 *(760/1e6) mmHg/uM; //Convert Henry's gas const to mmHg
  real pCO(cCO) = (cCO*kh) ;  // Conc of CO in uM


// Default Dissociation constants for CO binding to each heme site:
real Kd1 = 1 uM;
real Kd2 = 1 uM;
real Kd3 = 1 uM;
real Kd4 = 1 uM;
 
// For CO binding, on rates for binding of CO to each heme site:
 real k1 1/(uM*sec), 
      k2 1/(uM*sec), 
      k3 1/(uM*sec), 
      k4 1/(uM*sec); 

// off rates for binding of CO to each heme site:
 real k1_off 1/sec, 
      k2_off 1/sec, 
      k3_off 1/sec, 
      k4_off 1/sec; 

// For default values, assume no interactions between sites, 
// thus the relationships between on and off rates are statistical: 

k1_off = 0.0025; // k4_off/4
k2_off = 0.003;  // k4_off/3 
k3_off = 0.005;  // k4_off/2
k4_off = 0.01; 

k1 = k1_off/Kd1; 
k2 = k2_off/Kd2;
k3 = k3_off/Kd3; 
k4 = k4_off/Kd4;

 
 real cHb(t,cCO) uM,   // Unbound hemoglobin conc as a function of t
      cHbCO(t,cCO) uM,  // Conc of hemoglobin with one site bound as a function of t.
      cHbCO_2(t,cCO) uM, //    ''                  two sites     ''
      cHbCO_3(t,cCO) uM, //    ''                 three sites    ''
      cHbCO_4(t,cCO) uM, //    ''                  four sites    ''
      cHbTot = 1 uM; // Total amount of hemoglobin in system.

 real SHbCO(t,cCO); // Total saturation of hemoglobin with CO at the four sites

// Total saturation, normalized to four sites:
 SHbCO(t,cCO) = ((cHbCO + 2*cHbCO_2 + 3*cHbCO_3 + 4*cHbCO_4)/(cHbTot*4)); 
 

// Init conditions:
 when (t=t.min) { cHbCO =0; cHbCO_2 =0; cHbCO_3 =0; cHbCO_4 =0;}

// Total Hb is constant:
 cHb = cHbTot - cHbCO - cHbCO_2 - cHbCO_3 - cHbCO_4;

// *****************************
// Differential equations:

 cHbCO:t = k1*cHb*cCO -k1_off*cHbCO - k2*cHbCO*cCO + k2_off*cHbCO_2;
 cHbCO_2:t = k2*cHbCO*cCO - k2_off*cHbCO_2 - k3*cHbCO_2*cCO + k3_off*cHbCO_3;
 cHbCO_3:t = k3*cHbCO_2*cCO -k3_off*cHbCO_3 - k4*cHbCO_3*cCO + k4_off*cHbCO_4;
 cHbCO_4:t = k4*cHbCO_3*cCO -k4_off*cHbCO_4;

//  ****************************
//  Hill calc (for comparison purposes):
real SHill(cCO);
real KpCO = 0.5 uM;  //KpO is p50 saturation of Hb in uM used in Hill Eq, SHill
real Kpref = 1 uM;		//dummy to normalize pO2 and KpO in Hill Eq
real n = 2.8; // hill coeff
SHill = (cCO/Kpref)^n/((KpCO/Kpref)^n + (cCO/Kpref)^n);
//  End of Hill calc

// Adair Hemoglobin saturation eq (For comparison):
real a1 =1 , a2=1, a3 = 1, a4 =1; // 4 Adair coefficients
real SHbCOadair(cCO) = (a1*(cCO) + 2*a2*(cCO)^2 + 3*a3*(cCO)^3 + 4*a4*(cCO)^4)/
                          (4*(1 +a1*(cCO) + a2*(cCO)^2 + a3*(cCO)^3 + a4*(cCO)^4));


}

/*
 DETAILED DESCRIPTION:
  Model describes the binding kinetics of carbon monoxide to hemoglobin. 
  It uses a simplified model where the binding kinetics of the four sites 
  on the hemoglobin molecule are independent of each other.
  The Hill equation for hemoglobin saturation is used for comparison. The Hill 
  Equation assumes instantaneous equilibrative binding of carbon monoxide to Hb. 
  A model that takes into account cooperative binding will give a better 
  description of the data. See HbO2 cooperative model for ways to modify 
  this model described here (NSR Physiome model: HbCoop).
  These models are applied to data from Antonini and Brunori's "Hemoglobin 
  and Myoglobin In Their Reactions With Ligands", 1971, Fig 10.16, p261 
  Data sets in JSim labeled: conc.045_Antonini and conc.237_Antoni

	
 SHORTCOMINGS/GENERAL COMMENTS:
	- Assumes that HbCO binding site is not influenced
          by the other sites on the hemoglobin molecule.
 
 KEY WORDS: Binding kinetics, Hemoglobin, Carbon Monoxide, HbCO, Adair, saturation, 
        blood gases, cooperativity, oxygen, Data, Tutorial  

 REFERENCES:
  Antonini E, Brunori M: "Kinetics of the reactions of hemoglobin and myoglobin with ligands"
  Chapter 8, p189-217 in: Hemoglobin and Myoglobin in their Reactions with Ligands,
  Frontiers of Biology, Volume 21, North-Holland Publishing Company, 1971 
     
  Hill AV: The diffusion of oxygen and lactic acid through tissues. 
  Proc R Soc Lond (Biol) 104: 39-96, 1928.

  Hill AV: The possible effects of the aggregation of the molecules of haemoglobin on its
  dissociation curves. J Physiol 40: iv-vii, 1910

  Hill R: Oxygen dissociation curves of muscle hemoglobin. Proc Roy Soc Lond B
  120: 472-480, 1936.  

 REVISION HISTORY:
	Original Author : BEJ  Date: 05/25/09
	Revised by      : BEJ  Date: 14Jun2010  
        Revised by   BEJ:16sep2013: Make two-way by adding off rates for binding of CO to Hb 
	          
	
 COPYRIGHT AND REQUEST FOR ACKNOWLEDGMENT OF USE:   
  Copyright (C) 1999-2013 University of Washington. From the National Simulation Resource,  
  Director J. B. Bassingthwaighte, Department of Bioengineering, University of Washington, Seattle WA 98195-5061. 
  Academic use is unrestricted. Software may be copied so long as this copyright notice is included.
  
  This software was developed with support from NIH grant HL073598. 
  Please cite this grant in any publication for which this software is used and send one reprint 
  to the address given above. 

*/


