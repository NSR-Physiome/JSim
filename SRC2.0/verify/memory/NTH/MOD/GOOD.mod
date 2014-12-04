//
// MODEL NUMBER: 0176
// MODEL NAME: One_Slab_Diffusion_Partition
// SHORT DESCRIPTION: This model simulates the diffusion of a substance through a 
// region with a constant diffusivity and different solubilities inside and outside the region. 



// -----------------------------------------------------------------------------
//                 O N E - S L A B   D I F F U S I O N   M O D E L
//		     with   P A R T I T I O N   C O E F F I C I E N T  
// -----------------------------------------------------------------------------

        import nsrunit;
	unit conversion on;

	math One_Slab_PartCoeff{

// -----------------------------------------------------------------------------
//   PARAMETERS OF          O N E - S L A B   D I F F U S I O N   M O D E L  
//		             with   P A R T I T I O N   C O E F F I C I E N T  
// -----------------------------------------------------------------------------

	real					// MODEL PARAMETERS
	D = 0.001 cm^2/sec,			// Diffusion coefficient
	C_lh = 10 mM,				// Concentration at LH boundary
	C_rh = 3 mM,				// Concentration at RH boundary
	C_0 = 0 mM,
	Lambda = 1.25;
	
// -----------------------------------------------------------------------------
//   VARIABLES OF           O N E - S L A B   D I F F U S I O N   M O D E L 
//		             with   P A R T I T I O N   C O E F F I C I E N T  
// -----------------------------------------------------------------------------

	realDomain 				// DOMAIN VARIABLES
	t sec; t.min=0; 			// Time domain
	t.max=0.15; t.delta=0.01;
    	realDomain 
	x cm; x.min=0;				// Spatial domain
	x.max=.1; x.delta=0.02;

	real					// MODEL VARIABLES
	C(x,t) mM,				// Concentration in region
	Q_lh(t) mM*cm/s,			// Flux in at LH boundary
	Q_rh(t) mM*cm/s;			// Flux out at RH boundary

// -----------------------------------------------------------------------------
//   INITIAL CONDS OF       O N E - S L A B   D I F F U S I O N   M O D E L 
//		             with   P A R T I T I O N   C O E F F I C I E N T   
// -----------------------------------------------------------------------------

	when (t=t.min) {
		C = if(x=x.min) C_lh else
			if(x=x.max) C_rh 
			else C_0;
		}
	when (x=x.min) {
		C:x = -Q_lh/D;
		}
	when (x=x.max) {
		C:x = -Q_rh/D;
		}
		
// -----------------------------------------------------------------------------
//   SYSTEM OF EQNS OF      O N E - S L A B   D I F F U S I O N   M O D E L
//		             with   P A R T I T I O N   C O E F F I C I E N T   
// -----------------------------------------------------------------------------

//	  Approximate concentration flux at left hand and 
//        right hand boundaries using a central difference
	Q_lh = D * ( (3*C_lh*Lambda) - 4*C(x.min+x.delta,t) +1*C(x.min+2*x.delta,t))/(2*x.delta);
	Q_rh = D * (-C(x.max-2*x.delta,t) +4*C(x.max-x.delta,t) - 3*(C_rh*Lambda) )/(2*x.delta);

	
//        Governing PDE

	C:t = D * (C:x:x);
 
}

/*
 FIGURE:
                                 
                            |                 |
                            |                 |
               C_0 = 10 mM  |                 |  C_0 = 10 mM
                            |        D        |
                            |                 |  
                            | Lambda = 1.25   |
                            | relative to     |
                            | external soln   |

                           x=0               x=1


 DETAILED DESCRIPTION:
   One dimensional diffusion into and across a uniform slab in which the
   solubility in the slab is different from that in the solutions outside.
   The slab partition coefficient, Lambda, = the ratio of inside/outside
   concentrations at equilibrium. Solubility in the two solutions is the same,
   in this case.  
   On each side of the region the concentration is fixed at C_0 = 10 mM and 
   there is a difference in solubility of the diffusive species inside and 
   outside the region. Initially the concentration in the prescribed region is 0 mM.  

    Lambda is the slab partition coefficient, the ratio of concentrations 
    at equilibrium:

                                     C(x = 0+)
                          Lambda  = ----------
                                     C_0

    where C_0 and C(x = 0+) are the concentrations of the solute just outside 
    and just inside the region at the boundary at the steady state.  
    Diffusion begins at t = 0 seconds and progresses according to the
    governing equation for diffusion in one dimension with a constant D.

   
                                          2
                            dC           d C
                           ----  =  D * -----
                            dt             2
                                         dx


    We can chose to impose boundary conditions on the concentration or the 
    concentration flux at the boundary.  The condition on the concentration
    on the boundary assumes the concentration just inside the region at the
    boundaries immediately goes to the steady state concentration (e.g. 
    C(x=0+) = C_0*Lambda).  While this is entirely accurate at the surface, the 
    concentrations just inside the boundaries creep up to this steady
    state condition with time.  The flux boundary condition imposed below
    simulates the correct temporal behavior right at the boundaries.  	

 SHORTCOMINGS/GENERAL COMMENTS:
	- Specific inadequacies or next level steps
 
 KEY WORDS: Partition coefficient, Solubility, Diffusion, One region, Transport physiology,
  one slab

 REFERENCES:
	Bassingthwaighte JB. Transport in Biological Systems, Springer Verlag, New York, 2007. 

 REVISION HISTORY:
	Original Author : BCarlson  Date: 7/Apr/07
	Revised by      : JBB   Date: 1/Mar/2010  
        Revision: 1) define partition coeff
	         
	Revised by      :  BEJ  Date: 1/Mar/2010   
        Revision: 1) Update format of comments

        Revised by      :  BEJ  Date: 1/Feb/2011   
        Revision: 1) Changed solver from Toms to MacCormack due to error:
            Job "Run model One_Slab_Diffusion_Partition" in progress...
            aborted: Toms731 (moving-grid) faild: idid=-12

        Revised by      : GMR Date: 1/31/13
        Revision: 1) Correct boundary flux condition to be 2nd order 
                     accurate.
	        
 COPYRIGHT AND REQUEST FOR ACKNOWLEDGMENT OF USE:   
  Copyright (C) 1999-2010 University of Washington. From the National Simulation Resource,  
  Director J. B. Bassingthwaighte, Department of Bioengineering, University of Washington, Seattle WA 98195-5061. 
  Academic use is unrestricted. Software may be copied so long as this copyright notice is included.
  
  This software was developed with support from NIH grant HL073598. 
  Please cite this grant in any publication for which this software is used and send an email 
  with the citation and, if possible, a PDF file of the paper to: staff@physiome.org. 

*/



