import nsrunit; unit conversion on;

math HbCO {	
  
  realDomain t sec;t.min =0; t.max=.2; t.delta=0.1;
  realDomain cCO uM; cCO.min = 0; cCO.max = 2; cCO.delta = 1;

  real Kd1 = 1 uM;
  real k1_off = 0.0025 1/sec;
  real k1 = k1_off/Kd1;
  real cHbTot = 1 uM; 

  real cHbCO(t,cCO) uM; 

  real cHb(t,cCO) = cHbTot - cHbCO;
       
  when (t=t.min) cHbCO =0; 
  cHbCO:t = k1*cHb*cCO - k1_off*cHbCO;
}

