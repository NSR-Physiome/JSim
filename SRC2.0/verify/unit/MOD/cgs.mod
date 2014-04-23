import nsrunitcgs;
unit conversion on;

math main {
  real Vcgs = 1 cm^.5*g^.5/s;
  real Vsi volt;
  real Vcgs = Vsi;
  real Vcgs_inv = 1/Vcgs;

  real Acgs = 1 cm^1.5*g^.5/s^2;
  real Asi amp;
  real Acgs = Asi;
  real Asi_inv = 1/Asi;
  
  real Ccgs = 1 cm^1.5*g^.5/s;
  real Csi coulomb;
  real Ccgs = Csi;
  real Csi_inv = 1/Csi;
  
  real Fcgs = 1 cm;
  real Fsi farad;
  real Fcgs = Fsi;
  real Fsi_inv = 1/Fsi;
  
  real Rcgs = 1 s/cm;
  real Rsi ohm;
  real Rcgs = Rsi;
  real Rsi_inv = 1/Rsi;
  
  real Scgs = 1 cm/s;
  real Ssi siemens;
  real Scgs = Ssi;
  real Ssi_inv = 1/Ssi;

  real Hcgs = 1 s^2/cm;
  real Hsi henry;
  real Hcgs = Hsi;
  real Hcgs_inv = 1/Hcgs;

}
