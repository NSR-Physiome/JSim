// This model generated automatically from SBML
// WARNING: Renamed variable _2DG to xxx2DG due to JSim namespace restrictions
// WARNING: Renamed variable 3DG to xxx3DG due to JSim namespace restrictions

// unit definitions
import nsrunit;
unit conversion off;

// SBML property definitions
property sbmlRole=string;
property sbmlName=string;
property sbmlCompartment=string;

// SBML reactions

math main {
  realDomain time second;
  time.min=0;
  extern time.max;
  extern time.delta;

  // variable definitions
  real comp1 = 10 L;
  real xxx2DG = 22 mol;
  real xxx3DG(time) mol;

  // equations
  xxx3DG = time+xxx2DG;

  // variable properties
  comp1.sbmlRole="compartment";
  xxx2DG.sbmlRole="species";
  xxx2DG.sbmlCompartment="comp1";
  xxx3DG.sbmlRole="species";
  xxx3DG.sbmlCompartment="comp1";
}

