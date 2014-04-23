// invert extern voltage
math voltage_inverter {
  realDomain t;       // time
  t.min=0; t.max=10; t.delta=0.1;
  extern real Vin(t); // externally provided input voltage
  real Vout(t);       // output voltage
  Vout = -Vin;        // constraint for Vout
}
