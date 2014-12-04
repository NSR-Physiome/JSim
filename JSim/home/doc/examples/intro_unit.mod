// introduction to MML unit conversion
unit conversion on;      // enable unit checking
import nsrunit;          // standard units file
math main {
  real s1 = 50 cm;       // s1 in centimeters
  real s2 = 100 mm;      // s2 in millimeters
  real t = 5 sec;        // t in seconds
  real v1 = s1/t;        // v1 will be 10 cm/sec 
  real v2 = s2/t;        // v2 will be 20 mm/sec
  real s3 = (v1+2*v2)*t; // s3 will be 70 cm
}
