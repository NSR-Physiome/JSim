// equivalent to example6a, but with unit conversion off
import nsrunit;
unit conversion off;
math example6b {
  real dist1 m, dist2 m;  // dist1 and dist2 in meters
  real dist3 cm;  // dist3 in centimeters
  dist1 = 3;
  dist2 = 2;
  dist3 = 100*(dist1 + dist2);
}
