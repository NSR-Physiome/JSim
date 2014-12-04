// adding user-defined units
import nsrunit;
unit teaspoon = 0.203 cm^3 prefixable,
furlong = 201 m,
grosdik = 77 furlong/sec;
unit conversion on;
math example7 {
  real vol = 15 microteaspoon;
  real len = 8 furlong;
  real rate = vol/len;
}
