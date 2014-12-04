// 2 bounded non-linear implicit eqns
math implicit6 {
  real x, y;
  x^2 + y^2 = 1;
  y = x^2;
  x >= 0;
  x <= 1;
  y >= 0;
  y <= 1;
}
