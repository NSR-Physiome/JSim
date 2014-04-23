// extended template example
math template ABC {
  real a, b, c;
  a+b=c;
}
ABC template ABCD {
  real d;
  d=2*c;
}
math main {
  ABCD X, Y;
  X.a = 1;
  X.b = 2;
  Y.b = 5;
  X.c = Y.c;
}
