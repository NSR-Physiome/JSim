// simple MML template
math template ABC {
  real a, b, c;
  a+b=c;
}

math main {
  ABC X, Y;
  X.a = 1;
  X.b = 2;
  Y.b = 5;
  X.c = Y.c;
}
