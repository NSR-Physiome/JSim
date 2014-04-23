// 1-eqn non-linear implicit (entire) bounds, no starting value

math main {
  real u;
  u^2 - 5*u + 6 = 0;
  u >= 0;
  u <= 10;
}
