// 1-eqn non-linear implicit (entire) complete bounds

math main {
  real u;
  u^2 - 5*u + 6 = 0;
  u >= 0;
  u <= 10;
  u ~= 5;
}
