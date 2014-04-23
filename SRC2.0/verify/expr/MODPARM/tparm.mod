// parameterized Kin(t), make sure Kin=2 works at runtime

math main {
  realDomain t;
  t.min=0; t.max=1; t.delta=1;
  real Kin(t), Kout(t);
  Kin = 1;
  Kout = Kin;
}
