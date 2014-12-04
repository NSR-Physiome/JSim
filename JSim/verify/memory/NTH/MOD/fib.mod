math main {
  realDomain n;
  n.min=1; n.max=10; n.delta=1;
  intState f(n);
  when (n=n.min) f=1;
  event (n>2) f = f(n-1) + f(n-2);
}
