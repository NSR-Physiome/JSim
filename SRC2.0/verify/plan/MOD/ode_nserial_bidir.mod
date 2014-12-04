// ODE with n serial tanks, bidirectional exchange
//   run-time numerical problems with 1.6, can 1.7 do it right?

math main {
	int n = 3;
	realDomain t, k;
	t.min=0; t.max=3; t.delta=0.25;
	k.min=1; k.max=n; k.delta=1;
	real Cin(t) = 1-exp(-t);
	real u(t,k);
	real D(k) = k;
	when (t=t.min) u=0;
	real ufwd(t,k) = if (k=k.min) Cin else u(t,k-1);
	real ubwd(t,k) = if (k=k.max) 0 else u(t,k+1);
	u:t = D*(ufwd + ubwd - 2*u);
}
