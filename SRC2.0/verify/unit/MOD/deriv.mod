// checks unit-safe derivative expansion for some operators
//   this test fails pre-1.6.86

import nsrunit; 
unit conversion on;

math main { 
realDomain t sec; t.min=0; t.max=5; t.delta=1;

real D1 = 1 cm;
real D2 = 1 cm;
real u(t) = t^2;

real m1(t) = ((D1+D2)*u):t;
real m2(t) = (u*(D1+D2)):t;
real d1(t) = ((D1+D2)/u):t;
real d2(t) = (u/(D1+D2)):t;
}
