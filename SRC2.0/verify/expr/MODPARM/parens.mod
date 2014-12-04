// test parenthesis rendering

math template BOX {
  real a=1, b=2, c=3;
  real t1 = a+b+c;
  real t2 = a-b-c;
  real t3 = a*b*c;
  real t4 = a/b/c;
  real t5 = a^b^c;
  real t11 = (a+b)+c;
  real t12 = (a-b)-c;
  real t13 = (a*b)*c;
  real t14 = (a/b)/c;
  real t15 = (a^b)^c;
  real t21 = a+(b+c);
  real t22 = a-(b-c);
  real t23 = a*(b*c);
  real t24 = a/(b/c);
  real t25 = a^(b^c);

  real a1=a+b-c;
  real a2=a-b+c;
  real a3=a*b/c;
  real a4=a/b*c;
  real a5=a*b^c;
  real a6=a^b*c;
  real a7=a+b^c;
  real a8=a^b+c;

  real a11=a+(b-c);
  real a12=a-(b+c);
  real a13=a*(b/c);
  real a14=a/(b*c);
  real a15=a*(b^c);
  real a16=a^(b*c);
  real a17=a+(b^c);
  real a18=a^(b+c);

  real a21=(a+b)-c;
  real a22=(a-b)+c;
  real a23=(a*b)/c;
  real a24=(a/b)*c;
  real a25=(a*b)^c;
  real a26=(a^b)*c;
  real a27=(a+b)^c;
  real a28=(a^b)+c;

  real s1=a*(b+c);
  real s2=a+(b*c);
  real s3=a^(b+c);
  real s4=a+(b^c);
  real s5=a^(b*c);
  real s6=a*(b^c);
  
  real e;
  e-a=b-c;
}

math main {
  BOX box;
}

