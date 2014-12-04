// 4th order linear forced differential equation 
math example6 {              
  realDomain t; t.min=0; t.max=30; t.delta=0.1; // time

  real a4=1,                   // a4 which multiplies d4u/dt4
       a3=3,                   // a3 which multiplies d3u/dt3
       a2=4,                   // a2 which multiplies d2u/dt2
       a1=2,                   // a1 which multiplies du/dt
       a0=0;                   // a0 which multiplies u

  extern real Cin(t);	       // externally calculated variable	

  real d3udt3(t),
       d2udt2(t),
       dudt(t),
       u(t) ;
		
  when(t=t.min) {              // Initial Condition
    d3udt3 = 0;
    d2udt2 = 0;
    dudt = 1;
    u = 1;
 }

                               // ODEs solving    
                               // a4*u''''+a3*u'''+a2*u''
                               // +a1*u'+a0*u=Cin(t)
   u:t      = dudt;
   dudt:t   = d2udt2;
   d2udt2:t = d3udt3;
   d3udt3:t = (1/a4)*(Cin-a3*d3udt3-a2*d2udt2-a1*dudt-a0*u); 
}
