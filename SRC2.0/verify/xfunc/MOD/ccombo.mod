JSim v1.1

source real function square(a) {
	language = "C";
	reentrant="true";
	maincode={{
	    double aval = a->realVal[0];
	    JSIM_RETURN(aval*aval);
	}};
}

source real function integral1(a) {
 	language = "C";
 	maincode={{
 	    int ct, i, mult;
	    double tot = 0;
 	    JSimGrid *t = a->grids;
	    ct = t->ct;
	    for (i=0; i<ct; i++) {
	    	mult = (i==0 || i==(ct-1)) ? 1 : 2;
	    	tot += a->realVal[i]*mult;
	    }
	    tot *= (t->max - t->min) / (2 * (t->ct - 1));
 	    JSIM_RETURN(tot);
	}};
}

source procedure reverse(u@t; v@t) {
	language = "C";
  	maincode={{
	    int i;
	    int ct = u->grids[0].ct;
	    for (i=0; i<ct; i++)
	        v->realVal[i] = u->realVal[ct-1-i];
    	    JSIM_RETURN(); 
	}};
}

math main {
	realDomain t;
	t.min=0; t.max=4; t.delta=1;
	real u(t) = square(t+1);
	real v = integral1(u@t);
	real w(t);
	reverse(u@t, w@t);
}

