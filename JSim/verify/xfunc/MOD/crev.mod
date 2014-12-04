JSim v1.1

source procedure reverse(u@t; v@t) {
	language = "C";
	reentrant="true";
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
	real u(t) = t^2;
	real v(t);
	reverse(u@t, v@t);
}

