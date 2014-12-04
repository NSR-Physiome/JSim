#include "jsimapi.h"

JSIM_REAL_FUNCTION(EBCommon,csquare) {
	JSIM_INIT();
    	JSimArg *a = JSIM_ARG(0);
    	double aval = a->realVal[0];
    	JSIM_RETURN(aval*aval);
}

JSIM_REAL_FUNCTION(EBCommon,cintegral) {
	JSIM_INIT();
    	JSimArg *u = JSIM_ARG(0);

 	int ct, i, mult;
	double tot = 0;
 	JSimGrid *t = u->grids;
	ct = t->ct;
	for (i=0; i<ct; i++) {
	    mult = (i==0 || i==(ct-1)) ? 1 : 2;
	    tot += u->realVal[i]*mult;
	}
	tot *= (t->max - t->min) / (2 * (t->ct - 1));
 	JSIM_RETURN(tot);
}

JSIM_PROCEDURE(EBCommon,crev) {
	JSIM_INIT();
    	JSimArg *u = JSIM_ARG(0);
    	JSimArg *v = JSIM_ARG(1);

	int i;
	int ct = u->grids[0].ct;
	int ct1 = v->grids[0].ct;
	if (ct != ct1) 
	    JSIM_ERROR("argument grid lengths differ");
	for (i=0; i<ct; i++)
	    v->realVal[i] = u->realVal[ct-1-i];
    	JSIM_RETURN(); 
}

