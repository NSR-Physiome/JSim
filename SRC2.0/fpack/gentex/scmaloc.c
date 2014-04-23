/* SIMCON model memory allocation wrappers.
 *
 * File scmaloc.c (Version 1.4).  Last modified at 12:53:54 on 9/26/97.
 *
 *..............................................................................
 *
 * From:   National Simulation Resource
 *         Center for Bioengineering
 *         Box 357962
 *         University of Washington
 *         Seattle, WA 98195-7962
 *
 *         Dr. J. B. Bassingthwaighte, Director
 *
 *..............................................................................
 * Copyright (C) 1996-7 by
 * National Simulation Resource, Center for Bioengineering,
 * University of Washington, Seattle, WA 98195-7962.
 * All rights reserved.
 *
 *******************************************************************************
 *            This module contains source code for SIMCON.                     *
 *      This code is proprietary and trade secret.  No portion may be          *
 *      copied or distributed without written authorization from the           *
 *      National Simulation Resource, Dr. J.B. Bassingthwaighte, Director.     *
 *******************************************************************************
 *
 * Wrappers are provided for the following functions:
 *	Malloc, Calloc, Realloc, and Free
 *
 *..............................................................................
 *
 * WRITTEN:  OCT96 - R.B. King
 *
 * MODIFIED: See SCCS history
 *
 *..............................................................................
 */

#include	<stdio.h>
#include	<stdlib.h>

#if defined(ALPHA) || defined(SOLARIS) || defined(LINUX)
typedef	void*	ptr;
#else
typedef	char*	ptr;
#endif

typedef unsigned Size;

/*-----------------------------------------------------------------------scmaloc
 */

void scmaloc()
{
	/* SCCS strings */
	static	char	sid1[] =
		{"@(#)scmaloc.c	1.4 created on 9/26/97 at 12:53:54.\n"};
	static	char	sid2[] =
		{"@(#) retrieved on 10/23/97 at 14:33:14.\n"};
	return;
}

/*------------------------------------------------------------------------------
 */

ptr Malloc(size)
Size size;
{
	ptr p;		/* the pointer from malloc()	*/
	p = (ptr)malloc(size);
	if (!p) return (ptr)NULL;
	else    return p;
}

ptr Calloc(n,size)
Size n, size;
{
	ptr p;		/* the pointer from calloc()	*/
	p = (ptr)calloc(n,size);
	if (!p) return (ptr)NULL;
	else    return p;
}

ptr Realloc(p,size)
ptr p;
Size size;
{
	ptr pnew;	/* the pointer from realloc()	*/
	pnew = (ptr)realloc(p,size);
	if (!pnew) return (ptr)NULL;
	else       return pnew;
}

int Free(p)
ptr p;
{
	free(p);
	return 0;
}
