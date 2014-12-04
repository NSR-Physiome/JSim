/* jsfprintf.c: fprintf with null file check */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void jsfprintf(FILE *f, char *fmt, ... )
{
         va_list p;
                                                                                
         va_start(p, fmt);
         if (f != NULL)
             (void) vfprintf(f,fmt,p);
         va_end(p);
                                                                                
}

 
