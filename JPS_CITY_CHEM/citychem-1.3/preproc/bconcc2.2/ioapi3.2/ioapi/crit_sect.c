/**************************************************************************
VERSION "$Id: crit_sect.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
     PVM critical sections for  I/O API coupling mode.

PRECONDITIONS:

CALLS:

REVISION HISTORY
     prototype   1996 by Atanas Trayanov,
     MCNC Environmental Modeling Center

**************************************************************************/

include <stdio.h>
#include <string.h>
#include "pvm3.h"

#define ERROR(fmt, err, txt) cpl_warning(fmt, err, txt); return 0
#define MAXERRMSG 256

/* Function prototypes */
void cpl_warning(char *, int, char *);

/* =========================================================== */

int begin_critical_section(char *crit_sect_name)
{

  int bufid, cc;

  bufid = pvm_initsend( PvmDataDefault );
  if ( bufid < 0 )
    {
      ERROR("pvm_initsend ERROR (%s), in %s", bufid, "cri_sec" );
    }

  while ((cc=pvm_putinfo(crit_sect_name, bufid, PvmMboxDefault)) == PvmDenied);
  if (cc == 0) {
    cc=pvm_freebuf( bufid );
    if ( cc < 0 )
      {
	ERROR( "pvm_freebuf ERROR (%s), in %s",cc,"cri_sec" );
      }
    return 1;
  }

  if ( cc < 0 )
    {
      ERROR( "pvm_putinfo ERROR (%s) writing maibox %s",cc,crit_sect_name );
    }
  return 0;
}



/* =========================================================== */

int end_critical_section(char *crit_sect_name)
{

  return pvm_delinfo(crit_sect_name, 0, PvmMboxDefault);
}

/* =========================================================== */
int main()

{
  int mytid;


  /* Enter PVM */

  mytid = pvm_mytid();
  if ( mytid < 0 )
    {
      ERROR("*** Could not connect to PVM (%s)...\n     \
*** Caller: %s.  Exiting... ***\n", mytid, "testForOpen3");
    }

  begin_critical_section("TEST_C_S");

  printf("I am in critical section: %x\n", mytid);
  sleep(10);
  printf("I am leaving critical section: task %x\n", mytid);
  
  end_critical_section("TEST_C_S");

  return 1;

}










/* =========================================================== */
void cpl_warning(char *format, int errcode, char *txt)
{
  char errmsg[MAXERRMSG];
  extern char *pvm_errlist[];

  if (errcode < 0) {
    sprintf(errmsg, format, pvm_errlist[-errcode], txt);
  }
  else if (errcode > 0) {
    char *fmt;
    fmt = (char *) malloc(STRLEN(format) + 6);
    if (fmt != NULL) {
      sprintf(fmt,"%s %s",format, " (%d)");
      sprintf(errmsg, fmt, txt, errcode);
      free(fmt);
    }
    else {
      sprintf(errmsg,"%s %s", 
	      "ERROR in cpl_warning: malloc failed, when printing ",format);
    }
  }
  else {
    sprintf(errmsg, format, txt);
  }

  fprintf(stderr,"%s\n",errmsg);
  return;
}

/* ======================================================== */
