/***********************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "iobin3.c" version "$Id: iocpl.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems, and
    (C) 2015 UNC Institute for the Environment.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Implementation of I/O API Coupling Mode.

CALLS:  PVM

REVISION HISTORY:
    Prototype  6/1998 by Atanas Trayanov, MCNC EMC.

    Modified 02/2015 by CJC for I/O API version 3.2:  M3INT8  support

***********************************************************************/

#ifdef IOAPICPL


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h> 
#include "pvm3.h"
#include "iodecl3.h"


#define MAXERRMSG 256
#define MAXPATHNAME 512
#define BARRIER_1_TAG     1
#define BARRIER_2_TAG     2
#define BARRIER_3_TAG     3
#define NOTIFY_WRITER_TAG 98
#define NOTIFY_UPON_EXIT_TAG 99
#define DELIM_CHAR '#'
#define NSTEPS2KEEP 2
#define DONT_REMOVE (-1)
#define ANY_SRC     (-1)
#define ANY_CTX     (-1)
#define MAXFNAMELEN 256
#define MAXSPECLEN   16
#define MAXDATELEN    7
#define MAXTIMELEN    6
#define MAXHEADERLEN (MAXFNAMELEN+1+5+1)
#define MAXLOCKLEN   (MAXFNAMELEN+1+4+1)
#define MAXENTRYLEN  (MAXFNAMELEN+1+MAXSPECLEN+1+MAXDATELEN+1+MAXTIMELEN+1)

#define BLOCKING_RECV 2
#define BLOCKING      1
#define NON_BLOCKING  0

#define ERROR(fmt, err, txt) cpl_warning(fmt, err, txt); return 0

/* Typedefs */

typedef struct _ReaderList {
  int tid;
  int context;
  char *pgm;
  struct _ReaderList *next;
} ReaderList;

typedef struct _WriterList {
  int context;
  int mhid;
  char *file;
  struct _WriterList *next;
} WriterList;

typedef struct _NetFileList {
  int owner_tid;
  char *filename;
  struct _NetFileList *next;
} NetFileList;

typedef struct _TidList {
  int tid;
  struct _TidList *next;
} TidList;

/* Global variables */

static int mhid=0;
static int _NSteps2Keep=0;
static int _iocpl_mode=0;
static int _iocpl_oper=NON_BLOCKING;
static int _iocpl_owner_tid=-1;
static int _WaitForSynchro=0;
static ReaderList *rp_head=NULL;
static WriterList *wp_head=NULL;
static NetFileList *flp_head=NULL;
static TidList *ttl_head=NULL; /* list of tids of terminated tasks */
static TidList *ntl_head=NULL; /* list of tids of notified tasks */
static char mypgmname[MAXPATHNAME]="UNKNOWN";

/* Function prototypes */
void cpl_warning(char *, int, char *);
void delete_readlist(ReaderList **);
void delete_writelist(WriterList **);
void delete_netfilelist(NetFileList **);
void delete_tidlist(TidList **);
int iocpl_barrier(int);
int find_earliest(char *, char *, int *, int *);
int   pack_desc3(IOAPI_Bdesc3 *, IOAPI_Cdesc3 *, int);
int unpack_desc3(IOAPI_Bdesc3 *, IOAPI_Cdesc3 *, int *);
int remove_mbox_entry(char *, char *, int, int);
ReaderList *new_readlist(int, int, char *);
WriterList *new_writelist(int, int, char *);
NetFileList *new_netfilelist(int, char *);
TidList *new_tidlist(int);
int checkIfOwnerAlive(int);
int getOwnerTid(char *);
int notifyOwner(int);


#ifdef _CRAYMPP
#include <fp.h>
#ifdef HUGE_VAL
#undef HUGE_VAL
#include <math.h>
#endif

/*
   The next set of 2 wrappers are provided to handle the problem on 
   the CRAY T3D/T3E that fortran reals are 8 bytes and C floats are 4 bytes. 
   Thus when a fortran program calls a C routine there is a mismatch. 
*/

/* ************************************************************************* */
/* ************************************************************************* */
int pvm_pkreal(double *dbl_buffer, int numvalues, int step)
{
  float  *flt_buffer;
  int i, ret;


  /* set the float ptr to point to the beginning of the double */
  flt_buffer = (float*)dbl_buffer;

  /* convert the doubles to floats in place, therefore no malloc required */
  /* start at the low end since doubles take more space */
  /* if the double buffer has an initial value of NaN, reinitialize it to 0 */
  for (i=0;i<numvalues;i+=step) {
    switch (fpclassify(dbl_buffer[i])) {
    case FP_NAN:
    case FP_INFINITE:
    case FP_SUBNORMAL:
      dbl_buffer[i]=0.0;
    }

    if (fabs(dbl_buffer[i]) >= MAXFLOAT) {
      printf("\n\n double of %g exceeds float bounds, reset to %g\n\n",
	    dbl_buffer[i],MAXFLOAT);
      dbl_buffer[i] = MAXFLOAT;
    }
    flt_buffer[i] = (float)dbl_buffer[i];
  }

  ret = pvm_pkfloat(flt_buffer,numvalues,step);
  while ((numvalues-=step) >= 0) {
    dbl_buffer[numvalues]=(double)flt_buffer[numvalues];
  }
  return ret;
}


/* ************************************************************************* */
/* ************************************************************************* */
int pvm_upkreal (double *dbl_buffer, int numvalues, int step)
{
  int ret;
  float *flt_buffer;
  /* set the float ptr to point to the beginning of the double */
  flt_buffer = (float*)dbl_buffer;

  ret=pvm_upkfloat(flt_buffer,numvalues,step);
  /* convert the floats to doubles in place, therefore no malloc required */
  /* start at the high end since doubles take more space */
	while ((numvalues-=step) >= 0) {
		dbl_buffer[numvalues]=(double)flt_buffer[numvalues];
	      }

  return ret;
}


/* ************************************************************************* */
/* ************************************************************************* */
#else
#define pvm_pkreal(x,n,s) pvm_pkfloat(x,n,s)
#define pvm_upkreal(x,n,s) pvm_upkfloat(x,n,s)
#endif

/* =========================================================== */
/* =========================================================== */
int read_header(char *fname, IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p,
		int blocking, int *context) 
{
  int bufid, cc;
  char header[MAXHEADERLEN];
  int flag;


  sprintf(header,"%s%c%s",fname,DELIM_CHAR,"DESC3");

  if (blocking) {
    flag = PvmMboxWaitForInfo;
    _iocpl_oper=BLOCKING;
    _iocpl_owner_tid = getOwnerTid(fname);
  }
  else {
    flag = PvmMboxDefault;
    _iocpl_oper=NON_BLOCKING;
    _iocpl_owner_tid = -1;
  }
    
  if (!checkIfOwnerAlive(getOwnerTid(fname))) return 0;

  /* potentially BLOCKING_READ */
  bufid = pvm_recvinfo( header, 0, flag );
  _iocpl_oper=NON_BLOCKING;
  _iocpl_owner_tid = -1;
  if ( bufid < 0 ) {
    ERROR("INTERNAL IOCPL ERROR: pvm_recvinfo (%s): receiving mailbox %s", 
	  bufid,header );
  }

  cc=pvm_setrbuf( bufid );
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_setrbuf (%s), mailbox %s",cc,header );
    }
  if (!unpack_desc3(bdesc3p, cdesc3p, context)) return 0;
  cc=pvm_freebuf( bufid );
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_freebuf (%s), in %s",cc,
	     "read_header" );
    }


#ifdef DEBUG
  printf("DEBUG:header info:%s: date=%d time=%d rows=%d cols=%d ftype=%d\n",
	 header,
	 bdesc3p->sdate,bdesc3p->stime,
	 bdesc3p->nrows,bdesc3p->ncols,bdesc3p->ftype);
#endif
  return 1;
}

/* =========================================================== */

int write_header(char *fname, IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p,
		 int context) 
{
  int bufid, cc;

  char header[MAXHEADERLEN];

  sprintf(header,"%s%c%s",fname,DELIM_CHAR,"DESC3");

  /* Create Message */
  bufid = pvm_initsend( PvmDataDefault );
  if ( bufid < 0 )
    {
      ERROR("INTERNAL IOCPL ERROR: pvm_initsend (%s), in %s",
	    bufid, "write_header" );
    }
  if (!pack_desc3(bdesc3p, cdesc3p, context)) return 0;
  
  cc = pvm_putinfo( header, bufid, PvmMboxDefault);
  if ( cc < 0 )
    {
      ERROR("INTERNAL IOCPL ERROR: pvm_putinfo (%s) writing header %s",
	    cc,header);
    }
#ifdef DEBUG
  printf( "DEBUG:Inserted header (%d,%d) Into \"%s\"\n",
	 bdesc3p->sdate, bdesc3p->stime, header );
#endif

  cc=pvm_freebuf( bufid );
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_freebuf (%s) in %s",cc,
	     "write_header" );
    }

  return 1;
}

/* =========================================================== */
int open3net(char *fname, 
	     IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p,
	     int status, char *pgm)
{
  
  int mytid;
  int cc;
  int bufid;
  int context;

  /* vars related to removal of enrties */
  int nclass, i;
  struct pvmmboxinfo *mboxinfo;
  char errmsg[MAXERRMSG];
  ReaderList **rpp;
  WriterList **wpp;
  NetFileList **flpp;

  int tid, found;
  int old_context;
  int mhid_w; /* there is one MHF for every vir. file: context is diff. */
  char lockfile[MAXLOCKLEN];
  char header[MAXHEADERLEN];
  char wpgm[MAXPATHNAME];

  /* message handling function prototypes */
  int taskExit(int);
  int taskReader(int);

  /* Enter PVM */

  mytid = pvm_mytid();
  if ( mytid < 0 )
    {
      ERROR("*** Could not connect to PVM (%s)...\n     \
*** Caller: %s.  Exiting... ***\n", mytid, "open3net");
    }

  strcpy(mypgmname, pgm);

  /* 
   open file specific lock file. First task to get it would allocate
   message context 
   */
  sprintf(lockfile,"%s%c%s",fname, DELIM_CHAR, "LOCK");
  bufid = pvm_initsend( PvmDataDefault );
  if ( bufid < 0 )
    {
      ERROR("INTERNAL IOCPL ERROR: pvm_initsend (%s), in %s",
	    bufid, "open3net" );
    }
  cc=pvm_pkstr(pgm);
  cc=pvm_putinfo(lockfile, bufid, PvmMboxMultiInstance);
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_putinfo (%s) writing maibox %s",
	     cc,lockfile );
    }
  
  /* add message handler to receive notification from pvm deamon
     upon abnormal exit/interrupt of another task */
  if(mhid == 0) {
    mhid=pvm_addmhf(ANY_SRC, NOTIFY_UPON_EXIT_TAG, ANY_CTX, taskExit);
  }

  switch (status) 
    {
    case FSREAD3:
      if (!read_header(fname, bdesc3p, cdesc3p, BLOCKING, &context)) return 0;
      
      /* get writer's tid FROM DESC3 */
      sprintf(header,"%s%c%s",fname,DELIM_CHAR,"DESC3");
      cc=pvm_getmboxinfo(header, &nclass, &mboxinfo);
      if (nclass != 1) {
	fprintf(stderr,"%s\n","Multiple entries in DESC3");
	return 0;
      }
      tid = mboxinfo->mi_owners[0];
      
      cc=pvm_getmboxinfo(lockfile, &nclass, &mboxinfo);
      if (nclass != 1) {
	fprintf(stderr,"Multiple entries in %s\n",lockfile);
	return 0;
      }
      
      /* Find the writer: loop over entries and compare TIDs */
      found = 0;
      for (i=0; i < mboxinfo->mi_nentries; i++) {
	if (tid == mboxinfo->mi_owners[i]) {
	  found = 1;
	  break;
	}
      }
      if (! found) {
	sprintf(errmsg,"Cannot find the task that created file %s", fname);
	m3mesgc(errmsg);
	return 0;
      }

      /* update list of opened net files */
      for (flpp=&flp_head; *flpp != NULL; flpp = &(*flpp)->next) {
      }
      *flpp = new_netfilelist(tid,fname);

      /* set notification to the writer (You_Got_a_Reader) */
      old_context=pvm_setcontext(context);
      pvm_initsend(PvmDataDefault);
      pvm_pkint(&mytid, 1, 1);
      pvm_pkstr(fname);
      cc=pvm_send(tid, NOTIFY_WRITER_TAG);
      if (cc < 0) {
	ERROR("INTERNAL IOCPL ERROR: pvm_send (%s), in %s", cc, "open3net" );
      }
      pvm_setcontext(old_context);


      /* receive the index entry of writer */
      _iocpl_oper=BLOCKING;
      _iocpl_owner_tid = getOwnerTid(fname);
      /* BLOCKING_READ */
      bufid = pvm_recvinfo(lockfile, i, PvmMboxWaitForInfo);
      _iocpl_oper=NON_BLOCKING;
      _iocpl_owner_tid = -1;
      if (bufid < 0) {
	ERROR( "INTERNAL IOCPL ERROR: pvm_recvinfo (%s) receiving mailbox %s",
	       bufid,lockfile );
      }

      cc=pvm_setrbuf( bufid );
      cc=pvm_upkstr(wpgm);
      cc=pvm_freebuf( bufid );
      
      /* insert new entry in ReaderList linked-list */
      found = 0;
      for (rpp=&rp_head; *rpp != NULL; rpp = &(*rpp)->next) {
	if (((*rpp)->tid == tid) && ((*rpp)->context==context)) {
	  found = 1;
	  break;
	}
      }

      if (!found) {
	*rpp = new_readlist(tid,context,wpgm);
      }

      /* ALT:
	 originally only READERS got notified upon task termination
	 and the call to pvm_addmhf was here.
	 It is moved to the beginning of the function
	 so that READERS and WRITTER get proper notification
      ALT */

      notifyOwner(tid);

      break;

    case FSNEW3:
    case FSUNKN3:
      if ( _NSteps2Keep==0 ) {
	_NSteps2Keep=envintc("IOAPI_KEEP_NSTEPS",NULL,NSTEPS2KEEP,&cc);
	if (_NSteps2Keep < 0) {
	  ERROR("%s",0,
		"ERROR:Negative value for IOAPI_KEEP_NSTEPS is not allowed!");
	}
	if (_NSteps2Keep == 0) {
	  _NSteps2Keep = DONT_REMOVE;
	  sprintf(errmsg,
		  "WARNING: no records will be removed for virtual file \"%s\"\n",
		  fname);
	  m3mesgc(errmsg);
	}
      }
      
      context = pvm_newcontext();
      if( !write_header(fname, bdesc3p, cdesc3p, context)) return 0;
      
      /* write was successful, log the info */
      sprintf(errmsg,
	      "OPEN3: task %s (tid %x) created virtual file \"%s\" to WRITE\n",
	      pgm, mytid, fname);
      m3mesgc(errmsg);
      
      /* update list of opened net files */
      for (flpp=&flp_head; *flpp != NULL; flpp = &(*flpp)->next) {
      }
      *flpp = new_netfilelist(mytid,fname);

      /* update list of writers */

      for (wpp=&wp_head; *wpp != NULL; wpp = &(*wpp)->next) {
      }

      mhid_w = pvm_addmhf(ANY_SRC, NOTIFY_WRITER_TAG, context, taskReader);

      *wpp = new_writelist(context,mhid_w,lockfile);

      break;
      
    default:
      sprintf(errmsg,"ERROR in open3net: Invalid file open flag entered: %d",
	      status);
      m3mesgc(errmsg);
      return (0);
    }
  
  _iocpl_mode = 1;

  return 1;
}


/* =========================================================== */

/*
   read3net: reads 1 timestep of data for a given file and variable.
   we check the header: if start day/time are older than requested date/time, 
   return error, else block until the new record becomes and return the data
*/
int read3net(char *fname,
	     char *vname,
	     int skip, 
	     int jdate,
	     int jtime,
	     void *buffer,
	     int n,
	     int buftype)
{

  int i;
  int bufid,cc;
  int sdate, stime;
  char entry[MAXENTRYLEN];
  char errdatestr[]=
    "ERROR:Requested timestep is before current SDATE/STIME YYYYJJJ/HHMMSS";

  sprintf(entry,"%s%c%s%c%07d:%06d", fname, DELIM_CHAR,
	  vname, DELIM_CHAR, jdate, jtime);

  bufid = pvm_recvinfo( entry, 0, PvmMboxDefault );
  if (bufid == PvmNotFound) {
    if (!find_earliest(fname, vname, &sdate, &stime)) return 0;
    if (secsdiffc(sdate, stime, jdate, jtime)<0) {
      sprintf(errdatestr+55,"%07d/%06d",sdate, stime);
      ERROR("%s",0, errdatestr);
    }
    if (!checkIfOwnerAlive(getOwnerTid(fname))) return 0;
    _iocpl_oper=BLOCKING;
    _iocpl_owner_tid = getOwnerTid(fname);
    /* BLOCKING_READ */
    bufid = pvm_recvinfo( entry, 0, PvmMboxWaitForInfo );
    _iocpl_oper=NON_BLOCKING;
    _iocpl_owner_tid = -1;
  }
  if ( bufid < 0 ) {
    ERROR("INTERNAL IOCPL ERROR: pvm_recvinfo (%s): receiving mailbox %s", 
	  bufid,entry );
  }
#ifdef DEBUG
  printf( "DEBUG:Received Message from mbox \"%s\"\n", entry);
#endif
  if (n <= 0) goto cleanup;
  cc=pvm_setrbuf( bufid );
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_setrbuf (%s), in %s",cc,"read3net" );
    }
  cc=PvmOk;
  switch (buftype) 
    {
    case M3INT:
      for (i=0; i<skip; i++) {
	    cc |= pvm_upkint( buffer,1,1 );
      }
      cc |= pvm_upkint( buffer,n,1 );
      break;
    case M3REAL:
      for (i=0; i<skip; i++) {
	cc |= pvm_upkreal( buffer,1,1 );
      }
      cc |= pvm_upkreal( buffer,n,1 );
      break;
    case M3DBLE:
      for (i=0; i<skip; i++) {
	cc |= pvm_upkdouble( buffer,1,1 );
      }
      cc |= pvm_upkdouble( buffer,n,1 );
      break;
    case M3INT8:
      for (i=0; i<skip; i++) {
	    cc |= pvm_upklong( buffer,1,1 );
      }
      cc |= pvm_upklong( buffer,n,1 );
      break;
    }
  if (cc < 0) {
    ERROR("INTERNAL IOCPL ERROR: pvm_unpack (%s), in %s",cc,"read3net");
  }
 cleanup:
  cc=pvm_freebuf( bufid );
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_freebuf (%s), in %s",cc,"read3net" );
    }

  return 1;
}

/* =========================================================== */

/* 
   write3net: writes 1 timestep of data for a given file and variable.
   the following steps are performed:
   1) put the new record in mailbox
   2) update header
   3) delete old record(s)
*/
int write3net(char *fname, char *vname, 
	      int jdate, int jtime, void *buffer, int n, int buftype)
{

  int bufid, cc, index;
  char entry[MAXENTRYLEN];

/* put the new record into maibox */
  sprintf(entry,"%s%c%s%c%07d:%06d", fname,  DELIM_CHAR,
	  vname, DELIM_CHAR, jdate, jtime);

  bufid = pvm_initsend(PvmDataDefault);
  if (bufid < 0) {
    ERROR("INTERNAL IOCPL ERROR: pvm_initsend (%s), in %s",
	  bufid, "write3net" );
  }

  switch (buftype) 
    {
    case M3INT:
      cc = pvm_pkint( buffer,n,1 );
      break;
    case M3REAL:
      cc = pvm_pkreal( buffer,n,1 );
      break;
    case M3DBLE:
      cc = pvm_pkdouble( buffer,n,1 );
      break;
    case M3INT8:
      cc = pvm_pklong( buffer,n,1 );
      break;
    }
  if (cc < 0) {
    ERROR("INTERNAL IOCPL ERROR: pvm_pack (%s), in %s",cc,"write3net");
  }

  index = pvm_putinfo( entry, bufid, PvmMboxDefault );
  if ( index < 0 ) {
    ERROR("INTERNAL IOCPL ERROR: pvm_putinfo (%s) writing %s",index,entry);
  }
  cc=pvm_freebuf( bufid );
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_freebuf (%s), in %s",cc,"write3net" );
    }

#ifdef DEBUG
  printf( "DEBUG:Inserted Message Into \"%s\" at index=%d:\n", entry, index );
#endif

  if (_NSteps2Keep != DONT_REMOVE) {
    if (!remove_mbox_entry(fname, vname, jdate, jtime)) return 0;
  }

  return 1;
}


/* =========================================================== */
int remove_mbox_entry(char *fname, char *vname, int jdate, int jtime)
{

  int i, index, nclass, cc, len;
  int cdate, ctime, odate, otime;
  int too_old;
  int context;
  char entry[MAXENTRYLEN];
  char *name;
  struct pvmmboxinfo *mboxinfo;
  IOAPI_Bdesc3 bdesc3;
  IOAPI_Cdesc3 cdesc3;
  void nextimec();


/* get a fresh copy of the virtual file header */
  if (!read_header(fname, &bdesc3, &cdesc3, NON_BLOCKING, &context))
    return 0;

  sprintf(entry,"%s%c%s%c*:*", fname, DELIM_CHAR, vname, DELIM_CHAR);
  cc=pvm_getmboxinfo(entry, &nclass, &mboxinfo);
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_getmboxinfo (%s), for %s",cc,entry );
    }

  if (bdesc3.tstep == 0) return 1; /* we don't remove time-invariant records */
  too_old = -sec2timec(_NSteps2Keep*time2secc(abs(bdesc3.tstep)));
  odate = jdate;
  otime = jtime;
  nextimec(&odate,&otime, too_old);

/* remove old record(s) from mailbox */
  for (i=0;i<nclass; i++) {
    name = mboxinfo[i].mi_name;
    index = mboxinfo[i].mi_indices[0];
    len = STRLEN(name);
    sscanf(&name[len-14],"%07d:%06d",&cdate,&ctime);

    if (secsdiffc(cdate,ctime, odate, otime) >= 0) {
      cc = pvm_delinfo(name,index,PvmMboxDefault);
      if (cc < 0) {
	ERROR( "INTERNAL IOCPL ERROR: pvm_delinfo (%s), for %s",cc,name );
      }
#ifdef DEBUG
      printf("DEBUG:Deleting %s successful\n", name);
#endif
    }
  }

  return 1;
}

/* =========================================================== */

int find_earliest(char *fname, char *vname, int *sdate, int *stime)
{
  int i, nclass, cc, len;
  int nentries, cdate, ctime;
  int context;
  char entry[MAXENTRYLEN];
  char *name;
  struct pvmmboxinfo *mboxinfo;
  IOAPI_Bdesc3 bdesc3;
  IOAPI_Cdesc3 cdesc3;


  sprintf(entry,"%s%c%s%c*", fname, DELIM_CHAR, vname, DELIM_CHAR);
  cc=pvm_getmboxinfo(entry, &nclass, &mboxinfo);
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_getmboxinfo (%s), for %s",cc,entry );
    }


/*
   scan all the records in the mailbox, skip the ones scheduled to be deleted
   and compute the new sdate, stime
*/
  if (nclass == 0) {
    if (!read_header(fname, &bdesc3, &cdesc3, NON_BLOCKING, &context))
      return 0;
    if (bdesc3.tstep == 0) {
      *sdate = 0;
      *stime = 0;
    }
    else {
      *sdate = bdesc3.sdate;
      *stime = bdesc3.stime;
    }
  }
  else {
    for (i=0;i<nclass; i++) {
      name = mboxinfo[i].mi_name;
      if ((nentries=mboxinfo[i].mi_nentries) != 1) {
	ERROR("Multiple entries in mailbox %s", nentries, name);
      }
      len = STRLEN(name);
      sscanf(&name[len-14],"%07d:%06d",&cdate,&ctime);
      if (i==0) {
	*sdate = cdate;
	*stime = ctime;
      }
      else {
	if (secsdiffc(cdate,ctime, *sdate, *stime) >= 0) {
	  *sdate = cdate;
	  *stime = ctime;
	}
      }
    }
  }
  return 1;
}

/* =========================================================== */
int desc3net(char *name, IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p)
{
  int dummy_context;
  return read_header(name, bdesc3p, cdesc3p, NON_BLOCKING, &dummy_context);
}

/* =========================================================== */
int close3net(char *name)
{

  cpl_warning("WARNING: CLOSE3 is NOP for virtual files: %s",0, name);
  return 1;
}

/* =========================================================== */
int shut3net()
{
  int cc;
  int old_setopt;
  int ret_val=1;
  WriterList *wp;

  int getWaitForSynchro();
  void attempt2sleep();

  if (! _iocpl_mode) return 1;
  if (getWaitForSynchro()) {
    goto cleanup;
  }

  attempt2sleep();

  if (! iocpl_barrier(BARRIER_1_TAG)) {
    ret_val = 0;
    goto cleanup;
  }

  if(mhid != 0) {
    cc=pvm_delmhf(mhid);
    mhid = 0;
    if (cc < 0)  {
      ERROR("INTERNAL IOCPL ERROR: pvm_delmph (%s), in %s", cc, "shut3net" );
    }
  }

  for (wp=wp_head; wp != NULL; wp = wp->next) {
    cc=pvm_delmhf(wp->mhid);
    if (cc < 0)  {
      ERROR("INTERNAL IOCPL ERROR: pvm_delmph (%s), in %s", cc, "shut3net" );
    }
  }
    
  if (! iocpl_barrier(BARRIER_2_TAG)) {
    ret_val = 0;
    goto cleanup;
  }


 cleanup:
  old_setopt = pvm_setopt(PvmAutoErr, 0); /* shut up the annoying libpvm 
					     message about pvm_delmhf 
					     being not found; since 
					     pvm3.4.beta6 is trying to call 
					     pvm_delmh again on pvm_exit */

  delete_readlist(&rp_head);
  delete_writelist(&wp_head);
  delete_netfilelist(&flp_head);
  delete_tidlist(&ttl_head);
  delete_tidlist(&ntl_head);

  pvm_exit();
  _iocpl_mode=0;

  return ret_val;

}

/* =========================================================== */
/* =========================================================== */

int iocpl_barrier(int msg_wait)
{
  int i;
  int mytid, cc;
  int nclass, bufid;
  int context, old_context;
  int tid;
  int NumNetTasks;
  struct pvmmboxinfo *mboxinfo;
  struct timeval tmout;
  ReaderList *rp;
  WriterList *wp;

  mytid = pvm_mytid();
  if (mytid < 0) return 0;

  /* first send confirmation message to all writers */
  bufid = pvm_initsend( PvmDataDefault );
  if ( bufid < 0 )
    {
      ERROR("INTERNAL IOCPL ERROR: pvm_initsend (%s), in %s",
	    bufid, "iocpl_barrier" );
    }
  for (rp=rp_head; rp != NULL; rp = rp->next) {
    old_context = pvm_setcontext(rp->context);
    if (!checkIfOwnerAlive(rp->tid)) continue;
    cc = pvm_send(rp->tid, msg_wait);
    if (cc < 0) {
      ERROR("INTERNAL IOCPL ERROR: pvm_send (%s), in %s",
	    cc, "iocpl_barrier" );
    }
    context = pvm_setcontext(old_context);
  }

  for (wp=wp_head; wp != NULL; wp = wp->next) {

    cc = pvm_getmboxinfo(wp->file, &nclass, &mboxinfo);
    if (cc < 0) {
      ERROR( "INTERNAL IOCPL ERROR: pvm_getmboxinfo (%s) in %s",
	     cc,"iocpl_barrier");
    }
    NumNetTasks = mboxinfo->mi_nentries;
#ifdef DEBUG
fprintf(stderr,"DEBUG:: iocpl_barrier: waiting for %d MESG for file %s\n",
	NumNetTasks, wp->file);
#endif
    old_context=pvm_setcontext(wp->context);
    for (i=0; i < NumNetTasks; i++ ) {
      tid = mboxinfo->mi_owners[i];
      if (tid == mytid) continue;
    try_again:
      if (!checkIfOwnerAlive(tid)) continue;
      if (tid > 0) {
	_iocpl_oper=BLOCKING_RECV;
	_iocpl_owner_tid = tid;
	/* BLOCKING_READ */
	tmout.tv_sec=60;
	tmout.tv_usec=0;
	cc=pvm_trecv(tid, msg_wait, &tmout);
      	_iocpl_oper=NON_BLOCKING;
	_iocpl_owner_tid = -1;
	if (cc == 0) goto try_again;
	if (cc < 0) {
	  ERROR("INTERNAL IOCPL ERROR: pvm_recv (%s), in %s",
		cc, "iocpl_barrier" );
	}
      }
    }
    context=pvm_setcontext(old_context);
  }
  return 1;
}
/* =========================================================== */
/* =========================================================== */

int pack_desc3(IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p, int context)
{

  int cc;

  cc = pvm_pkint(&context,1,1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_pkint (%s), in %s",
	   cc,"pack_decs3");
  }

  cc=pvm_pkdouble(&(bdesc3p->p_alp), 9, 1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_pkdouble (%s), in %s",cc,"pack_decs3");
  }
  cc=pvm_pkint(&(bdesc3p->ftype), 16, 1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_pkint (%s), in %s",cc,"pack_decs3");
  }
  cc=pvm_pkreal(&(bdesc3p->vgtop), (MXLAYS3 + 2), 1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_pkreal (%s), in %s",cc,"pack_decs3");
  }
  cc=pvm_pkint(bdesc3p->vtype, MXVARS3, 1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_pkint (%s), in %s",cc,"pack_decs3");
  }

  cc=pvm_pkbyte(cdesc3p->gdnam, sizeof(IOAPI_Cdesc3), 1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_pkbyte (%s), in %s",cc,"pack_decs3");
  }

  return 1;
}

/* =========================================================== */
int unpack_desc3(IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p, int *context)
{

  int cc;
  cc = pvm_upkint(context,1,1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_upkint (%s), in %s",cc,"unpack_decs3");
  }

  cc=pvm_upkdouble(&(bdesc3p->p_alp),9,1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_upkdouble (%s), in %s",cc,"uppack_decs3");
  }
  cc=pvm_upkint(&(bdesc3p->ftype),16,1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_upkint (%s), in %s",cc,"uppack_decs3");
  }
  cc=pvm_upkreal(&(bdesc3p->vgtop), (MXLAYS3 + 2), 1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_upkreal (%s), in %s",cc,"uppack_decs3");
  }
  cc=pvm_upkint(bdesc3p->vtype, MXVARS3, 1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_upkint (%s), in %s",cc,"uppack_decs3");
  }

  cc=pvm_upkbyte(cdesc3p->gdnam, sizeof(IOAPI_Cdesc3), 1);
  if (cc < 0) {
    ERROR( "INTERNAL IOCPL ERROR: pvm_upkbyte (%s), in %s",cc,"uppack_decs3");
  }

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
  m3mesgc(errmsg);
  return;
}

/* =========================================================== */
/*         PVM Message Handler Routines                        */
/* =========================================================== */
int taskExit(int mid)
{
  int tid;
  int oldbuf;
  int found;
  char errmsg[MAXERRMSG];
  ReaderList *rp;
  TidList **tlpp;
  int cc;
  int old_setopt;
  void setWaitForSynchro(int);

  oldbuf=pvm_setrbuf(mid);
  pvm_upkint(&tid,1,1);
  /* restore r-buffer */
  pvm_freebuf( pvm_setrbuf( oldbuf ) );

  /* update terminated tasks list */
  found = 0;
  for (tlpp=&ttl_head; *tlpp != NULL; tlpp = &(*tlpp)->next) {
    if ((*tlpp)->tid == tid) {
      found = 1;
      break;
    }
  }
  if (!found) {
    *tlpp = new_tidlist(tid);
  }

  /* ALT: do not let task exit unsynchronously
  setWaitForSynchro(2);
  ALT */



  if (_iocpl_oper != NON_BLOCKING && _iocpl_owner_tid == tid ) {
    if (_iocpl_oper == BLOCKING_RECV) return 0;

    for (rp=rp_head; rp != NULL; rp = rp->next) {
      if (tid == rp->tid) break;
    }

    sprintf(errmsg,
	    "PANIC: from %s: TASK \"%s\" exited abnormally!\n\
             Returning FALSE to caller\n", 
	     mypgmname, rp ? rp->pgm: "Unknown");

    m3mesgc(errmsg);

    old_setopt = pvm_setopt(PvmAutoErr, 0); /* shut up annoying libpvm */ 
    cc = pvm_recvinfo("Non_existing_mailbox_to_force_failure_on_blocking_read",
		      0, PvmMboxDefault);
    pvm_setopt(PvmAutoErr, old_setopt);

    if ( cc < 0 )
      {
	return 1;
      }
  }

  return 0;

}

/* =========================================================== */

int taskReader(int mid)
{
  int tid;
  int oldbuf;
  char fname[MAXPATHNAME];
#ifdef DEBUG
  char errmsg[MAXERRMSG];
#endif

  oldbuf=pvm_setrbuf(mid);
  pvm_upkint(&tid,1,1);
  pvm_upkstr(fname);
  /* restore r-buffer */
  pvm_setrbuf(oldbuf);

  notifyOwner(tid);

#ifdef DEBUG
  sprintf(errmsg,"Got a reader: TID (%x) for file file %s", tid, fname);
  m3mesgc(errmsg);
#endif

  return 0; /* success */

}

/* =========================================================== */
/*               Link list manipulation routines               */
/* =========================================================== */

WriterList *new_writelist(int context, int mhid, char *file)
{
  WriterList *wp;

  wp = (WriterList *)malloc(sizeof(WriterList));
  if (wp != NULL) {
    wp->next = NULL;
    wp->context = context;
    wp->mhid = mhid;
    wp->file = strdup(file);
  }

  return wp;

}

/* =========================================================== */
ReaderList *new_readlist(int tid, int context, char *pgm)
{
  ReaderList *rp;

  rp = (ReaderList *)malloc(sizeof(ReaderList));
  if (rp != NULL) {
    rp->next = NULL;
    rp->tid = tid;
    rp->context = context;
    rp->pgm = strdup(pgm);
  }

  return rp;

}

/* =========================================================== */

NetFileList *new_netfilelist(int tid, char *file)
{
  NetFileList *flp;

  flp = (NetFileList *)malloc(sizeof(NetFileList));
  if (flp != NULL) {
    flp->next = NULL;
    flp->owner_tid = tid;
    flp->filename = strdup(file);
  }

  return flp;

}

/* =========================================================== */

TidList *new_tidlist(int tid)
{
  TidList *tlp;

  tlp = (TidList *)malloc(sizeof(TidList));
  if (tlp != NULL) {
    tlp->next = NULL;
    tlp->tid = tid;
  }

  return tlp;

}

/* =========================================================== */


void delete_readlist(ReaderList **headp)
{
  ReaderList **rpp, *rp, *rpnext;

  for (rpp=headp; *rpp != NULL; ) {
    rp = (*rpp);
    if (rp->pgm != NULL) {
      free(rp->pgm);
      rp->pgm = NULL;
    }
    
    rpnext = rp->next;
    rpp = &rpnext;
    free(rp);
    if (rpnext==NULL) break;
  }
  (*headp) = NULL;

  return;

}

/* =========================================================== */

void delete_writelist(WriterList **headp)
{
  int cc;
  WriterList **wpp, *wp, *wpnext;

  for (wpp=headp; *wpp != NULL; ) {
    wp = (*wpp);
    if (wp->file != NULL) {
      free(wp->file);
      wp->file = NULL;
    }

    cc = pvm_freecontext(wp->context);

    wpnext = wp->next;
    wpp = &wpnext;
    free(wp);
    if (wpnext==NULL) break;
  }
  (*headp) = NULL;

  return;

}

/* =========================================================== */
void delete_netfilelist(NetFileList **headp)
{
  NetFileList **nflpp, *nflp, *nflpnext;

  for (nflpp=headp; *nflpp != NULL; ) {
    nflp = (*nflpp);
    if (nflp->filename != NULL) {
      free(nflp->filename);
      nflp->filename = NULL;
    }

    nflpnext = nflp->next;
    nflpp = &nflpnext;
    free(nflp);
    if (nflpnext==NULL) break;
  }
  (*headp) = NULL;

  return;

}

/* =========================================================== */
void delete_tidlist(TidList **headp)
{
  TidList **tlpp, *tlp, *tlpnext;

  for (tlpp=headp; *tlpp != NULL; ) {
    tlp = (*tlpp);
    tlpnext = tlp->next;
    tlpp = &tlpnext;
    free(tlp);
    if (tlpnext==NULL) break;
  }
  (*headp) = NULL;

  return;

}

/* =========================================================== */

int getOwnerTid(char *name)
{

  NetFileList *flp;

  for (flp=flp_head; flp != NULL; flp = flp->next) {
    if (!strcmp(flp->filename, name)) {
      return flp->owner_tid;
    }
  }

  return PvmNotFound;  /* any negative number would do */

}

/* =========================================================== */

int checkIfOwnerAlive(int tid)
{
  TidList *tlp;

  /* go over the list of known tids that have exited */
  for (tlp=ttl_head; tlp != NULL; tlp = tlp->next) {
    if (tlp->tid == tid) {
      return 0;
    }
  }

  return 1;

}

/* =========================================================== */

int notifyOwner(int tid)
{
  int cc;
  int mytid;
  TidList **tlpp;
  
  mytid = pvm_mytid();
  if (mytid == tid) return 0;

  for (tlpp=&ntl_head; *tlpp != NULL; tlpp = &(*tlpp)->next) {
    if ((*tlpp)->tid == tid) {
      return 0;
    }
  }
  *tlpp = new_tidlist(tid);

  cc=pvm_notify(PvmTaskExit,NOTIFY_UPON_EXIT_TAG,1,&tid);
  if ( cc < 0 )
    {
      ERROR( "INTERNAL IOCPL ERROR: pvm_notify (%s) in %s",cc,
	     "notifyOwner" );
    }

  return 1;
}
/* =========================================================== */

/*
 * A version string.
 */
#define SKIP_UNUSED 32 /* # of chars prior to the actual version */
#define XSTRING(x)      #x
#define STRING(x)       XSTRING(x)
static const char _iocpl_libvers[] =
        "\044Id: \100(#) iocpl library version " STRING(VERSION) " of "__DATE__" "__TIME__" $";

const char *iocpl_version()
{

  return &_iocpl_libvers[SKIP_UNUSED];
}


/* =========================================================== */
/* =========================================================== */
/* =========================================================== */

/* =========================================================== */
/*       Task Synchronization routines (last part of the file) */
/* =========================================================== */



/* =========================================================== */
int getWaitForSynchro()
{
  return _WaitForSynchro;
}

/* =========================================================== */
void setWaitForSynchro(int new_value)
{

  _WaitForSynchro=new_value;

  return;

}

/* =========================================================== */
void attempt2sleep()
{
  char errmsg[MAXERRMSG];
  char *sleep_str;
  int sleep_val;
  unsigned int time2sleep;

  if ((sleep_str=getenv("IOAPI_EXIT_SLEEP")) == NULL) return;
  sleep_val = atoi(sleep_str);
  if (sleep_val == 0) return;
  if (sleep_val < 0) {
    sprintf(errmsg, "Env. var. IOAPI_EXIT_SLEEP should not be negative (%d) sleep ignored!", 
	    sleep_val);
    m3mesgc(errmsg);
    return;
  }

  sprintf(errmsg, "IOAPI/iocpl to going to sleep for %d seconds", sleep_val);
  m3mesgc(errmsg);

  time2sleep = sleep_val;
  do {
  } while ((time2sleep = sleep(time2sleep)) > 0);

  return;
}

/* =========================================================== */

#endif      /*  ifdef IOAPICPL   */
