/* wrgr.c (write graph to plain text file) */

/***********************************************************************
*  This code is part of GLPK (GNU Linear Programming Kit).
*
*  Copyright (C) 2009-2016 Andrew Makhorin, Department for Applied
*  Informatics, Moscow Aviation Institute, Moscow, Russia. All rights
*  reserved. E-mail: <mao@gnu.org>.
*
*  GLPK is free software: you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  GLPK is distributed in the hope that it will be useful, but WITHOUT
*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
*  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
*  License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with GLPK. If not, see <http://www.gnu.org/licenses/>.
***********************************************************************/

#include "env.h"
#include "glpk.h"

#define xfprintf glp_format

/***********************************************************************
*  NAME
*
*  glp_write_graph - write graph to plain text file
*
*  SYNOPSIS
*
*  int glp_write_graph(glp_graph *G, const char *fname).
*
*  DESCRIPTION
*
*  The routine glp_write_graph writes the specified graph to a plain
*  text file.
*
*  RETURNS
*
*  If the operation was successful, the routine returns zero. Otherwise
*  it prints an error message and returns non-zero. */

int glp_write_graph(glp_graph *G, const char *fname)
{     glp_file *fp;
      glp_vertex *v;
      glp_arc *a;
      int i, count, ret;
      xprintf("Writing graph to '%s'...\n", fname);
      fp = glp_open(fname, "w"), count = 0;
      if (fp == NULL)
      {  xprintf("Unable to create '%s' - %s\n", fname, get_err_msg());
         ret = 1;
         goto done;
      }
      xfprintf(fp, "%d %d\n", G->nv, G->na), count++;
      for (i = 1; i <= G->nv; i++)
      {  v = G->v[i];
         for (a = v->out; a != NULL; a = a->t_next)
            xfprintf(fp, "%d %d\n", a->tail->i, a->head->i), count++;
      }
#if 0 /* FIXME */
      xfflush(fp);
#endif
      if (glp_ioerr(fp))
      {  xprintf("Write error on '%s' - %s\n", fname, get_err_msg());
         ret = 1;
         goto done;
      }
      xprintf("%d lines were written\n", count);
      ret = 0;
done: if (fp != NULL) glp_close(fp);
      return ret;
}

/* eof */
