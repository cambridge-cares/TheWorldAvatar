/* rdgr.c (read graph from plain text file) */

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
#include "glpsdf.h"

/***********************************************************************
*  NAME
*
*  glp_read_graph - read graph from plain text file
*
*  SYNOPSIS
*
*  int glp_read_graph(glp_graph *G, const char *fname);
*
*  DESCRIPTION
*
*  The routine glp_read_graph reads a graph from a plain text file.
*
*  RETURNS
*
*  If the operation was successful, the routine returns zero. Otherwise
*  it prints an error message and returns non-zero. */

int glp_read_graph(glp_graph *G, const char *fname)
{     glp_data *data;
      jmp_buf jump;
      int nv, na, i, j, k, ret;
      glp_erase_graph(G, G->v_size, G->a_size);
      xprintf("Reading graph from '%s'...\n", fname);
      data = _glp_sdf_open_file(fname);
      if (data == NULL)
      {  ret = 1;
         goto done;
      }
      if (setjmp(jump))
      {  ret = 1;
         goto done;
      }
      _glp_sdf_set_jump(data, jump);
      nv = _glp_sdf_read_int(data);
      if (nv < 0)
         _glp_sdf_error(data, "invalid number of vertices\n");
      na = _glp_sdf_read_int(data);
      if (na < 0)
         _glp_sdf_error(data, "invalid number of arcs\n");
      xprintf("Graph has %d vert%s and %d arc%s\n",
         nv, nv == 1 ? "ex" : "ices", na, na == 1 ? "" : "s");
      if (nv > 0) glp_add_vertices(G, nv);
      for (k = 1; k <= na; k++)
      {  i = _glp_sdf_read_int(data);
         if (!(1 <= i && i <= nv))
            _glp_sdf_error(data, "tail vertex number out of range\n");
         j = _glp_sdf_read_int(data);
         if (!(1 <= j && j <= nv))
            _glp_sdf_error(data, "head vertex number out of range\n");
         glp_add_arc(G, i, j);
      }
      xprintf("%d lines were read\n", _glp_sdf_line(data));
      ret = 0;
done: if (data != NULL) _glp_sdf_close_file(data);
      return ret;
}

/* eof */
