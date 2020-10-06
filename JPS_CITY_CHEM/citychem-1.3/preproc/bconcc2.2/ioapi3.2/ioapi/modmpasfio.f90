
MODULE MODMPASFIO

    !!.........................................................................
    !!  Version "$Id: modmpasfio.f90 67 2017-11-22 21:07:51Z coats $"
    !!  Copyright (c) 2017 Carlie J. Coats, Jr. and UNC Institute for the Environment
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!  See file "LGPL.txt" for conditions of use.
    !!........................................................................
    !!
    !!  DESCRIPTION:
    !!      Holds 2-D grid description and grid-related routines for a specified
    !!      unstructured MPAS grid.
    !!
    !!      NOTE 1:  Single-precision version
    !!
    !!      NOTE 2:  Latitudes and Longitudes are converted from MPAS "radians"
    !!      usage to 0 <= LON < 360 (_not_ ISO 6709 compliant) degrees.
    !!
    !!      NOTE 3:  Non-sphere case, and periodic and weighted meshes are not supported.
    !!
    !!  REVISION  HISTORY:
    !!      Beta version  05/2017 by Carlie J. Coats, Jr., Ph.D.
    !!      Version       05/17/2017 by CJC:  bug-fix in time-dimension handling
    !!      Version       08/11/2017 by CJC:  bug-fixes for ALONE, KAREAS
    !!      Version       09/07/2017 by CJC:  add extra global attributes "Conventions"
    !!          and "model_name", required for NCAR's Java "MPASConventionl.java"
    !!      Version       09/18/2017 by CJC:  add standard variables "edgesOnEdge"
    !!                   "weightsOnEdge", and "meshDensity"
    !!      Version       09/29/2017 by CJC:  bug-fix in CREATEMPAS()
    !!      Version       10/25/2017 by CJC:  Fix for SPHEREDIST() fInternational-Date-Line
    !                     issues.  X-Y-Z based SPHEREDIST().  Generic MPINTERP()
    !!      Version       11/01/2017 by CJC:  fix MPINTERP() generic
    !!      Version       11/11/2017 by CJC:  Add MPCELLMATX() generic, integer-array
    !!          versions of MPINTERP(); OMP-threadsafe FINDCELL()
    !!      Version       11/11/2017 by CJC:  handle weighting for out-of-grid cases in VERTWT()
    !!...................................................................................

    USE MODNCFIO
    USE M3UTILIO

    IMPLICIT NONE

    PUBLIC :: INITMPGRID, INITREARTH, SHUTMPGRID, OPENMPAS, CREATEMPAS,     &
              DESCMPAS, READMPSTEPS, WRITEMPSTEP, READMPAS, WRITEMPAS,      &
              ARC2MPAS, FINDCELL, FINDVRTX, SPHEREDIST, MPSTR2DT, MPDT2STR, &
              BARYFAC, MPBARYMATX, MPBARYMULT, MPCELLMATX, MPINTERP


    !!........   Generic interfaces:

    INTERFACE INITMPGRID
        MODULE PROCEDURE  INITMPGRIDA, INITMPGRIDF
    END INTERFACE INITMPGRID

    INTERFACE CREATEMPAS
        MODULE PROCEDURE  CREATEMPAS1, CREATEMPAS2
    END INTERFACE CREATEMPAS

    INTERFACE DESCMPAS
        MODULE PROCEDURE  DESCMPAS1, DESCMPAS2
    END INTERFACE DESCMPAS

    INTERFACE READMPAS
        MODULE PROCEDURE                                                           &
            READMPAS0DD,  READMPAS1DD,  READMPAS2DD,  READMPAS3DD,  READMPAS4DD,   &
            READMPAS0DR,  READMPAS1DR,  READMPAS2DR,  READMPAS3DR,  READMPAS4DR,   &
            READMPAS0DI,  READMPAS1DI,  READMPAS2DI,  READMPAS3DI,  READMPAS4DI,   &
            READMPAS0DS,  READMPAS1DS,  READMPAS2DS,  READMPAS3DS,  READMPAS4DS,   &
            READMPAS0DB,  READMPAS1DB,  READMPAS2DB,  READMPAS3DB,  READMPAS4DB,   &
            READMPAS0DDT, READMPAS1DDT, READMPAS2DDT, READMPAS3DDT, READMPAS4DDT,  &
            READMPAS0DRT, READMPAS1DRT, READMPAS2DRT, READMPAS3DRT, READMPAS4DRT,  &
            READMPAS0DIT, READMPAS1DIT, READMPAS2DIT, READMPAS3DIT, READMPAS4DIT,  &
            READMPAS0DST, READMPAS1DST, READMPAS2DST, READMPAS3DST, READMPAS4DST,  &
            READMPAS0DBT, READMPAS1DBT, READMPAS2DBT, READMPAS3DBT, READMPAS4DBT
    END INTERFACE READMPAS

    INTERFACE WRITEMPAS
        MODULE PROCEDURE                                                                &
            WRITEMPAS0DD,  WRITEMPAS1DD,  WRITEMPAS2DD,  WRITEMPAS3DD,  WRITEMPAS4DD,   &
            WRITEMPAS0DR,  WRITEMPAS1DR,  WRITEMPAS2DR,  WRITEMPAS3DR,  WRITEMPAS4DR,   &
            WRITEMPAS0DI,  WRITEMPAS1DI,  WRITEMPAS2DI,  WRITEMPAS3DI,  WRITEMPAS4DI,   &
            WRITEMPAS0DS,  WRITEMPAS1DS,  WRITEMPAS2DS,  WRITEMPAS3DS,  WRITEMPAS4DS,   &
            WRITEMPAS0DB,  WRITEMPAS1DB,  WRITEMPAS2DB,  WRITEMPAS3DB,  WRITEMPAS4DB,   &
            WRITEMPAS0DDT, WRITEMPAS1DDT, WRITEMPAS2DDT, WRITEMPAS3DDT, WRITEMPAS4DDT,  &
            WRITEMPAS0DRT, WRITEMPAS1DRT, WRITEMPAS2DRT, WRITEMPAS3DRT, WRITEMPAS4DRT,  &
            WRITEMPAS0DIT, WRITEMPAS1DIT, WRITEMPAS2DIT, WRITEMPAS3DIT, WRITEMPAS4DIT,  &
            WRITEMPAS0DST, WRITEMPAS1DST, WRITEMPAS2DST, WRITEMPAS3DST, WRITEMPAS4DST,  &
            WRITEMPAS0DBT, WRITEMPAS1DBT, WRITEMPAS2DBT, WRITEMPAS3DBT, WRITEMPAS4DBT
    END INTERFACE WRITEMPAS

    INTERFACE ARC2MPAS
        MODULE PROCEDURE  ARC2MPAS2D, ARC2MPAS3D, ARC2MPAS3D1, ARC2MPAS3D2
    END INTERFACE ARC2MPAS

    INTERFACE MPINTERP
        MODULE PROCEDURE  MPINTERP0DI,  MPINTERP1DI,  MPINTERP2DI,                                  &
                          MPINTERP0RI,  MPINTERP1RI,  MPINTERP2RI,                                  &
                          MPINTERPL0DI, MPINTERPL1DI, MPINTERPL2DI,                                 &
                          MPINTERPL0RI, MPINTERPL1RI, MPINTERPL2RI,                                 &
                          MPINTERP0DD,  MPINTERP1DD,  MPINTERP2DD,  MPINTERPE2DD,  MPINTERPG2DD,    &
                          MPINTERP0DF,  MPINTERP1DF,  MPINTERP2DF,  MPINTERPE2DF,  MPINTERPG2DF,    &
                          MPINTERPL0DD, MPINTERPL1DD, MPINTERPL2DD, MPINTERPEL2DD, MPINTERPGL2DD,   &
                          MPINTERPL0DF, MPINTERPL1DF, MPINTERPL2DF, MPINTERPEL2DF, MPINTERPGL2DF
    END INTERFACE MPINTERP

    INTERFACE MPCELLMATX
        MODULE PROCEDURE MPCELLMATX1F, MPCELLMATX1D, MPCELLMATX2F, MPCELLMATX2D
    END INTERFACE MPCELLMATX

    INTERFACE MPBARYMATX
        MODULE PROCEDURE MPBARYMATX1F, MPBARYMATX1D, MPBARYMATX1DF,     &
                         MPBARYMATX2F, MPBARYMATX2D, MPBARYMATX2DF,     &
                         MPBARYEMTX2F, MPBARYEMTX2D, MPBARYEMTX2DF,     &
                         MPBARYGMTX2F, MPBARYGMTX2D, MPBARYGMTX2DF
    END INTERFACE MPBARYMATX

    INTERFACE MPBARYMULT
        MODULE PROCEDURE MPBARYMULT1F1, MPBARYMULT1FL, MPBARYMULT1D1, MPBARYMULT1DL,    &
                         MPBARYMULT2F1, MPBARYMULT2FL, MPBARYMULT2D1, MPBARYMULT2DL
    END INTERFACE MPBARYMULT

    INTERFACE SPHEREDIST
        MODULE PROCEDURE  DISTD, DISTR, DISTDM, DISTRM, DXYZD, DXYZR, DXYZDM, DXYZRM
    END INTERFACE SPHEREDIST

    INTERFACE FINDCELL
        MODULE PROCEDURE  FINDCELLD, FINDCELLF
    END INTERFACE FINDCELL

    INTERFACE FINDVRTX
        MODULE PROCEDURE  FINDVRTXD, FINDVRTXF
    END INTERFACE FINDVRTX

    INTERFACE CHKFILL     !!  PRIVATE procedure to check input-data against netCDF fill-values
        MODULE PROCEDURE                                                           &
            CHKFILL_0DD,  CHKFILL_1DD,  CHKFILL_2DD,  CHKFILL_3DD,  CHKFILL_4DD,   &
            CHKFILL_0DR,  CHKFILL_1DR,  CHKFILL_2DR,  CHKFILL_3DR,  CHKFILL_4DR,   &
            CHKFILL_0DI,  CHKFILL_1DI,  CHKFILL_2DI,  CHKFILL_3DI,  CHKFILL_4DI,   &
            CHKFILL_0DS,  CHKFILL_1DS,  CHKFILL_2DS,  CHKFILL_3DS,  CHKFILL_4DS,   &
            CHKFILL_0DB,  CHKFILL_1DB,  CHKFILL_2DB,  CHKFILL_3DB,  CHKFILL_4DB
    END INTERFACE CHKFILL


    !!........   Parameters:  required MPAS-file "header" structure:

    INTEGER     , PUBLIC, PARAMETER :: MPSTRLEN  = 64
    INTEGER     , PUBLIC, PARAMETER :: NMPASDIMS = 11
    INTEGER     , PUBLIC, PARAMETER :: NMPASVARS = 36

    CHARACTER(LEN=16), PUBLIC, PARAMETER :: MPASDIMNAMES( NMPASDIMS ) = (/   &
           'Time           ',       &       !!  1
           'TWO            ',       &       !!  2
           'StrLen         ',       &       !!  3
           'nCells         ',       &       !!  4
           'nEdges         ',       &       !!  5
           'nVertices      ',       &       !!  6
           'vertexDegree   ',       &       !!  7
           'maxEdges       ',       &       !!  8
           'maxEdges2      ',       &       !!  9
           'nVertLevels    ',       &       !! 10
           'nVertLevelsP1  '   /)           !! 11

    CHARACTER(LEN=MPSTRLEN), PUBLIC, PARAMETER :: MPASVARNAMES( NMPASVARS ) =  (/  &
           'indexToCellID       ',  &       !!  1
           'indexToEdgeID       ',  &       !!  2
           'indexToVertexID     ',  &       !!  3
           'nEdgesOnCell        ',  &       !!  4
           'nEdgesOnEdge        ',  &       !!  5
           'cellsOnCell         ',  &       !!  6
           'edgesOnCell         ',  &       !!  7
           'verticesOnCell      ',  &       !!  8
           'cellsOnEdge         ',  &       !!  9
           'edgesOnEdge         ',  &       !! 10
           'verticesOnEdge      ',  &       !! 11
           'cellsOnVertex       ',  &       !! 12
           'edgesOnVertex       ',  &       !! 13
           'latCell             ',  &       !! 14
           'lonCell             ',  &       !! 15
           'latEdge             ',  &       !! 16
           'lonEdge             ',  &       !! 17
           'latVertex           ',  &       !! 18
           'lonVertex           ',  &       !! 19
           'xCell               ',  &       !! 20
           'yCell               ',  &       !! 21
           'zCell               ',  &       !! 22
           'xEdge               ',  &       !! 23
           'yEdge               ',  &       !! 24
           'zEdge               ',  &       !! 25
           'xVertex             ',  &       !! 26
           'yVertex             ',  &       !! 27
           'zVertex             ',  &       !! 28
           'weightsOnEdge       ',  &       !! 29
           'dvEdge              ',  &       !! 30
           'dcEdge              ',  &       !! 31
           'angleEdge           ',  &       !! 32
           'areaCell            ',  &       !! 33
           'areaTriangle        ',  &       !! 34
           'kiteAreasOnVertex   ',  &       !! 35
           'meshDensity         '   /)      !! 36

    INTEGER, PUBLIC, PROTECTED, SAVE :: NMPASDIMIDS( NMPASDIMS )     !!  netCDF dimension-IDs
    INTEGER, PUBLIC, PROTECTED, SAVE :: MPASDIMSIZE( NMPASDIMS )     !!  netCDF dimension-extents
    INTEGER, PUBLIC, PROTECTED, SAVE :: MPASTIMEDID = IMISS3         !!  time-dimension ID


    !!........   Public variables:  MPAS-file "header":
    !!........   required attributes, variables and dimensions

    CHARACTER*64, PUBLIC, PROTECTED, SAVE :: ONSPHERE  = 'YES'      !!  should be "YES"
    CHARACTER*64, PUBLIC, PROTECTED, SAVE :: MESH_ID   = CMISS3     !!  for tracking mesh provenance.
    CHARACTER*64, PUBLIC, PROTECTED, SAVE :: MESH_SPEC = CMISS3     !!  version of MPAS Mesh specification

    INTEGER, PUBLIC, PROTECTED, SAVE :: MPSTEPS = 0  !!  # of time steps
    INTEGER, PUBLIC, PROTECTED, SAVE :: MPCELLS      !!  # of primary cells in the mesh
    INTEGER, PUBLIC, PROTECTED, SAVE :: MPEDGES      !!  # of edges in the mesh
    INTEGER, PUBLIC, PROTECTED, SAVE :: MPVRTXS      !!  # of vertices in the mesh
    INTEGER, PUBLIC, PROTECTED, SAVE :: MPVLVLS      !!  # of vertical levels
    INTEGER, PUBLIC, PROTECTED, SAVE :: MPVORDR      !!  max vertex-order:  # of cells/edges per vertex
    INTEGER, PUBLIC, PROTECTED, SAVE :: MPBNDYC      !!  max # of vertices/edges per cell
    INTEGER, PUBLIC, PROTECTED, SAVE :: MPBNDY2      !!  2 * max # of vertices/edges per cell

    REAL(8), PUBLIC, PROTECTED, SAVE :: REARTH = 6370.0d3           !! from MM5/WRF-ARW usage

    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: MPDATES(:)    !! (MPSTEPS): Julian dates YYYYDDD
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: MPTIMES(:)    !! (MPSTEPS): Julian times  HHMMSS

    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: CELLID(:)     !! (MPCELLS): global cell-ID
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: EDGEID(:)     !! (MPEDGES): global edge-ID
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: VRTXID(:)     !! (MPVRTXS): global vertex-ID

    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: NBNDYE(:)     !! (MPCELLS): # of edges per cell
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: NEDGEE(:)     !! (MPEDGES): # of edges per edge (0 or 1)

    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: BNDYCELL(:,:) !! (MPBNDYC,MPCELLS): boundary-cell indices for this cell
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: BNDYEDGE(:,:) !! (MPBNDYC,MPCELLS): boundary-edge indices for this cell
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: BNDYVRTX(:,:) !! (MPBNDYC,MPCELLS): boundary-vertex indices for this cell
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  ::   ECELLS(:,:) !! (2,MPEDGES): cell indices for this edge
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  ::   EVRTXS(:,:) !! (2,MPEDGES): vertex indices for this edge
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  ::   VCELLS(:,:) !! (MPVORDR,MPVRTXS): Cell indices that radiate from a given vertex.
    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  ::   VEDGES(:,:) !! (MPVORDR,MPVRTXS): Edge indices that radiate from a given vertex

    INTEGER, PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: EEDGES(:,:) !! (MPBNDY2,MPEDGES): Edge indices used to reconstruct tangential velocities.

    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: ALATC(:)      !! (MPCELLS):  latitude-degrees for cell-centers
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: ALONC(:)      !! (MPCELLS): longitude-degrees for cell-centers:  0 <= alon < 360.0d0
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: ALATE(:)      !! (MPEDGES):  latitude-degrees for edge-centers
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: ALONE(:)      !! (MPEDGES): longitude-degrees for edge-centers:  0 <= alon < 360.0d0
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: ALATV(:)      !! (MPVRTXS):  latitude-degrees for vertices
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: ALONV(:)      !! (MPVRTXS): longitude-degrees for vertices:  0 <= alon < 360.0d0

    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: XCELL(:)      !! (MPCELLS): X-coordinate for cell-centers
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: YCELL(:)      !! (MPCELLS): Y-coordinate for cell-centers
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: ZCELL(:)      !! (MPCELLS): Z-coordinate for cell-centers
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: XEDGE(:)      !! (MPCELLS): X-coordinate for edge-centers
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: YEDGE(:)      !! (MPCELLS): Y-coordinate for edge-centers
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: ZEDGE(:)      !! (MPCELLS): Z-coordinate for edge-centers
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: XVRTX(:)      !! (MPVRTXS): X-coordinate for vertices
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: YVRTX(:)      !! (MPVRTXS): Y-coordinate for vertices
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: ZVRTX(:)      !! (MPVRTXS): Z-coordinate for vertice
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: EWGHTS(:,:)   !! (MPBNDY2,MPEDGES): weights used to reconstruct tangential velocities.
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: DVEDGE(:)     !! (MPEDGES):  edge lengths (M)
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: DCEDGE(:)     !! (MPEDGES):  distance between the cell-centers that saddle a given edge (M)
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: EANGLE(:)     !! (MPEDGES):  angle from edge-normal vector to Easting
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: CAREAS(:)     !! (MPCELLS):  cell areas (M^2)
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: VAREAS(:)     !! (MPVRTXS):  dual-mesh triangle areas (M^2)
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: KAREAS(:,:)   !! (MPBNDYC,MPCELLS):  kite-area:  intersection of cell with dual-cell centered at vertex
    REAL(8), PUBLIC, PROTECTED, SAVE, ALLOCATABLE  :: MSHDEN(:)     !! (MPCELLS):  mesh density (none)


    !!........   Private variables and parameters:

    PRIVATE         !!  everything else

    LOGICAL, SAVE :: INITFLAG = .FALSE.

    REAL(8), PARAMETER :: PI      = 3.14159265358979324d0
    REAL(8), PARAMETER :: PI180   = PI / 180.0d0
    REAL(8), PARAMETER :: RPI180  = 180.0d0 / PI
    REAL   , PARAMETER :: PI180F  = PI180            !!  as single-precision
    REAL   , PARAMETER :: RPI180F = RPI180

    CHARACTER*8, PARAMETER :: NCTYPES( 10 ) =                              &
         (/ 'INT1    ', 'CHAR    ', 'INT2    ', 'INT     ', 'REAL    ',    &
            'REAL8   ', 'UINT1   ', 'UINT2   ', 'UINT    ', 'INT8    '     /)

    !!........   File-Properties table (input and output files):

    INTEGER     , SAVE :: MPCOUNT = 0                            !!  current number of active output MP-files

    CHARACTER*16, SAVE :: MPFILES ( MXFILE3 ) = CMISS3           !!  logical file-names table
    INTEGER     , SAVE :: MPCDFID ( MXFILE3 )                    !!  netCDF file IDs
    INTEGER     , SAVE :: MPTIMDID( MXFILE3 ) = IMISS3           !!  netCDF time-dimension IDs
    INTEGER     , SAVE :: MPTWODID( MXFILE3 ) = IMISS3           !!  netCDF "TWO"-dimension IDs
    INTEGER     , SAVE :: MPSTRDID( MXFILE3 ) = IMISS3           !!  netCDF "STRLEN"-dimension IDs
    INTEGER     , SAVE :: MPCELDID( MXFILE3 ) = IMISS3           !!  netCDF cell-dimension IDs
    INTEGER     , SAVE :: MPEDGDID( MXFILE3 ) = IMISS3           !!  netCDF edge-dimension IDs
    INTEGER     , SAVE :: MPVRTDID( MXFILE3 ) = IMISS3           !!  netCDF vertex-dimension IDs
    INTEGER     , SAVE :: MPBDYDID( MXFILE3 ) = IMISS3           !!  netCDF bdy-cell-dimension IDs
    INTEGER     , SAVE :: MP2BDYID( MXFILE3 ) = IMISS3           !!  netCDF bdy-cell-dimension IDs
    INTEGER     , SAVE :: MPDEGDID( MXFILE3 ) = IMISS3           !!  netCDF vertex-degree dimension IDs
    INTEGER     , SAVE :: MPLVLDID( MXFILE3 ) = IMISS3           !!  netCDF level-dimension IDs
    INTEGER     , SAVE :: MPNRECS ( MXFILE3 ) = 0                !!  netCDF max step-number (1,2,...)
    INTEGER     , SAVE :: MPNVARS ( MXFILE3 )                    !!  number of variables, per file
    INTEGER     , SAVE :: MPNCELLS( MXFILE3 )                    !!  number of cells, per file
    INTEGER     , SAVE :: MPNEDGES( MXFILE3 )                    !!  number of edges, per file
    INTEGER     , SAVE :: MPNVRTXS( MXFILE3 )                    !!  number of vertics, per file
    INTEGER     , SAVE :: MPNVLVLS( MXFILE3 )                    !!  number of levels, per file
    INTEGER     , SAVE :: MPNVORDR( MXFILE3 )                    !!  max cells per vertex, per file
    INTEGER     , SAVE :: MPNBNDYC( MXFILE3 )                    !!  max edges per cell, per file
    CHARACTER*32, SAVE :: MPVNAME ( MXVARS3, MXFILE3 ) = CMISS3  !!  variable-names table
    INTEGER     , SAVE :: MPVARID ( MXVARS3, MXFILE3 )           !!  netCDF variable-IDs
    INTEGER     , SAVE :: MPVTYPE ( MXVARS3, MXFILE3 )           !!  netCDF variable-types (M3REAL, etc.)
    INTEGER     , SAVE :: MPVDCNT ( MXVARS3, MXFILE3 )           !!  dimension-count per variable
    CHARACTER*32, SAVE :: MPVDNAM ( 7, MXVARS3, MXFILE3 )        !!  dimension-names  "
    INTEGER     , SAVE :: MPVDIDS ( 7, MXVARS3, MXFILE3 )        !!  dimension-IDs    "
    INTEGER     , SAVE :: MPVDIMS ( 7, MXVARS3, MXFILE3 )        !!  dimension-sizes  "

    LOGICAL     , SAVE :: VERBOSE  = .FALSE.
    LOGICAL     , SAVE :: CHK_FILL = .FALSE.


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    !!...................................................................................
    !!  Generic interfaces:
    !!      Initialize state variables from a file or from arguments (not yet implemented);
    !!      initialize Earth-radius.
    !!...................................................................................


    LOGICAL FUNCTION INITREARTH( RADEARTH )     !!  initialize REARTH from argument

        REAL(8), INTENT( IN ) :: RADEARTH        !!  Earth-radius (Meters)

        REARTH      = RADEARTH
        INITREARTH = .TRUE.
        RETURN

    END  FUNCTION INITREARTH


    !!.......................................................................
    !!  initialize MODMPASFIO from arguments:


    LOGICAL FUNCTION INITMPGRIDA( NCELLS,           &
                                  NEDGES,           &
                                  NVRTXS,           &
                                  NVLVLS,           &
                                  NVORDR,           &
                                  NBNDYC,           &
                                  NBNDY2,           &
                                  CELLID1,          &
                                  EDGEID1,          &
                                  VRTXID1,          &
                                  NBNDYE1,          &
                                  NEDGEE1,          &
                                  BYCELL1,          &
                                  BYEDGE1,          &
                                  BYVRTX1,          &
                                  ECELLS1,          &
                                  EVRTXS1,          &
                                  VCELLS1,          &
                                  VEDGES1,          &
                                  EEDGES1,          &
                                  ALATC1,           &
                                  ALONC1,           &
                                  ALATE1,           &
                                  ALONE1,           &
                                  ALATV1,           &
                                  ALONV1,           &
                                  XCELL1,           &
                                  YCELL1,           &
                                  ZCELL1,           &
                                  XEDGE1,           &
                                  YEDGE1,           &
                                  ZEDGE1,           &
                                  XVRTX1,           &
                                  YVRTX1,           &
                                  ZVRTX1,           &
                                  EWGHTS1,          &
                                  DVEDGE1,          &
                                  DCEDGE1,          &
                                  EANGLE1,          &
                                  CAREAS1,          &
                                  VAREAS1,          &
                                  KAREAS1,          &
                                  MSHDEN1,          &
                                  ONSPHERE1,        &
                                  MESH_ID1,         &
                                  MESH_SPEC1,       &
                                  REARTH1 )

        !!........  Arguments

        INTEGER, INTENT( IN ) :: NCELLS                     !!  # of primary cells in the mesh
        INTEGER, INTENT( IN ) :: NEDGES                     !!  # of edges in the mesh
        INTEGER, INTENT( IN ) :: NVRTXS                     !!  # of vertices in the mesh
        INTEGER, INTENT( IN ) :: NVLVLS                     !!  # of vertical levels
        INTEGER, INTENT( IN ) :: NVORDR                     !!  max vertex-order:  # of cells/edges per vertex
        INTEGER, INTENT( IN ) :: NBNDYC                     !!  max # of vertices/edges per cell
        INTEGER, INTENT( IN ) :: NBNDY2                     !!  2 * max # of vertices/edges per cell
        INTEGER, INTENT( IN ) :: CELLID1( NCELLS )          !! (NCELLS): global cell-ID
        INTEGER, INTENT( IN ) :: EDGEID1( NEDGES )          !! (NEDGES): global edge-ID
        INTEGER, INTENT( IN ) :: VRTXID1( NVRTXS )          !! (NVRTXS): global vertex-ID
        INTEGER, INTENT( IN ) :: NBNDYE1( NCELLS )          !! (NCELLS): # of edges per cell
        INTEGER, INTENT( IN ) :: NEDGEE1( NEDGES )          !! (NEDGES): # of edges per edge (0 or 1)
        INTEGER, INTENT( IN ) :: BYCELL1( NBNDYC,NCELLS )   !! (NBNDYC,NCELLS): boundary-cell indices for this cell
        INTEGER, INTENT( IN ) :: BYEDGE1( NBNDYC,NCELLS )   !! (NBNDYC,NCELLS): boundary-edge indices for this cell
        INTEGER, INTENT( IN ) :: BYVRTX1( NBNDYC,NCELLS )   !! (NBNDYC,NCELLS): boundary-vertex indices for this cell
        INTEGER, INTENT( IN ) :: ECELLS1(      2,NEDGES )   !! (2,NEDGES): cell indices for this edge
        INTEGER, INTENT( IN ) :: EVRTXS1(      2,NEDGES )   !! (2,NEDGES): vertex indices for this edge
        INTEGER, INTENT( IN ) :: VCELLS1( NVORDR,NVRTXS )   !! (NVORDR,NVRTXS): Cell indices that radiate from a given vertex.
        INTEGER, INTENT( IN ) :: VEDGES1( NVORDR,NVRTXS )   !! (NVORDR,NVRTXS): Edge indices that radiate from a given vertex
        INTEGER, INTENT( IN ) :: EEDGES1( NBNDY2,NEDGES )   !! (NBNDY2,NEDGES): Edge indices used to reconstruct tangential velocities.
        REAL(8), INTENT( IN ) ::  ALATC1( NCELLS )          !! (NCELLS):  latitude-degrees for cell-centers
        REAL(8), INTENT( IN ) ::  ALONC1( NCELLS )          !! (NCELLS): longitude-degrees for cell-centers
        REAL(8), INTENT( IN ) ::  ALATE1( NEDGES )          !! (NEDGES):  latitude-degrees for edge-centers
        REAL(8), INTENT( IN ) ::  ALONE1( NEDGES )          !! (NEDGES): longitude-degrees for edge-centers
        REAL(8), INTENT( IN ) ::  ALATV1( NVRTXS )          !! (NVRTXS):  latitude-degrees for vertices
        REAL(8), INTENT( IN ) ::  ALONV1( NVRTXS )          !! (NVRTXS): longitude-degrees for vertices
        REAL(8), INTENT( IN ) ::  XCELL1( NCELLS )          !! (NCELLS): X-coordinate for cell-centers
        REAL(8), INTENT( IN ) ::  YCELL1( NCELLS )          !! (NCELLS): Y-coordinate for cell-centers
        REAL(8), INTENT( IN ) ::  ZCELL1( NCELLS )          !! (NCELLS): Z-coordinate for cell-centers
        REAL(8), INTENT( IN ) ::  XEDGE1( NEDGES )          !! (NCELLS): X-coordinate for edge-centers
        REAL(8), INTENT( IN ) ::  YEDGE1( NEDGES )          !! (NCELLS): Y-coordinate for edge-centers
        REAL(8), INTENT( IN ) ::  ZEDGE1( NEDGES )          !! (NCELLS): Z-coordinate for edge-centers
        REAL(8), INTENT( IN ) ::  XVRTX1( NVRTXS )          !! (NVRTXS): X-coordinate for vertices
        REAL(8), INTENT( IN ) ::  YVRTX1( NVRTXS )          !! (NVRTXS): Y-coordinate for vertices
        REAL(8), INTENT( IN ) ::  ZVRTX1( NVRTXS )          !! (NVRTXS): Z-coordinate for vertice
        REAL(8), INTENT( IN ) :: EWGHTS1( 2*NBNDYC,NEDGES ) !! (NBNDY2,NEDGES): weights used to reconstruct tangential velocities.
        REAL(8), INTENT( IN ) :: DVEDGE1( NEDGES )          !! (NEDGES):  edge lengths(M)
        REAL(8), INTENT( IN ) :: DCEDGE1( NEDGES )          !! (NEDGES):  distance between the cell-centers that saddle a given edge(M)
        REAL(8), INTENT( IN ) :: EANGLE1( NEDGES )          !! (NEDGES):  angle from edge-normal vector to Easting
        REAL(8), INTENT( IN ) :: CAREAS1( NCELLS )          !! (NCELLS):  cell areas (M^2)
        REAL(8), INTENT( IN ) :: VAREAS1( NVRTXS )          !! (NVRTXS):  dual-mesh triangle areas (M^2)
        REAL(8), INTENT( IN ) :: KAREAS1( NVORDR, NVRTXS )  !! (NBNDYC,NCELLS):  kite-area:  intersection of cell with dual-cell centered at vertex
        REAL(8), INTENT( IN ) :: MSHDEN1( NCELLS )          !! (NCELLS):  mesh density (none)

        CHARACTER*24, PARAMETER :: PNAME = 'MODMPASFIO/INITMPGRID():'

        CHARACTER*64, OPTIONAL, INTENT( IN ) :: ONSPHERE1
        CHARACTER*64, OPTIONAL, INTENT( IN ) :: MESH_ID1
        CHARACTER*64, OPTIONAL, INTENT( IN ) :: MESH_SPEC1
        REAL(8),      OPTIONAL, INTENT( IN ) :: REARTH1    !!  Earth-radius (M)

        !!........   Local Variables:

        INTEGER         MXDIM, ISTAT, LOG
        LOGICAL         EFLAG
        CHARACTER*512   MESG


        !!......................   begin body of function  ....................................

        IF ( INITFLAG ) THEN
            CALL M3MESG( PNAME // '  already initialized)' )
            INITMPGRIDA = .TRUE.
            RETURN
        END IF

        LOG = INIT3()
        WRITE( LOG, '( 5X, A )' )   'Module MODMPASFIO',                    &
        'Version $Id: modmpasfio.f90 67 2017-11-22 21:07:51Z coats $',&
        'Copyright (C) 2017 Carlie J. Coats, Jr., Ph.D. and',               &
        'UNC Institute for the Environment.',                               &
        'Distributed under the GNU LESSER GENERAL PUBLIC LICENSE v 2.1',    &
        ''

        EFLAG = .FALSE.                  ! if dscgrid() failed for output grid

        VERBOSE = ENVYN( 'MPAS_VERBOSE', 'Verbose log output?', .FALSE., ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = PNAME // 'Bad environment variable "MPAS_VERBOSE"'
            CALL M3MESG( MESG )
        END IF

        CHK_FILL = ENVYN( 'MPAS_CHKFILL', 'Check input against NCF fill-values?', .TRUE., ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = PNAME // 'Bad environment variable "MPAS_CHKFILL"'
            CALL M3MESG( MESG )
        END IF

        IF ( NBNDY2 .NE. 2*NBNDYc ) THEN
            EFLAG = .TRUE.
            MESG  = PNAME // 'Bad arguments:  NBNDY2 vs NBNDYC'
            CALL M3MESG( MESG )
        END IF

        IF ( EFLAG ) THEN
            INITMPGRIDA = .FALSE.
            RETURN
        END IF

        !!........   Allocate the variables

        MPSTEPS = 0
        MXDIM   = MAX( NCELLS, NEDGES, NVORDR*NVRTXS )
        ALLOCATE( MPDATES( MPSTEPS ),              &
                  MPTIMES( MPSTEPS ),              &
                   CELLID( NCELLS ),               &
                   EDGEID( NEDGES ),               &
                   VRTXID( NVRTXS ),               &
                   NBNDYE( NCELLS ),               &
                   NEDGEE( NEDGES ),               &
                 BNDYCELL( NBNDYC, NCELLS ),       &
                 BNDYEDGE( NBNDYC, NCELLS ),       &
                 BNDYVRTX( NBNDYC, NCELLS ),       &
                   ECELLS(      2, NEDGES ),       &
                   EVRTXS(      2, NEDGES ),       &
                   VCELLS( NVORDR, NVRTXS ),       &
                   VEDGES( NVORDR, NVRTXS ),       &
                   EEDGES( NBNDY2, NEDGES ),       &
                   EWGHTS( NBNDY2, NEDGES ),       &
                    ALATC( NCELLS ),               &
                    ALONC( NCELLS ),               &
                    ALATE( NEDGES ),               &
                    ALONE( NEDGES ),               &
                    ALATV( NVRTXS ),               &
                    ALONV( NVRTXS ),               &
                    XCELL( NCELLS ),               &
                    YCELL( NCELLS ),               &
                    ZCELL( NCELLS ),               &
                    XEDGE( NCELLS ),               &
                    YEDGE( NCELLS ),               &
                    ZEDGE( NCELLS ),               &
                    XVRTX( NVRTXS ),               &
                    YVRTX( NVRTXS ),               &
                    ZVRTX( NVRTXS ),               &
                   DVEDGE( NEDGES ),               &
                   DCEDGE( NEDGES ),               &
                   EANGLE( NEDGES ),               &
                   CAREAS( NCELLS ),               &
                   MSHDEN( NCELLS ),               &
                   VAREAS( NVRTXS ),               &
                   KAREAS( NVORDR, NVRTXS ),      STAT = ISTAT )

        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'Buffer allocation failed:  STAT=', ISTAT
            CALL M3EXIT( 'MODMPASFIO/INITMPGRID', 0, 0, MESG, 2 )
        END IF

        MPCELLS  = NCELLS
        MPEDGES  = NEDGES
        MPVRTXS  = NVRTXS
        MPVLVLS  = NVLVLS
        MPVORDR  = NVORDR
        MPBNDYC  = NBNDYC
        MPBNDY2  = NBNDY2

        CELLID   = CELLID1
        EDGEID   = EDGEID1
        VRTXID   = VRTXID1
        NBNDYE   = NBNDYE1
        NEDGEE   = NEDGEE1
        BNDYCELL = BYCELL1
        BNDYEDGE = BYEDGE1
        BNDYVRTX = BYVRTX1
        ECELLS   = ECELLS1
        EVRTXS   = EVRTXS1
        VCELLS   = VCELLS1
        VEDGES   = VEDGES1
        EEDGES   = EEDGES1
        ALATC    = ALATC1
        ALONC    = ALONC1
        ALATE    = ALATE1
        ALONE    = ALONE1
        ALATV    = ALATV1
        ALONV    = ALONV1
        XCELL    = XCELL1
        YCELL    = YCELL1
        ZCELL    = ZCELL1
        XEDGE    = XEDGE1
        YEDGE    = YEDGE1
        ZEDGE    = ZEDGE1
        XVRTX    = XVRTX1
        YVRTX    = YVRTX1
        ZVRTX    = ZVRTX1
        EWGHTS   = EWGHTS1
        DVEDGE   = DVEDGE1
        DCEDGE   = DCEDGE1
        EANGLE   = EANGLE1
        CAREAS   = CAREAS1
        VAREAS   = VAREAS1
        KAREAS   = KAREAS1
        MSHDEN   = MSHDEN1

        IF ( PRESENT( MESH_ID1   ) ) MESH_ID   = MESH_ID1
        IF ( PRESENT( MESH_SPEC1 ) ) MESH_SPEC = MESH_SPEC1
        IF ( PRESENT( ONSPHERE1  ) ) ONSPHERE  = ONSPHERE1
        IF ( PRESENT( REARTH1    ) ) REARTH    = REARTH1

        INITMPGRIDA = .TRUE.

        RETURN

    END  FUNCTION INITMPGRIDA



    !!.......................................................................


    LOGICAL FUNCTION INITMPGRIDF( FNAME )           !!  initialize MODMPASFIO from MPAS-file

        CHARACTER*(*), INTENT(IN   ) :: FNAME       !!  logical file name

        CHARACTER*1, PARAMETER :: BLANK = ' '

        INTEGER         FID, F, V, N, LOG
        INTEGER         ISTAT, ID, TID, MXDIM
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        CHARACTER*24, PARAMETER :: PNAME = 'MODMPASFIO/INITMPGRID():'

        !!......................   begin body of function

        IF ( INITFLAG ) THEN
            CALL M3MESG( PNAME // '  already initialized)' )
            INITMPGRIDF = .TRUE.
            RETURN
        END IF

        LOG = INIT3()
        WRITE( LOG, '( 5X, A )' )   'Module MODMPASFIO',                    &
        'Version $Id: modmpasfio.f90 67 2017-11-22 21:07:51Z coats $',&
        'Copyright (C) 2017 Carlie J. Coats, Jr., Ph.D.',                   &
        'and UNC Institute for the Environment.',                           &
        'Distributed under the GNU LESSER GENERAL PUBLIC LICENSE v 2.1',    &
        BLANK

        EFLAG = .FALSE.                  ! if dscgrid() failed for output grid

        VERBOSE = ENVYN( 'MPAS_VERBOSE', 'Verbose log output?', .FALSE., ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = PNAME // 'Bad environment variable "MPAS_VERBOSE"'
            CALL M3MESG( MESG )
        END IF

        CHK_FILL = ENVYN( 'MPAS_CHKFILL', 'Check input against NCF fill-values?', .TRUE., ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = PNAME // 'Bad environment variable "MPAS_CHKFILL"'
            CALL M3MESG( MESG )
        END IF

        F = 1
        IF ( .NOT. OPENMPAS( FNAME, FSREAD3 ) ) THEN
            EFLAG = .TRUE.
            MESG  = PNAME // ' Error opening "' //TRIM( FNAME )// '"'
            CALL M3MESG( MESG )
        END IF

        IF ( EFLAG ) THEN
            INITMPGRIDF = .FALSE.
            RETURN
        END IF


        FID     = MPCDFID( F )
        MPSTEPS = MPNRECS ( 1 )
        MPCELLS = MPNCELLS( 1 )
        MPEDGES = MPNEDGES( 1 )
        MPVRTXS = MPNVRTXS( 1 )
        MPVORDR = MPNVORDR( 1 )
        MPBNDYC = MPNBNDYC( 1 )
        MPBNDY2 = MPNBNDYC( 1 ) * 2
        MPVLVLS = MPNVLVLS( 1 )

        MPASDIMSIZE(  1 ) = NF_UNLIMITED
        MPASDIMSIZE(  2 ) = 2
        MPASDIMSIZE(  3 ) = MPSTRLEN
        MPASDIMSIZE(  4 ) = MPCELLS
        MPASDIMSIZE(  5 ) = MPEDGES
        MPASDIMSIZE(  6 ) = MPVRTXS
        MPASDIMSIZE(  7 ) = MPVORDR
        MPASDIMSIZE(  8 ) = MPBNDYC
        MPASDIMSIZE(  9 ) = MPBNDY2     !! = 2 * MPBNDYC
        MPASDIMSIZE( 10 ) = MPVLVLS
        MPASDIMSIZE( 11 ) = MPVLVLS + 1

        ISTAT = NF_GET_ATT_DOUBLE( FID, NF_GLOBAL, 'sphere_radius', REARTH )
        IF ( ISTAT .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( PNAME // ' Error reading "sphere_radius" from "' //TRIM(FNAME)// '"' )
            EFLAG = .TRUE.
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_GET_ATT_TEXT( FID, NF_GLOBAL, 'mesh_id', MESH_ID )
        IF ( ISTAT .NE. 0 ) THEN
            CALL M3MESG( ' ' )
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( 'WARNING:  The MPAS I/O spec REQUIRES header-attribute "mesh_id"' )
            MESG = PNAME // ' Error reading "mesh_id" from "' //TRIM(FNAME)// '": '
            CALL M3MESG( MESG )
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_GET_ATT_TEXT( FID, NF_GLOBAL, 'mesh_spec', MESH_SPEC )
        IF ( ISTAT .NE. 0 ) THEN
            CALL M3MESG( ' ' )
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( 'WARNING:  The MPAS I/O spec REQUIRES header-attribute "mesh_spec"' )
            MESG = PNAME // ' Error reading "mesh_spec" from "' //TRIM(FNAME)// '": '
            CALL M3MESG( MESG )
        END IF          !  ISTAT nonzero:  operation failed


        IF ( EFLAG ) THEN
            INITMPGRIDF = .FALSE.
            RETURN
        END IF

        WRITE( MESG, '( A, F10.0 )' ) '    REARTH = ', REARTH
        CALL M3MESG( MESG )
        CALL M3MESG( BLANK )

        MPCOUNT = 1

        !!........   Allocate the variables

        MXDIM = MAX( MPCELLS, MPEDGES, MPVORDR*MPVRTXS )
        ALLOCATE( MPDATES( MPSTEPS ),               &
                  MPTIMES( MPSTEPS ),               &
                   CELLID( MPCELLS ),               &
                   EDGEID( MPEDGES ),               &
                   VRTXID( MPVRTXS ),               &
                   NBNDYE( MPCELLS ),               &
                   NEDGEE( MPEDGES ),               &
                 BNDYCELL( MPBNDYC, MPCELLS ),      &
                 BNDYEDGE( MPBNDYC, MPCELLS ),      &
                 BNDYVRTX( MPBNDYC, MPCELLS ),      &
                   ECELLS(       2, MPEDGES ),      &
                   EVRTXS(       2, MPEDGES ),      &
                   VCELLS( MPVORDR, MPVRTXS ),      &
                   VEDGES( MPVORDR, MPVRTXS ),      &
                 EEDGES( 2*MPBNDYC, MPEDGES ),      &
                 EWGHTS( 2*MPBNDYC, MPEDGES ),      &
                    ALATC( MPCELLS ),               &
                    ALONC( MPCELLS ),               &
                    ALATE( MPEDGES ),               &
                    ALONE( MPEDGES ),               &
                    ALATV( MPVRTXS ),               &
                    ALONV( MPVRTXS ),               &
                    XCELL( MPCELLS ),               &
                    YCELL( MPCELLS ),               &
                    ZCELL( MPCELLS ),               &
                    XEDGE( MPCELLS ),               &
                    YEDGE( MPCELLS ),               &
                    ZEDGE( MPCELLS ),               &
                    XVRTX( MPVRTXS ),               &
                    YVRTX( MPVRTXS ),               &
                    ZVRTX( MPVRTXS ),               &
                   DVEDGE( MPEDGES ),               &
                   DCEDGE( MPEDGES ),               &
                   EANGLE( MPEDGES ),               &
                   CAREAS( MPCELLS ),               &
                   MSHDEN( MPCELLS ),               &
                   VAREAS( MPVRTXS ),               &
                   KAREAS( MPVORDR, MPVRTXS ),      STAT = ISTAT )

        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'Buffer allocation failed:  STAT=', ISTAT
            CALL M3EXIT( 'MODMPASFIO/INITMPGRID', 0, 0, MESG, 2 )
        END IF


        !!........   Read the standard variables

        V = MAX( INDEX1( 'xtime', MPNVARS( F ), MPVNAME( :,F ) ) ,  &
                 INDEX1( 'xTime', MPNVARS( F ), MPVNAME( :,F ) ) )

        IF ( V .GT. 0 ) THEN
            IF ( .NOT.READMPSTEPS( FNAME, MPVNAME( V,F ), N, MPSTEPS, MPDATES, MPTIMES ) )   EFLAG = .TRUE.
        END IF

        IF ( .NOT.READMPAS( FNAME, 'indexToCellID',   MPCELLS, CELLID ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'indexToEdgeID',   MPEDGES, EDGEID ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'indexToVertexID', MPVRTXS, VRTXID ) )   EFLAG = .TRUE.

        IF ( .NOT.READMPAS( FNAME, 'nEdgesOnCell',    MPCELLS, NBNDYE ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'nEdgesOnEdge',    MPEDGES, NEDGEE ) )   EFLAG = .TRUE.

        IF ( .NOT.READMPAS( FNAME, 'cellsOnCell',     MPBNDYC, MPCELLS, BNDYCELL ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'edgesOnCell',     MPBNDYC, MPCELLS, BNDYEDGE ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'edgesOnEdge',   2*MPBNDYC, MPEDGES, EEDGES   ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'verticesOnCell',  MPBNDYC, MPCELLS, BNDYVRTX ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'cellsOnEdge',           2, MPEDGES, ECELLS   ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'verticesOnEdge',        2, MPEDGES, EVRTXS   ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'cellsOnVertex',   MPVORDR, MPVRTXS, VCELLS   ) )   EFLAG = .TRUE.
        IF ( .NOT.READMPAS( FNAME, 'edgesOnVertex',   MPVORDR, MPVRTXS, VEDGES   ) )   EFLAG = .TRUE.

        IF ( .NOT.READR8_1D( FNAME, 'latCell',      F, MPCELLS, ALATC  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'lonCell',      F, MPCELLS, ALONC  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'latEdge',      F, MPEDGES, ALATE  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'lonEdge',      F, MPEDGES, ALONE  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'latVertex',    F, MPVRTXS, ALATV  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'lonVertex',    F, MPVRTXS, ALONV  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'xCell',        F, MPCELLS, XCELL  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'yCell',        F, MPCELLS, YCELL  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'zCell',        F, MPCELLS, ZCELL  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'xEdge',        F, MPEDGES, XEDGE  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'yEdge',        F, MPEDGES, YEDGE  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'zEdge',        F, MPEDGES, ZEDGE  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'xVertex',      F, MPVRTXS, XVRTX  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'yVertex',      F, MPVRTXS, YVRTX  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'zVertex',      F, MPVRTXS, ZVRTX  ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'dvEdge',       F, MPEDGES, DVEDGE ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'dcEdge',       F, MPEDGES, DCEDGE ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'angleEdge',    F, MPEDGES, EANGLE ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'areaCell',     F, MPCELLS, CAREAS ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'areaTriangle', F, MPVRTXS, VAREAS ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_1D( FNAME, 'meshDensity',  F, MPCELLS, MSHDEN ) )   EFLAG = .TRUE.

        IF ( .NOT.READR8_2D( FNAME, 'weightsOnEdge',     F, 2*MPBNDYC, MPEDGES, EWGHTS ) )   EFLAG = .TRUE.
        IF ( .NOT.READR8_2D( FNAME, 'kiteAreasOnVertex', F,   MPVORDR, MPVRTXS, KAREAS ) )   EFLAG = .TRUE.

        IF ( EFLAG ) THEN
            DEALLOCATE( MPDATES, MPTIMES,                                   &
                        CELLID, EDGEID, VRTXID, NBNDYE, NEDGEE,             &
                        BNDYCELL, BNDYEDGE, BNDYVRTX, ECELLS, EVRTXS,       &
                        VCELLS, VEDGES, EEDGES, EWGHTS, ALATC, ALONC,       &
                        ALATE, ALONE, ALATV, ALONV, XCELL, YCELL, ZCELL,    &
                        XEDGE, YEDGE, ZEDGE, XVRTX, YVRTX, ZVRTX, MSHDEN,   &
                        DVEDGE, DCEDGE, EANGLE, CAREAS, VAREAS, KAREAS, STAT = ISTAT )
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' ) 'Deallocation failed:  STAT=', ISTAT
                CALL M3EXIT( 'MODMPASFIO/INITMPGRID', 0, 0, MESG, 2 )
            END IF
            INITMPGRIDF = .FALSE.
            RETURN
        END IF


        !!........   Convert lat-lon coordinates to degrees:

        DO N = 1, MPCELLS
            ALATC( N ) =      RPI180 * ALATC( N )
            ALONC( N ) = MOD( RPI180 * ALONC( N ) + 360.0D0, 360.0D0 )
        END DO

        DO N = 1, MPEDGES
            ALATE( N )  =      RPI180 * ALATE( N )
            ALONE( N )  = MOD( RPI180 * ALONE( N ) + 360.0D0, 360.0D0 )
            EANGLE( N ) =      RPI180 * EANGLE( N )
        END DO

        DO N = 1, MPVRTXS
            ALATV( N ) =      RPI180 * ALATV( N )
            ALONV( N ) = MOD( RPI180 * ALONV( N ) + 360.0D0, 360.0D0 )
        END DO


        INITFLAG    = .TRUE.
        INITMPGRIDF = ( .NOT.EFLAG )
        RETURN


    END FUNCTION INITMPGRIDF


    !!.......................................................................


    LOGICAL FUNCTION  READR8_1D( FNAME, VNAME, F, N, R8BUF )

        CHARACTER*(*), INTENT(IN    ) :: FNAME       !!  logical file name
        CHARACTER*(*), INTENT(IN    ) :: VNAME       !!  variable-name
        INTEGER       , INTENT(IN   ) :: F           !!  file-index
        INTEGER       , INTENT(IN   ) :: N           !!  dimension
        REAL(8)       , INTENT(  OUT) :: R8BUF( N )

        CHARACTER*24, PARAMETER :: PNAME = 'MODMPASFIO/INITMPGRID():'

        REAL            R4BUF( N )
        INTEGER         V
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        !!......................   begin body of function

        EFLAG = .FALSE.

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )

        IF ( V .LE. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = PNAME // ' Standard variable "' // TRIM( VNAME ) // '" not found in "' //TRIM(FNAME)
            CALL M3MESG( MESG )

        ELSE IF ( MPVTYPE( V,F ) .EQ. M3DBLE ) THEN

            IF ( .NOT.READMPAS( FNAME, VNAME, N, R8BUF ) )   EFLAG = .TRUE.

        ELSE IF ( MPVTYPE( V,F ) .EQ. M3REAL ) THEN

            MESG = 'WARNING:  precision for variable "' // TRIM( VNAME ) // '" inadequate for high resolution modeling'
            CALL M3MESG( MESG )
            IF ( .NOT.READMPAS( FNAME, VNAME, N, R4BUF ) )   EFLAG = .TRUE.
            R8BUF = DBLE( R4BUF )

        ELSE

            EFLAG = .TRUE.
            MESG  = PNAME // ' Wrong type for variable "' // TRIM( VNAME ) // '" in "' //TRIM(FNAME)
            CALL M3MESG( MESG )

        END IF

        READR8_1D = ( .NOT.EFLAG )
        RETURN

    END FUNCTION  READR8_1D


    !!.......................................................................


    LOGICAL FUNCTION  READR8_2D( FNAME, VNAME, F, M, N, R8BUF )

        CHARACTER*(*), INTENT(IN    ) :: FNAME       !!  logical file name
        CHARACTER*(*), INTENT(IN    ) :: VNAME       !!  variable-name
        INTEGER       , INTENT(IN   ) :: F           !!  file-index
        INTEGER       , INTENT(IN   ) :: M, N        !!  dimensions
        REAL(8)       , INTENT(  OUT) :: R8BUF( M, N )

        CHARACTER*24, PARAMETER :: PNAME = 'MODMPASFIO/INITMPGRID():'

        REAL            R4BUF( M, N )
        INTEGER         V
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        !!......................   begin body of function

        EFLAG = .FALSE.

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )

        IF ( V .LE. 0 ) THEN
            EFLAG = .TRUE.
            MESG  = PNAME // ' Standard variable "' // TRIM( VNAME ) // '" not found in "' //TRIM(FNAME)
            CALL M3MESG( MESG )

        ELSE IF ( MPVTYPE( V,F ) .EQ. M3DBLE ) THEN

            IF ( .NOT.READMPAS( FNAME, VNAME, M, N, R8BUF ) )   EFLAG = .TRUE.

        ELSE IF ( MPVTYPE( V,F ) .EQ. M3REAL ) THEN

            IF ( .NOT.READMPAS( FNAME, VNAME, M, N, R4BUF ) )   EFLAG = .TRUE.
            R8BUF = DBLE( R4BUF )

        ELSE

            EFLAG = .TRUE.
            MESG  = PNAME // ' Wrong type for variable "' // TRIM( VNAME ) // '" in "' //TRIM(FNAME)
            CALL M3MESG( MESG )

        END IF

        READR8_2D = ( .NOT.EFLAG )
        RETURN

    END FUNCTION  READR8_2D


    !!.......................................................................
    !!  Generic interfaces:
    !!      Distance between two points
    !!.......................................................................
    !!  Forms using Lat-Lon coordinates for the point
    !!      Spherical distance-squared from <XLAT, XLON> to <YLAT, YLON>
    !!      using Haversine formula
    !!
    !!  See https://en.wikipedia.org/wiki/Great-circle_distance
    !!.......................................................................

    REAL   FUNCTION  DISTR( RADE, XLAT, XLON, YLAT, YLON )

        REAL, INTENT( IN ) :: RADE                    !! Earth-radius (Meters)
        REAL, INTENT( IN ) :: XLAT, XLON, YLAT, YLON  !! Lat-Lon (degrees)

        REAL    YY, C1, C2, SS1, SS2

        !!......................   begin body of function

        C1   = COS( PI180F * XLAT )
        C2   = COS( PI180F * YLAT )
        IF ( XLON .LT. 0.0 .AND. YLON .GE. 180.0 ) THEN
            YY = YLON - 360.0
        ELSE
            YY = YLON
        END IF
        SS1  = SIN( 0.5 * PI180F * ( XLAT - YLAT ) ) **2
        SS2  = SIN( 0.5 * PI180F * ( XLON -   YY ) ) **2

        DISTR = 2.0 * RADE * ASIN( SQRT( SS1 + C1 * C2 * SS2 ) )
        RETURN

    END FUNCTION  DISTR


    !!.......................................................................


    REAL(8) FUNCTION  DISTD( RADE, XLAT, XLON, YLAT, YLON )

        REAL(8), INTENT( IN ) :: RADE                    !! Earth-radius (Meters)
        REAL(8), INTENT( IN ) :: XLAT, XLON, YLAT, YLON  !! Lat-Lon (degrees)

        REAL(8)  YY, C1, C2, SS1, SS2

        !!......................   begin body of function

        C1   = COS( PI180 * XLAT )
        C2   = COS( PI180 * YLAT )
        IF ( XLON .LT. 0.0D0 .AND. YLON .GE. 180.0D0 ) THEN
            YY = YLON - 360.0D0
        ELSE
            YY = YLON
        END IF
        SS1  = SIN( 0.5d0 * PI180 * ( XLAT - YLAT ) ) **2
        SS2  = SIN( 0.5d0 * PI180 * ( XLON -   YY ) ) **2

        DISTD = 2.0D0 * RADE * ASIN( SQRT( SS1 + C1 * C2 * SS2 ) )
        RETURN

    END FUNCTION  DISTD


    !!.......................................................................


    REAL   FUNCTION  DISTRM( XLAT, XLON, YLAT, YLON )

        REAL, INTENT( IN ) :: XLAT, XLON, YLAT, YLON

        REAL    RADE

        RADE  = REARTH
        DISTRM = DISTR( RADE, XLAT, XLON, YLAT, YLON )     !!  use module-vble for Earth-radius
        RETURN

    END FUNCTION  DISTRM


    !!.......................................................................


    REAL(8) FUNCTION  DISTDM( XLAT, XLON, YLAT, YLON )

        REAL(8), INTENT( IN ) :: XLAT, XLON, YLAT, YLON

        DISTDM = DISTD( REARTH, XLAT, XLON, YLAT, YLON )     !!  use module-vble for Earth-radius
        RETURN

    END FUNCTION  DISTDM


    !!.......................................................................
    !!  Foirms using MPAS Cartesian X-Y-Z coordinates
    !!.......................................................................

    REAL   FUNCTION  DXYZR( RADE, X1, Y1, Z1, X2, Y2, Z2 )

        REAL, INTENT( IN ) :: RADE                    !! Earth-radius  (Meters)
        REAL, INTENT( IN ) :: X1, Y1, Z1, X2, Y2, Z2  !! Cartesian XYZ (Meters)

        REAL    DSQ

        !!......................   begin body of function

        DSQ = ( X2 - X1 )**2 + ( Y2 - Y2 )**2 + ( Z2 - Z1 )**2

        DXYZR = 2.0 * RADE * ASIN( 0.5 * SQRT( DSQ )/RADE )
        RETURN

    END FUNCTION  DXYZR


    !!.......................................................................

    REAL(8)  FUNCTION  DXYZD( RADE, X1, Y1, Z1, X2, Y2, Z2 )

        REAL(8), INTENT( IN ) :: RADE                    !! Earth-radius  (Meters)
        REAL(8), INTENT( IN ) :: X1, Y1, Z1, X2, Y2, Z2  !! Cartesian XYZ (Meters)

        REAL(8)   DSQ

        !!......................   begin body of function

        DSQ = ( X2 - X1 )**2 + ( Y2 - Y2 )**2 + ( Z2 - Z1 )**2

        DXYZD = 2.0D0* RADE * ASIN( 0.5D0 * SQRT( DSQ )/RADE )
        RETURN

    END FUNCTION  DXYZD


    !!.......................................................................

    REAL   FUNCTION  DXYZRM( X1, Y1, Z1, X2, Y2, Z2 )

        REAL, INTENT( IN ) :: X1, Y1, Z1, X2, Y2, Z2  !! Cartesian XYZ (Meters)

        REAL    DSQ, RADE

        !!......................   begin body of function

        DSQ  = ( X2 - X1 )**2 + ( Y2 - Y2 )**2 + ( Z2 - Z1 )**2
        RADE = REARTH

        DXYZRM = 2.0 * RADE * ASIN( 0.5 * SQRT( DSQ )/RADE )
        RETURN

    END FUNCTION  DXYZRM


    !!.......................................................................

    REAL(8)   FUNCTION  DXYZDM( X1, Y1, Z1, X2, Y2, Z2 )

        REAL(8), INTENT( IN ) :: X1, Y1, Z1, X2, Y2, Z2  !! Cartesian XYZ (Meters)

        REAL(8)    DSQ

        !!......................   begin body of function

        DSQ = ( X2 - X1 )**2 + ( Y2 - Y2 )**2 + ( Z2 - Z1 )**2

        DXYZDM = 2.0D0 * REARTH * ASIN( 0.5D0 * SQRT( DSQ )/REARTH )
        RETURN

    END FUNCTION  DXYZDM


    !!.......................................................................
    !!  Find the subscript for the unstructured-grid cell that contains
    !!  the point <ALAT,ALON>
    !!.......................................................................

    INTEGER FUNCTION FINDCELLF( ALAT, ALON )

        REAL, INTENT( IN ) :: ALAT, ALON

        REAL(8)      ALATD, ALOND

        ALATD = DBLE( ALAT )
        ALOND = DBLE( ALON )
        FINDCELLF = FINDCELLD( ALATD, ALOND )

        RETURN

    END FUNCTION FINDCELLF


    !!.......................................................................


    INTEGER FUNCTION FINDCELLD( ALAT, ALON )

        REAL(8), INTENT( IN ) :: ALAT, ALON

        INTEGER, SAVE :: INDX = 1       !!  cell from last run.

        INTEGER     I, J, K, N

        REAL(8)     DMIN, DBDY

        !!......................   begin body of function  FINDCELL

        IF ( .NOT.INITFLAG ) THEN
            CALL M3MESG( 'MODMPASFIO/FINDCELL():  must call INITMPGRID() before FINDCELL()' )
            FINDCELLD = IMISS3
            RETURN
        END IF


        I    = INDX
        DMIN = SPHEREDIST( ALAT, ALON, ALATC( I ), ALONC( I ) )

        DO  !!........  Search-loop:  traverse all neighbors, finding min distance

            N = I

            DO J = 1, NBNDYE( I )
                K    = BNDYCELL( J,I )
                IF ( K .EQ. 0 )  CYCLE
                DBDY = SPHEREDIST( ALAT, ALON, ALATC( K ), ALONC( K ) )
                IF ( DBDY .LT. DMIN ) THEN
                    DMIN = DBDY
                    N    = K
                END IF
            END DO

            IF ( N .EQ. I )  EXIT

            I = N

        END DO  !!........  End search-loop

!$OMP   CRITICAL( MP_FIND )
        INDX = N
!$OMP   END CRITICAL( MP_FIND )

        FINDCELLD = N
        RETURN

    END FUNCTION FINDCELLD


    !!.......................................................................
    !!  Find the subscript for the dual-grid cell (triangles centered at vertices)
    !!  that contains the point <ALAT,ALON>
    !!.......................................................................

    INTEGER FUNCTION FINDVRTXF( ALAT, ALON )

        REAL, INTENT( IN ) :: ALAT, ALON

        REAL(8)     ALATD, ALOND

        ALATD = DBLE( ALAT )
        ALOND = DBLE( ALON )
        FINDVRTXF = FINDVRTXD( ALATD, ALOND )

        RETURN

    END FUNCTION FINDVRTXF


    !!.......................................................................


    INTEGER FUNCTION FINDVRTXD( ALAT, ALON )

        REAL(8), INTENT( IN ) :: ALAT, ALON

        INTEGER     I, J, K, V1, V2, M, N, V
        REAL(8)     X, Y, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3


        REAL(8)     DMIN, DBDY, LATV, LONV

        !!......................   begin body of function  FINDVRTX

        IF ( .NOT.INITFLAG ) THEN
            CALL M3MESG( 'MODMPASFIO/FINDVRTX():  must call INITMPGRID() before FINDVRTX()' )
            FINDVRTXD = IMISS3
            RETURN
        END IF

        M = FINDCELLD( ALAT, ALON )
        IF ( M .LT. 0 ) THEN
            FINDVRTXD = IMISS3
            RETURN
        ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
            FINDVRTXD = IMISS3
            RETURN
        END IF

        X = MOD( 360.0D0 + ALON, 360.0D0 )
        Y = ALAT
        DO J = 1,  NBNDYE( M )

            V  = BNDYVRTX( J,M )
            X1 = ALONC( VCELLS( 1,V ) )
            X2 = ALONC( VCELLS( 2,V ) )
            X3 = ALONC( VCELLS( 3,V ) )
            Y1 = ALATC( VCELLS( 1,V ) )
            Y2 = ALATC( VCELLS( 2,V ) )
            Y3 = ALATC( VCELLS( 3,V ) )

            IF ( BARYFAC( Y, X, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN
                FINDVRTXD = V
                RETURN
            END IF

        END DO

        FINDVRTXD = IMISS3
        RETURN

    END FUNCTION FINDVRTXD


    !!.......................................................................
    !!  interface:  2D version
    !!      Find the number of MPAS cells that intersect the arc from <ALAT,ALON>
    !!      to <ZLAT,ZLON>, the corresponding cell-indices and weights.
    !!.......................................................................

    LOGICAL FUNCTION ARC2MPAS2D( ALAT, ALON, ZLAT, ZLON, NMAX, NSEGS, CELLS, WGHTS )

        REAL   , INTENT(IN   ) :: ALAT, ALON, ZLAT, ZLON
        INTEGER, INTENT(IN   ) :: NMAX
        INTEGER, INTENT(  OUT) :: NSEGS
        INTEGER, INTENT(  OUT) :: CELLS( NMAX )
        REAL   , INTENT(  OUT) :: WGHTS( NMAX )

        REAL  , PARAMETER :: EPS = 1.0D-10

        INTEGER     IA, IZ, II
        INTEGER     I, J, K, KK, L, M, N, NN
        REAL        DARC
        REAL        XX, YY, X1, Y1, X2, Y2, XXX, YYY
        REAL        A, B, C, D, E, F, DET, U, V

        !!......................   begin body of function  ARC2MPAS2D

        IF ( .NOT.INITFLAG ) THEN
            CALL M3MESG( 'MODMPASFIO/ARC2MPAS2D():  must call INITMPGRID() before ARC2MPAS2D()' )
            ARC2MPAS2D = .FALSE.
            RETURN
        END IF

        IA = FINDCELL( ALAT, ALON )
        IZ = FINDCELL( ZLAT, ZLON )

        IF ( IA .EQ. IZ ) THEN
            NSEGS      = 1
            CELLS( 1 ) = I
            WGHTS( 1 ) = 1.0d0
            ARC2MPAS2D = .TRUE.
            RETURN
        END IF

        DARC = SPHEREDIST( ALAT, ALON, ZLAT, ZLON )

        YY = ALAT
        XX = ALON
        II = IA             !!  current cell index
        KK = -9999          !!  last edge-index

        DO NN = 1, NMAX     !!  loop finding intersections of current arc with current-cell edges

            DO J = 1, NBNDYE( II )

                N  = BNDYCELL( J,II )       !! <ZLAT,ZLON> in this bdy-cell?
                IF ( N .EQ. 0 )  CYCLE
                IF ( N .EQ. IZ ) THEN
                    NSEGS       = NN
                    CELLS( NN ) = N
                    WGHTS( NN ) = DISTRM( YY, XX, ZLAT, ZLON ) / DARC
                    ARC2MPAS2D = .TRUE.
                    RETURN
                END IF

                K  = BNDYEDGE( J,II )
                IF ( K .EQ. KK ) CYCLE      !!  skip the edge we're coming from, if any

                L  = EVRTXS( 1,K )
                M  = EVRTXS( 2,K )
                X1 = ALONV( L )
                Y1 = ALONV( L )
                X2 = ALONV( M )
                Y2 = ALONV( M )

                !!  Solve line-intersection system below, where we need
                !!  0 < u < 1, 0 < v < 1 for the intersection=point to be on this edge
                !!     (x2 - x1) u - (zlon - xx) v = xx - x1
                !!     (y2 - y1) u - (zlat - yy) v = yy - y1

                A = X2   - X1
                B = ZLON - XX
                C = Y2   - Y1
                D = ZLAT - YY
                E  = XX - X1
                F  = YY - Y1

                DET = A*D - B*C
                IF ( ABS( DET ) .LT. EPS ) CYCLE

                U = ( E*C - F*A ) / DET
                V = ( E*D - F*B ) / DET

                IF ( U .GE. 0.0D0 .AND. U .LE. 1.0D0 .AND.    &
                     V .GE. 0.0D0 .AND. V .LE. 1.0D0 )  THEN

                    XXX = X1 + U * A
                    YYY = Y1 + U * C
                    CELLS( NN ) = N
                    WGHTS( NN ) = DISTRM( YY, XX, YYY, XXX ) / DARC
                    II          = N     !!  this cell
                    KK          = K     !!  this edge
                    XX          = XXX
                    YY          = YYY
                    EXIT

                END IF

            END DO      !!  end loop on edges for this cell

        END DO      !!  end loop on cells

        !!  if you get to here:  did not find the end <ZLAT,ZLON> of this arc within

        ARC2MPAS2D = .FALSE.
        RETURN

    END FUNCTION ARC2MPAS2D


    !!.......................................................................
    !!  interface:  3D version
    !!      Find the number of MPAS cells that intersect the arc from <ALAT,ALON>
    !!      to <ZLAT,ZLON>, the corresponding cell-indices and layered weights.
    !!      Uses VERTWT() to allocate cell-weights relative to the input
    !!      MPAS-gridded layer structure ZGRID
    !!.......................................................................

    LOGICAL FUNCTION ARC2MPAS3D( ALAT, ALON, AHGT, ZLAT, ZLON, ZHGT,    &
                                 NLAYS, NMAX, ZGRID,                    &
                                 NSEGS, CELLS, WGHTS )

        REAL   , INTENT(IN   ) :: ALAT, ALON, AHGT, ZLAT, ZLON, ZHGT
        INTEGER, INTENT(IN   ) :: NLAYS, NMAX
        REAL   , INTENT(IN   ) :: ZGRID( NLAYS+1, MPCELLS )
        INTEGER, INTENT(  OUT) :: NSEGS
        INTEGER, INTENT(  OUT) :: CELLS( NMAX )
        REAL   , INTENT(  OUT) :: WGHTS( NLAYS, NMAX )

        REAL  , PARAMETER :: EPS = 1.0D-10

        INTEGER     IA, IZ, II
        INTEGER     I, J, K, KK, L, M, N, NN
        REAL        DARC
        REAL        XX, YY, ZZ, XXX, YYY, ZZZ, X1, Y1, Z1, X2, Y2, Z2
        REAL        A, B, C, D, E, F, DET, U, V, WW

        !!......................   begin body of function  ARC2MPAS3D

        IF ( .NOT.INITFLAG ) THEN
            CALL M3MESG( 'MODMPASFIO/ARC2MPAS3D():  must call INITMPGRID() before ARC2MPAS3D()' )
            ARC2MPAS3D = .FALSE.
            RETURN
        END IF

        IA = FINDCELL( ALAT, ALON )
        IZ = FINDCELL( ZLAT, ZLON )

        IF ( IA .EQ. IZ ) THEN
            NSEGS      = 1
            CELLS( 1 ) = IA
            CALL VERTWT( AHGT, ZHGT, 1.0, IA, 1, NLAYS, NMAX, ZGRID, WGHTS )
            ARC2MPAS3D = .TRUE.
            RETURN
        END IF

        DARC = SPHEREDIST( ALAT, ALON, ZLAT, ZLON )

        YY = ALAT
        XX = ALON
        ZZ = AHGT
        II = IA             !!  current cell-index
        KK = -9999          !!  last edge-index

        DO NN = 1, NMAX     !!  loop finding intersections of current arc with current-cell edges;
                            !!  terminates when final end-point is found in a cell.
            DO J = 1, NBNDYE( II )

                N  = BNDYCELL( J,II )       !! <ZLAT,ZLON> in this bdy-cell?
                IF ( N .EQ. 0 )  CYCLE
                IF ( N .EQ. IZ ) THEN
                    NSEGS       = NN
                    CELLS( NN ) = N
                    WW          = SPHEREDIST( YY, XX, ZLAT, ZLON ) / DARC           !!  this fraction of total distance
                    CALL VERTWT( ZZ, ZHGT, WW, II, NN, NLAYS, NMAX, ZGRID, WGHTS )
                    ARC2MPAS3D = .TRUE.
                    RETURN
                END IF

                K  = BNDYEDGE( J,II )
                IF ( K .EQ. KK ) CYCLE      !!  skip the edge we're coming from, if any

                L  = EVRTXS( 1,K )
                M  = EVRTXS( 2,K )
                X1 = ALONV( L )
                Y1 = ALONV( L )
                X2 = ALONV( M )
                Y2 = ALONV( M )

                !!  Solve line-intersection system below, where we need
                !!  0 < u < 1, 0 < v < 1 for the intersection=point to be on this edge
                !!     (x2 - x1) u + (zlon - xx) v = xx - x1    :: Au + Bv = E
                !!     (y2 - y1) u + (zlat - yy) v = yy - y1    :: Cu + Dv = F

                A = X2 - X1
                B = XX - ZLON
                C = Y2 - Y1
                D = YY - ZLAT
                E = XX - X1
                F = YY - Y1

                DET = A*D - B*C
                IF ( ABS( DET ) .LT. EPS ) CYCLE

                U = ( E*C - F*A ) / DET
                V = ( E*D - F*B ) / DET

                IF ( U .GE. 0.0D0 .AND. U .LE. 1.0D0 .AND.    &
                     V .GE. 0.0D0 .AND. V .LE. 1.0D0 )  THEN

                    XXX = X1 + V * A
                    YYY = Y1 + V * C
                    ZZZ = ZZ + V * ( ZHGT - ZZ )
                    CELLS( NN ) = N
                    WW          = SPHEREDIST( YY, XX, YYY, XXX ) / DARC           !!  this fraction of total distance
                    CALL VERTWT( ZZ, ZZZ, WW, II, NN, NLAYS, NMAX, ZGRID, WGHTS )
                    II          = N     !!  this cell
                    KK          = K     !!  this edge
                    XX          = XXX
                    YY          = YYY
                    ZZ          = ZZZ

                    EXIT        !!  to next-cell intersection-problem

                END IF

            END DO      !!  end loop on edges for this cell

        END DO      !!  end loop on cells

        !!  if you get to here:  did not find the end <ZLAT,ZLON> of this arc within

        ARC2MPAS3D = .FALSE.
        RETURN

    END FUNCTION ARC2MPAS3D


    !!.......................................................................
    !!  for generic:  instrumented 3D versions
    !!.......................................................................

    LOGICAL FUNCTION ARC2MPAS3D1( ALAT, ALON, AHGT, ZLAT, ZLON, ZHGT,    &
                                  NLAYS, NMAX, ZGRID,                    &
                                  NSEGS, CELLS, WGHTS, ZBOTS, ZTOPS, WSUMS )

        REAL   , INTENT(IN   ) :: ALAT, ALON, AHGT, ZLAT, ZLON, ZHGT
        INTEGER, INTENT(IN   ) :: NLAYS, NMAX
        REAL   , INTENT(IN   ) :: ZGRID( NLAYS+1, MPCELLS )
        INTEGER, INTENT(  OUT) :: NSEGS
        INTEGER, INTENT(  OUT) :: CELLS( NMAX )
        REAL   , INTENT(  OUT) :: WGHTS( NLAYS, NMAX )
        REAL   , INTENT(  OUT) :: ZBOTS( NMAX )
        REAL   , INTENT(  OUT) :: ZTOPS( NMAX )
        REAL   , INTENT(  OUT) :: WSUMS( NMAX )

        REAL  , PARAMETER :: EPS = 1.0D-10

        INTEGER     IA, IZ, II
        INTEGER     I, J, K, KK, L, M, N, NN
        REAL        DARC
        REAL        XX, YY, ZZ, XXX, YYY, ZZZ, X1, Y1, Z1, X2, Y2, Z2
        REAL        A, B, C, D, E, F, DET, U, V, WW

        !!......................   begin body of function  ARC2MPAS3D

        IF ( .NOT.INITFLAG ) THEN
            CALL M3MESG( 'MODMPASFIO/ARC2MPAS3D():  must call INITMPGRID() before ARC2MPAS3D()' )
            ARC2MPAS3D1 = .FALSE.
            RETURN
        END IF

        IA   = FINDCELL( ALAT, ALON )
        IZ   = FINDCELL( ZLAT, ZLON )
        DARC = SPHEREDIST( ALAT, ALON, ZLAT, ZLON )

        IF ( IA .EQ. IZ ) THEN
            NSEGS      = 1
            CELLS( 1 ) = IA
            CALL VERTWT1( AHGT, ZHGT, 1.0, IA, 1, NLAYS, NMAX, ZGRID, WGHTS, ZBOTS, ZTOPS, WSUMS )
            ARC2MPAS3D1 = .TRUE.
            RETURN
        END IF

        YY = ALAT
        XX = ALON
        ZZ = AHGT
        II = IA             !!  current cell-index
        KK = -9999          !!  last edge-index

NNLOOP: DO NN = 1, 999999999    !!  loop finding intersections of current arc with current-cell edges;
                                !!  terminates when final end-point is found in a cell.
            IF ( NN .GT. NMAX ) THEN
                CALL M3MESG( 'MODMPASFIO/ARC2MPAS3D():  output-array overflow; increase NMAX' )
                ARC2MPAS3D1 = .FALSE.
                RETURN
            END IF

            DO J = 1, NBNDYE( II )

                N  = BNDYCELL( J,II )       !! <ZLAT,ZLON> in this bdy-cell?
                IF ( N .EQ. 0 )  CYCLE
                IF ( N .EQ. IZ ) THEN
                    NSEGS       = NN
                    CELLS( NN ) = N
                    WW          = SPHEREDIST( YY, XX, ZLAT, ZLON ) / DARC
                    CALL VERTWT1( ZZ, ZHGT, WW, II, NN, NLAYS, NMAX, ZGRID, WGHTS, ZBOTS, ZTOPS, WSUMS )
                    ARC2MPAS3D1 = .TRUE.
                    RETURN
                END IF

                K  = BNDYEDGE( J,II )
                IF ( K .EQ. KK ) CYCLE      !!  skip the edge we're coming from, if any

                L  = EVRTXS( 1,K )
                M  = EVRTXS( 2,K )
                X1 = ALONV( L )
                Y1 = ALONV( L )
                X2 = ALONV( M )
                Y2 = ALONV( M )

                !!  Solve line-intersection system below, where we need
                !!  0 < u < 1, 0 < v < 1 for the intersection=point to be on this edge
                !!     (x2 - x1) u + (zlon - xx) v = xx - x1    :: Au + Bv = E
                !!     (y2 - y1) u + (zlat - yy) v = yy - y1    :: Cu + Dv = F

                A = X2 - X1
                B = XX - ZLON
                C = Y2 - Y1
                D = YY - ZLAT
                E = XX - X1
                F = YY - Y1

                DET = A*D - B*C
                IF ( ABS( DET ) .LT. EPS ) CYCLE

                U = ( E*C - F*A ) / DET
                V = ( E*D - F*B ) / DET

                IF ( U .GE. 0.0 .AND. U .LE. 1.0 .AND.    &
                     V .GE. 0.0 .AND. V .LE. 1.0 )  THEN

                    XXX = X1 + V * A
                    YYY = Y1 + V * C
                    ZZZ = ZZ + V * ( ZHGT - ZZ )
                    CELLS( NN ) = N
                    WW          = SPHEREDIST( YY, XX, YYY, XXX ) / DARC
                    CALL VERTWT1( ZZ, ZZZ, WW, N, NN, NLAYS, NMAX, ZGRID, WGHTS, ZBOTS, ZTOPS, WSUMS )
                    II          = N     !!  this cell
                    KK          = K     !!  this edge
                    XX          = XXX
                    YY          = YYY
                    ZZ          = ZZZ

                    CYCLE NNLOOP    !!  to next-cell intersection-problem

                END IF

            END DO      !!  end loop on edges for this cell
            
            EXIT

        END DO NNLOOP   !!  end loop on cells

        !!  if you get to here:  did not find the end <ZLAT,ZLON> of this arc within

        ARC2MPAS3D1 = .FALSE.
        RETURN

    END FUNCTION ARC2MPAS3D1

    !!.......................................................................

    LOGICAL FUNCTION ARC2MPAS3D2( ALAT, ALON, AHGT, ZLAT, ZLON, ZHGT,   &
                                  NLAYS, NMAX, ZGRID,                   &
                                  NSEGS, CELLS, LAYLO, LAYHI,           &
                                  WGHTS, ZBOTS, ZTOPS, WSUMS )

        REAL   , INTENT(IN   ) :: ALAT, ALON, AHGT, ZLAT, ZLON, ZHGT
        INTEGER, INTENT(IN   ) :: NLAYS, NMAX
        REAL   , INTENT(IN   ) :: ZGRID( NLAYS+1, MPCELLS )
        INTEGER, INTENT(  OUT) :: NSEGS
        INTEGER, INTENT(  OUT) :: CELLS( NMAX )
        INTEGER, INTENT(  OUT) :: LAYLO( NMAX )
        INTEGER, INTENT(  OUT) :: LAYHI( NMAX )
        REAL   , INTENT(  OUT) :: WGHTS( NLAYS, NMAX )
        REAL   , INTENT(  OUT) :: ZBOTS( NMAX )
        REAL   , INTENT(  OUT) :: ZTOPS( NMAX )
        REAL   , INTENT(  OUT) :: WSUMS( NMAX )

        REAL  , PARAMETER :: EPS = 1.0D-10

        INTEGER     IA, IZ, II
        INTEGER     I, J, K, KK, L, M, N, NN
        REAL        DARC
        REAL        XX, YY, ZZ, XXX, YYY, ZZZ, X1, Y1, Z1, X2, Y2, Z2
        REAL        A, B, C, D, E, F, DET, U, V, WW

        !!......................   begin body of function  ARC2MPAS3D

        IF ( .NOT.INITFLAG ) THEN
            CALL M3MESG( 'MODMPASFIO/ARC2MPAS3D():  must call INITMPGRID() before ARC2MPAS3D()' )
            ARC2MPAS3D2 = .FALSE.
            RETURN
        END IF

        IA   = FINDCELL( ALAT, ALON )
        IZ   = FINDCELL( ZLAT, ZLON )
        DARC = SPHEREDIST( ALAT, ALON, ZLAT, ZLON )

        IF ( IA .EQ. IZ ) THEN
            NSEGS      = 1
            CELLS( 1 ) = IA
            CALL VERTWT2( AHGT, ZHGT, 1.0, IA, 1, NLAYS, NMAX, ZGRID, WGHTS, LAYLO, LAYHI, ZBOTS, ZTOPS, WSUMS )
            ARC2MPAS3D2 = .TRUE.
            RETURN
        END IF

        YY = ALAT
        XX = ALON
        ZZ = AHGT
        II = IA             !!  current cell-index
        KK = -9999          !!  last edge-index

NNLOOP: DO NN = 1, 999999999    !!  loop finding intersections of current arc with current-cell edges;
                                !!  terminates when final end-point is found in a cell.
            IF ( NN .GT. NMAX ) THEN
                CALL M3MESG( 'MODMPASFIO/ARC2MPAS3D():  output-array overflow; increase NMAX' )
                ARC2MPAS3D2 = .FALSE.
                RETURN
            END IF

            DO J = 1, NBNDYE( II )

                N  = BNDYCELL( J,II )       !! <ZLAT,ZLON> in this bdy-cell?
                IF ( N .EQ. 0 )  CYCLE
                IF ( N .EQ. IZ ) THEN
                    NSEGS       = NN
                    CELLS( NN ) = N
                    WW          = SPHEREDIST( YY, XX, ZLAT, ZLON ) / DARC
                    CALL VERTWT2( ZZ, ZHGT, WW, II, NN, NLAYS, NMAX, ZGRID, WGHTS, LAYLO, LAYHI, ZBOTS, ZTOPS, WSUMS )
                    ARC2MPAS3D2 = .TRUE.
                    RETURN
                END IF

                K  = BNDYEDGE( J,II )
                IF ( K .EQ. KK ) CYCLE      !!  skip the edge we're coming from, if any

                L  = EVRTXS( 1,K )
                M  = EVRTXS( 2,K )
                X1 = ALONV( L )
                Y1 = ALONV( L )
                X2 = ALONV( M )
                Y2 = ALONV( M )

                !!  Solve line-intersection system below, where we need
                !!  0 < u < 1, 0 < v < 1 for the intersection=point to be on this edge
                !!     (x2 - x1) u + (zlon - xx) v = xx - x1    :: Au + Bv = E
                !!     (y2 - y1) u + (zlat - yy) v = yy - y1    :: Cu + Dv = F

                A = X2 - X1
                B = XX - ZLON
                C = Y2 - Y1
                D = YY - ZLAT
                E = XX - X1
                F = YY - Y1

                DET = A*D - B*C
                IF ( ABS( DET ) .LT. EPS ) CYCLE

                U = ( E*C - F*A ) / DET
                V = ( E*D - F*B ) / DET

                IF ( U .GE. 0.0 .AND. U .LE. 1.0 .AND.    &
                     V .GE. 0.0 .AND. V .LE. 1.0 )  THEN

                    XXX = X1 + V * A
                    YYY = Y1 + V * C
                    ZZZ = ZZ + V * ( ZHGT - ZZ )
                    CELLS( NN ) = N
                    WW          = SPHEREDIST( YY, XX, YYY, XXX ) / DARC
                    CALL VERTWT2( ZZ, ZZZ, WW, N, NN, NLAYS, NMAX, ZGRID, WGHTS, LAYLO, LAYHI, ZBOTS, ZTOPS, WSUMS )
                    II          = N     !!  this cell
                    KK          = K     !!  this edge
                    XX          = XXX
                    YY          = YYY
                    ZZ          = ZZZ

                    CYCLE NNLOOP    !!  to next-cell intersection-problem

                END IF

            END DO      !!  end loop on edges for this cell
            
            EXIT

        END DO NNLOOP     !!  end loop on cells

        !!  if you get to here:  did not find the end <ZLAT,ZLON> of this arc within

        ARC2MPAS3D2 = .FALSE.
        RETURN

    END FUNCTION ARC2MPAS3D2


    !!.......................................................................
    !!  Allocate the Z-values Z1:Z2 to the vertical column at cell II
    !!  to WGHTS( :,NN ), so that total vertical-column-sum is WW.
    !!.......................................................................

    SUBROUTINE VERTWT( Z1, Z2, WW, II, NN, NLAYS, NMAX, ZGRID, WGHTS )

        REAL   , INTENT(IN   ) :: Z1, Z2, WW
        INTEGER, INTENT(IN   ) :: II, NN, NLAYS, NMAX
        REAL   , INTENT(IN   ) :: ZGRID( NLAYS+1, MPCELLS )
        REAL   , INTENT(  OUT) :: WGHTS( NLAYS  , NMAX )

        INTEGER     LLO, LHI, L
        REAL        ZLO, ZHI, DDZ

        !!......................   begin body of function

        ZLO = MIN( Z1, Z2 )
        ZHI = MAX( Z1, Z2 )

        LLO = 1
        LHI = 1
        DO L = 1, NLAYS
            IF ( ZLO .GE. ZGRID( L,II   ) )  LLO = L
            IF ( ZHI .LE. ZGRID( L,II+1 ) )  LHI = L
            WGHTS( L,NN ) = 0.0
        END DO

        IF ( LLO .EQ. LHI ) THEN
            WGHTS( LLO,NN ) = WW
            RETURN
        END IF

        DDZ = 1.0 / ( ZHI - ZLO )
        IF ( ZHI .GT. ZGRID( NLAYS+1,II ) ) THEN
            ZHI = ZGRID( NLAYS+1,II )
        END IF

        WGHTS( LLO,NN ) = WW * DDZ * ( ZGRID( LLO+1,II ) - ZLO )
        WGHTS( LHI,NN ) = WW * DDZ * ( ZHI  -  ZGRID( LHI-1,II ) )

        DO L = LLO+1, LHI-1
            WGHTS( L,NN ) = WW * DDZ * ( ZGRID( L+1,II ) - ZGRID( L,II ) )
        END DO

        RETURN

    END SUBROUTINE VERTWT


    !!.......................................................................
    !!  "Instrumented" versions VERTWT1, VERTWT2
    !!.......................................................................

    SUBROUTINE VERTWT1( Z1, Z2, WW, II, NN, NLAYS, NMAX, ZGRID, WGHTS, ZBOTS, ZTOPS, WSUMS )

        REAL   , INTENT(IN   ) :: Z1, Z2, WW
        INTEGER, INTENT(IN   ) :: II, NN, NLAYS, NMAX
        REAL   , INTENT(IN   ) :: ZGRID( NLAYS+1, MPCELLS )
        REAL   , INTENT(  OUT) :: WGHTS( NLAYS  , NMAX )
        REAL   , INTENT(  OUT) :: ZBOTS( NMAX )
        REAL   , INTENT(  OUT) :: ZTOPS( NMAX )
        REAL   , INTENT(  OUT) :: WSUMS( NMAX )

        INTEGER     LLO, LHI, L
        REAL        ZLO, ZHI, DDZ, DW

        !!......................   begin body of function

        ZLO = MIN( Z1, Z2 )
        ZHI = MAX( Z1, Z2 )

        LLO = 1
        LHI = 1
        DO L = 1, NLAYS
            IF ( ZLO .GE. ZGRID( L,II   ) )  LLO = L
            IF ( ZHI .LE. ZGRID( L,II+1 ) )  LHI = L
            WGHTS( L,NN ) = 0.0
        END DO

        IF ( LLO .EQ. LHI ) THEN
            WGHTS( LLO,NN ) = WW
            ZBOTS( NN ) = ZLO
            ZTOPS( NN ) = ZHI
            WSUMS( NN ) = WW
            RETURN
        END IF

        DDZ = 1.0 / ( ZHI - ZLO )
        IF ( ZHI .GT. ZGRID( NLAYS+1,II ) ) THEN
            DW  = DDZ * ( ZHI - ZGRID( NLAYS+1,II ) )
            ZHI = ZGRID( NLAYS+1,II )
        ELSE
            DW = 0.0
        END IF

        WGHTS( LLO,NN ) = WW * DDZ * ( ZGRID( LLO+1,II ) - ZLO )
        WGHTS( LHI,NN ) = WW * DDZ * ( ZHI  -  ZGRID( LHI,II ) )

        DO L = LLO+1, LHI-1
            WGHTS( L,NN ) = WW * DDZ * ( ZGRID( L+1,II ) - ZGRID( L,II ) )
        END DO

        ZBOTS( NN ) = ZLO
        ZTOPS( NN ) = ZHI
        WSUMS( NN ) = WW - DW

        RETURN

    END SUBROUTINE VERTWT1

    !!.......................................................................

    SUBROUTINE VERTWT2( Z1, Z2, WW, II, NN, NLAYS, NMAX, ZGRID, WGHTS, LAYLO, LAYHI, ZBOTS, ZTOPS, WSUMS )

        REAL   , INTENT(IN   ) :: Z1, Z2, WW
        INTEGER, INTENT(IN   ) :: II, NN, NLAYS, NMAX
        REAL   , INTENT(IN   ) :: ZGRID( NLAYS+1, MPCELLS )
        REAL   , INTENT(  OUT) :: WGHTS( NLAYS  , NMAX )
        INTEGER, INTENT(  OUT) :: LAYLO( NMAX )
        INTEGER, INTENT(  OUT) :: LAYHI( NMAX )
        REAL   , INTENT(  OUT) :: ZBOTS( NMAX )
        REAL   , INTENT(  OUT) :: ZTOPS( NMAX )
        REAL   , INTENT(  OUT) :: WSUMS( NMAX )

        INTEGER     LLO, LHI, L
        REAL        ZLO, ZHI, DDZ, DW

        !!......................   begin body of function

        ZLO = MIN( Z1, Z2 )
        ZHI = MAX( Z1, Z2 )

        LLO = 1
        LHI = 1
        DO L = 1, NLAYS
            IF ( ZLO .GE. ZGRID( L,II ) )  LLO = L
            IF ( ZHI .GE. ZGRID( L,II ) )  LHI = L
            WGHTS( L,NN ) = 0.0
        END DO

        IF ( LLO .EQ. LHI ) THEN
            WGHTS( LLO,NN ) = WW
            LAYLO( NN ) = LLO
            LAYHI( NN ) = LHI
            ZBOTS( NN ) = ZLO
            ZTOPS( NN ) = ZHI
            WSUMS( NN ) = WW
            RETURN
        END IF

        DDZ = 1.0 / ( ZHI - ZLO )
        IF ( ZHI .GT. ZGRID( NLAYS+1,II ) ) THEN
            DW  = DDZ * ( ZHI - ZGRID( NLAYS+1,II ) )
            ZHI = ZGRID( NLAYS+1,II )
        ELSE
            DW = 0.0
        END IF

        WGHTS( LLO,NN ) = WW * DDZ * ( ZGRID( LLO+1,II ) - ZLO )
        WGHTS( LHI,NN ) = WW * DDZ * ( ZHI  -  ZGRID( LHI,II ) )

        DO L = LLO+1, LHI-1
            WGHTS( L,NN ) = WW * DDZ * ( ZGRID( L+1,II ) - ZGRID( L,II ) )
        END DO

        LAYLO( NN ) = LLO
        LAYHI( NN ) = LHI
        ZBOTS( NN ) = ZLO
        ZTOPS( NN ) = ZHI
        WSUMS( NN ) = WW - DW

        RETURN

    END SUBROUTINE VERTWT2


    !!.......................................................................
    !!  Shut down module
    !!.......................................................................

    SUBROUTINE SHUTMPGRID( )

        INTEGER     IERR, ISTAT, F, FID

        !!......................   begin body of function

        IF ( .NOT.INITFLAG ) THEN
            RETURN
        END IF

        DO F = 1, MPCOUNT
            FID = MPCDFID( F )
            IERR =  NF_CLOSE( FID )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR(IERR) )
                CALL M3MESG( 'MODMPASFIO/SHUTMPGRID:  Error closing "' // TRIM( MPFILES(F) ) // '"' )
            END IF
        END DO

        DEALLOCATE( MPDATES, MPTIMES, CELLID, EDGEID, VRTXID,       &
                    NBNDYE, NEDGEE, BNDYCELL, BNDYEDGE, BNDYVRTX,   &
                    ECELLS, EVRTXS, VCELLS, VEDGES, EEDGES, EWGHTS, &
                    ALATC,  ALONC,  ALATE, ALONE, ALATV, ALONV,     &
                    XCELL,  YCELL,  ZCELL, XEDGE, YEDGE, ZEDGE,     &
                    XVRTX,  YVRTX,  ZVRTX, DVEDGE, DCEDGE, EANGLE,  &
                    CAREAS, VAREAS, KAREAS, MSHDEN, STAT = ISTAT )

        IF ( ISTAT .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/SHUTMPGRID:  Error with DEALLOCATE()' )
        END IF

        INITFLAG = .FALSE.
        MPSTEPS  = 0
        MPCOUNT  = 0

        RETURN

    END  SUBROUTINE SHUTMPGRID


    !!.......................................................................
    !!  Date-and-Time manipulation routines
    !!.......................................................................

    SUBROUTINE MPSTR2DT( CBUF, JDATE, JTIME )

        CHARACTER*(*), INTENT(IN   ) :: CBUF
        INTEGER      , INTENT(  OUT) :: JDATE, JTIME

        INTEGER     MNTH, MDAY, YEAR, HOUR, MINS, SECS
        INTEGER     I1, I2, I3, I4, I5, I6

        CHARACTER*4, PARAMETER :: DELIMS = '-_: '

        !!......................   begin body of function

        I1 = SCAN( CBUF,          DELIMS )
        I2 = SCAN( CBUF( I1+1: ), DELIMS ) + I1
        I3 = SCAN( CBUF( I2+1: ), DELIMS ) + I2
        I4 = SCAN( CBUF( I3+1: ), DELIMS ) + I3
        I5 = SCAN( CBUF( I4+1: ), DELIMS ) + I4
        I6 = SCAN( CBUF( I5+1: ), DELIMS ) + I5

        YEAR = STR2INT( CBUF(     :I1-1 ) )
        MNTH = STR2INT( CBUF( I1+1:I2-1 ) )
        MDAY = STR2INT( CBUF( I2+1:I3-1 ) )
        HOUR = STR2INT( CBUF( I3+1:I4-1 ) )
        MINS = STR2INT( CBUF( I4+1:I5-1 ) )
        IF ( I6 .GT. 0 ) THEN
            SECS = STR2INT( CBUF( I5+1:I6-1 ) )
        ELSE
            SECS = 0
        END IF

        IF ( MIN( MNTH, MDAY, YEAR, HOUR, MINS, SECS ) .EQ. IMISS3 ) THEN
            JDATE = IMISS3
            JTIME = IMISS3
        ELSE
            JDATE = 1000 * YEAR + JULIAN( YEAR, MNTH, MDAY )
            JTIME = SECS + 100 * ( MINS + 100 * HOUR )
        END IF

        RETURN

    END SUBROUTINE MPSTR2DT


    !!.......................................................................


    SUBROUTINE MPDT2STR( JDATE, JTIME, CBUF )

        INTEGER      , INTENT(IN   ) :: JDATE, JTIME
        CHARACTER*(*), INTENT(  OUT) :: CBUF

        INTEGER     MNTH, MDAY, YEAR, HOUR, MINS, SECS

        CALL DAYMON( JDATE, MNTH, MDAY )
        YEAR = JDATE / 1000
        HOUR =      JTIME / 10000
        MINS = MOD( JTIME / 100  , 100 )
        SECS = MOD ( JTIME       , 100 )

        WRITE( CBUF, '(I4.4, 5( A, I2.2 ) )' ) YEAR, '-', MNTH, '-', MDAY, '_', HOUR, ':', MINS, ':', SECS

        RETURN

    END SUBROUTINE MPDT2STR


    !!.......................................................................
    !!  Read array of Dates-and-Times from file FNAME, variable VNAME
    !!.......................................................................


    LOGICAL FUNCTION READMPSTEPS( FNAME, VNAME, NSTEPS, MXSTEP, JDATES, JTIMES )

        CHARACTER*(*), INTENT(IN   ) :: FNAME               !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME               !!  time-variable name
        INTEGER      , INTENT(IN   ) :: MXSTEP              !!  array-dimension
        INTEGER      , INTENT(  OUT) :: NSTEPS              !!  number of actual timesteps
        INTEGER      , INTENT(  OUT) :: JDATES( MXSTEP )    !!  dates YYYYDDD
        INTEGER      , INTENT(  OUT) :: JTIMES( MXSTEP )    !!  times HHMMSS

        CHARACTER*25, PARAMETER :: PNAME = 'MODMPASFIO/READMPSTEPS():'

        INTEGER         F, V, N, FID, TID, VID, ISTAT
        INTEGER         START( 2 ), COUNT( 2 )
        LOGICAL         EFLAG
        CHARACTER*64    CBUF( MXSTEP )
        CHARACTER*256   MESG

        !!......................   begin body of function

        EFLAG = .FALSE.
        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( PNAME // ' File "' // TRIM(FNAME) // '" not yet opened' )
            READMPSTEPS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( PNAME // ' Time-variable "' // TRIM(VNAME) // '" not found in "' // TRIM(FNAME) // '"' )
            NSTEPS = 0
            READMPSTEPS = .TRUE.
            RETURN
        ELSE IF ( MPVTYPE( V,F ) .NE. NF_CHAR ) THEN
            CALL M3MESG( PNAME // ' Invalid type for time-variable "' // TRIM(VNAME) // '" in "' // TRIM(FNAME) // '"' )
            NSTEPS = 0
            READMPSTEPS = .FALSE.
            RETURN
        ELSE IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( PNAME // ' Invalid rank for time-variable "' // TRIM(VNAME) // '" in "' // TRIM(FNAME) // '"' )
            NSTEPS = 0
            READMPSTEPS = .FALSE.
            RETURN
        ELSE IF ( MPTIMDID( F ) .NE. MPVDIDS( 2,V,F ) ) THEN
            CALL M3MESG( PNAME // ' Invalid time-variable "' // TRIM(VNAME) // '" in "' // TRIM(FNAME) // '"' )
            READMPSTEPS = .FALSE.
            RETURN
        ELSE
            TID = MPTIMDID(  F )
            VID = MPVARID( V,F )
        END IF

        NSTEPS = MPVDIMS( 2,V,F )

        START(1) = 1
        START(2) = 1
        COUNT(1) = MPVDIMS( 1,V,F )
        COUNT(2) = NSTEPS

        ISTAT = NF_GET_VARA_TEXT( FID, VID, START, COUNT, CBUF )
        IF ( ISTAT .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( ISTAT ) )
            MESG = PNAME // '" Error reading "' // TRIM(VNAME) // '" from "' // TRIM(FNAME) //  '"'
            CALL M3MESG( MESG )
            READMPSTEPS = .FALSE.
            RETURN
        END IF          !  ISTAT nonzero:  operation failed

        DO N = 1, NSTEPS

            CALL MPSTR2DT( CBUF( N ), JDATES( N ), JTIMES( N ) )

        END DO

        READMPSTEPS = .TRUE.
        RETURN

    END FUNCTION READMPSTEPS


    !!.......................................................................
    !!  Write Date-and-Time to file FNAME, variable VNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPSTEP( FNAME, VNAME, ISTEP, JDATE, JTIME )

        CHARACTER*(*), INTENT(IN   ) :: FNAME       !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME       !!  time-variable name
        INTEGER      , INTENT(IN   ) :: ISTEP       !!  timestep-number (1,2,...)
        INTEGER      , INTENT(IN   ) :: JDATE       !!  date YYYYDDD
        INTEGER      , INTENT(IN   ) :: JTIME       !!  time HHMMSS

        CHARACTER*25, PARAMETER :: PNAME = 'MODMPASFIO/WRITEMPSTEP():'

        INTEGER         F, V, N, FID, TID, VID, ISTAT, START( 2 ), COUNT( 2 )
        CHARACTER*64    CBUF
        CHARACTER*256   MESG

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( PNAME// ' File "' // TRIM(FNAME) // '" not yet opened' )
            WRITEMPSTEP = .FALSE.
            RETURN
        END IF
        FID = MPCDFID ( F )
        TID = MPTIMDID( F )

        IF ( TID .EQ. IMISS3 ) THEN
            CALL M3MESG( PNAME// ' File "' // TRIM(FNAME) // '" time-dimension undefined' )
            WRITEMPSTEP = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( PNAME // ' Time-variable "' // TRIM(VNAME) // '" not found in "' // TRIM(FNAME) // '"' )
            WRITEMPSTEP = .TRUE.
            RETURN
        ELSE IF ( MPVTYPE( V,F ) .NE. NF_CHAR ) THEN
            CALL M3MESG( PNAME // ' Invalid type for time-variable "' // TRIM(VNAME) // '" in "' // TRIM(FNAME) // '"' )
            WRITEMPSTEP = .FALSE.
            RETURN
        ELSE IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( PNAME // ' Invalid rank for time-variable "' // TRIM(VNAME) // '" in "' // TRIM(FNAME) // '"' )
            WRITEMPSTEP = .FALSE.
            RETURN
        ELSE IF ( MPTIMDID( F ) .NE. MPVDIDS( 2,V,F ) ) THEN
            CALL M3MESG( PNAME // ' Invalid time-variable "' // TRIM(VNAME) // '" in "' // TRIM(FNAME) // '"' )
            WRITEMPSTEP = .FALSE.
            RETURN
        ELSE
            TID = MPTIMDID(  F )
            VID = MPVARID( V,F )
        END IF

        CALL MPDT2STR( JDATE, JTIME, CBUF )

        START(1) = 1
        START(2) = ISTEP
        COUNT(1) = MPVDIMS( 1,V,F )
        COUNT(2) = 1

        ISTAT = NF_PUT_VARA_TEXT( FID, VID, START, COUNT, CBUF )
        IF ( ISTAT .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( PNAME // '" Error WRITING "' // TRIM(VNAME) // '" to "' //TRIM(FNAME)// '"' )
            WRITEMPSTEP = .FALSE.
            RETURN
        END IF          !  ISTAT nonzero:  operation failed

        WRITEMPSTEP = .TRUE.
        RETURN

    END FUNCTION WRITEMPSTEP


    !!.......................................................................
    !!      Open a new input MPAS-file according to the given mode
    !!.......................................................................

    LOGICAL FUNCTION OPENMPAS( FNAME, FMODE )

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        INTEGER      , INTENT(IN   ) :: FMODE

        CHARACTER*1,  PARAMETER :: BLANK = ' '
        CHARACTER*20, PARAMETER :: PNAME = 'MODMPASFIO/OPENMPAS:'

        LOGICAL         EFLAG
        INTEGER         FID
        INTEGER         ISTAT, IMODE, ID, F, N, V, TID, NATTS
        INTEGER         NVARS, NSTEPS, NCELLS, NEDGES, NVRTXS, NLVLS, NVORDR, NBNDYC
        INTEGER         VTYPE, VNDIM, VDIMS( 7 )
        REAL(8)         EARTHR
        CHARACTER*64    VNAME
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        N = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( N .GT. 0 ) THEN
            OPENMPAS = .TRUE.
            RETURN
        END IF

        F = MPCOUNT+1
        IF ( F .GT. MXFILE3 ) THEN
            MESG = PNAME // ' File-table overflow for "' //TRIM(FNAME)// '"'
            CALL M3MESG( MESG )
            OPENMPAS = .FALSE.
            RETURN
        END IF

        CALL NAMEVAL( FNAME, EQNAME )

        IF (      FMODE .EQ. FSREAD3 ) THEN
            IMODE = NF_NOWRITE
        ELSE IF ( FMODE .EQ. FSRDWR3 ) THEN
            IMODE = IOR( NF_WRITE, NF_SHARE )
        ELSE
            MESG = PNAME // ' Invalid file-mode for "' //TRIM(FNAME)// '" - neither red-only nor read-write'
            CALL M3MESG( MESG )
            OPENMPAS = .FALSE.
            RETURN
        END IF

        ISTAT = NF_OPEN( EQNAME, IMODE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // ' Error opening "' // TRIM(FNAME) // '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            OPENMPAS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        MPFILES( F ) = FNAME
        MPCDFID( F ) = FID


        ISTAT = NF_GET_ATT_DOUBLE( FID, NF_GLOBAL, 'sphere_radius', EARTHR )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // ' Error reading "sphere_radius" from "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE IF ( DBLERR( REARTH, EARTHR ) ) THEN
            MESG = PNAME// ' WARNING -- "sphere_radius" from "' //TRIM(FNAME)// '" does not match MODMPASFIO/REARTH'
            CALL M3MESG( MESG )
        END IF          !  ISTAT nonzero:  operation failed, or...
                EFLAG = .FALSE.
        ISTAT = NF_INQ_DIMID( FID, 'Time', ID )
        IF ( ISTAT .EQ. NF_EEXIST ) THEN
            MESG = PNAME // ' No Time-dimension found for "' // TRIM( FNAME )
            CALL M3MESG( MESG )
            MPTIMDID( F ) = IMISS3
            NSTEPS = 1
        ELSE IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // ' Error reading Time-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            NSTEPS = 0
        ELSE
            MPTIMDID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, NSTEPS )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // ' Error reading Time-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                MPNRECS( F ) = NSTEPS
            END IF
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_DIMID( FID, 'TWO', ID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // ' Error reading TWO-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MPTWODID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, N )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // ' Error reading TWO-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE IF ( N .NE. 2 ) THEN
                MESG = PNAME // ' Invalid TWO-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF          !  ISTAT nonzero:  operation failed
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_DIMID( FID, 'StrLen', ID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // ' Error reading StrLen-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MPSTRDID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, N )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // ' Error reading StrLen-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE IF ( N .NE. MPSTRLEN ) THEN
                MESG = PNAME // ' Invalid StrLen-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF          !  ISTAT nonzero:  operation failed
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_DIMID( FID, 'nCells', ID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // ' Error reading nCells-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MPCELDID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, NCELLS )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // ' Error reading nCells-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                MPNCELLS( 1 ) = NCELLS
            END IF          !  ISTAT nonzero:  operation failed
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_DIMID( FID, 'nEdges', ID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // ' Error reading nEdges-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MPDEGDID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, NEDGES )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // ' Error reading nEdges-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                MPNEDGES( 1 ) = NEDGES
            END IF          !  ISTAT nonzero:  operation failed
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_DIMID( FID, 'nVertices', ID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // 'Error reading nVertices-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MPVRTDID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, NVRTXS )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // 'Error reading nVertices-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                MPNVRTXS( F ) = NVRTXS
            END IF          !  ISTAT nonzero:  operation failed
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_DIMID( FID, 'maxEdges', ID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // 'Error reading maxEdges-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MPBDYDID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, NBNDYC )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // 'Error reading maxEdges-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                MPNBNDYC( F ) = NBNDYC
            END IF          !  ISTAT nonzero:  operation failed
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_DIMID( FID, 'maxEdges2', ID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // 'Error reading maxEdges-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MP2BDYID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, NBNDYC )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // 'Error reading maxEdges2-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE IF ( NBNDYC .NE. 2*MPNBNDYC( F ) ) THEN
                MESG = PNAME // 'Inconsistent maxEdges2-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF          !  ISTAT nonzero:  operation failed
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_DIMID( FID, 'vertexDegree', ID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // 'Error reading vertexDegree-dime ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MPDEGDID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, NVORDR )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // 'Error reading vertexDegree-dimm for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                MPNVORDR( F ) = NVORDR
            END IF          !  ISTAT nonzero:  operation failed
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_DIMID( FID, 'nVertLevels', ID )
        IF ( ISTAT .EQ. NF_EEXIST ) THEN
            MESG = 'WARNING -- missing nVertLevels-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( MESG )
            NLVLS         = 0
            MPNVLVLS( F ) = 0
        ELSE IF ( ISTAT .NE. 0 ) THEN
            MESG = PNAME // ' Error reading nVertLevels-dimension ID for "' //TRIM(FNAME)// '"'
            CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            MPLVLDID( F ) = ID
            ISTAT = NF_INQ_DIMLEN( FID, ID, NLVLS )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = PNAME // ' Error reading nVertLevels-dim for "' //TRIM(FNAME)// '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                MPNVLVLS( F ) = NLVLS
            END IF          !  ISTAT nonzero:  operation failed
        END IF          !  ISTAT nonzero:  operation failed

        ISTAT = NF_INQ_NVARS( FID, NVARS )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error getting NVARS for "' // TRIM( FNAME ) // '"'
                CALL M3MESG( NF_STRERROR(ISTAT) )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE IF ( NVARS .GT. MXVARS3 ) THEN
            WRITE( MESG, '( 2( A, I3, 2X ), 3A )' )     &
                'Actual NVARS=', NVARS, 'exceeds MXVAR=', MXVARS3, 'in "', TRIM( FNAME ), '":  list truncated'
            CALL M3WARN( PNAME, 0,0, MESG )
            NVARS = MXVARS3
        ELSE
            MPNVARS( F ) = NVARS
        END IF


        IF ( EFLAG ) THEN
            OPENMPAS = .FALSE.
            RETURN
        END IF


        CALL M3MESG( BLANK )
        CALL M3MESG( 'Input NAS-file ' // FNAME )
        CALL M3MESG(                  '    Path: '// EQNAME )
        WRITE( MESG, '( A, I9 )' )    '    NVARS  = ', NVARS
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NSTEPS = ', NSTEPS
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NCELLS = ', NCELLS
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NEDGES = ', NEDGES
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NVRTXS = ', NVRTXS
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NVLVLS = ', NLVLS
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NVORDR = ', NVORDR
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NBNDYC = ', NBNDYC
        CALL M3MESG( MESG )
        CALL M3MESG( BLANK )

        DO V = 1, NVARS

            ISTAT = NF_INQ_VAR( FID, V, VNAME, VTYPE, VNDIM, VDIMS, NATTS  )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
                CALL M3MESG( MESG )
                MESG = NF_STRERROR()
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( VNDIM .GT. 7 ) THEN
                MESG = 'Number of dimensions > 7 for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF          !!  ierr nonzero:  NF_INQ_VAR() failed

            MPVNAME( V,F ) = VNAME
            MPVARID( V,F ) = V
            MPVTYPE( V,F ) = VTYPE
            MPVDCNT( V,F ) = VNDIM

            DO N = 1, VNDIM
                ID = VDIMS( N )
                MPVDIDS( N,V,F ) = VDIMS( N )
                ISTAT = NF_INQ_DIM( FID, ID, MPVDNAM( N,V,F ), MPVDIMS( N,V,F ) )
                IF ( ISTAT .NE. 0 ) THEN
                    MESG = 'Error reading DIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
                    CALL M3MESG( MESG )
                    CALL M3MESG( NF_STRERROR() )
                    EFLAG = .TRUE.
                END IF          !!  ierr nonzero:  NF_INQ_VAR() failed

            END DO

        END DO      !!  end loop on variables for this file

        MPCOUNT  = F
        OPENMPAS = ( .NOT.EFLAG )
        RETURN

    END FUNCTION OPENMPAS


    !!.......................................................................
    !!      Open a new output MPAS-file using the "standard" grid,
    !!      according to the given specification for variables
    !!.......................................................................

    LOGICAL FUNCTION CREATEMPAS1( FNAME, NVARS, VNAMES, VTYPES,     &
                                  VNDIMS, VDNAME, VUNITS )

        CHARACTER*(*), INTENT(IN   ) :: FNAME                       !!  logical file name
        INTEGER      , INTENT(IN   ) :: NVARS                       !!  number of (extra) output variables
        CHARACTER*(*), INTENT(IN   ) :: VNAMES( NVARS )             !!  variable-names
        INTEGER      , INTENT(IN   ) :: VTYPES( NVARS )             !!  variable-type M3REAL, etc...)
        INTEGER      , INTENT(IN   ) :: VNDIMS( NVARS )             !!  rank (number of dimensions)
        CHARACTER*(*), INTENT(IN   ) :: VDNAME( NVARS,NMPASDIMS )   !!  names for dimensions used for the variables
        CHARACTER*(*), INTENT(IN   ), OPTIONAL :: VUNITS( NVARS )   !!  variable-units

        IF ( PRESENT( VUNITS ) ) THEN
            CREATEMPAS1 = CREATEMPAS2( FNAME, NMPASDIMS,  MPASDIMNAMES, MPASDIMSIZE,    &
                                       NVARS, VNAMES, VTYPES, VNDIMS, VDNAME, VUNITS )
        ELSE
            CREATEMPAS1 = CREATEMPAS2( FNAME, NMPASDIMS,  MPASDIMNAMES, MPASDIMSIZE,    &
                                       NVARS, VNAMES, VTYPES, VNDIMS, VDNAME )
        END IF

        RETURN

    END FUNCTION CREATEMPAS1


    !!.......................................................................
    !!      Open a new output MPAS-file according to the given specification
    !!.......................................................................

    LOGICAL FUNCTION CREATEMPAS2( FNAME, NDIMS,  DNAMES, DSIZES,            &
                                  NVARS, VNAMES, VTYPES, VNDIMS, VDNAME,    &
                                  VUNITS )

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIMS                   !!  number of dimensions used
        CHARACTER*(*), INTENT(IN   ) :: DNAMES( NDIMS )         !!  dimension-names
        INTEGER      , INTENT(IN   ) :: DSIZES( NDIMS )         !!  dimension-values
        INTEGER      , INTENT(IN   ) :: NVARS                   !!  number of (extra) output variables
        CHARACTER*(*), INTENT(IN   ) :: VNAMES( NVARS )         !!  variable-names
        INTEGER      , INTENT(IN   ) :: VTYPES( NVARS )         !!  variable-type M3REAL, etc...)
        INTEGER      , INTENT(IN   ) :: VNDIMS( NVARS )         !!  rank (number of dimensions)
        CHARACTER*(*), INTENT(IN   ) :: VDNAME( NVARS,NDIMS )   !!  names for dimensions used for the variables
        CHARACTER*(*), INTENT(IN   ), OPTIONAL :: VUNITS( NVARS )   !!  variable-names

        CHARACTER*24, PARAMETER :: PNAME = 'MODMPASFIO/CREATEMPAS():'

        INTEGER         FID, DID, F
        INTEGER         ISTAT, IERR, ID, I, K, M, N, V, VV
        LOGICAL         EFLAG, AFLAG

        CHARACTER(LEN=24)           VNAME
        CHARACTER(LEN=MPSTRLEN) ::  DSCBUF
        CHARACTER(LEN=256)          MESG
        CHARACTER(LEN=512)          EQNAME

        INTEGER         FMODE        !!!  netCDF file-opening mode

        INTEGER         DCOUNT
        INTEGER         DIMID( NMPASDIMS+NDIMS )
        INTEGER         DSIZE( NMPASDIMS+NDIMS )
        CHARACTER*32    DNAME( NMPASDIMS+NDIMS )

        INTEGER         DIMS( 7 )
        INTEGER         DIDS( 7 )

        REAL            RADIANS( MPCELLS+MPEDGES+MPVRTXS )

        !!......................   begin body of function

        IF ( .NOT.INITFLAG ) THEN
            CALL M3MESG( PNAME // ' must call INITMPGRID() before CREATEMPAS()' )
            CREATEMPAS2 = .FALSE.
            RETURN
        END IF

        IF ( NVARS .GT. MXVARS3 ) THEN
            MESG = 'Max NVARS for this build exceeded:  file' // FNAME
            CALL M3WARN( PNAME, 0,0, MESG )
            CREATEMPAS2 = .FALSE.
            RETURN
        END IF

        !!........  If already open, check header and return:

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .GT. 0 ) THEN
            CREATEMPAS2 = MPASCHECK( FNAME, F, NDIMS, NVARS, VNAMES, VTYPES, VNDIMS, VDNAME )
            RETURN
        END IF

        F = MPCOUNT+1
        IF ( F .GT. MXFILE3 ) THEN
            MESG = PNAME // ' File-table overflow for "' //TRIM(FNAME)// '"'
            CALL M3MESG( MESG )
            CREATEMPAS2 = .FALSE.
            RETURN
        END IF

        FMODE = IOR( IOR( NF_NOCLOBBER, NF_SHARE ), NF_64BIT_OFFSET )

        CALL NAMEVAL( FNAME, EQNAME )

        IERR = NF_CREATE( EQNAME, FMODE, FID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error opening "' // TRIM( FNAME ) // '"' )
            CREATEMPAS2 = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        EFLAG = .FALSE.
        MPCDFID( F ) = FID
        MPFILES( F ) = FNAME

        !!........  Global attributes:

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'on_a_sphere',  3, 'YES' )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating netCDF file attribute "on_a_sphere" for ' // FNAME )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'mesh_id',  LEN_TRIM( MESH_ID ), MESH_ID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating netCDF file attribute "mesh_id" for ' // FNAME )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed         !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'mesh_spec',  LEN_TRIM( MESH_SPEC ), MESH_ID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating netCDF file attribute "mesh_spec" for ' // FNAME )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_DOUBLE( FID, NF_GLOBAL, 'sphere_radius', NF_DOUBLE, 1, REARTH )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating netCDF file attribute "sphere_radius" for ' // FNAME )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        !!........  The following are NOWHERE mentioned in the MPAS file specification,
        !!........  but *are* required for the NCAR Java "MPASConvention.java" to work.

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'Conventions',  LEN_TRIM( 'MPAS' ), 'MPAS' )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating netCDF file attribute "Conventions" for ' // FNAME )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'model_name',  LEN_TRIM( 'mpas' ), 'mpas' )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating netCDF file attribute "model_name" for ' // FNAME )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        !!........  Dimensions:  First, "standard" dimensions; then extra "user" dimensions:

        DO N = 1, NMPASDIMS

            DNAME( N ) = MPASDIMNAMES( N )
            I = INDEX1( DNAME( N ), NDIMS, DNAMES )
            IF ( I .GT. 0 ) THEN
                DSIZE( N ) = DSIZES( I )
            ELSE
                DSIZE( N ) = MPASDIMSIZE( N )
            END IF

            IERR = NF_DEF_DIM( FID, DNAME( N ), DSIZE( N ), DIMID( N ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating netCDF dimension "' // TRIM( DNAME( N ) ) // '" for ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END DO

        K = NMPASDIMS
        DO N = 1, NDIMS     !!  Loop through user dimensions:

            I = INDEX1( DNAMES( N ), NMPASDIMS, MPASDIMNAMES )
            IF ( I .GT. 0 ) THEN
                IF ( DSIZES( N ) .NE. MPASDIMSIZE( I ) ) THEN
                    CALL M3MESG( PNAME // ' WARNING: inconsistent dim "' // TRIM( DNAMES( N ) ) // '" for ' // FNAME )
                    EFLAG = .TRUE.
                END IF
                CYCLE
            END IF

            K = K + 1
            DNAME( K ) = DNAMES( N )
            DSIZE( K ) = DSIZES( N )

            IERR = NF_DEF_DIM( FID, DNAMES( N ), DSIZES( N ), DIMID( K ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating netCDF dim "' // TRIM( DNAMES( N ) ) // '" for ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END DO

        DCOUNT = K

        IF ( EFLAG ) THEN
            IERR        = NF_ABORT( FID )
            CREATEMPAS2 = .FALSE.
            RETURN
        END IF

        MPTIMDID( F ) = DIMID(  1 )
        MPTWODID( F ) = DIMID(  2 )
        MPSTRDID( F ) = DIMID(  3 )
        MPCELDID( F ) = DIMID(  4 )
        MPEDGDID( F ) = DIMID(  5 )
        MPVRTDID( F ) = DIMID(  6 )
        MPDEGDID( F ) = DIMID(  7 )
        MPBDYDID( F ) = DIMID(  8 )
        MP2BDYID( F ) = DIMID(  9 )
        MPLVLDID( F ) = DIMID( 10 )

        MPNRECS ( F ) = DSIZE(  1 )
        MPNCELLS( F ) = DSIZE(  4 )
        MPNEDGES( F ) = DSIZE(  5 )
        MPNVRTXS( F ) = DSIZE(  6 )
        MPNVORDR( F ) = DSIZE(  7 )
        MPNBNDYC( F ) = DSIZE(  8 )
        MPNVLVLS( F ) = DSIZE( 10 )


        !!........  Define standard Variables:

        VV = 1
        MPVNAME( VV,F )   = 'indexToCellID'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 4 )
        MPVDIMS( 1,VV,F ) = DSIZE( 4 )
        MPVDIDS( 1,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'indexToEdgeID'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'indexToVertexID'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 6 )
        MPVDIMS( 1,VV,F ) = DSIZE( 6 )
        MPVDIDS( 1,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'nEdgesOnCell'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 4 )
        MPVDIMS( 1,VV,F ) = DSIZE( 4 )
        MPVDIDS( 1,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'nEdgesOnEdge'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'cellsOnCell'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 8 )
        MPVDIMS( 1,VV,F ) = DSIZE( 8 )
        MPVDIDS( 1,VV,F ) = DIMID( 8 )
        MPVDNAM( 2,VV,F ) = MPASDIMNAMES( 4 )
        MPVDIMS( 2,VV,F ) = DSIZE( 4 )
        MPVDIDS( 2,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'edgesOnCell'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 8 )
        MPVDIMS( 1,VV,F ) = DSIZE( 8 )
        MPVDIDS( 1,VV,F ) = DIMID( 8 )
        MPVDNAM( 2,VV,F ) = DNAME( 4 )
        MPVDIMS( 2,VV,F ) = DSIZE( 4 )
        MPVDIDS( 2,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'edgesOnEdge'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 9 )
        MPVDIMS( 1,VV,F ) = DSIZE( 9 )
        MPVDIDS( 1,VV,F ) = DIMID( 9 )
        MPVDNAM( 2,VV,F ) = DNAME( 5 )
        MPVDIMS( 2,VV,F ) = DSIZE( 5 )
        MPVDIDS( 2,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'verticesOnCell'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 8 )
        MPVDIMS( 1,VV,F ) = DSIZE( 8 )
        MPVDIDS( 1,VV,F ) = DIMID( 8 )
        MPVDNAM( 2,VV,F ) = DNAME( 4 )
        MPVDIMS( 2,VV,F ) = DSIZE( 4 )
        MPVDIDS( 2,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'cellsOnEdge'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 2 )
        MPVDIMS( 1,VV,F ) = 2
        MPVDIDS( 1,VV,F ) = DIMID( 2 )
        MPVDNAM( 2,VV,F ) = DNAME( 5 )
        MPVDIMS( 2,VV,F ) = DSIZE( 5 )
        MPVDIDS( 2,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'verticesOnEdge'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 2 )
        MPVDIMS( 1,VV,F ) = 2
        MPVDIDS( 1,VV,F ) = DIMID( 2 )
        MPVDNAM( 2,VV,F ) = DNAME( 5 )
        MPVDIMS( 2,VV,F ) = DSIZE( 5 )
        MPVDIDS( 2,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'cellsOnVertex'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 7 )
        MPVDIMS( 1,VV,F ) = DSIZE( 7 )
        MPVDIDS( 1,VV,F ) = DIMID( 7 )
        MPVDNAM( 2,VV,F ) = DNAME( 6 )
        MPVDIMS( 2,VV,F ) = DSIZE( 6 )
        MPVDIDS( 2,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'edgesOnVertex'
        MPVTYPE( VV,F )   = M3INT
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 7 )
        MPVDIMS( 1,VV,F ) = DSIZE( 7 )
        MPVDIDS( 1,VV,F ) = DIMID( 7 )
        MPVDNAM( 2,VV,F ) = DNAME( 6 )
        MPVDIMS( 2,VV,F ) = DSIZE( 6 )
        MPVDIDS( 2,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE
            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'latCell'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 4 )
        MPVDIMS( 1,VV,F ) = DSIZE( 4 )
        MPVDIDS( 1,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'radians'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'lonCell'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 4 )
        MPVDIMS( 1,VV,F ) = DSIZE( 4 )
        MPVDIDS( 1,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE
            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'radians'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'latEdge'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'radians'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'lonEdge'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'radians'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'latVertex'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 6 )
        MPVDIMS( 1,VV,F ) = DSIZE( 6 )
        MPVDIDS( 1,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'radians'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'lonVertex'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 6 )
        MPVDIMS( 1,VV,F ) = DSIZE( 6 )
        MPVDIDS( 1,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'radians'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'xCell'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 4 )
        MPVDIMS( 1,VV,F ) = DSIZE( 4 )
        MPVDIDS( 1,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'yCell'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 4 )
        MPVDIMS( 1,VV,F ) = DSIZE( 4 )
        MPVDIDS( 1,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'zCell'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 4 )
        MPVDIMS( 1,VV,F ) = DSIZE( 4 )
        MPVDIDS( 1,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'xEdge'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'yEdge'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'zEdge'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'xVertex'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 6 )
        MPVDIMS( 1,VV,F ) = DSIZE( 6 )
        MPVDIDS( 1,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'yVertex'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 6 )
        MPVDIMS( 1,VV,F ) = DSIZE( 6 )
        MPVDIDS( 1,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'zVertex'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 6 )
        MPVDIMS( 1,VV,F ) = DSIZE( 6 )
        MPVDIDS( 1,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'weightsOnEdge'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 9 )
        MPVDIMS( 1,VV,F ) = DSIZE( 9 )
        MPVDIDS( 1,VV,F ) = DIMID( 9 )
        MPVDNAM( 2,VV,F ) = DNAME( 5 )
        MPVDIMS( 2,VV,F ) = DSIZE( 5 )
        MPVDIDS( 2,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'dvEdge'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'dcEdge'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'angleEdge'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 5 )
        MPVDIMS( 1,VV,F ) = DSIZE( 5 )
        MPVDIDS( 1,VV,F ) = DIMID( 5 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'radians'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'areaCell'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 4 )
        MPVDIMS( 1,VV,F ) = DSIZE( 4 )
        MPVDIDS( 1,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M^2'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'areaTriangle'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 6 )
        MPVDIMS( 1,VV,F ) = DSIZE( 6 )
        MPVDIDS( 1,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M^2'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'kiteAreasOnVertex'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 7 )
        MPVDNAM( 2,VV,F ) = DNAME( 6 )
        MPVDIMS( 1,VV,F ) = DSIZE( 7 )
        MPVDIMS( 2,VV,F ) = DSIZE( 6 )
        MPVDIDS( 1,VV,F ) = DIMID( 7 )
        MPVDIDS( 2,VV,F ) = DIMID( 6 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'M^2'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'meshDensity'
        MPVTYPE( VV,F )   = M3DBLE
        MPVDCNT( VV,F )   = 1
        MPVDNAM( 1,VV,F ) = DNAME( 4 )
        MPVDIMS( 1,VV,F ) = DSIZE( 4 )
        MPVDIDS( 1,VV,F ) = DIMID( 4 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE
            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'none'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed

        VV = VV + 1
        MPVNAME( VV,F )   = 'xtime'
        MPVTYPE( VV,F )   = M3CHAR
        MPVDCNT( VV,F )   = 2
        MPVDNAM( 1,VV,F ) = DNAME( 3 )      !!  StrLen
        MPVDNAM( 2,VV,F ) = DNAME( 1 )      !!  Time
        MPVDIMS( 1,VV,F ) = DSIZE( 3 )
        MPVDIMS( 2,VV,F ) = DSIZE( 1 )
        MPVDIDS( 1,VV,F ) = DIMID( 3 )
        MPVDIDS( 2,VV,F ) = DIMID( 1 )
        IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( MPVNAME(VV,F) ) // '" for ' // FNAME )
            EFLAG = .TRUE.

        ELSE

            DSCBUF = MPVNAME( VV,F )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ) , 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( MPVNAME( VV,F ) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = 'YYYY-MM-DD_HH:MM:SS'
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "units" for "' // MPVNAME( VV,F ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END IF              !  ierr nonzero:  operation failed


        !!........  Define "user "Variables:

        DO V = 1, NVARS

            IF ( VNDIMS( V ) .GT. 7 ) THEN
                EFLAG = .TRUE.
                MESG  = 'Too many dimensions for vble "' // TRIM( VNAMES( V ) ) // '" in "' // FNAME
                CALL M3MESG( MESG )
                CYCLE
            END IF

            AFLAG = .FALSE.

            DO N = 1, VNDIMS( V )

                I = INDEX1( VDNAME( V,N ), DCOUNT, DNAME )
                IF ( I .GT. 0 ) THEN
                    DIMS( N ) = DSIZE( I )
                    DIDS( N ) = DIMID( I )
                ELSE
                    AFLAG = .TRUE.
                    MESG  =  PNAME//' Invalid dim "'//TRIM( VDNAME(V,N) )//'" for vble "'//TRIM( VNAMES(V) )//'" in ' // FNAME
                    CALL M3MESG( MESG )
                END IF

            END DO

            IF ( AFLAG ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            M  = VNDIMS( V )
            VV = VV + 1
            MPVNAME( VV,F ) = VNAMES( V )
            MPVTYPE( VV,F ) = VTYPES( V )
            MPVDCNT( VV,F ) = VNDIMS( V )
            MPVDNAM( 1:M,  VV,F ) = VDNAME( V,1:M )
            MPVDIMS( 1:M,  VV,F ) =   DIMS( 1:M )
            MPVDIDS( 1:M,  VV,F ) =   DIDS( 1:M )
            IERR = NF_DEF_VAR( FID, MPVNAME( VV,F ), MPVTYPE( VV,F ), MPVDCNT( VV,F ), MPVDIDS( 1,VV,F ), MPVARID( VV,F ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( VNAMES(V) ) // '" for ' // FNAME )
                EFLAG = .TRUE.
                CYCLE
            END IF              !  ierr nonzero:  operation failed

            DSCBUF = VNAMES( V )
            IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'long_name', MPSTRLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( VNAMES(V) ) // '" in ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

            IF ( PRESENT( VUNITS ) ) THEN
                DSCBUF = VUNITS( V )
                IERR = NF_PUT_ATT_TEXT( FID, MPVARID( VV,F ), 'units', MPSTRLEN, DSCBUF )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3MESG( NF_STRERROR( IERR ) )
                    CALL M3MESG( PNAME // ' Error creating att "units" for "' // TRIM( VNAMES(V) ) // '" in ' // FNAME )
                    EFLAG = .TRUE.
                END IF              !  ierr nonzero:  operation failed
            END IF

        END DO


        !!........  Put FNAME back into data mode:  attributes and variables now defined.

        IERR = NF_ENDDEF( FID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error putting "' // FNAME // '" into data mode' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IF ( EFLAG ) THEN
            IERR        = NF_ABORT( FID )
            CREATEMPAS2 = .FALSE.
            RETURN
        END IF


        MPNVARS( F ) = VV
        MPCOUNT      = F


        !!........  Write standard Variables:  note that Lat- and Lon-coordinates
        !!........  must convert to MPAS radians-unit usage.

        IF ( .NOT.WRITEMPAS( FNAME, 'indexToCellID',  MPCELLS, CELLID ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'indexToEdgeID',  MPEDGES, EDGEID ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'indexToVertexID',MPVRTXS, VRTXID ) )   EFLAG = .TRUE.

        IF ( .NOT.WRITEMPAS( FNAME, 'nEdgesOnCell',   MPCELLS, NBNDYE ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'nEdgesOnEdge',   MPEDGES, NEDGEE ) )   EFLAG = .TRUE.

        IF ( .NOT.WRITEMPAS( FNAME, 'cellsOnCell',    MPBNDYC, MPCELLS, BNDYCELL ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'edgesOnCell',    MPBNDYC, MPCELLS, BNDYEDGE ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'edgesOnEdge',  2*MPBNDYC, MPEDGES, EEDGES   ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'verticesOnCell', MPBNDYC, MPCELLS, BNDYVRTX ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'cellsOnEdge',          2, MPEDGES, ECELLS   ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'verticesOnEdge',       2, MPEDGES, EVRTXS   ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'cellsOnVertex',  MPVORDR, MPVRTXS, VCELLS   ) )   EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'edgesOnVertex',  MPVORDR, MPVRTXS, VEDGES   ) )   EFLAG = .TRUE.

        RADIANS( 1:MPCELLS ) = PI180F * ALATC( 1:MPCELLS )
        IF ( .NOT.WRITEMPAS( FNAME, 'latCell',      MPCELLS, RADIANS ) )  EFLAG = .TRUE.
        RADIANS( 1:MPCELLS ) = PI180F * ALONC( 1:MPCELLS )
        IF ( .NOT.WRITEMPAS( FNAME, 'lonCell',      MPCELLS, RADIANS ) )  EFLAG = .TRUE.
        RADIANS( 1:MPEDGES ) = PI180F * ALATE( 1:MPEDGES )
        IF ( .NOT.WRITEMPAS( FNAME, 'latEdge',      MPEDGES, RADIANS ) )  EFLAG = .TRUE.
        RADIANS( 1:MPEDGES ) = PI180F * ALONE( 1:MPEDGES )
        IF ( .NOT.WRITEMPAS( FNAME, 'lonEdge',      MPEDGES, RADIANS ) )  EFLAG = .TRUE.
        RADIANS( 1:MPVRTXS ) = PI180F * ALATV( 1:MPVRTXS )
        IF ( .NOT.WRITEMPAS( FNAME, 'latVertex',    MPVRTXS, RADIANS ) )  EFLAG = .TRUE.
        RADIANS( 1:MPVRTXS ) = PI180F * ALONV( 1:MPVRTXS )
        IF ( .NOT.WRITEMPAS( FNAME, 'lonVertex',    MPVRTXS, RADIANS ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'xCell',        MPCELLS, XCELL   ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'yCell',        MPCELLS, YCELL   ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'zCell',        MPCELLS, ZCELL   ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'xEdge',        MPEDGES, XEDGE   ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'yEdge',        MPEDGES, YEDGE   ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'zEdge',        MPEDGES, ZEDGE   ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'xVertex',      MPVRTXS, XVRTX   ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'yVertex',      MPVRTXS, YVRTX   ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'zVertex',      MPVRTXS, ZVRTX   ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'dvEdge',       MPEDGES, DVEDGE  ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'dcEdge',       MPEDGES, DCEDGE  ) )  EFLAG = .TRUE.
        RADIANS( 1:MPVRTXS ) = PI180F * EANGLE( 1:MPVRTXS )
        IF ( .NOT.WRITEMPAS( FNAME, 'angleEdge',    MPEDGES, RADIANS ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'areaCell',     MPCELLS, CAREAS  ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'areaTriangle', MPVRTXS, VAREAS  ) )  EFLAG = .TRUE.

        IF ( .NOT.WRITEMPAS( FNAME, 'weightsOnEdge',    2*MPBNDYC, MPEDGES, EWGHTS ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'kiteAreasOnVertex',MPVORDR,   MPVRTXS, KAREAS ) )  EFLAG = .TRUE.
        IF ( .NOT.WRITEMPAS( FNAME, 'meshDensity',      MPCELLS,            MSHDEN ) )  EFLAG = .TRUE.

        IF ( EFLAG ) THEN
            IERR        = NF_ABORT( FID )
            MPCOUNT     = MPCOUNT - 1
            CREATEMPAS2 = .FALSE.
            RETURN
        END IF


        !!........  Log file-opening:

        CALL M3MESG( ' ' )
        CALL M3MESG( 'Output MPAS-file ' // FNAME )
        CALL M3MESG(                  '    Path: '// TRIM( EQNAME ) )
        WRITE( MESG, '( A, I9 )' )    '    NCELLS = ', MPCELLS
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NEDGES = ', MPEDGES
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NVRTXS = ', MPVRTXS
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NVLVLS = ', MPVLVLS
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NVORDR = ', MPVORDR
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NBNDYC = ', MPBNDYC
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, I9 )' )    '    NVARS  = ', NVARS
        CALL M3MESG( MESG )
        WRITE( MESG, '( A, F10.0 )' ) '    REARTH = ', REARTH
        CALL M3MESG( MESG )
        IF ( VERBOSE ) THEN
            CALL M3MESG( 'Variables:' )
            DO V = 1, MPNVARS( F )
                WRITE( MESG, '( 4X, A, 2X, 2 A, 999 ( I6, A, : ) )' )           &
                    NCTYPES( MPVTYPE( V,F ) )( 1:5 ), MPVNAME( V,F ),     &
                    '(', ( MPVDIMS( N,V,F ), ',', N = 1, MPVDCNT (V,F ) )
                N = LEN_TRIM( MESG )
                MESG( N:N ) = ')'
                CALL M3MESG( MESG )
            END DO
        END IF
        CALL M3MESG( ' ' )

        MPCOUNT     = F
        CREATEMPAS2 = .TRUE.
        RETURN

    END FUNCTION CREATEMPAS2


    !!.......................................................................
    !!      Return description for MPAS-file
    !!.......................................................................

    LOGICAL FUNCTION DESCMPAS1( FNAME, NRECS, NVARS, VNAMES, VTYPES, VUNITS, VNDIMS, VDIMS, VDNAME )

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        INTEGER      , INTENT(  OUT) :: NRECS                   !!  number of time steps
        INTEGER      , INTENT(  OUT) :: NVARS                   !!  number of (extra) output variables
        CHARACTER*(*), INTENT(  OUT) :: VNAMES( MXVARS3 )       !!  variable-names
        CHARACTER*(*), INTENT(  OUT) :: VUNITS( MXVARS3 )       !!  variable-names
        INTEGER      , INTENT(  OUT) :: VTYPES( MXVARS3 )       !!  variable-type M3REAL, etc...)
        INTEGER      , INTENT(  OUT) :: VNDIMS( MXVARS3 )       !!  rank (number of dimensions)
        INTEGER      , INTENT(  OUT) :: VDIMS ( 7,MXVARS3 )     !!  list of dimensions
        CHARACTER*(*), INTENT(  OUT) :: VDNAME( 7,MXVARS3 )     !!  names for dimensions used for the variables

        INTEGER         F, V, N, IERR
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/DESCMPAS():  must call INITMPGRID() before DESCMPAS()' )
            DESCMPAS1 = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/DESCMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            DESCMPAS1 = .FALSE.
            RETURN
        END IF

        EFLAG = .FALSE.

        IF ( DESCMPAS2( FNAME, NRECS, NVARS, VNAMES, VTYPES, VNDIMS, VDIMS, VDNAME ) ) THEN

            NRECS = MPNRECS( F )
            NVARS = MPNVARS( F )

            DO V = 1, MIN( MXVARS3, NVARS )

                IERR = NF_GET_ATT_TEXT( MPCDFID( F ), MPVARID( V,F ), 'units', VUNITS( V ) )
                IF ( IERR .EQ. NF_EEXIST ) THEN
                    VUNITS( V ) = CMISS3
                ELSE IF ( IERR .NE. 0 ) THEN
                    MESG = 'MODMPASFIO/DESCMPAS(): Error reading att "units" for  variable "' // &
                           TRIM( VNAMES(V) ) // '" in file "' // TRIM( FNAME ) // '"'
                    CALL M3MESG( NF_STRERROR( IERR ) )
                    CALL M3MESG( MESG )
                    VUNITS( V ) = CMISS3
                END IF

            END DO

        END IF

        DESCMPAS1 = ( .NOT.EFLAG )
        RETURN

    END FUNCTION DESCMPAS1


    !!.......................................................................

    LOGICAL FUNCTION DESCMPAS2( FNAME, NRECS, NVARS, VNAMES, VTYPES, VNDIMS, VDIMS, VDNAME )

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        INTEGER      , INTENT(  OUT) :: NRECS                   !!  number of time steps
        INTEGER      , INTENT(  OUT) :: NVARS                   !!  number of (extra) output variables
        CHARACTER*(*), INTENT(  OUT) :: VNAMES( MXVARS3 )       !!  variable-names
        INTEGER      , INTENT(  OUT) :: VTYPES( MXVARS3 )       !!  variable-type M3REAL, etc...)
        INTEGER      , INTENT(  OUT) :: VNDIMS( MXVARS3 )       !!  rank (number of dimensions)
        INTEGER      , INTENT(  OUT) :: VDIMS ( 7,MXVARS3 )     !!  list of dimensions
        CHARACTER*(*), INTENT(  OUT) :: VDNAME( 7,MXVARS3 )     !!  names for dimensions used for the variables

        CHARACTER*1, PARAMETER :: BLANK = ' '

        INTEGER         F, V, N, IERR
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/DESCMPAS():  must call INITMPGRID() before DESCMPAS()' )
            DESCMPAS2 = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/DESCMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            DESCMPAS2 = .FALSE.
            RETURN
        END IF

        NRECS = MPNRECS( F )
        NVARS = MPNVARS( F )
        EFLAG = .FALSE.

        DO V = 1, MIN( MXVARS3, NVARS )

            VNAMES( V ) = MPVNAME( V,F )
            VTYPES( V ) = MPVTYPE( V,F )
            VNDIMS( V ) = MPVDCNT( V,F )
            VNDIMS( V ) = MPVDCNT( V,F )

            N = VNDIMS( V )
            VDIMS (  1:N, V ) = MPVDIMS( 1:N,V,F )
            VDIMS ( N+1:, V ) = 0
            VDNAME(  1:N, V ) = MPVDNAM( 1:N,V,F )
            VDNAME( N+1:, V ) = BLANK

        END DO

        DESCMPAS2 = ( .NOT.EFLAG )
        RETURN

    END FUNCTION DESCMPAS2


    !!.......................................................................
    !!      Check output MPAS-file header, according to the given specification
    !!.......................................................................

    LOGICAL FUNCTION MPASCHECK( FNAME, FNUM, NDIMS, NVARS, VNAMES, VTYPES, VNDIMS, VDNAME )

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        INTEGER      , INTENT(IN   ) :: FNUM                    !!  MPAS file-table subscript
        INTEGER      , INTENT(IN   ) :: NDIMS                   !!  number of dimensions used
        INTEGER      , INTENT(IN   ) :: NVARS                   !!  number of (extra) output variables
        CHARACTER*(*), INTENT(IN   ) :: VNAMES( NVARS )         !!  variable-names
        INTEGER      , INTENT(IN   ) :: VTYPES( NVARS )         !!  variable-type M3REAL, etc...)
        INTEGER      , INTENT(IN   ) :: VNDIMS( NVARS )         !!  rank (number of dimensions)
        CHARACTER*(*), INTENT(IN   ) :: VDNAME( NVARS,NDIMS )   !!  names for dimensions used for the variables

        CHARACTER*20, PARAMETER :: PNAME = 'MODMPASFIO/MPASCHECK'

        INTEGER         V, N, I, K
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        !!......................   begin body of function

        EFLAG = .FALSE.     !!  no errors yet

        IF ( NVARS .NE. MPNVARS( FNUM ) ) THEN
            CALL M3MESG( PNAME//':  Bad NVARS for ' // FNAME )
            EFLAG = .TRUE.
        END IF

        DO V = 1, NVARS

            IF ( VNAMES( V ) .NE. MPVNAME( V,FNUM ) ) THEN
                MESG = PNAME//':  Bad vble-name "' // TRIM( VNAMES(V) ) // '" in ' // FNAME
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( VTYPES( V ) .NE. MPVTYPE( V,FNUM ) ) THEN
                MESG = PNAME//':  Bad vble-type for "' // TRIM( VNAMES(V) ) // '" in ' // FNAME
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( VNDIMS( V ) .NE. MPVDCNT( V,FNUM ) ) THEN
                MESG = PNAME//':  Bad vble-NDIMS for "' // TRIM( VNAMES(V) ) // '" in ' // FNAME
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE

                N = MPVDCNT( V,FNUM )
                DO K = 1, N
                    IF ( VDNAME( V,K ) .NE. MPVDNAM( K,V,FNUM ) ) THEN
                        MESG = PNAME // ':  Bad DIM "' // TRIM( VDNAME( V,K ) ) //      &
                                        '" for vble "' // TRIM( VNAMES( V ) )   //      &
                                        '" in '        // FNAME
                        CALL M3MESG( MESG )
                        EFLAG = .TRUE.
                    END IF
                END DO

            END IF

        END DO      !!  end loop on variables

        MPASCHECK = ( .NOT.EFLAG )

        RETURN

    END FUNCTION MPASCHECK



    !!.......................................................................
    !!  TIME INDEPENDENT CASES:  READ
    !!.......................................................................
    !!  Generic interfaces:  REAL*8 SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS0DD( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        REAL(8)      , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DD = .FALSE.
            RETURN
        END IF

        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DD = .TRUE.
        RETURN

    END FUNCTION READMPAS0DD

    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS0DR( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        REAL         , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DR = .FALSE.
            RETURN
        END IF

        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DR = .TRUE.
        RETURN

    END FUNCTION READMPAS0DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS0DI( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DI = .FALSE.
            RETURN
        END IF

        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DI = .TRUE.
        RETURN

    END FUNCTION READMPAS0DI


    !!  Generic interfaces:  INTEGER*2 4-D SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS0DS( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER*2    , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DS = .FALSE.
            RETURN
        END IF

        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DS = .TRUE.
        RETURN

    END FUNCTION READMPAS0DS


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 4-D SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS0DB( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER*1    , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DB = .FALSE.
            RETURN
        END IF

        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DB = .TRUE.
        RETURN

    END FUNCTION READMPAS0DB


    !!.......................................................................
    !!  Generic interfaces:  REAL(8) 1-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS1DD( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        REAL(8)      , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DD = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS1DD = .TRUE.
        RETURN

    END FUNCTION READMPAS1DD


    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS1DR( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        REAL         , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DR = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS1DR = .TRUE.
        RETURN

    END FUNCTION READMPAS1DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS1DI( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        INTEGER      , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DI = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS1DI = .TRUE.
        RETURN

    END FUNCTION READMPAS1DI


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*2 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS1DS( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        INTEGER*2    , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DS = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS1DS = .TRUE.
        RETURN

    END FUNCTION READMPAS1DS


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS1DB( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        INTEGER*1    , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DB = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS1DB = .TRUE.
        RETURN

    END FUNCTION READMPAS1DB


    !!.......................................................................
    !!  Generic interfaces:  REAL(8) 2-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS2DD( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        REAL(8)      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DD = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS2DD = .TRUE.
        RETURN

    END FUNCTION READMPAS2DD


    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS2DR( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        REAL         , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DR = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS2DR = .TRUE.
        RETURN

    END FUNCTION READMPAS2DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS2DI( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        INTEGER      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DI = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS2DI = .TRUE.
        RETURN

    END FUNCTION READMPAS2DI


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*2 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS2DS( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        INTEGER*2    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DS = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS2DS = .TRUE.
        RETURN

    END FUNCTION READMPAS2DS



    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS2DB( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        INTEGER*1    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DB = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS2DB = .TRUE.
        RETURN

    END FUNCTION READMPAS2DB




    !!.......................................................................
    !!  Generic interfaces:  REAL(8) 3-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS3DD( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        REAL(8)      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DD = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS3DD = .TRUE.
        RETURN

    END FUNCTION READMPAS3DD


    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS3DR( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        REAL         , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DR = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS3DR = .TRUE.
        RETURN

    END FUNCTION READMPAS3DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS3DI( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        INTEGER      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DI = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS3DI = .TRUE.
        RETURN

    END FUNCTION READMPAS3DI


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*2 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS3DS( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        INTEGER*2    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DS = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS3DS = .TRUE.
        RETURN

    END FUNCTION READMPAS3DS


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS3DB( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        INTEGER*1    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DB = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS3DB = .TRUE.
        RETURN

    END FUNCTION READMPAS3DB


    !!.......................................................................
    !!  Generic interfaces:  REAL*8 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS4DD( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        REAL(8)      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DD = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DD = .TRUE.
        RETURN

    END FUNCTION READMPAS4DD


    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS4DR( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        REAL         , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DR = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DR = .TRUE.
        RETURN

    END FUNCTION READMPAS4DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS4DI( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DI = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DI = .TRUE.
        RETURN

    END FUNCTION READMPAS4DI


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS4DS( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*2    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DS = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DS = .TRUE.
        RETURN

    END FUNCTION READMPAS4DS


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION READMPAS4DB( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*1    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DB = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DB = .TRUE.
        RETURN

    END FUNCTION READMPAS4DB


    !!.............................................................................\
    !!  TIME STEPPED CASES:  READ
    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS0DDT( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        REAL(8)      , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS0DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS0DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DDT = .TRUE.
        RETURN

    END FUNCTION READMPAS0DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS0DRT( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        REAL         , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS0DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS0DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DRT = .TRUE.
        RETURN

    END FUNCTION READMPAS0DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS0DIT( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER      , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS0DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS0DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DIT = .TRUE.
        RETURN

    END FUNCTION READMPAS0DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS0DST( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER*2    , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS0DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS0DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DST = .TRUE.
        RETURN

    END FUNCTION READMPAS0DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS0DBT( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER*1    , INTENT(  OUT) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS0DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS0DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS0DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS0DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS0DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS0DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS0DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS0DBT = .TRUE.
        RETURN

    END FUNCTION READMPAS0DBT


    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS1DDT( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        REAL(8)      , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS1DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS1DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS1DDT = .TRUE.
        RETURN

    END FUNCTION READMPAS1DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS1DRT( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        REAL         , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS1DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS1DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS1DRT = .TRUE.
        RETURN

    END FUNCTION READMPAS1DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS1DIT( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        INTEGER      , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS1DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS1DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS1DIT = .TRUE.
        RETURN

    END FUNCTION READMPAS1DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS1DST( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        INTEGER*2    , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS1DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS1DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        READMPAS1DST = .TRUE.
        RETURN

    END FUNCTION READMPAS1DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS1DBT( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        INTEGER*1    , INTENT(  OUT) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS1DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS1DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS1DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS1DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS1DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS1DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS1DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS1DBT = .TRUE.
        RETURN

    END FUNCTION READMPAS1DBT


    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS2DDT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        REAL(8)      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS2DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS2DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS2DDT = .TRUE.
        RETURN

    END FUNCTION READMPAS2DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS2DRT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        REAL         , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS2DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS2DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        READMPAS2DRT = .TRUE.
        RETURN

    END FUNCTION READMPAS2DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS2DIT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        INTEGER      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS2DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS2DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS2DIT = .TRUE.
        RETURN

    END FUNCTION READMPAS2DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS2DST( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        INTEGER*2    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS2DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS2DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        READMPAS2DST = .TRUE.
        RETURN

    END FUNCTION READMPAS2DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS2DBT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        INTEGER*1    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS2DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS2DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS2DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS2DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS2DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS2DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS2DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS2DBT = .TRUE.
        RETURN

    END FUNCTION READMPAS2DBT


    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS3DDT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        REAL(8)      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS3DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        READMPAS3DDT = .TRUE.
        RETURN

    END FUNCTION READMPAS3DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS3DRT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        REAL         , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS3DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS3DRT = .TRUE.
        RETURN

    END FUNCTION READMPAS3DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS3DIT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        INTEGER      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS3DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        READMPAS3DIT = .TRUE.
        RETURN

    END FUNCTION READMPAS3DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS3DST( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        INTEGER*2    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS3DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS3DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS3DST = .TRUE.
        RETURN

    END FUNCTION READMPAS3DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS3DBT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        INTEGER*1    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS3DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS3DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS3DBT = .TRUE.
        RETURN

    END FUNCTION READMPAS3DBT


    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS4DDT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        REAL(8)      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS4DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DDT = .TRUE.
        RETURN

    END FUNCTION READMPAS4DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS4DRT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        REAL         , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS4DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DRT = .TRUE.
        RETURN

    END FUNCTION READMPAS4DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS4DIT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER      , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS4DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_GET_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DIT = .TRUE.
        RETURN

    END FUNCTION READMPAS4DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS4DST( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*2    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS4DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS4DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DST = .TRUE.
        RETURN

    END FUNCTION READMPAS4DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION READMPAS4DBT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*1    , INTENT(  OUT) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  must call INITMPGRID() before READMPAS()' )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  No Time-dimension in ' // FNAME )
            READMPAS4DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/READMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            READMPAS4DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        READMPAS4DBT = .TRUE.
        RETURN

    END FUNCTION READMPAS4DBT



    !!.......................................................................
    !!  TIME INDEPENDENT CASES:  WRITE
    !!.......................................................................
    !!  Generic interfaces:  REAL*8 SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS0DD( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        REAL(8)      , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DD = .FALSE.
            RETURN
        END IF

        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS0DD = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DD

    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS0DR( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        REAL         , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DR = .FALSE.
            RETURN
        END IF

        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS0DR = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS0DI( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DI = .FALSE.
            RETURN
        END IF

        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS0DI = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DI


    !!  Generic interfaces:  INTEGER*2 4-D SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS0DS( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER*2    , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DS = .FALSE.
            RETURN
        END IF

        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS0DS = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DS


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 4-D SCALAR case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS0DB( FNAME, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER*1    , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DB = .FALSE.
            RETURN
        END IF

        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS0DB = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DB


    !!.......................................................................
    !!  Generic interfaces:  REAL*8 1-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS1DD( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DD = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS1DD = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DD


    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS1DR( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DR = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS1DR = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS1DI( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DI = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS1DI = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DI


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*2 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS1DS( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DS = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS1DS = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DS


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS1DB( FNAME, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DB = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DELS( 1 ) = NDIM1
        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS1DB = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DB


    !!.......................................................................
    !!  Generic interfaces:  REAL*8 2-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS2DD( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DD = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS2DD = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DD


    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS2DR( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DR = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS2DR = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS2DI( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DI = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS2DI = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DI


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*2 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS2DS( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DS = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS2DS = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DS



    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS2DB( FNAME, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DB = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS2DB = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DB




    !!.......................................................................
    !!  Generic interfaces:  REAL*8 3-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS3DD( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DD = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS3DD = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DD


    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS3DR( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DR = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS3DR = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS3DI( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DI = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS3DI = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DI


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*2 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS3DS( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DS = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS3DS = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DS


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS3DB( FNAME, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DB = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS3DB = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DB


    !!.......................................................................
    !!  Generic interfaces:  REAL*8 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS4DD( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DD = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DD = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DD = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DD = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DD = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DD = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS4DD = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DD


    !!.......................................................................
    !!  Generic interfaces:  REAL 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS4DR( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DR = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DR = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DR = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DR = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DR = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DR = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS4DR = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DR


    !!.......................................................................
    !!  Generic interfaces:  INTEGER 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS4DI( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DI = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DI = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DI = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DI = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DI = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DI = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS4DI = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DI


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS4DS( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DS = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DS = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DS = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DS = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DS = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DS = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS4DS = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DS


    !!.......................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D array case:
    !!  Write variable VNAME to previously-opened MPAS file FNAME
    !!.......................................................................

    LOGICAL FUNCTION WRITEMPAS4DB( FNAME, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DB = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DB = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DB = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DB = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DB = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DB = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS4DB = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DB


    !!.............................................................................\
    !!  TIME STEPPED CASES:  WRITE
    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS0DDT( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        REAL(8)      , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS0DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS0DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS0DDT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS0DRT( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        REAL         , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS0DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS0DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS0DRT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS0DIT( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER      , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS0DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS0DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS0DIT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS0DST( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER*2    , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS0DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS0DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS0DST = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped SCALAR case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS0DBT( FNAME, ISTEP, VNAME, SCALAR )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER*1    , INTENT(IN   ) :: SCALAR

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS0DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS0DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS0DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS0DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS0DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 1,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS0DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = ISTEP
        DELS( 1 ) = 1
        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, SCALAR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS0DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS0DBT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS0DBT


    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS1DDT( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS1DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS1DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS1DDT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS1DRT( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS1DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS1DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS1DRT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS1DIT( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS1DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS1DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS1DIT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS1DST( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS1DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS1DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS1DST = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS1DBT( FNAME, ISTEP, VNAME, NDIM1, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS1DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS1DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS1DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS1DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS1DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 2,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS1DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = 1
        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS1DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS1DBT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS1DBT


    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS2DDT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        WRITEMPAS2DDT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS2DRT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS2DRT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS2DIT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS2DIT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS2DST( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS2DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS2DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS2DST = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS2DBT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 3,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = 1
        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS2DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS2DBT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS2DBT


    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS3DDT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS3DDT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS3DRT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS3DRT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS3DIT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS3DIT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS3DST( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS3DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS3DST = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS3DBT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 4,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = 1
        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS3DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS3DBT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS3DBT


    !!.............................................................................
    !!  Generic interfaces:  REAL*8 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS4DDT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DDT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS4DDT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DDT


    !!.............................................................................
    !!  Generic interfaces:  REAL 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS4DRT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DRT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS4DRT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DRT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER  4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS4DIT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DIT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS4DIT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DIT


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*2 ("short") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS4DST( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS4DST = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DST = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS4DST = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DST


    !!.............................................................................
    !!  Generic interfaces:  INTEGER*1 ("byte") 4-D time-stepped array case:
    !!  Write timestep ISTEP of variable VNAME to previously-opened MPAS file FNAME
    !!.............................................................................

    LOGICAL FUNCTION WRITEMPAS4DBT( FNAME, ISTEP, VNAME, NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: ISTEP, NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER         FID, VID
        INTEGER         IERR, ID, N, F, V
        INTEGER         DIMS( 7 ), DELS( 7 )
        LOGICAL         EFLAG
        CHARACTER*512   EQNAME, MESG

        !!......................   begin body of function

        IF ( MPCOUNT .EQ. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  must call INITMPGRID() before WRITEMPAS()' )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF

        F = INDEX1( FNAME, MPCOUNT, MPFILES )
        IF ( F .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  File "' // TRIM( FNAME ) // '" not yet opened' )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF
        FID = MPCDFID( F )

        IF ( ISTEP .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Invalid (nonpositive) time step number for ' // FNAME )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF

        V = INDEX1( VNAME, MPNVARS( F ), MPVNAME( :,F ) )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Variable "' // TRIM( VNAME ) // '" not found in ' // FNAME )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF
        VID = MPVARID( V,F )

        IF ( MPVDCNT( V,F ) .NE. 5 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIMS for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 1,V,F ) .NE. NDIM1 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 2,V,F ) .NE. NDIM2 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM2 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 3,V,F ) .NE. NDIM3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM3 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPVDIMS( 4,V,F ) .NE. NDIM4 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM4 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF

        IF ( MPTIMDID( F ) .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  No Time-dimension in ' // FNAME )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        ELSE IF ( MPVDIDS( 5,V,F ) .NE. MPTIMDID( F ) ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  Bad NDIM1 for "' // TRIM( VNAME ) // '" in ' // FNAME )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF

        DIMS( 1 ) = 1
        DIMS( 2 ) = 1
        DIMS( 3 ) = 1
        DIMS( 4 ) = 1
        DIMS( 5 ) = ISTEP
        DELS( 1 ) = NDIM1
        DELS( 2 ) = NDIM2
        DELS( 3 ) = NDIM3
        DELS( 4 ) = NDIM4
        DELS( 5 ) = 1
        IERR  = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, ARRAY )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'MODMPASFIO/WRITEMPAS():  error writing "' // TRIM( VNAME ) // '" to ' // FNAME )
            CALL M3MESG( NF_STRERROR( IERR ) )
            WRITEMPAS4DBT = .FALSE.
            RETURN
        END IF              !  ierr nonzero:  operation failed

        MPNRECS( F ) = MAX( ISTEP, MPNRECS( F ) )
        WRITEMPAS4DBT = .TRUE.
        RETURN

    END FUNCTION WRITEMPAS4DBT


    !!.............................................................................
    !!   Validate input data:  check against netCDF fill-values
    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_4DD( NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        REAL(8), PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_DOUBLE
        REAL(8), PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_DOUBLE

        INTEGER     I, J, K, L, M
        REAL(8)     VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_4DD = .TRUE.
            RETURN
        END IF

        DO L = 1, NDIM4
        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K,L )
            IF ( VAL .GT. FILL_LO ) THEN
                IF ( VAL .LT. FILL_HI ) THEN
                    CHKFILL_4DD = .FALSE.
                    RETURN
                END IF
            END IF
        END DO
        END DO
        END DO
        END DO

        CHKFILL_4DD = .TRUE.
        RETURN

    END FUNCTION CHKFILL_4DD


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_4DR( NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )
        INTEGER, INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        REAL   , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        REAL, PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_FLOAT
        REAL, PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_FLOAT

        INTEGER     I, J, K, L, M
        REAL        VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_4DR = .TRUE.
            RETURN
        END IF

        DO L = 1, NDIM4
        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K,L )
            IF ( VAL .GT. FILL_LO ) THEN
                IF ( VAL .LT. FILL_HI ) THEN
                    CHKFILL_4DR = .FALSE.
                    RETURN
                END IF
            END IF
        END DO
        END DO
        END DO
        END DO

        CHKFILL_4DR = .TRUE.
        RETURN

    END FUNCTION CHKFILL_4DR


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_4DI( NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )
        INTEGER, INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER, INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER     I, J, K, L, M
        INTEGER     VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_4DI = .TRUE.
            RETURN
        END IF

        DO L = 1, NDIM4
        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K,L )
            IF ( VAL .EQ. NF_FILL_INT ) THEN
                CHKFILL_4DI = .FALSE.
                RETURN
            END IF
        END DO
        END DO
        END DO
        END DO

        CHKFILL_4DI = .TRUE.
        RETURN

    END FUNCTION CHKFILL_4DI


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_4DS( NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )
        INTEGER  , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*2, INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER     I, J, K, L, M
        INTEGER*2   VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_4DS = .TRUE.
            RETURN
        END IF

        DO L = 1, NDIM4
        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K,L )
            IF ( VAL .EQ. NF_FILL_INT2 ) THEN
                CHKFILL_4DS = .FALSE.
                RETURN
            END IF
        END DO
        END DO
        END DO
        END DO

        CHKFILL_4DS = .TRUE.
        RETURN

    END FUNCTION CHKFILL_4DS


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_4DB( NDIM1, NDIM2, NDIM3, NDIM4, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3, NDIM4
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3, NDIM4 )

        INTEGER     I, J, K, L, M
        INTEGER*1   VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_4DB = .TRUE.
            RETURN
        END IF

        DO L = 1, NDIM4
        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K,L )
            IF ( VAL .EQ. NF_FILL_INT1 ) THEN
                CHKFILL_4DB = .FALSE.
                RETURN
            END IF
        END DO
        END DO
        END DO
        END DO

        CHKFILL_4DB = .TRUE.
        RETURN

    END FUNCTION CHKFILL_4DB


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_3DD( NDIM1, NDIM2, NDIM3, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        REAL(8), PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_DOUBLE
        REAL(8), PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_DOUBLE

        INTEGER     I, J, K
        REAL(8)     VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_3DD = .TRUE.
            RETURN
        END IF

        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K )
            IF ( VAL .GT. FILL_LO ) THEN
                IF ( VAL .LT. FILL_HI ) THEN
                    CHKFILL_3DD = .FALSE.
                    RETURN
                END IF
            END IF
        END DO
        END DO
        END DO

        CHKFILL_3DD = .TRUE.
        RETURN

    END FUNCTION CHKFILL_3DD


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_3DR( NDIM1, NDIM2, NDIM3, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        REAL   , PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_FLOAT
        REAL   , PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_FLOAT

        INTEGER     I, J, K
        REAL        VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_3DR = .TRUE.
            RETURN
        END IF

        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K )
            IF ( VAL .GT. FILL_LO ) THEN
                IF ( VAL .LT. FILL_HI ) THEN
                    CHKFILL_3DR = .FALSE.
                    RETURN
                END IF
            END IF
        END DO
        END DO
        END DO

        CHKFILL_3DR = .TRUE.
        RETURN

    END FUNCTION CHKFILL_3DR


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_3DI( NDIM1, NDIM2, NDIM3, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER     I, J, K
        INTEGER     VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_3DI = .TRUE.
            RETURN
        END IF

        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K )
            IF ( VAL .EQ. NF_FILL_INT ) THEN
                CHKFILL_3DI = .FALSE.
                RETURN
            END IF
        END DO
        END DO
        END DO

        CHKFILL_3DI = .TRUE.
        RETURN

    END FUNCTION CHKFILL_3DI


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_3DS( NDIM1, NDIM2, NDIM3, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER     I, J, K
        INTEGER*2   VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_3DS = .TRUE.
            RETURN
        END IF

        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K )
            IF ( VAL .EQ. NF_FILL_INT2 ) THEN
                CHKFILL_3DS = .FALSE.
                RETURN
            END IF
        END DO
        END DO
        END DO

        CHKFILL_3DS = .TRUE.
        RETURN

    END FUNCTION CHKFILL_3DS


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_3DB( NDIM1, NDIM2, NDIM3, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2, NDIM3
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2, NDIM3 )

        INTEGER     I, J, K
        INTEGER*1   VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_3DB = .TRUE.
            RETURN
        END IF

        DO K = 1, NDIM3
        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J,K )
            IF ( VAL .EQ. NF_FILL_INT1 ) THEN
                CHKFILL_3DB = .FALSE.
                RETURN
            END IF
        END DO
        END DO
        END DO

        CHKFILL_3DB = .TRUE.
        RETURN

    END FUNCTION CHKFILL_3DB


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_2DD( NDIM1, NDIM2, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        REAL(8), PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_DOUBLE
        REAL(8), PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_DOUBLE

        INTEGER     I, J
        REAL(8)     VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_2DD = .TRUE.
            RETURN
        END IF

        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J )
            IF ( VAL .GT. FILL_LO ) THEN
                IF ( VAL .LT. FILL_HI ) THEN
                    CHKFILL_2DD = .FALSE.
                    RETURN
                END IF
            END IF
        END DO
        END DO

        CHKFILL_2DD = .TRUE.
        RETURN

    END FUNCTION CHKFILL_2DD


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_2DR( NDIM1, NDIM2, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        REAL   , PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_FLOAT
        REAL   , PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_FLOAT

        INTEGER     I, J
        REAL        VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_2DR = .TRUE.
            RETURN
        END IF

        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J )
            IF ( VAL .GT. FILL_LO ) THEN
                IF ( VAL .LT. FILL_HI ) THEN
                    CHKFILL_2DR = .FALSE.
                    RETURN
                END IF
            END IF
        END DO
        END DO

        CHKFILL_2DR = .TRUE.
        RETURN

    END FUNCTION CHKFILL_2DR


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_2DI( NDIM1, NDIM2, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER     I, J
        INTEGER     VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_2DI = .TRUE.
            RETURN
        END IF

        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J )
            IF ( VAL .EQ. NF_FILL_INT ) THEN
                CHKFILL_2DI = .FALSE.
                RETURN
            END IF
        END DO
        END DO

        CHKFILL_2DI = .TRUE.
        RETURN

    END FUNCTION CHKFILL_2DI


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_2DS( NDIM1, NDIM2, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER     I, J
        INTEGER*2   VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_2DS = .TRUE.
            RETURN
        END IF

        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J )
            IF ( VAL .EQ. NF_FILL_INT2 ) THEN
                CHKFILL_2DS = .FALSE.
                RETURN
            END IF
        END DO
        END DO

        CHKFILL_2DS = .TRUE.
        RETURN

    END FUNCTION CHKFILL_2DS


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_2DB( NDIM1, NDIM2, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1, NDIM2
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1, NDIM2 )

        INTEGER     I, J
        INTEGER*1   VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_2DB = .TRUE.
            RETURN
        END IF

        DO J = 1, NDIM2
        DO I = 1, NDIM1
            VAL = ARRAY( I,J )
            IF ( VAL .EQ. NF_FILL_INT1 ) THEN
                CHKFILL_2DB = .FALSE.
                RETURN
            END IF
        END DO
        END DO

        CHKFILL_2DB = .TRUE.
        RETURN

    END FUNCTION CHKFILL_2DB


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_1DD( NDIM1, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1
        REAL(8)      , INTENT(IN   ) :: ARRAY( NDIM1 )

        REAL(8), PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_DOUBLE
        REAL(8), PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_DOUBLE

        INTEGER     I
        REAL(8)     VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_1DD = .TRUE.
            RETURN
        END IF

        DO I = 1, NDIM1
            VAL = ARRAY( I )
            IF ( VAL .GT. FILL_LO ) THEN
                IF ( VAL .LT. FILL_HI ) THEN
                    CHKFILL_1DD = .FALSE.
                    RETURN
                END IF
            END IF
        END DO

        CHKFILL_1DD = .TRUE.
        RETURN

    END FUNCTION CHKFILL_1DD


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_1DR( NDIM1, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1
        REAL         , INTENT(IN   ) :: ARRAY( NDIM1 )

        REAL   , PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_FLOAT
        REAL   , PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_FLOAT

        INTEGER     I
        REAL        VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_1DR = .TRUE.
            RETURN
        END IF

        DO I = 1, NDIM1
            VAL = ARRAY( I )
            IF ( VAL .GT. FILL_LO ) THEN
                IF ( VAL .LT. FILL_HI ) THEN
                    CHKFILL_1DR = .FALSE.
                    RETURN
                END IF
            END IF
        END DO

        CHKFILL_1DR = .TRUE.
        RETURN

    END FUNCTION CHKFILL_1DR


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_1DI( NDIM1, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1
        INTEGER      , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER     I
        INTEGER     VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_1DI = .TRUE.
            RETURN
        END IF

        DO I = 1, NDIM1
            VAL = ARRAY( I )
            IF ( VAL .EQ. NF_FILL_INT ) THEN
                CHKFILL_1DI = .FALSE.
                RETURN
            END IF
        END DO

        CHKFILL_1DI = .TRUE.
        RETURN

    END FUNCTION CHKFILL_1DI


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_1DS( NDIM1, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1
        INTEGER*2    , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER     I
        INTEGER*2   VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_1DS = .TRUE.
            RETURN
        END IF

        DO I = 1, NDIM1
            VAL = ARRAY( I )
            IF ( VAL .EQ. NF_FILL_INT2 ) THEN
                CHKFILL_1DS = .FALSE.
                RETURN
            END IF
        END DO

        CHKFILL_1DS = .TRUE.
        RETURN

    END FUNCTION CHKFILL_1DS


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_1DB( NDIM1, ARRAY )
        INTEGER      , INTENT(IN   ) :: NDIM1
        INTEGER*1    , INTENT(IN   ) :: ARRAY( NDIM1 )

        INTEGER     I
        INTEGER*1   VAL

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_1DB = .TRUE.
            RETURN
        END IF

        DO I = 1, NDIM1
            VAL = ARRAY( I )
            IF ( VAL .EQ. NF_FILL_INT1 ) THEN
                CHKFILL_1DB = .FALSE.
                RETURN
            END IF
        END DO

        CHKFILL_1DB = .TRUE.
        RETURN

    END FUNCTION CHKFILL_1DB


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_0DD( SCALAR )
        REAL(8)      , INTENT(IN   ) :: SCALAR

        REAL(8), PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_DOUBLE
        REAL(8), PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_DOUBLE

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_0DD = .TRUE.
            RETURN
        END IF

        IF ( SCALAR .GT. FILL_LO ) THEN
            IF ( SCALAR .LT. FILL_HI ) THEN
                CHKFILL_0DD = .FALSE.
                RETURN
            END IF
        END IF

        CHKFILL_0DD = .TRUE.
        RETURN

    END FUNCTION CHKFILL_0DD


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_0DR( SCALAR )
        REAL, INTENT(IN   ) :: SCALAR

        REAL, PARAMETER :: FILL_LO  = 0.999999 * NF_FILL_FLOAT
        REAL, PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_FLOAT

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_0DR = .TRUE.
            RETURN
        END IF

        IF ( SCALAR .GT. FILL_LO ) THEN
            IF ( SCALAR .LT. FILL_HI ) THEN
                CHKFILL_0DR = .FALSE.
                RETURN
            END IF
        END IF

        CHKFILL_0DR = .TRUE.
        RETURN

    END FUNCTION CHKFILL_0DR


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_0DI( SCALAR )
        INTEGER, INTENT(IN   ) :: SCALAR
        REAL(8), PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_DOUBLE

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_0DI = .TRUE.
            RETURN
        END IF

        CHKFILL_0DI = ( SCALAR .NE. NF_FILL_INT )
        RETURN

    END FUNCTION CHKFILL_0DI


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_0DS( SCALAR )
        INTEGER*2, INTENT(IN   ) :: SCALAR
        REAL(8), PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_DOUBLE

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_0DS = .TRUE.
            RETURN
        END IF

        CHKFILL_0DS = ( SCALAR .NE. NF_FILL_INT2 )
        RETURN

    END FUNCTION CHKFILL_0DS


    !!.............................................................................


    LOGICAL FUNCTION CHKFILL_0DB( SCALAR )
        INTEGER*1    , INTENT(IN   ) :: SCALAR
        REAL(8), PARAMETER :: FILL_HI  = 1.000001 * NF_FILL_DOUBLE

        IF ( .NOT.CHK_FILL ) THEN
            CHKFILL_0DB = .TRUE.
            RETURN
        END IF

        CHKFILL_0DB = ( SCALAR .NE. NF_FILL_INT1 )
        RETURN

    END FUNCTION CHKFILL_0DB


    !!.............................................................................
    !!  MPINTERP:  Interpolate MPAS-cell-variable Z to lat-lon location(s) <Y=ALAT,X=ALON>
    !!  using barycentric-linear interpolation for REAL variables, and cell-incidence for
    !!  INTEGER variables
    !!      See  https://codeplea.com/triangular-interpolation
    !!  For 2-D grids with argument A, normalize the result by the ratio of input-cell areas
    !!  to output-cell areas (as is necessary for mass-per-cell emissions)
    !!  Forms for points, sets of points, and 2-D grids of points and for
    !!  non-layered and layered variables to be interpolated.
    !!  Note that layered results follow MPAS layer-subscript-first conventions.
    !!.............................................................................
    !!  BARYFAC computes weights for barycentric-linear interpolation to (X,Y).
    !!  and returns TRUE iff the requested (X,Y) is in the triangle defined by
    !!  the input points (X1,Y1), X2,Y2), X3,Y3).
    !!.............................................................................
    !!  MPBARYMATX() computes 3-band barycentric interpolation matrix
    !!.............................................................................
    !!  MPBARYMULT() multiplies 3-band barycentric interpolation matrix by an MPAS-variable
    !!.............................................................................


    LOGICAL FUNCTION  BARYFAC( X, Y, X1, Y1, X2, Y2, X3, Y3, W1, W2, W3 )
        REAL(8), INTENT(IN   ) :: X,  Y                     !!  interpolation point
        REAL(8), INTENT(IN   ) :: X1, Y1, X2, Y2, X3, Y3    !!  triangle-vertices
        REAL(8), INTENT(  OUT) :: W1, W2, W3                !!  coefficients

        REAL(8)     DD

        DD = 1.0d0 / ( ( Y2 - Y3 )*( X1 - X3 ) + ( X3 - X2 )*( Y1 - Y3 ) )
        W1 = ( ( Y2 - Y3 )*( X  - X3 ) + ( X3 - X2 )*( Y  - Y3 ) ) * DD
        W2 = ( ( Y3 - Y1 )*( X  - X3 ) + ( X1 - X3 )*( Y  - Y3 ) ) * DD
        W3 = 1.0d0 - W1 - W2

        BARYFAC = ( W1 .GE. 0.0d0 .AND. W1 .LE. 1.0d0 .AND.    &
                    W2 .GE. 0.0d0 .AND. W2 .LE. 1.0d0 .AND.    &
                    W3 .GE. 0.0d0 .AND. W3 .LE. 1.0d0 )

        RETURN

    END  FUNCTION BARYFAC


    !!.............................................................................


    LOGICAL FUNCTION MPCELLMATX1F( NP, LAT, LON, KCELL )
        INTEGER, INTENT(IN   ) ::   NP
        REAL,    INTENT(IN   ) ::   LAT( NP )
        REAL,    INTENT(IN   ) ::   LON( NP )
        INTEGER, INTENT(  OUT) :: KCELL( NP )

        INTEGER     C, M
        LOGICAL     EFLAG

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                     &
!$OMP&              SHARED( NP, LAT, LON, KCELL ),      &
!$OMP&             PRIVATE( C, M ),                     &
!$OMP&           REDUCTION( .OR.:  EFLAG )

CLOOP:  DO C = 1, NP

            M = FINDCELL( LAT( C ), LON( C ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE
                KCELL( C ) = M
            END IF

        END DO CLOOP

        MPCELLMATX1F = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPCELLMATX1F


    !!.............................................................................


    LOGICAL FUNCTION MPCELLMATX1D( NP, LAT, LON, KCELL )
        INTEGER, INTENT(IN   ) ::   NP
        REAL(8), INTENT(IN   ) ::   LAT( NP )
        REAL(8), INTENT(IN   ) ::   LON( NP )
        INTEGER, INTENT(  OUT) :: KCELL( NP )

        INTEGER     C, M
        LOGICAL     EFLAG

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                     &
!$OMP&              SHARED( NP, LAT, LON, KCELL ),      &
!$OMP&             PRIVATE( C, M ),                     &
!$OMP&           REDUCTION( .OR.:  EFLAG )

CLOOP:  DO C = 1, NP

            M = FINDCELL( LAT( C ), LON( C ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE
                KCELL( C ) = M
            END IF

        END DO CLOOP

        MPCELLMATX1D = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPCELLMATX1D


    !!.............................................................................


    LOGICAL FUNCTION MPCELLMATX2F( NC, NR, LAT, LON, KCELL )
        INTEGER, INTENT(IN   ) ::   NC, NR
        REAL,    INTENT(IN   ) ::   LAT( NC,NR )
        REAL,    INTENT(IN   ) ::   LON( NC,NR )
        INTEGER, INTENT(  OUT) :: KCELL( NC,NR )

        INTEGER     C, R, M
        LOGICAL     EFLAG

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                     &
!$OMP&              SHARED( NC, NR, LAT, LON, KCELL ),  &
!$OMP&             PRIVATE( C, R, M ),                  &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            M = FINDCELL( LAT( C,R ), LON( C,R ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE
                KCELL( C,R ) = M
            END IF

        END DO CLOOP
        END DO

        MPCELLMATX2F = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPCELLMATX2F


    !!.............................................................................


    LOGICAL FUNCTION MPCELLMATX2D( NC, NR, LAT, LON, KCELL )
        INTEGER, INTENT(IN   ) ::   NC, NR
        REAL(8), INTENT(IN   ) ::   LAT( NC,NR )
        REAL(8), INTENT(IN   ) ::   LON( NC,NR )
        INTEGER, INTENT(  OUT) :: KCELL( NC,NR )

        INTEGER     C, R, M
        LOGICAL     EFLAG

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                     &
!$OMP&              SHARED( NC, NR, LAT, LON, KCELL ),  &
!$OMP&             PRIVATE( C, R, M ),                  &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            M = FINDCELL( LAT( C,R ), LON( C,R ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE
                KCELL( C,R ) = M
            END IF

        END DO CLOOP
        END DO

        MPCELLMATX2D = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPCELLMATX2D


    !!.............................................................................


    LOGICAL FUNCTION MPBARYMATX1F( NP, LAT, LON, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NP
        REAL,    INTENT(IN   ) :: LAT( NP )
        REAL,    INTENT(IN   ) :: LON( NP )
        INTEGER, INTENT(  OUT) :: KCELL( 3,NP )
        REAL,    INTENT(  OUT) :: WCELL( 3,NP )

        INTEGER     C, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                             &
!$OMP&              SHARED( NP, LAT, LON, NBNDYE, BNDYCELL,     &
!$OMP&                      ALONC, ALATC, KCELL, WCELL ),       &
!$OMP&             PRIVATE( C, J, K, M, XX, YY, X1, Y1,         &
!$OMP&                      X2, Y2, X3, Y3, W1, W2, W3 ),       &
!$OMP&           REDUCTION( .OR.:  EFLAG )

CLOOP:  DO C = 1, NP

            XX = MOD( LON( C)+360.0D0, 360.0D0 )
            YY = LAT( C )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        KCELL( 1,C ) = M
                        KCELL( 2,C ) = BNDYCELL( J,M )
                        KCELL( 3,C ) = BNDYCELL( K,M )
                        WCELL( 1,C ) = W1
                        WCELL( 2,C ) = W2
                        WCELL( 3,C ) = W3
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP

        MPBARYMATX1F = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYMATX1F


    !!.............................................................................


    LOGICAL FUNCTION MPBARYMATX1D( NP, LAT, LON, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NP
        REAL(8), INTENT(IN   ) :: LAT( NP )
        REAL(8), INTENT(IN   ) :: LON( NP )
        INTEGER, INTENT(  OUT) :: KCELL( 3,NP )
        REAL(8), INTENT(  OUT) :: WCELL( 3,NP )

        INTEGER     C, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                             &
!$OMP&              SHARED( NP, LAT, LON, NBNDYE, BNDYCELL,     &
!$OMP&                      ALONC, ALATC, KCELL, WCELL ),       &
!$OMP&             PRIVATE( C, J, K, M, XX, YY, X1, Y1,         &
!$OMP&                      X2, Y2, X3, Y3, W1, W2, W3 ),       &
!$OMP&           REDUCTION( .OR.:  EFLAG )

CLOOP:  DO C = 1, NP

            XX = MOD( LON( C)+360.0D0, 360.0D0 )
            YY = LAT( C )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        KCELL( 1,C ) = M
                        KCELL( 2,C ) = BNDYCELL( J,M )
                        KCELL( 3,C ) = BNDYCELL( K,M )
                        WCELL( 1,C ) = W1
                        WCELL( 2,C ) = W2
                        WCELL( 3,C ) = W3
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP

        MPBARYMATX1D = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYMATX1D


    !!.............................................................................


    LOGICAL FUNCTION MPBARYMATX1DF( NP, LAT, LON, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NP
        REAL(8), INTENT(IN   ) :: LAT( NP )
        REAL(8), INTENT(IN   ) :: LON( NP )
        INTEGER, INTENT(  OUT) :: KCELL( 3,NP )
        REAL,    INTENT(  OUT) :: WCELL( 3,NP )

        INTEGER     C, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                             &
!$OMP&              SHARED( NP, LAT, LON, NBNDYE, BNDYCELL,     &
!$OMP&                      ALONC, ALATC, KCELL, WCELL ),       &
!$OMP&             PRIVATE( C, J, K, M, XX, YY, X1, Y1,         &
!$OMP&                      X2, Y2, X3, Y3, W1, W2, W3 ),       &
!$OMP&           REDUCTION( .OR.:  EFLAG )

CLOOP:  DO C = 1, NP

            XX = MOD( LON( C)+360.0D0, 360.0D0 )
            YY = LAT( C )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        KCELL( 1,C ) = M
                        KCELL( 2,C ) = BNDYCELL( J,M )
                        KCELL( 3,C ) = BNDYCELL( K,M )
                        WCELL( 1,C ) = W1
                        WCELL( 2,C ) = W2
                        WCELL( 3,C ) = W3
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP

        MPBARYMATX1DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYMATX1DF


    !!.............................................................................


    LOGICAL FUNCTION MPBARYMATX2F( NC, NR, LAT, LON, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL,    INTENT(IN   ) :: LAT( NC,NR )
        REAL,    INTENT(IN   ) :: LON( NC,NR )
        INTEGER, INTENT(  OUT) :: KCELL( 3,NC,NR )
        REAL,    INTENT(  OUT) :: WCELL( 3,NC,NR )

        INTEGER     C, R, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                             &
!$OMP&              SHARED( NC, NR, LAT, LON, NBNDYE, BNDYCELL, &
!$OMP&                      ALONC, ALATC, KCELL, WCELL ),       &
!$OMP&             PRIVATE( C, R, J, K, M, XX, YY, X1, Y1,      &
!$OMP&                      X2, Y2, X3, Y3, W1, W2, W3 ),       &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( LON( C,R)+360.0D0, 360.0D0 )
            YY = LAT( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        KCELL( 1,C,R ) = M
                        KCELL( 2,C,R ) = BNDYCELL( J,M )
                        KCELL( 3,C,R ) = BNDYCELL( K,M )
                        WCELL( 1,C,R ) = W1
                        WCELL( 2,C,R ) = W2
                        WCELL( 3,C,R ) = W3
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPBARYMATX2F = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYMATX2F


    !!.............................................................................


    LOGICAL FUNCTION MPBARYMATX2D( NC, NR, LAT, LON, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: LAT( NC,NR )
        REAL(8), INTENT(IN   ) :: LON( NC,NR )
        INTEGER, INTENT(  OUT) :: KCELL( 3,NC,NR )
        REAL(8), INTENT(  OUT) :: WCELL( 3,NC,NR )

        INTEGER     C, R, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                             &
!$OMP&              SHARED( NC, NR, LAT, LON, NBNDYE, BNDYCELL, &
!$OMP&                      ALONC, ALATC, KCELL, WCELL ),       &
!$OMP&             PRIVATE( C, R, J, K, M, XX, YY, X1, Y1,      &
!$OMP&                      X2, Y2, X3, Y3, W1, W2, W3 ),       &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( LON( C,R)+360.0D0, 360.0D0 )
            YY = LAT( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        KCELL( 1,C,R ) = M
                        KCELL( 2,C,R ) = BNDYCELL( J,M )
                        KCELL( 3,C,R ) = BNDYCELL( K,M )
                        WCELL( 1,C,R ) = W1
                        WCELL( 2,C,R ) = W2
                        WCELL( 3,C,R ) = W3
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPBARYMATX2D = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYMATX2D


    !!.............................................................................


    LOGICAL FUNCTION MPBARYMATX2DF( NC, NR, LAT, LON, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: LAT( NC,NR )
        REAL(8), INTENT(IN   ) :: LON( NC,NR )
        INTEGER, INTENT(  OUT) :: KCELL( 3,NC,NR )
        REAL,    INTENT(  OUT) :: WCELL( 3,NC,NR )

        INTEGER     C, R, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                             &
!$OMP&              SHARED( NC, NR, LAT, LON, NBNDYE, BNDYCELL, &
!$OMP&                      ALONC, ALATC, KCELL, WCELL ),       &
!$OMP&             PRIVATE( C, R, J, K, M, XX, YY, X1, Y1,      &
!$OMP&                      X2, Y2, X3, Y3, W1, W2, W3 ),       &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( LON( C,R)+360.0D0, 360.0D0 )
            YY = LAT( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        KCELL( 1,C,R ) = M
                        KCELL( 2,C,R ) = BNDYCELL( J,M )
                        KCELL( 3,C,R ) = BNDYCELL( K,M )
                        WCELL( 1,C,R ) = W1
                        WCELL( 2,C,R ) = W2
                        WCELL( 3,C,R ) = W3
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPBARYMATX2DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYMATX2DF


    !!.............................................................................


    LOGICAL FUNCTION MPBARYEMTX2F( NC, NR, LAT, LON, A, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL,    INTENT(IN   ) :: LAT( NC,NR )
        REAL,    INTENT(IN   ) :: LON( NC,NR )
        REAL,    INTENT(IN   ) :: A
        INTEGER, INTENT(  OUT) :: KCELL( 3,NC,NR )
        REAL,    INTENT(  OUT) :: WCELL( 3,NC,NR )

        INTEGER     C, R, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3, ARAT

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                                     &
!$OMP&              SHARED( NC, NR, LAT, LON, A, NBNDYE, BNDYCELL,      &
!$OMP&                      CAREAS, ALONC, ALATC, KCELL, WCELL ),       &
!$OMP&             PRIVATE( C, R, J, K, M, XX, YY, X1, Y1,              &
!$OMP&                      X2, Y2, X3, Y3, ARAT, W1, W2, W3 ),         &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( LON( C,R)+360.0D0, 360.0D0 )
            YY = LAT( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A / CAREAS( M )
                        KCELL( 1,C,R ) = M
                        KCELL( 2,C,R ) = BNDYCELL( J,M )
                        KCELL( 3,C,R ) = BNDYCELL( K,M )
                        WCELL( 1,C,R ) = W1 * ARAT
                        WCELL( 2,C,R ) = W2 * ARAT
                        WCELL( 3,C,R ) = W3 * ARAT
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPBARYEMTX2F = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYEMTX2F


    !!.............................................................................


    LOGICAL FUNCTION MPBARYEMTX2D( NC, NR, LAT, LON, A, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: LAT( NC,NR )
        REAL(8), INTENT(IN   ) :: LON( NC,NR )
        REAL(8), INTENT(IN   ) :: A
        INTEGER, INTENT(  OUT) :: KCELL( 3,NC,NR )
        REAL(8), INTENT(  OUT) :: WCELL( 3,NC,NR )

        INTEGER     C, R, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     ARAT, W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                                     &
!$OMP&              SHARED( NC, NR, LAT, LON, A, NBNDYE, BNDYCELL,      &
!$OMP&                      CAREAS, ALONC, ALATC, KCELL, WCELL ),       &
!$OMP&             PRIVATE( C, R, J, K, M, XX, YY, X1, Y1,              &
!$OMP&                      X2, Y2, X3, Y3, ARAT, W1, W2, W3 ),         &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( LON( C,R)+360.0D0, 360.0D0 )
            YY = LAT( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A / CAREAS( M )
                        KCELL( 1,C,R ) = M
                        KCELL( 2,C,R ) = BNDYCELL( J,M )
                        KCELL( 3,C,R ) = BNDYCELL( K,M )
                        WCELL( 1,C,R ) = W1 * ARAT
                        WCELL( 2,C,R ) = W2 * ARAT
                        WCELL( 3,C,R ) = W3 * ARAT
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPBARYEMTX2D = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYEMTX2D


    !!.............................................................................


    LOGICAL FUNCTION MPBARYEMTX2DF( NC, NR, LAT, LON, A, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: LAT( NC,NR )
        REAL(8), INTENT(IN   ) :: LON( NC,NR )
        REAL(8), INTENT(IN   ) :: A
        INTEGER, INTENT(  OUT) :: KCELL( 3,NC,NR )
        REAL,    INTENT(  OUT) :: WCELL( 3,NC,NR )

        INTEGER     C, R, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     ARAT, W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                                 &
!$OMP&              SHARED( NC, NR, LAT, LON, A, NBNDYE, BNDYCELL,  &
!$OMP&                      CAREAS, ALONC, ALATC, KCELL, WCELL ),   &
!$OMP&             PRIVATE( C, R, J, K, M, XX, YY, X1, Y1,          &
!$OMP&                      X2, Y2, X3, Y3, ARAT, W1, W2, W3 ),     &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( LON( C,R)+360.0D0, 360.0D0 )
            YY = LAT( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A / CAREAS( M )
                        KCELL( 1,C,R ) = M
                        KCELL( 2,C,R ) = BNDYCELL( J,M )
                        KCELL( 3,C,R ) = BNDYCELL( K,M )
                        WCELL( 1,C,R ) = W1 * ARAT
                        WCELL( 2,C,R ) = W2 * ARAT
                        WCELL( 3,C,R ) = W3 * ARAT
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPBARYEMTX2DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYEMTX2DF


    !!.............................................................................


    LOGICAL FUNCTION MPBARYGMTX2F( NC, NR, LAT, LON, A, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL,    INTENT(IN   ) :: LAT( NC,NR )
        REAL,    INTENT(IN   ) :: LON( NC,NR )
        REAL,    INTENT(IN   ) ::   A( NC,NR )
        INTEGER, INTENT(  OUT) :: KCELL( 3,NC,NR )
        REAL,    INTENT(  OUT) :: WCELL( 3,NC,NR )

        INTEGER     C, R, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     ARAT, W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                                     &
!$OMP&              SHARED( NC, NR, LAT, LON, A, NBNDYE, BNDYCELL,      &
!$OMP&                      CAREAS, ALONC, ALATC, KCELL, WCELL ),       &
!$OMP&             PRIVATE( C, R, J, K, M, XX, YY, X1, Y1,              &
!$OMP&                      X2, Y2, X3, Y3, ARAT, W1, W2, W3 ),         &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( LON( C,R)+360.0D0, 360.0D0 )
            YY = LAT( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A( C,R ) / CAREAS( M )
                        KCELL( 1,C,R ) = M
                        KCELL( 2,C,R ) = BNDYCELL( J,M )
                        KCELL( 3,C,R ) = BNDYCELL( K,M )
                        WCELL( 1,C,R ) = W1 * ARAT
                        WCELL( 2,C,R ) = W2 * ARAT
                        WCELL( 3,C,R ) = W3 * ARAT
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPBARYGMTX2F = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYGMTX2F


    !!.............................................................................


    LOGICAL FUNCTION MPBARYGMTX2D( NC, NR, LAT, LON, A, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: LAT( NC,NR )
        REAL(8), INTENT(IN   ) :: LON( NC,NR )
        REAL(8), INTENT(IN   ) ::   A( NC,NR )
        INTEGER, INTENT(  OUT) :: KCELL( 3,NC,NR )
        REAL(8), INTENT(  OUT) :: WCELL( 3,NC,NR )

        INTEGER     C, R, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     ARAT, W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                                 &
!$OMP&              SHARED( NC, NR, LAT, LON, A, NBNDYE, BNDYCELL,  &
!$OMP&                      CAREAS, ALONC, ALATC, KCELL, WCELL ),   &
!$OMP&             PRIVATE( C, R, J, K, M, XX, YY, X1, Y1,          &
!$OMP&                      X2, Y2, X3, Y3, ARAT, W1, W2, W3 ),     &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( LON( C,R)+360.0D0, 360.0D0 )
            YY = LAT( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A( C,R ) / CAREAS( M )
                        KCELL( 1,C,R ) = M
                        KCELL( 2,C,R ) = BNDYCELL( J,M )
                        KCELL( 3,C,R ) = BNDYCELL( K,M )
                        WCELL( 1,C,R ) = W1 * ARAT
                        WCELL( 2,C,R ) = W2 * ARAT
                        WCELL( 3,C,R ) = W3 * ARAT
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPBARYGMTX2D = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYGMTX2D


    !!.............................................................................


    LOGICAL FUNCTION MPBARYGMTX2DF( NC, NR, LAT, LON, A, KCELL, WCELL )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: LAT( NC,NR )
        REAL(8), INTENT(IN   ) :: LON( NC,NR )
        REAL(8), INTENT(IN   ) ::   A( NC,NR )
        INTEGER, INTENT(  OUT) :: KCELL( 3,NC,NR )
        REAL,    INTENT(  OUT) :: WCELL( 3,NC,NR )

        INTEGER     C, R, J, K, L, M
        LOGICAL     EFLAG
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     ARAT, W1, W2, W3

        EFLAG = .FALSE.

!$OMP  PARALLEL DO DEFAULT( NONE ),                                 &
!$OMP&              SHARED( NC, NR, LAT, LON, A, NBNDYE, BNDYCELL,  &
!$OMP&                      CAREAS, ALONC, ALATC, KCELL, WCELL ),   &
!$OMP&             PRIVATE( C, R, J, K, M, XX, YY, X1, Y1,          &
!$OMP&                      X2, Y2, X3, Y3, ARAT, W1, W2, W3 ),     &
!$OMP&           REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( LON( C,R)+360.0D0, 360.0D0 )
            YY = LAT( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A( C,R ) / CAREAS( M )
                        KCELL( 1,C,R ) = M
                        KCELL( 2,C,R ) = BNDYCELL( J,M )
                        KCELL( 3,C,R ) = BNDYCELL( K,M )
                        WCELL( 1,C,R ) = W1 * ARAT
                        WCELL( 2,C,R ) = W2 * ARAT
                        WCELL( 3,C,R ) = W3 * ARAT
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPBARYGMTX2DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPBARYGMTX2DF


    !!.............................................................................


    SUBROUTINE MPBARYMULT1F1( NP, KCELL, WCELL, Z, V )
        INTEGER, INTENT(IN   ) :: NP
        INTEGER, INTENT(IN   ) :: KCELL( 3,NP )
        REAL,    INTENT(IN   ) :: WCELL( 3,NP )
        REAL,    INTENT(IN   ) :: Z( MPCELLS )      !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NP )        !!  result

        INTEGER C, K1, K2, K3
        REAL    W1, W2, W3

!$OMP   PARALLEL DO DEFAULT( NONE ),                        &
!$OMP&               SHARED( NP, KCELL, WCELL, V, Z ),      &
!$OMP&              PRIVATE( C, K1, K2, K3, W1, W2, W3 )

        DO C = 1, NP
            K1 = KCELL( 1,C )
            K2 = KCELL( 2,C )
            K3 = KCELL( 3,C )
            W1 = WCELL( 1,C )
            W2 = WCELL( 2,C )
            W3 = WCELL( 3,C )
            V( C ) = W1 * Z( K1 ) + W2 * Z( K2 ) + W3 * Z( K3 )
        END DO

    END SUBROUTINE MPBARYMULT1F1


    !!.............................................................................


    SUBROUTINE MPBARYMULT1FL( NP, NL, KCELL, WCELL, Z, V )
        INTEGER, INTENT(IN   ) :: NP, NL
        INTEGER, INTENT(IN   ) :: KCELL( 3,NP )
        REAL,    INTENT(IN   ) :: WCELL( 3,NP )
        REAL,    INTENT(IN   ) :: Z( NL,MPCELLS )      !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NP,NL )        !!  result

        INTEGER C, L, K1, K2, K3
        REAL    W1, W2, W3

!$OMP   PARALLEL DO DEFAULT( NONE ),                        &
!$OMP&               SHARED( NP, NL, KCELL, WCELL, V, Z ),  &
!$OMP&              PRIVATE( C, L, K1, K2, K3, W1, W2, W3 )

        DO C = 1, NP
            K1 = KCELL( 1,C )
            K2 = KCELL( 2,C )
            K3 = KCELL( 3,C )
            W1 = WCELL( 1,C )
            W2 = WCELL( 2,C )
            W3 = WCELL( 3,C )
            DO L = 1, NL
                V( L,C ) = W1 * Z( L,K1 ) + W2 * Z( L,K2 ) + W3 * Z( L,K3 )
            END DO
        END DO

    END SUBROUTINE MPBARYMULT1FL


    !!.............................................................................


    SUBROUTINE MPBARYMULT1D1( NP, KCELL, WCELL, Z, V )
        INTEGER, INTENT(IN   ) :: NP
        INTEGER, INTENT(IN   ) :: KCELL( 3,NP )
        REAL(8), INTENT(IN   ) :: WCELL( 3,NP )
        REAL(8), INTENT(IN   ) :: Z( MPCELLS )      !!  variable to be interpolated
        REAL(8), INTENT(  OUT) :: V( NP )           !!  result

        INTEGER C, K1, K2, K3
        REAL    W1, W2, W3

!$OMP   PARALLEL DO DEFAULT( NONE ),                        &
!$OMP&               SHARED( NP, KCELL, WCELL, V, Z ),      &
!$OMP&              PRIVATE( C, K1, K2, K3, W1, W2, W3 )

        DO C = 1, NP
            K1 = KCELL( 1,C )
            K2 = KCELL( 2,C )
            K3 = KCELL( 3,C )
            W1 = WCELL( 1,C )
            W2 = WCELL( 2,C )
            W3 = WCELL( 3,C )
            V( C ) = W1 * Z( K1 ) + W2 * Z( K2 ) + W3 * Z( K3 )
        END DO

    END SUBROUTINE MPBARYMULT1D1


    !!.............................................................................


    SUBROUTINE MPBARYMULT1DL( NP, NL, KCELL, WCELL, Z, V )
        INTEGER, INTENT(IN   ) :: NP, NL
        INTEGER, INTENT(IN   ) :: KCELL( 3,NP )
        REAL(8), INTENT(IN   ) :: WCELL( 3,NP )
        REAL(8), INTENT(IN   ) :: Z( NL,MPCELLS )       !!  variable to be interpolated
        REAL(8), INTENT(  OUT) :: V( NP,NL )            !!  result

        INTEGER C, L, K1, K2, K3
        REAL    W1, W2, W3

!$OMP   PARALLEL DO DEFAULT( NONE ),                            &
!$OMP&               SHARED( NP, NL, KCELL, WCELL, V, Z ),      &
!$OMP&              PRIVATE( C, L, K1, K2, K3, W1, W2, W3 )

        DO C = 1, NP
            K1 = KCELL( 1,C )
            K2 = KCELL( 2,C )
            K3 = KCELL( 3,C )
            W1 = WCELL( 1,C )
            W2 = WCELL( 2,C )
            W3 = WCELL( 3,C )
            DO L = 1, NL
                V( L,C ) = W1 * Z( L,K1 ) + W2 * Z( L,K2 ) + W3 * Z( L,K3 )
            END DO
        END DO

    END SUBROUTINE MPBARYMULT1DL


    !!.............................................................................


    SUBROUTINE MPBARYMULT2F1( NC, NR, KCELL, WCELL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        INTEGER, INTENT(IN   ) :: KCELL( 3,NC,NR )
        REAL(8), INTENT(IN   ) :: WCELL( 3,NC,NR )
        REAL,    INTENT(IN   ) :: Z( MPCELLS )      !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NC,NR )        !!  result

        INTEGER C, R, K1, K2, K3
        REAL    W1, W2, W3

!$OMP   PARALLEL DO DEFAULT( NONE ),                            &
!$OMP&               SHARED( NC, NR, KCELL, WCELL, V, Z ),      &
!$OMP&              PRIVATE( C, R, K1, K2, K3, W1, W2, W3 )

        DO R = 1, NR
        DO C = 1, NC
            K1 = KCELL( 1,C,R )
            K2 = KCELL( 2,C,R )
            K3 = KCELL( 3,C,R )
            W1 = WCELL( 1,C,R )
            W2 = WCELL( 2,C,R )
            W3 = WCELL( 3,C,R )
            V( C,R ) = W1 * Z( K1 ) + W2 * Z( K2 ) + W3 * Z( K3 )
        END DO
        END DO

    END SUBROUTINE MPBARYMULT2F1


    !!.............................................................................


    SUBROUTINE MPBARYMULT2FL( NC, NR, NL, KCELL, WCELL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        INTEGER, INTENT(IN   ) :: KCELL( 3,NC,NR )
        REAL(8), INTENT(IN   ) :: WCELL( 3,NC,NR )
        REAL,    INTENT(IN   ) :: Z( NL,MPCELLS )      !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NC,NR,NL )        !!  result

        INTEGER C, R, L, K1, K2, K3
        REAL    W1, W2, W3

!$OMP   PARALLEL DO DEFAULT( NONE ),                            &
!$OMP&               SHARED( NC, NR, NL, KCELL, WCELL, V, Z ),  &
!$OMP&              PRIVATE( C, R, L, K1, K2, K3, W1, W2, W3 )

        DO R = 1, NR
        DO C = 1, NC
            K1 = KCELL( 1,C,R )
            K2 = KCELL( 2,C,R )
            K3 = KCELL( 3,C,R )
            W1 = WCELL( 1,C,R )
            W2 = WCELL( 2,C,R )
            W3 = WCELL( 3,C,R )
            DO L = 1, NL
                V( L,C,R ) = W1 * Z( L,K1 ) + W2 * Z( L,K2 ) + W3 * Z( L,K3 )
            END DO
        END DO
        END DO

    END SUBROUTINE MPBARYMULT2FL


    !!.............................................................................


    SUBROUTINE MPBARYMULT2D1( NC, NR, KCELL, WCELL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        INTEGER, INTENT(IN   ) :: KCELL( 3,NC,NR )
        REAL(8), INTENT(IN   ) :: WCELL( 3,NC,NR )
        REAL(8), INTENT(IN   ) :: Z( MPCELLS )      !!  variable to be interpolated
        REAL(8), INTENT(  OUT) :: V( NC,NR )        !!  result

        INTEGER C, R, K1, K2, K3
        REAL    W1, W2, W3

!$OMP   PARALLEL DO DEFAULT( NONE ),                            &
!$OMP&               SHARED( NC, NR, KCELL, WCELL, V, Z ),      &
!$OMP&              PRIVATE( C, R, K1, K2, K3, W1, W2, W3 )

        DO R = 1, NR
        DO C = 1, NC
            K1 = KCELL( 1,C,R )
            K2 = KCELL( 2,C,R )
            K3 = KCELL( 3,C,R )
            W1 = WCELL( 1,C,R )
            W2 = WCELL( 2,C,R )
            W3 = WCELL( 3,C,R )
            V( C,R ) = W1 * Z( K1 ) + W2 * Z( K2 ) + W3 * Z( K3 )
        END DO
        END DO

    END SUBROUTINE MPBARYMULT2D1


    !!.............................................................................


    SUBROUTINE MPBARYMULT2DL( NC, NR, NL, KCELL, WCELL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        INTEGER, INTENT(IN   ) :: KCELL( 3,NC,NR )
        REAL(8), INTENT(IN   ) :: WCELL( 3,NC,NR )
        REAL(8), INTENT(IN   ) :: Z( NL,MPCELLS )      !!  variable to be interpolated
        REAL(8), INTENT(  OUT) :: V( NC,NR,NL )        !!  result

        INTEGER C, R, L, K1, K2, K3
        REAL    W1, W2, W3

!$OMP   PARALLEL DO DEFAULT( NONE ),                            &
!$OMP&               SHARED( NC, NR, NL, KCELL, WCELL, V, Z ),  &
!$OMP&              PRIVATE( C, R, L, K1, K2, K3, W1, W2, W3 )

        DO R = 1, NR
        DO C = 1, NC
            K1 = KCELL( 1,C,R )
            K2 = KCELL( 2,C,R )
            K3 = KCELL( 3,C,R )
            W1 = WCELL( 1,C,R )
            W2 = WCELL( 2,C,R )
            W3 = WCELL( 3,C,R )
            DO L = 1, NL
                V( L,C,R ) = W1 * Z( L,K1 ) + W2 * Z( L,K2 ) + W3 * Z( L,K3 )
            END DO
        END DO
        END DO

    END SUBROUTINE MPBARYMULT2DL


    !!.............................................................................


    LOGICAL FUNCTION MPINTERP0DI( Y, X, Z, V )
        REAL(8), INTENT(IN   ) :: Y, X          !!  latitude, longitude (degrees)
        INTEGER, INTENT(IN   ) :: Z( MPCELLS )
        INTEGER, INTENT(  OUT) :: V

        INTEGER     J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        M = FINDCELL( Y, X )
        IF ( M .LT. 0 ) THEN
            MPINTERP0DI = .FALSE.
        ELSE
            V = Z( M )
            MPINTERP0DI = .TRUE.
        END IF

        RETURN

    END FUNCTION MPINTERP0DI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERP1DI( NPTS, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NPTS
        REAL(8), INTENT(IN   ) :: Y( NPTS )             !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NPTS )             !!  longitude, degrees
        INTEGER, INTENT(IN   ) :: Z( MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NPTS )

        INTEGER     I, M
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),               &
!$OMP&                SHARED( NPTS, Y, X, Z, V ),   &
!$OMP&               PRIVATE( I, M  ),              &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO I = 1, NPTS

            M = FINDCELL( Y( I ), X( I ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
            ELSE
                V( I ) = Z( M )
            END IF

        END DO

        MPINTERP1DI = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERP1DI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERP2DI( NC, NR, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: Y( NC,NR )             !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NC,NR )             !!  longitude, degrees
        INTEGER, INTENT(IN   ) :: Z( MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NC,NR )

        INTEGER     C, R, M
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),               &
!$OMP&                SHARED( NC, NR, Y, X, Z, V ), &
!$OMP&               PRIVATE( C, R, M  ),           &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
        DO C = 1, NC

            M = FINDCELL( Y( C,R ), X( C,R ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
            ELSE
                V( C,R ) = Z( M )
            END IF

        END DO
        END DO

        MPINTERP2DI = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERP2DI

    !!.............................................................................


    LOGICAL FUNCTION MPINTERP0RI( Y, X, Z, V )
        REAL   , INTENT(IN   ) :: Y, X          !!  latitude, longitude (degrees)
        INTEGER, INTENT(IN   ) :: Z( MPCELLS )
        INTEGER, INTENT(  OUT) :: V

        INTEGER     M

        M = FINDCELL( Y, X )
        IF ( M .LT. 0 ) THEN
            MPINTERP0RI = .FALSE.
        ELSE
            V = Z( M )
            MPINTERP0RI = .TRUE.
        END IF
        RETURN

    END FUNCTION MPINTERP0RI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERP1RI( NPTS, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NPTS
        REAL   , INTENT(IN   ) :: Y( NPTS )             !!  latitude,  degrees
        REAL   , INTENT(IN   ) :: X( NPTS )             !!  longitude, degrees
        INTEGER, INTENT(IN   ) :: Z( MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NPTS )

        INTEGER     I, M
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),               &
!$OMP&                SHARED( NPTS, Y, X, Z, V ),   &
!$OMP&               PRIVATE( I, M  ),              &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO I = 1, NPTS

            M = FINDCELL( Y( I ), X( I ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
            ELSE
                V( I ) = Z( M )
            END IF

        END DO

        MPINTERP1RI = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERP1RI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERP2RI( NC, NR, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL   , INTENT(IN   ) :: Y( NC,NR )             !!  latitude,  degrees
        REAL   , INTENT(IN   ) :: X( NC,NR )             !!  longitude, degrees
        INTEGER, INTENT(IN   ) :: Z( MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NC,NR )

        INTEGER     C, R, M
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),               &
!$OMP&                SHARED( NC, NR, Y, X, Z, V ), &
!$OMP&               PRIVATE( C, R, M  ),           &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
        DO C = 1, NC

            M = FINDCELL( Y( C,R ), X( C,R ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
            ELSE
                V( C,R ) = Z( M )
            END IF

        END DO
        END DO

        MPINTERP2RI = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERP2RI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERPL0DI( Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NL
        REAL(8), INTENT(IN   ) :: Y, X          !!  latitude, longitude (degrees)
        INTEGER, INTENT(IN   ) :: Z( NL,MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NL )

        INTEGER     L, M

        M = FINDCELL( Y, X )
        IF ( M .LT. 0 ) THEN
            MPINTERPL0DI = .FALSE.
        ELSE
            DO L = 1, NL
                V(L) = Z( L,M )
            END DO
            MPINTERPL0DI = .TRUE.
        END IF

        RETURN

    END FUNCTION MPINTERPL0DI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERPL1DI( NPTS, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NPTS, NL
        REAL(8), INTENT(IN   ) :: Y( NPTS )         !!  latitude, longitude (degrees)
        REAL(8), INTENT(IN   ) :: X( NPTS )
        INTEGER, INTENT(IN   ) :: Z( NL,MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NL,NPTS )

        INTEGER     I, L, M
        LOGICAL     EFLAG

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),                   &
!$OMP&                SHARED( NPTS, NL, Y, X, Z, V ),   &
!$OMP&               PRIVATE( I, M ),                   &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO I = 1, NPTS

            M = FINDCELL( Y( I ), X( I ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
            ELSE
                DO L = 1, NL
                    V(L,I) = Z( L,M )
                END DO
            END IF

        END DO

        MPINTERPL1DI = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPL1DI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERPL2DI( NC, NR, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL(8), INTENT(IN   ) :: Y( NC,NR )         !!  latitude, longitude (degrees)
        REAL(8), INTENT(IN   ) :: X( NC,NR )
        INTEGER, INTENT(IN   ) :: Z( NL,MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NL,NC,NR )

        INTEGER     C, R, L, M
        LOGICAL     EFLAG

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),                       &
!$OMP&                SHARED( NC, NR, NL, Y, X, Z, V ),     &
!$OMP&               PRIVATE( C, R, M ),                    &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
        DO C = 1, NC

            M = FINDCELL( Y( C,R ), X( C,R ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
            ELSE
                DO L = 1, NL
                    V(L,C,R) = Z( L,M )
                END DO
            END IF

        END DO
        END DO

        MPINTERPL2DI = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPL2DI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERPL0RI( Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NL
        REAL   , INTENT(IN   ) :: Y, X          !!  latitude, longitude (degrees)
        INTEGER, INTENT(IN   ) :: Z( NL,MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NL )

        INTEGER     L, M

        M = FINDCELL( Y, X )
        IF ( M .LT. 0 ) THEN
            MPINTERPL0RI = .FALSE.
        ELSE
            DO L = 1, NL
                V(L) = Z( L,M )
            END DO
            MPINTERPL0RI = .TRUE.
        END IF
        RETURN

    END FUNCTION MPINTERPL0RI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERPL1RI( NPTS, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NPTS, NL
        REAL,    INTENT(IN   ) :: Y( NPTS )         !!  latitude, longitude (degrees)
        REAL,    INTENT(IN   ) :: X( NPTS )
        INTEGER, INTENT(IN   ) :: Z( NL,MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NL,NPTS )

        INTEGER     I, L, M
        LOGICAL     EFLAG

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),                   &
!$OMP&                SHARED( NPTS, NL, Y, X, Z, V ),   &
!$OMP&               PRIVATE( I, M ),                   &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO I = 1, NPTS

            M = FINDCELL( Y( I ), X( I ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
            ELSE
                DO L = 1, NL
                    V(L,I) = Z( L,M )
                END DO
            END IF

        END DO

        MPINTERPL1RI = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPL1RI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERPL2RI( NC, NR, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL,    INTENT(IN   ) :: Y( NC,NR )         !!  latitude, longitude (degrees)
        REAL,    INTENT(IN   ) :: X( NC,NR )
        INTEGER, INTENT(IN   ) :: Z( NL,MPCELLS )
        INTEGER, INTENT(  OUT) :: V( NL,NC,NR )

        INTEGER     C, R, L, M
        LOGICAL     EFLAG

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),                       &
!$OMP&                SHARED( NC, NR, NL, Y, X, Z, V ),     &
!$OMP&               PRIVATE( C, R, M ),                    &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
        DO C = 1, NC

            M = FINDCELL( Y( C,R ), X( C,R ) )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
            ELSE
                DO L = 1, NL
                    V(L,C,R) = Z( L,M )
                END DO
            END IF

        END DO
        END DO

        MPINTERPL2RI = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPL2RI


    !!.............................................................................


    LOGICAL FUNCTION MPINTERP0DF( Y, X, Z, V )
        REAL,    INTENT(IN   ) :: Y, X              !!  latitude, longitude (degrees)
        REAL,    INTENT(IN   ) :: Z( MPCELLS )      !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V                 !!  result

        INTEGER     J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        M = FINDCELL( Y, X )
        IF ( M .LT. 0 ) THEN
            MPINTERP0DF = .FALSE.
            RETURN
        ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
            MPINTERP0DF = .FALSE.
            RETURN
        END IF

        !!........  Find closest cells K1, K2 (in that order)

        XX = MOD( X+360.0D0, 360.0D0 )
        YY = Y
        X1 = ALONC( M )
        Y1 = ALATC( M )

        DO J = 1,  NBNDYE( M )
            K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
            X2 = ALONC( BNDYCELL( J,M ) )
            Y2 = ALATC( BNDYCELL( J,M ) )
            X3 = ALONC( BNDYCELL( K,M ) )
            Y3 = ALATC( BNDYCELL( K,M ) )
            IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                    V = W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K )
                    MPINTERP0DF = .TRUE.
                    RETURN

            END IF

        END DO      !! end loop on boundary-cells for this cell

        MPINTERP0DF = .FALSE.
        RETURN

    END FUNCTION MPINTERP0DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERP0DD( Y, X, Z, V )
        REAL(8), INTENT(IN   ) :: Y, X          !!  latitude, longitude (degrees)
        REAL,    INTENT(IN   ) :: Z( MPCELLS )
        REAL,    INTENT(  OUT) :: V

        INTEGER     J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        M = FINDCELL( Y, X )
        IF ( M .LT. 0 ) THEN
            MPINTERP0DD = .FALSE.
            RETURN
        ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
            MPINTERP0DD = .FALSE.
            RETURN
        END IF

        !!........  Find closest cells K1, K2 (in that order)

        XX = MOD( X+360.0D0, 360.0D0 )
        YY = Y
        X1 = ALONC( M )
        Y1 = ALATC( M )

        DO J = 1,  NBNDYE( M )
            K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
            X2 = ALONC( BNDYCELL( J,M ) )
            Y2 = ALATC( BNDYCELL( J,M ) )
            X3 = ALONC( BNDYCELL( K,M ) )
            Y3 = ALATC( BNDYCELL( K,M ) )
            IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                    V = W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K )
                    MPINTERP0DD = .TRUE.
                    RETURN

            END IF

        END DO      !! end loop on boundary-cells for this cell

        MPINTERP0DD = .FALSE.
        RETURN

    END FUNCTION MPINTERP0DD

    !!.............................................................................

    LOGICAL FUNCTION MPINTERP1DF( NPTS, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NPTS
        REAL,    INTENT(IN   ) :: Y( NPTS )             !!  latitude,  degrees
        REAL,    INTENT(IN   ) :: X( NPTS )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( MPCELLS )          !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NPTS )

        INTEGER     I, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),                                               &
!$OMP&                SHARED( NPTS, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL ),   &
!$OMP&               PRIVATE( I, J, K, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,           &
!$OMP&                        W1, W2, W3 ),                                         &
!$OMP&             REDUCTION( .OR.:  EFLAG )

ILOOP:  DO I = 1, NPTS

            XX = MOD( X( I ) + 360.0D0, 360.0D0 )
            YY = Y( I )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        V( I ) = W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K )
                        CYCLE ILOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO ILOOP

        MPINTERP1DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERP1DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERP1DD( NPTS, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NPTS
        REAL(8), INTENT(IN   ) :: Y( NPTS )             !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NPTS )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( MPCELLS )
        REAL,    INTENT(  OUT) :: V( NPTS )

        INTEGER     I, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),                                               &
!$OMP&                SHARED( NPTS, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL ),   &
!$OMP&               PRIVATE( I, J, K, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,           &
!$OMP&                        W1, W2, W3 ),                                         &
!$OMP&             REDUCTION( .OR.:  EFLAG )

ILOOP:  DO I = 1, NPTS

            XX = MOD( X( I ) + 360.0D0, 360.0D0 )
            YY = Y( I )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        V( I ) = W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K )
                        CYCLE ILOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO ILOOP

        MPINTERP1DD = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERP1DD

    !!.............................................................................

    LOGICAL FUNCTION MPINTERP2DF( NC, NR, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL,    INTENT(IN   ) :: Y( NC, NR )             !!  latitude,  degrees
        REAL,    INTENT(IN   ) :: X( NC, NR )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( MPCELLS )            !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),                                               &
!$OMP&                SHARED( NC, NR, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL ), &
!$OMP&               PRIVATE( C, R, J, K, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,        &
!$OMP&                        W1, W2, W3 ),                                         &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        V( C,R ) = W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K )
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERP2DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERP2DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERP2DD( NC, NR, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: Y( NC, NR )             !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NC, NR )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( MPCELLS )
        REAL,    INTENT(  OUT) :: V( NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO DEFAULT( NONE ),                                               &
!$OMP&                SHARED( NC, NR, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL ), &
!$OMP&               PRIVATE( C, R, J, K, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,        &
!$OMP&                        W1, W2, W3 ),                                         &
!$OMP&             REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        V( C,R ) = W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K )
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERP2DD = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERP2DD

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPE2DF( NC, NR, A, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL,    INTENT(IN   ) :: A                     !!  area of output-grid cells
        REAL,    INTENT(IN   ) :: Y( NC, NR )           !!  latitude,  degrees
        REAL,    INTENT(IN   ) :: X( NC, NR )           !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( MPCELLS )          !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                                    &
!$OMP&       DEFAULT( NONE ),                                                           &
!$OMP&        SHARED( NC, NR, A, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL, CAREAS ),  &
!$OMP&       PRIVATE( C, R, J, K, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,                    &
!$OMP&                W1, W2, W3 ),                                                     &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        V(C,R) = ( W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K ) ) * A / CAREAS( M )
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPE2DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPE2DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPE2DD( NC, NR, A, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: A                     !!  area of output-grid cells
        REAL(8), INTENT(IN   ) :: Y( NC, NR )           !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NC, NR )           !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( MPCELLS )
        REAL,    INTENT(  OUT) :: V( NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                                    &
!$OMP&       DEFAULT( NONE ),                                                           &
!$OMP&        SHARED( NC, NR, A, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL, CAREAS ),  &
!$OMP&       PRIVATE( C, R, J, K, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,                    &
!$OMP&                W1, W2, W3 ),                                                     &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        V(C,R) = ( W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K ) ) * A / CAREAS( M )
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPE2DD = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPE2DD

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPG2DF( NC, NR, A, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL,    INTENT(IN   ) :: A( NC, NR )           !!  area of output-grid cells
        REAL,    INTENT(IN   ) :: Y( NC, NR )           !!  latitude,  degrees
        REAL,    INTENT(IN   ) :: X( NC, NR )           !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( MPCELLS )          !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                                    &
!$OMP&       DEFAULT( NONE ),                                                           &
!$OMP&        SHARED( NC, NR, A, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL, CAREAS ),  &
!$OMP&       PRIVATE( C, R, J, K, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,                    &
!$OMP&                W1, W2, W3 ),                                                     &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        V(C,R) = ( W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K ) ) * A( C,R ) / CAREAS( M  )
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPG2DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPG2DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPG2DD( NC, NR, A, Y, X, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR
        REAL(8), INTENT(IN   ) :: A( NC, NR )           !!  area of output-grid cells
        REAL(8), INTENT(IN   ) :: Y( NC, NR )           !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NC, NR )           !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( MPCELLS )
        REAL,    INTENT(  OUT) :: V( NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                                    &
!$OMP&       DEFAULT( NONE ),                                                           &
!$OMP&        SHARED( NC, NR, A, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL, CAREAS ),  &
!$OMP&       PRIVATE( C, R, J, K, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,                    &
!$OMP&                W1, W2, W3 ),                                                     &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        V(C,R) = ( W1 * Z( M ) + W2 * Z( J ) + W3 * Z( K ) ) * A( C,R ) / CAREAS( M  )
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPG2DD = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPG2DD


    !!.............................................................................


    LOGICAL FUNCTION MPINTERPL0DF( Y, X, NL, Z, V )
        REAL,    INTENT(IN   ) :: Y, X              !!  latitude, longitude (degrees)
        INTEGER, INTENT(IN   ) :: NL                !!  number of layers
        REAL,    INTENT(IN   ) :: Z( NL,MPCELLS )   !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NL )           !!  result

        INTEGER     J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        M = FINDCELL( Y, X )
        IF ( M .LT. 0 ) THEN
            MPINTERPL0DF = .FALSE.
            RETURN
        ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
            MPINTERPL0DF = .FALSE.
            RETURN
        END IF

        !!........  Find closest cells K1, K2 (in that order)

        XX = MOD( X + 360.0D0, 360.0D0 )
        YY = Y
        X1 = ALONC( M )
        Y1 = ALATC( M )

        DO J = 1,  NBNDYE( M )
            K = 1 + MOD( K, NBNDYE( M ) )
            X2 = ALONC( BNDYCELL( J,M ) )
            Y2 = ALATC( BNDYCELL( J,M ) )
            X3 = ALONC( BNDYCELL( K,M ) )
            Y3 = ALATC( BNDYCELL( K,M ) )
            IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                    DO L = 1, NL
                        V(L) = W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K )
                    END DO
                    MPINTERPL0DF = .TRUE.
                    RETURN

            END IF

        END DO      !! end loop on boundary-cells for this cell

        MPINTERPL0DF = .FALSE.
        RETURN

    END FUNCTION MPINTERPL0DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPL0DD( Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NL
        REAL(8), INTENT(IN   ) :: Y, X          !!  latitude, longitude (degrees)
        REAL,    INTENT(IN   ) :: Z( NL,MPCELLS )
        REAL,    INTENT(  OUT) :: V( NL )

        INTEGER     J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3

        M = FINDCELL( Y, X )
        IF ( M .LT. 0 ) THEN
            MPINTERPL0DD = .FALSE.
            RETURN
        ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
            MPINTERPL0DD = .FALSE.
            RETURN
        END IF

        !!........  Find closest cells K1, K2 (in that order)

        XX = MOD( X + 360.0D0, 360.0D0 )
        YY = Y
        X1 = ALONC( M )
        Y1 = ALATC( M )

        DO J = 1,  NBNDYE( M )
            K = 1 + MOD( K, NBNDYE( M ) )
            X2 = ALONC( BNDYCELL( J,M ) )
            Y2 = ALATC( BNDYCELL( J,M ) )
            X3 = ALONC( BNDYCELL( K,M ) )
            Y3 = ALATC( BNDYCELL( K,M ) )
            IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                    DO L = 1, NL
                        V(L) = W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K )
                    END DO
                    MPINTERPL0DD = .TRUE.
                    RETURN

            END IF

        END DO      !! end loop on boundary-cells for this cell

        MPINTERPL0DD = .FALSE.
        RETURN

    END FUNCTION MPINTERPL0DD

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPL1DF( NPTS, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NPTS, NL
        REAL,    INTENT(IN   ) :: Y( NPTS )             !!  latitude,  degrees
        REAL,    INTENT(IN   ) :: X( NPTS )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( NL,MPCELLS )       !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NL,NPTS )

        INTEGER     I, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                            &
!$OMP&       DEFAULT( NONE ),                                                   &
!$OMP&        SHARED( NPTS, NL, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL ),   &
!$OMP&       PRIVATE( I, J, K, L, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,            &
!$OMP&                W1, W2, W3 ),                                             &
!$OMP&     REDUCTION( .OR.:  EFLAG )

ILOOP:  DO I = 1, NPTS

            XX = MOD( X( I ) + 360.0D0, 360.0D0 )
            YY = Y( I )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        DO L = 1, NL
                            V( L,I ) = W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K )
                        END DO
                        CYCLE ILOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO ILOOP

        MPINTERPL1DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPL1DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPL1DD( NPTS, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NPTS, NL
        REAL(8), INTENT(IN   ) :: Y( NPTS )             !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NPTS )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( NL,MPCELLS )
        REAL,    INTENT(  OUT) :: V( NL,NPTS )

        INTEGER     I, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                            &
!$OMP&       DEFAULT( NONE ),                                                   &
!$OMP&        SHARED( NPTS, NL, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL ),   &
!$OMP&       PRIVATE( I, J, K, L, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,            &
!$OMP&                W1, W2, W3 ),                                             &
!$OMP&     REDUCTION( .OR.:  EFLAG )

ILOOP:  DO I = 1, NPTS

            XX = MOD( X( I ) + 360.0D0, 360.0D0 )
            YY = Y( I )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        DO L = 1, NL
                            V( L,I ) = W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K )
                        END DO
                        CYCLE ILOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO ILOOP

        MPINTERPL1DD = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPL1DD

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPL2DF( NC, NR, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL,    INTENT(IN   ) :: Y( NC, NR )             !!  latitude,  degrees
        REAL,    INTENT(IN   ) :: X( NC, NR )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( NL,MPCELLS )         !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NL, NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                            &
!$OMP&       DEFAULT( NONE ),                                                   &
!$OMP&        SHARED( NC, NR, NL, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL ), &
!$OMP&       PRIVATE( C, R, J, K, L, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,         &
!$OMP&                W1, W2, W3 ),                                             &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        DO L = 1, NL
                            V( L,C,R ) = W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K )
                        END DO
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPL2DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPL2DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPL2DD( NC, NR, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL(8), INTENT(IN   ) :: Y( NC, NR )             !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NC, NR )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( NL, MPCELLS )
        REAL,    INTENT(  OUT) :: V( NL, NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                            &
!$OMP&       DEFAULT( NONE ),                                                   &
!$OMP&        SHARED( NC, NR, NL, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL ), &
!$OMP&       PRIVATE( C, R, J, K, L, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,         &
!$OMP&                W1, W2, W3 ),                                             &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        DO L = 1, NL
                            V( L,C,R ) = W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K )
                        END DO
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPL2DD = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPL2DD

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPEL2DF( NC, NR, A, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL,    INTENT(IN   ) :: A                       !!  area of output-grid cell
        REAL,    INTENT(IN   ) :: Y( NC, NR )             !!  latitude,  degrees
        REAL,    INTENT(IN   ) :: X( NC, NR )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( NL,MPCELLS )         !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NL, NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        REAL        ARAT            !!  area-ratio
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                                        &
!$OMP&       DEFAULT( NONE ),                                                               &
!$OMP&        SHARED( NC, NR, NL, A, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL, CAREAS ),  &
!$OMP&       PRIVATE( C, R, J, K, L, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,                     &
!$OMP&                W1, W2, W3, ARAT ),                                                   &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A / CAREAS( M )
                        DO L = 1, NL
                            V( L,C,R ) = ARAT * ( W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K ) )
                        END DO
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPEL2DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPEL2DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPEL2DD( NC, NR, A, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL(8), INTENT(IN   ) :: A
        REAL(8), INTENT(IN   ) :: Y( NC, NR )             !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NC, NR )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( NL, MPCELLS )
        REAL,    INTENT(  OUT) :: V( NL, NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        REAL        ARAT            !!  area-ratio
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                                        &
!$OMP&       DEFAULT( NONE ),                                                               &
!$OMP&        SHARED( NC, NR, NL, A, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL, CAREAS ),  &
!$OMP&       PRIVATE( C, R, J, K, L, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,                     &
!$OMP&                W1, W2, W3, ARAT ),                                                   &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A / CAREAS( M )
                        DO L = 1, NL
                            V( L,C,R ) = ARAT * ( W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K ) )
                        END DO
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPEL2DD = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPEL2DD

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPGL2DF( NC, NR, A, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL,    INTENT(IN   ) :: A( NC, NR )             !!  area of output-grid cells
        REAL,    INTENT(IN   ) :: Y( NC, NR )             !!  latitude,  degrees
        REAL,    INTENT(IN   ) :: X( NC, NR )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( NL,MPCELLS )         !!  variable to be interpolated
        REAL,    INTENT(  OUT) :: V( NL, NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        REAL        ARAT            !!  area-ratio
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                                        &
!$OMP&       DEFAULT( NONE ),                                                               &
!$OMP&        SHARED( NC, NR, NL, A, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL, CAREAS ),  &
!$OMP&       PRIVATE( C, R, J, K, L, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,                     &
!$OMP&                W1, W2, W3, ARAT ),                                                   &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A( C,R ) / CAREAS( M )
                        DO L = 1, NL
                            V( L,C,R ) = ARAT * ( W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K ) )
                        END DO
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPGL2DF = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPGL2DF

    !!.............................................................................

    LOGICAL FUNCTION MPINTERPGL2DD( NC, NR, A, Y, X, NL, Z, V )
        INTEGER, INTENT(IN   ) :: NC, NR, NL
        REAL(8), INTENT(IN   ) :: A( NC, NR )
        REAL(8), INTENT(IN   ) :: Y( NC, NR )             !!  latitude,  degrees
        REAL(8), INTENT(IN   ) :: X( NC, NR )             !!  longitude, degrees
        REAL,    INTENT(IN   ) :: Z( NL, MPCELLS )
        REAL,    INTENT(  OUT) :: V( NL, NC, NR )

        INTEGER     C, R, J, K, L, M
        REAL(8)     XX, YY, X1, Y1, X2, Y2, X3, Y3
        REAL(8)     W1, W2, W3
        REAL        ARAT            !!  area-ratio
        LOGICAL     EFLAG

        !!........  Find closest cells K1, K2 (in that order)

        EFLAG = .FALSE.     !!  no errors yet

!$OMP    PARALLEL DO                                                                        &
!$OMP&       DEFAULT( NONE ),                                                               &
!$OMP&        SHARED( NC, NR, NL, A, Y, X, Z, V, ALONC, ALATC, NBNDYE, BNDYCELL, CAREAS ),  &
!$OMP&       PRIVATE( C, R, J, K, L, M, XX, YY, X1, Y1, X2, Y2, X3, Y3,                     &
!$OMP&                W1, W2, W3, ARAT ),                                                   &
!$OMP&     REDUCTION( .OR.:  EFLAG )

        DO R = 1, NR
CLOOP:  DO C = 1, NC

            XX = MOD( X( C,R ) + 360.0D0, 360.0D0 )
            YY = Y( C,R )

            M = FINDCELL( YY, XX )
            IF ( M .LT. 0 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            ELSE IF ( NBNDYE(M) .LT. 2 ) THEN
                EFLAG = .TRUE.
                CYCLE CLOOP
            END IF

            X1 = ALONC( M )
            Y1 = ALATC( M )

            DO J = 1,  NBNDYE( M )
                K  = 1 + MOD( J, NBNDYE( M ) )     !!  next adjacent bdycell-subscript
                X2 = ALONC( BNDYCELL( J,M ) )
                Y2 = ALATC( BNDYCELL( J,M ) )
                X3 = ALONC( BNDYCELL( K,M ) )
                Y3 = ALATC( BNDYCELL( K,M ) )
                IF ( BARYFAC( YY, XX, Y1, X1, Y2, X2, Y3, X3, W1, W2, W3 ) ) THEN

                        ARAT = A( C,R ) / CAREAS( M )
                        DO L = 1, NL
                            V( L,C,R ) = ARAT * ( W1 * Z( L,M ) + W2 * Z( L,J ) + W3 * Z( L,K ) )
                        END DO
                        CYCLE CLOOP

                END IF

            END DO      !! end loop on boundary-cells for this cell

            EFLAG = .TRUE.

        END DO CLOOP
        END DO

        MPINTERPGL2DD = ( .NOT.EFLAG )
        RETURN

    END FUNCTION MPINTERPGL2DD


    !!.............................................................................


    LOGICAL FUNCTION DBLERR( P, Q )
        REAL*8, INTENT( IN ) :: P, Q
        DBLERR = ( (P - Q)**2  .GT.  1.0D-10*( P*P + Q*Q + 1.0D-5 ) )
    END FUNCTION DBLERR



END MODULE MODMPASFIO
