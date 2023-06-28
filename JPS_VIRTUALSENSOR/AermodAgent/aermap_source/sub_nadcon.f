C**   PROGRAM NADCON
C**
C**   THE NATIONAL GEODETIC SURVEY NADCON PROGRAM (version 2.10)
C**   HAS BEEN ADAPTED FOR THE AERMAP TERRAIN PROCESSOR.

      SUBROUTINE NADCON(XPT,YPT,XPT2,YPT2,DLOS,DLAS,DLOM,DLAM,KEY)

***********************************************************************
*                                                                     *
* PROGRAM :   NADCON                                                  *
*                                                                     *
* PURPOSE:    COMPUTATION PROGRAM TO CONVERT (OR TRANSFORM)           *
*             POSITIONAL DATA (E.G., LATITUDES AND LONGITUDES) FROM   *
*             THE NORTH AMERICAN DATUM OF 1927 (NAD 27) TO THE        *
*             NORTH AMERICAN DATUM OF 1983 (NAD 83).  THIS PROGRAM    *
*             CAN COMPUTE FROM FROM EITHER DATUM TO THE OTHER.        *
*                                                                     *
*             THE ACTUAL COMPUTATION IS PERFORMED AS AN INTERPOLATION *
*             FROM A REGULARLY-SPACED GRID OF POINTS OBTAINED FROM THE*
*             FITTING OF A MINIMUM-CURVATURE SURFACE TO THE ACTUAL    *
*             SHIFT DATA RESULTING FROM THE NAD 83 ADJUSTMENT.        *
*                                                                     *
*             THE INTERPOLATION IS ACCOMPLISHED BY LOCALLY FITTING    *
*             A CURVED POLYNOMIAL SURFACE TO THE FOUR DATA POINTS     *
*             DEFINING THE SQUARE WHICH SURROUND THE (X,Y) PAIR       *
*             WHERE THE INTERPOLATION IS TO TAKE PLACE.               *
*                                                                     *
*             THE POLYNOMIAL SURFACE IS DEFINED BY:                   *
*                                                                     *
*                         A+BX+CY+DXY=Z                               *
*                                                                     *
*             THE PROGRAM REQUIRES THAT THE USER SPECIFY:             *
*                                                                     *
*             1)  THE NAME OF AN OUTPUT FILE                          *
*                                                                     *
*             2)  THE NAME OF AN INPUT FILE (IF AVAILABLE).           *
*                                                                     *
*                                                                     *
*                                                                     *
*             ESTIMATES OF DATUM SHIFTS IN TERMS OF METERS ARE        *
*             COMPUTED FROM THE SHIFT ESTIMATES USING ELLIPSOIDAL     *
*             SCALING.                                                *
*                                                                     *
*             THIS PROGRAM ALLOWS FOR EITHER NGS STANDARD HORIZONTAL  *
*             DATA FORMATS AS SPECIFIED IN THE FGCC PUBLICATION,      *
*             COMMONLY KNOWN AS THE 'HORIZONTAL BLUE BOOK' (SEE       *
*             SUBROUTINE TYPE3), OR IN A GENERIC FILE FORMAT (SEE     *
*             SUBROUTINE TYPE1 OR SUBROUTINE TYPE2).                  *
*                                                                     *
*             THE CODE CAN BE EASILY MODIFIED TO ACCOMMODATE CUSTOM   *
*             FILE SPECIFICATIONS BY MODIFYING SUBROUTINES: ENDREP,   *
*             GETPT, IPARMS, WRTPT, AND (OPTIONALLY) FHELP.           *
*                                                                     *
*                                                                     *
* VERSION CODE:  1.03                                                 *
*                                                                     *
* VERSION DATE:  APRIL 1, 1991                                        *
*                                                                     *
*        AUTHOR:   WARREN T. DEWHURST, PH.D.                          *
*                    LIEUTENANT COMMANDER, NOAA                       *
*                  ALICE R. DREW                                      *
*                    SENIOR GEODESIST, HORIZONTAL NETWORK BRANCH      *
*                  NATIONAL GEODETIC SURVEY, NOS, NOAA                *
*                  ROCKVILLE, MD   20852                              *

c version 2.10 - 1/20/92
c      added option to select HPGN grids and compute NAD 83 - HPGN
c      conversions - jmb
***********************************************************************

***********************************************************************
*                                                                     *
*                  DISCLAIMER                                         *
*                                                                     *
*   THIS PROGRAM AND SUPPORTING INFORMATION IS FURNISHED BY THE       *
* GOVERNMENT OF THE UNITED STATES OF AMERICA, AND IS ACCEPTED AND     *
* USED BY THE RECIPIENT WITH THE UNDERSTANDING THAT THE UNITED STATES *
* GOVERNMENT MAKES NO WARRANTIES, EXPRESS OR IMPLIED, CONCERNING THE  *
* ACCURACY, COMPLETENESS, RELIABILITY, OR SUITABILITY OF THIS         *
* PROGRAM, OF ITS CONSTITUENT PARTS, OR OF ANY SUPPORTING DATA.       *
*                                                                     *
*   THE GOVERNMENT OF THE UNITED STATES OF AMERICA SHALL BE UNDER NO  *
* LIABILITY WHATSOEVER RESULTING FROM ANY USE OF THIS PROGRAM.  THIS  *
* PROGRAM SHOULD NOT BE RELIED UPON AS THE SOLE BASIS FOR SOLVING A   *
* PROBLEM WHOSE INCORRECT SOLUTION COULD RESULT IN INJURY TO PERSON   *
* OR PROPERTY.                                                        *
*                                                                     *
*   THIS PROGRAM IS PROPERTY OF THE GOVERNMENT OF THE UNITED STATES   *
* OF AMERICA.  THEREFORE, THE RECIPIENT FURTHER AGREES NOT TO ASSERT  *
* PROPRIETARY RIGHTS THEREIN AND NOT TO REPRESENT THIS PROGRAM TO     *
* ANYONE AS BEING OTHER THAN A GOVERNMENT PROGRAM.                    *
*                                                                     *
***********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION VRSION
      INTEGER MXAREA
c     PARAMETER (VRSION = 1.03D0, MXAREA = 8)
      PARAMETER (VRSION = 2.10D0, MXAREA = 8)

      DOUBLE PRECISION ADLAM, VDLAM, ADLOM, VDLOM
      DOUBLE PRECISION ADLAS, VDLAS, ADLOS, VDLOS
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      DOUBLE PRECISION XPT, YPT, XPT2, YPT2
      INTEGER KEY, IPAGE, ITYPE, IFILE
      LOGICAL PAGE, NODATA, SCREEN,dsel

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

**********************
* INITIALIZE VARIABLES
**********************
      CALL INITL (SCREEN, PAGE, IPAGE, ITYPE,
     +            SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +            SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +            ADLAM, VDLAM, SDLAM, SDLAM2,
     +            ADLOM, VDLOM, SDLOM, SDLOM2,
     +            ADLAS, VDLAS, SDLAS, SDLAS2,
     +            ADLOS, VDLOS, SDLOS, SDLOS2,
     +            XSMALL, XBIG, YSMALL, YBIG,dsel)


******************************************************** 
* OPEN NADCON DATA FILES (LATITUDE AND LONGITUDE GRIDS)
*******************************************************

      LUOUT = 7 ! Information is written to AERMAP.OUT  ! crwb

       dsel = .TRUE.

      ITYPE = 3      ! rwb

*********************************
* LOOP (ONCE FOR EACH CONVERSION)
*********************************

      CALL MLOOP (IPAGE, ITYPE, KEY, VRSION,
crwb     +            DLAM, DLOM, DLAS, DLOS,
     +            XPT, YPT, XPT2, YPT2, DLAM, DLOM, DLAS, DLOS,
     +            SDLAM, SDLAM2, SDLOM, SDLOM2,
     +            SDLAS, SDLAS2, SDLOS, SDLOS2,
     +            SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +            SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +            XSMALL, XBIG, YSMALL, YBIG,
     +            SCREEN,dsel)

      CLOSE (NIN, STATUS='KEEP')
      CLOSE (NOUT, STATUS='KEEP')
      CLOSE (NAPAR, STATUS='KEEP')
      
crwb  check for values of DLOS and DLAS out-of-range;
crwb  very large values have been seen with corrupted 
crwb  grid files.  write message to main output file
      IF (DABS(DLOS).GT.100.0D0 .OR. DABS(DLAS).GT.100.0D0) THEN
         WRITE(LUOUT,*) ' '
         WRITE(LUOUT,*) 'POTENTIAL NADCON ERROR!!'
         WRITE(LUOUT,*) 'NAD shift values are out-of-range.'
         WRITE(LUOUT,*) 'NAD Grid files may be corrupted.'
         WRITE(LUOUT,*) 'NAD Shifts in arc-seconds:'
         WRITE(LUOUT,*) '    DLOS = ', DLOS
         WRITE(LUOUT,*) '    DLAS = ', DLAS
         WRITE(LUOUT,*) ' '
      END IF

 9999 RETURN

      END


      SUBROUTINE INITL (SCREEN, PAGE, IPAGE, ITYPE,
     +                  SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                  SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                  ADLAM, VDLAM, SDLAM, SDLAM2,
     +                  ADLOM, VDLOM, SDLOM, SDLOM2,
     +                  ADLAS, VDLAS, SDLAS, SDLAS2,
     +                  ADLOS, VDLOS, SDLOS, SDLOS2,
     +                  XSMALL, XBIG, YSMALL, YBIG,dsel)

*** This subroutine initializes all the variables needed in NADCON

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*20 B20
      CHARACTER*80 B80
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)

      DOUBLE PRECISION ADLAM, VDLAM, ADLOM, VDLOM
      DOUBLE PRECISION ADLAS, VDLAS, ADLOS, VDLOS
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      INTEGER IPAGE, ITYPE
      LOGICAL PAGE, SCREEN, dsel

      CHARACTER*80 CARD
      COMMON /CURNT/ CARD

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

* Initialize card variable in common CURNT to blank

      CARD = B80


******************************************************************
*                             INITIALIZE
******************************************************************

* Defaults: SCREEN = .TRUE. => send results to screen
*           PAGE = .FALSE.  => don't start a new page in the output file
*           IPAGE = 0       => current output file page number is 0
*           ITYPE = 0       => interactive input of points
*           dsel  = .FALSE. => select NAD 83, HPGN datum conversion

      SCREEN = .TRUE.
      PAGE = .FALSE.
      IPAGE = 0
      ITYPE = 0
      dsel = .FALSE.

      SMDLAM =  1.0D10
      BGDLAM = -1.0D10
      SMDLOM =  1.0D10
      BGDLOM = -1.0D10
      SMDLAS =  1.0D10
      BGDLAS = -1.0D10
      SMDLOS =  1.0D10
      BGDLOS = -1.0D10

      ADLAM = 0.0D0
      VDLAM = 0.0D0
      SDLAM = 0.0D0
      SDLAM2 = 0.0D0
      ADLOM = 0.0D0
      VDLOM = 0.0D0
      SDLOM = 0.0D0
      SDLOM2 = 0.0D0

      ADLAS = 0.0D0
      VDLAS = 0.0D0
      SDLAS = 0.0D0
      SDLAS2 = 0.0D0
      ADLOS = 0.0D0
      VDLOS = 0.0D0
      SDLOS = 0.0D0
      SDLOS2 = 0.0D0

      XSMALL =  1.0D10
      XBIG   = -1.0D10
      YSMALL =  1.0D10
      YBIG   = -1.0D10

      RETURN
      END
      
      SUBROUTINE COEFF (TEE1, TEE2, TEE3, TEE4, AY, BEE, CEE, DEE)

**********************************************************************
** SUBROUTINE COEFF: GENERATES COEFFICIENTS FOR SURFACE FUNCTION     *
**********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION AY, BEE, CEE, DEE
      DOUBLE PRECISION TEE1, TEE2, TEE3, TEE4

      AY = TEE1
      BEE = TEE3 - TEE1
      CEE = TEE2 - TEE1
      DEE = TEE4 - TEE3 - TEE2 + TEE1

      RETURN
      END
      

      SUBROUTINE DGRIDS(ILEN_FLD,NGPATH)

* This subroutine opens the NADCON grids using the default grid
* names and locations.  The default names of the grid areas are
* given in DAREAS and the default base file locations are in DFILES

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER ILEN_FLD
      INTEGER MXAREA, MXDEF
      PARAMETER (MXAREA = 8, MXDEF = MXAREA)

      CHARACTER*80 B80
      CHARACTER*65 B65
      CHARACTER*20 B20
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)
      PARAMETER (B65 = B20//B20//B20//'     ')

      DOUBLE PRECISION XMAX1, XMIN1, YMAX1, YMIN1
      DOUBLE PRECISION DX1, DY1
      INTEGER IDEF, ITEMP, NC1
      CHARACTER*80 DUM
      CHARACTER*15 DFILES(MXAREA)
      CHARACTER (LEN=ILEN_FLD+15) :: AFILE
      CHARACTER (LEN=ILEN_FLD) :: NGPATH
      CHARACTER*15 DAREAS(MXDEF)
      LOGICAL NOGO, GFLAG

      CHARACTER*15 AREAS
      COMMON /AREAS/ AREAS(MXAREA)

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA DUM / B80 /

* DFILES contains the default locations (pathname) of the grid files
* without the .las and .los extensions. (For example 'conus' would
* indicate that the conus.las and conus.los grid files are in the
* current working directory.)  The length of each entry in DFILES may
* be up to 65 characters.  DAREAS contains the default names of these
* areas.  The names are used internally in the program and in the
* program output.  They may be no longer than 15 characters.  They
* must correspond on a one-to-one basis with the file locations in
* the DFILES array.  That is, the first area name in DAREAS must
* be the name that you wish for the first data file set in the
* DFILES array.  You may, of course, have the arrays the same if
* the location of the data file is no longer than 15 characters.
* The locations of the grid files may be differ for each
* installation.  If the pathnames are not correct DFILES (and, possibly,
* DAREAS) may be changed and the program recompiled.

      DATA DFILES /'conus', 'hawaii', 'prvi',
     +             'stlrnc', 'stgeorge', 'stpaul', 'alaska', ' '/
      DATA DAREAS /'Conus', 'Hawaii', 'P.R. and V.I.',
     +             'St. Laurence I.', 'St. George I.', 'St. Paul I.',
     +             'Alaska', ' '/

      GFLAG = .FALSE.
      WRITE (LUOUT, 80) NGPATH
   80 FORMAT (/, '      Default Data Grids', /,
     +           '      NADGRIDS Pathname: ', A,/,
     +           '   #  AREA NAME', /, 1X, 79('=') )

      DO 140 IDEF = 1, MXDEF
        AFILE = NGPATH//DFILES(IDEF)
        IF (AFILE .EQ. B65) GOTO 999

* Try to open a set of default files.
* Do not print error messages for non-existing files.

        ITEMP = NAREA + 1
        CALL OPENFL (AFILE, ITEMP, GFLAG, NOGO, DX1, DY1,
     +               XMAX1, XMIN1, YMAX1, YMIN1, NC1, DUM,
     +               ILEN_FLD+15)

        IF (.NOT. NOGO) THEN

* Set of files opened OK and variables read

          NAREA = ITEMP
          AREAS(NAREA) = DAREAS(IDEF)
          DX(NAREA) = DX1
          DY(NAREA) = DY1
          XMAX(NAREA) = XMAX1
          XMIN(NAREA) = XMIN1
          YMAX(NAREA) = YMAX1
          YMIN(NAREA) = YMIN1
          NC(NAREA) = NC1

          WRITE (LUOUT,120) NAREA, AREAS(NAREA)
  120     FORMAT (2X, I2, 2X, A15/)

        ENDIF

  140 CONTINUE

  999 RETURN
      END
      

      SUBROUTINE FGRID (XPT, YPT, DX, DY, XMAX, XMIN,
     +                  YMAX, YMIN, XGRID, YGRID, IROW, JCOL, NOGO)

**********************************************************************
** SUBROUTINE FGRID: IDENTIFIES THE LOCAL GRID SQUARE FOR INTRP.     *
**********************************************************************

* This subroutine is designed to identify the grid square in which a
* particular point is located and get the corner coordinates
* converted into the index coordinate system.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION XPT, YPT, XGRID, YGRID
      DOUBLE PRECISION XMAX, XMIN, YMAX, YMIN
      DOUBLE PRECISION DX, DY
      INTEGER IROW, JCOL
      LOGICAL NOGO

      NOGO = .FALSE.

* Check to see it the point is outside the area of the gridded data

      IF (XPT .GE. XMAX  .OR.  XPT .LE. XMIN   .OR.
     +    YPT .GE. YMAX  .OR.  YPT .LE. YMIN ) THEN
        NOGO = .TRUE.
*       WRITE (*,*) '***THE POINT IS OUT OF BOUNDS***'
        GOTO 200
      ENDIF

* Calculate the coordinate values for the point to be interpolated
* in terms of grid indices

      XGRID = ( XPT - XMIN )/DX + 1.D0
      YGRID = ( YPT - YMIN )/DY + 1.D0

* Find the I,J values for the SW corner of the local square

      IROW = IDNINT(YGRID)
      JCOL = IDNINT(XGRID)

  200 RETURN
      END
      

      SUBROUTINE HMS (DD, ID, IM, S)

* Use this to change from decimal degrees (double precision)
* to integer degrees, integer minutes, and decimal seconds (double prec)
* Seconds are assumed to have no more than 5 decimal places

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION SMALL
      PARAMETER (SMALL = 1.D-5)

      DOUBLE PRECISION DD, TEMP
      DOUBLE PRECISION S
      INTEGER ID, IM

      ID = IDINT(DD)
      TEMP = ( DD - DBLE(ID) )*60.0D0
      IM = IDINT(TEMP)
      S = ( TEMP - DBLE(IM) )*60.0D0

      IF (IM .EQ. 60) THEN
        IM = 0
        ID = ID + 1
      ENDIF

      IF (S .LT. SMALL) S = 0.0D0

      IF (S .GT. (60.0D0-SMALL)  ) THEN
        S = 0.0D0
        IM = IM + 1
      ENDIF

      RETURN
      END


      SUBROUTINE INTRP (IAREA, IROW, NC, JCOL, XGRID, YGRID,
     +                  XPT, YPT, XPT2, YPT2, DLOS, DLAS, DLAM, DLOM)

**********************************************************************
** DETERMINE SURFACE FUNCTION FOR THIS GRID SQUARE                   *
** AND INTERPOLATE A VALUE, ZEE, FOR XPT, YPT                        *
**********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA, MAXCOL
      PARAMETER (MAXCOL = 600, MXAREA = 8)

      DOUBLE PRECISION XPT, YPT, XPT2, YPT2, XGRID, YGRID
      DOUBLE PRECISION DLOS, DLAS, DLAM, DLOM
      DOUBLE PRECISION TEE1, TEE2, TEE3, TEE4, ZEE
      INTEGER IROW, JCOL, NC, IAREA, IFILE, IDUM, J
      REAL BUF(MAXCOL)
      
      CHARACTER*8 DUMMY

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DOUBLE PRECISION AY1, BEE1, CEE1, DEE1, AY2, BEE2, CEE2, DEE2
      SAVE AY1, BEE1, CEE1, DEE1, AY2, BEE2, CEE2, DEE2
      INTEGER IROWL, JCOLL, IAREAL
      SAVE IROWL, JCOLL, IAREAL

      DATA IROWL / 0 /, JCOLL / 0 /, IAREAL / 0 /

**********
* LATITUDE
**********

      IF ( IROW .NE. IROWL  .OR.  JCOL .NE. JCOLL  .OR.
     +    IAREA .NE. IAREAL ) THEN

* Lower boundary

        IFILE = LUAREA( 2*IAREA - 1 )
        READ (IFILE,REC=IROW+1, ERR=99) IDUM, (BUF(J), J=1,NC)
        TEE1 = DBLE( BUF(JCOL) )
*       TEE4 = DBLE( BUF(JCOL+1) )
        TEE3 = DBLE( BUF(JCOL+1) )

* Upper boundary

        READ (IFILE,REC=IROW+2, ERR=99) IDUM, (BUF(J), J=1,NC)
        TEE2 = DBLE( BUF(JCOL) )
*       TEE3 = DBLE( BUF(JCOL+1) )
        TEE4 = DBLE( BUF(JCOL+1) )

        CALL COEFF (TEE1, TEE2, TEE3, TEE4, AY1, BEE1, CEE1, DEE1)

      ENDIF


      CALL SURF (XGRID, YGRID, ZEE, AY1, BEE1, CEE1, DEE1, IROW, JCOL)
      DLAS = ZEE

***********
* LONGITUDE
***********

      IF ( IROW .NE. IROWL  .OR.  JCOL .NE. JCOLL  .OR.
     +    IAREA .NE. IAREAL ) THEN


* Lower boundary

        IFILE = LUAREA( 2*IAREA )
        READ (IFILE,REC=IROW+1, ERR=99) IDUM, (BUF(J), J=1,NC)
        TEE1 = DBLE( BUF(JCOL) )
*       TEE4 = DBLE( BUF(JCOL+1) )
        TEE3 = DBLE( BUF(JCOL+1) )

* Upper boundary

        READ (IFILE,REC=IROW+2, ERR=99) IDUM, (BUF(J), J=1,NC)
        TEE2 = DBLE( BUF(JCOL) )
*       TEE3 = DBLE( BUF(JCOL+1) )
        TEE4 = DBLE( BUF(JCOL+1) )

        CALL COEFF (TEE1, TEE2, TEE3, TEE4, AY2, BEE2, CEE2, DEE2)

      ENDIF

      CALL SURF (XGRID, YGRID, ZEE, AY2, BEE2, CEE2, DEE2, IROW, JCOL)
C*    Since we are using negative for West longitude, reverse sign of DLOS, rwb
      DLOS = -1.0D0*ZEE

**************************
* COMPUTE THE NAD 83 VALUES
**************************

      YPT2 = YPT + DLAS/3600.0D0

* Longitude is negative West in this subroutine, rwb

      XPT2 = XPT + DLOS/3600.0D0

*********************************************************************
* USE THE NEW ELLIPSOIDAL VARIABLES TO COMPUTE THE SHIFTS IN METERS
*********************************************************************

      CALL METERS (YPT, XPT, YPT2, XPT2, DLAM, DLOM)

* Update the last-value variables

      IROWL = IROW
      JCOLL = JCOL
      IAREAL = IAREA

      GO TO 90

 99   CONTINUE
      WRITE(DUMMY,'("NGRID",I3.3)') IFILE
      CALL ERRHDL('NC','INTRP','E','510',DUMMY)

 90   CONTINUE

      RETURN
      END
      

      SUBROUTINE METERS (LAT1, LONG1, LAT2, LONG2, LATMTR, LONMTR)

* This subroutine computes the difference in two positions in meters.
*
* This method utilizes ellipsoidal rather than spherical
* parameters.  I believe that the original approach and code
* for this came from Ed McKay.
* The reference used by Ed McKay for this was:
*       'A Course in Higher Geodesy' by P.W. Zakatov, Israel Program
*       for Scientific Translations, Jerusalem, 1962
*
*       Warren T. Dewhurst
*       11/1/89
* Note that this subroutine is set up for +west longitude

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

* I think that these are GRS80 parameters

      DOUBLE PRECISION AXIS, E2, RHOSEC
      PARAMETER (AXIS = 6378137.0D0)
      PARAMETER (E2 = 0.0066943800229D0)
      PARAMETER (RHOSEC = 206264.806247D0)

      DOUBLE PRECISION W, LM, LP, AVLAT
      DOUBLE PRECISION LAT1S, LAT2S, LONG1S, LONG2S, LAT1, LAT2
      DOUBLE PRECISION LONG1, LONG2, DLAT, DLONG
      DOUBLE PRECISION LATMTR, LONMTR


*     LAT1  = (LATSEC + 60.D0*( LATMIN + 60.D0*LATDEG) )/RHOSEC
*     LONG1 = (LONSEC + 60.D0*( LONMIN + 60.D0*LONDEG) )/RHOSEC
*     LAT2  = (LATSEC + 60.D0*( LATMIN + 60.D0*LATDEG) )/RHOSEC
*     LONG2 = (LONSEC + 60.D0*( LONMIN + 60.D0*LONDEG) )/RHOSEC

* Change into sec.ddd and convert to +west longitude

      LAT1S =    LAT1*60.D0*60.D0/RHOSEC
      LONG1S =  LONG1*60.D0*60.D0/RHOSEC       ! adjusted for negative West, rwb
      LAT2S =    LAT2*60.D0*60.D0/RHOSEC
      LONG2S =  LONG2*60.D0*60.D0/RHOSEC       ! adjusted for negative West, rwb

      DLAT  = ( LAT2S -  LAT1S)*RHOSEC
      DLONG = (LONG2S - LONG1S)*RHOSEC

      AVLAT = (LAT1S + LAT2S)/2.0D0

      W  = DSQRT(1.0D0 - E2*DSIN(AVLAT)**2)
      LM = AXIS*(1.0D0 - E2)/(W**3*RHOSEC)
      LP = AXIS*DCOS(AVLAT)/(W*RHOSEC)

      LATMTR = LM*DLAT
      LONMTR = LP*DLONG

      RETURN
      END
      
      SUBROUTINE MLOOP (IPAGE, ITYPE, KEY, VRSION,
crwb     +                  DLAM, DLOM, DLAS, DLOS,
     +                  XPT, YPT, XPT2, YPT2, DLAM, DLOM, DLAS, DLOS,
     +                  SDLAM, SDLAM2, SDLOM, SDLOM2,
     +                  SDLAS, SDLAS2, SDLOS, SDLOS2,
     +                  SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                  SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                  XSMALL, XBIG, YSMALL, YBIG,
     +                  SCREEN,dsel)

**********************************************************************
* THIS SUBROUTINE LOOPS THROUGH THE INPUT DATA (EITHER AN INPUT DATA *
* FILE OR INTERACTIVELY), CALCULATES THE TRANSFORMATION VALUES,      *
* UPDATES THE MINIMUM, MAXIMUM, AND STATISTICAL SUMMATIONS, AND THEN *
* PRINTS THE RESULTS TO THE OUTPUT FILE AND/OR THE SCREEN.           *
**********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION DLAM2, DLOM2, DLAS2, DLOS2
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG, XPT, XPT2, YPT, YPT2
      DOUBLE PRECISION VRSION
      DOUBLE PRECISION SLA, SLO, SLA2, SLO2
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      INTEGER IPAGE, ITYPE, KEY, IFMT, IPREC
      INTEGER IDLA, IMLA, IDLO, IMLO
      INTEGER IDLA2, IMLA2, IDLO2, IMLO2
      CHARACTER*80 NAME
      CHARACTER*44 FIRST
      CHARACTER*30 LAST
      CHARACTER*15 RESP
      LOGICAL NOGO, SCREEN, NOPT, EOF,dsel

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

* set defaults for those variables not used by every format type

      DATA NAME /' '/, FIRST /' '/, LAST /' '/, IFMT /0/

*******************************************************************
* BEGIN THE COMPUTATION LOOP FOR EACH CONVERSION
* DO UNTIL END OF FILE OR NO MORE CONVERSIONS REQUESTED
*******************************************************************

C*        Longitude is negative West, rwb

          CALL HMS (YPT, IDLA, IMLA, SLA)    ! rwb
          CALL HMS (XPT, IDLO, IMLO, SLO)    ! rwb


************************
* DO THE TRANSFORMATION
************************
        NOGO = .FALSE.
        CALL TRANSF (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +               DLAM, DLOM, DLAS, DLOS, KEY, ITYPE)

****************************************************
* CHECK TO SEE IF THIS POINT CAN BE TRANSFORMED
* IF NOGO IS TRUE THEN GET ANOTHER POINT AND DON'T
* DO THE COMPUTATION - POINT IS OUT OF BOUNDS
* IF NOGO IS NOT TRUE THEN PROCEED - ESTIMATE MADE
****************************************************

        IF (NOGO) GOTO 9999

        IF (KEY .EQ. 1) THEN

**********************
* FOR NAD 27 TO NAD 83
**********************

          CALL HMS (YPT2, IDLA2, IMLA2, SLA2)
          CALL HMS (XPT2, IDLO2, IMLO2, SLO2)

        ELSEIF (KEY .EQ. -1) THEN

**********************
* FOR NAD 83 TO NAD 27
**********************

          IDLA2 = IDLA
          IMLA2 = IMLA
          SLA2 = SLA
          IDLO2 = IDLO
          IMLO2 = IMLO
          SLO2 = SLO
          CALL HMS (YPT, IDLA, IMLA, SLA)
          CALL HMS (XPT, IDLO, IMLO, SLO)
        ENDIF

 9999 RETURN
      END
      


crwb      SUBROUTINE NGRIDS (NODATA,dsel,VRSION)
      SUBROUTINE NGRIDS (NODATA,dsel,ILEN_FLD,NGPATH)

* This subroutine opens the NADCON grids which contain datum shifts.
* A total of two files are necessary for each area; 1 for each latitude
* and longitude shift table (gridded data set) expressed in arc seconds.

* If a file named AREA.PAR exists it will be read for the names and
* locations of the gridded data.  The format of the data in
* the AREA.PAR file is given in the GRIDS subroutine.

* If the AREA.PAR file does not exist, or there is still room in the
* arrays in the GDINFO common then the default area names used.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER ILEN_FLD
      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      
      CHARACTER*1 ANS
      LOGICAL NODATA,dsel
      
      CHARACTER (LEN=ILEN_FLD) :: NGPATH

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      DOUBLE PRECISION VRSION
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

* Initialize

      NODATA = .FALSE.
      NAREA = 0
      LUOUT = 7

* If NAREA>=MXAREA, then skip the section that opens the default files.
* If NAREA<MXAREA or no 'AREA.PAR' file exists, then open default names
* in the subroutine DGRIDS.

c if state hpgn chosen(dsel=false) then only 1 file can be open at 
c a time.  If an hpgn file is not in area.par, then the user can 
c choose a state in SGRIDS.
       if(dsel) then
c default grids chosen
          IF (NAREA .LT. MXAREA) THEN

              CALL DGRIDS(ILEN_FLD,NGPATH)

          ENDIF
       end if


      IF (NAREA .EQ. 0) THEN
        NODATA = .TRUE.
        WRITE (LUOUT, 970)
  970   FORMAT (/, ' ******* ERROR *********', /,
     +          ' No grid files were opened -- program ending!')
      ENDIF

      RETURN
      END
      
      SUBROUTINE OPENFL (AFILE, ITEMP, GFLAG, NOGO, DX, DY,
     +                   XMAX1, XMIN1, YMAX1, YMIN1, NC1, CARD,
     +                   ILEN_FLD)

*** Given base name of gridded data files, open the two data files

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*23 B23
      PARAMETER (B23 = '                       ')
      CHARACTER*69 B69
      PARAMETER (B69 = B23//B23//B23)

      DOUBLE PRECISION XMAX1, XMIN1, YMAX1, YMIN1, DX, DY
      REAL DX1, DY1, DX2, DY2
      REAL X01, Y01, ANGLE1, X02, Y02, ANGLE2
      INTEGER IFLAG1, IFLAG2, N1, N2, N3, N4
      INTEGER ITEMP, LRECL, ILA, ILO, IFILE, IOS
      INTEGER NC1, NR1, NZ1, NC2, NR2, NZ2
      CHARACTER*80 CARD
      CHARACTER (LEN=ILEN_FLD+4) :: ALAS, ALOS
      CHARACTER (LEN=ILEN_FLD) :: AFILE
      CHARACTER*56 RIDENT
      CHARACTER*8 PGM
      LOGICAL GFLAG, NOGO, OFLAG, EFLAG1, EFLAG2

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA IFLAG1 /1/, IFLAG2 /2/
      DATA OFLAG /.FALSE./, EFLAG1 /.FALSE./, EFLAG2 /.FALSE./


* Initialize

      NOGO = .FALSE.

* Form complete names of grid files

      N2 = LEN_TRIM(AFILE)
      IF (N2 .EQ. 0) STOP 'Logical Coding Error in OPENF'

      ALAS = B69
      ALAS(1:N2) = AFILE

      ALAS(N2+1:N2+4) = '.las'
      ALOS = B69
      ALOS(1:N2) = AFILE
      ALOS(N2+1:N2+4) = '.los'

*******************************************************
* DIRECT ACCESS GRID FILES
* Each file is opened once to get the grid variables.
* The file is then closed and reopened to ensure that
* the record length is correct
*******************************************************

* Seconds of latitude grid file

      LRECL = 256
      ILA = 2*ITEMP - 1
      IFILE = ILA + 10
      LUAREA(ILA) = IFILE
      INQUIRE (FILE=ALAS, EXIST=EFLAG1, OPENED=OFLAG)
      IF (.NOT. EFLAG1) GOTO 100
      IF (OFLAG) GOTO 980
      OPEN (IFILE,FILE=ALAS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       ACTION='READ')
      READ (IFILE,REC=1) RIDENT, PGM, NC1, NR1, NZ1, X01, DX1,
     +                   Y01, DY1, ANGLE1
      CLOSE (IFILE)

      LRECL = 4*(NC1+1)
      OPEN (IFILE,FILE=ALAS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       ACTION='READ')

* Seconds of longitude grid file

  100 LRECL = 256
      ILO = 2*ITEMP
      IFILE = ILO + 10
      LUAREA(ILO) = IFILE
      INQUIRE (FILE=ALOS, EXIST=EFLAG2, OPENED=OFLAG)
      IF (.NOT. EFLAG1) GOTO 910
      IF (.NOT. EFLAG2) GOTO 920
      IF (OFLAG) GOTO 980
      OPEN (IFILE,FILE=ALOS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       ACTION='READ')
      READ (IFILE,REC=1) RIDENT, PGM, NC2, NR2, NZ2, X02, DX2,
     +                   Y02, DY2, ANGLE2
      CLOSE (IFILE)

      LRECL = 4*(NC2+1)
      OPEN (IFILE,FILE=ALOS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       ACTION='READ')

* Check to see if the two files have the same variables

      IF ( (NC2 .NE. NC1)  .OR.  (NR2 .NE. NR1)  .OR.
     +     (NZ2 .NE. NZ1)  .OR.
     +     (X02 .NE. X01)  .OR.  (DX2 .NE. DX1)  .OR.
     +     (Y02 .NE. Y01)  .OR.  (DY2 .NE. DY1)  .OR.
     +     (ANGLE2 .NE. ANGLE1) ) GOTO 960

* Calculate values used in this program

      XMIN1 = DBLE(X01)
      YMIN1 = DBLE(Y01)
      XMAX1 = DBLE(X01) + DBLE(NC1-1)*DBLE(DX1)
      YMAX1 = DBLE(Y01) + DBLE(NR1-1)*DBLE(DY1)
      DX = DBLE( ABS(DX1) )
      DY = DBLE( ABS(DY1) )

*****************************************
* REPORT SOMETHING ABOUT THE GRIDDED DATA
*****************************************
      WRITE (LUOUT,4050) RIDENT, PGM, NC1, NR1
 4050 FORMAT (1X, A56, /, 1X, A8, /, I5, I5)
      WRITE (LUOUT,*) 'DX,DY,NR,NC', DX1, DY1, NR1, NC1
      WRITE (LUOUT,4055) -XMAX1, -XMIN1, YMIN1, YMAX1
 4055 FORMAT (' MIN Longitude = ', F10.4, ' MAX Longitude = ', F10.4, /,
     +        ' MIN Latitude  = ', F10.4, ' MAX Latitude  = ', F10.4, /)
*****************************************

 9999 RETURN

****************************
* WARNING AND ERROR MESSAGES
****************************

* Grid files do not exist

  910 CONTINUE
      NOGO = .TRUE.
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

  920 CONTINUE
      NOGO = .TRUE.
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

* Grid file(s) already open

  940 CONTINUE
      NOGO = .TRUE.
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

* Grid files do not agree

  960 CONTINUE
      NOGO = .TRUE.
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

* Grid files already open

  980 CONTINUE
      NOGO = .TRUE.
      GOTO 9999
      
      END
      
      SUBROUTINE SURF (XGRID, YGRID, ZEE, AY, BEE, CEE, DEE, IROW, JCOL)

**********************************************************************
** SUBROUTINE SURF: INTERPOLATES THE Z VALUE                         *
**********************************************************************

* Calculated the value of the grid at the point XPT, YPT.  The
* interpolation is done in the index coordinate system for convenience.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION XGRID, YGRID
      DOUBLE PRECISION AY, BEE, CEE, DEE
      DOUBLE PRECISION ZEE, ZEE1, ZEE2, ZEE3, ZEE4
      INTEGER IROW, JCOL

      ZEE1 = AY
      ZEE2 = BEE*(XGRID - DBLE(JCOL) )
      ZEE3 = CEE*(YGRID - DBLE(IROW) )
      ZEE4 = DEE*(XGRID - DBLE(JCOL) )*(YGRID - DBLE(IROW) )
      ZEE  = ZEE1 + ZEE2 + ZEE3 + ZEE4

      RETURN
      END
      
      SUBROUTINE TO83 (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +                 DLAM, DLOM, DLAS, DLOS, ITYPE)

* This subroutine predicts the NAD 83 latitude and longitude values
* given the NAD 27 latitude and longitude values in degree decimal
* format.  In addition, the program returns the shift values between
* The datums in both arc secs and meters.

* All of the predictions are based upon a straight-forward interpolation
* of a gridded data set of datum shifts.  The datum shifts are assumed
* to be provided in the files opened in the NGRIDS subroutine.  The
* common AREAS contains the names of the valid areas while the common
* GDINFO contains the grid variables.  NAREA is the number of areas
* which had data files opened.  A total of two files are necessary for
* each area: one latitude and one longitude shift table (gridded data
* set) expressed in arc seconds.

*       Author:     Warren T. Dewhurst, PH. D.
*                   National Geodetic Survey
*                   November 1, 1989

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION XPT, YPT, XPT2, YPT2
      DOUBLE PRECISION XGRID, YGRID
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION DX0, DY0, XMAX0, XMIN0, YMAX0, YMIN0
      INTEGER IROW, JCOL, IAREA, I, NC0, ITYPE
      INTEGER IFLAG1, IFLAG2, N1, N2
      CHARACTER*15 RESP
      LOGICAL NOGO, FLAG

      CHARACTER*15 AREAS
      COMMON /AREAS/ AREAS(MXAREA)

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      CHARACTER*80 CARD
      COMMON /CURNT/ CARD

      SAVE FLAG

      DATA IFLAG1 /1/, IFLAG2 /2/, FLAG /.FALSE./

******************************************************************
*                             INITIALIZE
******************************************************************

      NOGO  =  .FALSE.

****************************************************
* READ WHERE TO GET THE DATA AND HOW IT IS ORGANIZED
****************************************************

* Check to see which set of gridded files XPT,YPT is in.

      DO IAREA = 1, NAREA

        DX0 = DX(IAREA)
        DY0 = DY(IAREA)
        XMAX0 = XMAX(IAREA)
        XMIN0 = XMIN(IAREA)
        YMAX0 = YMAX(IAREA)
        YMIN0 = YMIN(IAREA)
        NC0 = NC(IAREA)

        CALL FGRID (XPT, YPT, DX0, DY0, XMAX0, XMIN0,
     +              YMAX0, YMIN0, XGRID, YGRID, IROW, JCOL, NOGO)

        IF (.NOT. NOGO) EXIT
        
      END DO

      IF (NOGO) GO TO 9999
      
* Point in area number IAREA and named AREAS(IAREA)

        RESP = AREAS(IAREA)
        CALL INTRP (IAREA, IROW, NC0, JCOL, XGRID, YGRID,
     +              XPT, YPT, XPT2, YPT2, DLOS, DLAS, DLAM, DLOM)

* Error Messages

9999  RETURN
      END

      SUBROUTINE TRANSF (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +                   DLAM, DLOM, DLAS, DLOS, KEY, ITYPE)

* This subroutine computes either the forward or inverse coordinate
* transformation depending upon the value of the integer variable 'key'
c 1/20/92 - IF the HPGN option is chosen, statements in this subroutine
c which refer to NAD 27 apply to NAD 83; 
c statements which refer to NAD 83 apply to HPGN -jmb

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA, ITMAX
      DOUBLE PRECISION SMALL
      PARAMETER (MXAREA = 8, ITMAX = 10, SMALL = 1.0D-9 )

      DOUBLE PRECISION XPT, YPT, XPT2, YPT2
      DOUBLE PRECISION XTEMP, YTEMP, XDIF, YDIF
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION DXLAST, DYLAST
      INTEGER KEY, NUM, ITYPE
      CHARACTER*15 RESP
      LOGICAL NOGO

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      IF (KEY .EQ. 1) THEN

**********************
* FOR NAD 27 TO NAD 83
**********************

        CALL TO83 (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +             DLAM, DLOM, DLAS, DLOS, ITYPE)

      ELSEIF (KEY .EQ. -1) THEN

***************************
* FOR NAD 83 TO NAD 27)
* THIS IS DONE BY ITERATION
***************************

        NUM = 0

**************************************************
* SET THE XPT,YPT TO TEMPORARY VALUES
* (REMEMBER, XPT AND YPT ARE REALLY NAD 83 VALUES)
**************************************************

        XTEMP = XPT
        YTEMP = YPT

**************************************************************
* PRETEND THAT THESE TEMPORARY VALUES ARE REALLY NAD 27 VALUES
* FOR A FIRST GUESS AND COMPUTE PSEUDO-NAD 83 COORDINATES
**************************************************************

  200   CONTINUE
          NUM = NUM + 1

          CALL TO83 (NOGO, RESP, XTEMP, YTEMP, XPT2, YPT2,
     +               DLAM, DLOM, DLAS, DLOS, ITYPE)
          DXLAST = DLOS
          DYLAST = DLAS

**************************************
* COMPARE TO ACTUAL NAD 83 COORDINATES
**************************************

          XDIF = XPT - XPT2
          YDIF = YPT - YPT2

****************************************************************
* COMPUTE A NEW GUESS UNLESS THE DIFFERENCES ARE LESS THAN SMALL
* WHERE SMALL IS DEFINED (ABOVE) TO BE;  SMALL = 1.0D-9
****************************************************************

          IF (NUM .EQ. 1) THEN
            IF (DABS(XDIF) .GT. SMALL) THEN
              XTEMP = XPT - DLOS/3600.0D0
            ENDIF
            IF (DABS(YDIF) .GT. SMALL) THEN
              YTEMP = YPT - DLAS/3600.0D0
            ENDIF
          ELSE
            IF (DABS(XDIF) .GT. SMALL) THEN
              XTEMP = XTEMP - (XPT2 - XPT)
            ENDIF
            IF (DABS(YDIF) .GT. SMALL) THEN
              YTEMP = YTEMP - (YPT2 - YPT)
            ENDIF

          ENDIF

          IF (NUM.GE.ITMAX .OR. (DABS(YDIF).LE.SMALL .AND.
     &                           DABS(XDIF).LE.SMALL)) THEN

******************************
* IF CONVERGED THEN LEAVE LOOP
******************************

            XPT = XTEMP
            YPT = YTEMP
            GOTO 1000
          ENDIF

*******************************
* IF NOT CONVERGED THEN ITERATE
*******************************

        GOTO 200

      ENDIF
 1000 RETURN
      END

