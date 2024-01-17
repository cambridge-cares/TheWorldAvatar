      SUBROUTINE GRDWS
C=======================================================================
C                GRDWS module of the AERMOD Dispersion Model
C
C   Purpose:     To construct a profile of gridded values of wind speed
C
C   Input:       Parameter profile array
C                Number of levels in the profile
C                Height at which data are required
C
C   Output:      Array of values at the specified grid heights.
C
C   Assumptions:
C
C   Called by:
C
C   Programmer:  Jim Paumier                          30 Sept 1993
C                Pacific Environmental Services
C
C   Revision history:
C         12/5/94 - J. Paumier (Pacific Environmental Svcs., Inc)
C                 - changed the tolerance for a gridded profile height
C                   to be within 0.1 m of an observed profile height
C                   rather than 0.5 m
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER (LEN=12) :: MODNAM

      INTEGER          :: PINDEX, GINDEX
      DOUBLE PRECISION :: VBELOW, HTBELO
C
C---- Data definitions
C        PINDEX    Array index for the profile of observed values
C        GINDEX    Array index for the profile of gridded values
C        VBELOW    Nonmissing value from the observed data profile
C                  that is below the the gridded level
C
C
C---- Data initializations
C
      MODNAM = 'GRDWS       '
      PATH   = 'MX'
C
      GINDEX = 1
      PINDEX = 1
      VBELOW = -999.0D0
C
C     ------------------------------------------------------------------
C     Loop over each grid level until a value is computed for each level
C     where a value is required OR the number of levels in the gridded
C     profile exceeds the maximum number
C     ------------------------------------------------------------------

      DO WHILE( GINDEX .LE. MXGLVL )


C        -------------------------------------------
C        Now begin looping over the observed profile
C        -------------------------------------------
C
         DO WHILE( GRIDWS(GINDEX).LT.-90.0D0 .AND. PINDEX.LE.NPLVLS )
C
            IF( PFLWS(PINDEX) .GE. 0.0D0 )THEN
C
C              -------------------------------------------------
C              Data at this level are not missing; determine its
C              location relative to the height at which data are
C              required and act accordingly.
C              -------------------------------------------------
               IF( DABS(PFLHT(PINDEX) - GRIDHT(GINDEX) ).LE.0.1D0 )THEN
C                 USE the parameter at this level
                  GRIDWS(GINDEX) = PFLWS(PINDEX)
C
               ELSEIF( GRIDHT(GINDEX)  .GT.  PFLHT(PINDEX) )THEN
                  IF( PINDEX .LT. NPLVLS )THEN
C                    SAVE value for possible interpolation
                     VBELOW = PFLWS(PINDEX)
                     HTBELO = PFLHT(PINDEX)

                  ELSE   ! this is the top level
C                    PROFILE upward from this level        --- CALL XTRPWS
                     CALL XTRPWS ( PFLHT(PINDEX), PFLWS(PINDEX),
     &                             GRIDHT(GINDEX), GRIDWS(GINDEX) )
                  ENDIF
C
               ELSEIF( GRIDHT(GINDEX)  .LT.  PFLHT(PINDEX) )THEN
                  IF( VBELOW .GE. 0.0D0 )THEN
C                    INTERPOLATE between the two values    --- CALL NTRPWS
                     CALL NTRPWS ( HTBELO, VBELOW, PFLHT(PINDEX),
     &                             PFLWS(PINDEX), GRIDHT(GINDEX),
     &                             GRIDWS(GINDEX) )

                  ELSE   ! BELOW is missing
C                    PROFILE down from this level          --- CALL XTRPWS
                     CALL XTRPWS ( PFLHT(PINDEX), PFLWS(PINDEX),
     &                             GRIDHT(GINDEX), GRIDWS(GINDEX) )

                  ENDIF
C
               ELSE
C                 This section is for DEBUGging - the program should never
C                 reach this point
                  PRINT *, ' ---> ERROR: The search for data to'
                  PRINT *, '             construct the gridded profile'
                  PRINT *, '    of speed failed on ', KURDAT
C
               ENDIF
C
            ELSE
C
C              -------------------------------------------------------
C              The parameter at this level is missing - if this is not
C              the top level, continue the search; if it is the top
C              level, then make a computation.
C              -------------------------------------------------------
C
               IF( PINDEX .EQ. NPLVLS )THEN
                  IF( VBELOW  .GE.  0.0D0 )THEN
C                    PROFILE up from BELOW                 --- CALL XTRPWS
                     CALL XTRPWS ( HTBELO, VBELOW,
     &                             GRIDHT(GINDEX), GRIDWS(GINDEX) )

                  ELSE   ! there are no data
C                    PROFILE up from BELOW with UREF       --- CALL XTRPWS
                     CALL XTRPWS ( UREFHT, UREF,
     &                             GRIDHT(GINDEX), GRIDWS(GINDEX) )
                  ENDIF
C
               ELSE   ! this is not the top level, repeat loop
                  CONTINUE
C
               ENDIF
C
            ENDIF   ! parameter (not) missing at this level
C
C           ---------------------------------------------------------
C           Increment the observed profile counter if a value at this
C           grid level was not computed on this pass; continue
C           processing
C           ---------------------------------------------------------
C
            IF( (GRIDWS(GINDEX) .LT. 0.0D0)  .AND.
     &          (PINDEX .LT. NPLVLS) )THEN
               PINDEX = PINDEX + 1
            ENDIF
C
         END DO   ! Loop over observed data profile
C
C        ------------------------------------------------------------
C        Increment the gridded profile counter and repeat the process
C        starting with the observed value from the profile height as
C        defined by PINDEX
C        ------------------------------------------------------------
C

C        The wind speed at any gridded level cannot be less than 
C        UMINGR, a value defined in a PARAMETER  statement in 
C        MAIN1.INC, and taken to be 0.01 m/s for now

         GRIDWS(GINDEX) = MAX( UMINGR, GRIDWS(GINDEX) )

         GINDEX = GINDEX + 1
C
      END DO   ! Loop over gridded data profile
C
      RETURN
      END

      SUBROUTINE REFWS( PRFLHT, UTHEOR )
C=======================================================================
C                REFWS  Module of the AERMOD Dispersion Model
C
C   Purpose:     To estimate the theoretical wind speed at the
C                specified height from a single observation of wind
C                speed (first nonmissing value above 7*Z0M)
C                and the log-profile function with corrections
C                for stability.
C
C   Input:       Observed wind speed at the reference height (UREF)
C                Observed reference height (UREFHT)
C                Height at which data are needed (PRFLHT)
C                Monin-Obukhov length (OBULEN) as a stability parameter
C                Surface roughness length (SFCZ0)
C
C   Output:      Wind speed at the required level (UTHEOR)
C
C   Assumptions: All the input data exist (i.e., are nonmissing)
C
C   Subprograms:
C                UNSTU  -  algorithm to compute the wind speed at PRFLHT
C                           for an unstable atmosphere (OBULEN < 0.0)
C                STBLU  -  algorithm to compute the wind speed at PRFLHT
C                           for a stable atmosphere
C
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C                Jim Paumier, PES                    5 July 1995
C                  - pass the value of pi as an argument for the 
C                    unstable case
C
C   Reference(s): Addendum (dated 6/2/93) to the 8/6/92 Interface Model
C                 Coding Abstract
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: UNSTU, STBLU, PRFLHT, UTHEOR, VLDLB, PIVALU, 
     &           KVALU, UFACT, UFACT2

C
C
C---- Data dictionary
C
C     UNSTU    Wind speed returned from the theoretical profile for
C              unstable atmosphere
C     STBLU    Wind speed returned from the theoretical profile for
C              stable atmosphere
C     VLDLB    Lower bound height for which the computations are valid
C
C---- Data initializations
C
      MODNAM = ' REFWS'
      PIVALU = PI
      KVALU  = VONKAR
C
C
C     Compute the minimum valid height (VLDLB) for these computations.
C     ----------------------------------------------------------------
C
      VLDLB =  7.0D0 * SFCZ0
C
C     -------------------------------------------------------------
C     Check the location of the reference height and the height at
C     which data are required and profile accordingly.
C
C     The computation is stability dependent: check the stability
C     (the sign of the Monin-Obukhov length: negative for an
C     unstable amosphere, positive for a stable atmosphere).
C     -------------------------------------------------------------
C
      IF( (UREFHT .GT. VLDLB)  .AND.  (UREFHT .LE. ZI) )THEN
C
C        ----------------------------------------------------------
C        The reference height is above the valid lower bound height
C        and below the mixing height
C        ----------------------------------------------------------
C
         IF( (PRFLHT .GT. VLDLB)  .AND.  (PRFLHT .LE. ZI) )THEN
C
C           ---------------------------------------------------------
C           The required height is above the valid lower bound height
C           and below the mixing height - profile directly to the
C           required height
C           ---------------------------------------------------------
C
            IF( OBULEN .LT. 0.0D0 )THEN
               UTHEOR = UNSTU( USTAR,PRFLHT,OBULEN,SFCZ0,
     &                  PIVALU, KVALU )
            ELSE
               UTHEOR = STBLU( USTAR,PRFLHT,OBULEN,SFCZ0,KVALU )
            ENDIF
C
         ELSEIF( PRFLHT .LE. VLDLB )THEN
C
C           ----------------------------------------------------------
C           The required height is below the displacement height -
CRJP        profile only to the valid lower bound height and persist
CRJP        down to the required height
C           profile only to the valid lower bound height and linearly
C           interpolate down to the required height (assuming about a 
CRJP        zero wind speed at Z=0.
C           ----------------------------------------------------------
C
CRJP        Add UFACT

            UFACT = PRFLHT / VLDLB

            IF( OBULEN .LT. 0.0D0 )THEN
               UTHEOR = UNSTU( USTAR,VLDLB,OBULEN,SFCZ0,
     &                  PIVALU, KVALU ) * UFACT
            ELSE
               UTHEOR = STBLU( USTAR,VLDLB,OBULEN,
     &                  SFCZ0,KVALU ) * UFACT
            ENDIF
C
         ELSEIF( PRFLHT .GT. ZI )THEN
C
C           --------------------------------------------------------
C           The required height is above the mixing height - profile
C           only to the mixing height and persist to the required ht
C           --------------------------------------------------------
C
            IF( OBULEN .LT. 0.0D0 )THEN
               UTHEOR = UNSTU( USTAR,ZI,OBULEN,SFCZ0,
     &                  PIVALU, KVALU )
            ELSE
               UTHEOR = STBLU( USTAR,ZI,OBULEN,SFCZ0,KVALU )
            ENDIF
C
         ENDIF   ! required ht
C
      ELSEIF( UREFHT .GT. ZI )THEN
C
C        -----------------------------------------------
C        The reference height is above the mixing height
C        -----------------------------------------------
C

         IF( (PRFLHT .GT. VLDLB)  .AND.  (PRFLHT .LE. ZI) )THEN
C
C           ---------------------------------------------------------
C           The required height is above the valid lower bound height
C           and below the mixing height.
C           ---------------------------------------------------------
C
            IF( OBULEN .LT. 0.0D0 )THEN
               UTHEOR = UNSTU( USTAR,PRFLHT,OBULEN,SFCZ0,
     &                  PIVALU, KVALU )
            ELSE
               UTHEOR = STBLU( USTAR,PRFLHT,OBULEN,SFCZ0,KVALU )
            ENDIF
C
C
         ELSEIF( PRFLHT .LT. VLDLB )THEN
C
C           ---------------------------------------------------------
C           The required height is below the valid lower bound height
C           ---------------------------------------------------------
C
            IF( OBULEN .LT. 0.0D0 )THEN
               UTHEOR = UNSTU( USTAR,VLDLB,OBULEN,SFCZ0,
     &                  PIVALU, KVALU )
            ELSE
               UTHEOR = STBLU( USTAR,VLDLB,OBULEN,
     &                  SFCZ0, KVALU )
            ENDIF
C
         ELSEIF( PRFLHT .GT. ZI )THEN
C
C           --------------------------------------------------------
C           The required height is above the mixing height - use the
C           value from the reference height
C           --------------------------------------------------------
C
            IF( OBULEN .LT. 0.0D0 )THEN
               UTHEOR = UREF
            ELSE
               UTHEOR = UREF
            ENDIF
C
         ENDIF   ! required ht
      
      ELSEIF (UREFHT .LE. VLDLB) THEN
C        
         UFACT2 = UREFHT/VLDLB

         IF( (PRFLHT .GT. VLDLB)  .AND.  (PRFLHT .LE. ZI) )THEN
C
C           ---------------------------------------------------------
C           The required height is above the valid lower bound height
C           and below the mixing height - profile directly to the
C           required height
C           ---------------------------------------------------------
C
            IF( OBULEN .LT. 0.0D0 )THEN
               UTHEOR = UNSTU( USTAR,PRFLHT,OBULEN,SFCZ0,
     &                  PIVALU, KVALU )
            ELSE
               UTHEOR = STBLU( USTAR,PRFLHT,OBULEN,SFCZ0,KVALU )
            ENDIF
C
         ELSEIF( PRFLHT .LE. VLDLB )THEN
C
C           ----------------------------------------------------------
C           The required height is below the displacement height -
CRJP        profile only to the valid lower bound height and persist
CRJP        down to the required height
C           profile only to the valid lower bound height and linearly
C           interpolate down to the required height (assuming about a 
CRJP        zero wind speed at Z=0.
C           ----------------------------------------------------------
C
CRJP        Add UFACT

            UFACT = PRFLHT / VLDLB

            UTHEOR = UREF * UFACT / UFACT2

C
         ELSEIF( PRFLHT .GT. ZI )THEN
C
C           --------------------------------------------------------
C           The required height is above the mixing height - profile
C           only to the mixing height and persist to the required ht
C           --------------------------------------------------------
C
            IF( OBULEN .LT. 0.0D0 )THEN
               UTHEOR = UNSTU( USTAR,ZI,OBULEN,SFCZ0,
     &                         PIVALU, KVALU )
            ELSE
               UTHEOR = STBLU( USTAR,ZI,OBULEN,SFCZ0,KVALU )
            ENDIF
C
         ENDIF   ! required ht
C
      ENDIF   ! reference ht
C
C
      RETURN
      END
C
C
      DOUBLE PRECISION FUNCTION UNSTU( USTR, Z, OBLEN, Z0, VALPI, 
     &                                 VALK )
C=======================================================================
C                UNSTU  Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the wind speed at the specified height
C                for an unstable atmosphere
C
C   Input:       Friction velocity (USTR)
C                Height at which data are needed (Z)
C                Monin-Obukhov length (OBLEN) as a stability parameter
C                Surface roughness length (Z0)
C                Value of pi (3.14159) (VALPI)
C                von Karman constant (VALK = 0.4)
C
C   Output:      Wind speed at the required level (UNSTU)
C
C   Assumptions: All the input data exist (i.e., are nonmissing)
C
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C     12/07/94 -  Added the PSI(Z0/L) term to the theoretical wind speed
C     07/05/95 -  removed inclusion of MAIN1.INC; passing in value of pi
C     09/18/95 -  Corrected PSIM terms to include 2 rather than z in the
C                 denominator of the ALOG term.
C
C   Reference(s):  August 6, 1992 Model Coding Abstract and addenda from
C                  the AERMIC workgroup
C
C-----------------------------------------------------------------------
C
CC  X values in this subroutine changed to X0 to avoid conflict
CC   with the global variable--rfl 5/13/94
C
C---- Variable declarations
C
      IMPLICIT NONE

      DOUBLE PRECISION :: USTR, Z, OBLEN, Z0, X0, XZ0, PSIM, 
     &         PSIMZ0, VALPI, VALK
C
C
C---- Data dictionary
C
C     VALPI   = 3.14159...
C     VALK    = 0.4 (von Karman constant)
C     PSIM    Correction for stability at Z
C     PSIMZ0  Correction for stability at Z0
C     X0      Interim calculation
C
C---- Data initializations
C
C.......................................................................
C
      
      X0  = ( 1.0D0 - 16.0D0 * Z / OBLEN ) ** 0.25D0

Cjop  Compute term for computation of PSI(Z0/L)
      XZ0 = ( 1.0D0 - 16.0D0 * Z0/ OBLEN ) ** 0.25D0
C
      PSIM   =  2.0D0 * DLOG( (1.0D0 + X0) / 2.0D0 )    +
     &                  DLOG( (1.0D0 + X0*X0) / 2.0D0 ) -
     &          2.0D0 * DATAN( X0 ) + VALPI / 2.0D0

Cjop  Compute PSI(Z0/L)
      PSIMZ0 =  2.0D0 * DLOG( (1.0D0 + XZ0) / 2.0D0 )     +
     &                  DLOG( (1.0D0 + XZ0*XZ0) / 2.0D0 ) -
     &          2.0D0 * DATAN( XZ0 ) + VALPI / 2.0D0
C
      UNSTU = (USTR/VALK) * ( DLOG( Z / Z0 ) - PSIM + PSIMZ0 )
C
      RETURN
      END
C
C
      DOUBLE PRECISION FUNCTION STBLU( USTR, Z, OBLEN, Z0, VALK )
C=======================================================================
C                STBLU  Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the wind speed at the specified height
C                for the stable atmosphere
C
C   Input:       Friction velocity (USTR)
C                Height at which data are needed (Z)
C                Monin-Obukhov length (OBLEN) as a stability parameter
C                Surface roughness length (Z0)
C                von Karman constant (VALK = 0.4)
C
C   Output:      Wind speed at the required level (STBLU)
C
C   Assumptions: All the input data exist (i.e., are nonmissing)
C
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C     12/7/94 -  Added the PSI(Z0/L) term to the theoretical wind speed
C
C   Reference(s):  August 6, 1992 Model Coding Abstract and addenda from
C                  the AERMIC workgroup
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      IMPLICIT NONE

      DOUBLE PRECISION :: USTR, Z, OBLEN, Z0, PSIM, VALK, 
     &                    PSIMZ0
C
C
C---- Data dictionary
C
C     VALK    = 0.4 (von Karman constant)
C     PSIM    Correction for stability at Z
C     PSIMZ0  Correction for stability at Z0
C
C---- Data initializations
C
C.......................................................................
C
      PSIM   = -17.0D0 * ( 1.0D0 - DEXP( -0.29D0 * Z / OBLEN ) )
      
Cjop  Compute PSI(Z0/L)
      PSIMZ0 = -17.0D0 * ( 1.0D0 - DEXP( -0.29D0 * Z0 / OBLEN ) )

      STBLU = (USTR/VALK) * ( DLOG( Z / Z0 ) - PSIM + PSIMZ0 )
C
      RETURN
      END
C
C
      SUBROUTINE NTRPWS ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALOUT )
C=======================================================================
C                NTRPWS Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the wind speed at an intermediate level by
C                interpolating between two observed values.
C
C   Input:       Profile height below the level at which wind speed
C                   is required (HTBELO)
C                Wind speed at HTBELO
C                Profile height above the level at which wind speed
C                   is required (HTABOV)
C                Wind speed at HTABOV
C                Height at which data are required (REQDHT)
C
C   Output:      Wind speed at the required level (VALOUT)
C
C   Called by:
C
C   Assumptions: No missing data
C
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C      JOP    March 14, 1995 - modified the check on the ratio for a
C                              positive slope and observed values
C
C   Reference(s):  Revision dated 6/2/93 to the Interface Model Coding
C                  Abstract
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      IMPLICIT NONE

      DOUBLE PRECISION :: REFABV, REFBLW, RATIO, VALOUT, VABOVE, HTABOV,
     &                    VBELOW, REQDHT, HTBELO, REFREQ, VALINT, REFINT
C
C---- Data dictionary
C
C
C---- Data initializations
C
C
C.......................................................................
C---- The computation requires 3 estimates from the reference/theoretical
C     profile: one height above, one height below and from the level at
C     which the parameter is needed.  The ratio of the differences
C     [EST(requested ht) - EST(ht below)] / [EST(ht above) - EST(ht below)]
C     is applied to the difference between the observed values to obtain
C     interpolated value.
C
C     COMPUTE the value of the parameter from the reference profile
C     at the height below the requested height (REFBLW)    --- CALL REFWS
C
      CALL REFWS (  HTBELO, REFBLW )

C     COMPUTE the value of the parameter from the reference profile
C     at the height above the requested height (REFABV)    --- CALL REFWS
C
      CALL REFWS (  HTABOV, REFABV )
C     COMPUTE the value of the parameter from the reference profile
C     at the requested height (REFREQ)                     --- CALL REFWS
C
      CALL REFWS ( REQDHT, REFREQ )

C
C     Linearly interpolate to REQDHT from observed and reference profiles
      CALL GINTRP ( HTBELO,VBELOW, HTABOV, VABOVE, REQDHT,VALINT )
      CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
C     REFREQ is value from REFerence profile at REQuired height
C     REFINT is value from REFerence profile linearly INTerpolated to req ht
C     VALINT is the observed VALue linearly INTerpolated to required height

      RATIO  = REFREQ/REFINT
      VALOUT = RATIO * VALINT

      RETURN
      END

      SUBROUTINE XTRPWS ( PFLZ, PFLVAL, GRDZ, VALOUT )
C=======================================================================
C                XTRPWS Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the wind speed by extrapolating outside
C                (either above or below) the range of observed data
C                (i.e., there is at least one observation of wind
C                speed in the profile).
C
C   Input:       Profile height from which data are extrapolated (PFLZ)
C                Value at the height PFLZ (PFLVAL)
C                Gridded height at which wind speed is required (GRDZ)
C
C   Output:      Wind speed at the required level (VALOUT)
C
C   Called by:
C
C   Assumptions: No missing data
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C                <none>
C
C   Reference(s):  Revision dated 6/2/93 to the Interface Model Coding
C                  Abstract
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      IMPLICIT NONE

      DOUBLE PRECISION :: PFLZ, PFLVAL, GRDZ, VALOUT, VALOBS, VALGRD, 
     &                    RATIO
C
C---- Data dictionary
C     VALOBS   Value returned from the reference profile at PFLZ
C     VALGRD   Value returned from the reference profile at GRDZ
C
C---- Data initializations
C
C
C.......................................................................
C---- The computation requires estimates from the reference/theoretical
C     profile at the height of the highest(lowest) observation and at
C     the height the parameter is needed.  The ratio of these two values
C     is applied to the observed parameter at the highest(lowest)
C     observed height.
C
C     COMPUTE the value of the parameter from the reference profile
C     at the height of the highest(lowest) observed value --- CALL REFWS
C
      CALL REFWS ( PFLZ, VALOBS )
C
C     COMPUTE the value of the parameter from the reference profile
C     at the height where a value is required             --- CALL REFWS
C
      CALL REFWS ( GRDZ, VALGRD )
C
      RATIO  = VALGRD / VALOBS
C
      VALOUT = RATIO * PFLVAL
C
      RETURN
      END


      SUBROUTINE GRDWD
C=======================================================================
C                GRDWD module of the AERMOD Dispersion Model
C
C   Purpose:     To construct a profile of gridded values of wind
C                direction.
C
C   Input:       Parameter profile array
C                Number of levels in the profile
C                Height at which data are required
C
C   Output:      Array of values at the specified grid heights.
C
C   Assumptions:
C
C   Called by:
C
C   Programmer:  Jim Paumier                          30 Sept 1993
C                Pacific Environmental Services
C
C   Revision history:
C         12/5/94 - J. Paumier (Pacific Environmental Svcs., Inc)
C                 - changed the tolerance for a gridded profile height
C                   to be within 0.1 m of an observed profile height
C                   rather than 0.5 m
C
C                <none>
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER          :: PINDEX, GINDEX
      DOUBLE PRECISION :: VBELOW, HTBELO
C
C---- Data definitions
C        PINDEX    Array index for the profile of observed values
C        GINDEX    Array index for the profile of gridded values
C        VBELOW    Value from the observed data profile that is
C                  below the the gridded level
C
C
C---- Data initializations
C
      MODNAM = 'GRDWD '
C
      GINDEX = 1
      PINDEX = 1
      VBELOW = -999.0D0
C
C     ------------------------------------------------------------------
C     Loop over each grid level until a value is computed for each level
C     where a value is required OR the number of levels in the gridded
C     profile exceeds the maximum number
C     NOTE: There are 3 options for turning the wind with height.
C     ------------------------------------------------------------------

      DO WHILE( GINDEX .LE. MXGLVL )

C        -------------------------------------------
C        Now begin looping over the observed profile
C        -------------------------------------------
C
         DO WHILE( GRIDWD(GINDEX).LT.-90.0D0 .AND. PINDEX.LE.NPLVLS )
C
            IF( PFLWD(PINDEX) .GE. 0.0D0 )THEN
C
C              ----------------------------------------------------------
C              Data at this level are not missing; determine its location
C              relative to the height at which data are required and act
C              accordingly.
C              ----------------------------------------------------------
               IF( DABS(PFLHT(PINDEX)-GRIDHT(GINDEX)) .LE. 0.1D0 )THEN
C                 USE the parameter at this level

                  IF (PFLWD(PINDEX) .GT. 360.0D0) THEN
                      PFLWD(PINDEX) = PFLWD(PINDEX) - 360.0D0
                  ELSE IF (PFLWD(PINDEX) .LE. 0.0D0) THEN
                      PFLWD(PINDEX) = PFLWD(PINDEX) + 360.0D0
                  END IF

                  GRIDWD(GINDEX) = PFLWD(PINDEX)
C
               ELSEIF( GRIDHT(GINDEX)  .GT.  PFLHT(PINDEX) )THEN
                  IF( PINDEX .LT. NPLVLS )THEN
C                    SAVE value for possible interpolation
                     VBELOW = PFLWD(PINDEX)
                     HTBELO = PFLHT(PINDEX)

                  ELSE   ! this is the top level
C                    PROFILE upward from this level        --- CALL XTRPWD
C JAT 06/22/21 D065 REMOVE PFLHT AND GRIDHT AS UNUSED INPUT ARGUMENTS
c                     CALL XTRPWD ( PFLHT(PINDEX), PFLWD(PINDEX),
c     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                      CALL XTRPWD (  PFLWD(PINDEX),GRIDWD(GINDEX) )
                  ENDIF
C
               ELSEIF( GRIDHT(GINDEX)  .LT.  PFLHT(PINDEX) )THEN
                  IF( VBELOW .GE. 0.0D0 )THEN
C                    INTERPOLATE between the two values    --- CALL NTRPWD
                     CALL NTRPWD ( HTBELO, VBELOW, PFLHT(PINDEX),
     &                             PFLWD(PINDEX), GRIDHT(GINDEX),
     &                             GRIDWD(GINDEX) )

                  ELSE   ! BELOW is missing
C                    PROFILE down from this level          --- CALL XTRPWD
C JAT 06/22/21 D065 REMOVE PFLHT AND GRIDHT AS UNUSED INPUT ARGUMENTS
c                     CALL XTRPWD ( PFLHT(PINDEX), PFLWD(PINDEX),
c     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                     CALL XTRPWD ( PFLWD(PINDEX),GRIDWD(GINDEX) )
                  ENDIF
C
               ELSE
C                 This section is for DEBUGging - the program should never
C                 reach this point
                  PRINT *, ' ---> ERROR: The search for data to'
                  PRINT *, '             construct the gridded profile'
                  PRINT *, '      of WDIR failed on ', KURDAT
C
               ENDIF
C
            ELSE
C
C              -------------------------------------------------------
C              The parameter at this level is missing - if this is not
C              the top level, continue the search; if it is the top
C              level, then make a computation.
C              -------------------------------------------------------
C
               IF( PINDEX .EQ. NPLVLS )THEN
                  IF( VBELOW .GE. 0.0D0 )THEN
C                    PROFILE up from BELOW                 --- CALL XTRPWD
c JAT 06/22/21 REMOVE HTBELO AND GRIDHT AS UNUSED INPUT ARGUMENTS
C                     CALL XTRPWD ( HTBELO, VBELOW,
C     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                      CALL XTRPWD ( VBELOW, GRIDWD(GINDEX) )

                  ELSE   ! there are no data
C                    PROFILE up from BELOW with WDREF      --- CALL XTRPWD
C JAT 06/22/21 REMOVE UREFHT AND GRIDHT AS UNUSED INPUT ARGUMENTS
c                     CALL XTRPWD ( UREFHT, WDREF,
c     &                             GRIDHT(GINDEX), GRIDWD(GINDEX) )
                     CALL XTRPWD ( WDREF, GRIDWD(GINDEX) )

                  ENDIF
C
               ELSE   ! this is not the top level, repeat loop
                  CONTINUE
C
               ENDIF
C
            ENDIF   ! parameter (not) missing at this level
C
C           ---------------------------------------------------------
C           Increment the observed profile counter if a value at this
C           grid level was not computed on this pass
C           ---------------------------------------------------------
C
            IF( (GRIDWD(GINDEX) .LT. 0.0D0)  .AND.
     &          (PINDEX .LT. NPLVLS) )THEN
               PINDEX = PINDEX + 1
            ENDIF
C
         END DO   ! Loop over observed data profile
C
C        ------------------------------------------------------------
C        Increment the gridded profile counter and repeat the process
C        starting with the observed value from the profile height as
C        defined by PINDEX
C        ------------------------------------------------------------
C
         GINDEX = GINDEX + 1

      END DO   ! Loop over gridded data profile
C
      RETURN
      END

      SUBROUTINE REFWD1( WDREQD )
C=======================================================================
C                REFWD1 Module of the AERMOD Dispersion Model
C
C   Purpose:     To estimate the 'theoretical' wind direction at the
C                specified height from a single observation of the
C                direction (first level of nonmissing winds above
C                7*Z0).
C
C   Input:       Wind direction at reference height (WDREF (in COMMON))
C
C   Output:      Wind direction at the required height (WDREQD)
C
C   Assumptions: The wind does not turn with height (as selected by
C                the user), so the theoretical wind direction is
C                simply the value at the reference height.
C                The calling programs take care of properly
C                interpreting the results.
C
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C                <none>
C
C   Reference(s): Addendum (dated 6/2/93) to the 8/6/92 Interface Model
C                 Coding Abstract
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      DOUBLE PRECISION :: WDREQD

C
C---- Data dictionary
C
C---- Data initializations
C
C
      WDREQD = WDREF
C
      RETURN
      END

      SUBROUTINE NTRPWD ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALOUT )
C=======================================================================
C                NTRPWD Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the wind direction at an intermediate level
C                by interpolating between two observed values.
C
C   Input:
C
C
C
C
C   Output:      Value of the parameter at the required level
C
C   Called by:
C
C   Assumptions:
C
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C      RWB    August 26, 1996 - modified to use local variable for
C                               VALABV (value above) rather than
C                               VABOVE from argument list, since
C                               the value may be modified by the
C                               subroutine.
C
C      JOP    March 14, 1995  - modified the check on the ratio for a
C                               positive slope and observed values
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE

      DOUBLE PRECISION :: HTBELO, VBELOW, HTABOV, VABOVE, REQDHT,VALOUT,
     &        VALABV, REFABV, REFBLW, RATIO, REFREQ, VALINT, REFINT
C
C---- Data dictionary
C
C
C---- Data initializations
      VALABV = VABOVE
C
C
C.......................................................................
C---- The computation requires 3 estimates from the reference/theoretical
C     profile: one height above, one height below and from the level at
C     which the parameter is needed.  The ratio of the differences
C     [EST(requested ht) - EST(ht below)] / [EST(ht above) - EST(ht below)]
C     is applied to the difference between the observed values to obtain
C     interpolated value.
C
C     COMPUTE the value of the parameter from the reference profile
C     at the height below the requested height (REFBLW)   --- CALL REFWD1

      CALL REFWD1 ( REFBLW )

C     COMPUTE the value of the parameter from the reference profile
C     at the height above the requested height (REFABV)   --- CALL REFWD1

      CALL REFWD1 ( REFABV )

C     COMPUTE the value of the parameter from the reference profile
C     at the requested height (REFREQ)                    --- CALL REFWD1

      CALL REFWD1 ( REFREQ )

      IF( (VALABV-VBELOW) .LT. -180.0D0) THEN
         VALABV = VALABV + 360.0D0
      ELSE IF( (VALABV-VBELOW) .GT. 180.0D0) THEN
         VALABV = VALABV - 360.0D0
      END IF

C     Linearly interpolate to REQDHT from observed and reference profiles
      CALL GINTRP ( HTBELO,VBELOW, HTABOV, VALABV, REQDHT,VALINT )
      CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
C     REFREQ is value from REFerence profile at REQuired height
C     REFINT is value from REFerence profile linearly INTerpolated to req ht
C     VALINT is the observed VALue linearly INTerpolated to required height
      RATIO  = REFREQ/REFINT
      VALOUT = RATIO * VALINT

      IF (VALOUT .GT. 360.0D0) THEN
         VALOUT = VALOUT - 360.0D0
      ELSE IF (VALOUT .LE. 0.0D0) THEN
         VALOUT = VALOUT + 360.0D0
      END IF

C
      RETURN
      END

C JAT 06/22/21 D065
C REMOVE PFLZ AND GRDZ AS UNUSED INPUT ARGUMENTS
C      SUBROUTINE XTRPWD ( PFLZ, PFLVAL, GRDZ, VALOUT )
      SUBROUTINE XTRPWD (  PFLVAL, VALOUT )
C=======================================================================
C                XTRPWD Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the wind direction at the required height
C                by extrapolating outside (either above or below)
C                the range of observed data (i.e., there is at least
C                one observation of the wind direction in the profile).
C
C   Input:
C
C
C
C
C   Output:      Value of the parameter at the required level
C
C   Called by:
C
C   Assumptions:
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C                <none>
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
C JAT 06/22/21 D065
C REMOVE PFLZ AND GRDZ AS UNUSED VARIABLES
c      DOUBLE PRECISION :: PFLZ, PFLVAL, GRDZ, VALOUT, RATIO, VALOBS,
      DOUBLE PRECISION :: PFLVAL, VALOUT, RATIO, VALOBS, 
     &                    VALGRD
C
C---- Data dictionary
C
C     Note that PFLZ and GRDZ are no longer used since reference WD
C     profile is constant with height.
C
C---- Data initializations
C
C
C.......................................................................
C---- The computation requires estimates from the reference/theoretical
C     profile at the height of the highest(lowest) observation and at
C     the height the parameter is needed.  The ratio of these two values
C     is applied to the observed parameter at the highest(lowest)
C     observed height.
C
C     COMPUTE the value of the parameter from the reference profile
C     at height of the highest(lowest) observed value  --- CALL REFWD1

      CALL REFWD1 ( VALOBS )

C     COMPUTE the value of the parameter from the reference profile
C     at the height where a value is required          --- CALL REFWD1
C

      CALL REFWD1 ( VALGRD )

      RATIO  = VALGRD / VALOBS
C
      VALOUT = RATIO * PFLVAL

      IF (VALOUT .GT. 360.0D0) THEN
         VALOUT = VALOUT - 360.0D0
      ELSE IF (VALOUT .LE. 0.0D0) THEN
         VALOUT = VALOUT + 360.0D0
      END IF

C
      RETURN
      END
