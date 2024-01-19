      SUBROUTINE COMPTG ()
C=======================================================================
C                COMPTG module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the vertical potential temperature gradient
C                from a profile of observed temperatures.
C
C   Input:       Array of temperatures in profile
C                Array of heights in the profile
C
C   Output:      Array of potential temperature gradients.
C
C   Assumptions: Subroutine is called even if there is only 1
C                level of observed temperatures in the profile - the
C                logic takes care of this situation
C
C   Called by:   METEXT
C
C   Programmer:  Jim Paumier (PES, Inc.)              30 Sept 1993
C
C   Revision history:
C         12/5/94 - J. Paumier (PES, Inc.)
C                 - changed the tolerance for a gridded profile height
C                   to be within 0.1 m of an observed profile height
C                   rather than 0.5 m
C         07/5/95 - J. Paumier (PES, Inc.)
C                 - the minimum value for the observed potential
C                   temperature gradient in stable atmosphere is
C                   0.002 K/m (value is set in SPTGMN in a parameter
C                   statement in MAIN1.INC)
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER   NDXBLW, NDXABV, NLVL

      MODNAM = 'COMPTG'
      PATH   = 'MX'

C---- Definitions
C
C     NDXBLW    Index of an observed profile level below
C     NDXABV    Index of an observed profile level above

C---- Variable initializations

      NDXBLW = 1
      NDXABV = 2
      NTGLVL = 0

C     Loop through the levels, searching for two levels of nonmissing
C     temperature data.  The constant GOVRCP is the conversion from
C     temperature gradient to potential temperature gradient.

      DO WHILE( NDXABV .LE. NPLVLS )

         IF( PFLTA(NDXBLW) .GT. 0.0D0 )THEN

            IF( PFLTA(NDXABV) .GT. 0.0D0 )THEN

               NTGLVL = NTGLVL + 1
               PFLTG(NTGLVL) = (PFLTA(NDXABV) - PFLTA(NDXBLW)) /
     &                         (PFLHT(NDXABV) - PFLHT(NDXBLW)) + GOVRCP
               PFLTGZ(NTGLVL) = (PFLHT(NDXABV) + PFLHT(NDXBLW)) / 2.0D0
               NDXBLW = NDXABV
               NDXABV = NDXABV + 1

            ELSE
               NDXABV = NDXABV + 1

            ENDIF

         ELSE
            NDXABV = NDXABV + 1
            NDXBLW = NDXBLW + 1

         ENDIF

      ENDDO

C     For a stable atmosphere, check the observation and do not let it
C     be less than a minimum value, defined by SPTGMN = 0.002 in MAIN1.INC
      IF( STABLE )THEN
         DO NLVL = 1, NTGLVL
            PFLTG(NLVL) = MAX( SPTGMN, PFLTG(NLVL) )
         END DO
      ENDIF

C     For the structure in GRDPTG to be the same as in the other
C     profiling modules, the number of levels of data must be
C     at least one level of data, whether it is missing or not

      IF( NTGLVL .EQ. 0 )THEN
         NTGLVL = 1
      ENDIF

      RETURN
      END


      SUBROUTINE TGINIT ()
C=======================================================================
C                TGINIT module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the temperature scaling parameter and
C                gradient at 5 m for the stable atmosphere
C
C   Input:       Friction velocity, Obukhov length, ambient temperature,
C                surface roughness length
C
C   Output:      THETA_STAR and dTHETA/dZ at TREFHT
C
C   Assumptions:
C
C   Called by:   GRDPTG
C
C   Programmer:  Jim Paumier (PES, Inc.)              30 Sept 1993
C
C   Revision history:
C     JOP  March 14, 1995  The initial gradient is now stored in
C                          TG4PFL rather than the first level of
C                          profiled gradients
C
C   References:  Model Coding Abstract for the Met Interface dated
C                August 6, 1992 and all subsequent addenda/corrigenda
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION, PARAMETER :: TGMINHT = 2.0D0, TGMAXHT = 100.0D0
      DOUBLE PRECISION :: THSTAR0, THSTARN, NCLOUD8, USTARMax
      INTEGER :: LVL
      DOUBLE PRECISION :: REFLVL
C Unused:      DOUBLE PRECISION :: USTAR0, THSTR1, USTAR1

      MODNAM = 'TGINIT'
      PATH   = 'MX'

C---- Variable initializations


C --- The temperature gradient computations are made only for a stable atmosphere

      IF( STABLE )THEN

C ---    First check for observed temperature profile to use in 
C        calculating THSTAR, and determine the reference level 
C        for computing a temperature gradient for profiling

         REFLVL = -99.0D0
         LVL    =  1
         DO WHILE( REFLVL .LT. 0.0D0 .AND.  LVL .LE. NTGLVL )

            IF( PFLTGZ(LVL) .GT. SFCZ0 )THEN
               REFLVL = PFLTGZ(LVL)

            ELSE
               LVL = LVL + 1

            ENDIF

         ENDDO

C ---    Make initial calculation of THSTAR based on observed 
C        temperature profile, if available, or with "standard"
C        approach
         IF( REFLVL .GT. 0.0D0 .AND. REFLVL .LE. 100.0D0 )THEN
            THSTAR = PFLTG(LVL) * VONKAR * PFLTGZ(LVL) /
     &               ( 1.0D0 + 5.0D0 * PFLTGZ(LVL) / OBULEN )

         ELSE
C ---       Calculate THSTAR based on USTAR, OBULEN, and TA since
C           observed temperature profile is not available
            THSTAR = USTAR**2 / ( G * VONKAR * OBULEN / TA )

         ENDIF

C ---    Make adjustments to THSTAR calculations if ADJ_U* option is used or
C        if no observed temperature profile is available (i.e., REFLVL < 0.0)
C
         IF( L_AdjUstar .AND. L_BULKRN .AND. REFLVL .LT. 0.0D0 )THEN
C ---       Use Luhar/Raynor approach (2009, BLM v132) for ADJ_U* w/ BULKRN, 
C           unless observed temperature profile is available

            USTARMax = 0.6D0 * ( UREFHT/
     &               DLOG((UREFHT-5.*SFCZ0)/SFCZ0) )**0.333333D0

            NCLOUD8 = DBLE(NINT(DBLE(NCLOUD)*0.8D0))

            THSTAR0 = 1.4D0* (0.005D0/(TAN(0.17D0*(2.0D0*NCLOUD8 + 
     &                                                1.0D0))) + 0.05D0)

            THSTARN = THSTAR0*(1.0D0 - 
     &                    ( (2.0D0*USTAR/USTARMAX)-1.0D0 )**2.)
 
            THSTAR = THSTARN

         ELSEIF( L_AdjUstar .AND. .NOT. L_BULKRN 
     &                      .AND. REFLVL .LT. 0.0D0 )THEN
C ---       Use constant THSTAR = 0.08 per Venkatram paper (2011, BLM v138), 
C           unless observed temperature profile is available
            THSTAR = 0.08D0

         ELSEIF( REFLVL .LT. 0.0D0 )THEN
C ---       No ADJ_U* option and no observed temperature profile;
C ---       Apply "standard" approach for THSTAR
            THSTAR = USTAR**2 / ( G * VONKAR * OBULEN / TA )

         ENDIF

         CONTINUE

C        Compute DTHETA/dZ at TREFHT

         IF( L_AdjUstar )THEN
            TG4PFL = ( THSTAR / ( VONKAR * TGMINHT ) ) *
     &               ( 0.74D0 + 4.7D0 * TGMINHT / OBULEN )
            TG4XTR = ( THSTAR / ( VONKAR * TGMAXHT ) ) *
     &               ( 0.74D0 + 4.7D0 * TGMAXHT / OBULEN )

         ELSE
            TG4PFL = ( THSTAR / ( VONKAR * TGMINHT ) ) *
     &               ( 1.0D0 + 5.0D0 * TGMINHT / OBULEN )
            TG4XTR = ( THSTAR / ( VONKAR * TGMAXHT ) ) *
     &               ( 1.0D0 + 5.0D0 * TGMAXHT / OBULEN )
         ENDIF

C ---    Check for TG4PFL out-of-range; issue warning if lapse rate > 0.5 K/m
         IF( TG4PFL .GT. 0.5D0 )THEN
            WRITE(DUMMY,'("TG4PFL=",F5.3)') TG4PFL
            CALL ERRHDL(PATH,MODNAM,'W','479',DUMMY)
         ENDIF

      ELSE

C        For the unstable case
         THSTAR = -9.0D0
         TG4PFL = XVAL

      ENDIF

      RETURN
      END

      SUBROUTINE GRDPTG ()
C=======================================================================
C                GRDPTG module of the AERMOD Dispersion Model
C
C   Purpose:     To construct a profile of gridded values of the
C                vertical potential temperature gradient
C
C   Input:       Profile array of observed data (PFLTG)
C                Number of levels in the profile (NTGLVL)
C                Gridded heights at which data are required (GRIDHT)
C
C   Output:      Potential temperature gradient at the grid heights
C                (GRIDTG)
C
C   Assumptions: No value of the potential temperature gradient
C                (observed or computed) is less than -50.0
C
C   Called by:   METEXT
C
C   Programmer:  Jim Paumier (PES, Inc.)              30 Sept 1993
C
C   Revision history:
C         12/5/94  - J. Paumier (PES, Inc)
C                  - changed the tolerance for a gridded profile height
C                    to be within 0.1 m of an observed profile height
C                    rather than 0.5 m
C         07/07/95 - J. Paumier (PES,Inc)
C                  - moved the check for a minimum value in stable
C                    layers to outside the initial DO WHILE ... ENDDO
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER     PINDEX, GINDEX, NDX
      DOUBLE PRECISION        VBELOW, HTBELO
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
C
      MODNAM = 'GRDPTG'
      GINDEX = 1
      PINDEX = 1
      VBELOW = -999.0D0
C
C     ------------------------------------------------------------------
C     Loop over each grid level until a value is computed for each level
C     where a value is required OR the number of levels in the gridded
C     profile exceeds the maximum number
C     ------------------------------------------------------------------
C
      DO WHILE( GINDEX .LE. MXGLVL )
C
C       -------------------------------------------
C       Now begin looping over the observed profile
C       -------------------------------------------

C       The 'blending' of the reference profile with observations now
C       applies only to the stable atmosphere

        IF( STABLE )THEN
C
         DO WHILE( GRIDTG(GINDEX).LT.-90.0D0 .AND. PINDEX.LE.NTGLVL )
C
            IF( PFLTG(PINDEX) .GE. -50.0D0 )THEN
C
C              -------------------------------------------------
C              Data at this level are not missing; determine its
C              location relative to the height at which data are
C              required and act accordingly.
C              -------------------------------------------------
               IF( DABS(PFLTGZ(PINDEX)-GRIDHT(GINDEX)) .LE. 0.1D0 )THEN
C                 USE the parameter at this level
                  GRIDTG(GINDEX) = PFLTG(PINDEX)
C
               ELSEIF( GRIDHT(GINDEX)  .GT.  PFLTGZ(PINDEX) )THEN
                  IF( PINDEX .LT. NTGLVL )THEN
C                    SAVE value for possible interpolation
                     VBELOW = PFLTG(PINDEX)
                     HTBELO = PFLTGZ(PINDEX)

                  ELSE   ! this is the top level
C                    PROFILE upward from this level        --- CALL XTRPTG
                     CALL XTRPTG ( PFLTGZ(PINDEX), PFLTG(PINDEX),
     &                             GRIDHT(GINDEX), GRIDTG(GINDEX) )
                  ENDIF
C
               ELSEIF( GRIDHT(GINDEX)  .LT.  PFLTGZ(PINDEX) )THEN
                  IF( VBELOW .GE. -50.0D0 )THEN
C                    INTERPOLATE between the two values    --- CALL NTRPTG
                     CALL NTRPTG ( HTBELO, VBELOW, PFLTGZ(PINDEX),
     &                             PFLTG(PINDEX), GRIDHT(GINDEX),
     &                             GRIDTG(GINDEX) )

                  ELSE   ! BELOW is missing
C                    PROFILE down from this level          --- CALL XTRPDN

                     CALL XTRPDN ( GRIDHT(GINDEX), GRIDTG(GINDEX) )

                  ENDIF
C
               ELSE
C                 This section is for DEBUGging - the program should never
C                 reach this point
                  PRINT *, ' ---> ERROR: The search for data to'
                  PRINT *, '             construct the gridded profile'
                  PRINT *, '             failed on ', KURDAT
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
               IF( PINDEX .EQ. NTGLVL )THEN
                  IF( VBELOW  .GE.  -50.0D0 )THEN
C                    PROFILE up from BELOW                 --- CALL XTRPTG
                     CALL XTRPTG ( PFLTGZ(PINDEX), PFLTG(PINDEX),
     &                             GRIDHT(GINDEX), GRIDTG(GINDEX) )

                  ELSE   ! there are no data
C                    COMPUTE value: full parameterization  --- CALL REFPTG
                     CALL REFPTG ( GRIDHT(GINDEX), GRIDTG(GINDEX) )
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
C           grid level was not computed on this pass and continue
C           processing
C           ---------------------------------------------------------
C
            IF( (GRIDTG(GINDEX) .LT. -50.0D0)  .AND.
     &          (PINDEX .LT. NTGLVL) )THEN
               PINDEX = PINDEX + 1
            ENDIF
C
          END DO   ! Loop over observed data profile

         ELSEIF( UNSTAB )THEN

            CALL REFPTG( GRIDHT(GINDEX), GRIDTG(GINDEX) )

         ENDIF
C        ------------------------------------------------------------
C        Increment the gridded profile counter and repeat the process
C        starting with the observed value from the profile height as
C        defined by PINDEX
C        ------------------------------------------------------------
C
         GINDEX = GINDEX + 1
C
      END DO   ! Loop over gridded data profile
C
C
C        ------------------------------------------------------------
C        Apply lower limit of SPTGMN (=0.002 K/m in MAIN1.INC) to
C        lapse rate for stable layers.
C        ------------------------------------------------------------
C
      DO NDX = 1,MXGLVL
         IF( STABLE .OR. (UNSTAB .AND. GRIDHT(NDX).GT.ZI) )THEN
            GRIDTG(NDX) = MAX( SPTGMN, GRIDTG(NDX) )
         ENDIF
      END DO

      RETURN
      END


      SUBROUTINE REFPTG ( HTINP, VALUE )
C=======================================================================
C                REFPTG Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the value of the potential temperature
C                gradient from the surface friction velocity, Monin-
C                Obukhov length and roughness length in the stable
C                boundary layer; in the unstable boundary layer, the
C                reference gradient is constant in the various layers.
C
C   Input:       Stability, mixing height, potential temperature
C                gradient between near the surface (TG4PFL),
C                the height of the computation for TG4PFL (TREFHT)
C
C   Output:      Potential temperature gradient (VALUE) at the
C                required height (HEIGHT)
C
C   Assumptions: All parameters required to make the computation are
C                not missing.
C
C   Programmer:  Jim Paumier (PES, Inc.)             30 September 1993
C
C   Revision history:
C                Roger Brode, PES                     22 January 1998
C                Modified to use Stull for entire profile above TREFHT.
C
C                Roger Brode, PES                     19 November 1996
C                Modified to incorporate guidance from AERMIC for
C                second round of Beta testing.
C
C                Jim Paumier, PES                     7 July 1995
C                In keeping with the modification made by Russ Lee (EPA)
C                during testing, HEIGHT in the aguement list is replaced
C                by HTINP and the statement HEIGHT=HTINP is added.  This
C                modification prevents some unintended changes from
C                happening to GRIDHT(1).
C
C                Profiling below 10 m or the lowest level of observed
C                gradients uses Businger's surface layer similarity.
C                 
C
C   Reference(s): Model Coding Abstract for the Met Interface, dated
C                 August 6, 1992 and and subsequent addenda
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION, PARAMETER :: HDELTH = 100.0D0
      DOUBLE PRECISION               HTINP, HEIGHT, VALUE, EXPARG

      MODNAM = 'REFPTG'

C
C---- Data dictionary
C          VALUE   = potential temperature gradient at requested level
C          XVAL    = potential temperature gradient in the mixing layer;
C                    assigned as 0.0 in MODULE MAIN1
C          EFOLDH  = 0.44 and is defined as a parameter in MODULE MAIN1
C
C.......................................................................
C---- Check the stability and then the location of the height relative 
C     to the mixing height.

      HEIGHT = HTINP
      IF( UNSTAB )THEN
C
         IF( HEIGHT  .LE.  ZI )THEN
C ---       Assign unstable lapse rate of 0.0 for height .LE. ZI;
C           value of XVAL is assigned 0.0 in MODULE MAIN1
            VALUE  = XVAL

         ELSE IF( HEIGHT .LE. ZI+500.0D0 )THEN
            VALUE  = VPTGZI

         ELSE
            VALUE  = 0.005D0

         ENDIF
C
      ELSE   ! stable atmosphere
C        THETA_STAR and TG4PFL (dTheta/dZ at TREFHT) are computed
C        in TGINIT
C
         IF (HEIGHT .LE. 2.0D0) THEN

            VALUE = TG4PFL

         ELSE IF (HEIGHT .LE. 100.0D0) THEN

            IF( L_AdjUstar )THEN
               VALUE = ( THSTAR / ( VONKAR * HEIGHT ) ) *
     &                 ( 0.74D0 + 4.7D0 * HEIGHT / OBULEN )
            ELSE
               VALUE = ( THSTAR / ( VONKAR * HEIGHT ) ) *
     &                 ( 1.00D0 + 5.0D0 * HEIGHT / OBULEN )
            ENDIF

         ELSE
C           COMPUTE gradient from gradient at TREFHT
            EXPARG =  -1.0D0*(HEIGHT-100.0D0) / (EFOLDH*MAX(HDELTH,ZI) )
            IF (EXPARG .GT. EXPLIM) THEN
               VALUE = TG4XTR * DEXP( EXPARG )
            ELSE
               VALUE = 0.0D0
            END IF

         ENDIF

C        Apply minimum value of 0.002 to all levels of stable profile.
         VALUE = MAX( VALUE, SPTGMN )

      ENDIF

      RETURN
      END


      SUBROUTINE NTRPTG ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
C=======================================================================
C                NTRPTG Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the potential temperature gradient at an
C                intermediate level by interpolating between two
C                observed values.
C
C   Input:       Profile heights and values above and below the height
C                at which the value is required
C
C   Output:      Potential temperature gradient at the required level
C
C   Called by:   REFPTG
C
C   Assumptions:
C
C
C   Programmer:  Jim Paumier (PES, Inc.)            30 September 1993
C
C   Revision history:
C      JOP    March 14, 1995 - modified the check on the ratio for a
C                              positive slope and observed values
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      IMPLICIT NONE
      DOUBLE PRECISION :: HTBELO, VBELOW, HTABOV, VABOVE, REQDHT, VALUE,
     &        REFABV, REFBLW, RATIO, REFREQ, VALINT, REFINT

C
C---- Data dictionary
C
C      REFABV  = Reference profile value above the height at which a
C                value is required (HTABOV)
C      REFBLW  = Reference profile value bbelow the height at which a
C                value is required (HTBELO)
C      REFREQ  = Reference profile value at the height at which a
C                value is required (REQDHT)
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
C     Compute the reference profile value at the height below the
C     requested height                                     --- CALL REFPTG
      CALL REFPTG ( HTBELO, REFBLW )
C
C     Compute the reference profile value at the height above the
C     requested height                                     --- CALL REFPTG
      CALL REFPTG ( HTABOV, REFABV )
C
C     Compute the reference profile value at the requested height
C                                                          --- CALL REFPTG
      CALL REFPTG ( REQDHT, REFREQ )
C
      IF( DABS(REFABV - REFBLW) .GT. 0.0001D0 )THEN
C
C        Linearly interpolate to REQDHT from observed and reference profiles
         CALL GINTRP ( HTBELO,VBELOW, HTABOV, VABOVE, REQDHT,VALINT )
         CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
C        REFREQ is value from REFerence profile at REQuired height
C        REFINT is value from REFerence profile linearly INTerpolated to req ht
C        VALINT is the observed VALue linearly INTerpolated to required height
         RATIO = REFREQ/REFINT
         VALUE = RATIO * VALINT
      ELSE
C        INTERPOLATE between VABOVE and VBELOW
         CALL GINTRP ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
C
      ENDIF

      RETURN
      END


      SUBROUTINE XTRPTG ( PFLZ, PFLVAL, GRDZ, VALUE )
C=======================================================================
C                XTRPTG Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the potential temperature gradient
C                by extrapolating outside (either above or below)
C                the range of observed data (i.e., there is at least
C                one observation of the gradient in the profile).
C
C   Input:       Profile height and value below(above) the height (PFLZ
C                and PFLVAL ) at which the value is required and the
C                height at which the value is required (GRDZ)
C
C   Output:      Potential temperature gradient (VALUE) at the
C                required level (GRDZ)
C
C   Called by:   REFPTG
C
C   Assumptions:
C
C   Programmer:  Jim Paumier (PES, Inc.)            30 September 1993
C
C   Revision history:
C                Modified to "restart" the exponential term when
C                extrapolating above the highest Dtheta/Dz measurement
C                level if that level is greater than 100m.
C                R. Brode, PES, Inc. - 8/9/01
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION, PARAMETER :: HDELTH = 100.0D0
      DOUBLE PRECISION  VALOBS, VALGRD, PFLZ, GRDZ, VALUE, PFLVAL, RATIO

      MODNAM = 'XTRPTG'
C
C---- Data dictionary

C     VALOBS  = Reference value at the measurement height (PFLZ)
C     VALGRD  = Reference value at the gridded profile height (GRDZ)

C---- Data initializations
C
C.......................................................................
C---- The computation requires estimates from the reference/theoretical
C     profile at the height of the highest(lowest) observation and at
C     the height the parameter is needed.  The ratio of these two values
C     is applied to the observed parameter at the highest(lowest)
C     observed height.
C
C     Compute the reference profile value at the height of the highest
C     (lowest) observed value                              --- CALL REFPTG

      CALL REFPTG ( PFLZ, VALOBS )

C     Compute the reference profile value at the height where a value
C     is required                                          --- CALL REFPTG

      CALL REFPTG ( GRDZ, VALGRD )

C     The potential temperature gradient is the only profile parameter
C     that can take on a negative value (and in the initial programming
C     of AERMOD, this is in the well-mixed layer for an unstable
C     atmosphere); therefore, if VALOBS is zero, then RATIO = 1.0.

      IF( DABS( VALOBS ) .LT. 0.0001D0 ) THEN
         RATIO = 1.0D0
      ELSE
         RATIO = VALGRD / VALOBS
      ENDIF
C
      IF (PFLZ .LE. 100.0D0) THEN
         VALUE = RATIO * PFLVAL
      ELSE
C        Highest measured Dtheta/Dz is above 100m.  Apply exponential
C        extrapolation term above PFLZ.
         VALUE = PFLVAL * DEXP( -(GRDZ-PFLZ) /
     &                       (EFOLDH * MAX(HDELTH,ZI) ) )
      END IF
C
      RETURN
      END


      SUBROUTINE XTRPDN ( GRDZ, VALUE )
C=======================================================================
C                XTRPDN Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the potential temperature gradient
C                by extrapolating downward from the uppermost height of 
C                observed potential temperature gradients 
C                (i.e., there is at least one gradient in the profile).
C
C   Input:       The height at which the value is required
C
C   Output:      Potential temperature gradient (VALUE) at the
C                required level (GRDZ)
C
C   Called by:   GRDPTG
C
C   Assumptions:
C
C   Programmer:  Jim Paumier (PES, Inc.)            05 August 1998
C
C   Revision history:
C                <none>
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations

      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION  GRDZ, VALUE

      MODNAM = 'XTRPDN'
C
C---- Data dictionary

C     GRDZ    = Grid profile height
C     VALUE   = Value at the gridded profile height (GRDZ)

C---- Data initializations
C
C.......................................................................
      IF( UNSTAB )THEN

         IF( GRDZ  .LE.  ZI )THEN
            VALUE  = XVAL

         ELSE IF( GRDZ .LE. ZI+500.0D0 )THEN
            VALUE  = VPTGZI

         ELSE
            VALUE  = 0.005D0

         ENDIF

      ELSE   ! stable atmosphere
C        THETA_STAR and TG4PFL (dTheta/dZ at TREFHT) were computed
C        in TGINIT

         IF( GRDZ .LT. 2.0D0 )THEN
            VALUE = TG4PFL

         ELSE
C           Extrapolate gradient using similarity
            IF( L_AdjUstar )THEN
               VALUE  = ( THSTAR / ( VONKAR * GRDZ ) ) *
     &                  ( 0.74D0  +  4.7D0 * GRDZ / OBULEN )
            ELSE
               VALUE  = ( THSTAR / ( VONKAR * GRDZ ) ) *
     &                  ( 1.0D0  +  5.0D0 * GRDZ / OBULEN )
            ENDIF

         ENDIF
      ENDIF

      RETURN
      END      


      SUBROUTINE GRDPT ()
C=======================================================================
C                GRDPT module of the AERMOD Dispersion Model
C
C   Purpose:     To construct a profile of gridded values of
C                potential temperature
C
C   Input:       Profile of gridded potential temperature gradients
C                Temperature at the reference height
C                Profile of grid heights
C
C   Output:      Potential temperature profile at the grid heights.
C
C   Assumptions: There is at least one grid level below the reference
C                temperature height (which should be satisfied
C                because the lowest grid level is 0.5 meters)
C
C   Called by:   METEXT
C
C   Programmer:  Jim Paumier                          30 Sept 1993
C                Pacific Environmental Services
C
C   Revision history:
C        12/10/97  - R. Brode, PES, Inc.
C                    Corrected the order of array indices used for profiling
C                    potential temperature above the reference height.
C        12/16/94  - J. Paumier, PES, Inc.
C                  - CALL LOCATE to get the number of levels below the
C                    temperature reference height, replacing the original
C                    method which relied on grid heights being every 10 m
C
C-----------------------------------------------------------------------
C
C---- Variable declarations

      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: L, NBELOW
      DOUBLE PRECISION :: PTREF

      MODNAM = 'GRDPT'

C---- Data definitions
C
C
C---- Data initializations
C
C
C.......................................................................
C

C---- Determine the grid level below the temperature reference
C     height (as defined in the scalar file)               ---- CALL LOCATE

      CALL LOCATE( GRIDHT, 1, MXGLVL, TREFHT, NBELOW )

C---- Compute the potential temperature at the reference level
C     using the reference temperature (TA), the reference
C     temperature height (TREFHT), and the average stack base
C     elevation of all the emission sources (ZBASE)

      PTREF = TA + GOVRCP * (TREFHT + ZBASE)

C---- Compute the potential temperature at the grid level below
C     the temperature reference height      

      GRIDPT(NBELOW) = PTREF - 
     &                 0.5D0 * (GRIDTG(NBELOW+1) + GRIDTG(NBELOW)) *
     &                       (TREFHT - GRIDHT(NBELOW))


C---- Compute Potential Temp Values for Grid Levels Below Reference Ht.
      DO L = NBELOW-1, 1, -1

         GRIDPT(L) = GRIDPT(L+1) - 0.5D0 * (GRIDTG(L+1) + GRIDTG(L)) *
     &                                     (GRIDHT(L+1) - GRIDHT(L))

      END DO


C---- Compute Potential Temp Values for Grid Levels Above Reference Ht.
      DO L = NBELOW+1, MXGLVL

         GRIDPT(L) = GRIDPT(L-1) + 0.5D0 * (GRIDTG(L) + GRIDTG(L-1)) *
     &                                     (GRIDHT(L) - GRIDHT(L-1))

      END DO

      RETURN
      END


      SUBROUTINE GRDDEN
C=======================================================================
C                GRDDEN module of the AERMOD Dispersion Model
C
C   Purpose:     To construct a profile of gridded values of ambient
C                air density - use in PRIME downwash algorithm.
C
C   Input:       Profile of gridded potential temperature gradients
C                Temperature at the reference height
C                Profile of grid heights
C
C   Output:      Ambient air density profile at the grid heights.
C
C   Called by:   METEXT
C
C   Programmer:  Roger W. Brode                         August 9, 2001
C                Pacific Environmental Services
C
C   Revision history:
C                <none>
C
C-----------------------------------------------------------------------
C
C---- Variable declarations

      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I
      DOUBLE PRECISION :: TAMB0, TAMB, TBAR, RAMB0, RGASM

      MODNAM = 'GRDDEN'

C---- Data definitions
C
C
C---- Data initializations
C
C
C.......................................................................
C
c --- Set the surface temperature (deg. K) & air density (kg/m**3)
      tamb0=TA
      ramb0=1.2D0

c --- Set the gas constant (m**2/s**2/deg. K)
      rgasm=287.026D0

C---- Compute Ambient Air Density Values
      DO I = 1, MXGLVL

c ---    Compute ambient air density at height, ZGPTA(i)

         tamb = gridpt(i) - govrcp * (gridht(i) + zbase)
         tbar = 0.5D0 * (tamb + tamb0)
         GRIDRHO(I) = ramb0*(tamb0/tamb)*DEXP(-g*gridht(i)/
     &                (rgasm*tbar))

      END DO

      RETURN
      END
