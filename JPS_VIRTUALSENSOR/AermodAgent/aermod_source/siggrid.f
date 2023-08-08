      SUBROUTINE GRDSV ()
C=======================================================================
C                GRDSV module of the AERMOD Dispersion Model
C
C   Purpose:     To construct a profile of gridded values of the
C                standard deviation of the horizontal wind speed
C                flucuations.
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
      CHARACTER MODNAM*12

      INTEGER     PINDEX, GINDEX
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
      MODNAM = 'GRDSV '

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
C        -------------------------------------------
C        Now begin looping over the observed profile
C        -------------------------------------------
C
         DO WHILE( GRIDSV(GINDEX) .LT. -90.0D0 .AND. PINDEX.LE.NPLVLS )
C
            IF( PFLSV(PINDEX) .GE. 0.0D0 )THEN
C
C              -------------------------------------------------
C              Data at this level are not missing; determine its
C              location relative to the height at which data are
C              required and act accordingly.
C              -------------------------------------------------
               IF( DABS( PFLHT(PINDEX)-GRIDHT(GINDEX)) .LE. 0.1D0 )THEN
C                 USE the parameter at this level
                  GRIDSV(GINDEX) = PFLSV(PINDEX)
C
               ELSEIF( GRIDHT(GINDEX)  .GT.  PFLHT(PINDEX) )THEN
                  IF( PINDEX .LT. NPLVLS )THEN
C                    SAVE value for possible interpolation
                     VBELOW = PFLSV(PINDEX)
                     HTBELO = PFLHT(PINDEX)

                  ELSE   ! this is the top level
C                    PROFILE upward from this level        --- CALL XTRPSV
                     CALL XTRPSV ( PFLHT(PINDEX), PFLSV(PINDEX),
     &                             GRIDHT(GINDEX), GRIDSV(GINDEX) )
                  ENDIF
C
               ELSEIF( GRIDHT(GINDEX)  .LT.  PFLHT(PINDEX) )THEN
                  IF( VBELOW .GE. 0.0D0 )THEN
C                    INTERPOLATE between the two values    --- CALL NTRPSV
                     CALL NTRPSV ( HTBELO, VBELOW, PFLHT(PINDEX),
     &                             PFLSV(PINDEX), GRIDHT(GINDEX),
     &                             GRIDSV(GINDEX) )

                  ELSE   ! BELOW is missing
C                    PROFILE down from this level          --- CALL XTRPSV
                     CALL XTRPSV ( PFLHT(PINDEX), PFLSV(PINDEX),
     &                             GRIDHT(GINDEX), GRIDSV(GINDEX) )

                  ENDIF
C
               ELSE
C                 This section is for DEBUGging - the program should never
C                 reach this point
                  WRITE(*,*) ' ---> ERROR: The search for data to'
                  WRITE(*,*) '             construct the gridded '
                  WRITE(*,*) '             profile failed on ', KURDAT
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
C                    PROFILE up from BELOW                 --- CALL XTRPSV
                     CALL XTRPSV ( HTBELO, VBELOW,
     &                             GRIDHT(GINDEX), GRIDSV(GINDEX) )

                  ELSE   ! there are no data
C                    COMPUTE value: full parameterization  --- CALL REFSV
                     CALL REFSV ( GRIDHT(GINDEX), GRIDSV(GINDEX) )
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
            IF( (GRIDSV(GINDEX) .LT. 0.0D0)  .AND.
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
C
      END DO   ! Loop over gridded data profile
C
      RETURN
      END

      SUBROUTINE REFSV ( HTINP, VALUE )
C=======================================================================
C                REFSV Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the value of sigma-V, the horizontal
C                (lateral) dispersion parameter from the surface
C                friction velocity and convective scaling velocity
C                alone (i.e., no observations of sigma-V)
C
C   Input:       Mixing height (ZI)
C                Friction velocity (USTAR)
C                Convective scaling velocity (WSTAR)
C                Height at which sigma-V is needed (HEIGHT)
C
C   Output:      sigmaV at the required level (VALUE)
C
C   Assumptions: Mixing height, friction and convective scaling
C                velocities are nonmissing
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
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: HEIGHT, VALUE, SVC, SVM
      DOUBLE PRECISION :: HTINP
C
C---- Data dictionary
C
C---- Data initializations
C
      MODNAM = 'REFSV '
      
      HEIGHT = HTINP
C
C.......................................................................
C
      IF( UNSTAB )THEN
C        Compute the convective component of sigma-v, SVC
         CALL REFSVC( HEIGHT, SVC )

C        Compute the mechanical component of sigma-v, SVM
         CALL REFSVM( HEIGHT, SVM )

         VALUE = DSQRT( SVC*SVC + SVM*SVM )

      ELSEIF( STABLE )THEN
C        Compute the mechanical component of sigma-v, SVM
         CALL REFSVM( HEIGHT, SVM )

         VALUE = SVM

      ENDIF

C
      RETURN
      END

      SUBROUTINE REFSVC ( HTINP, VALUE )
C=======================================================================
C                REFSVC Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the value of sigma-V, the horizontal
C                (lateral) dispersion parameter from the surface
C                friction velocity and convective scaling velocity
C                alone (i.e., no observations of sigma-V)
C
C   Input:       Mixing height (ZI)
C                Friction velocity (USTAR)
C                Convective scaling velocity (WSTAR)
C                Height at which sigma-V is needed (HEIGHT)
C
C   Output:      sigmaV at the required level (VALUE)
C
C   Assumptions: Mixing height, friction and convective scaling
C                velocities are nonmissing
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
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: HEIGHT, VALUE, SV2, ZDCRS, ATZI, SV2DCR, VAL2
      DOUBLE PRECISION :: HTINP
C
C---- Data dictionary
C     SV2    = variance of the horizontal component of the wind
C     ZDCRS  = value of 1.2 * ZICONV used in profiling SV2
C     ATZI   = value of SV at ZICONV
C     AT1PT2 = the fraction of and above the mixing height
C              through which the parameter is changing
C
C     AT1PT2 is defined in a PARAMETER statement in MODULE MAIN1
C
C---- Data initializations
C
      MODNAM = 'REFSVC'
      
      HEIGHT = HTINP
      ZDCRS  =  AT1PT2 * ZICONV
C
C.......................................................................
C
      SV2 = 0.35D0 * WSTAR**2
C
      IF( HEIGHT  .LE.  ZICONV )THEN
         VALUE = DSQRT( SV2 )

      ELSEIF( HEIGHT .GT. ZICONV  .AND.  HEIGHT .LE. ZDCRS )THEN
C        COMPUTE sigmaV at 1.2*ZI
         SV2DCR = MIN( SV2, 0.25D0 )
C        INTERPOLATE between value of SV2 at ZI and at 1.2*ZI
         CALL GINTRP ( ZICONV, SV2, ZDCRS, SV2DCR, HEIGHT, VAL2 )
         VALUE = DSQRT( VAL2 )

      ELSE   ! requested height is above 1.2*mixing height
         ATZI  = DSQRT( SV2 )
         VALUE = MIN( ATZI, 0.5D0 )

      ENDIF
C
      RETURN
      END

      SUBROUTINE REFSVM ( HTINP, VALUE )
C=======================================================================
C                REFSVM Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the value of sigma-V, the horizontal
C                (lateral) dispersion parameter from the surface
C                friction velocity and convective scaling velocity
C                alone (i.e., no observations of sigma-V)
C
C   Input:       Mixing height (ZI)
C                Friction velocity (USTAR)
C                Convective scaling velocity (WSTAR)
C                Height at which sigma-V is needed (HEIGHT)
C
C   Output:      sigmaV at the required level (VALUE)
C
C   Assumptions: Mixing height, friction and convective scaling
C                velocities are nonmissing
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
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: HEIGHT, VALUE, SV02, SV2ZI, VARV
      DOUBLE PRECISION :: HTINP
C
C---- Data dictionary
C     SV02   = variance of the horizontal component of the wind at the
C              surface.
C
C---- Data initializations
C
      MODNAM = 'REFSVM'
      
      HEIGHT = HTINP
C
C.......................................................................
C
C     Compute SV2 at the surface
      SV02 = 3.6D0 * USTAR**2
C
C     Compute SV2 at ZI;
C     Do not let SV2 at ZI exceed the surface value.
      SV2ZI = MIN( SV02, 0.25D0 )

      IF( HEIGHT  .LE.  ZIMECH )THEN
C        INTERPOLATE between these two values of the variance if HEIGHT is
C        below ZIMECH
         CALL GINTRP ( 0.0D0, SV02, ZIMECH, SV2ZI, HEIGHT, VARV )
         VALUE = DSQRT(VARV)

      ELSE
C        Persist value at ZIMECH upward.
         VALUE = DSQRT(SV2ZI)

      ENDIF
C
      RETURN
      END

      SUBROUTINE NTRPSV ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
C=======================================================================
C                NTRPSV Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the deviation of the horizontal wind speed
C                at an intermdiate level by interpolating between two
C                observed values.
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
C                Jim Paumier, PES                    5 July 1995
C                  - using AT1PT2 for 1.2, with the value defined 
C                    in MAIN1.INC
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      IMPLICIT NONE
      DOUBLE PRECISION ::  HTBELO, VBELOW, HTABOV, VABOVE, REQDHT, 
     &             VALUE, REFABV, REFBLW, RATIO, REFREQ, VALINT, REFINT
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
C     at the height below the requested height (REFBLW)    --- CALL REFSV
C
      CALL REFSV (  HTBELO, REFBLW )

C     COMPUTE the value of the parameter from the reference profile
C     at the height above the requested height (REFABV)    --- CALL REFSV
C
      CALL REFSV (  HTABOV, REFABV )
C     COMPUTE the value of the parameter from the reference profile
C     at the requested height (REFREQ)                     --- CALL REFSV
C
      CALL REFSV ( REQDHT, REFREQ )

C
C     Linearly interpolate to REQDHT from observed and reference profiles
      CALL GINTRP ( HTBELO,VBELOW, HTABOV, VABOVE, REQDHT,VALINT )
      CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
C     REFREQ is value from REFerence profile at REQuired height
C     REFINT is value from REFerence profile linearly INTerpolated to req ht
C     VALINT is the observed VALue linearly INTerpolated to required height
      RATIO = REFREQ/REFINT
      VALUE = RATIO * VALINT

      RETURN
      END

      SUBROUTINE XTRPSV ( PFLZ, PFLVAL, GRDZ, VALUE )
C=======================================================================
C                XTRPSV Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the deviation of the horizontal wind
C                speed by extrapolating outside (either above or
C                below) the range of observed data (i.e., there is
C                at least one observation of sigmaV in the profile).
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
      IMPLICIT NONE
      DOUBLE PRECISION :: PFLZ, PFLVAL, GRDZ, VALUE, VALOBS, VALGRD, 
     &                    RATIO
C
C---- Data dictionary
C
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
C     at the height of the highest(lowest) observed value --- CALL REFSV
C
      CALL REFSV ( PFLZ, VALOBS )
C
C     COMPUTE the value of the parameter from the reference profile
C     at the height where a value is required             --- CALL REFSV
C
      CALL REFSV ( GRDZ, VALGRD )
C
      RATIO = VALGRD / VALOBS
C
      VALUE = RATIO * PFLVAL
C
      RETURN
      END



      SUBROUTINE GRDSW ()
C=======================================================================
C                GRDSW module of the AERMOD Dispersion Model
C
C   Purpose:     To construct a profile of gridded values of the
C                deviation of the vertical wind speed
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
      CHARACTER MODNAM*12

      INTEGER     PINDEX, GINDEX
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
      MODNAM = 'GRDSW '
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
         DO WHILE( GRIDSW(GINDEX) .LT. -90.0D0 .AND. PINDEX.LE.NPLVLS )
C
            IF( PFLSW(PINDEX) .GE. 0.0D0 )THEN
C
C              -------------------------------------------------
C              Data at this level are not missing; determine its
C              location relative to the height at which data are
C              required and act accordingly.
C              -------------------------------------------------
               IF( DABS( PFLHT(PINDEX) - GRIDHT(GINDEX) ).LE.0.1D0 )THEN
C                 USE the parameter at this level
                  GRIDSW(GINDEX) = PFLSW(PINDEX)
C
               ELSEIF( GRIDHT(GINDEX)  .GT.  PFLHT(PINDEX) )THEN
                  IF( PINDEX .LT. NPLVLS )THEN
C                    SAVE value for possible interpolation
                     VBELOW = PFLSW(PINDEX)
                     HTBELO = PFLHT(PINDEX)

                  ELSE   ! this is the top level
C                    PROFILE upward from this level        --- CALL XTRPSW
                     CALL XTRPSW ( PFLHT(PINDEX), PFLSW(PINDEX),
     &                             GRIDHT(GINDEX), GRIDSW(GINDEX) )
                  ENDIF
C
               ELSEIF( GRIDHT(GINDEX)  .LT.  PFLHT(PINDEX) )THEN
                  IF( VBELOW .GE. 0.0D0 )THEN
C                    INTERPOLATE between the two values    --- CALL NTRPSW
                     CALL NTRPSW ( HTBELO, VBELOW, PFLHT(PINDEX),
     &                             PFLSW(PINDEX), GRIDHT(GINDEX),
     &                             GRIDSW(GINDEX) )

                  ELSE   ! BELOW is missing
C                    PROFILE down from this level          --- CALL XTRPSW
                     CALL XTRPSW ( PFLHT(PINDEX), PFLSW(PINDEX),
     &                             GRIDHT(GINDEX), GRIDSW(GINDEX) )

                  ENDIF
C
               ELSE
C                 This section is for DEBUGging - the program should never
C                 reach this point
                  WRITE(*,*) ' ---> ERROR: The search for data to'
                  WRITE(*,*) '             construct the gridded '
                  WRITE(*,*) '             profile failed on ', KURDAT
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
C                    PROFILE up from BELOW                 --- CALL XTRPSW
                     CALL XTRPSW ( HTBELO, VBELOW,
     &                             GRIDHT(GINDEX), GRIDSW(GINDEX) )

                  ELSE   ! there are no data
C                    COMPUTE value: full parameterization  --- CALL REFSW
                     CALL REFSW ( GRIDHT(GINDEX), GRIDSW(GINDEX) )
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
            IF( (GRIDSW(GINDEX) .LT. 0.0D0)  .AND.
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
C
      END DO   ! Loop over gridded data profile
C


      RETURN
      END

      SUBROUTINE REFSW ( HTINP, VALUE )
C=======================================================================
C                REFSW Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the value of sigma-W, the vertical
C                dispersion parameter from the surface friction
C                velocity and convective scaling velocity alone
C                (i.e., no observations of sigma-W)
C
C   Input:
C
C
C
C
C   Output:      Value of the parameter at the required level
C
C   Assumptions: Mixing height, friction and convective scaling
C                velocities are nonmissing
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C                Jim Paumier, PES                    5 July 1995
C                  - modified the coefficient on the u* term for CBL;
C                  - made the value 1.2 a parameter in MAIN1;
C                  - modified the form of sigma-w at all height for SBL;
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: HEIGHT, VALUE, SWC, SWM
      DOUBLE PRECISION :: HTINP
C
C---- Data dictionary
C
C     SWC    = convective component of the variance of the vertical velocity
C     SWM    = mechanical component of the variance of the vertical velocity
C
C---- Data initializations
C
      MODNAM = 'REFSW '
      
      HEIGHT = HTINP
C
C.......................................................................
C
      IF (UNSTAB) THEN
C        Compute the convective component of sigma-w, SWC
         CALL REFSWC( HEIGHT, SWC )

C        Compute the mechanical component of sigma-w, SWM
         CALL REFSWM( HEIGHT, SWM )

C        Apply a lower limit > 0 to convective and mechanical components of sigma-w
         SWC = MAX( SWC, 0.0001D0 )
         SWM = MAX( SWM, 0.0001D0 )

         VALUE = DSQRT( SWC*SWC + SWM*SWM )

crwb     Output convective and mechanical components of profile.
crwb         if (METEOR) then
crwb            if (height .eq. 0.0D0) then
crwb               write(dbmunt,909) kurdat, ziconv, zimech, zi
crwb909            format(1x,'DATE= ',i8,2x,'ZIc= ',f8.2,2x,'ZIm= ',f8.2,2x,
crwb     &                'ZI = ',f8.2,/,
crwb     &          '   GRIDHT    SIGW_COVN      SIGW_MECH       SIGW_TOT')
crwb            end if
crwb            write(dbmunt,919) height, swc, swm, value
crwb919         format(1x,f8.2,3(3x,g13.5))
crwb         end if

      ELSE IF (STABLE) THEN

C        Compute the mechanical component of sigma-w, SWM
         CALL REFSWM( HEIGHT, SWM )

C        Apply a lower limit > 0 to convective and mechanical components of sigma-w
         VALUE = MAX( SWM, 0.0001D0 )

      END IF

      RETURN
      END

      SUBROUTINE REFSWC ( HTINP, VALUE )
C=======================================================================
C                REFSWC Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the value of sigma-W, the vertical
C                dispersion parameter from the surface friction
C                velocity and convective scaling velocity alone
C                for the convective boundary layer.
C                (i.e., no observations of sigma-W)
C
C   Input:
C
C
C
C
C   Output:      Value of the parameter at the required level
C
C   Assumptions: Mixing height, friction and convective scaling
C                velocities are nonmissing
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C                Jim Paumier, PES                    5 July 1995
C                  - modified the coefficient on the u* term for CBL;
C                  - made the value 1.2 a parameter in MAIN1;
C                  - modified the form of sigma-w at all height for SBL;
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: HEIGHT, VALUE, SW2, EXPARG
      DOUBLE PRECISION :: HTINP
C
C---- Data dictionary
C
C     SW2    = variance of the vertical velocity
C
C---- Data initializations
C
      MODNAM = 'REFSWC'
      
      HEIGHT = HTINP
C
C.......................................................................
C
      IF( HEIGHT  .LE.  0.1D0*ZICONV )THEN
         SW2 = 1.6D0 * ( HEIGHT / ZICONV )**(2.0D0*THIRD) * WSTAR**2
         VALUE  = DSQRT( SW2 )

      ELSEIF( HEIGHT .GT. 0.1D0*ZICONV  .AND.  HEIGHT .LE. ZICONV )THEN
         VALUE = DSQRT( 0.35D0 * WSTAR**2 )

      ELSEIF( HEIGHT .GT. ZICONV )THEN
         EXPARG = -6.0D0*(HEIGHT - ZICONV)/ZICONV
         IF (EXPARG .GT. EXPLIM) THEN
            SW2 = 0.35D0 * WSTAR**2 * DEXP( EXPARG )
            VALUE = DSQRT( SW2 )
         ELSE
            SW2 = 0.0D0
            VALUE = 0.0D0
         END IF

      ENDIF
C

      RETURN
      END

      SUBROUTINE REFSWM ( HTINP, VALUE )
C=======================================================================
C                REFSWM Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the value of sigma-W, the vertical
C                dispersion parameter from the surface friction
C                velocity and convective scaling velocity alone
C                for the mechanical boundary layer.
C                (i.e., no observations of sigma-W)
C
C   Input:
C
C
C
C
C   Output:      Value of the parameter at the required level
C
C   Assumptions: Mixing height, friction and convective scaling
C                velocities are nonmissing
C
C   Programmer:  Jim Paumier                        30 September 1993
C                Pacific Environmental Services
C
C   Revision history:
C                Jim Paumier, PES                    5 July 1995
C                  - modified the coefficient on the u* term for CBL;
C                  - made the value 1.2 a parameter in MAIN1;
C                  - modified the form of sigma-w at all height for SBL;
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: HEIGHT, VALUE, SWR, SWBL
      DOUBLE PRECISION :: HTINP
C
C---- Data dictionary
C
C     SWR    = variance of the vertical velocity due to residual turbulence
C     SWBL   = variance of the vertical velocity in the boundary layer
C
C---- Data initializations
C
      MODNAM = 'REFSWM'
      
      HEIGHT = HTINP
C
C.......................................................................
C
C

      SWR  = SWRMAX * MIN( 1.0D0, HEIGHT/ZI)

      IF (HEIGHT .LT. ZI) THEN
         SWBL = 1.3D0 * USTAR * DSQRT( 1.0D0 - HEIGHT/ZI)
      ELSE
         SWBL = 0.0D0
      END IF

      VALUE = DSQRT( SWR*SWR + SWBL*SWBL )


      RETURN
      END

      SUBROUTINE NTRPSW ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
C=======================================================================
C                NTRPSW Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the deviation of the vertical wind
C                speed at an intermediate level by interpolating
C                between two observed values.
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
C      JOP    March 14, 1995 - modified the check on the ratio for a
C                              positive slope and observed values
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      IMPLICIT NONE

      DOUBLE PRECISION :: REFABV, REFBLW, RATIO, REFREQ, VALINT, REFINT,
     &        HTBELO, VBELOW, HTABOV, VABOVE, REQDHT, VALUE
C
C---- Data dictionary
C
C
C---- Data initializations
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
C     at the height below the requested height (REFBLW)         --- CALL REFSW
      CALL REFSW ( HTBELO, REFBLW )

C     COMPUTE the value of the parameter from the reference profile
C     at the height above the requested height (REFABV)         --- CALL REFSW
      CALL REFSW ( HTABOV, REFABV )
C
C     COMPUTE the value of the parameter from the reference profile
C     at the requested height (REFREQ)                          --- CALL REFSW
      CALL REFSW ( REQDHT, REFREQ )
C
C
C     Linearly interpolate to REQDHT from observed and reference profiles
      CALL GINTRP ( HTBELO,VBELOW, HTABOV, VABOVE, REQDHT,VALINT )
      CALL GINTRP ( HTBELO,REFBLW, HTABOV, REFABV, REQDHT,REFINT )
C     REFREQ is value from REFerence profile at REQuired height
C     REFINT is value from REFerence profile linearly INTerpolated to req ht
C     VALINT is the observed VALue linearly INTerpolated to required height
      RATIO = REFREQ/REFINT
      VALUE = RATIO * VALINT

      RETURN
      END

      SUBROUTINE XTRPSW ( PFLZ, PFLVAL, GRDZ, VALUE )
C=======================================================================
C                XTRPSW Module of the AERMOD Dispersion Model
C
C   Purpose:     To compute the the deviation of the vertical wind
C                speed by extrapolating outside (either above or below)
C                the range of observed data (i.e., there is at least
C                one observation of sigma_W in the profile).
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
      IMPLICIT NONE

      DOUBLE PRECISION :: GRDZ, PFLVAL, PFLZ, RATIO, VALOBS, VALGRD, 
     &                    VALUE
C
C---- Data dictionary
C
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
C     at the height of the highest(lowest) observed value      --- CALL REFSW
      CALL REFSW ( PFLZ, VALOBS )
C
C     COMPUTE the value of the parameter from the reference profile
C     at the height where a value is required                  --- CALL REFSW
      CALL REFSW ( GRDZ, VALGRD )
C
      RATIO = VALGRD / VALOBS
C
      VALUE = RATIO * PFLVAL
C
      RETURN
      END
