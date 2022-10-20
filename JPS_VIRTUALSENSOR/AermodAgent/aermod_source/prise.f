      SUBROUTINE DELTAH ( XARG )
C***********************************************************************
C             DELTAH Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: To calculate plume rise
C
C   PROGRAMMER: Roger Brode, Jim Paumier, PES, Inc.
C
C   DATE:    September 30, 1993
C
C   CHANGES:  Removed reference to PARAMS.PRI include file.
C             R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C   CHANGES:  Modified to maintain better consistency with ISCST3 for
C             Schulman-Scire downwash algorithm.
C             Roger Brode, PES, Inc. - 12/6/99
C
C   CHANGES:  Corrected variable name from SVMP to SVPM.
C             Roger Brode, PES, Inc. - 5/24/99
C
C   CHANGES:  Added XARG as actual argument for CBLPRN.
C             Roger Brode, PES, Inc. - 12/5/94
C
C   INPUTS:  The distance at which to make the computation, XARG
C
C   OUTPUTS: Distance-Dependent Plume Rise, DHP (m)
C
C   CALLED FROM:   PCALC
C
C   Assumptions:  All plume rise calculations are for gradual rise,
C                 except in stable conditions when the downwind distance
C                 exceeds XMAX
C
C   References:   "Dispersion in the Stable Boundary Layer",
C                 A. Venkatram, 2/12/93
C                 "A Dispersion Model for the Convective Boundary Layer",
C                 J. Weil, 8/17/93
C                 "Plume Penetration of the CBL and Source 3: Source
C                 Strength and Plume Rise", J. Weil, 9/1/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE

      CHARACTER MODNAM*12
      INTEGER :: KITER, NDXZPL
      DOUBLE PRECISION :: XARG, XMAXTMP, XRISE, ZPLM, DHPOLD,
     &                    SVPM, UPM, TGPM, PTPM, PTP

C     Variable Initializations
      MODNAM = 'DELTAH'

      IF( (STABLE  .OR.  (UNSTAB  .AND.  (HS .GE. ZI)))  .AND.
     &      (XARG .GE. XMAX) )THEN
C        Use final stable plume rise (DHF) calculated in DISTF (DHP)
C        at XMAX
         DHP = DHFAER


      ELSE IF( (STABLE  .OR. (UNSTAB  .AND.  (HS .GE. ZI))) .AND. 
     &                                     (XARG .LT. XMAX) ) THEN
C----    Compute stable plume rise for the distance XARG   --- CALL SBLRIS
C        Use iterative approach to plume rise calculations.
C        First compute temporary distance to "final rise" based on current
C        values of UP and BVPRIM.  Then, don't pass a distance larger than
C        XMAXTMP to SBLRIS.  This avoids potential for math error in
C        SUB. SBLRIS for distances beyond the value of XMAX computed
C----    iteratively outside the receptor loop in SUB. DISTF.
         XMAXTMP = UP * DATAN2( FM*BVPRIM, -FB ) / BVPRIM
         XRISE   = MIN( XARG, XMAXTMP )
         CALL SBLRIS ( XRISE )
         KITER = 0

50       ZPLM = HSP + 0.5D0 * DHP
         DHPOLD = DHP

C----    Locate index below ZPLM

         CALL LOCATE(GRIDHT, 1, MXGLVL, ZPLM, NDXZPL)

C----    Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
C        vertical potential temperature gradient, if stable.

         CALL GINTRP( GRIDHT(NDXZPL), GRIDSV(NDXZPL),
     &        GRIDHT(NDXZPL+1), GRIDSV(NDXZPL+1), ZPLM, SVPM )
         CALL GINTRP( GRIDHT(NDXZPL), GRIDWS(NDXZPL),
     &        GRIDHT(NDXZPL+1), GRIDWS(NDXZPL+1), ZPLM, UPM )

         SVPM = MAX( SVPM, SVMIN, SVUMIN*UPM )
         IF( L_VECTORWS )THEN
             UPM = DSQRT( UPM*UPM + 2.0D0*SVPM*SVPM )
         ENDIF
         UPM  = MAX( UPM, WSMIN )

CRWB     Use average of stack top and midpoint wind speeds.
         UP = 0.5D0 * (US + UPM)
         
         CALL GINTRP( GRIDHT(NDXZPL), GRIDTG(NDXZPL),
     &        GRIDHT(NDXZPL+1), GRIDTG(NDXZPL+1), ZPLM, TGPM )
         CALL GINTRP( GRIDHT(NDXZPL), GRIDPT(NDXZPL),
     &        GRIDHT(NDXZPL+1), GRIDPT(NDXZPL+1), ZPLM, PTPM )
CRWB     Use average of stack top and midpoint temperature gradients.
         TGP = 0.5D0 * (TGS + TGPM)
         PTP = 0.5D0 * (PTS + PTPM)
         BVF = DSQRT( G * TGP / PTP)
         IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
         BVPRIM  = 0.7D0 * BVF

C        Repeat calculation of temporary distance to "final rise" using
C        current values of UP and BVPRIM.
         XMAXTMP = UP * DATAN2( FM*BVPRIM, -FB ) / BVPRIM
         XRISE   = MIN( XARG, XMAXTMP )
         CALL SBLRIS ( XRISE )

         KITER = KITER + 1

CRJP     Add temporary debugging statements

         IF(DEBUG) THEN
            WRITE(DBGUNT,6001) KITER,DHPOLD, DHP, ZPLM, UP,TGP
6001        FORMAT(/,5X,'OPTH2 ITER. #',I1,': OLD DELH = ',
     &       F6.1,' M; NEW DELH = ',F6.1,' M; MET LEVEL = ',
     &       F6.1,' M; NEW Upl = ',F5.2,' M/S; NEW DTHDZ = ',
     &       F7.4,' K/M')
         ENDIF
         
C        Check for convergence
         IF(DHP.GT.0.0D0 .AND. DABS((DHPOLD-DHP)/DHP).LT.0.001D0 .AND.
     &                                         KITER .GE. 5)THEN
            IF( DHP .LE. 1.0D-5 )THEN
               DHP = 1.0D-5
            ENDIF
            GO TO 60
         ELSEIF(KITER .LT. 10)THEN
            GO TO 50
         ENDIF
         
         IF(KITER .GE. 5) THEN
            DHP = 0.5D0 * (DHP + DHPOLD)
            IF(DEBUG) WRITE(DBGUNT,6002) DHP
6002        FORMAT(/,5X,'OPTH2 ITERATION FAILED TO CONVERGE; PLUME',
     &      ' RISE SET AT ',F6.1,' METERS.',/)
            GO TO 60
         ELSE
            GO TO 50
         ENDIF

60       CONTINUE

CRWB     After completing iteration, reset UP and TGP to stack top
CRWB     values for subsequent distance-dependent plume rise calcs.
         UP  = US
         TGP = TGS
         PTP = PTS
         BVF = DSQRT( G * TGP / PTP )
         IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
         BVPRIM  = 0.7D0 * BVF
ccrfl-3/6/95 Make sure SBL rise is not greater than CBL rise.
         CALL CBLPRD(XARG)
         DHP = MIN(DHP,DHP1)
         DHP = MIN(DHP,DHFAER)

      ELSEIF( UNSTAB )THEN
C        (i.e., for UNSTABle cases, with HS < ZI)

C        Compute  plume rise for the direct plume       --- CALL CBLPRD
         CALL CBLPRD ( XARG )

C        Compute  plume rise for the indirect plume        --- CALL CBLPRN
         CALL CBLPRN ( XARG )

         IF( PPF .GT. 0.0D0 )THEN
C           Compute plume rise for the penetrated plume    --- CALL CBLPR3
            CALL CBLPR3

         ELSE
C           No plume penetration - plume rise is zero for this source
            DHP3 = 0.0D0

         ENDIF

      ENDIF

      RETURN
      END

      SUBROUTINE PRMDELH ( XARG, L_INWAKE )
C***********************************************************************
C             PRMDELH Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: To calculate plume rise for PRIME concentration
C
C   PROGRAMMER: Roger Brode, PES, Inc.
C
C   DATE:     July 5, 2001
C
C   CHANGES:  Replaced reference to PARAMS.PRI include file with
C             PRIME_wakedat MODULE subprogram.
C             R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C             Modified to save L_INWAKE to local variable.
C             R. W. Brode, MACTEC (f/k/a PES), Inc., 08/02/05
C
C   INPUTS:  The distance at which to make the computation, XARG
C
C   OUTPUTS: Distance-Dependent Plume Rise, DHP (m)
C
C   CALLED FROM:   PRMCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
c --- Include PRIME plume rise parameters
      USE PRIME_wakedat
      USE PRM2_WAKEDAT, ONLY: DFSN2CALL
      IMPLICIT NONE

      CHARACTER MODNAM*12
      INTEGER :: NUMWAKE, ierr
      DOUBLE PRECISION :: XARG
      DOUBLE PRECISION, SAVE :: hseff, reff
c --- Dimension work arrays for PRIME numerical plume rise
      DOUBLE PRECISION, SAVE :: xtr(mxntr), ytr(mxntr),
     &                          ztr(mxntr), rtr(mxntr)

      LOGICAL       :: L_INWAKE
      LOGICAL, SAVE :: L_INWAKE_SAVE, CAPPED, HORIZ

C     Variable Initializations
      MODNAM = 'PRMDELH'
      CAPPED = .FALSE.
      HORIZ  = .FALSE.

c
c --- PRIME ---------------------------------------------------
      IF (WAKE) THEN
c ---    Calculate final rise & array of transitional rise values
c ---    for first receptor only
         if (PRM_FSTREC) then
            PRM_FSTREC = .FALSE.
            L_INWAKE = .FALSE.
            DFSN2CALL = .FALSE.
            hseff=hs
c ---       Compute stack radius from diameter
            reff=0.5D0*ds
            if (srctyp(isrc) .eq. 'POINTCAP') then
               capped = .TRUE.
            else
               capped = .FALSE.
            end if
            if (srctyp(isrc) .eq. 'POINTHOR') then
               horiz = .TRUE.
            else
               horiz = .FALSE.
            end if
c ---       Calculate transitional & final plume rise       ---   CALL NUMRISE
            call NUMRISE(PRIMEDBG,hseff,reff,ts,vs,mxntr,capped,horiz,
     &                   dsfact,xtr,ytr,ztr,rtr,L_INWAKE,numwake,ierr,
     &                   PRMDBUNT)
            IF (ierr .eq. 1) then
c ---          Error occurred during PRIME numerical plume rise.
c              Write fatal error message - source parameters may be suspect.
               CALL ERRHDL(PATH,MODNAM,'E','499',SRCID(ISRC))
               RUNERR = .TRUE.
               RETURN
            END IF
            IF (NUMWAKE .LE. 1) THEN
               L_INWAKE = .FALSE.
            END IF
c ---       ZTR is effective plume ht. - compute final rise
            DHF = ztr(mxntr) - hseff
c ---       Report selected data to file for debug          ---   CALL WAKE_DBG
            if(PRIMEDBG) call WAKE_DBG(PRMDBUNT,mxntr,xtr,ytr,ztr,rtr,
     &                                 nobid,hseff)
            L_INWAKE_SAVE = L_INWAKE
         else
            L_INWAKE = L_INWAKE_SAVE
         endif
c
c ---    Determine the plume rise for current receptor
         IF (XARG .LT. xtr(mxntr)) THEN
c ---       Interpolate in rise table to get gradual rise   ---   CALL NUMGRAD
            call NUMGRAD(xarg,xtr,ztr,mxntr,zeff)
            dhp = zeff - hseff
         ELSE
            DHP = ztr(mxntr) - hseff
         END IF

      ENDIF

      RETURN
      END

      FUNCTION HSPRIM(US,VS,HS,DS)
C***********************************************************************
C                 HSPRIM Module of the ISC Model - Version 2
C
C        PURPOSE: Calculates Stack Height Adjusted for Stack
C                 Tip Downwash (HS')
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Wind Speed Adjusted to Stack Height
C
C        OUTPUTS: Adjusted Stack Height (m)
C
C        CALLED FROM:   PHEFF
C***********************************************************************

C     Variable Declarations
      IMPLICIT NONE
      DOUBLE PRECISION :: US, VS, HS, DS, HSPRIM
      CHARACTER MODNAM*6
C     Variable Initializations
      MODNAM = 'HSPRIM'

C     Calculate Adjusted Stack Height (Eqn. 1-7)

      IF (VS .LT. 1.5D0*US) THEN
         HSPRIM = HS - 2.0D0*DS*(1.5D0-VS/US)
      ELSE
         HSPRIM = HS
      END IF

      IF (HSPRIM .LT. 0.0D0)  HSPRIM = 0.0D0

      RETURN
      END

      SUBROUTINE SBLRIS ( XARG )
C***********************************************************************
C             SBLRIS Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE:  To calculate plume rise for the stable boundary layer
C             or releases above the convective boundary layer
C
C   PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc
C
C   DATE:    September 30, 1993
C
C   INPUTS:  Brunt-Vaisala frequency, S
C            Buoyancy flux, FB
C            Momentum flux, FM
C            Downwind distance, XARG
C            Wind speed at stack top, UP
C
C   OUTPUTS: Plume Rise, DHP (m)
C
C   CALLED FROM:   PCALC
C
C   Assumptions:  Wind speed is nonzero at stack top
C
C   References:   "Dispersion in the Stable Boundary Layer",
C                 A. Venkatram, 2/12/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG, TERMA, TERMB, TERMC, TERMD, TERME
      DOUBLE PRECISION :: XLN, DELHNN


C     Variable Initializations
      MODNAM = 'SBLRIS'

C---- Compute the stable plume rise; FB and BVF and UP have all been
C     checked previously to insure all are greater than 0.0

      TERMA =  FB / (BVF * BVF * UP)
      TERMB =  BVPRIM * FM / FB
      TERMC =  DSIN(BVPRIM * XARG / UP)
      TERMD =  DCOS(BVPRIM * XARG / UP)

C --- Calculate TERME to check for possible negative argument for DHP
      TERME = (TERMB*TERMC+1.0D0-TERMD)

      IF( TERME .GT. 0.0D0 )THEN
         DHP = 2.66D0 * (TERMA *(TERMB*TERMC+1.0D0-TERMD))**THIRD
      ELSE
         DHP = 2.66D0 * (TERMA*TERMB*TERMC)**THIRD
      ENDIF


C --- Equation 95 of MFD for distant-dependent stable plume rise
C     DHP = 2.66D0 * TERMA**THIRD * (TERMB*TERMC+1.0D0-TERMD)**THIRD

C      DHP = 2.66 * (FB / (BVF * BVF * UP) ) ** 0.333333 *
C     &              ( (BVPRIM * FM / FB) * SIN(BVPRIM * XARG / UP) +
C     &                1.0 - COS(BVPRIM * XARG / UP) ) ** 0.333333

C --- Apply lower limit on stable plume rise based on Equation 98
C     of the MFD
      XLN = FB/(UP*USTAR*USTAR)
      DELHNN = 1.2D0*XLN**0.6D0 * (HSP + 1.2D0*XLN)**0.4D0

      DHP = MIN( DHP, DHFAER, DELHNN )

      RETURN
      END

      SUBROUTINE CBLPRD ( XARG )
C***********************************************************************
C             CBLPRD Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: To calculate plume rise for the direct plume in the
C            convective boundary layer
C
C   PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc
C
C   DATE:    September 30, 1993
C
C   INPUTS:  Downwind distance (xarg)
C
C   OUTPUTS: Plume Rise, DHP1 (m)
C
C   CALLED FROM:   DELTAH
C
C   Assumptions:  Wind speed is nonzero at stack top
C                 BETA1 = 0.6D0 (assigned in MODULE MAIN1)
C
C   References:   "A Dispersion Model for the Convective Boundary Layer",
C                 J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'CBLPRD'

C --- Original code based on Eq. 91 of MFD.
      DHP1 = (3.0D0 * FM * XARG / (BETA1*BETA1 * UP*UP) +
     &        3.0D0 * FB * XARG*XARG /
     &       (2.0D0 * BETA1*BETA1 * UP*UP*UP) )**THIRD

      RETURN
      END

      SUBROUTINE CBLPRN ( XARG )
C***********************************************************************
C             CBLPRN Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: To calculate plume rise for the indirect plume in the
C            convective boundary layer
C
C   PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc
C
C   DATE:    September 30, 1993
C
C   CHANGES:  Equation for DHP2 revised per memorandum from Jeff Weil 
C             to AERMIC dated June 20, 1994.
C
C             Added XARG as formal argument for new formulation.
C             Roger Brode, PES, Inc. - 12/5/94
C
C   INPUTS:
C
C   OUTPUTS: Plume Rise, DHP2 (m)
C
C   CALLED FROM:   DELTAH
C
C   Assumptions:
C
C   References:   "A Dispersion Model for the Convective Boundary Layer",
C                 J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG, RSUBH, RYRZ, DELHI

C     Variable Initializations
      MODNAM = 'CBLPRN'

      RSUBH = BETA2 * (ZI - HSP)
      RYRZ  = RSUBH * RSUBH + 0.25D0*ASUBE*(LAMDAY**1.5D0) *
     &                        WSTAR*WSTAR*XARG*XARG/(UP*UP)
      DELHI = DSQRT( (2.D0*FB*ZI)/(ALPHAR*UP*RYRZ) ) * (XARG/UP)
      DHP2  = DELHI

      RETURN
      END

      SUBROUTINE CBLPR3
C***********************************************************************
C             CBLPR3 Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: To calculate plume rise for the penetrated plume in the
C            convective boundary layer
C
C   PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc
C
C   DATE:    September 30, 1993
C
C   INPUTS:  The ratio of delta(Hsub_e) to delta(Hsub_h), HEDHH
C            Mixing height, ZI
C            Source release height, HS
C
C   OUTPUTS: Plume Rise, DHP3 (m)
C
C   CALLED FROM:   DELTAH
C
C   Assumptions:
C
C   References:   "A Dispersion Model for the Convective Boundary
C                  Layer", J. Weil, 8/17/93
C                 "Plume Penetration of the CBL and Source 3: Source
C                  Strength and Plume Rise", J. Weil, 9/1/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'CBLPR3'

C     The plume rise for the penetrated source is delta(Hsub_3), given
C     by Eq. 9 in Jeff Weil's 9/1/93 document.  The variable HEDHH is
C     delta(Hsub_e)/delta(Hsub_h), calculated from Eq. 26a of Jeff Weil's
C     8/17/93 document, where delta(Hsub_h) is ZI-HS.

      IF (PPF .EQ. 1.0D0) THEN
         DHP3 = HEDHH * (ZI-HSP)
      ELSE
         DHP3 = 0.75D0 * (ZI-HSP) * HEDHH + 0.5D0 * (ZI-HSP)
      END IF

      RETURN
      END
