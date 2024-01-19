      SUBROUTINE AVERTS(XVIN,YVIN,XWD,YWD,NUMV)
C***********************************************************************
C*                AVERTS Module of AERMOD Model 
C*
C*       PURPOSE: Calculates coordinates of vertices for Wind
C*                Direction Coordinate system for OPENPIT sources.
C*
C*       PROGRAMMER: Jeff Wang, Roger Brode
C*       MODIFIED:   Jayant Hardikar, Roger Brode (for OPENPIT sources)
C*
C*       DATE:      July 7, 1993
C*
C*       INPUTS:  Source Coordinates for Specific Source
C*                Number of vertices + 1
C*
C*       OUTPUTS: Array of Vertex Coordinates for Specific Source
C*
C*       CALLED FROM:   PITEFF
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: NUMV, NSP

      DOUBLE PRECISION :: XVIN(NVMAX), YVIN(NVMAX)
      DOUBLE PRECISION :: XWD(NVMAX),  YWD(NVMAX)

C*    Variable Initializations
      MODNAM = 'AVERTS'
      
      DO NSP = 1, NUMV
         XWD(NSP) = -(XVIN(NSP)*WDSIN + YVIN(NSP)*WDCOS)
         YWD(NSP) =   XVIN(NSP)*WDCOS - YVIN(NSP)*WDSIN
      END DO

      RETURN
      END

      SUBROUTINE AREAIN
C***********************************************************************
C                 AREAIN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Hourly Concentration for AREA Sources
C                 Using Numerical Integration Algorithm
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED BY R. Brode, PES, Inc., 4/2/99, to change the lower limit
C                 on (ua-ub) from 1.0 to 0.01 meter.  This prevents potential
C                 anomalous results for very small area sources (about
C                 1.0 meter wide).
C
C        MODIFIED BY R. Brode, PES, Inc. to use -3.9 to +3.9 for width of
C                 the plume for internal consistency with PWIDTH.  Also
C                 added error checks on the number of "sides" exceeding
C                 NVMAX, which could happen with complex AREAPOLY shapes.
C                 12/14/98
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Concentration for Particular Source/Receptor Combination
C
C        CALLED FROM:   ACALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      integer :: i
      INTEGER :: KSIDE, NCP, KSP, KS
      DOUBLE PRECISION :: UCRIT, UA, UB, VA, VB, VNMIN, VNMAX, WA, VNORM
      DOUBLE PRECISION :: VAL, DVAL
      LOGICAL QGO

C     Variable Initializations
      MODNAM = 'AREAIN'

C     INITIALIZE VARIABLES FOR INTEGRATION PROCEDURE.
      UCRIT = 1.01D0
      VAL   = 0.0D0
      UVERT = 0.0D0
      VVERT = 0.0D0
      KSIDE = 0

      DO ncp = 1, NVERT
         ua = SPA(ncp,1)
         ub = SPA(ncp+1,1)
         va = SPA(ncp,2)
         vb = SPA(ncp+1,2)
         IF (ua .ge. ucrit) THEN
            kside = kside + 1

            IF (kside .LE. NVMAX+1) THEN
               uvert(kside) = ua
               vvert(kside) = va
            ELSE
C              Write Error Message:  Number of "sides" exceeds NVMAX
               CALL ERRHDL(PATH,MODNAM,'E','406',SRCID(ISRC))
               RUNERR = .TRUE.
               RETURN
            END IF
         END IF
         IF ((ua .ge. ucrit .AND. ub .lt. ucrit) .OR.
     &       (ua .lt. ucrit .AND. ub .ge. ucrit)) THEN
            kside = kside+1
            IF (kside .LE. NVMAX+1) THEN
               uvert(kside) = ucrit
               vvert(kside) = va+(ucrit-ua)*(vb-va)/(ub-ua)
            ELSE
C              Write Error Message:  Number of "sides" exceeds NVMAX
               CALL ERRHDL(PATH,MODNAM,'E','406',SRCID(ISRC))
               RUNERR = .TRUE.
               RETURN
            END IF
         END IF
      END DO

      if( AREADBG )then
         if( .NOT. EVONLY )then
c ---       Non-EVENT processing
          write(AREADBUNT,100) KURDAT, SRCID(ISRC), kside
100       format(/1x,'AREA Debug Sub_AREAIN:   YYMMDDHH = ',I8.8,
     &          //1x, A12,' kside= ',i3,//
     &          '  I          UVERT           VVERT' )
        else
c ---      EVENT processing
          write(AREADBUNT,1001) EVDATE(IEVENT), SRCID(ISRC), kside
1001      format(/1x,'AREA Debug Sub_AREAIN:   YYMMDDHH = ',I8.8,
     &          //1x, A12,' kside= ',i3,//
     &          '  I          UVERT           VVERT' )
        endif
         do i=1,kside
            write(AREADBUNT,101) i, uvert(i), vvert(i)
101         format(1x, i2, 2(2x,f15.6))
         enddo
      endif

      QGO = .FALSE.
      IF (kside .gt. 2) THEN
         QGO = .TRUE.
         vnmin=  3.9D0
         vnmax= -3.9D0
         DO ncp = 1, kside
            ua = uvert(ncp)
            va = vvert(ncp)
            call pwidth(ua,va,vnorm,wa)
            vnvert(ncp) = vnorm
            wvert(ncp)  = wa
            vnmax = MAX(vnorm,vnmax)
            vnmin = MIN(vnorm,vnmin)
         END DO
         IF (vnmin .ge. 3.9D0 .or. vnmax .le. -3.9D0) QGO = .FALSE.

         if( AREADBG )then
           write(AREADBUNT,*)
           write(AREADBUNT,*) ' I          VNVERT           WVERT'
           do i=1,kside
            write(AREADBUNT,101) i,vnvert(i),wvert(i)
           enddo
           write(AREADBUNT,*)
           write(AREADBUNT,102) vnmin, vnmax, qgo
102        format(' VNMIN= ',f15.6,'; VNMAX= ',f15.6,'; QGO= ',l2/)
         end if
      END IF

C     Integrate Between Vertices u(1),u(2) THEN u(2),u(3); etc.
      IF (QGO) THEN
C        MAKE 1st Point Same as Last
         ksp = kside+1
         IF (ksp .LE. NVMAX+1) THEN
            uvert(ksp)  = uvert(1)
            vvert(ksp)  = vvert(1)
            vnvert(ksp) = vnvert(1)
            wvert(ksp)  = wvert(1)
         ELSE
C           Write Error Message:  Number of "sides" exceeds NVMAX
            CALL ERRHDL(PATH,MODNAM,'E','406',SRCID(ISRC))
            RUNERR = .TRUE.
            RETURN
         END IF
         nsegs = 0
         LSEG = .FALSE.
         DO ks = 1, kside
            QGO = .TRUE.
            ivert = ks
            ua = uvert(ks)
            ub = uvert(ks+1)
            dval  = 0.0D0
            IF (DABS(ua-ub) .lt. 0.01D0) QGO = .FALSE.
            IF (QGO .AND. FASTAREA) THEN
               call pside_tox(ua,ub,dval)
            ELSE IF (QGO) THEN
               call pside(ua,ub,dval)
            END IF
            val  = val + dval
         END DO
         IF (nsegs .gt. 0) THEN
            LSEG = .TRUE.
            IF (FASTAREA) THEN
               call pside2_tox(dval)
            ELSE
               call pside2(dval)
            END IF
            val  = val + dval
         END IF

C----    Calculate hourly value, HRVAL
C----    Note that 1/U term is now included in VAL.
         HRVAL(ITYP) = DABS(VAL)*QTK*EMIFAC(ITYP)
      ELSE

         HRVAL(ITYP)  = 0.0D0
      END IF

      IF (DEBUG) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
         CALL DEBOUT
      END IF

      RETURN
      END

      SUBROUTINE PLUMEF(XARG,POUT)
C***********************************************************************
C                 PLUMEF Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Driving Routine for Plume Calculations
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED:   To include call to CRITDS for gases, and to
C                    correct a problem with the AREADPLT option.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C        MODIFIED:   To assign value to XDIST for use in dry depletion.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
C
C        MODIFIED BY R. Brode, PES, Inc. to call separate ADIS_ routines
C                    for sigma-y and sigma-z for optimization. - 12/10/98
C
C        MODIFIED BY D. Strimaitis and Yicheng Zhuang, SRC (for DEPOSITION)
C
C        MODIFIED BY R. Brode, PES, Inc. to move calculation of dispersion
C                    coefficients to a new ADIS subroutine - 7/21/94
C
C        DATE:    September 28, 1993
C
C        INPUTS:  Downwind Distance (in km !)
C                 Source Parameter Arrays
C
C        OUTPUTS: Concentration for Particular Source/Receptor Combination
C                 For A Certain Downwind Distance
C
C        CALLED FROM:   TRAPZD
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Declare Local Variables
      INTEGER :: J
      DOUBLE PRECISION :: XARG, POUT, PTMP, VAL, VT, VN, ADJ

C     Variable Initializations
      MODNAM = 'PLUMEF'

      PTMP  = 0.0D0
      POUT  = 0.0D0

C --- Assign XDIST for use in dry depletion (FUNCTION F2INT)
      XDIST = XARG

C     Determine Deposition Correction Factors
      IF (NPD .EQ. 0 .AND. .NOT.ARDPLETE .AND. (LDGAS .OR. LWGAS)) THEN
         CALL PDEPG (XARG)
      ELSE IF (.NOT.ARDPLETE) THEN
         DQCORG = 1.0D0
         WQCORG = 1.0D0
      END IF
      IF (.NOT.ARDPLETE .AND. (LDPART .OR. LWPART)) THEN
         CALL PDEP (XARG)
      ELSE IF (.NOT.ARDPLETE .AND. NPD .GT. 0) THEN
C        Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0      
         DQCOR = 1.0D0
         WQCOR = 1.0D0
      ENDIF

C     Define plume centroid height (CENTER) for use in
C     inhomogeniety calculations
      CALL CENTROID (XARG)

C     If the atmosphere is unstable and the stack
C     top is below the mixing height, calculate
C     the CBL PDF coefficients                              ---   CALL PDF
      IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         CALL PDF
      ENDIF

C     Determine Effective Plume Height                      ---   CALL HEFF
      CALL HEFF ( XARG )

C     Iterative average through plume rise layer
      CALL IBLVAL (XARG)

C     Call PDF & HEFF again for final CBL plume heights
      IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
         CALL PDF
         CALL HEFF (XARG)
      END IF

C --- Determine lateral term, VAL
C     MODIFIED to NOT compute vn, val for cases with val=1.0, uses LSEG,
C     a logical variable set in PSIDE2, AREAIN to establish case
      IF (LSEG) THEN
C        Calculate vertical dispersion coefficients, SZ
C        Lateral dispersion coefficient not needed since VAL = 1.0
         CALL ADISZ(XARG)
         VAL = 1.0D0
      ELSE
C        Determine width area covered by the plume
         CALL XWIDTH(XARG,VT)
         CALL PWIDTH(XARG,VT,VN,VAL)
C        Calculate vertical dispersion coefficient, SZ
         CALL ADISZ(XARG)
      END IF

      IF (NPD .EQ. 0) THEN
C        Perform calculations for gases
C        Assign plume tilt, HV = 0.0
         HV = 0.0D0

         ADJ = DQCORG * WQCORG

         IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C           Calculate height of the "effective reflecting surface"
            CALL REFL_HT (HE, XARG, 0.0D0, VSIGZ, HSBL)
         ELSEIF ( UNSTAB ) THEN
            HSBL = 0.0D0
         ENDIF

C        Determine the CRITical Dividing Streamline---   CALL CRITDS
         CALL CRITDS (HE)

C        Calculate the fraction of plume below
C        HCRIT, PHEE                               ---   CALL PFRACT
         CALL PFRACT (HE)

C        Calculate FOPT = f(PHEE)                  ---   CALL FTERM
         CALL FTERM

C        Calculate Concentration
         CALL AER_ACHI( XARG, ADJ, VDEPG, 0, VAL, POUT )

      ELSE
C        Perform calculations for particles, loop through particle sizes

         DO J = 1, NPD

C           Calculate Plume Tilt Due to Settling, HV
            HV = (XARG/US) * VGRAV(J)

C           Adjust Jth contribution by mass fraction and source
C           depletion
            ADJ = PHI(J) * DQCOR(J) * WQCOR(J)

            IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C              Calculate height of the "effective reflecting surface"
C              Calculate Settled Plume Height(s), HESETL
               HESETL = MAX( 0.0D0, HE - HV )
               CALL REFL_HT (HESETL, XARG, 0.0D0, VSIGZ, HSBL)
            ELSE IF ( UNSTAB ) THEN
C              Calculate Settled Plume Height(s), HESETL
               HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
               HSBL = 0.0D0
            END IF

C           Determine the CRITical Dividing Streamline---   CALL CRITDS
            CALL CRITDS (HESETL)

C           Calculate the fraction of plume below
C           HCRIT, PHEE                               ---   CALL PFRACT
            CALL PFRACT (HESETL)

C           Calculate FOPT = f(PHEE)                  ---   CALL FTERM
            CALL FTERM

C           Calculate Concentration
            CALL AER_ACHI( XARG, ADJ, VDEP(J), J, VAL, PTMP )
            POUT  = POUT + PTMP

         END DO

      END IF

      RETURN
      END


      SUBROUTINE PSIDE(U1,U2,DVAL)
C***********************************************************************
C                 PSIDE Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: INTEGRATES SIDE K of POLYGON
C                 int f(u)*CNF(v(u)/sig(u))=f(u)*vn(u) from u1 to u2
C                           CNF = cumulative normal distribution
C                 Computes W(1),W(2)--normalized plume width at   u1    u2
C                 Checks for w(i) outside of -3.9,3.9 with i+, i-
C                 L=-3.9  U=3.9  = bounds for testing
C                 Integrates according to case encountered:
C                 situation     CASE    iplus    iminus  integral limits
C                 L<w1,w2<U      1        0        0         u1,u2
C                 w1,w2<L        2        0       1+2      don't compute
C                 w1,w2>U        3       1+2       0         u1,u2
C                 w1<L<w2<U      4        0        1         u-,u2
C                 w2<L<w1<U      5        0        2         u1,u-
C                 L<w1<U<w2      6        2        0       u1,u+  u+,u2
C                 L<w2<U<w1      7        1        0       u1,u+  u+,u2
C                 w1<L<U<w2      8        2        1       u-,u+  u+,u2
C                 w2<L<U<w1      9        1        2       u1,u+  u+,u-
C
C                 u+ = value of u such that w(u)=U=3.9
C                 u- =     "                w(u)=L=-3.9
C                 u+,u- computed with Brent's Algorithm
C
C                 IF uplus >0, part of side is outside plume
C                 but is integrated anyway, unless there is
C                 a corresponding part on another side that will
C                 cause cancellation.  This is determined in
C                 PSIDE2;
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED by Roger Brode, PES, Inc. to use QATR routine for
C                    integration, and to pass VN(1) and VN(2) to
C                    ZBRENT.  Also changed to use -3.9 to +3.9 for
C                    width of plume for internal consistency with
C                    SUBROUTINE PWIDTH. - 12/14/98
C
C        MODIFIED by Roger Brode, PES, Inc. to correct lower integration
C                    limit for Case 4, and to remove extraneous calls
C                    to XWIDTH and PWIDTH after calls to ZBRENT. - 7/29/94
C
C        INPUTS:  End Points of The Segments
C
C        OUTPUTS: Integral Value (if any) for Segment
C
C        CALLED FROM:   AREAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C---- Set convergence criteria for calls to QATR3:
C         NDIM = maximum number of integration steps
C         IMIN = minimum number of integration steps
C         EPSR = relative error tolerance for integral
C         EPST = minimum value threshold for integral
C----
      INTEGER, PARAMETER :: NDIM = 12, IMIN = 5
      DOUBLE PRECISION, PARAMETER :: EPSR = 1.0D-2, EPST = 1.0D-5

      integer icase
      INTEGER :: I, KS, IMINUS, IPLUS, NOUT, ICON
      DOUBLE PRECISION :: DVAL, U1, U2, UMINUS, UPLUS, AUX(NDIM),
C     JAT DO65 8/29/21, V1 AND W SET BUT NEVER USED
C     &                    u(2), v1(2), vn(2), w(2)
     &                    u(2), vn(2)

C     Variable Initializations
      MODNAM = 'PSIDE'

C     NSEG = number of segments; set to 0 in AREAIN
C     for each source/rcvr/time step
      dval  = 0.0D0
C     JAT DO65 8/29/21, V1 AND W SET BUT NEVER USED
      DO i =  1,2
         ks    = ivert + i-1
         u(i)  = uvert(ks)
C         v1(i) = vvert(ks)
         vn(i) = vnvert(ks)
C         w(i)  = wvert(ks)
      END DO

      iminus = 0
      iplus  = 0
      uminus = -1.0D0
      uplus  = -1.0D0
      DO i = 1,2
         IF (vn(i) .lt. -3.9D0) iminus = i + iminus
         IF (vn(i) .gt.  3.9D0) iplus  = i + iplus
      END DO

      IF (iplus.EQ.1 .or. iplus.EQ.2) THEN
         call zbrent( 1,u(1),u(2),vn(1),vn(2),1.0D-3,uplus)
      END IF
      IF (iminus.EQ.1 .or. iminus.EQ.2) THEN
         call zbrent(-1,u(1),u(2),vn(1),vn(2),1.0D-3,uminus)
      END IF

      if( AREADBG) then
         write(AREADBUNT,*)
         write(AREADBUNT,*) 'AREA Debug Sub_PSIDE: '
         write(AREADBUNT,*) '  ISRC   = ',isrc
C ---
         IF( EVONLY )THEN
            write(AREADBUNT,*) '  IEVT   = ',ievent
         ELSE
            write(AREADBUNT,*) '  IREC   = ',irec
         ENDIF
         write(AREADBUNT,*) ' iplus  iminus  case'
         if( iplus.eq.0 .and. iminus.eq.0 )then
            icase = 1
         elseif( iplus.eq.0 .and. iminus.eq.3 )then
            icase = 2
         elseif( iplus.eq.0 .and. iminus.eq.1 )then
            icase = 4
         elseif( iplus.eq.0 .and. iminus.eq.2 )then
            icase = 5
         elseif( iplus.eq.1 .and. iminus.eq.0 )then
            icase = 7
         elseif( iplus.eq.1 .and. iminus.eq.2 )then
            icase = 9
         elseif( iplus.eq.2 .and. iminus.eq.0 )then
            icase = 6
         elseif( iplus.eq.2 .and. iminus.eq.1 )then
            icase = 8
         elseif( iplus.eq.3 .and. iminus.eq.0 )then
            icase = 3
         endif
         write(AREADBUNT,'(I5,I7,I6)') iplus, iminus, icase
         write(AREADBUNT,*)
      endif

c---- CASE DEPENDs on iplus, iminus
      IF (iplus .EQ. 0) THEN
         IF (iminus .EQ. 0) THEN
c                                             iplus  iminus  case
c                                               0     0       1
            call qatr3(u1,u2,epsr,epst,ndim,imin,dval,
     &                 icon,nout,aux)
         ELSE IF (iminus .EQ. 3) THEN
c                                               0     3       2
            dval = 0.0D0
         ELSE IF (iminus .EQ. 1) THEN
c                                               0     1       4
            call qatr3(uminus,u2,epsr,epst,ndim,imin,dval,
     &                 icon,nout,aux)
         ELSE
c                                               0     2       5
            call qatr3(u1,uminus,epsr,epst,ndim,imin,dval,
     &                 icon,nout,aux)
         END IF

      ELSE IF (iplus .EQ. 1) THEN
         nsegs = nsegs+1
         uasegs(nsegs) = u1
         ubsegs(nsegs) = uplus
         IF (iminus .EQ. 0) THEN
c                                               1     0       7
            call qatr3(uplus,u2,epsr,epst,ndim,imin,dval,
     &                 icon,nout,aux)
         ELSE
c                                               1     2       9
            call qatr3(uplus,uminus,epsr,epst,ndim,imin,dval,
     &                 icon,nout,aux)
         END IF

      ELSE IF (iplus .EQ. 2) THEN
         nsegs = nsegs+1
         uasegs(nsegs) = uplus
         ubsegs(nsegs) = u2
         IF (iminus .EQ. 0) THEN
c                                               2     0       6
            call qatr3(u1,uplus,epsr,epst,ndim,imin,dval,
     &                 icon,nout,aux)
         ELSE
c                                               2     1       8
            call qatr3(uminus,uplus,epsr,epst,ndim,imin,dval,
     &                 icon,nout,aux)
         END IF

      ELSE
c                                               3     0       3
         nsegs = nsegs+1
         uasegs(nsegs) = u1
         ubsegs(nsegs) = u2
      END IF

      if( AREADBG )then
         write(AREADBUNT,*)
         write(AREADBUNT,*) '  ISRC   = ',isrc

         IF( EVONLY )THEN
            write(AREADBUNT,*) '  IEVT   = ',ievent
         ELSE
            write(AREADBUNT,*) '  IREC   = ',irec
         ENDIF

         write(AREADBUNT,*) '  NSEGS  = ',nsegs
         if( nsegs .gt. 0 )then
            write(AREADBUNT,*) '  UASEGS = ',uasegs(nsegs)
            write(AREADBUNT,*) '  UBSEGS = ',ubsegs(nsegs)
         else
            write(AREADBUNT,*) '  UASEGS = NA'
            write(AREADBUNT,*) '  UBSEGS = NA'
         endif
         write(AREADBUNT,*) '  DVAL   = ',dval
      endif
  
      RETURN
      END

      SUBROUTINE XWIDTH(U,XOUT)
C***********************************************************************
C                 XWIDTH Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Given Any Y Coordinate of A Vertex of an Area
C                 Source, Calculate the X Coordinate
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:  The Y Coordinate
C
C        OUTPUTS: The X Coordinate Value
C
C        CALLED FROM:   ZBRENT
C                       PSIDE
C                       PLUMEF
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: XOUT, U, U1, U2, V1, V2

C     Variable Initializations
      MODNAM = 'XWIDTH'

      U1 = UVERT(IVERT)
      U2 = UVERT(IVERT+1)
      V1 = VVERT(IVERT)
      V2 = VVERT(IVERT+1)
      XOUT = V1+(U-U1)*(V2-V1)/(U2-U1)

      RETURN
      END

      SUBROUTINE PWIDTH(XARG,V1,VN,WIDTH)
C***********************************************************************
C                 PWIDTH Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates The Effective Area of The Plume for A
C                 Certain Downwind Distance
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED BY R. Brode, PES, Inc. to move calculation of dispersion
C                    coefficients to a new ADIS subroutine - 7/21/94
C
C        MODIFIED BY R. Brode, PES, Inc. to correct table of GA values
C                    and extend GA to 79 values - 7/29/94
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Receptor Height Above Ground
C                 Source Parameter Arrays
C
C        OUTPUTS: The Effective Width
C
C        CALLED FROM:   ZBRENT
C                       PSIDE
C                       PLUMEF
C                       AREAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ITEMP
      DOUBLE PRECISION :: XARG, WIDTH, VN, V1, TEMP, GA(79)

C     Variable Initializations
C     GA ARE VALUES OF THE CUMULATIVE NORMAL DISTRIBUTION IN
C     INCREMENTS OF 0.1 S.
      DATA GA/0.0D0,.0001D0,.0001D0,.0002D0,.0002D0,.0003D0,.0005D0,
     &.0007D0,.0010D0,.0013D0,.0019D0,.0026D0,.0035D0,.0047D0,.0062D0,
     &.0082D0,.0107D0,.0139D0,.0179D0,.0227D0,.0287D0,.0359D0,.0446D0,
     &.0548D0,.0668D0,.0808D0,.0968D0,.1151D0,.1357D0,.1587D0,.1841D0,
     &.2119D0,.2420D0,.2742D0,.3085D0,.3445D0,.3821D0,.4207D0,.4602D0,
     &.5000D0,.5398D0,.5793D0,.6179D0,.6555D0,.6915D0,.7258D0,.7580D0,
     &.7881D0,.8159D0,.8413D0,.8643D0,.8849D0,.9032D0,.9192D0,.9332D0,
     &.9452D0,.9554D0,.9641D0,.9713D0,.9773D0,.9821D0,.9861D0,.9893D0,
     &.9918D0,.9938D0,.9953D0,.9965D0,.9974D0,.9981D0,.9987D0,.9990D0,
     &.9993D0,.9995D0,.9997D0,.9998D0,.9998D0,.9999D0,.9999D0,1.000D0/
     
      MODNAM = 'PWIDTH'

      IF (XARG .EQ. 0.0D0) THEN
         SY = 1.0D0
         VN = V1
         WIDTH = VN
C        Exit Routine
         GO TO 999
      END IF

C     Define plume centroid height (CENTER) for use in
C     inhomogeniety calculations                            ---   CALL CENTROID
      CALL CENTROID (XARG)

C     If the atmosphere is unstable and the stack
C     top is below the mixing height, calculate
C     the CBL PDF coefficients                              ---   CALL PDF
      IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         CALL PDF
      ENDIF

C     Determine Effective Plume Height                      ---   CALL HEFF
      CALL HEFF ( XARG )

C     Iterative average through plume rise layer
      CALL IBLVAL (XARG)

C     Calculate lateral dispersion coefficient, SY          ---   CALL ADISY
      CALL ADISY (XARG)

      VN = V1/SY
      TEMP = 10.0D0*VN + 40.0D0
      ITEMP = IDINT(TEMP)
      WIDTH = 0.0D0

      IF (ITEMP .GT. 78) THEN
         WIDTH = 1.0000D0
      ELSE
         IF (ITEMP .GE. 1) THEN
            WIDTH = GA(ITEMP)+(TEMP-DBLE(ITEMP))*
     &              (GA(ITEMP+1)-GA(ITEMP))
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE ZBRENT(IFD,X1,X2,VN1,VN2,TOL,OUTVAL)
C***********************************************************************
C                 ZBRENT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Divide The Segments According to The Plume Split
C                 And Edge Effects
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Source Parameter Arrays
C
C        OUTPUTS: The Effective Integration Segments
C
C        CALLED FROM:   PSIDE
C***********************************************************************

C     Variable Declarations
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER, PARAMETER :: ITMAX = 10
      DOUBLE PRECISION, PARAMETER :: EPSZ  = 1.0D-3
      INTEGER :: IFD, ITER
      DOUBLE PRECISION :: OUTVAL, TOL, X1, X2, A1, B1, V1, W1, VN, FA, 
     &          FB, FC, C1, D1, E1, TOL1, XM, P1, Q1, R1, S1, VN1, VN2

C     Variable Initializations
      MODNAM = 'ZBRENT'
      
CCRT  Initialize variables, 12/27/2017
      e1 = 0.0D0
      c1 = 0.0D0

      a1 = x1
      b1 = x2
      fa = vn1-DBLE(ifd)*3.9D0
      fb = vn2-DBLE(ifd)*3.9D0

      IF (fb*fa .LE. 0.0D0) THEN
         fc = fb
         DO iter = 1, itmax
            IF (fb*fc .gt. 0.0D0) THEN
               c1 = a1
               fc = fa
               d1 = b1-a1
               e1 = d1
            END IF
            IF (DABS(fc) .lt. DABS(fb)) THEN
               a1 = b1
               b1 = c1
               c1 = a1
               fa = fb
               fb = fc
               fc = fa
            END IF
            tol1 = 2.0D0*epsz*DABS(b1)+0.5D0*tol
            xm = 0.5D0*(c1-b1)
            IF (DABS(xm).le.tol1  .or. fb .EQ. 0.0D0) THEN
               outval = b1
               RETURN
            END IF
            IF (DABS(e1).ge.tol1 .AND. DABS(fa).gt.DABS(fb)) THEN
               s1 = fb/fa
               IF (a1 .EQ. c1) THEN
                  p1 = 2.0D0*xm*s1
                  q1 = 1.0D0-s1
               ELSE
                  q1 = fa/fc
                  r1 = fb/fc
                  p1 = s1*(2.0D0*xm*q1*(q1-r1)-(b1-a1)*(r1-1.0D0))
                  q1 = (q1-1.0D0)*(r1-1.0D0)*(s1-1.0D0)
               END IF
               IF(p1 .gt. 0.0D0) q1 = -q1
               p1 = DABS(p1)
               IF (2.0D0*p1.lt.min(3.0D0*xm*q1-
     &             DABS(tol1*q1),DABS(e1-q1))) THEN
                  e1 = d1
                  d1 = p1/q1
               ELSE
                  d1 = xm
                  e1 = d1
               END IF
            ELSE
               d1 = xm
               e1 = d1
            END IF
            a1 = b1
            fa = fb
            IF (DABS(d1).gt. tol1) THEN
               b1 = b1+d1
            ELSE
               b1 = b1+dsign(tol1,xm)
            END IF
            call xwidth(b1,v1)
            call pwidth(b1,v1,vn,w1)
            fb = vn-DBLE(ifd)*3.9D0
         END DO
         outval = b1
      END IF

      RETURN
      END

      SUBROUTINE PSIDE2(DVAL)
C***********************************************************************
C                 PSIDE2 Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Integrates Over Segments For Which DABS(VN) > VNTEST
C                 Eliminates Overlap of Segments And Useless Integration
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:   Number of The Original Segments
C                  End Points Array of The Segments
C
C        OUTPUT:   The Correction of The Results From PSIDE
C
C        CALLED FROM:   AREAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C---- Set convergence criteria for call to QATR3:
C         NDIM = maximum number of integration steps
C         IMIN = minimum number of integration steps
C         EPSR = relative error tolerance for integral
C         EPST = minimum value threshold for integral
C----
      INTEGER, PARAMETER :: NDIM = 12, IMIN = 5
      DOUBLE PRECISION, PARAMETER :: EPSR = 1.0D-2, EPST = 1.0D-5

      INTEGER :: I, J, ISEG, NPTS, NOUT, ICON
      DOUBLE PRECISION :: DVAL, TEMP, U1, U2, UAV, UBV, TMPVAL, 
     &                    AUX(NDIM)
      DOUBLE PRECISION ulist(nvmax2), useg(nvmax,2)
      integer usign(nvmax), ufac, usegf(nvmax)
      LOGICAL Ltest1,Ltest2

C     Variable Initializations
      MODNAM = 'PSIDE2'

      j = 1
      DO i = 1, nsegs
         ulist(j) = uasegs(i)
         j = j+1
         ulist(j) = ubsegs(i)
         j = j+1
      END DO
      npts = 2*nsegs

      call hpsort(npts,ulist,nvmax2)

      DO i = 1, nsegs
         usign(i) = 1
         IF (uasegs(i) .GT. ubsegs(i)) THEN
            usign(i) = -1
            temp = uasegs(i)
            uasegs(i) = ubsegs(i)
            ubsegs(i) = temp
         END IF
         IF (uasegs(i) .EQ. ubsegs(i)) usign(i) = 0
      END DO
      iseg = 0

      DO i = 2,npts
         u1 = ulist(i-1)
         u2 = ulist(i)
         ufac = 0
c*****
c           compare segment [u1,u2] against each ua,ub
c*****
         IF (u1 .ne. u2) THEN
            DO j = 1, nsegs
               IF (u1.ge.uasegs(j) .AND. u2 .le. ubsegs(j)) THEN
                  ufac = ufac + usign(j)
               END IF
            END DO
c*****
c              make table of segments and factors
c*****
            IF (ufac .ne. 0) THEN
               iseg = iseg+1
               useg(iseg,1) = u1
               useg(iseg,2) = u2
               usegf(iseg) = ufac
            END IF
         END IF
      END DO
c*****
c            CONSOLIDATE SEGMENTS IF iseg>1
c*****
      nsegs = iseg
      IF (nsegs .gt. 1) THEN
         DO iseg = 2, nsegs
            Ltest1 = useg(iseg,1) .EQ. useg(iseg-1,2)
            Ltest2 = usegf(iseg)*usegf(iseg-1) .gt. 0
            IF (Ltest1 .AND. Ltest2) THEN
               usegf(iseg-1) = 0
               useg(iseg,1) = useg(iseg-1,1)
            END IF
         END DO
      END IF
      dval  = 0.0D0
      IF (nsegs .gt. 0) THEN
         DO iseg = 1, nsegs
            IF (usegf(iseg) .ne. 0) THEN
               uav = useg(iseg,1)
               ubv = useg(iseg,2)
               ufac = usegf(iseg)
               call qatr3(uav,ubv,epsr,epst,ndim,imin,tmpval,
     &                    icon,nout,aux)
               dval  = dval + DBLE(ufac)*tmpval
            END IF
         END DO
      END IF

      if( AREADBG )then
         write(AREADBUNT,*)
         write(AREADBUNT,*) 'AREA Debug Sub_PSIDE2: '
         write(AREADBUNT,*) '  ISRC   = ',isrc

         IF( EVONLY )THEN
            write(AREADBUNT,*) '  IEVT   = ',ievent
         ELSE
            write(AREADBUNT,*) '  IREC   = ',irec
         ENDIF

         write(AREADBUNT,*) '  NSEGS  = ',nsegs
         if( nsegs .gt. 0 )then
            write(AREADBUNT,*) '  UASEGS = ',uasegs(nsegs)
            write(AREADBUNT,*) '  UBSEGS = ',ubsegs(nsegs)
         else
            write(AREADBUNT,*) '  UASEGS = NA'
            write(AREADBUNT,*) '  UBSEGS = NA'
         endif
         write(AREADBUNT,*) '  DVAL   = ',dval
      endif

      RETURN
      END

      SUBROUTINE HPSORT(NVAR,UVAR,IDIM)
C***********************************************************************
C                 HPSORT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: A General Program For Heap Sort of An Array
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED BY R. Brode, PES, Inc. to include a single RETURN,
C                    avoided "no path to statement" messages for
C                    some compilers. - 12/1/97
C
C        INPUTS:  The Array To Be Sorted
C
C        OUTPUTS: The Array Sorted
C
C        CALLED FROM:   PSIDE2
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IDIM, NVAR, ILMID, IR
      DOUBLE PRECISION :: RU, UVAR(IDIM)

C     Variable Initializations
      MODNAM = 'HPSORT'

      ILMID = NVAR/2 + 1
      IR = NVAR

10    CONTINUE

      IF (ilmid.gt.1) THEN
         ilmid = ilmid-1
         ru = uvar(ilmid)
      ELSE
         ru = uvar(ir)
         uvar(ir) = uvar(1)
         ir = ir-1
         IF (ir .EQ. 1)THEN
            uvar(1) = ru
C           Processing is done
            GO TO 999
         END IF
      END IF

      i = ilmid
      j = ilmid+ilmid

      DO WHILE (j. le. ir)
         IF (j. lt. ir) THEN
            IF (uvar(j).lt.uvar(j+1) ) j = j+1
         END IF
         IF (ru.lt.uvar(j)) THEN
            uvar(i) = uvar(j)
            i = j
            j = 2*j
         ELSE
            j = ir+1
         END IF
      END DO

      uvar(i) = ru

      GO TO 10

 999  RETURN
      END

C***  End new code for area source numerical integration algorithm - 7/7/93



C***  Subroutines for OPENPIT Source algorithms - 7/19/94


      SUBROUTINE ESCAPE(ICAT)
C***********************************************************************
C*                ESCAPE Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To Calculate Escape Fractions for a Particle
C*                Size Category
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Index for Particle Size Category Being Processed
C*                Gravitational Settling Velocity for Current 
C*                     Particle Size Category & Current Source
C*                10-meter Wind Speed for the Current Hour
C*                Constant ALPHA (= 0.029)
C*                
C*
C*       OUTPUTS: The Escape Fraction for the current Size Category
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ICAT
C*    Variable Initializations
      MODNAM = 'ESCAPE'

      EFRAC(ICAT) = 1.0D0/(1.0D0 + VGRAV(ICAT) / (ALPHA * UREF10) )
      
      RETURN
      END


      SUBROUTINE ADJEMI(ICAT,QPTOT)
C***********************************************************************
C*                ADJEMI Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To Adjust Emission Rate for Current Particle
C*                Size Category Being Processed
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Index for Particle Size Category Being Processed
C*                Escape Fraction for the Current Size Category
C*                Mass Fraction of the Current Size Category
C*                Total Emission Rate Per Unit Area
C*                
C*
C*       OUTPUTS: Adjusted Emission Rate for the Current Size Category
C*                Cumulative Adjusted Emission Rate Over All Categories
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ICAT
      DOUBLE PRECISION :: QPTOT
C*    Variable Initializations
      MODNAM = 'ADJEMI'

      QPART(ICAT) = EFRAC(ICAT) * PHI(ICAT) * QS
      QPTOT = QPTOT + QPART(ICAT)
          
      RETURN
      END


      SUBROUTINE AMFRAC(QPTOT)
C***********************************************************************
C*                AMFRAC Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To Adjust the Mass Fractions for each Particle
C*                Size Category
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Array of Adjusted Emission Rates
C*                Cumulative Adjusted Emission Rate Over All Categories
C*
C*       OUTPUTS: Array of Adjusted Mass Fractions
C*                
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ICAT
      DOUBLE PRECISION :: QPTOT
C*    Variable Initializations
      MODNAM = 'AMFRAC'

      DO ICAT = 1,NPD
         PHI(ICAT) = QPART(ICAT)/QPTOT
      END DO
          
      RETURN
      END


      SUBROUTINE LWIND
C***********************************************************************
C*                LWIND Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To Calculate the Along-Wind Length of the OPENPIT
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Wind Flow Vector for the Current Hour
C*                Angle of the Long OPENPIT dimension from the North
C*                Length of the OPENPIT
C*                Width of the OPENPIT
C*
C*       OUTPUTS: Along-Wind Length of the OPENPIT
C*                
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      DOUBLE PRECISION :: THOUT
      CHARACTER MODNAM*12

C*    Variable Initializations
      MODNAM = 'LWIND'
      THOUT = 0.0D0
      
C*    Determine the Wind Direction Angle Relative to the Long
C*    Axis of the OpenPit
      CALL CTHETA(AFV,PALPHA,THOUT)
C*    Assign THOUT to global variable, THETA
      THETA = THOUT
      
C*    Determine the Along-Wind Length of the OPENPIT
      PITL = PITLEN * (1.0D0 - THETA/90.D0) + PITWID * (THETA / 90.D0)
      
      RETURN
      END


      SUBROUTINE PDEPTH
C***********************************************************************
C*                PDEPTH Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To Calculate the Relative Depth of the OPENPIT Source
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Effective Depth of the OPENPIT
C*                Release Height Above 
C*                Along Wind Length of the OPENPIT
C*                
C*       OUTPUTS: Relative Depth of the OPENPIT
C*                
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C*    Variable Initializations
      MODNAM = 'PDEPTH'

      PDREL = (PDEFF-EMIHGT) / PITL

      RETURN
      END
      

      SUBROUTINE PTFRAC 
C***********************************************************************
C*                PTFRAC Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To Calculate the Fractional Size of the Effective
C*                Area of the OPENPIT Source
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Relative Pit Depth
C*
C*       OUTPUTS: Fractional Size of the Effective Area of the OPENPIT
C*                
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C*    Variable Initializations
      MODNAM = 'PTFRAC'
      
      IF (PDREL .GE. 0.2D0) THEN
         PITFRA = 0.08D0
      ELSE
         PITFRA = DSQRT(1.0D0 - 1.7D0*(PDREL**THIRD) )
      ENDIF
      
      RETURN
      END
      

      SUBROUTINE PITEFF
C**************************************3********************************
C*                PITEFF Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To Determine the Coordinates of the OPENPIT Source
C*                in Wind Direction Coordinate System
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C        MODIFIED:   Moved calculation of center of effective area
C                    source for OPENPIT sources from subroutine ARDIST.
C                    Previous versions would have skipped calculation
C                    of center coordinates if first receptor was located 
C                    inside the actual OPENPIT source.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, XX/YY/2013
C
C*       INPUTS:  
C*
C*       OUTPUTS: Coordinates of the OPENPIT Source in Wind 
C*                Direction Coordinate System
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, II, IUPWND
      DOUBLE PRECISION :: SPAMIN, EFFANG, EFFWID, EFFLEN
      DOUBLE PRECISION :: XTEMP(NVMAX), YTEMP(NVMAX)
      DOUBLE PRECISION, PARAMETER :: EPSLON = 1.0D-05
      
C*    Variable Initializations
      MODNAM = 'PITEFF'
      XTEMP(:) = 0.0D0
      YTEMP(:) = 0.0D0
      
C*    Get Vertices of Actual Pit in WD-Coordinate System    ---   CALL AVERTS
      CALL AVERTS(XVERT,YVERT,XTEMP,YTEMP,NVERT+1)
              
C*    Find the Upwind Vertex of the Pit (one with minimum X)
      SPAMIN = 1.0D+20
      IUPWND = 0
      DO IVERT = 1,NVERT
         IF (XTEMP(IVERT) .LT. SPAMIN) THEN
            IUPWND = IVERT
            SPAMIN = XTEMP(IVERT)-EPSLON
         ENDIF
      END DO

C*    If DEBUG Requested, Write Out Pit Info; include in AREA debug file if
C     specified, otherwise in the MODEL debug file
      IF (AREADBG) THEN 
         WRITE (AREADBUNT,*) 
         WRITE (AREADBUNT,*) 'ACTUAL PIT COORDINATES:'
         WRITE (AREADBUNT,*) '----------------------'
         WRITE (AREADBUNT,8009) 
 8009    FORMAT (' SYSTEM        X1        Y1        X2        Y2',
     &                  '        X3        Y3        X4        Y4'/
     &          ' ---------  -------- ---------  -------- ---------',
     &                    '  -------- ---------  -------- ---------')
         WRITE (AREADBUNT,8000) (XVERT(II), YVERT(II), II=1,NVERT)
         WRITE (AREADBUNT,8100) (XTEMP(II), YTEMP(II), II=1,NVERT)
         WRITE (AREADBUNT,*)
         WRITE (AREADBUNT,*) ' UPWIND VERTEX OF THE PIT        = ', 
     &                                                            IUPWND
         WRITE (AREADBUNT,*) ' WIND DIR W.R.T. PIT LONG AXIS   = ', 
     &                                                            THETA
         WRITE (AREADBUNT,*) ' ALONGWIND LENGTH OF THE PIT     = ', 
     &                                                            PITL
         WRITE (AREADBUNT,*) ' RELATIVE DEPTH OF THE PIT       = ', 
     &                                                            PDREL
         WRITE (AREADBUNT,*)
8000     FORMAT (1X,'User      ',8(f9.0,1x))
8100     FORMAT (1X,'Wind-Dir  ',8(f9.0,1x))
      ELSEIF (DEBUG) THEN
         WRITE (DBGUNT,*) 
         WRITE (DBGUNT,*) 'ACTUAL PIT COORDINATES:'
         WRITE (DBGUNT,*) '----------------------'
         WRITE (DBGUNT,8009) 
         WRITE (DBGUNT,8000) (XVERT(II), YVERT(II), II=1,NVERT)
         WRITE (DBGUNT,8100) (XTEMP(II), YTEMP(II), II=1,NVERT)
         WRITE (DBGUNT,*)
         WRITE (DBGUNT,*) ' UPWIND VERTEX OF THE PIT        = ', IUPWND
         WRITE (DBGUNT,*) ' WIND DIR W.R.T. PIT LONG AXIS   = ', THETA
         WRITE (DBGUNT,*) ' ALONGWIND LENGTH OF THE PIT     = ', PITL
         WRITE (DBGUNT,*) ' RELATIVE DEPTH OF THE PIT       = ', PDREL
         WRITE (DBGUNT,*)
      ENDIF
      
C*    Determine the Angle of the Effective Pit Relative to North
      EFFANG = ANGLE + (90.D0*DBLE(IUPWND - 1))
      
C*    Determine Length and Width Dimensions of the
C*    Effective Pit Area
      EFFWID = PITFRA**(1.0D0 - (DCOS(THETA*DTORAD))**2)*PITWID
      EFFLEN = PITFRA**((DCOS(THETA*DTORAD))**2)*PITLEN

C*    Calculate the Coordinates of the Vertices of the Effective Pit Area
C*    Set Coordinates of Vertices for Rectangular Area (in Kilometers).
C*    Vertices Start with the "Southwest" Corner and Are Defined
C*    Clockwise.  The First Vertex is Repeated as the Last Vertex.


C*    First determine proper 'x-dim' and 'y-dim' for effective area,
C*    taking into account angle of orientation and relation to actual pit.

      IF (XINIT .LE. YINIT .AND. (IUPWND.EQ.1 .OR. IUPWND.EQ.3)) THEN
         XEFF = EFFWID
         YEFF = EFFLEN
      ELSE IF (XINIT.LE.YINIT .AND. (IUPWND.EQ.2 .OR. IUPWND.EQ.4)) THEN
         XEFF = EFFLEN
         YEFF = EFFWID
      ELSE IF (XINIT.GT.YINIT .AND. (IUPWND.EQ.1 .OR. IUPWND.EQ.3)) THEN
         XEFF = EFFLEN
         YEFF = EFFWID
      ELSE IF (XINIT.GT.YINIT .AND. (IUPWND.EQ.2 .OR. IUPWND.EQ.4)) THEN
         XEFF = EFFWID
         YEFF = EFFLEN
      END IF

      XTEMP(1) = XVERT(IUPWND)
      YTEMP(1) = YVERT(IUPWND)

      XTEMP(2) = XTEMP(1) + (YEFF*DSIN(EFFANG*DTORAD))
      YTEMP(2) = YTEMP(1) + (YEFF*DCOS(EFFANG*DTORAD))

      XTEMP(3) = XTEMP(2) + (XEFF*DCOS(EFFANG*DTORAD))
      YTEMP(3) = YTEMP(2) - (XEFF*DSIN(EFFANG*DTORAD))

      XTEMP(4) = XTEMP(3) - (YEFF*DSIN(EFFANG*DTORAD))
      YTEMP(4) = YTEMP(3) - (YEFF*DCOS(EFFANG*DTORAD))

      XTEMP(5) = XVERT(IUPWND)
      YTEMP(5) = YVERT(IUPWND)


C*    Calculate Coordinates of the Effective Pit Area in
C*    Wind Direction Coordinate System                      ---   CALL AVERTS
      CALL AVERTS(XTEMP,YTEMP,XVERT,YVERT,NVERT+1)

C*    If DEBUG Requested, Write Out Pit Info; include in AREA debug file if
C     specified, otherwise in the MODEL debug file
      IF (AREADBG) THEN 
         WRITE (AREADBUNT,*) 
         WRITE (AREADBUNT,*) 'EFFECTIVE PIT COORDINATES:'
         WRITE (AREADBUNT,*) '-------------------------'
         WRITE (AREADBUNT,8009) 

         WRITE (AREADBUNT,8000) (XTEMP(II), YTEMP(II), II=1,NVERT)
         WRITE (AREADBUNT,8100) (XVERT(II), YVERT(II), II=1,NVERT)
         WRITE (AREADBUNT,*)

         WRITE (AREADBUNT,*) ' EFFECTIVE PIT LENGTH            = ', 
     &                                                            EFFLEN
         WRITE (AREADBUNT,*) ' EFFECTIVE PIT WIDTH             = ', 
     &                                                            EFFWID
         WRITE (AREADBUNT,*) ' ORIENTATION RELATIVE TO NORTH   = ', 
     &                                                            EFFANG
         WRITE (AREADBUNT,*) ' FRACTIONAL SIZE OF EFF PIT AREA = ', 
     &                                                            PITFRA
      ELSEIF (DEBUG) THEN
         WRITE (DBGUNT,*) 
         WRITE (DBGUNT,*) 'EFFECTIVE PIT COORDINATES:'
         WRITE (DBGUNT,*) '-------------------------'
         WRITE (DBGUNT,8009) 

         WRITE (DBGUNT,8000) (XTEMP(II), YTEMP(II), II=1,NVERT)
         WRITE (DBGUNT,8100) (XVERT(II), YVERT(II), II=1,NVERT)
         WRITE (DBGUNT,*)

         WRITE (DBGUNT,*) ' EFFECTIVE PIT LENGTH            = ', EFFLEN
         WRITE (DBGUNT,*) ' EFFECTIVE PIT WIDTH             = ', EFFWID
         WRITE (DBGUNT,*) ' ORIENTATION RELATIVE TO NORTH   = ', EFFANG
         WRITE (DBGUNT,*) ' FRACTIONAL SIZE OF EFF PIT AREA = ', PITFRA
      ENDIF

C     Reassign Effective Area Coordinates to Global Arrays for Subsequent Calcs.
      DO I = 1, 5
         XVERT(I) = XTEMP(I)
         YVERT(I) = YTEMP(I)
      END DO

C --- Calculate coordinates for center of the effective pit source
C     (formerly done in subroutine ARDIST)
      XCNTR = 0.0D0
      YCNTR = 0.0D0
      DO I = 1, NVERT
         XCNTR = XCNTR + XVERT(I)
         YCNTR = YCNTR + YVERT(I)
      END DO
      XCNTR = XCNTR/DBLE(NVERT)
      YCNTR = YCNTR/DBLE(NVERT)

C*    If DEBUG Requested, Write Out coordinates for center of effective pit source;
C     include in AREA debug file if specified, otherwise in the MODEL debug file
      IF (AREADBG) THEN 
         WRITE (AREADBUNT,*) 
         WRITE (AREADBUNT,*) ' X-COORD FOR CENTER OF EFF PIT   = ', 
     &                                                          XCNTR
         WRITE (AREADBUNT,*) ' Y-COORD FOR CENTER OF EFF PIT   = ', 
     &                                                          YCNTR
      ELSEIF (DEBUG) THEN
         WRITE (DBGUNT,*) 
         WRITE (DBGUNT,*) ' X-COORD FOR CENTER OF EFF PIT   = ', 
     &                                                          XCNTR
         WRITE (DBGUNT,*) ' Y-COORD FOR CENTER OF EFF PIT   = ', 
     &                                                          YCNTR
      ENDIF

      RETURN
      END


      SUBROUTINE PITEMI(QPTOT)
C***********************************************************************
C*                PITEMI Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To Determine the Emission Rate for the Effective 
C*                Pit Area
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Fractional Area of the Pit
C*                Total Adjusted Emission Rate
C*
C*       OUTPUTS: Emission Rate for the Effective Area of the Current
C*                OPENPIT Source
C*                
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: QPTOT
C*    Variable Initializations
      MODNAM = 'PITEMI'
      
      QEFF = QPTOT / PITFRA
      
      RETURN
      END


      SUBROUTINE CTHETA(AFVIN,ALFIN,THOUT)
C***********************************************************************
C*                CTHETA Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To Determine the Wind Direction Angle Relative to 
C*                the Pit Long Axis
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 26, 1994
C*
C*       INPUTS:  Flow Vector 
C*                Angle of Pit Long Axis from North
C*
C*       OUTPUTS: THETA - Wind Direction Angle Relative to 
C*                the Pit Long Axis
C*                
C*
C*       CALLED FROM:   LWIND
C***********************************************************************

      IMPLICIT NONE

      DOUBLE PRECISION :: THOUT, ALFIN, AFVIN, THETA

      if (dabs(AFVIN-ALFIN) .le. 90.0D0) then
         THOUT = dabs(AFVIN-ALFIN)
      else if (AFVIN .gt. ALFIN) then
         theta = AFVIN - ALFIN
         if (theta .gt. 90.0D0) then
            theta = AFVIN-180.0D0 - ALFIN
         endif
         if (theta .gt. 90.0D0) then
            theta = AFVIN-360.0D0 - ALFIN
         endif
         if (theta .gt. 90.0D0) then
            theta = AFVIN-540.0D0 - ALFIN
         endif
         THOUT = dabs(theta)
      else if (AFVIN .lt. ALFIN) then
         theta = AFVIN - ALFIN
         if (theta .lt. -90.0D0) then
            theta = AFVIN + 180.0D0 - ALFIN
         endif
         if (theta .lt. -90.0D0) then
            theta = AFVIN + 360.0D0 - ALFIN
         endif
         if (theta .lt. -90.0D0) then
            theta = AFVIN + 540.0D0 - ALFIN
         endif
         THOUT = dabs(theta)
      endif
      
      RETURN
      end
      
c-----------------------------------------------------------------------
      subroutine qatr3(xl,xu,eps,epst,ndim,imin,y,ier,i,aux)
c-----------------------------------------------------------------------
c
c --- AERMOD    QATR3
c
c PURPOSE:      Integration routine adapted from the IBM SSP program
c               DQATR.  Modified for single precision.  This is a COPY
c               of QATR for use in area source integrations.
c
c ARGUMENTS:
c    PASSED:    xl,xu   lower and upper limits of integration        [r]
c               eps     fractional error used to define convergence  [r]
c               epst    lower theshold check for value of integral   [r]
c               ndim    dimension of array aux (max no. of steps)    [i]
c               imin    minimum number of "steps" for integral       [i]
c               fct     external function (integrand)                [r]
c               aux     working array, passed to allow variable dim. [r]
c  RETURNED:    y       value of integral                            [r]
c               ier     status flag at termination                   [i]
c               i       number of subdivision steps                  [i]
c
c CALLING ROUTINES:     PSIDE, PSIDE2
c
c EXTERNAL ROUTINES:    none
c-----------------------------------------------------------------------

c  NOTES: status flags denote the following --
c               ier=0   value of integral converged to within eps
c               ier=1   value of integral is diverging (not used)
c               ier=2   value of integral did not converge to within
c                       eps before ndim limit was reached

      IMPLICIT NONE

      INTEGER :: NDIM, IMIN, IER
      DOUBLE PRECISION :: Y, EPS, EPST, XU, XL, H, HH, DELT2, P,
C     JAT D065 8/9/21 DELT1 SET BUT NOT USED
C     &           DELT1, HD, X, SM, Q, AUX(NDIM),
     &           HD, X, SM, Q, AUX(NDIM),
     &           P1, P2
      integer :: i, ii, ji, j, jj

c---  Initialize AUX array
      AUX(:)  = 0.0D0

c---  Preparations for Romberg loop
      CALL PLUMEF(XL,P1)
      CALL PLUMEF(XU,P2)
      aux(1) = 0.5D0 * (P1+P2)
      h = xu - xl

      if(h .EQ. 0.0D0 .OR. aux(1) .EQ. 0.0D0) then
         ier=0
         y  = 0.0D0
         return
      endif

      hh = h
      delt2 = 0.D0
      p  = 1.0D0
      jj = 1
      
C     JAT D065 8/9/21 DELT1 SET BUT NOT USED
      do i = 2, ndim
         y = aux(1)
C         delt1 = delt2
         hd = hh
         hh = 0.5D0 * hh
         p  = 0.5D0 * p
         x  = xl + hh
         sm  = 0.0D0

         do j = 1, jj
            CALL PLUMEF(X,P1)
            sm  = sm + P1
            x   = x + hd
         end do

c----    A new approximation to the integral is computed by means
c        of the trapezoidal rule
         aux(i)  = 0.5D0*aux(i-1) + p*sm

c----    Start of Rombergs extrapolation method

         q  = 1.0D0
         ji = i-1
         do j = 1, ji
            ii = i-j
            q  = q+q
            q  = q+q
            aux(ii)  = aux(ii+1) + (aux(ii+1)-aux(ii))/(q-1.0D0)
         end do

c----    End of Romberg step

c        Compute absolute error, delt2
         delt2 = DABS(y-aux(1))

         if (i .GE. imin) then
c           Check for covergence of algorithm
            if (DABS(aux(1)) .LT. epst) then
c              Lower threshold convergence test
               ier = 0
               y  = h*aux(1)
               return
            else if (delt2 .LE. eps*DABS(aux(1)) ) then
c              Relative error convergence test
               ier = 0
               y  = h*aux(1)
               return
            else if (dabs(hh) .LT. 1.0D0) then
c              Minimum "delta-x" convergence test; < 1.0m
               ier = 0
               y  = h*aux(1)
               return
            end if
         end if

         jj = jj+jj
      end do

c     Convergence not reached within maximum number of steps
      ier = 2
      y  = h*aux(1)

      return
      end

      SUBROUTINE PSIDE_TOX(U1,U2,DVAL)
C***********************************************************************
C                 PSIDE_TOX Module of the AMS/EPA Regulatory Model - AERMOD
C
C        Special version of PSIDE optimized for TOXICS applications.
C        Utilizes Romberg Integration (QATR3) or Gaussian Quadrature (QG2)
C        depending on the source receptor geometry.
C
C        PURPOSE: INTEGRATES SIDE K of POLYGON
C                 int f(u)*CNF(v(u)/sig(u))=f(u)*vn(u) from u1 to u2
C                           CNF = cumulative normal distribution
C                 Computes W(1),W(2)--normalized plume width at   u1    u2
C                 Checks for w(i) outside of -3.9,3.9 with i+, i-
C                 L=-3.9  U=3.9  = bounds for testing
C                 Integrates according to case encountered:
C                 situation     CASE    iplus    iminus  integral limits
C                 L<w1,w2<U      1        0        0         u1,u2
C                 w1,w2<L        2        0       1+2      don't compute
C                 w1,w2>U        3       1+2       0         u1,u2
C                 w1<L<w2<U      4        0        1         u-,u2
C                 w2<L<w1<U      5        0        2         u1,u-
C                 L<w1<U<w2      6        2        0       u1,u+  u+,u2
C                 L<w2<U<w1      7        1        0       u1,u+  u+,u2
C                 w1<L<U<w2      8        2        1       u-,u+  u+,u2
C                 w2<L<U<w1      9        1        2       u1,u+  u+,u-
C
C                 u+ = value of u such that w(u)=U=3.9
C                 u- =     "                w(u)=L=-3.9
C                 u+,u- computed with Brent's Algorithm
C
C                 IF uplus >0, part of side is outside plume
C                 but is integrated anyway, unless there is
C                 a corresponding part on another side that will
C                 cause cancellation.  This is determined in
C                 PSIDE2;
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED by Roger Brode, PES, Inc. to use QATR routine for
C                    integration, and to pass VN(1) and VN(2) to
C                    ZBRENT.  Also changed to use -3.9 to +3.9 for
C                    width of plume for internal consistency with
C                    SUBROUTINE PWIDTH. - 12/14/98
C
C        MODIFIED by Roger Brode, PES, Inc. to correct lower integration
C                    limit for Case 4, and to remove extraneous calls
C                    to XWIDTH and PWIDTH after calls to ZBRENT. - 7/29/94
C
C        INPUTS:  End Points of The Segments
C
C        OUTPUTS: Integral Value (if any) for Segment
C
C        CALLED FROM:   AREAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C---- Set convergence criteria for calls to QATR3:
C         NDIM = maximum number of integration steps
C         IMIN = minimum number of integration steps
C         EPSR = relative error tolerance for integral
C         EPST = minimum value threshold for integral
C----
      INTEGER, PARAMETER :: NDIM = 10, IMIN = 4
      DOUBLE PRECISION, PARAMETER :: EPSR = 2.0D-2, EPST = 1.0D-5

C---- Set distance factor for switching to Gaussian Quadrature, QG_FACT
C     If Xmax - Xmin is .LT. QG_FACT*Xmin, then use QG2, where
C     Xmax and Xmin are the distances to the endpoints of the side.
      DOUBLE PRECISION, PARAMETER :: QG_FACT = 5.0D0

      integer icase
      INTEGER :: I, KS, IMINUS, IPLUS, NOUT, ICON
      DOUBLE PRECISION :: DVAL, U1, U2, UMINUS, UPLUS, AUX(NDIM),
C     JAT DO65 8/29/21, V1 AND W SET BUT NEVER USED
C     &                    u(2), v1(2), vn(2), w(2)
     &                    u(2), vn(2)

C     Variable Initializations
      MODNAM = 'PSIDE_TOX'

C     NSEG = number of segments; set to 0 in AREAIN
C     for each source/rcvr/time step
      dval  = 0.0D0
C     JAT DO65 8/29/21, V1 AND W SET BUT NEVER USED
      DO i =  1,2
         ks    = ivert + i-1
         u(i)  = uvert(ks)
C         v1(i) = vvert(ks)
         vn(i) = vnvert(ks)
C         w(i)  = wvert(ks)
      END DO

      iminus = 0
      iplus  = 0
      uminus = -1.0D0
      uplus  = -1.0D0
      DO i = 1,2
         IF (vn(i) .lt. -3.9D0) iminus = i + iminus
         IF (vn(i) .gt.  3.9D0) iplus  = i + iplus
      END DO

      IF (iplus.EQ.1 .or. iplus.EQ.2) THEN
         call zbrent( 1,u(1),u(2),vn(1),vn(2),1.0D-3,uplus)
      END IF
      IF (iminus.EQ.1 .or. iminus.EQ.2) THEN
         call zbrent(-1,u(1),u(2),vn(1),vn(2),1.0D-3,uminus)
      END IF

      if( AREADBG) then
         write(AREADBUNT,*)
         write(AREADBUNT,*) 'AREA Debug Sub_PSIDE_TOX: '
         write(AREADBUNT,*) '  ISRC   = ',isrc

         IF( EVONLY )THEN
            write(AREADBUNT,*) '  IEVT   = ',ievent
         ELSE
            write(AREADBUNT,*) '  IREC   = ',irec
         ENDIF

         write(AREADBUNT,*) ' iplus  iminus  case'
         if( iplus.eq.0 .and. iminus.eq.0 )then
            icase = 1
         elseif( iplus.eq.0 .and. iminus.eq.3 )then
            icase = 2
         elseif( iplus.eq.0 .and. iminus.eq.1 )then
            icase = 4
         elseif( iplus.eq.0 .and. iminus.eq.2 )then
            icase = 5
         elseif( iplus.eq.1 .and. iminus.eq.0 )then
            icase = 7
         elseif( iplus.eq.1 .and. iminus.eq.2 )then
            icase = 9
         elseif( iplus.eq.2 .and. iminus.eq.0 )then
            icase = 6
         elseif( iplus.eq.2 .and. iminus.eq.1 )then
            icase = 8
         elseif( iplus.eq.3 .and. iminus.eq.0 )then
            icase = 3
         endif
         write(AREADBUNT,'(I5,I7,I6)') iplus, iminus, icase
         write(AREADBUNT,*)
      endif

c---- CASE DEPENDs on iplus, iminus
      IF (iplus .EQ. 0) THEN
         IF (iminus .EQ. 0) THEN
c                                             iplus  iminus  case
c                                               0     0       1
            if (DABS(u2-u1) .lt. QG_FACT*min(u1,u2)) then
               call qg2(u1,u2,dval)
            else
               call qatr3(u1,u2,epsr,epst,ndim,imin,dval,
     &                    icon,nout,aux)
            end if
         ELSE IF (iminus .EQ. 3) THEN
c                                               0     3       2
            dval = 0.0D0
         ELSE IF (iminus .EQ. 1) THEN
c                                               0     1       4
            if (DABS(u2-uminus) .lt. QG_FACT*min(uminus,u2)) then
               call qg2(uminus,u2,dval)
            else
               call qatr3(uminus,u2,epsr,epst,ndim,imin,dval,
     &                    icon,nout,aux)
            end if
         ELSE
c                                               0     2       5
            if (dabs(uminus-u1) .lt. QG_FACT*min(u1,uminus)) then
               call qg2(u1,uminus,dval)
            else
               call qatr3(u1,uminus,epsr,epst,ndim,imin,dval,
     &                    icon,nout,aux)
            end if
         END IF

      ELSE IF (iplus .EQ. 1) THEN
         nsegs = nsegs+1
         uasegs(nsegs) = u1
         ubsegs(nsegs) = uplus
         IF (iminus .EQ. 0) THEN
c                                               1     0       7
            if (DABS(u2-uplus) .lt. QG_FACT*min(uplus,u2)) then
               call qg2(uplus,u2,dval)
            else
               call qatr3(uplus,u2,epsr,epst,ndim,imin,dval,
     &                    icon,nout,aux)
            end if
         ELSE
c                                               1     2       9
            if (DABS(uminus-uplus) .lt. QG_FACT*min(uplus,uminus)) then
               call qg2(uplus,uminus,dval)
            else
               call qatr3(uplus,uminus,epsr,epst,ndim,imin,dval,
     &                    icon,nout,aux)
            end if
         END IF

      ELSE IF (iplus .EQ. 2) THEN
         nsegs = nsegs+1
         uasegs(nsegs) = uplus
         ubsegs(nsegs) = u2
         IF (iminus .EQ. 0) THEN
c                                               2     0       6
            if (DABS(uplus-u1) .lt. QG_FACT*min(u1,uplus)) then
               call qg2(u1,uplus,dval)
            else
               call qatr3(u1,uplus,epsr,epst,ndim,imin,dval,
     &                    icon,nout,aux)
            end if
         ELSE
c                                               2     1       8
            if (DABS(uplus-uminus) .lt. QG_FACT*min(uminus,uplus)) then
               call qg2(uminus,uplus,dval)
            else
               call qatr3(uminus,uplus,epsr,epst,ndim,imin,dval,
     &                    icon,nout,aux)
            end if
         END IF

      ELSE
c                                               3     0       3
         nsegs = nsegs+1
         uasegs(nsegs) = u1
         ubsegs(nsegs) = u2
      END IF

      if( AREADBG )then
         write(AREADBUNT,*)
         write(AREADBUNT,*) '  ISRC   = ',isrc

         IF( EVONLY )THEN
            write(AREADBUNT,*) '  IEVT   = ',ievent
         ELSE
            write(AREADBUNT,*) '  IREC   = ',irec
         ENDIF
         write(AREADBUNT,*) '  NSEGS  = ',nsegs
         if( nsegs .gt. 0 )then
            write(AREADBUNT,*) '  UASEGS = ',uasegs(nsegs)
            write(AREADBUNT,*) '  UBSEGS = ',ubsegs(nsegs)
         else
            write(AREADBUNT,*) '  UASEGS = NA'
            write(AREADBUNT,*) '  UBSEGS = NA'
         endif
         write(AREADBUNT,*) '  DVAL   = ',dval
      endif

      RETURN
      END

      SUBROUTINE PSIDE2_TOX(DVAL)
C***********************************************************************
C                 PSIDE2_TOX Module of the AMS/EPA Regulatory Model - AERMOD
C
C        Special version of PSIDE2_TOX optimized for TOXICS applications.
C        Utilizes Romberg Integration (QATR3) or Gaussian Quadrature (QG2)
C        depending on the source receptor geometry.
C
C        PURPOSE: Integrates Over Segments For Which DABS(VN) > VNTEST
C                 Eliminates Overlap of Segments And Useless Integration
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:   Number of The Original Segments
C                  End Points Array of The Segments
C
C        OUTPUT:   The Correction of The Results From PSIDE
C
C        CALLED FROM:   AREAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C---- Set convergence criteria for call to QATR3:
C         NDIM = maximum number of integration steps
C         IMIN = minimum number of integration steps
C         EPSR = relative error tolerance for integral
C         EPST = minimum value threshold for integral
C----
      INTEGER, PARAMETER :: NDIM = 10, IMIN = 4
      DOUBLE PRECISION, PARAMETER :: EPSR = 2.0D-2, EPST = 1.0D-5

C---- Set distance factor for switching to Gaussian Quadrature, QG_FACT
C     If Xmax - Xmin is .LT. QG_FACT*Xmin, then use QG2, where
C     Xmax and Xmin are the distances to the endpoints of the side.
      DOUBLE PRECISION, PARAMETER :: QG_FACT = 5.0D0

      INTEGER :: I, J, ISEG, NPTS, NOUT, ICON
      DOUBLE PRECISION :: DVAL, TEMP, U1, U2, UAV, UBV, TMPVAL, 
     &                    AUX(NDIM)
      DOUBLE PRECISION :: ulist(nvmax2), useg(nvmax,2)
      integer usign(nvmax), ufac, usegf(nvmax)
      LOGICAL Ltest1,Ltest2

C     Variable Initializations
      MODNAM = 'PSIDE2_TOX'

      j = 1
      DO i = 1, nsegs
         ulist(j) = uasegs(i)
         j = j+1
         ulist(j) = ubsegs(i)
         j = j+1
      END DO
      npts = 2*nsegs

      call hpsort(npts,ulist,nvmax2)

      DO i = 1, nsegs
         usign(i) = 1
         IF (uasegs(i) .GT. ubsegs(i)) THEN
            usign(i) = -1
            temp = uasegs(i)
            uasegs(i) = ubsegs(i)
            ubsegs(i) = temp
         END IF
         IF (uasegs(i) .EQ. ubsegs(i)) usign(i) = 0
      END DO
      iseg = 0

      DO i = 2,npts
         u1 = ulist(i-1)
         u2 = ulist(i)
         ufac = 0
c*****
c           compare segment [u1,u2] against each ua,ub
c*****
         IF (u1 .ne. u2) THEN
            DO j = 1, nsegs
               IF (u1.ge.uasegs(j) .AND. u2 .le. ubsegs(j)) THEN
                  ufac = ufac + usign(j)
               END IF
            END DO
c*****
c              make table of segments and factors
c*****
            IF (ufac .ne. 0) THEN
               iseg = iseg+1
               useg(iseg,1) = u1
               useg(iseg,2) = u2
               usegf(iseg) = ufac
            END IF
         END IF
      END DO
c*****
c            CONSOLIDATE SEGMENTS IF iseg>1
c*****
      nsegs = iseg
      IF (nsegs .gt. 1) THEN
         DO iseg = 2, nsegs
            Ltest1 = useg(iseg,1) .EQ. useg(iseg-1,2)
            Ltest2 = usegf(iseg)*usegf(iseg-1) .gt. 0
            IF (Ltest1 .AND. Ltest2) THEN
               usegf(iseg-1) = 0
               useg(iseg,1) = useg(iseg-1,1)
            END IF
         END DO
      END IF
      dval  = 0.0D0
      IF (nsegs .gt. 0) THEN
         DO iseg = 1, nsegs
            IF (usegf(iseg) .ne. 0) THEN
               uav = useg(iseg,1)
               ubv = useg(iseg,2)
               ufac = usegf(iseg)
               if (DABS(ubv-uav) .lt. QG_FACT*min(uav,ubv)) then
                  call qg2(uav,ubv,tmpval)
               else
                  call qatr3(uav,ubv,epsr,epst,ndim,imin,tmpval,
     &                       icon,nout,aux)
               end if
               dval  = dval + DBLE(ufac)*tmpval
            END IF
         END DO
      END IF

      if( AREADBG )then
         write(AREADBUNT,*)
         write(AREADBUNT,*) 'AREA Debug Sub_PSIDE2_TOX: '
         write(AREADBUNT,*) '  ISRC   = ',isrc

         IF( EVONLY )THEN
            write(AREADBUNT,*) '  IEVT   = ',ievent
         ELSE
            write(AREADBUNT,*) '  IREC   = ',irec
         ENDIF
         write(AREADBUNT,*) '  NSEGS  = ',nsegs
         if( nsegs .gt. 0 )then
            write(AREADBUNT,*) '  UASEGS = ',uasegs(nsegs)
            write(AREADBUNT,*) '  UBSEGS = ',ubsegs(nsegs)
         else
            write(AREADBUNT,*) '  UASEGS = NA'
            write(AREADBUNT,*) '  UBSEGS = NA'
         endif
         write(AREADBUNT,*) '  DVAL   = ',dval
      endif

      RETURN
      END

C
C     ..................................................................
C
C        SUBROUTINE QG2
C
C        PURPOSE
C           TO COMPUTE INTEGRAL(FCT(X), SUMMED OVER X FROM XL TO XU)
C
C        USAGE
C           CALL QG3 (XL,XU,FCT,Y)
C           PARAMETER FCT REQUIRES AN EXTERNAL STATEMENT
C
C        DESCRIPTION OF PARAMETERS
C           XL     - THE LOWER BOUND OF THE INTERVAL.
C           XU     - THE UPPER BOUND OF THE INTERVAL.
C           FCT    - THE NAME OF AN EXTERNAL FUNCTION SUBPROGRAM USED.
C           Y      - THE RESULTING INTEGRAL VALUE.
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           THE EXTERNAL FUNCTION SUBPROGRAM FCT(X) MUST BE FURNISHED
C           BY THE USER.
C
C        METHOD
C           EVALUATION IS DONE BY MEANS OF 2-POINT GAUSS QUADRATURE
C           FORMULA, WHICH INTEGRATES POLYNOMIALS UP TO DEGREE 3
C           EXACTLY.
C           FOR REFERENCE, SEE
C           V.I.KRYLOV, APPROXIMATE CALCULATION OF INTEGRALS,
C           MACMILLAN, NEW YORK/LONDON, 1962, PP.100-111 AND 337-338.
C
C     ..................................................................
C
      SUBROUTINE QG2(XL,XU,Y)
C
C
      IMPLICIT NONE

      DOUBLE PRECISION :: A, B, Y, XL, XU, P1, P2

      A = 0.5D0*(XU+XL)
      B = XU-XL
      Y = 0.2886751D0*B
      CALL PLUMEF(A+Y,P1)
      CALL PLUMEF(A-Y,P2)
      Y = 0.5D0*B*(P1+P2)

      RETURN
      END
