      SUBROUTINE BL_CALC (KK)
C***********************************************************************
C             BL_CALC Routine of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates concentration or deposition values
C                 for BUOYANT LINE sources
C                 (Adpated from the BLP source code)
C
C        PROGRAMMER: Amec Foster Wheeler
C
C        DATE:    June 30, 2015
C
C        MODIFIED:
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: 1-hr CONC or DEPOS Values for Each Receptor for
C                 Particular Source
C
C        CALLED FROM:   CALC
C
C        Assumptions:
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE

      IMPLICIT NONE

      CHARACTER MODNAM*12

      DOUBLE PRECISION, PARAMETER  :: BLCRIT = 0.02D0
      DOUBLE PRECISION, PARAMETER  :: SRT2DP = 0.7978846D0
      DOUBLE PRECISION, PARAMETER  :: AVFACT = 1.0D0
      DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::
     &        WSPEXP = [0.10D0, 0.15D0, 0.20D0, 0.25D0, 0.30D0, 0.30D0]
      DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::
     &         TERAN = [0.5D0, 0.5D0, 0.5D0, 0.5D0, 0.30D0, 0.30D0]
      DOUBLE PRECISION, PARAMETER, DIMENSION(2) ::
     &         DTHTA = [0.02D0, 0.035D0]

      INTEGER :: I, KK, NBLINES
C Unused:      INTEGER :: J, K, INOUT
      INTEGER, PARAMETER :: MAXIT  = 14
      INTEGER, PARAMETER, DIMENSION(7) :: NSEGA = [3,5,9,17,33,65,129]

      DOUBLE PRECISION   :: DECFAC, FTSAVE(129)
      DOUBLE PRECISION   :: BL_XVZ, BL_XVY
      DOUBLE PRECISION   :: BL_UEFF, BL_THETA
C JAT 06/22/21 D065 REMOVE DPBL AS UNUSED VARIABLE
C      DOUBLE PRECISION   :: P, S, TER1, FBRG, WSST, DPBL, CUQ, SUMFPRM
      DOUBLE PRECISION   :: P, S, TER1, FBRG, WSST, CUQ, SUMFPRM
      DOUBLE PRECISION   :: RI, UNSRT, XFSXX, DLMIN, ZLINEHS, ZLINEBASE
      DOUBLE PRECISION   :: YV, ZV, XB, YB, XE, YE, XMAXL, YMAXL, XMINL
      DOUBLE PRECISION   :: YMINL, DXEL, DYEL, SUMM, XRECEP, THT, HNT
      DOUBLE PRECISION   :: DOWNX, CROSSY, SY0, SZ0, SYC, YRECEP
      DOUBLE PRECISION   :: XRMXKM, YLOW, YHIGH, VIRTXZ, VIRTXY
      DOUBLE PRECISION   :: VXYKM, VXZKM, SIGY, SIGZ, Z
      DOUBLE PRECISION   :: TERRAN, H, FT, DELTAT, SUM2, DIFF, CORR
      DOUBLE PRECISION   :: XSEG1, XDIFF, YSEG1, YDIFF, WEIGHT
      DOUBLE PRECISION   :: XCLN, YCLN, EM, B, SFCZ0_SAV
! CRCO D095 added for BLP debug
      DOUBLE PRECISION   :: FPLMHT
      INTEGER :: JITCT, IWPBL, ITHETA, IDELTA, IWOSIG, IDW, IDIV
      INTEGER :: IDIST, ISEG, ISEG2, ISEG3, ITER, INDL, I2, IG
      INTEGER :: IGSUB, NSEG, NSEG0, NSEGM1, INDLSV, INDLLN, NNEW
      INTEGER :: NCONTRIB, IBMIN, IBMAX, IBMAX1, IH, IGMAX, IGMAX1
      INTEGER :: LNUM, KSRC, LSRC, ISRC_SAV, KST_SAV, BL_URBKST

C     Variable Initializations
      MODNAM = 'BL_CALC'
      JITCT  = 0
      IWPBL  = 0

CCRT  Intialize variables ! 12/27/2017
      TER1 = 0.0D0
      FBRG = 0.0D0                                            ! Wood_D41
      
      SFCZ0_SAV = 0.0D0
      ISRC_SAV  = 0
      KST_SAV   = 0

C     The original BLP used meteorology from the met processor CRSTER.
C      CRSTER did not allow wind speeds to be less than 1 m/s.  Set the
C      reference wind speed used in the calculations below to be a minimum
C      of 1.0 m/s.  Use the local variable BL_UREF.  Keep the reference
C      wind speed height as UREFHT.
c      IF (UREF .GE. 1.0D0) THEN
c         BL_UREF = UREF
c      ELSE
c         BL_UREF = 1.0D0
C        WRITE Message              ! Ref ht wind speed less than minimum
c         WRITE(DUMMY,'(''on '',I8.8)') KURDAT
c         CALL ERRHDL(PATH,MODNAM,'W','471',DUMMY)
c      END IF

C     Save the source number, as determined when the source records are
C       processed by AERMOD, when BL_CALC is called since this routine
C       is not exited until all (one or more) bouyant line source groups
C       are processed
      ISRC_SAV = ISRC

C     LTOPG is called in METEXT.f in subr.SET_METDATA for a buoyant line
C       source; save that value in case it is changed for an urban source
      KST_SAV  = KST

C     Save original value for Z0
      SFCZ0_SAV = SFCZ0

C --- Loop over the buoyant line source groups
Cjop      BLPGROUP_LOOP: DO KK = 1, NUMBLGRPS

C        Initialize total concentration and partial contributions from
C          each line to 0 for all receptors
         CHIBL(:)  = 0.0D0
         PARTCH    = 0.0D0

C        Assign the average rise parameters from the arrays (processed
C         during setup) to the scalar
         BLAVGLLEN = BLAVGINP_LLEN(KK)
         BLAVGBHGT = BLAVGINP_BHGT(KK)
         BLAVGBWID = BLAVGINP_BWID(KK)
         BLAVGLWID = BLAVGINP_LWID(KK)
         BLAVGBSEP = BLAVGINP_BSEP(KK)
         BLAVGFPRM = BLAVGINP_FPRM(KK)

C ---    Assign # of individual lines for this BL source group to a scalar
         NBLINES = NBLINGRP(KK)

C        Set Mixing Height and Obukhov Length for Urban Option if Needed
C         A tacit assumption is that all lines in a buoyant line source
C         are either ALL urban or rural, i.e., no combination of urban/rural
C         is allowed.  This condition should have been checked in the QA of
C         the source input.

         IF (L_BLURBAN(KK)) THEN
C           Find urban area index for buoyant lines
C            Remember that once one individual BL is encountered that
C            all buoyant lines and BL source groups are processed
C            before moving on to the next non-BL source, so 
C            ISRC will not be changing.  The BL lines are already 
C            grouped together (group ID index = KK)
            LNUM = 0
            IF (KK .EQ.1 )THEN
               KSRC = ISRC

            ELSE IF (KK .GE. 2) THEN
               DO I = 1,NUMSRC
                 IF (SRCTYP(I) .EQ. 'BUOYLINE') THEN
                    LNUM = LNUM + 1
                    IF( BLINEPARMS(LNUM)%IBLPGRPNUM .EQ. KK) THEN
                       KSRC = I
                       EXIT
                    END IF
                 END IF
               END DO 
            END IF

            DO I = 1, NUMURB
               IF (IURBGRP(KSRC,I) .EQ. 1) THEN
                  IURB = I
                  EXIT
               END IF
            END DO

            IF (STABLE .OR. L_MorningTrans(IURB)) THEN
               URBSTAB = .TRUE.

C              Set AERMET file values to urban values for use below
               ZI = MAX( ZIURB(IURB), ZIMECH )

C              ... and for LTOPG
               SFCZ0 = URBZ0(IURB)
               IF (L_MorningTrans(IURB)) THEN
                  OBULEN = URBOBULEN(IURB)
C ---          Call LTOPG for a buoyant line source in an urban area
                  CALL LTOPG(BL_URBKST)
                  KST = BL_URBKST
               ELSE
                  OBULEN = DABS( URBOBULEN(IURB) )
C CRCO 12/8/2021 Decision was to set stability to 4 for all urban nighttime hours 
                  KST = 4
               END IF





            ELSE
C              Reset ZI and OBULEN back to rural values that were saved in 
C              SET_METDATA and URBCALC (both in metext.f)
C              KST was not changed and retains the rural/original value
               URBSTAB = .FALSE.
               ZI = ZIRUR
               OBULEN = RUROBULEN
            END IF

         ELSE IF (URBAN .AND. .NOT. L_BLURBAN(KK) ) THEN
C           Reset ZI and OBULEN back to rural values that were saved in 
C           SET_METDATA and URBCALC (both in metext.f)
C           KST was not changed and retains the rural/original value
            URBSTAB = .FALSE.
            ZI = ZIRUR
            OBULEN = RUROBULEN

         ELSE
C ---       Rural
C           KST was not changed and retains the rural/original value
            URBSTAB = .FALSE.
         END IF

C        Use the decay coefficient, DECOEF (default or input by the user),
C         but only under the conditions noted, for BLP's decay factor (DECFAC)
c         IF (DFAULT .AND. URBAN .AND. POLLUT.EQ.'SO2' .AND.
c        &    L_BLURBAN(KK)) THEN
C        modified 9/12/17 JAT, use half-life for SO2 URBAN even without DFAULT
c        if HALFLIFE or DCAYCOEF used, use that value, not 4-hours

         IF (URBAN .AND. POLLUT.EQ.'SO2' .AND. L_BLURBAN(KK) .AND.
     +     ((ICSTAT(7) .EQ. 0 .AND. ICSTAT(8) .EQ. 0) .OR. DFAULT)) THEN
            DECFAC = DECOEF
         ELSE IF ((POLLUT.EQ.'SO2' .AND. L_BLURBAN(KK)) .OR.
     +      DFAULT) THEN  !rural source for SO2 or default modified 10/12/17
            DECFAC = 0.0D0
         ELSE
c           DECFAC = 0.0D0 !commented out JAT 10/12/17
            DECFAC = DECOEF !now add ability to use DECAY coefficient or half life
         END IF

C ---    Wood, 12/1/2019: modified for multiple buoyant lines
C        If there is an hourly emissions file with buoyant lines,
C          calculate the average FPRIME (buoyancy parameter) from the
C          values in the file
C
         IF (HRLYBLCOUNT(KK) .GT. 0) THEN
C           Compute the average BLAVGFPRM from ther values in the
C            HOUREMIS file
            LNUM = 0
            SUMFPRM = 0.0
            
            DO ISRC = 1, NUMSRC
               IF (SRCTYP(ISRC) .EQ. 'BUOYLINE') THEN
                  LNUM = LNUM + 1
                  LSRC = BLINEPARMS(LNUM)%ISRCNUM
                  IF (QFLAG(LSRC) .EQ. 'HOURLY' .AND. 
     &                BLINEPARMS(LNUM)%IBLPGRPNUM .EQ. KK) THEN
                     SUMFPRM = SUMFPRM + AFP(LSRC)
                  END IF
               END IF
            END DO
            BLAVGFPRM = SUMFPRM / DBLE(HRLYBLCOUNT(KK))
         END IF

C        Calculate distance (from XFB) to final neutral plume rise
C         assuming plumes interact before reaching terminal rise
C         Note: the multiplication needs to be done when hourly values
C               are not available from an HOUREMIS file and BLAVGFPRM 
C               is entered on the BLPINPUT record(s)
         FBRG = DBLE(NBLINES) * BLAVGFPRM/PI
         IF (FBRG .LE. 55.0D0) THEN
C           Value: 49 = 3.5*14.
            BL_XFINAL = 49.0D0 * FBRG**0.625D0
         ELSE
C           Value: 120.72 = 3.5 * 34.49 (34.49=CONST3 in BLP)
            BL_XFINAL =  120.72D0 * FBRG**0.4D0
         END IF
         XMATCH = BL_XFINAL

C        Compute the difference between the endpoint of the x coordinates
C         AFTER the first translation/rotation for the BL group being
C         processed
         DO LNUM = 1,NBLTOTAL
            IF (BLINEPARMS(LNUM)%IBLPGRPNUM .EQ. KK) THEN
               DEL(LNUM) = 
     &             BLINEPARMS(LNUM)%XEND_TR1-BLINEPARMS(LNUM)%XBEG_TR1
            END IF
         END DO

C        Rotate source and receptors so the y-axis of the buoyant line
C         is parallel to the wind direction for this BL source group

         CALL BL_ROTATE2 (WDREF, BL_THETA, ITHETA, NUMREC, NBLTOTAL, KK)

C ---    Create array of BLINEs (part of RWB method for determining if
C          receptor in in bounding box - did not work for horizontal and
C          vertical sources)
C         DO K = 1, 4
C          DO I = 1, NBLINES
C             DO J = 1, 129
C                XBL_RCS_MAX(K) = MAX( XBL_RCS_MAX(K), XS_RCS(I,J) )
C                YBL_RCS_MAX(K) = MAX( YBL_RCS_MAX(K), YS_RCS(I,J) )
C                XBL_RCS_MIN(K) = MIN( XBL_RCS_MIN(K), XS_RCS(I,J) )
C                YBL_RCS_MIN(K) = MIN( YBL_RCS_MIN(K), YS_RCS(I,J) )
C             ENDDO
C          ENDDO
C         ENDDO

C        Get the power law exponent based on the stability;
C         calculate WSST = stack height wind speed
C         calculate S for stable conditions (BLTA is the saved ambient
C         temperature since it is modified when point sources are run)
         P = WSPEXP(KST)
         WSST = BL_UREF * (BLAVGBHGT/UREFHT)**P
         IF (KST .GT. 4) S = 9.80616D0 * DTHTA(KST-4)/BLTA

C        Calculate an effective wind speed, BL_UEFF, with the line source
C         plume rise eqn
C        INPUT:  KST, WSST, S, and P
C        OUTPUT: BL_UEFF = an effective wind speed
         CALL BL_WSC(KST,WSST,BL_UEFF,S,P,NBLINES)

C        Calculate XFB,LEFF,LD,R0
         CALL BL_LENG(BL_THETA,BL_UEFF,AVFACT,NBLINES)

C        Calculate distance to final rise
C         BL_XFS = distance to final rise
C         BL_XFB = distance to full buoyancy
C         BL_XFINAL = final neutral plume rise
         IF (KST .LE. 4) THEN
C           Calculate distance to final rise for neutral/unstable conditions
            BL_XFS = BL_XFB + BL_XFINAL

C           Find 5 intermediate downwind distances (in addition to XFB)
C           at which plume rise will be calculated
            DO IDIST = 2,7
               RI = DBLE(IDIST)
               BL_XDIST(IDIST) = BL_XFS -
     &                       (BL_XFS - BL_XFB)*(7.0D0 - RI)/5.0D0
            END DO

         ELSE
C           Calculate distance to final rise for stable conditions
            UNSRT = (16.0D0*BL_UEFF*BL_UEFF/S) - (BL_XFB*BL_XFB/3.0D0)

            IF (UNSRT .LE. 0.0D0) THEN
               BL_XFS = (12.0D0*BL_XFB*BL_UEFF*BL_UEFF/S)**(0.333333D0)
            ELSE
               BL_XFS = 0.5D0 * (BL_XFB + DSQRT(UNSRT))
            END IF

            XFSXX = BL_UEFF*PI/DSQRT(S)
            BL_XFS = DMIN1(BL_XFS,XFSXX)

            IF (BL_XFS .LE. BL_XFB) THEN
               DO IDIST = 2,7
                  BL_XDIST(IDIST) = BL_XFS
               END DO

            ELSE
C              Find 5 intermediate downwind distances (in addition to XFB)
C               at which plume rise will be calculated
               DO IDIST = 2,7
                  RI=DBLE(IDIST)
                  BL_XDIST(IDIST) = BL_XFS -
     &                          (BL_XFS - BL_XFB)*(7.0D0-RI)/5.0D0
               END DO
            END IF
         END IF


         CALL BL_RISE(BL_UEFF,KST, S)
C
C        CALCULATE CONCENTRATIONS DUE TO THE LINE SOURCES
C
C        LOOP OVER LINES
C
C        Since this routine is called only once for each bouyant line source
C         group, start the search for the buoyant lines with the current
C         source, i.e. the first buoyant line encountered in the group
C
C        The original source number was saved above to the correct source
C         so as we change the source number here, the processing does not
C         get messed up; the original source number is returned at the end
C         of the BUOY_LINES loop.

         BUOY_LINES: DO LNUM = 1,NBLTOTAL
            IF (BLINEPARMS(LNUM)%IBLPGRPNUM .EQ. KK) THEN
               ISRC = BLINEPARMS(LNUM)%ISRCNUM
               DLMIN     = DEL(LNUM)/128.0D0
               ZLINEBASE = BLINEPARMS(LNUM)%ELEV
               ZLINEHS   = BLINEPARMS(LNUM)%BLHS

! CRCO D095 added for BLP debug, specifically compute plume rise
         IF (BLPDBUG) THEN
          FPLMHT = ZLINEHS + DH(7)
          WRITE(BLPUNT,3333) SRCID(ISRC),IYEAR,IMONTH,IDAY,IHOUR,DH,
     &                       BL_XDIST,BL_XFB,BL_XFS,KST,URBOBULEN,
     &                       OBULEN,ZLINEHS,FPLMHT
 3333      FORMAT(1X,A12,4(2X,I2),14(F8.2),2X,2(F8.2),4X,I1,
     &            4(1X,F8.2))        
         ENDIF

C              Compute the profiled wind speed at the buoyant line
C               release height
               WSST  = BL_UREF * (ZLINEHS/UREFHT)**P

C              Assign the emissions stored in the variable AQS to QS
C               (either the constant on the SRCPARM keyword or the 
C                hourly emission rate)
               CALL SETSRC

C              Apply the emission factor to the input argument QS
C               (result is stored in QTK)
               CALL EMFACT(QS)

               CUQ   = QTK * 1.0D06 / (DBLE(NSEGA(1)-1)*WSST)
               SZ0   = R0 * SRT2DP
               ZV    = 1000.0D0 * BL_XVZ(SZ0,KST)
               SY0   = SZ0/2.0D0
               YV    = 1000.0D0 * BL_XVY(SY0,KST)
               XB    = XS_RCS(LNUM,1)
               YB    = YS_RCS(LNUM,1)
               XE    = XS_RCS(LNUM,129)
               YE    = YS_RCS(LNUM,129)
               XMAXL = DMAX1(XB,XE)
               XMINL = DMIN1(XB,XE)
               YMAXL = DMAX1(YB,YE)
               YMINL = DMIN1(YB,YE)
               DXEL  = XE - XB
               DYEL  = YE - YB

C              LOOP OVER RECEPTORS
               RECEPTOR_LOOP: DO IREC = 1, NUMREC
                  SUMM      = 0.0D0
                  PARTCH    = 0.0D0
                  NSEG      = 0
                  NCONTRIB  = 0
                  XRECEP    = XR_RCS(IREC)

C Wood_D11 - Include flagpole receptor heights
                  ZFLAG = AZFLAG(IREC)
                  THT   = AZELEV(IREC) - ZLINEBASE + ZFLAG

C                 If the receptor is upwind of the line
                  IF (XRECEP .LE. XMINL) THEN
                     CYCLE RECEPTOR_LOOP
                  END IF

C                 If the receptor is inside the rectangle defined by the source
C                  cycle to the next receptor ...
                  IF (BL_RFLAG(IREC,KK)) THEN
                     CYCLE RECEPTOR_LOOP
                  END IF

C                 Check for receptor located inside "boundary" of BUOYLINE sources
C                   (RWB method)
C                  DO I = 1, 4
C                     IF( XR_RCS(I) .LE. XBL_RCS_MAX(I) .AND.
C           &             XR_RCS(I) .GE. XBL_RCS_MIN(I) )THEN
C                        CHIBL(IREC) = 0.0D0
C                        CYCLE RECEPTOR_LOOP
C                     ENDIF
C                  END DO

                  YRECEP    = YR_RCS(IREC)
C                 IWOSIG keeps track of whether any line segment is within
C                  one sigma y of the current receptor (0=NO,1=YES)
                  IWOSIG = 0

C                 Define region of influence
C                  Max distance from any source segment to current receptor
C                  is equal to (XRECEP-XMINL)
                  XRMXKM = (XRECEP-XMINL)/1000.0D0
                  CALL BL_SIGMAY(XRMXKM,KST,SYC)

                  YLOW  = YMINL - 4.0D0*SYC
                  YHIGH = YMAXL + 4.0D0*SYC
                  IF (YRECEP .LT. YLOW .OR. YRECEP .GT. YHIGH) THEN
                     CYCLE RECEPTOR_LOOP
                  END IF

                  YLOW  = YLOW  + DLMIN
                  YHIGH = YHIGH - DLMIN
                  IF (YRECEP .LT. YLOW .OR. YRECEP .GT. YHIGH) THEN
                     CYCLE RECEPTOR_LOOP
                  END IF

C                 Check if receptor is directly downwind of the line
C                  (IDW=0=NO,IDW=1=YES)
                  IDW = 1

                  IF (YRECEP .LT. YMINL .OR. YRECEP .GT. YMAXL) IDW = 0

C                 Check if receptor is on the downwind side of the line
                  IF (XRECEP .LT. XMAXL )THEN
                     IF (MOD(ITHETA,90) .NE. 0) THEN
                        EM = DYEL/DXEL
                        B = YE - EM*XE
                        IF (XRECEP .LT. (YRECEP-B)/EM) NCONTRIB = 999
                     END IF
                  END IF

                  NSEG0  = NSEGA(1)
                  NNEW   = NSEG0
                  ITER   = 0
                  INDL   = 1
                  IDELTA = 128/(NSEG0-1)
498               CONTINUE
                  NSEG = NSEG+NNEW
C
C                 Loop over line segments
C
                  SEGMENT_LOOP: DO ISEG = 1,NNEW
                     FTSAVE(INDL) = 0.0D0

C                    If current receptor is upwind of a source segment,
C                    then this source segment does not contribute

                     IF (XS_RCS(LNUM,INDL) .GE. XRECEP) GO TO 495
                     DOWNX  = XRECEP - XS_RCS(LNUM,INDL)
                     CROSSY = YRECEP - YS_RCS(LNUM,INDL)
                     VIRTXZ = DOWNX + ZV
                     VIRTXY = DOWNX + YV
                     VXYKM  = VIRTXY/1000.0D0
                     VXZKM  = VIRTXZ/1000.0D0

                     if( VXYKM .LT. 0.0D0 .OR. VXZKM .LT. 0.0D0 )then
C ---                   Virtual distance is < 0.0; skip this segment
                        GO TO 495
                     endif
                     CALL BL_DBTSIG(VXZKM,VXYKM,KST,SIGY,SIGZ)

C                    If crosswind distance > 4 * SIGY, then this source
C                     segment does not contribute
                     IF (4.0D0*SIGY .LT. DABS(CROSSY)) GO TO 495
                     IF (DABS(CROSSY) .LT. SIGY) IWOSIG = 1
                     CALL BL_ZRISE(LNUM,INDL,IREC,Z)
C
C                    Include terrain correction in determining the plume height
C
                     TER1 = 1.0D0 - TERAN(KST)
                     HNT  = Z + ZLINEHS
C                    TER1=(1.0-TERAN(KST)); THT=RELEV(I)-LELEV(LNUM)
C                    ZI is the AERMOD-determined mixing height
                     TERRAN = TER1 * DMIN1(HNT,THT)
                     H = HNT - TERRAN

                     IF (H .GT. ZI .AND. KST .LE. 4)GO TO 495
C
C                    Solve the gaussian point source equation
C
                     CALL BL_GAUSS(KST,ZI,CROSSY,SIGY,SIGZ,H,FT)
C                    Include decay in determining chi
                     DELTAT = DOWNX/WSST
                     FTSAVE(INDL) = FT*(1.0D0 - DELTAT*DECFAC)
                     NCONTRIB = NCONTRIB + 1
495                  INDL   = INDL + IDELTA
                  END DO SEGMENT_LOOP

C
C                 First time through loop, calculate the first chi estimate
C
                  IF (NNEW .NE. NSEG0) GO TO 714
                  INDL = 1
                  NSEGM1 = NSEG0 - 1
                  SUMM = (FTSAVE(1) + FTSAVE(129))/2.0D0
                  DO ISEG2 = 2,NSEGM1
                     INDL = INDL + IDELTA
                     SUMM = SUMM + FTSAVE(INDL)
                  END DO

C                 If receptor is within region of influence but not directly
C                  downwind of any part of the line, and SUM=0.0, CHI=0.0
                  IF (SUMM .LE. 0.0D0 .AND. IDW .NE. 1) THEN
                     CYCLE RECEPTOR_LOOP
                  END IF
C
C                 Calculate the refined chi estimate
C
713               CONTINUE
                  ITER   = ITER + 1
                  IDIV   = MIN0(ITER,2)
                  IDELTA = IDELTA/IDIV
                  INDL   = 1 + IDELTA/2
C                 INDL is the subcript of the first new line segment
C                   (save as INDLSV)
                  INDLSV = INDL

                  NNEW = NSEGM1**ITER + 0.1D0

C                 If more than 129 line segments (i.e., 64 new segments)
C                  are required, continue to increase the number of
C                  segments but only over the section of the line
C                  which is contributing

                  IF (NNEW .GT. 64) GO TO 759
                  GO TO 498

714               CONTINUE

C                 Subscript of the first new line segment is INDLSV
C                 Subscript of the last new line segment is INDLLN
                  INDLLN = 129 - IDELTA/2

C                 Sum the first and last new line segments
                  SUM2 = FTSAVE(INDLSV) + FTSAVE(INDLLN)

C                 If there are only 2 new line segments, skip this loop
                  IF (NNEW .GT. 2) THEN
                     INDL = INDLSV
                     I2   = NNEW-1
C
C                    Find the sum of all the new line segments
                     DO ISEG3=2,I2
                        INDL = INDL + IDELTA
                        SUM2 = SUM2 + FTSAVE(INDL)
                     END DO

                  END IF
C
C                 Compare the new estimate with the previous estimate
C
                  SUM2 = SUMM/2.0D0 + SUM2/(2.0D0**ITER)

C                 At least one line segment must be within one SIGMA Y of
C                  the line (if the receptor is directly downwind of any
C                  part of the line)
                  IF (IDW .EQ. 1 .AND. IWOSIG .NE. 1) GO TO 758

                  DIFF = DABS(SUM2-SUMM)
                  IF (DIFF*CUQ .LT. 0.1D0) THEN
                     GO TO 720
                  END IF

                  CORR = DIFF/SUM2

                  IF (CORR .LT. BLCRIT) THEN
                     GO TO 720
                  END IF

758               CONTINUE
                  SUMM = SUM2
                  GO TO 713

C                 If 129 source segments not sufficient, continue
C                  to increase number of segments, but only over the
C                  section of line which is contributing
759               CONTINUE

                  CALL BL_SORT(FTSAVE,IBMIN,IBMAX,IWPBL)

                  IF (IWPBL .NE. 999) GO TO 4949
                  IWPBL  = 0
                  PARTCH = 0.0D0
                  CYCLE RECEPTOR_LOOP

4949              CONTINUE

                  IBMAX1 = IBMAX - 1
                  IH     = 0
                  IGMAX  = 1

939               CONTINUE
                  SUM2   = 0.0D0
                  IGMAX1 = IGMAX + 1

                  DO 940 IG = IBMIN,IBMAX1
C                    XCLN = x coordinate (RCS) of current (newest) line segment
C                    YCLN = y coordinate (RCS) of current (newest) line segment
C                    XRCS and YRCS are the translated and rotated line source
C                      segments w.r.t. the hourly flow vector
                     XSEG1 = XS_RCS(LNUM,IG)
                     XDIFF = XS_RCS(LNUM,IG+1) - XSEG1
                     YSEG1 = YS_RCS(LNUM,IG)
                     YDIFF = YS_RCS(LNUM,IG+1) - YSEG1

                     DO 941 IGSUB = 1,IGMAX
                        WEIGHT = DBLE(IGSUB)/DBLE(IGMAX1)
                        XCLN   = XSEG1 + WEIGHT*XDIFF
                        YCLN   = YSEG1 + WEIGHT*YDIFF
                        DOWNX  = XRECEP - XCLN
                        CROSSY = YRECEP - YCLN
                        VIRTXZ = DOWNX + ZV
                        VIRTXY = DOWNX + YV
                        VXYKM  = VIRTXY/1000.0D0
                        VXZKM  = VIRTXZ/1000.0D0

                        if( VXYKM .LT. 0.0D0 .OR. VXZKM .LT. 0.0D0 )then
C ---                      Virtual distance is < 0.0; skip this segment
                           GO TO 941                                   ! not sure if this is best fix
                        endif

                        CALL BL_DBTSIG(VXZKM,VXYKM,KST,SIGY,SIGZ)
                        CALL BL_ZRISE(LNUM,IG,IREC,Z)

C                       Include terrain correction in determining plume height
                        HNT    = Z + ZLINEHS
C                       TER1   = (1.0-TERAN(KST)); THT=RELEV(I)-LELEV(LNUM)
                        TERRAN = TER1*DMIN1(HNT,THT)
                        H      = HNT - TERRAN
                        CALL BL_GAUSS(KST,ZI,CROSSY,SIGY,SIGZ,H,FT)

C                       Include decay in determining chi
                        DELTAT = DOWNX/WSST
                        FT     = FT*(1.0D0 - DELTAT*DECFAC)
                        SUM2   = SUM2 + FT
                        NCONTRIB = NCONTRIB + 1
941                  CONTINUE
      
940               CONTINUE

C                 Compare the new estimate with the previous estimate
                  SUM2 = SUMM/2.0D0 + SUM2/(2.0D0**ITER)

                  DIFF = ABS(SUM2-SUMM)
                  IF (DIFF*CUQ .LT. 0.1D0) THEN
                     GO TO 720
                  ENDIF

                  CORR = DIFF/SUM2
                  IF (CORR .LT. BLCRIT) THEN
                     GO TO 720
                  END IF

                  SUMM = SUM2
                  ITER = ITER + 1
                  IF (ITER .GE. MAXIT) THEN
                     SUMM = SUM2
                     PARTCH      = CUQ*SUMM
                     CHIBL(IREC) = CHIBL(IREC) + PARTCH
                     IF (DEBUG) THEN
                        WRITE(DBGUNT,6000) IREC, CHIBL(IREC)
6000                    FORMAT(/,5X,'Buoyant Line iteration fails to ',
     &                    ' converge for receptor',I6,', conc. =',F13.6)
                        JITCT = JITCT+1
                        IF (MOD(JITCT,100) .EQ. 0 ) THEN
                           WRITE(DBGUNT,6001) JITCT
6001                       FORMAT(/,5X,'Buoyant Line iterations for',
     &                       ' all receptors fails for ',I8,'th time' )
                        END IF
                     ENDIF
                     CYCLE RECEPTOR_LOOP
                  END IF
                  IH = IH + 1
                  IGMAX = 2**IH
                  GO TO 939
720               CONTINUE

                  SUMM = SUM2

C                 Test to make sure at least two line segments contributed
C                  to the chi estimate (unless receptor is on the upwind side
C                  of the line with some source segments downwind and some
C                  source segments upwind -- in that case just use the test
C                  for convergence)
                  IF (NCONTRIB .LT. 2) GO TO 713

C                 Calculate concentration (in micrograms);
C                  use stack height wind speed for dilution

                  PARTCH = CUQ*SUMM
                  CHIBL(IREC)  = CHIBL(IREC) + PARTCH
                  HRVAL = PARTCH

C                 For the initial implementation of BLP into AERMOD, we
C                  are skipping calculations with PVMRM, OLM.
C                  As BLP is integrated more into AERMOD, those options
C                  will be included (e.g., see PCALC).
C
                  IF (ARM2) THEN
C ---                Store conc by source and receptor for ARM/ARM2 options
                     DO ITYP = 1, NUMTYP
                        CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
                     END DO

C                    Initialize __VAL arrays (1:NUMTYP)
                     HRVAL   = 0.0D0

                  END IF

C                 Sum HRVAL to AVEVAL and ANNVAL Arrays      ---   CALL SUMVAL
                  IF (EVONLY) THEN
                     CALL EV_SUMVAL
                  ELSE
                     DO IGRP = 1, NUMGRP
                        CALL SUMVAL
                     END DO
                  END IF

                  IF (EVAL(ISRC)) THEN
C                    Check ARC centerline values for EVALFILE
C                    output                                  ---   CALL EVALCK
                     CALL EVALCK
                  END IF

C                 Initialize __VAL arrays (1:NUMTYP)
                  HRVAL   = 0.0D0

               END DO RECEPTOR_LOOP
            END IF                    ! BLINEPARMS(LNUM)%IBLPGRPNUM = KK
         END DO BUOY_LINES

C        Since all buoyant line groups and the associated individual
C         buoyant lines are processed in consecutive calls to BL_CALC
C         from subr,CALC, the PG stability, mixing height, roughness 
C         length, and Obukhov length need to be restored to the 
C         original values if the BL group was an urban source
          IF (URBSTAB) THEN
             ISRC = ISRC_SAV
             KST  = KST_SAV
             ZI   = ZIRUR
             SFCZ0  = SFCZ0_sav
             OBULEN = RUROBULEN
          ELSE
             ISRC = ISRC_SAV
             KST  = KST_SAV
             SFCZ0  = SFCZ0_sav
          END IF

C     END DO BLPGROUP_LOOP

C     ISRC = ISRC_SAV
C     KST  = KST_SAV
C     ZI   = ZIRUR
C     SFCZ0  = SFCZ0_sav
C     OBULEN = RUROBULEN

      RETURN
      END

      SUBROUTINE BL_DBTSIG (XZ,XY,ISTAB,SY,SZ)
C***********************************************************************
C             BL_DBTSIG Routine of the AMS/EPA Regulatory Model - AERMOD
C
C     PURPOSE: To estimate sigma-y and sigma-z for buoyant line sources
C
C     INPUT:
C       XZ     Virtual distance (kilometers) - VXZKM in calling program
C       XY     Virtual distance (kilometers) - VXYKM in calling program
C       ISTAB  PG stability category
C
C     OUTPUT:
C       SY     Sigma-y
C       SZ     Sigma-z
C
C     ------------------------------------------------------------------
C
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN)  :: XZ, XY
      DOUBLE PRECISION, INTENT(OUT) :: SY, SZ
      INTEGER, INTENT (IN) :: ISTAB
      INTEGER :: ID

      DOUBLE PRECISION :: TH

      DOUBLE PRECISION, PARAMETER, DIMENSION(7) ::
     &    XA = [0.5D0, 0.4D0, 0.3D0, 0.25D0, 0.2D0, 0.15D0, 0.1D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(2) ::
     &    XB = [0.4D0, 0.2D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(5) ::
     &    XD = [30.0D0, 10.0D0, 3.0D0, 1.0D0, 0.3D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::
     &    XE = [40.0D0, 20.0D0, 10.0D0, 4.0D0, 2.0D0,
     &           1.0D0, 0.3D0, 0.1D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::
     &    XF = [60.0D0, 30.0D0, 15.0D0, 7.0D0, 3.0D0,
     &           2.0D0, 1.0D0, 0.7D0, 0.2D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::
     &    AA = [453.85D0, 346.75D0, 258.89D0, 217.41D0,
     &          179.52D0, 170.22D0, 158.08D0, 122.8D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::
     &    BA = [2.1166D0, 1.7283D0, 1.4094D0, 1.2644D0,
     &          1.1262D0, 1.0932D0, 1.0542D0, 0.9447D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(3) ::
     &    AB = [109.30D0, 98.483D0, 90.673D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(3) ::
     &    BB = [1.0971D0, 0.98332D0, 0.93198D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::
     &    AD = [44.053D0, 36.650D0, 33.504D0, 32.093D0,
     &          32.093D0, 34.459D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::
     &    BD = [0.51179D0, 0.56589D0, 0.60486D0, 0.64403D0,
     &          0.81066D0, 0.86974D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::
     &    AE = [47.618D0, 35.420D0, 26.970D0, 24.703D0,
     &          22.534D0, 21.628D0, 21.628D0, 23.331D0, 24.26D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::
     &    BE = [0.29592D0, 0.37615D0, 0.46713D0, 0.50527D0,
     &          0.57154D0, 0.63077D0, 0.75660D0, 0.81956D0, 0.8366D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(10) ::
     &    AF = [34.219D0, 27.074D0, 22.651D0, 17.836D0, 16.187D0,
     &          14.823D0, 13.953D0, 13.953D0, 14.457D0, 15.209D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(10) ::
     &    BF = [0.21716D0, 0.27436D0, 0.32681D0, 0.41507D0, 0.46490D0,
     &          0.54503D0, 0.63227D0, 0.68465D0, 0.78407D0, 0.81558D0]

C      GO TO (10,20,30,40,50,60),ISTAB

CCRT  Initialize TH, 12/27/2017
      TH = 0.0D0

      SELECT CASE (ISTAB)
         CASE (1)
C           STABILITY A (10)
            TH = (24.167D0 - 2.5334D0 * DLOG(XY)) / 57.2958D0
            IF (XZ .GT. 3.11D0) THEN
               SZ = 5000.0D0
            ELSE
               DO ID = 1,7
                  IF (XZ .GE. XA(ID)) GO TO 12
               END DO
               ID = 8
   12          SZ = AA(ID) * XZ ** BA(ID)
            ENDIF

         CASE (2)
C           STABILITY B (20)
            TH = (18.333D0 - 1.8096D0 * DLOG(XY)) / 57.2958D0
            IF (XZ .GT. 35.0D0) THEN
               SZ = 5000.0D0
            ELSE
               DO ID = 1,2
                  IF (XZ .GE. XB(ID)) GO TO 22
               END DO
               ID = 3
   22          SZ = AB(ID) * XZ ** BB(ID)
               IF (SZ .GT. 5000.0D0) SZ = 5000.0D0
            ENDIF

         CASE (3)
C           STABILITY C (30)
            TH = (12.5D0 - 1.0857D0 * DLOG(XY)) / 57.2958D0
            SZ = 61.141D0 * XZ ** 0.91465D0
            IF (SZ .GT. 5000.0D0) SZ = 5000.0D0

         CAsE (4)
C           STABILITY D (40)
            TH = (8.3333D0 - 0.72382D0 * DLOG(XY)) / 57.2958D0

            DO ID = 1,5
               IF (XZ .GE. XD(ID)) GO TO 42
            END DO
            ID = 6
   42       SZ = AD(ID) * XZ ** BD(ID)
            IF (SZ .GT. 5000.0D0) SZ = 5000.0D0

         CASE (5)
C           STABILITY E (50)
            TH = (6.25D0 - 0.54287D0 * DLOG(XY)) / 57.2958D0
            DO ID = 1,8
               IF (XZ .GE. XE(ID)) GO TO 52
            END DO
            ID = 9
   52       SZ = AE(ID) * XZ ** BE(ID)
            IF (SZ .GT. 5000.0D0) SZ = 5000.0D0

         CASE (6)
C           STABILITY F (60)
            TH = (4.1667D0 - 0.36191D0 * DLOG(XY)) / 57.2958D0
            DO ID = 1,9
               IF (XZ .GE. XF(ID)) GO TO 62
            END DO
            ID = 10
   62       SZ = AF(ID) * XZ ** BF(ID)
            IF (SZ .GT. 5000.0D0) SZ = 5000.0D0
      END SELECT

      SY = 1000.0D0 * XY * DSIN(TH)/(2.15D0 * DCOS(TH))

      RETURN
      END
C
C     ------------------------------------------------------------------
      SUBROUTINE BL_SIGMAY(XKM,ISTAB,SY)
C
C             BL_SIGMAY Routine of the AMS/EPA Regulatory Model - AERMOD
C
C     Calculates sigma Y
C
C     INPUT:
C       XKM    Virtual distance (kilometers) - VXZKM in calling program
C       ISTAB  PG stability category
C
C     OUTPUT:
C       SY     Sigma-y
C
C     ------------------------------------------------------------------
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN)  :: XKM
      DOUBLE PRECISION, INTENT(OUT) :: SY
      INTEGER, INTENT(IN)           :: ISTAB

      DOUBLE PRECISION              :: TH

CCRT  Initialize TH  ! 12/27/2017
      TH = 0.0D0

      SELECT CASE (ISTAB)

         CASE (1)
C           STABILITY A(10)
            TH = (24.167D0 - 2.5334D0 * DLOG(XKM)) / 57.2958D0

         CASE (2)
C           STABILITY B
            TH = (18.333D0 - 1.8096D0 * DLOG(XKM)) / 57.2958D0

C           STABILITY C
         CASE (3)
            TH = (12.5D0- 1.0857D0 * DLOG(XKM)) / 57.2958D0

C           STABILITY D
         CASE (4)
            TH = (8.3333 - 0.72385D0 * DLOG(XKM)) / 57.2958D0

C           STABILITY E
         CASE (5)
            TH = (6.25D0 - 0.54287D0 * DLOG(XKM)) / 57.2958D0

C           STABILITY F
         CASE (6)
            TH = (4.1667D0 - 0.36191D0 * DLOG(XKM)) / 57.2958D0

      END SELECT

      SY = 1000.0D0 * XKM * DSIN(TH)/(2.15D0 * DCOS(TH))

      RETURN
      END

      SUBROUTINE BL_RISE(U,ISTAB,S)
C***********************************************************************
C             BL_RISE Routine of the AMS/EPA Regulatory Model - AERMOD
C
C     Calculates line source plume rise using an optional vertical wind
C     shear corrected 'effective' wind speed for both neutral and
C     stable conditions
C
C     Input
C       U       = wind speed
C       S       = computed parameter for stable conditions
C       ISTAB   = P-G stability category
C
C     Input via module.f
C       LEFF    = effective building length
C       R0      = edge radius
C       LD      = LEFF * sin (theta), where theta = angle between flow
C                  vector and the orientation of the line source
C       FPRMXNB = fprime * number of lines
C       BL_XFB  = distance to full buoyancy
C       BL_XFS  = distance to final rise for stable conditions
C       XDIST   = array of intermediate distances for downwash calcs
C
C     Output
C       DH      = array of intermediate plume rise values
C
C     ------------------------------------------------------------------
      USE BUOYANT_LINE
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN) :: U, S
      INTEGER, INTENT(IN) :: ISTAB

      DOUBLE PRECISION :: X, A, B, C, Z
      INTEGER :: IDIST
C
C
C     CONSTANT 1.5915494 = 3./(PI*BETA) WITH BETA=0.6
C     CONSTANT 5.0 = 3./BETA WITH BETA=0.6
      A = 1.5915494D0 * LEFF + 5.0D0 * R0

C     CONSTANT 5.3051648 = 6./(PI*BETA*BETA) WITH BETA=0.6
C     CONSTANT 8.3333333 = 3./(BETA*BETA) WITH BETA=0.6
      B = R0 * (5.3051648D0 * LD + 8.333333D0 * R0)

      DO 1000 IDIST = 2,7
         X = BL_XDIST(IDIST)

         IF (ISTAB .LE. 4  .OR. X .LT. BL_XFS) THEN
            IF( DABS(X - BL_XFB) .LT. 1.0D-10 )THEN
C              CONSTANT 0.4420971 = 1./(2.*PI*BETA*BETA) WITH BETA=0.6
               C = -0.4420971D0 * (FPRMXNB/BL_XFB) * (X/U)**3
               CALL BL_CUBIC(A,B,C,Z)
               DH(IDIST) = Z
            ELSE
C              CONSTANT 1.3262912 = 3./(2.*PI*BETA*BETA) WITH BETA=0.6
               C = -1.3262912D0 * FPRMXNB *
     &                     (BL_XFB*BL_XFB/3.0D0 + X*X - BL_XFB*X)/U**3
               CALL BL_CUBIC(A,B,C,Z)
               DH(IDIST) = Z

            END IF

         ELSE
C           With stable conditions, use neutral rise equation
C           for transitional rise calculations, but calculate
C           FINAL RISE BASED ON THE FINAL STABLE RISE EQUATION

C           Calculate final (stable) plume rise
C           Constant 5.3051648 = 6./(PI*BETA*BETA) WITH BETA=0.6
            C = -5.3051648D0 * FPRMXNB / (U*S)
            CALL BL_CUBIC(A,B,C,Z)
            DH(IDIST) = Z

         END IF

1000  CONTINUE

      RETURN
      END
C
      SUBROUTINE BL_ZRISE(IL,IS,IR,Z)
C***********************************************************************
C             BL_ZRISE Routine of the AMS/EPA Regulatory Model - AERMOD
C
C     Z1 is the plume height of the highest plume segment at X = XFB
C       (except in the special case of stable conditions with the
C       distance to final rise (XFS) less than XFB -- in that case, Z1
C       is the height of the highest plume element at X=XFS)
C
C     XI is the distance of the current line segment to XFB
C
C     Input:
C       IL = line number
C       IS = INDL = line segment
C       IR = receptor number
C
C     Output:
C       Z
C
C     ------------------------------------------------------------------
      USE BUOYANT_LINE

      DOUBLE PRECISION, INTENT(OUT) :: Z
      INTEGER, INTENT(IN)           :: IL, IR, IS
      DOUBLE PRECISION :: Z1, Z2, XI, ZXFB
C
      Z1 = DH(2)

      XI = BL_XFB - XS_RCS(IL,IS)
      XI = DMAX1(XI,0.0D0)
      XI = DMIN1(XI,BL_XFB)
      ZXFB = Z1*(1.0D0 - (BL_XFB-XI)/BL_XFB)

C     Z2, returned from BLP_INTRSE, is the plume height of the highest
C         segment at X
      CALL BL_INTRSE(XR_RCS(IR),Z2)

      Z = ZXFB + Z2 - Z1

      RETURN
      END

C     ------------------------------------------------------------------
      SUBROUTINE BL_INTRSE(X,Z)
C
C             BL_INTRSE Routine of the AMS/EPA Regulatory Model - AERMOD
C
C     Interpolates the plume rise of the top (highest) plume element
C       at any distance X using the calculated plume rise at
C       seven points (BL_XDIST(1-7)), as computed in BL_RISE just before
C       entering the loop over the buoyant lines.
C
C     Input:
C       X   = Distance X
C
C     Output:
C       Z   = Interpolated plume rise
C
C     ------------------------------------------------------------------
      USE BUOYANT_LINE
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN)  :: X
      DOUBLE PRECISION, INTENT(OUT) :: Z

      INTEGER    :: NDEX, NDEX1, IDIST
CC
      IF (X .GT. BL_XDIST(7)) THEN
C        PLUME REACHES FINAL RISE
         Z = DH(7)
      ELSE
C Wood_D117: BL_INTRSE error
C         NDEX = 5                                          ! D117
C         DO 10 IDIST = 2,6                                 ! D117

         DO 10 IDIST = 2,7                                  ! D117
            IF (X .LT. BL_XDIST(IDIST)) THEN                ! D117
               NDEX = IDIST
               EXIT
            ENDIF
10       CONTINUE
         NDEX1 = NDEX-1
         Z = DH(NDEX) - (DH(NDEX)-DH(NDEX1)) * (BL_XDIST(NDEX) - X) /
     &       (BL_XDIST(NDEX) - BL_XDIST(NDEX1))
      END IF

      RETURN
      END

      SUBROUTINE BL_GAUSS(ISTAB,DPBL,CROSSY,SIGY,SIGZ,H,FT)
C***********************************************************************
C             BL_GAUSS Routine of the AMS/EPA Regulatory Model - AERMOD
C
C     Input:
C       ISTAB   = PG stability category
C       DPBL    = mixing height
C       CROSSY  = crosswind distance from receptor to source segment
C       SIGY    = sigma-y calculated in BL_DBTSIG
C       SIGZ    = sigma-z calculated in BL_DBTSIG
C       H       = terrain-dependent height (for ??)
C
C     Output:
C       FT
C     ------------------------------------------------------------------
      USE BUOYANT_LINE

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ISTAB
      DOUBLE PRECISION, INTENT(IN) :: DPBL, CROSSY, SIGY, SIGZ, H
      DOUBLE PRECISION, INTENT(OUT) :: FT

      DOUBLE PRECISION :: TMIN, TMAX, TD1, YPSIG, EXPYP, EXPHP
      DOUBLE PRECISION :: ARG, F, F1, T, H2, HPSIG, EPSIL

      DATA TMIN/0.0512D0/,TMAX/9.21D0/, EPSIL/1.0D-30/

      TD1   = 3.1415927D0 * SIGY * SIGZ
      YPSIG = CROSSY/SIGY
      EXPYP = 0.5D0 * YPSIG * YPSIG

C     Prevent underflows
      IF (EXPYP .GT. 50.0D0) THEN
         F  = 0.0D0                 ! not really needed
         F1 = 0.0D0                 ! not really needed
         FT = 0.0D0
         RETURN
      END IF

      F = EXP(-EXPYP)

C     If mixing height (DPBL) GE 5000 meters or for stable conditions,
C     neglect the reflection terms
      IF (ISTAB .GE. 5 .OR. DPBL .GT. 5000.0D0) GO TO 451

C     If SIGZ GT 1.6*DPBL, assume a uniform vertical distribution
      IF (SIGZ .GT. 1.6D0*DPBL) GO TO 460

C     Calculate multiple eddy reflections terms
C     using a fourier series method -- see ERT MEMO CS 093

      F1 = 1
      T  = (SIGZ/DPBL)**2
      H2 = H/DPBL

      IF (T .LT. 0.6D0) THEN
         ARG = 2.0D0 * (1.0D0 - H2)/T
         IF (ARG .LT. TMAX) THEN
            IF (ARG .LT. TMIN) THEN
               F1 = F1 + 1.0D0 - ARG
            ENDIF
            IF(ARG .GE. TMIN) THEN
               F1 = F1 + EXP(-ARG)
            ENDIF
            ARG = 2.0D0 * (1.0D0 + H2)/T
            IF (ARG .LT. TMAX) THEN
               F1 = F1 + EXP(-ARG)
               ARG = 4.0D0 * (2.0D0 - H2)/T
               IF (ARG .LT. TMAX) THEN
                  F1 = F1 + EXP(-ARG)
                  ARG = 4.0D0 * (2.0D0 + H2)/T
                  IF (ARG .LT. TMAX) THEN
                     F1 = F1 + EXP(-ARG)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         ARG = -0.5D0 * H2 * H2/T
         IF (ARG .LT. -90.0D0) THEN
            F1 = 0.0D0
         ENDIF

C        The constant 0.797885 = SQRT(2./PI)
         IF (ARG .GE. -90.0D0) THEN
            F1 = 0.797885D0 * F1 * EXP(ARG)/SIGZ
         ENDIF
         IF (F1 .LT. EPSIL) F1 = 0.0D0

      ELSE
C        The constant 4.934802 = PI*PI/2.
         ARG = 4.934802D0 * T
         IF (ARG .LT. TMAX) THEN
             F1 = F1 + 2.0D0 * DEXP(-ARG) * DCOS(3.141593D0*H2)

C            The constant 19.739209 = 2.*PI*PI
             ARG = 19.739209D0 * T
             IF (ARG .LT. TMAX) THEN
                F1 = F1 + 2.0D0 * DEXP(-ARG) * DCOS(6.283185D0*H2)
             ENDIF
         ENDIF
         F1 = F1/DPBL
         IF (F1 .LT. EPSIL) F1 = 0.0D0
      ENDIF

C     The constant 1.25331414 = SQRT(PI/2.)
      F1 = 1.25331414D0 * SIGZ * F1
      GO TO 445
451   CONTINUE

      HPSIG = H/SIGZ
      EXPHP = 0.5D0 * HPSIG * HPSIG
      IF (EXPHP .GT. 50.0D0) THEN
         F1 = 0.0D0
      ELSE
         F1 = DEXP(-EXPHP)
      ENDIF

445   CONTINUE

C     Find product of exponential terms divided by (PI*SIGY*SIGZ)
      FT = F * F1/TD1
      GO TO 470
460   CONTINUE

C     Vertical distribution assumed uniform
C     The constant 2.5066283 = SQRT(2.*PI)
      FT = F/(2.5066283D0 * SIGY * DPBL)

470   RETURN
      END

C
C     ------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION BL_XVZ (SZO,ISTAB)
C
C
C     ------------------------------------------------------------------
      DOUBLE PRECISION :: SZO
      INTEGER :: ISTAB, ID

      DOUBLE PRECISION, PARAMETER, DIMENSION(7) ::
     &       SA = [13.95D0, 21.40D0, 29.3D0, 37.67D0, 47.44D0,
     &             71.16D0 ,104.65D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(2) ::
     &       SB = [20.23D0, 40.0D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(5) ::
     &       SD = [12.09D0, 32.09D0, 65.12D0, 134.9D0, 251.2D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::
     &       SE = [3.534D0, 8.698D0, 21.628D0, 33.489D0, 49.767D0,
     &             79.07D0, 109.3D0, 141.86D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::
     &       SF = [4.093D0, 10.93D0, 13.953D0, 21.627D0, 26.976D0,
     &             40.0D0, 54.89D0, 68.84D0, 83.25D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::
     &       AA = [122.8D0, 158.08D0, 170.22D0, 179.52D0, 217.41D0,
     *             258.89D0, 346.75D0, 453.85D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(3) ::
     &      AB = [90.673D0, 98.483D0, 109.3D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::
     &      AD = [34.459D0, 32.093D0, 32.093D0, 33.504D0,
     &            36.650D0, 44.053D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::
     &      AE = [24.26D0, 23.331D0, 21.628D0, 21.628D0, 22.534D0,
     &            24.703D0, 26.97D0, 35.42D0, 47.618D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(10) ::
     &      AF = [15.209D0, 14.457D0, 13.953D0, 13.953D0, 14.823D0,
     &            16.187D0, 17.836D0, 22.651D0, 27.074D0, 34.219D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(8) ::
     &      CA = [1.0585D0, 0.9486D0, 0.9147D0, 0.8879D0, 0.7909D0,
     &            0.7095D0, 0.5786D0, 0.4725D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(3) ::
     &      CB = [1.073D0, 1.017D0, 0.9115D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(6) ::
     &      CD = [1.1498D0, 1.2336D0, 1.5527D0, 1.6533D0,
     &            1.7671D0, 1.9539D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(9) ::
     &      CE = [1.1953D0, 1.2202D0, 1.3217D0, 1.5854D0, 1.7497D0,
     &            1.9791D0, 2.1407D0, 2.6585D0, 3.3793D0]

      DOUBLE PRECISION, PARAMETER, DIMENSION(10) ::
     &      CF = [1.2261D0, 1.2754D0, 1.4606D0, 1.5816D0, 1.8348D0,
     &            2.151D0, 2.4092D0, 3.0599D0, 3.6448D0, 4.6049D0]
C
CCRT  Initialize BL_XVZ
      BL_XVZ = 0.0D0

      SELECT CASE (ISTAB)

        CASE (1)
C          STABILITY A
           DO ID = 1,7
              IF (SZO .LE. SA(ID)) GO TO 12
           END DO
           ID = 8
12         BL_XVZ =(SZO/AA(ID))**CA(ID)

        CASE (2)
C          STABILITY B
           DO ID = 1,2
              IF (SZO .LE. SB(ID)) GO TO 22
           END DO
           ID = 3
22         BL_XVZ = (SZO/AB(ID))**CB(ID)

        CASE (3)
C          STABILITY C
           BL_XVZ = (SZO/61.141D0)**1.0933D0

        CASE (4)
C          STABILITY D
           DO ID = 1,5
              IF(SZO .LE. SD(ID)) GO TO 42
           END DO
           ID = 6
42         BL_XVZ = (SZO/AD(ID))**CD(ID)

        CASE (5)
C          STABILITY E
           DO ID = 1,8
              IF (SZO .LE. SE(ID)) GO TO 52
           END DO
           ID = 9
52         BL_XVZ = (SZO/AE(ID))**CE(ID)

        CASE (6)
C          STABILITY F
           DO ID = 1,9
              IF (SZO .LE. SF(ID)) GO TO 62
           END DO
           ID = 10
62         BL_XVZ = (SZO/AF(ID))**CF(ID)

        END SELECT

      END
C
C     ------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION BL_XVY (SYO,ISTAB)
C
C
C     ------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION :: SYO
      INTEGER          :: ISTAB
C
C
CCRT  Initialize BL_XVY, 12/27/2017
      BL_XVY = 0.0D0

      SELECT CASE (ISTAB)
         CASE (1)
            BL_XVY = (SYO/213.0D0)**1.1148D0

         CASE (2)
            BL_XVY = (SYO/155.0D0)**1.097D0

         CASE (3)
            BL_XVY = (SYO/103.0D0)**1.092D0

         CAsE (4)
            BL_XVY = (SYO/68.0D0)**1.076D0

         CASE (5)
            BL_XVY = (SYO/50.0D0)**1.086D0

         CASE (6)
            BL_XVY = (SYO/33.5D0)**1.083D0
      END SELECT

      END

      SUBROUTINE BL_WSC(ISTAB,UM,U,S,P,NBL)
C***********************************************************************
C             BL_WSC Routinee of the AMS/EPA Regulatory Model - AERMOD
C
C     Calculates an effective U using the line source plume rise
C     equation (line source term only) matched at X = XF (final rise)
C
C     INPUT:
C       ISTAB = P-G stability category
C       UM    = WSST = stack height wind speed from power law
C       S     = a computed parameter for stable conditions
C       NBL   = number of individual buoyant lines in the BL group 
C     OUTPUT:
C       U
C
C     ------------------------------------------------------------------
      USE BUOYANT_LINE
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(OUT) :: U
      DOUBLE PRECISION, INTENT(IN)  :: UM, S, P
      INTEGER, INTENT(IN) :: ISTAB, NBL

      DOUBLE PRECISION :: P2, P3, EP, EPI, T1, Z


      IF (ISTAB .LE. 4) THEN
C
C        NEUTRAL (OR UNSTABLE) CONDITIONS
C
         P3  = 3.0D0 * P
         EP  = 2.0D0 + P3
         EPI = 1.0D0 / EP

C        CONSTANT 2.4=4.*BETA WITH BETA=0.6
         T1 = (EP*EP * DBLE(NBL) * BLAVGFPRM * BLAVGBHGT**P3 /
     &            (2.4D0 * (2.0D0 + P) * BLAVGLLEN * UM**3))**EPI
         Z  = T1*XMATCH**(2.0D0*EPI)

C        CONSTANT 1.2 = 2.*BETA WITH BETA=0.6
         U  = (DBLE(NBL) * BLAVGFPRM /
     &         (1.2D0 * BLAVGLLEN) * (XMATCH/Z)*(XMATCH/Z))**0.333333D0
         U  = DMAX1(U,UM)

      ELSE
C
C        STABLE CONDITIONS
C
         P2 = 2.0D0 + P

C        CONSTANT 0.6 = BETA
         Z = (P2 * BLAVGBHGT**P * DBLE(NBL) * BLAVGFPRM /
     &       (0.6D0 * BLAVGLLEN * UM * S))**(1.0D0/P2)

C        CONSTANT 3.3333333 = 2./BETA WITH BETA=0.6
         U = 3.3333333D0 * DBLE(NBL) * BLAVGFPRM / (BLAVGLLEN*S*Z*Z)
         U = DMAX1(U,UM)
      ENDIF

      RETURN
      END
C
      SUBROUTINE BL_LENG(THETA, U, AVFACTOR, NBL)
C***********************************************************************
C             BL_LENG Routine of the AMS/EPA Regulatory Model - AERMOD
C
C     Calculates XFB,LEFF,LD,R0 (all in the buoyant_line module)
C
C     Input: 
C       THETA = angle of second rotation for the source y-axis (after
C               the first rotation) to align with the flow vector
C       U     = effective wind speed (BL_UEFF in calling routine)
C       AVFACTOR = constant (parameter) = 1.0D0
C       NBL   = number of individual buoyant lines in the BL group 
C
C     ------------------------------------------------------------------
      USE BUOYANT_LINE
      IMPLICIT NONE

      DOUBLE PRECISION, INTENT(IN) :: U, THETA, AVFACTOR
      DOUBLE PRECISION :: LEFF1, LEFFV, DXM, DXM833, RAD, T1, XI
      DOUBLE PRECISION :: THRAD, SINT, COST
      INTEGER, INTENT(IN) :: NBL

      DATA RAD/0.0174533D0/
C
C     BLAVGFPRM is the 'average' buoyancy flux of one line;
C     FPRMXNB is the 'effective' buoyancy flux of n lines
      FPRMXNB = DBLE(NBL) * BLAVGFPRM
      THRAD    = THETA * RAD
      SINT    = ABS(DSIN(THRAD))
      COST    = ABS(DCOS(THRAD))

C     Calculate distance of full buoyancy (XFB)
      DXM    = BLAVGBSEP + BLAVGBWID
      BL_XFB = BLAVGLLEN * COST + DBLE(NBL-1) * DXM * SINT

C     Calculate effective line source length (LEFF) and
C     effective downwash line length (LD)
      LEFF1 = BLAVGLLEN * SINT

      IF (NBL .EQ. 1) THEN
C        IF N = 1, NO INTERACTION AT ANY X, I.E.,
C        LEFFV = average line width;
C        'Effective' buoyancy flux (FPRMXNB) = average buoyancy flux;
C        Distance to full buoyancy (BL_XFB) =
C         (average building length) * COST + (average line width) * SINT
         LEFFV = BLAVGLWID
         FPRMXNB = BLAVGFPRM
         BL_XFB = BL_XFB + BLAVGLWID*SINT

      ELSE
C        CONSTANT 0.8333333 = 1./(2.*BETA) WITH BETA=0.6
         DXM833 = 0.8333333D0*DXM       ! DXM is 'average' distance between two lines

C        CONSTANT 2.2619467 = 2.*PI*BETA*BETA WITH BETA=0.6
C        CONSTANT 1.5915494 = 3./(PI*BETA) WITH BETA=0.6
         T1 = (2.2619467D0 * U**3 / BLAVGFPRM) *
     &              DXM833 * DXM833 * (DXM833 + 1.5915494D0*BLAVGLWID)
         XI = (T1*BLAVGLLEN)**0.333333D0

         IF (XI .GT. BLAVGLLEN) THEN
            XI = BLAVGLLEN/2.0D0 +
     &               DSQRT(12.0D0*T1 - 3.0D0*BLAVGLLEN*BLAVGLLEN)/6.0D0

C           CONSTANT 1.2 = 2.*BETA WITH BETA=0.6
C           CONSTANT 0.6283185 = PI*BETA/3. WITH BETA=0.6
            LEFFV = FPRMXNB * (BLAVGLLEN*BLAVGLLEN/3.0D0 +
     &          XI*(XI-BLAVGLLEN)) / (1.2D0 * U**3 * DXM833 * DXM833) -
     &          0.6283185D0 * DXM833

         ELSE
C           CONSTANT 3.6 = 6.*BETA WITH BETA=0.6
C           CONSTANT 0.6283185 = PI*BETA/3. WITH BETA=0.6
            LEFFV = FPRMXNB/(3.6D0 * BLAVGLLEN * DXM833 * DXM833) *
     &               (XI/U)**3 - 0.6283185D0 * DXM833
         ENDIF
      ENDIF

      LEFF = LEFF1 + LEFFV*COST
      LD   = LEFF * SINT

C     Calculate downwashed edge radius
      R0 = DMIN1(BLAVGBHGT,LD)/AVFACTOR

      RETURN
      END
C
C     ------------------------------------------------------------------
      SUBROUTINE BL_SORT(FTSAVE,IBMIN,IBMAX,IWPBL)
C
C             BL_SORT Routine of the AMS/EPA Regulatory Model - AERMOD
C
C     ------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT) :: FTSAVE(129)
      INTEGER, INTENT(INOUT)          :: IBMIN, IBMAX, IWPBL

C Unused:      DOUBLE PRECISION :: FT
      INTEGER          :: IB, ISAFE, ILEVEL, NEACHL, INCR, INDEXI, NC
      INTEGER          :: INCRM, INCRP

      ISAFE = 0
      IB    = 0
      IF (FTSAVE(129) .NE. 0.0D0) IB = 129
      IF (FTSAVE(1)   .NE. 0.0D0) IB = 1
      IF (IB .NE. 0) GO TO 970

      OUTER: DO ILEVEL = 1,7
         NEACHL = 2**(ILEVEL-1)
         INCR   = 2**(8-ILEVEL)
         INDEXI = 1 + INCR/2
         DO NC = 1,NEACHL
            IF (FTSAVE(INDEXI) .EQ. 0.0D0) GO TO 944
            IB = INDEXI
            GO TO 970
944         INDEXI = INDEXI + INCR
         END DO
      END DO OUTER

      IF (IB .NE. 0) GO TO 970
      IWPBL = 999
      RETURN

970   IBMIN = IB - 1
      IBMAX = IB + 1
      IBMIN = MAX(IBMIN,1)
      IBMAX = MIN(IBMAX,129)

975   CONTINUE
      INCRM = 0
      INCRP = 0
      IF (FTSAVE(IBMIN) .NE. 0.0D0) INCRM = 1
      IF (IBMIN .EQ.1) INCRM = 0
      IF (FTSAVE(IBMAX) .NE. 0.0D0) INCRP = 1
      IF (IBMAX .EQ. 129) INCRP = 0

      IBMIN = IBMIN - INCRM
      IBMAX = IBMAX + INCRP
      IF (INCRM .EQ. 0 .AND. INCRP .EQ. 0) GO TO 980
      ISAFE = ISAFE + 1
      IF (ISAFE .GT. 129) GO TO 980
      GO TO 975
980   CONTINUE

      RETURN
      END
C
C     ------------------------------------------------------------------
      SUBROUTINE BL_CUBIC(A,B,C,Z)
C
C             BL_CUBIC Routine of the AMS/EPA Regulatory Model - AERMOD
C
C     Solves for one root of the cubic equation:
C        Z**3 + A*Z**2 + B*Z + C = 0
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN)  :: A, B, C
      DOUBLE PRECISION, INTENT(OUT) :: Z
      DOUBLE PRECISION :: A3, AP, BP, AP3, BP2, TROOT, TR
      DOUBLE PRECISION :: APP, BSV, ONE, BPP, CM, ALPHA, SGN

      DATA ONE/1.0D0/

      A3    = A/3.0D0
      AP    = B - A*A3
      BP    = 2.0D0*A3**3 - A3*B + C
      AP3   = AP/3.0D0
      BP2   = BP/2.0D0
      TROOT = BP2*BP2 + AP3*AP3*AP3

      IF (TROOT .GT. 0.0D0) THEN
         TR  = DSQRT(TROOT)
         APP = (-BP2 + TR)**0.333333D0
         BSV = -BP2 - TR

         IF (BSV .EQ. 0.0D0) THEN
C           BSV (& BPP) = 0.0
            Z = APP-A3

         ELSE
            SGN = DSIGN(ONE,BSV)
            BPP = SGN*(DABS(BSV))**0.333333D0
            Z   = APP + BPP - A3
         ENDIF

      ELSE
         CM    = 2.0D0 * DSQRT(-AP3)
         ALPHA = DACOS(BP/(AP3*CM))/3.0D0
         Z     = CM*DCOS(ALPHA) - A3
      ENDIF

      RETURN
      END

      SUBROUTINE BL_ROTATE2 (WD1, THETA, ITHETA, NRECEP, NBL, KK)
C***********************************************************************
C             BL_ROTATE2 Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Rotates Sources and Receptors for Buoyant Line Source
C                 Calculations - Aligns the Y-Axis of the Source
C                 Parallel to the Wind Direction
C
C        PROGRAMMER: Amec Foster Wheeler
C
C        DATE:    January 5, 2015
C
C        INPUT:
C          WD1    - wind direction for the hour
C          NRECEP - number of receptors to rotate
C          NBL    - total number of buoyant lines
C          KK     - bouyant line source group number
C
C        OUTPUTS: 
C          Rotated Source and Receptor Coordinates
C          THETA  - Second rotation angle
C          ITHETA - Integer value of THETA
C
C        CALLED FROM:   BL_CALC
C***********************************************************************
C     Variable Declarations

      USE BUOYANT_LINE
C JAT 06/22/21 D065 COMMENT OUT KURDAT, NOT USED
C      USE MAIN1, ONLY : KURDAT

      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN)  :: WD1
      DOUBLE PRECISION, INTENT(OUT) :: THETA
      INTEGER, INTENT(IN)  :: NRECEP, NBL, KK
      INTEGER, INTENT(OUT) :: ITHETA

      DOUBLE PRECISION, DIMENSION(4) :: TCHK =
     &                                 [90.0D0,180.0D0,270.0D0,360.0D0]
      DOUBLE PRECISION :: DXX, XN, YN, XSAVE, YSAVE
      DOUBLE PRECISION :: COSTHETA, SINTHETA
      DOUBLE PRECISION,PARAMETER  :: DEG_PER_RAD = 57.29578D0

      INTEGER, DIMENSION(4) :: IL = [1,1,1,1]
      INTEGER, DIMENSION(4) :: ISEG = [1,129,129,1]
C JAT 06/22/21 REMOVE ISAVE AS UNUSED VARIABLE
C      INTEGER               :: I, J, ILINE, ISAVE, ISEGN, LNUM
      INTEGER               :: I, J, ILINE, ISEGN, LNUM
      INTEGER               :: IL12, IL34

      CHARACTER (LEN=12) MODNAM

C     Variable Initializations
      MODNAM = 'BL_ROTATE2'

CCRT  Initialize variables ! 12/27/2017
      YN = 0.0D0
      XN = 0.0D0
C D41_Wood  Initialize variables
      ILINE = 0
      ISEGN = 0

C     THETA is the rotation that aligns the y-axis (previously rotated
C      to be perpendicular to the buoyant lines) with the flow vector.

      THETA = 360.0D0 - (WD1 + TCOR(KK))
      IF (THETA .LT. 0.0D0) THETA = 360.0D0 + THETA
      THETA    = DMOD(THETA,360.0D0)
      ITHETA   = NINT(THETA)
      COSTHETA = DCOS(THETA/DEG_PER_RAD)
      SINTHETA = DSIN(THETA/DEG_PER_RAD)
C
C     Calculate source coordinates for each source line segment
C      Note that the coordinate YS_SCS was translated and rotated in
C      BL_ROTATE1 (soset.f)
C
      DO LNUM = 1,NBL
         IF (BLINEPARMS(LNUM)%IBLPGRPNUM .EQ. KK) THEN
            DXX = DEL(LNUM)/128.0D0
            XS_SCS(LNUM,1) = BLINEPARMS(LNUM)%XBEG_TR1

            DO J=2,129
               XS_SCS(LNUM,J) = XS_SCS(LNUM,J-1) + DXX
            END DO
         END IF
      END DO

C D41_WOOD: The values of the array IL must be adjusted so the correct 
C           XN and YN are determined for each BL source group
      IF (KK .EQ. 1) THEN
         IL(1) = 1
         IL(2) = 1
         IL(3) = NBLINGRP(KK)
         IL(4) = NBLINGRP(KK)

      ELSE IF (KK .GE. 2) THEN
         IL12 = 1
         IL34 = NBLINGRP(1)
         DO J = 2,KK
            IL12 = IL12 + NBLINGRP(J-1)
            IL34 = IL34 + NBLINGRP(J)
         END DO 
         IL(1) = IL12
         IL(2) = IL12
         IL(3) = IL34
         IL(4) = IL34
      END IF

C
C     Calculate XN, YN (origins of translated coordinate system
C      in terms of the SCS coordinates)
C
      OUTER: DO LNUM = 1,NBL
                IF (BLINEPARMS(LNUM)%IBLPGRPNUM .EQ. KK) THEN

      INNER:       DO I=1,4
                      IF (THETA .GE. TCHK(I)) CYCLE
                      ILINE = IL(I)
                      ISEGN = ISEG(I)
                      XN = XS_SCS(ILINE,ISEGN)
                      YN = YS_SCS(ILINE)
                      EXIT OUTER
                   END DO INNER
                END IF
             END DO OUTER

C
C --- Translate line source segment coordinates for this BL source
      DO LNUM = 1,NBL
         IF (BLINEPARMS(LNUM)%IBLPGRPNUM .EQ. KK) THEN
            DO J=1,129
               XS_RCS(LNUM,J) = XS_SCS(LNUM,J) - XN
               YS_RCS(LNUM,J) = YS_SCS(LNUM) - YN
               if(mod(j,10) == 0.0) then
               endif

            END DO
         END IF
      END DO
C
C     Translate receptor coordinates
      DO  I = 1,NRECEP
         XR_RCS(I) = XR_SCS(I,KK) - XN
         YR_RCS(I) = YR_SCS(I,KK) - YN
      END DO
C
C     Rotate coordinate system
C
C     Rotate line source segment coordinates
      DO LNUM = 1,NBL

         IF (BLINEPARMS(LNUM)%IBLPGRPNUM .EQ. KK) THEN
         
            DO J=1,129
               XSAVE = XS_RCS(LNUM,J)
               YSAVE = YS_RCS(LNUM,J)
               XS_RCS(LNUM,J) = XSAVE*COSTHETA + YSAVE*SINTHETA
               YS_RCS(LNUM,J) = YSAVE*COSTHETA - XSAVE*SINTHETA
            if(mod(j,10) == 0.0) then
            endif
            END DO
         END IF
      END DO

C     Rotate receptor coordinates
      DO I = 1,NRECEP
         XSAVE = XR_RCS(I)
         YSAVE = YR_RCS(I)
         XR_RCS(I) = XSAVE*COSTHETA + YSAVE*SINTHETA
         YR_RCS(I) = YSAVE*COSTHETA - XSAVE*SINTHETA
      END DO
      
      RETURN
      END
