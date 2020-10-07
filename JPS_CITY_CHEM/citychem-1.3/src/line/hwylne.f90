! <hwylne.f90 - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************! 
!* 
!* EPISODE - An urban-scale air quality model
!* ========================================== 
!* Copyright (C) 2018  NILU - Norwegian Institute for Air Research
!*                     Instituttveien 18
!*                     PO Box 100
!*                     NO-2027 Kjeller
!*                     Norway
!*
!*                     Contact persons: Gabriela Sousa Santos - gss@nilu.no
!*                                      Paul Hamer - pdh@nilu.no
!*
!* Unless explicitly acquired and licensed from Licensor under another license,
!* the contents of this file are subject to the Reciprocal Public License ("RPL")
!* Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!* allowed by the RPL, and You may not copy or use this file in either source code
!* or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!* All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!* WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!* DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!* See the RPL for specific language governing rights and limitations under the RPL.
!*
!* ========================================== 
!* The dispersion model EPISODE (Grønskei et. al., 1993; Larssen et al., 1994;
!* Walker et al., 1992, 1999; Slørdal et al., 2003, 2008) is an Eulerian grid model 
!* with embedded subgrid models for calculations of pollutant concentrations resulting 
!* from different types of sources (area-, line- and point sources). EPISODE solves the 
!* time dependent advection/-diffusion equation on a 3 dimensional grid. 
!* Finite difference numerical methods are applied to integrate the solution forward in time. 
!* It also includes extensions as the implementation of a simplified EMEP photochemistry 
!* scheme for urban areas (Walker et al. 2004) and a scheme for Secondary Organic Aerosol 
!* implemented by Håvard Slørdal
!*
!* Grønskei, K.E., Walker, S.E., Gram, F. (1993) Evaluation of a model for hourly spatial
!*    concentrations distributions. Atmos. Environ., 27B, 105-120.
!* Larssen, S., Grønskei, K.E., Gram, F., Hagen, L.O., Walker, S.E. (1994) Verification of 
!*    urban scale time-dependent dispersion model with sub-grid elements in Oslo, Norway. 
!*    In: Air poll. modelling and its appl. X. New York, Plenum Press.
!* Slørdal, L.H., McInnes, H., Krognes, T. (2008): The Air Quality Information System AirQUIS. 
!*    Info. Techn. Environ. Eng., 1, 40-47, 2008.
!* Slørdal, L.H., Walker, S.-E., Solberg, S. (2003) The Urban Air Dispersion Model EPISODE 
!*    applied in AirQUIS. Technical Description. NILU TR 12/2003. ISBN 82-425-1522-0.
!* Walker, S.E., Grønskei, K.E. (1992) Spredningsberegninger for on-line overvåking i Grenland. 
!*    Programbeskrivelse og brukerveiledning. Lillestrøm, 
!*    Norwegian Institute for Air Research (NILU OR 55/92).
!* Walker, S.E., Slørdal, L.H., Guerreiro, C., Gram, F., Grønskei, K.E. (1999) Air pollution 
!*    exposure monitoring and estimation. Part II. Model evaluation and population exposure. 
!*    J. Environ. Monit, 1, 321-326.
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a 
!*    simplified EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*****************************************************************************!

      subroutine HWYLNE (XRV,YRV,ZRV,MQLL,NQLL,QLN,RAQ,SAQ,RBQ,SBQ,       &
                        WROAD,WINFL,THETA,UE,KST,HL,SIGY0,SIGVE,SIGW0,       &
                        SIGWE,TLGRE,building_height,canyon,CRV)

! ----------------------------------------------------------------------------------
! Based on:
! Version Episode 5.5 (May29, 2012) prepared for BB-Stand-Alone
! Original source code of EPISODE by Sam-Erik Walker (NILU)
!
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
! ----------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!           2016  M. Karl: Statement functions replaced by functions in mod_util
!           20178 M. Karl: building_height and canyon for street canyon model
!
! ----------------------------------------------------------------------------------

       use mod_util

       implicit none
! *** This subroutine is the main routine in the EPA-model NWYLNE Ver. 89001
! *** which is part of NEWAY2.

! *** Modified Oct. 1979 to add Romberg integration enhancements.
! *** Modified Sep. 1989 to improve location search.

      INTEGER MQLL,NQLL,KST
      REAL XRV,YRV,ZRV,QLN(MQLL),RAQ(MQLL),SAQ(MQLL),RBQ(MQLL),       &
          SBQ(MQLL),WROAD,WINFL,THETA,UE,HL,SIGY0,SIGVE,SIGW0,       &
          SIGWE,TLGRE,CRV(MQLL)
!MSK start
      real             :: building_height
      logical          :: canyon
!MSK end

      REAL R,S,TR,SINT,COST,PIN,UZ,RREC,SREC,Z,R1,S1,R2,S2,QL,H,X1,X,       &
          X2,Y1,Y,Y2,TG30,XGR,XSM,YGR,YSM,XDIS,XA,XB,YA,YRT1,YB,YRT2,       &
          PARL1,PARL2,PARL3,PARL4,A,YRT,B,XRT,XRT1,XRT2,DISX,DISY,       &
          DISI,FILIM,SUMME,XDUM,YDUM,XZ,XY,AN,SY,SZ,RC,CURR,PREV,DELD,       &
          DX,DY,OLD1,DENOM,OLD2,TEST,CUROLD,YDIS,FAC

      REAL RR(1),SR(1)

      INTEGER NS,KMAX,INDEXE,ILIM,M,I,ILIM1,K,KK,KKK

      INTEGER NQ,NC

! *** IMPLICIT NONE inclusions (BRUCE)
      INTEGER NR
      REAL U,ZZ,TLGR

!C    REAL WROAD,WINFL

! *** This subroutine is the main routine in the EPA-model.

!                    SUBROUTINE NWYLNE (VERSION 89001),
!                     PART OF NEWAY2 

!     COMMON /SOL/ QLN(2),RAQ(2),SAQ(2),RBQ(2),SBQ(2),CON(200)

!C    INCLUDE 'include/inc_put
!C    INCLUDE 'include/inc_sol

!     COMMON /WEA/ THETA,U,KST,HL
!C    INCLUDE 'include/inc_wea     

!C    REAL R,S,TR,SINT,COST,PIN,UZ,RREC,SREC,Z,R1,S1,R2,S2,QL,H,X1,X,
!C   .     X2,Y1,Y,Y2,TG30,XGR,XSM,YGR,YSM,XDIS,XA,XB,YA,YRT1,YB,YRT2,
!C   .     PARL1,PARL2,PARL3,PARL4,A,YRT,B,XRT,XRT1,XRT2,DISX,DISY,
!C   .     DISI,FILIM,SUM,XDUM,YDUM,XZ,XY,AN,SY,SZ,RC,CURR,PREV,DELD,
!C   .     DX,DY,OLD1,DENOM,OLD2,TEST,CUROLD,YDIS,FAC

!C    INTEGER NS,KMAX,INDEX,ILIM,M,I,ILIM1,K,KK,KKK

!C    DIMENSION T(10)
      REAL T(10),CON(1)
      DATA KMAX /9/

!MSK start: Statement functions replaced by functions in mod_util
!
! *** MODIFIED OCT. 1979 TO ADD ROMBERG INTEGRATION ENHANCEMENTS.
! *** MODIFIED SEP. 1989 TO IMPROVE LOCATION SEARCH
!      X(R,S)=(R-RREC)*SINT+(S-SREC)*COST
! *** X is UPWIND DISTANCE OF R,S FROM RREC,SREC
!      Y(R,S)=(S-SREC)*SINT-(R-RREC)*COST
! ***   Y is CROSSWIND DISTANCE OF R,S FROM RREC,SREC
!
!MSK end

      NQ = NQLL
      NR = 1
      NC = 1

! *** Scale X,Y coordinates from m to km

      RR(1) = XRV/1000.
      SR(1) = YRV/1000.
      U = UE
      ZZ = ZRV
      TLGR = TLGRE

! *** Initialize concentrations

      CON(1) = 0.
      DO 10 NS = 1,NQ
      CRV(NS) = 0.
   10 CONTINUE

      TR=THETA/57.2958
      SINT=SIN(TR)
      COST=COS(TR)
      PIN=0.02
      UZ=U
! *** Calculate concentrations for each receptor.
!     DO 840 NC=1,NR
      RREC=RR(NC)
      SREC=SR(NC)
      Z=ZZ
! *** Sum concentrations over each lane.
      DO 830 NS=1,NQ
      R1=RAQ(NS)
      S1=SAQ(NS)
      R2=RBQ(NS)
      S2=SBQ(NS)
      QL=QLN(NS)
      H=0.0
!MSK start
!      X1=X(R1,S1)
!      X2=X(R2,S2)
!      Y1=Y(R1,S1)
!      Y2=Y(R2,S2)
      X1 = XLINE(R1,S1,SINT,COST,RREC,SREC)
      X2 = XLINE(R2,S2,SINT,COST,RREC,SREC)
      Y1 = YLINE(R1,S1,SINT,COST,RREC,SREC)
      Y2 = YLINE(R2,S2,SINT,COST,RREC,SREC)
!MSK end
      TG30=0.5774
      XDIS=(X2-X1)
      YDIS=(Y2-Y1)
      XGR=AMAX1(X2,X1)
      XSM=AMIN1(X2,X1)
      YGR=AMAX1(Y2,Y1)
      YSM=AMIN1(Y2,Y1)
      IF(XGR.LE.0) GO TO 830
      IF(XSM.GE.6) GO TO 840
!C    WRITE (*,*) 'HWYLNE: GR',XSM,YSM,XGR,YGR,XDIS,YDIS,TG30,X1,X2,
!C   .            Y1,Y2

! *** Check location of linesource in wind directed coordinates with 
! *** receptor as Origo , part of linesource to be considered is marked
! *** XA,XB,YA,YB and must be within a 60 degrees sector upwind from
! *** receptor. Exits to label 600 when XA,XB,YA,YB is found,
! *** If linesource is outside sector,get new line. The crossing points
! *** between the sector and extended linesource are named XRT1,YRT1
! *** (upper bound) and XRT2,YRT2 (lower bound)
      IF(XDIS.GT.-0.0005.AND.XDIS.LT.0.0005) THEN
          YRT1=X1*TG30+0.03
          YRT2=-X1*TG30-0.03
          IF(YGR.LT.YRT2.OR.YSM.GT.YRT1) GO TO 840
          IF(YGR.GE.YRT1)THEN
             XA=X1
             XB=X1
!C           WRITE (*,*) 'XB1: ',XB
             YA=YRT1
             IF(YSM.LE.YRT2)THEN
                YB=YRT2
             ELSE
                YB=YSM
             END IF
             GO TO 600
          ELSE IF(YSM.LE.YRT2)THEN
             XA=X1
             XB=X1
!C           WRITE (*,*) 'XB2: ',XB
             YA=YGR
             YB=YRT2
             GO TO 600
          ELSE
             XA=X1
             XB=X1
!C           WRITE (*,*) 'XB3: ',XB
             YA=YGR
             YB=YSM
             GO TO 600
          END IF
      END IF
! *** Extended linesource given by equation Y=X*A+B
        A=YDIS/XDIS
        B=(Y1*X2-X1*Y2)/XDIS
        PARL1=TG30+0.0001
        PARL2=TG30-0.0001
        PARL3=-TG30-0.0001
        PARL4=-TG30+0.0001
        XA=XGR
        YA=YGR
        XB=XSM
!C      WRITE (*,*) 'XB4: ',XB
        YB=YSM
! ***   PARL shows coefficients nearly parallel to the sector boundaries
        IF(A.LT.PARL1.AND.A.GT.PARL2)THEN
           YRT=(B+0.03)*0.5-0.03
           XRT=(-B-0.03)/(2*TG30)
           IF(XGR.LT.XRT) GO TO 840
           IF(YRT.GT.0) GO TO 830
              IF(XSM.LE.XRT)THEN
                XB=XRT
!C      WRITE (*,*) 'XB5: ',XB
                YB=YRT
              END IF
              GO TO 500
        ELSE IF(A.GT.PARL3.AND.A.LT.PARL4)THEN
           YA=YSM
           YB=YGR
           YRT=(B-0.03)*0.5+0.03
           XRT=(B-0.03)/(2*TG30)
           IF(XGR.LT.XRT) GO TO 840
           IF(YRT.LT.0) GO TO 830
              IF(XSM.LE.XRT)THEN
                XB=XRT
!C      WRITE (*,*) 'XB6: ',XB
                YB=YRT
              END IF
              GO TO 500
        END IF

          XRT1=(0.03-B)/(A-TG30)
          YRT1=XRT1*TG30+0.03
          XRT2=(-B-0.03)/(A+TG30)
          YRT2=-XRT2*TG30-0.03

        IF(A.GT.PARL1)THEN
            IF(XRT1.LT.0) GO TO 830
            IF(XSM.GT.XRT1) GO TO 830
            IF(XGR.LT.XRT2) GO TO 830
            IF(XGR.GE.XRT1)THEN
              XA=XRT1
              YA=YRT1
            END IF
            IF(XRT2.GT.XSM)THEN
              XB=XRT2
!C      WRITE (*,*) 'XB7: ',XB
              YB=YRT2
            END IF
            GO TO 500
        ELSE IF(A.GE.0) THEN
          IF(B.GT.0.03)THEN
            IF(XGR.LE.XRT1)GO TO 830
            IF(XB.LT.XRT1)THEN
               XB=XRT1
!C      WRITE (*,*) 'XB8: ',XB
               YB=YRT1
            END IF
          ELSE IF(B.LE.-0.03)THEN
            IF(XGR.LE.XRT2)GO TO 830
            IF(XB.LT.XRT2)THEN
               XB=XRT2
!C      WRITE (*,*) 'XB9: ',XB
               YB=YRT2
            END IF
          END IF
          GO TO 500
       ELSE IF(A.GT.PARL4)THEN
        YA=YSM
        YB=YGR
          IF(B.GT.0.03)THEN
            IF(XGR.LE.XRT1)GO TO 830
            IF(XB.LT.XRT1)THEN
               XB=XRT1
!C      WRITE (*,*) 'XB10: ',XB
               YB=YRT1
            END IF
          ELSE IF(B.LE.-0.03)THEN
            IF(XGR.LE.XRT2)GO TO 830
            IF(XB.LT.XRT2)THEN
               XB=XRT2
!C      WRITE (*,*) 'XB11: ',XB
               YB=YRT2
            END IF
          END IF
          GO TO 500
       ELSE
        YA=YSM
        YB=YGR
         IF(XRT2.LT.0)GO TO 830
         IF(XSM.GE.XRT2)GO TO 830
         IF(XGR.LE.XRT1)GO TO 830
         IF(XGR.GE.XRT2)THEN
              XA=XRT2
              YA=YRT2
         END IF
         IF(XRT1.GT.XSM)THEN
              XB=XRT1
!C      WRITE (*,*) 'XB12: ',XB
              YB=YRT1
         END IF
         GO TO 500
       END IF
  500 IF(XA.GE.6)THEN
       XA=6
       YA=6*A+B
      END IF
      IF(XB.LT.0)THEN
       XB=0
!C    WRITE (*,*) 'XB13: ',XB
       YB=B
      END IF
600   CONTINUE
! *** Do a TRAPEZOIDAL INTEGRATION from A to B in ten steps.
! *** It is likely that A or B have been redefined.
!C    WRITE (*,*) 'HWYLNE: A B ',XA,XB,YA,YB
      DISX=XB-XA
      DISY=YB-YA
      DISI=SQRT(DISX*DISX+DISY*DISY)
! *** DISI is distance (km) from A to B
      INDEXE=0
      ILIM=3
610   CONTINUE
      FILIM=FLOAT(ILIM)
      FAC=1000./FILIM
      DELD=DISI*FAC
! *** DELD is 1/FILIM DISI in meters.
      DX=DISX/FILIM
      DY=DISY/FILIM
      SUMME=0.
      XDUM=XA
      YDUM=YA
      IF (XDUM.LE.0.) GO TO 620
      XZ=XDUM
      XY=XDUM
!     CALL HWYRCX (UZ,Z,H,HL,XZ,XY,YDUM,KST,AN,M,SY,SZ,RC)
      call hwyrcx (UZ,Z,H,HL,XZ,XY,YDUM,WROAD,WINFL,SIGY0,SIGVE,SIGW0,       &
                  SIGWE,TLGR,KST,AN,M,SY,SZ,RC,building_height,canyon)
      SUMME=SUMME+RC/2.
620   ILIM1=ILIM-1
      DO 630 I=1,ILIM1
      XDUM=XDUM+DX
      YDUM=YDUM+DY
      IF (XDUM.LE.0.) GO TO 630
      XZ=XDUM
      XY=XDUM
!     CALL HWYRCX (UZ,Z,H,HL,XZ,XY,YDUM,KST,AN,M,SY,SZ,RC)
      call hwyrcx (UZ,Z,H,HL,XZ,XY,YDUM,WROAD,WINFL,SIGY0,SIGVE,SIGW0,       &
                  SIGWE,TLGR,KST,AN,M,SY,SZ,RC,building_height,canyon)
      SUMME=SUMME+RC
630   CONTINUE
      XDUM=XDUM+DX
      YDUM=YDUM+DY
      IF (XDUM.LE.0.) GO TO 640
      XZ=XDUM
      XY=XDUM
!     CALL HWYRCX (UZ,Z,H,HL,XZ,XY,YDUM,KST,AN,M,SY,SZ,RC)
      call hwyrcx (UZ,Z,H,HL,XZ,XY,YDUM,WROAD,WINFL,SIGY0,SIGVE,SIGW0,       &
                  SIGWE,TLGR,KST,AN,M,SY,SZ,RC,building_height,canyon)
      SUMME=SUMME+RC/2.
! *** Integrated value is CURR.
640   CURR=SUMME*DELD

      T(1)=CURR
      K=0
      DO 650 KK=2,10
        T(KK)=0.
650   CONTINUE

! *** First estimate completed here.
660   PREV=CURR
! *** Evaluate for POINTS in between those already evaluated.
      DELD=DELD/2.
      XDUM=XA+DX/2.
      YDUM=YA+DY/2.
      DO 680 I=1,ILIM
      IF (XDUM.LE.0.) GO TO 670
      XZ=XDUM
      XY=XDUM
!     CALL HWYRCX (UZ,Z,H,HL,XZ,XY,YDUM,KST,AN,M,SY,SZ,RC)
      call hwyrcx (UZ,Z,H,HL,XZ,XY,YDUM,WROAD,WINFL,SIGY0,SIGVE,SIGW0,       &
                  SIGWE,TLGR,KST,AN,M,SY,SZ,RC,building_height,canyon)
! *** NOTE: Add these to RC's found above.
      SUMME=SUMME+RC
670   XDUM=XDUM+DX
680   CONTINUE
      YDUM=YDUM+DY
      CURR=SUMME*DELD
! *** Second estimate completed here. Also fourth, sixth, etc.

      K=K+1
      OLD1=T(1)
      T(1)=CURR
      DENOM=4
      DO 690 KK=1,K
      KKK=KK+1
      OLD2=T(KKK)
      T(KKK)=T(KK)+(T(KK)-OLD1)/(DENOM-1)
      OLD1=OLD2
      DENOM=DENOM*4
690   CONTINUE
      CURR=T(KK)
      IF(K.EQ.1.AND.CURR.EQ.0) GO TO 700
      IF(K.EQ.3.AND.CURR.EQ.0) GO TO 770

!_LHS_Change_07March_2012_Start:
      if(ABS(CURR) <  TINY(CURR))then
        CURR = 2.0 * TINY(CURR)
        print '(A,E12.5)', 'HWYLNE: CURR = ',CURR
      endif
!_LHS_Change_07March_2012_End.

      IF (INDEXE.EQ.0) TEST=ABS((CURR-PREV)/CURR)
      IF (INDEXE.EQ.1) TEST=ABS((CURR-CUROLD)/CUROLD)
! *** If within PIN of last value (PREV), consider this as final value
!MSK start "arithmetic if"
!MSK   IF (TEST-PIN) 770,700,700

      IF ( (TEST-PIN) .LT. 0.0) GO TO 770
      IF ( (TEST-PIN) .EQ. 0.0) GO TO 700 
      IF ( (TEST-PIN) .GT. 0.0) GO TO 700

!MSK end      
700   ILIM=ILIM*2
      IF (K.GE.KMAX) GO TO 750
      PREV=CURR
! *** Evaluate points in between
      DELD=DELD/2.
      DX=DX/2.
      DY=DY/2.
      XDUM=XA+DX/2.
      YDUM=YA+DY/2.
      DO 720 I=1,ILIM
      IF (XDUM.LE.0.) GO TO 710
      XZ=XDUM
      XY=XDUM
!     CALL HWYRCX (UZ,Z,H,HL,XZ,XY,YDUM,KST,AN,M,SY,SZ,RC)
      call hwyrcx (UZ,Z,H,HL,XZ,XY,YDUM,WROAD,WINFL,SIGY0,SIGVE,SIGW0,       &
                  SIGWE,TLGR,KST,AN,M,SY,SZ,RC,building_height,canyon)
      SUMME=SUMME+RC
710   XDUM=XDUM+DX
720   CONTINUE
      YDUM=YDUM+DY
      CURR=SUMME*DELD

      K=K+1
      OLD1=T(1)
      T(1)=CURR
      DENOM=4
      DO 730 KK=1,K
      KKK=KK+1
      OLD2=T(KKK)
      T(KKK)=T(KK)+(T(KK)-OLD1)/(DENOM-1)
      OLD1=OLD2
      DENOM=DENOM*4
730   CONTINUE
      CURR=T(KK)
      IF(K.EQ.2.AND.CURR.EQ.0) GO TO 740

!_LHS_Change_07March_2012_Start:
      if(ABS(CURR) <  TINY(CURR))then
        CURR = 2.0 * TINY(CURR)
        print '(A,E12.5)', 'HWYLNE: CURR = ',CURR
      endif
!_LHS_Change_07March_2012_End.
      IF (INDEXE.EQ.0) TEST=ABS((CURR-PREV)/CURR)
      IF (INDEXE.EQ.1) TEST=ABS((CURR-CUROLD)/CUROLD)
! ***   Third ESTIMATE COMPLETED HERE. ALSO FIFTH,SEVENTH, ETC.

!MSK start "arithmetic if"
!MSK   IF (TEST-PIN) 770,740,740

      IF ( (TEST-PIN) .LT. 0.0) GO TO 770
      IF ( (TEST-PIN) .EQ. 0.0) GO TO 740 
      IF ( (TEST-PIN) .GT. 0.0) GO TO 740

!MSK end
740   ILIM=ILIM*2
      DX=DX/2.
      DY=DY/2.
      IF (K.GE.KMAX) GO TO 750
      GO TO 660
750   IF (INDEXE.EQ.1) GO TO 770
      CUROLD=CURR
      INDEXE=1
      ILIM=4
      GO TO 610
! *** At 770 have final value of integration in CURR
770   IF (INDEXE.EQ.1) CURR=AMIN1(CURR,CUROLD)
      RC=CURR
      CON(NC)=CON(NC)+RC*QL
      CRV(NS) = RC*QL
830   CONTINUE
      CON(NC)=1.0E+6*CON(NC)
      DO 850 NS=1,NQ
      CRV(NS) = 1.E+6*CRV(NS)
850   CONTINUE
840   CONTINUE
      RETURN

      end subroutine hwylne
