! <hwyrcx.f90 - A component of the City-scale
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

      subroutine HWYRCX (U,Z,H,HL,X,XY,Y,WROAD,WINFL,SIGY0,SIGVE,SIGW0,       &
                        SIGWE,TLGR,KST,AN,M,SY,SZ,RC,building_height,canyon)

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
!           2016  M. Karl: arithmetic if statements replaced
!
! ----------------------------------------------------------------------------------

!MSK start 26.09.2017
      implicit none
!MSK end 26.09.2017

      REAL U,Z,H,HL,X,XY,Y,WROAD,WINFL,SIGY0,SIGVE,SIGW0,SIGWE,TLGR,AN,       &
          SY,SZ,RC

      INTEGER KST,M

! *** The subroutine solves the dispersion equations in the EPA-model.
! *** Equations 7-9 in Users's Guide (1980).

      REAL A1,A2,A3,A4,A5,A6,A7,C1,C2,C3,C4,C5,C6,C7,C8,C9,CA,CB,CC,       &
          CD,CE,CF,DUM,SUMME,T,TEMP,THL,YD

!MSK start
      real             :: building_height
      logical          :: canyon
!MSK end

      !write(*,*) Z,H,HL
      !stop
! *** SUBROUTINE HWYRCX (VERSION 80090),
! *** Part of HIWAY2 and HIWAY2I.
! ***
! *** This is the 1979 version of HWYRCX.
! ***
! *** U     WIND SPEED (M/SEC)
! *** Z     RECEPTOR HEIGHT (M)
! *** H     EFFECTIVE STACK HEIGHT (M)
! *** HL=L  HEIGHT OF LIMITING LID (M)
! *** X     DISTANCE RECEPTOR IS DOWNWIND OF SOURCE (KM)
! *** XY    X+VIRTUAL DISTANCE USED FOR AREA SOURCE APPROX. (KM)
! *** Y     DISTANCE RECEPTOR IS CROSSWIND FROM SOURCE (KM)
! *** KST   STABILITY CLASS
! ***
! *** THE OUTPUT VARIABLES ARE....
! ***
! *** AN    THE NUMBER OF TIMES THE SUMMATION TERM IS EVALUATED AND ADDED IN.
! *** RC   RELATIVE CONCENTRATION (SEC/M**3)

      IF (KST.GE.5) GO TO 50
! *** If the source is above the lid, set RC = 0., and return.

!MSK start "arithmetic if"
!MSK   IF (H-HL) 10,10,40

      IF ( (H-HL) .LT. 0.0) GO TO 10
      IF ( (H-HL) .EQ. 0.0) GO TO 10 
      IF ( (H-HL) .GT. 0.0) GO TO 40

!MSK end

!MSK start "arithmetic if"
!MSK   10    IF (Z-HL) 50,50,40
10    CONTINUE
      IF ( (Z-HL) .LT. 0.0) GO TO 50
      IF ( (Z-HL) .EQ. 0.0) GO TO 50 
      IF ( (Z-HL) .GT. 0.0) GO TO 40

!MSK end

40    RC=0.
      RETURN
! *** If X is less than 1 meter, set RC=0. and return. This avoids
! *** problems of incorrect values near the source.

!MSK start "arithmetic if"
!MSK   50    IF (X-0.001) 40,60,60
50    CONTINUE
      IF ( (X-0.001) .LT. 0.0) GO TO 40
      IF ( (X-0.001) .EQ. 0.0) GO TO 60 
      IF ( (X-0.001) .GT. 0.0) GO TO 60

!MSK end

! *** Call HWYSIG to obtain values for SY and SZ
!_LHS_Bedre-Byluft_January-2010_Start:
!
!_LHS: HWYSIG modified to include wind-speed dependence of the settings
!_LHS: of sigma-y0 and sigma-z0, according to the User Guide for HIWAY-2.

!_LHS 60   CALL HWYSIG (X,XY,KST,SY,SZ)
  60  call hwysig (X,XY,KST,U,building_height,canyon,SY,SZ)
!MSK start
!MSK Note: this corrects for the typically too low vertical turbulence in
!MSK Gaussian plume models
!MSK BRUCE TEST: SY=SY*3.
        SY=SY*3.
!MSK end

!_LHS_Bedre-Byluft_January-2010_End.

! *** Call new routine based on Irwin and Venkatram formulas for
! *** calculation of sigma-y and sigma-z values

!60   CONTINUE
!C    CALL LSIG(X,WROAD,WINFL,U,SIGY0,SIGVE,SIGW0,SIGWE,TLGR,SY,SZ)
!C    WRITE (*,'(A10,I7,99F7.1)') 'IR,LSIG6: ',
!C   .KST,X,WROAD,WINFL,U,SIGY0,SIGVE,SIGW0,SIGWE,TLGR,SY,SZ

! *** SY = SIGMA Y, THE STANDARD DEVIATION OF CONCENTRATION IN THE Y-DIRECTION (M)
! *** SZ = SIGMA Z, THE STANDARD DEVIATION OF CONCENTRATION IN THE Z-DIRECTION (M)
      C1=1.

!MSK start "arithmetic if"
!MSK    IF (Y) 70,90,70
      IF ( Y .LT. 0.0) GO TO 70
      IF ( Y .EQ. 0.0) GO TO 90 
      IF ( Y .GT. 0.0) GO TO 70

!MSK end
70    YD=1000.*Y
! *** YD is crosswind distance in meters.
      DUM=YD/SY
      TEMP=0.5*DUM*DUM

!MSK start "arithmetic if"
!MSK    IF (TEMP-50.) 80,40,40
      IF ( (TEMP-50.) .LT. 0.0) GO TO 80
      IF ( (TEMP-50.) .EQ. 0.0) GO TO 40 
      IF ( (TEMP-50.) .GT. 0.0) GO TO 40

!MSK end
80    C1=EXP(TEMP)

!MSK start "arithmetic if"
!MSK    90    IF (KST-4) 100,100,110
90    CONTINUE
      IF ( (KST-4) .LT. 0.0) GO TO 100
      IF ( (KST-4) .EQ. 0.0) GO TO 100
      IF ( (KST-4) .GT. 0.0) GO TO 110

!MSK end

!MSK start "arithmetic if"
!MSK    100   IF (HL-5000.) 190,110,110
100   CONTINUE
      IF ( (HL-5000.) .LT. 0.0) GO TO 190
      IF ( (HL-5000.) .EQ. 0.0) GO TO 110
      IF ( (HL-5000.) .GT. 0.0) GO TO 110

!MSK end

! *** If stable condition or unlimited mixing height,
! *** use equation 3.2 if Z = 0, or eq. 3.1 for non-zero Z.
110   C2=2.*SZ*SZ

!MSK start "arithmetic if"
!MSK    IF (Z) 40,120,140
      IF ( Z .LT. 0.0) GO TO 40
      IF ( Z .EQ. 0.0) GO TO 120
      IF ( Z .GT. 0.0) GO TO 140

!MSK end
120   C3=H*H/C2

!MSK start "arithmetic if"
!MSK     IF (C3-50.) 130,40,40
      IF ( (C3-50.) .LT. 0.0) GO TO 130
      IF ( (C3-50.) .EQ. 0.0) GO TO 40
      IF ( (C3-50.) .GT. 0.0) GO TO 40

!MSK end
130   A2=1./EXP(C3)
! *** Wade equation 3.2.
      RC=A2/(3.14159*U*SY*SZ*C1)
      M=1
      RETURN
140   A2=0.
      A3=0.
      CA=Z-H
      CB=Z+H
      C3=CA*CA/C2
      C4=CB*CB/C2

!MSK start "arithmetic if"
!MSK     IF (C3-50.) 150,160,160
      IF ( (C3-50.) .LT. 0.0) GO TO 150
      IF ( (C3-50.) .EQ. 0.0) GO TO 160
      IF ( (C3-50.) .GT. 0.0) GO TO 160

!MSK end
150   A2=1./EXP(C3)

!MSK start "arithmetic if"
!MSK    160   IF (C4-50.) 170,180,180
160   CONTINUE
      IF ( (C4-50.) .LT. 0.0) GO TO 170
      IF ( (C4-50.) .EQ. 0.0) GO TO 180
      IF ( (C4-50.) .GT. 0.0) GO TO 180

!MSK end
170   A3=1./EXP(C4)
! *** Wade equation 3.1.
180   RC=(A2+A3)/(6.28318*U*SY*SZ*C1)
      M=2
      RETURN
! *** If SIGMA-Z is greater than 1.6 times the mixing height,
! *** the distribution below the mixing height is uniform with
! *** height regardless of source height.

!MSK start "arithmetic if"
!MSK    190   IF (SZ/HL-1.6) 210,210,200
190   CONTINUE
      IF ( (SZ/HL-1.6) .LT. 0.0) GO TO 210
      IF ( (SZ/HL-1.6) .EQ. 0.0) GO TO 210
      IF ( (SZ/HL-1.6) .GT. 0.0) GO TO 200

!MSK end

! *** Wade equation 3.5.
200   RC=1./(2.5066*U*SY*HL*C1)
      M=3
      RETURN
! *** Initial value of AN set = 0.
210   AN=0.

!MSK start "arithmetic if"
!MSK    IF (Z) 40,370,220
      IF ( Z .LT. 0.0) GO TO 40
      IF ( Z .EQ. 0.0) GO TO 370
      IF ( Z .GT. 0.0) GO TO 220

!MSK end

! *** Statements 220 to 360 calculate RC, the relative concentration,
! *** using the equation discussed above. Several intermediate variables
! *** are used to avoid repeating calculations. Checks are made to be 
! *** sure that the argument of the exponential function is never greater
! *** than 50 (or less than -50). If 'AN' becomes greater than 45, a line
! *** of output is printed informing of this.
! *** Calculate multiple eddy reflections for receptor height Z.
220   A1=1./(6.28318*U*SY*SZ*C1)
      C2=2.*SZ*SZ
      A2=0.
      A3=0.
      CA=Z-H
      CB=Z+H
      C3=CA*CA/C2
      C4=CB*CB/C2

!MSK start "arithmetic if"
!MSK    IF (C3-50.) 230,240,240
      IF ( (C3-50.).LT. 0.0 ) GO TO 230
      IF ( (C3-50.).EQ. 0.0 ) GO TO 240
      IF ( (C3-50.).GT. 0.0 ) GO TO 240

!MSK end
230   A2=1./EXP(C3)

!MSK start "arithmetic if"
!MSK    240   IF (C4-50.) 250,260,260
240   CONTINUE
      IF ( (C4-50.).LT. 0.0 ) GO TO 250
      IF ( (C4-50.).EQ. 0.0 ) GO TO 260
      IF ( (C4-50.).GT. 0.0 ) GO TO 260

!MSK end
250   A3=1./EXP(C4)
260   SUMME=0.
      THL=2.*HL
270   AN=AN+1.
      A4=0.
      A5=0.
      A6=0.
      A7=0.
      C5=AN*THL
      CC=CA-C5
      CD=CB-C5
      CE=CA+C5
      CF=CB+C5
      C6=CC*CC/C2
      C7=CD*CD/C2
      C8=CE*CE/C2
      C9=CF*CF/C2

!MSK start "arithmetic if"
!MSK    IF (C6-50.) 280,290,290
      IF ( (C6-50.).LT. 0.0 ) GO TO 280
      IF ( (C6-50.).EQ. 0.0 ) GO TO 290
      IF ( (C6-50.).GT. 0.0 ) GO TO 290

!MSK end
280   A4=1./EXP(C6)

!MSK start "arithmetic if"
!MSK    290   IF (C7-50.) 300,310,310
290   CONTINUE
      IF ( (C7-50.).LT. 0.0 ) GO TO 300
      IF ( (C7-50.).EQ. 0.0 ) GO TO 310
      IF ( (C7-50.).GT. 0.0 ) GO TO 310

!MSK end
300   A5=1./EXP(C7)

!MSK start "arithmetic if"
!MSK    310   IF (C8-50.) 320,330,330
310   CONTINUE
      IF ( (C8-50.).LT. 0.0 ) GO TO 320
      IF ( (C8-50.).EQ. 0.0 ) GO TO 330
      IF ( (C8-50.).GT. 0.0 ) GO TO 330

!MSK end
320   A6=1./EXP(C8)

!MSK start "arithmetic if"
!MSK    330   IF (C9-50.) 340,350,350
330   CONTINUE
      IF ( (C9-50.).LT. 0.0 ) GO TO 340
      IF ( (C9-50.).EQ. 0.0 ) GO TO 350
      IF ( (C9-50.).GT. 0.0 ) GO TO 350

!MSK end
340   A7=1./EXP(C9)
350   T=A4+A5+A6+A7
      SUMME=SUMME+T

!MSK start "arithmetic if"
!MSK     IF (T-0.01) 360,270,270
      IF ( (T-0.01).LT. 0.0 ) GO TO 360
      IF ( (T-0.01).EQ. 0.0 ) GO TO 270
      IF ( (T-0.01).GT. 0.0 ) GO TO 270

!MSK end
360   RC=A1*(A2+A3+SUMME)
      M=5
      RETURN
! *** Calculate multiple eddy reflections for ground level receptor H
370   A1=1./(6.28318*U*SY*SZ*C1)
      A2=0.
      C2=2.*SZ*SZ
      C3=H*H/C2

!MSK start "arithmetic if"
!MSK     IF (C3-50.) 380,390,390
      IF ( (C3-50.).LT. 0.0 ) GO TO 380
      IF ( (C3-50.).EQ. 0.0 ) GO TO 390
      IF ( (C3-50.).GT. 0.0 ) GO TO 390

!MSK end

380   A2=2./EXP(C3)
390   SUMME=0.
      THL=2.*HL
400   AN=AN+1.
      A4=0.
      A6=0.
      C5=AN*THL
      CC=H-C5
      CE=H+C5
      C6=CC*CC/C2
      C8=CE*CE/C2

!MSK start "arithmetic if"
!MSK     IF (C6-50.) 410,420,420
      IF ( (C6-50.).LT. 0.0 ) GO TO 410
      IF ( (C6-50.).EQ. 0.0 ) GO TO 420
      IF ( (C6-50.).GT. 0.0 ) GO TO 420

!MSK end
410   A4=2./EXP(C6)

!MSK start "arithmetic if"
!MSK     420   IF (C8-50.) 430,440,440
420   CONTINUE
      IF ( (C8-50.).LT. 0.0 ) GO TO 430
      IF ( (C8-50.).EQ. 0.0 ) GO TO 440
      IF ( (C8-50.).GT. 0.0 ) GO TO 440

!MSK end
430   A6=2./EXP(C8)
440   T=A4+A6
      SUMME=SUMME+T

!MSK start "arithmetic if"
!MSK      IF (T-0.01) 450,400,400
      IF ( (T-0.01).LT. 0.0 ) GO TO 450
      IF ( (T-0.01).EQ. 0.0 ) GO TO 400
      IF ( (T-0.01).GT. 0.0 ) GO TO 400

!MSK end
450   RC=A1*(A2+SUMME)
      M=4
      RETURN
      end subroutine hwyrcx
