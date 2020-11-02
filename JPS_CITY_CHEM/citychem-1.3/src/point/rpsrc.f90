! <rpsrc.f90 - A component of the City-scale
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

      subroutine RPSRC
      
! The subroutine reads all point source data from file.
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
!           2017  M. Karl: Chemicals must have the same compound number in runscript 
!                          and psrc file
!    15 Feb 2018  M. Karl: L326 Commented RETURN statement
!
! ----------------------------------------------------------------------------------
      
      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_psrc
      use mod_phot

      implicit none

! External functions

!MSK      LOGICAL ATTIME
!MSK      LOGICAL BFTIME

! ATTIME - If at     given time then true else false
! BFTIME - If before given time then true else false

! Local variables
      
      REAL LON0
      REAL QDIV
      REAL QEMV
      REAL,ALLOCATABLE :: QEMVEC(:)
      REAl QHBV
      REAL QHEV
      REAL QHSV
      REAL QSYV
      REAL QSZV
      REAL QTEV
      REAL QTGV
      REAL QVGV
      REAL QWBV
      REAL QXV
      REAL QYV
      REAL QZV
      REAL UTME
      REAL UTMN

      INTEGER I
      INTEGER I1
      INTEGER I2
      INTEGER IC
      INTEGER IQ
      INTEGER J
      INTEGER NPOS
!MSK      CHARACTER*256 QIDV
      character(len=10) QIDV
      LOGICAL LEOF
      
! I      - Index
! IC     - Compound index
! IQ     - Point source index
! I1     - Index
! I2     - Index
! J      - Index
! NPOS   - Number of positions
! LEOF   - If end of file then true else false
! LON0   - Longitude
! QDIV   - Point source diameter
! QEMV   - Point source emission
! QEMVEC - Point source emission vector
! QHBV   - Point source height of building
! QHEV   - Point source height of emission
! QHSV   - Point source stack height
! QIDV   - Point source identification
! QSYV   - Point source plume segment sigma-y
! QSZV   - Point source plume segment sigma-z
! QTEV   - Point source thermal energy
! QTGV   - Point source temperature of gas
! QVGV   - Point source velocity of gas
! QWBV   - Point source width of building
! QXV    - Point source coordinates
! QYV    - Point source coordinates
! QZV    - Point source coordinates
! UTME   - UTM east  coordinate
! UTMN   - UTM north coordinate

      REAL,ALLOCATABLE :: QDI2(:)
      REAL,ALLOCATABLE :: QEM2(:,:)
      REAL,ALLOCATABLE :: QHB2(:)
      REAL,ALLOCATABLE :: QHE2(:)
      REAL,ALLOCATABLE :: QHS2(:)
      REAL,ALLOCATABLE :: QPS2(:)
      REAL,ALLOCATABLE :: QSY2(:)
      REAL,ALLOCATABLE :: QSZ2(:)
      REAL,ALLOCATABLE :: QTE2(:)
      REAL,ALLOCATABLE :: QTG2(:)
      REAL,ALLOCATABLE :: QVG2(:)
      REAL,ALLOCATABLE :: QWB2(:)
      REAL,ALLOCATABLE :: QX2(:)
      REAL,ALLOCATABLE :: QY2(:)
      REAL,ALLOCATABLE :: QZ2(:)

      CHARACTER(LEN=256),ALLOCATABLE :: QID2(:)
      INTEGER,ALLOCATABLE :: CHANGE2(:)

      !  print *,'rpsrc: exist', PSRCFE, PSRCFN
 
! Check if point source file exists

      IF (MAINFE .AND. .NOT. PSRCFE) THEN     

! Set change time to end time

          DO 10 I = 1,6
              CHTIME(I) = EDAT(I)
   10     CONTINUE
          CHTIME(5) = 59
          CHTIME(6) = 59

! Set number of point sources to zero

          NQ = 0
          RONCE = .TRUE.
          IF (MESSFE) WRITE (MESSUN,2000)
         
          RETURN
         
      ENDIF

! Check if already read point sources file heading

      IF (RONCE) THEN

! the point source file header has been read, goto next point source entry
!         IF (MESSFE) WRITE (MESSUN,2030) (IQC(IC),IC=1,NC)
!         IF (MESSFE) WRITE (MESSUN,2040) IQU,IQT

          GOTO 200

      ENDIF

! Read point source file heading

! Read number of compound column indices

      IF (PSRCFE) THEN
          CALL NXTDAT(PSRCUN,LEOF)      
          READ (PSRCUN,*) NQVEC
      ENDIF

      IF (NQVEC .GT. 0) THEN
          IF (.NOT. ALLOCATED(QEMVEC)) ALLOCATE(QEMVEC(NQVEC))
          IF (.NOT. ALLOCATED(JVEC))   ALLOCATE(JVEC(NQVEC))
          IF (.NOT. ALLOCATED(CMPVEC)) ALLOCATE(CMPVEC(NQVEC))
      ENDIF

      MQVEC = NQVEC
 
! Read and calculate compound column index array

      IF (PSRCFE) THEN

          BACKSPACE(PSRCUN)
          READ (PSRCUN,*) NQVEC,(CMPVEC(J),JVEC(J),J=1,NQVEC)

           !print *,'rpsrc: NQVEC CMPVEC JVEC', NQVEC,(CMPVEC(J),JVEC(J),J=1,NQVEC)

! Find maximum of JVEC array
      
          MJVEC = 0
          DO 100 J = 1,NQVEC
              MJVEC = MAX(MJVEC,JVEC(J))
  100     CONTINUE

      
          IF (MJVEC .GT. MQVEC) THEN
              IF (MESSFE) WRITE (MESSUN,2020) MQVEC
              CALL STOPIT('RPSRC: Too many columns in file')
          ENDIF

      ENDIF


! Define compound read indices

!MSK start  Must be same compound number in runscript and psrc file
      if (NQVEC .ne. NC) then

          CALL STOPIT('RPSRC: NQVEC must be same as NC')

      endif 

!MSK end

!MSK      DO 110 IC = 1,NC
!MSK
!MSK          IQC(IC) = 0
!MSK
!MSK          DO 120 J = 1,NQVEC
!MSK
!MSK              I1 = INDEX(CMPVEC( J),' ') - 1
!MSK              I2 = INDEX( CMPND(IC),' ') - 1
!MSK
!MSK              IF (I1 .LT. 1) I1 = 1
!MSK              IF (I2 .LT. 1) I2 = 1
!MSK
!MSK              IF (CMPVEC(J)(1:I1) .EQ. CMPND(IC)(1:I2))      IQC(IC) = JVEC(J)
!MSK
!MSK  120     CONTINUE
!MSK
!MSK  110 CONTINUE

!MSK start
      J=1
      DO 110 IC = 1,NC

         IQC(IC) = 0

         IQC(IC) = JVEC(J)
         J = J + 1
              print *,'rpsrc spec ', IC,J,IQC(IC)
  110 CONTINUE
!MSK end

      IF (MESSFE) WRITE (MESSUN,2030) (IQC(IC),IC=1,NC)
      !print *,'rpsrc 1: iqc ',(IQC(IC),IC=1,NC)

! Read unit indicators for emission and temperature

      IF (PSRCFE) THEN
          CALL NXTDAT(PSRCUN,LEOF)
          READ (PSRCUN,*) IQU,IQT
      ENDIF


      IF (MESSFE) WRITE (MESSUN,2040) IQU,IQT

! Read time from which the following data are valid

      IF (PSRCFE) THEN

          CALL NXTDAT(PSRCUN,LEOF)
          READ (PSRCUN,*) (CHTIME(I),I=1,6)

          IF ( 0 .LE. CHTIME(1) .AND. CHTIME(1) .LT. 50)      &
            CHTIME(1) = CHTIME(1) + 2000
          IF (50 .LE. CHTIME(1) .AND. CHTIME(1) .LE. 99)      &
            CHTIME(1) = CHTIME(1) + 1900

      ENDIF


! Initially there are no sources

      NQ = 0

! Read new number of sources

      IF (PSRCFE) THEN

          CALL NXTDAT(PSRCUN,LEOF)
          READ (PSRCUN,*) CHANQ
          NQ = CHANQ
          MQ = NQ

      ENDIF

! Finished reading file heading

      RONCE = .TRUE.

      !print *,'rpsrc: CHANQ ', RONCE, CHANQ, NQ, MQ

!MSK start moved after allocation section:
! If too early then return
!
!      IF (BFTIME(CHTIME)) RETURN
!MSK end

! Allocate temporary arrays

      IF (NQ .GT. 0) THEN

          IF (.NOT. ALLOCATED(QDI))    ALLOCATE(QDI(NQ))
          IF (.NOT. ALLOCATED(QEM))    ALLOCATE(QEM(NQ,NC))
          IF (.NOT. ALLOCATED(QHB))    ALLOCATE(QHB(NQ))
          IF (.NOT. ALLOCATED(QHE))    ALLOCATE(QHE(NQ))
          IF (.NOT. ALLOCATED(QHS))    ALLOCATE(QHS(NQ))
          IF (.NOT. ALLOCATED(QPS))    ALLOCATE(QPS(NQ))
          IF (.NOT. ALLOCATED(QSY))    ALLOCATE(QSY(NQ))
          IF (.NOT. ALLOCATED(QSZ))    ALLOCATE(QSZ(NQ))
          IF (.NOT. ALLOCATED(QTE))    ALLOCATE(QTE(NQ))
          IF (.NOT. ALLOCATED(QTG))    ALLOCATE(QTG(NQ))
          IF (.NOT. ALLOCATED(QVG))    ALLOCATE(QVG(NQ))
          IF (.NOT. ALLOCATED(QWB))    ALLOCATE(QWB(NQ))
          IF (.NOT. ALLOCATED(QX))     ALLOCATE(QX(NQ))
          IF (.NOT. ALLOCATED(QY))     ALLOCATE(QY(NQ))
          IF (.NOT. ALLOCATED(QZ))     ALLOCATE(QZ(NQ))
          IF (.NOT. ALLOCATED(QID))    ALLOCATE(QID(NQ))
          IF (.NOT. ALLOCATED(CHANGE)) ALLOCATE(CHANGE(NQ))


!!!===============================================
!!!  for moving point source, by Kang Dec.10,2019
!!   further changed for circular moving route @ Jan 21, 2020 by Kang          
          IF (.NOT. ALLOCATED(QX1))     ALLOCATE(QX1(NQ))
          IF (.NOT. ALLOCATED(QY1))     ALLOCATE(QY1(NQ))
          IF (.NOT. ALLOCATED(vel_point))     ALLOCATE(vel_point(NQ))
          IF (.NOT. ALLOCATED(dd_point))      ALLOCATE(dd_point(NQ))
          IF (.NOT. ALLOCATED(t_mpoint1))     ALLOCATE(t_mpoint1(NQ))
          IF (.NOT. ALLOCATED(t_mpoint2))     ALLOCATE(t_mpoint2(NQ))     

          IF (.NOT. ALLOCATED(cir_ang))     ALLOCATE(cir_ang(NQ)) 
       
      ENDIF
!!!===============================================
 

  200 CONTINUE


!all point sources have new emission value every hour
          do IQ = 1,CHANQ
            CHANGE(IQ) = IQ
          enddo


!MSK start
! Allocation of QEMVEC every time step of reading
      IF (NQVEC .GT. 0) THEN
          IF (.NOT. ALLOCATED(QEMVEC)) ALLOCATE(QEMVEC(NQVEC))
      ENDIF

      DO 240 IQ = 1,CHANQ

! If this point source has not changed goto next point source
      
          IF (CHANGE(IQ) .EQ. 0) GOTO 240

! Read next point source data line
          
          IF (PSRCFE) THEN

              CALL NXTDAT(PSRCUN,LEOF)
              IF (LEOF) GOTO 250

! Read point source data
!!=================================================================================
!! original code:
 !             READ (PSRCUN,*) QIDV,QXV,QYV,QZV,QHSV,QDIV,QHBV,QWBV,      &
 !                            QTEV,QTGV,QVGV,(QEMVEC(J),J=1,MJVEC)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !print *,'rpsrc: point source data: ', QIDV,QXV,QYV,QZV,QHSV,QDIV,QHBV,QWBV,      &
         !                    QTEV,QTGV,QVGV,(QEMVEC(J),J=1,MJVEC)
              READ (PSRCUN,*) QIDV,QXV,QYV,QZV,&
                              QHSV,QDIV,QHBV,QWBV,      &
                             QTEV,QTGV,QVGV,&
                              vel_point(IQ),dd_point(IQ),cir_ang(IQ),t_mpoint1(IQ),t_mpoint2(IQ), &                             
                             (QEMVEC(J),J=1,MJVEC)
   !      print *,'rpsrc 2: IQ= ',IQ, 'vel_point=',vel_point(IQ),', dd_point=',dd_point(IQ), &
   !            ',t_mpoint1=',t_mpoint1(IQ),',t_mpoint2=',t_mpoint2(IQ)

! Process point source data and insert into common variables

              call ppsrc(IQ,QIDV,QXV,QYV,QZV,QHSV,QDIV,QHBV,QWBV,      &
                        QTEV,QTGV,QVGV,MQVEC,NQVEC,QEMVEC)

          ELSE

! Here the data must be given by the AirQUIS system
 
          ENDIF

! Next point source
      
  240 CONTINUE

! Read time until which the above data are valid

      LEOF = .FALSE.
      IF (PSRCFE) THEN

          CALL NXTDAT(PSRCUN,LEOF)
          IF (LEOF) GOTO 250
!!!========================================================
!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!changed by Kang @ Dec.5, 2019 for correctly reading point source
!!!!!!!!!   Again for 1 hr emission data file, may need to 
!!!!!!         (1):  further change the date in emission file 
!!!!!!       or (2):  modify the TSPSRC() function to avoid double call cpsrc
!!!!!!!!!!!!!!!!!!!!!
! orig:          READ (PSRCUN,*) (CHTIME(I),I=1,6)
          !print *,'rpsrc: CHTIME end ', (CHTIME(I),I=1,6)
           READ (PSRCUN,*) (CHTIME(I),I=1,6)
          CHTIME(5) = 59
          CHTIME(6) = 59
!!! end of change
!!!========================================================
          
          IF ( 0 .LE. CHTIME(1) .AND. CHTIME(1) .LT. 50)      &
         CHTIME(1) = CHTIME(1) + 2000
          IF (50 .LE. CHTIME(1) .AND. CHTIME(1) .LE. 99)      &
         CHTIME(1) = CHTIME(1) + 1900

      ENDIF


  250 CONTINUE

! Reached end of file, set change time to end time

      IF (LEOF) THEN
          DO 270 I = 1,6
              CHTIME(I) = EDAT(I)
  270     CONTINUE
          CHTIME(5) = 59
          CHTIME(6) = 59
      ENDIF

! Set actual number of point sources
      
      IF (ATTIME(BDAT) .OR. CHANQ .NE. NQ) THEN
          NQ = CHANQ
          MQ = NQ
          IF (MESSFE) WRITE (MESSUN,2070) NQ
      ENDIF

! Deallocate memory

      IF (ALLOCATED(QEMVEC)) DEALLOCATE(QEMVEC)

      RETURN
      
 1000 FORMAT (A256)
      
 2000 FORMAT ('RPSRC: No point sources!')
 2010 FORMAT ('RPSRC: Maximum number of point sources allowed = ',I4)
 2020 FORMAT ('RPSRC: Too many compound columns, maximum = ',I2,'!')
 2030 FORMAT ('RPSRC: Compound column indices = ',99I3)
 2040 FORMAT ('RPSRC: Emission and temperature unit indicators = ',2I2)
 2050 FORMAT ('RPSRC: Too many point sources, maximum = ',I4,'!')
 2070 FORMAT ('RPSRC: Actual number of point sources = ',I4)
      
! End of subroutine RPSRC
      
      end subroutine rpsrc

 
