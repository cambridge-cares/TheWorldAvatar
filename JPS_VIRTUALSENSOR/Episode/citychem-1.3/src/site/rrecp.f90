! <rrecp.f90 - A component of the City-scale
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

      subroutine RRECP

! The subroutine reads receptor points coordinates from file (km)
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
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site

        implicit none

! Local variables

      REAL XRV,YRV,ZRV,RMINRV
      INTEGER I,IR
      LOGICAL LEOF
      CHARACTER(len=256) TXTRV
      CHARACTER(len=256) TXTSTR
      CHARACTER(len=256) SIMID

! XRV    - Receptor point x-coordinate
! YRV    - Receptor point y-coordinate
! ZRV    - Receptor point z-coordinate
! RMINRV - Receptor point maximum distance
! I      - Textstring index
! IR     - Receptor point index
! LEOF   - If end of file then true else false
! TXTRV  - Receptor point textstring

! Open file

      call OPIFIL(RECPFN,RECPUN,RECPFE,RECPFM)

      IF (RECPFE) THEN
          call GETDAT(RECPUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) SIMID
          ENDIF
          print *,'receptor point file sim-id: ', SIMID
      ENDIF

! File exists?

      IF (RECPFE) THEN

! Yes!

! Read all receptor points

          IR = 0

  100     CONTINUE
          call NXTDAT(RECPUN,LEOF)
          IF (LEOF) GOTO 199

          READ (RECPUN,*,END=199) SIMID,XRV,YRV,ZRV,RMINRV,TXTRV

          IR = IR + 1

! Read next receptor point

          GOTO 100

  199     CONTINUE

! Found number of receptor points

          NR = IR

          IF (.NOT. ALLOCATED(RMINR)) ALLOCATE(RMINR(NR))
          IF (.NOT. ALLOCATED(XR))    ALLOCATE(XR(NR))
          IF (.NOT. ALLOCATED(YR))    ALLOCATE(YR(NR))
          IF (.NOT. ALLOCATED(ZR))    ALLOCATE(ZR(NR))
          IF (.NOT. ALLOCATED(TXTR))  ALLOCATE(TXTR(NR))

          MR = NR

! Rewind receptor file

          REWIND(RECPUN)
          call GETDAT(RECPUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) SIMID
          ENDIF
          print *,'receptor point file sim-id: ', SIMID

! Read all receptor points

          IR = 0

  200     CONTINUE

          call NXTDAT(RECPUN,LEOF)
          IF (LEOF) GOTO 999

          READ (RECPUN,*,END=999) SIMID,XRV,YRV,ZRV,RMINRV,TXTRV

          IR = IR + 1

          XR(IR) = XRV
          YR(IR) = YRV
          ZR(IR) = ZRV
          RMINR(IR) = RMINRV
          TXTR(IR)  = TXTRV

          IF (MESSFE) WRITE (MESSUN,2010) IR,XR(IR),YR(IR),ZR(IR),   &
                     RMINR(IR),(TXTR(IR)(I:I),I=1,10)

! Read next receptor point

          GOTO 200

  999     CONTINUE

      ELSE

! No!

! Main file exists?

          IF (MAINFE) THEN

! No receptor points

              NR = 0
              CALL STOPIT('You must provide a receptor file with at least one station point')

          ENDIF

      ENDIF

! Close file

      call CLIFIL(RECPFN,RECPUN,RECPFE,RECPFM)

      IF (MESSFE) THEN
          WRITE (MESSUN,2020) NR
      ENDIF


      RETURN

 1000 format(A256)

 2000 format('RRECP: Too many receptor points, max. = ',I4)
 2010 format('RRECP: Receptor nr. ',I6,': ',' X = ',F12.3,    &
            ' Y = ',F12.3,' Z = ',F10.3,' RMAX = ',F10.3,     &
            ' NAME = ',256A1)
 2020 format('RRECP: Number of receptor points: ',I6)

! End of subroutine RRECP
      end subroutine rrecp
