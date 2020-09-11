! <rasrc.f90 - A component of the City-scale
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

      subroutine rasrc

! The subroutine reads area sources data from file or homogeneous
! values.
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
      use mod_time
      use mod_conc
      use mod_asrc
      use mod_phot

      implicit none
      
! Local variables
      integer           :: IA,IC,IX,IY,J
      character(LEN=10) :: TEXT1, TEXT2

! SCALE           - Scale factor
! sum_traf_em(nc) - Summing up the traffic emissions.
! sum_heat_em(nc) - Summing up the emissions from house heting.
! IA              - Index of area source
! IC              - Index of compound
! IX              - Main grid index in x-direction
! IY              - Main grid index in y-direction
! J               - Index
! TEXT1,2         - Textstrings


! Go through all area sources and compounds

      DO 100 IA = 1,NA
      DO 110 IC = 1,NC

! Check if file exists

          !print *,'rasrc ',IC,IA,cmpnd(ic)
          IF (ASRCFE(IC,IA)) THEN

           !print *,'rasrc exist ',IC,IA,cmpnd(ic)
! Read area sources data from file

            CALL R4DFLD(ASRCUN(IC,IA),ASRCFM(IC,IA),IC,IA,TEXT1,TEXT2,  &
                     NC,NX,NY,NA,MC,MX,MY,MA,QAORIG)

          ELSE

! No!

! Main data file with file value exists?

!MSK          IF (MAINFE .AND. ASRCFV(IC,IA) .NE. MISS) THEN
            IF (MAINFE) THEN

! Setting homogeneous area source field

              CALL H4DFLD(ASRCFV(IC,IA),IC,IA,NC,NX,NY,NA,MC,MX,MY,MA,  &
                     QAORIG)

            ENDIF

          ENDIF

! Checking data

          DO 130 IY = 1,NY
          DO 140 IX = 1,NX

! Missing data is set equal to zero

              IF (QAORIG(IC,IX,IY,IA) .EQ. MISS)  &
               QAORIG(IC,IX,IY,IA) = 0.

! Negative data is not tolerated

              IF (QAORIG(IC,IX,IY,IA) .LT. 0.) THEN
                  IF (MESSFE) WRITE (MESSUN,2020)  &
                            IX,IY,QAORIG(IC,IX,IY,IA),IA,CMPND(IC)
                  CALL STOPIT('RASRC: Negative area source emission!')
              ENDIF

  140     CONTINUE
  130     CONTINUE

! Write message
          !print *,'rasrc: IA IC', IA, IC
          !print *,'rasrc: CMPND(IC)', CMPND(IC)
          IF (ASRCFE(IC,IA)) THEN
             IF (MESSFE) THEN

                 WRITE (MESSUN,2000) MOD(YEAR,100),MNTH,DAYM,HOUR
                 WRITE (MESSUN,2005) IA,CMPND(IC)

             ENDIF

          ENDIF

! Next compound or area source

  110 CONTINUE
  100 CONTINUE

      RETURN

 2000 format ('RASRC: Read area src. emis. from file   for time ',  4I2.2)
 2005 format (' and src nr. ',I2,' and comp. ',A10)

 2010 format ('RASRC: Homogeneous area src. emis. .... for time ',  &
             4I2.2,' = ',E8.1,' for src nr. ',I2,' and comp. ',A10)
 2020 format ('RASRC: Area source QAORIG(',I3,',',I3,') = ',    &
             E10.1,' g/s',                                     &
             ' for src. nr.',I2,' and comp. ',A10,' is negative!')
 2030 format ('RASRC: Area source scaling factor = ',           &
             E10.1,                                            &
             ' for src. nr.',I2,' and comp. ',A10,' is negative!')

! End of subroutine RASRC

      end subroutine rasrc
