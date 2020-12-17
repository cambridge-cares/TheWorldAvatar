! <wconcr.f90 - A component of the City-scale
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

      subroutine wconcr

! The subroutine writes receptor concentrations to file.
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

      USE mod_util
      USE mod_main
      USE mod_site
      USE mod_time
      USE mod_conc

! Local variables

      INTEGER ic,ir,NR_UT
      CHARACTER(len=10) TXT1,TXT2

! IC   - Index of compound
! IR   - Index of receptor point
! TXT1 - Textstring
! TXT2 - Textstring

      if (messfe) THEN
        if (nr >= 23)then
          NR_UT = 23
        else
          NR_UT = nr
        endif
        write (messun,*)
        write (messun,2100) MOD(YEAR,100),MNTH,DAYM,HOUR
        do ic = 1,nc
!          do ir = 1,nr
           do ir = 1,NR_UT
            WRITE(messun,'(A,I3,A,I3,A,F13.5)')   &
                        'WCONCR: IC = ',ic,    &
                        ' IR = ',ir,    &
                        ' CONCR(IC,IR) = ',cr(ic,ir) 
          enddo
        enddo
        write (messun,*)
      endif

! If no receptor concentration file then return

      if (RCONFN(1:1) .EQ. ' ') then

        
        RETURN
        
      endif

      IF (RCONFM .NE. 2) THEN

! Write receptor concentrations in ASCII mode

! Go through all compounds

      DO 100 IC = 1,NC

! If not concentration output for this compound goto next compound

          IF (COUT(IC) .EQ. 0) GOTO 100

! Write heading

!MSK          IF (NR .GE. 1) WRITE (RCONUN,2110)
          IF (NR .GE. 1) WRITE (RCONUN,2130) CUNIT(IC)

! Go through all receptor points

          DO 110 IR = 1,NR

! Write receptor concentration

!_CITYDELTA_START
!_LHS_SOA_June_2007_Start:
           

!              WRITE (RCONUN,2120) MOD(YEAR,100),MNTH,DAYM,HOUR,
!     .                            ' IC = ',IC,' IR = ',IR,
!     .                            CR(IC,IR)

            IF (CUNIT(IC)(1:3) .EQ. 'mol')THEN
!             IF (IC <= (NC - n_nochem))THEN
!             Convert from [molecules/cm3] to [ug/m3]:  
              WRITE (RCONUN,2120) MOD(YEAR,100),MNTH,DAYM,HOUR,  &
                                 ' IC = ',IC,' IR = ',IR,  &
                                 CR(IC,IR)*CMOLW(IC)/avogad*1.0E+12

            ELSE
!             The aerosols are advected and diffused as [ug/m3] and
!             need not be converted:
              WRITE (RCONUN,2120) MOD(YEAR,100),MNTH,DAYM,HOUR,  &
                                 ' IC = ',IC,' IR = ',IR,       &
                                 CR(IC,IR)             
            ENDIF
!_LHS_SOA_June_2007_End.
!_CITYDELTA_END

! Next receptor point

  110     CONTINUE

! Next compound

  100 CONTINUE

      ELSE

! Writing receptor concentrations in binary mode

      ENDIF

      RETURN

 2100 FORMAT ('WCONCR: Receptor  concentrations  for hour ',  &
             4I2.2)
!MSK 2110 FORMAT ('* Concentration ug/m3')
! 2120 FORMAT (4I3,A6,I3,A6,I4,F15.6)
 2120 FORMAT (4I3,A6,I3,A6,I4,F13.4)
 2130 FORMAT (A10)

! End of subroutine WCONCR

      end subroutine wconcr
