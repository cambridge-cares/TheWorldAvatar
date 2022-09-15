! <roldp.f90 - A component of the City-scale
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

      subroutine ROLDP

! The subroutine reads old plume segments data from file.
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
      use mod_mete
      use mod_conc
      use mod_psrc

      implicit none

! Local variables

      character(len=256) TXTSTR
      integer :: I
      integer :: IDAT(4)
      integer :: J
      logical :: LEOF

! TXTSTR - Textstring
! I      - Plume segment data index
! IDAT   - Input time
! J      - Plume segment data index
! LEOF   - If end of file then true else false

! Open file

      call opifil(OLDPFN,OLDPUN,OLDPFE,OLDPFM)

! Check if file exists

      IF (OLDPFE) THEN

! Read number of plume segments and timestamp

         call nxtdat(OLDPUN,LEOF)
         READ (OLDPUN,*) NP,(IDAT(I),I=1,4)

         !print *,'roldp: NP IDAT', NP, IDAT(1),IDAT(2),IDAT(3),IDAT(4)

         IF (.NOT. ALLOCATED(PU)) ALLOCATE(PU(10+2*NC,NP))
         MP = NP

         do j=1, (10 + 2*NC)
           do i=1,NP
             PU(j,i) = 0.0
           enddo
         enddo

! Read all old plume segments data


         DO 100 I = 1,NP
!!===================================================
!! changed by Kang @ 2019.11 @ CARES
!! orig:
           !   call getdat(OLDPUN,TXTSTR,LEOF)
!MSK problem  READ (TXTSTR,1000) (PU(J,I),J=1,10 + 2*NC)  
            !  READ (TXTSTR,*) (PU(J,I),J=1,10 + 2*NC)
!!            
             ! if (mod(I-1,5) .eq. 0) go to 100
          call nxtdat(OLDPUN,LEOF)
              read (OLDPUN,*)(PU(J,I),J=1,10 + 2*NC)
             ! print *,'roldp: PU', (PU(J,I),J=1,10 + 2*NC)
!!! end of change. 

! Next plume segment

  100     CONTINUE


          IF (MESSFE) WRITE (MESSUN,2000) NP,MOD(YEAR,100),MNTH,      &
                                         DAYM,HOUR

      ELSE

          IF (MAINFE) THEN

! If no old plume segment file then no plume segments

             NP = 0
             IF (MESSFE) WRITE (MESSUN,2010)

          ELSE

          ENDIF

      ENDIF

! Close file

      call clifil(OLDPFN,OLDPUN,OLDPFE,OLDPFM)


      RETURN

!MSK 1000 FORMAT (7F12.1,F7.1,F7.0,F7.0,99(F12.2,F12.2))

 2000 format ('ROLDP: Read ',I5,' old plume segments for time ',      &
            4I2.2)
 2010 format ('ROLDP: Old plume segments missing!')

! End of subroutine ROLDP

      end subroutine roldp
