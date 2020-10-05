! <psradi.f90 - A component of the City-scale
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

      subroutine PSRADI

! The subroutine calculates plume segments radioactive decays.
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
!           2016  M. Karl: commented the radioactive explosion/leakage part
!           2017  M. Karl: PNC coagulation term ("reduction")
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_psrc
!MSK start
      use mod_grid
!MSK end

      implicit none

! External functions

!MSK      REAL EXP4   !(is in mod_util)

! EXP4 - Exponent function

! Local variables

      real :: LAMBV
      real :: MASSV
      real :: QEMIV
      real :: TTOT

      integer :: IC
      integer :: IP
!MSK start
      real    :: K_COAG
      real    :: C0
      integer :: IXP, IYP, IZP
      integer :: ICC
!MSK end


! LAMBV - Radioactive decay coefficient
! MASSV - Plume segment mass
! QEMIV - Plume segment emission
! TTOT  - Total simulation time
! IC    - Compound index
! IP    - Plume segment index

! Calculate total elapsed simulation time (s)

      TTOT = THOUR*3600. + ITS*DT

!MSK start
      IF (NP .GT. 0) THEN
!MSK end

! Go through all compounds

        DO 100 IC = 1,NC

! If radioactive half-time is nonpositive then its missing

           IF (THALF(IC) .LE. 0) THALF(IC) = MISS

! If radioactive half-time is missing then goto next compound

           IF (THALF(IC) .EQ. MISS) GOTO 100

! Calculate radioactive decay coefficient

           LAMBV = (-LOG(2.)/THALF(IC))

! Go through all plume segments

           DO 110 IP = 1,NP

! Old plume segment emission and mass

               QEMIV = PU( 9 + 2*IC,IP)
               MASSV = PU(10 + 2*IC,IP)

!MSK start
!MSK commented the radioactive explosion/leakage part as not relevant
!MSK ! If nuclear disaster/explosion then all radioactive compounds is
!MSK ! assumed to be generated at start of simulation
!MSK
!MSK              IF (PU(5,IP) .EQ. 0.) THEN
!MSK
!MSK ! Reduce plume segment emission and mass from start of simulation
!MSK
!MSK                  QEMIV = QEMIV*EXP4(-LAMBV*TTOT)
!MSK                  MASSV = MASSV*EXP4(-LAMBV*TTOT)
!MSK
!MSK              ENDIF
!MSK
!MSK ! If nuclear leakage then all radioactive compounds is assumed to
!MSK ! be generated at time of emission
!MSK
!MSK              IF (PU(5,IP) .EQ. 0.) THEN
!MSK
!MSK ! Plume segment emission and mass unchanged
!MSK
!MSK                  QEMIV = QEMIV
!MSK                  MASSV = MASSV
!MSK
!MSK              ENDIF
!MSK

! Reduce plume segment emission and mass for one timestep

!MSK start
              MASSV = MAX(0.,MASSV)
              if (MASSV .lt. 1.e-10) MASSV=1.e-10


!MSK here comes in the PNC coagulation term ("reduction")
!MSK Special Treatment for all pnc types:
              if ( CMPND(IC)(1:3) .eq. 'pnc' ) then

! ***  first get grid cell [ix,iy,iz] of plume
! Calculate plume segment main gridcell
                CALL GETMGI(1,PU(1,IP),PU(2,IP),PU(7,IP),IXP,IYP,IZP)
! ***  Calculate non-linear pnc coagulation:
!      consider the concentration of all PNC components
! ***  i.e. C0 = PNC1 + PNC2 + PNC3
!
                C0 = 0.0
                do 200 ICC=1,NC

                  if ( CMPND(IC)(1:3) .eq. 'pnc' ) then
                    C0 = C0 + real(C(ICC,IXP+1,IYP+1,IZP)) 
                  endif

  200           continue

                !print *,'psradi: C0', IC,C0 
                if (C0.lt.1.e-10)   C0=1.e-10

! if C0 is zero, there will be no coagulation loss
                K_COAG=THALF(IC)*C0
       ! print *,'psradi1: KC QEMI', IP, IC, K_COAG, QEMIV

                QEMIV = QEMIV*EXP4(-DT*K_COAG)
                MASSV = MASSV*EXP4(-DT*K_COAG)
       ! print *,'psradi2: KC QEMI', IP, IC, K_COAG, QEMIV
                        
              else    ! original default for gases
                QEMIV = QEMIV*EXP4(-LAMBV*DT)
                MASSV = MASSV*EXP4(-LAMBV*DT)
              endif
!MSK end              


! New plume segment emission and mass

               PU( 9 + 2*IC,IP) = QEMIV
               PU(10 + 2*IC,IP) = MASSV

! Next plume segment

  110      CONTINUE

! Next compound

  100   CONTINUE

      ENDIF    ! end plume segments' decay

      RETURN

! End of subroutine PSRADI

      end subroutine psradi
