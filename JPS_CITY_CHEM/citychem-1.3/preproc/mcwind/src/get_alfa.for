! <get_alfa.for - A component of the City-scale
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
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a simplified 
!*    EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*
!* ========================================== 
!*
! **********************************************************************
      subroutine get_alfa
! **********************************************************************
! This routine calculate the value of the 'stability'-parameter: "alfa".
! The variable "method_alfa" determines which alternative methods of
! calculation:
!
!     method_alfa = 0 ; No value is given in this routine. The alfa-
!                       values are computed so as to minimize the
!                       "error" at the observations sites.
!     method_alfa = 1 ; Mathew values applied (See: User's guide to
!                       the CG-Mathew/Adpic Models Version 5.0 (1997))
!     method_alfa = 2 ; WINDS values applied (See: WINDS documentation).
!
! Pasquill class A:  Extremely unstable.  stab_class = 1
! Pasquill class B:  Very unstable.       stab_class = 2
! Pasquill class C:  Unstable.            stab_class = 3
! Pasquill class D:  Neutral.             stab_class = 4
! Pasquill class E:  Stable.              stab_class = 5
! Pasquill class F:  Very Stable.         stab_class = 6

! Modifications:
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!***********************************************************************

      use module_mc_wind_domain
      use module_mc_wind_met
      use module_mc_wind_adjust

! *** We require the use of IMPLICIT NONE:
      implicit none

! *** Local declarations:

      real :: ds1
      real :: absmobulv

! *** Content of the routine:

        absmobulv = ABS(mobulv)

      if (method_alfa == 0 ) then

! ***   Alfa found by testing against the measurement data.
! ***   Only the stab_class and the power-law exponent are defined.

        if (mobulv < 0.0 .AND. mobulv >= -20.0) then
! ***     Extremely unstable:
          stab_class = 1
          pwrexp     = 0.18
        else if (mobulv < -20.0 .AND. mobulv >= -40.0) then
! ***     Very unstable:
          stab_class = 2
          pwrexp     = 0.2
        else if(mobulv < -40.0 .AND. mobulv >= -200.0) then
! ***     Unstable:
          stab_class = 3
          pwrexp     = 0.22
        else if (absmobulv > 200.0) then
! ***     Neutral:
          stab_class = 4
          pwrexp     = 0.28
        else if(mobulv <= 200.0 .AND. mobulv > 40.0) then
! ***     Stable:
          stab_class = 5
          pwrexp     = 0.36
        else if(mobulv <= 40.0 .AND. mobulv >= 0.0) then
! ***     Very Stable:
          stab_class = 6
          pwrexp     = 0.42
        end if

      else if (method_alfa == 1 ) then

! ***   According to the MATHEW documentation:

!_LHS_Aug2007_Start:
!        ds1 = first_layer_depth
         ds1 = sigma(1)
!_LHS_Aug2007_End.

        if (mobulv < 0.0 .AND. mobulv >= -20.0) then
! ***     Extremely unstable:
          stab_class = 1
          pwrexp     = 0.18
          alfa       = (3.2 * 2.0 * ds1)/dx
        else if (mobulv < -20.0 .AND. mobulv >= -40.0) then
! ***     Very unstable:
          stab_class = 2
          pwrexp     = 0.2
          alfa       = (2.4 * 2.0 * ds1)/dx
        else if(mobulv < -40.0 .AND. mobulv >= -200.0) then
! ***     Unstable:
          stab_class = 3
          pwrexp     = 0.22
          alfa       = (1.6 * 2.0 * ds1)/dx
        else if (absmobulv > 200.0) then
! ***     Neutral:
          stab_class = 4
          pwrexp     = 0.28
          alfa       = (1.0 * 2.0 * ds1)/dx
        else if(mobulv <= 200.0 .AND. mobulv > 40.0) then
! ***     Stable:
          stab_class = 5
          pwrexp     = 0.36
          alfa       = (0.7 * 2.0 * ds1)/dx
        else if(mobulv <= 40.0 .AND. mobulv >= 0.0) then
! ***     Very stable:
          stab_class = 6
          pwrexp     = 0.42
          alfa       = (0.5 * 2.0 * ds1)/dx
        end if

      else if (method_alfa == 2 ) then

! ***   According to the WINDS documentation:

        if (mobulv < 0.0 .AND. mobulv >= -20.0) then
! ***     Extremely unstable:
          stab_class = 1
          pwrexp     = 0.18
          alfa       = 3.16
        else if (mobulv < -20.0 .AND. mobulv >= -40.0) then
! ***     Very unstable:
          stab_class = 2
          pwrexp     = 0.2
          alfa       = 2.45
        else if(mobulv < -40.0 .AND. mobulv >= -200.0) then
! ***     Unstable:
          stab_class = 3
          pwrexp     = 0.22
          alfa       = 1.41
        else if (absmobulv > 200.0) then
! ***     Neutral:
          stab_class = 4
          pwrexp     = 0.28
          alfa       = 1.00
        else if(mobulv <= 200.0 .AND. mobulv > 40.0) then
! ***     Stable:
          stab_class = 5
          pwrexp     = 0.36
          alfa       = 0.32
        else if(mobulv <= 40.0 .AND. mobulv >= 0.0) then
! ***     Very Stable:
          stab_class = 6
          pwrexp     = 0.42
          alfa       = 0.10
        end if

      else if (method_alfa == 3 ) then

! ***   Using the STAB_CLASS given by the user for test purposes:

!_LHS_Aug2007_Start:
!       ds1 = first_layer_depth
        ds1 = sigma(1)
!_LHS_Aug2007_End.

        if (stab_class == 1) then
! ***     Extremely unstable:
!          pwrexp     = 0.18
          pwrexp     = 0.0
          alfa       = (3.2 * 2.0 * ds1)/dx
        else if (stab_class == 2) then
!         Very unstable.
!          pwrexp     = 0.2
          pwrexp     = 0.0
          alfa       = (2.4 * 2.0 * ds1)/dx
        else if(stab_class == 3) then
!         Unstable:
!          pwrexp     = 0.22
          pwrexp     = 0.0
          alfa       = (1.6 * 2.0 * ds1)/dx
        else if (stab_class == 4) then
!         Neutral:
!          pwrexp     = 0.28
          pwrexp     = 0.0
          alfa       = (1.0 * 2.0 * ds1)/dx
        else if(stab_class == 5) then
!         Stable:
!          pwrexp     = 0.36
          pwrexp     = 0.0
          alfa       = (0.7 * 2.0 * ds1)/dx
        else if(stab_class == 6) then
!         Very stable:
!          pwrexp     = 0.42
          pwrexp     = 0.0
          alfa       = (0.5 * 2.0 * ds1)/dx
        else
          print *,' An invalid stability class has been used.'
        end if

      else if (method_alfa == 4 ) then

         alfa = constant_alfa

!      TEST where we apply a constant alfa-value:

        if (mobulv < 0.0 .AND. mobulv >= -20.0) then
!         Extremely unstable:
          stab_class = 1
          pwrexp     = 0.18
        else if (mobulv < -20.0 .AND. mobulv >= -40.0) then
!         Very unstable:
          stab_class = 2
          pwrexp     = 0.2
        else if(mobulv < -40.0 .AND. mobulv >= -200.0) then
!         Unstable:
          stab_class = 3
          pwrexp     = 0.22
        else if (absmobulv > 200.0) then
!         Neutral:
          stab_class = 4
          pwrexp     = 0.28
        else if(mobulv <= 200.0 .AND. mobulv > 40.0) then
!         Stable:
          stab_class = 5
          pwrexp     = 0.36
        else if(mobulv <= 40.0 .AND. mobulv >= 0.0) then
!         Very Stable:
          stab_class = 6
          pwrexp     = 0.42
        end if

      end if

      return
      end subroutine get_alfa
