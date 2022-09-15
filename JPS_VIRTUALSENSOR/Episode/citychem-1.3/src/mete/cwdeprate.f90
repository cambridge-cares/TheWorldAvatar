! <cwdeprate.f90 - A component of the City-scale
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

      subroutine cwdeprate

!     The subroutine calculates wet deposition rates assuming that
!     wet deposition can be described by a first order ordinary
!     differential equation of the type: dc/dt = - const * c.
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
!           2016  M. Karl: Moved EXP4 into mod_util.f90
!                          Allocation of WDEPRATE2D
!
! ----------------------------------------------------------------------------------

      use mod_site
      use mod_mete
      use mod_conc
      use mod_depo

      implicit none

!     External functions
!MSK EXP4 is included in mod_util
!MSK      REAL EXP4

!     Local variables

      integer :: IC
      integer :: IX
      integer :: IY

!     IC    - Compound index
!     IX    - Grid model grid index in x-direction
!     IY    - Grid model grid index in y-direction


      real :: PRECV
      real :: CL_H
      real :: RHO_W
      real :: V_drop
      real :: A_MA_PA
      real :: E_C_EFF
      real :: C_FAC1,C_FAC2


!MSK start
      if (.not. allocated(WDEPRATE2D)) allocate (WDEPRATE2D(NC,NX,NY))
!MSK end


!     PRECV   - Precipitation value
!     CL_H    - Height of cloud base. For this version we set
!               CL_H = MOD_H, i.e. the maximum model height.
!               This is consistent with the way the wet deposition
!               is included in the vertical turbulent flux routines:
!               ADIV, V_DIF and DIFV_CN.
!
!  Note that the variables below and their values are taken from 
!  page 64 in the Unified EMEP Model Description, Emep Report 1/2003.
!
!     RHO_W   - Density of rain water (1000 kg m^-3).
!     V_drop  - Raindrop fall speed (5 m/s).
!     A_MA_PA - Empirical coefficient assuming a Marshall-Palmer size
!               distribution for the rain drops (5.2 m^3 kg^-1 s^-1). 
!     E_C_EFF - The size dependent collection efficiency of 
!               aerosols by the raindrops (dimless)
!               (= 0.1 for PM2.5 and = 0.4 for the coarse fraction, i.e.
!               for PM10 - PM2.5). We apply 0.4 for PM10 here? Should
!               probably be somewhere between 0.1 and 0.4 ?????
!     C_FAC1  - Dummy factor.
!     C_FAC2  - Dummy factor. 
!

      RHO_W   = 1000.0
      V_drop  = 5.0
      A_MA_PA = 5.2

!_LHS_08_Oct_2004_Start:
!
!_LHS  The Wet-Deposition parameterizations below are only valid
!_LHS  as long as the height of the cloud is equal to the total
!_LHS  model height. If real (2-D) cloud base values are to be
!_LHS  included the parameterisations must be improved so that
!_LHS  wet deposition only affects the layers within or below
!_LHS  the raining clouds.

      CL_H    = MOD_H
!_LHS_08_Oct_2004_End.


      C_FAC1   = RHO_W * A_MA_PA / V_drop


!     Go through all componds and compute the wet scavenging rate 
!     for particulates (Based on the EMEP Unified Model expression):

      DO IC = 1,NC


        IF(TRIM(CMPND(IC)) .EQ. 'PM10')THEN

            E_C_EFF = 0.4
            C_FAC2  = C_FAC1 * E_C_EFF
	  
        ELSEIF(TRIM(CMPND(IC)) .EQ. 'PM2.5')THEN
     
            E_C_EFF = 0.1
            C_FAC2  = C_FAC1 * E_C_EFF
		  	  
        ELSEIF(TRIM(CMPND(IC)) .EQ. 'EP')THEN
        
            E_C_EFF = 0.1
            C_FAC2  = C_FAC1 * E_C_EFF
		  	  
        ELSEIF(TRIM(CMPND(IC)) .EQ. 'TSP')THEN
        
            E_C_EFF = 0.4
            C_FAC2  = C_FAC1 * E_C_EFF

        ELSE

!           For Gaseous compounds the Scavenging ratio is given
!           as a constant for each comound (NOTE: If CL_H values
!           less than MOD_H are to be used the logic below must
!           be changed.

            C_FAC2 = WDEPSR(IC) / CL_H

        ENDIF

!       The C_FAC2 value above is now defined in such a way that 
!       the Wet-Deposition Rate, WDEPRATE2D(IC,IX,IY), emerge as 
!       the product of C_FAC2 and the precipitation, PRECV (in m/s).
!       Thus: 

!       Go through all grid squares

        DO IY = 1,NY
          DO IX = 1,NX

!         Convert precipitation from mm/h to m/s

           PRECV = PREC(IX,IY)/3.6E+6


!         Calculate wet deposition velocity (m/s):

           WDEPRATE2D(IC,IX,IY)  = C_FAC2 * PRECV

!MSK          print *,'cwdeprate: ', WDEPRATE2D(IC,IX,IY)
!       Next grid model grid square
 
          ENDDO
        ENDDO

!     Next compound.
      ENDDO

      RETURN

!     End of subroutine CWDEPRATE
      end subroutine cwdeprate
