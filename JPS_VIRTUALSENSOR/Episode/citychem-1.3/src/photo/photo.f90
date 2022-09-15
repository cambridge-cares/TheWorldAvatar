! <photo.f90 - A component of the City-scale
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
!* ========================================== 
!*
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*****************************************************************************!

      subroutine photo(LATIV,LTOGV,YEARV,MNTHV,DAYMV,DAYYV,HOURV,MINUV,SECOV,CLOUV,TAIRV,CV)

!       2016 Matthias Karl, HZG, CityChem extension:
!       Substantial modification to calculate sun height above the horizon (RSH)
!       The subroutine calculates NO2, NO and O3 based on photochemical equilibrium.
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

!MSK
      use mod_util

      implicit none

      integer :: YEARV
      integer :: MNTHV
      integer :: DAYMV
      integer :: HOURV
!MSK start
      integer :: MINUV
      integer :: SECOV
      integer :: LTOGV
      integer :: DAYYV
!MSK end

      real    :: LATIV
      real    :: CLOUV
      real    :: TAIRV
      real    :: CV(3)

! Functions

!MSK start
!MSK abandoned use of external functions
!      REAL EXP4      !(is in mod_util)
!      REAL SUNH      !(is in "time_episode3.3"
!MSK end


!MSK start
      real    :: jhour
      real    :: PHI
      real    :: SUNHR
      real    :: RSH
      real    :: COSZEN
      real    :: ZENANG
!MSK end

! Local variables

      double precision :: AVOG
      double precision :: MW_NO2
      double precision :: MW_O3
!MSK      REAL PI     !(is in mod_util)
!MSK      REAL RAD    !(is in mod_util)
!MSK start
      double precision :: MW_NO
      double precision :: DECANG
!MSK end

      double precision :: AVAL
      double precision :: BVAL
      double precision :: CVAL
      double precision :: NOV
      double precision :: NOXV
      double precision :: NO2V
      double precision :: OXXV
      double precision :: O3V
      double precision :: RK1
      double precision :: RK2



! Initialization

      AVOG   = 6.023D+23
      MW_NO2 = 46.D0
      MW_O3  = 48.D0
!MSK defined in mod_util
!MSK      PI = 3.1415927
!MSK      RAD = PI/180.
!MSK start
      MW_NO  = 30.D0 
!MSK end

! If missing data then do nothing

      IF (CV(1) .LT. 0. .OR. CV(2) .LT. 0. .OR.         &
         CV(3) .LT. 0.) RETURN

! Convert from ug/m3 to molec/cm3

      NO2V = DBLE(CV(1))*((AVOG*1.D-12)/MW_NO2)
!MSK      NOV  = DBLE(CV(2))*((AVOG*1.D-12)/MW_NO2)
      NOV  = DBLE(CV(2))*((AVOG*1.D-12)/MW_NO)
      O3V  = DBLE(CV(3))*((AVOG*1.D-12)/MW_O3 )

! Define NOx and Ox levels (molec/cm3)

      NOXV = NO2V + NOV
      OXXV = NO2V + O3V

! Calculate photochemical coefficients
      !Bruce fixed mistake in RK1, was '1.4-12' which gave RK1=1.4
 
      !print *,'photo T ',TAIRV     
!MSK      RK1 = DBLE(1.4D-12*EXP4(-1310./TAIRV))
      RK1 = DBLE(1.4E-12*EXP4(-1310./ TAIRV ))

!MSK start ********************************************************
! *** Adopted from SUNH.FOR in BDE_episode time_episode3.3
! This had been lost in the beginning

!MSK Calculate local sun hour
      SUNHR = HOURV + (60.*MINUV + SECOV)/3600.

!MSK   Sun time correction for local time
! *** Convert to GMT. Assumes that SunHr = [1,24] !!!!

      jhour = SUNHR + LTOGV

      if (jhour < 1) then
        jhour = 24 + jhour
      elseif (jhour > 24) then
        jhour = jhour - 24
      endif

        
!MSK Calculate sun declination
! *** DAYYV   - Day of year
      DECANG = 23.27*COS(2.0*PI*(DAYYV - 172.0)/365.0)

! *** Negate effective latitude and declination angle if south
! *** of the equator. This turns summer into winter and winter
! *** into summer for sites south of the equator.

      PHI = LATIV

      IF (PHI .LT. 0.) THEN
        DECANG = -DECANG
        PHI    = -PHI
      ENDIF

!MSK Calculate the zenith angle

      COSZEN = COS(RAD*PHI)*COS(RAD*DECANG)  *      &
               COS(2.0*PI*(jhour - 12.0)/24.0) +      &
               SIN(RAD*PHI)*SIN(RAD*DECANG)
      ZENANG = ACOS(MAX(0.017,COSZEN))/RAD

!MSK Calculate sun height above the horizon

      RSH = 90. - ZENANG
      
!MSK end ***********************************************

      IF (RSH .GT. 0.D0) THEN

!MSK We assume here that some diffusive light is getting through
!MSK even if cloud cover =1

          RK2 = DBLE(1.45E-2*(1. - 0.5*CLOUV)*      &   
                  EXP4(-0.40/SIN(RSH*RAD)))
!TEST 14.05.2018: EXP4(-0.20/SIN(RSH*RAD)))  

      ELSE

        RK2 = 0.D0

      ENDIF

!MSK debug      print *,'photo RK', SUNH,RSH,CLOUV,SIN(RSH*RAD),RK1,RK2

! Calculate coefficients

      AVAL = -RK1
      BVAL = RK1*(OXXV - NOXV) - RK2
      CVAL = RK2*OXXV

! Calculate O3 and NO2 values (molec/cm3)

       O3V = (-BVAL - SQRT(BVAL*BVAL - 4.D0*AVAL*CVAL))/(2.D0*AVAL)
      NO2V = OXXV - O3V
       NOV = NOXV - NO2V

! Slight negative values can occur, they are set to 0

       O3V = MAX( O3V,0.D0)
      NO2V = MAX(NO2V,0.D0)
       NOV = MAX( NOV,0.D0)

! Convert from molec/cm3 to ug/m3

      CV(1) = REAL(NO2V*(MW_NO2/(AVOG*1.D-12)))
!MSK wrong MW     CV(2) = REAL( NOV*(MW_NO2/(AVOG*1.D-12)))
      CV(2) = REAL( NOV*(MW_NO/(AVOG*1.D-12)))
      CV(3) = REAL( O3V*(MW_O3 /(AVOG*1.D-12)))
!MSK debug      print *,'photo: NO2 ',NO2V,CV(1)
!MSK debug      print *,'photo: NOV ',NOV,CV(2)
!MSK debug      print *,'photo: O3V ',O3V,CV(3)

      RETURN

! End of subroutine PHOTO

      end subroutine photo
