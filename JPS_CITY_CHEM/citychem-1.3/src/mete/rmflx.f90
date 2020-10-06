! <rmflx.f90 - A component of the City-scale
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

      subroutine RMFLX

!       2016 Matthias Karl, HZG, This is an original CityChem subroutine
!       The subroutine reads mixing height from file. 
!       It is written in the same style as other EPISODE reading routines 
!       for meteorological variables.
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


            implicit none

! Local variables
      character(len=10) TEXT1
      character(len=10) TEXT2

      integer IX,IY

      REAL MAX_MFLX

! TEXT1,2 - Textstrings

      IF (MFLXFE) THEN

! Read mixing height from file

          CALL R2DFLD(MFLXUN,MFLXFM,TEXT1,TEXT2,NX,NY,MX,MY,MFLX)

      ELSE

! Main data file with file value exists?

          IF (MAINFE) THEN

! Set homogeneous mixing height

              CALL H2DFLD(MFLXFV,NX,NY,MX,MY,MFLX)

          ENDIF

      ENDIF

! Checking data

      MAX_MFLX = 0.0
      DO 100 IY = 1,NY
      DO 110 IX = 1,NX

            IF(IX == 1 .AND. IY ==1)THEN
              IF (MESSFE) WRITE (MESSUN,2030) IX,IY,MFLX(IX,IY)
            ENDIF

! Negative data not equal to missing data is not tolerated

          IF (ABS(MFLX(IX,IY)) .GT. ABS(MAX_MFLX))    &
              MAX_MFLX = MFLX(IX,IY)

  110 CONTINUE
  100 CONTINUE

! Write message

      IF (MFLXFE) THEN
          IF (MESSFE) WRITE (MESSUN,2000) &
                 MOD(YEAR,100),MNTH,DAYM,HOUR
      ELSE
          IF (MFLXFE) THEN
              IF (MESSFE) WRITE (MESSUN,2010) &
                     MOD(YEAR,100),MNTH,DAYM,HOUR,MFLX(1,1)
          ENDIF
      ENDIF

      RETURN

 2000 format ('RMFLX: Read surface moisture flux from file ... for time ', &
         4I2.2)

 2010 format ('RMFLX: Homogeneous moisture flux ...... for time ', &
         4I2.2,' = ',F10.3,' m')

 2011 format ('RMFLX: Zero moisture flux read for time ', &
         4I2.2)


 2030 format ('RMFLX: Moisture Flux MFLX(',I3,',',I3,') = ', &
         F10.3,' kg/m2/s')

! End of subroutine RMFLX

      end subroutine rmflx
