! <cphotl.f90 - A component of the City-scale
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

      subroutine CPHOTL(ICV)

! The subroutine calculates photochemistry for all line source
! associated receptor points.
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
!           2016  M. Karl: modified setting of cloud cover.
!                          Lower concentration cut off for very small values.
!                          Modified call to photo
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_phot
      use mod_lsrc

      implicit none

      integer :: ICV

! ICV - Update concentration indicator

! Local variables

      real    :: CLOUV
      real    :: CV(3)
      real    :: LATIV
      real    :: TAIRV
      real    :: QLRDV
      real    :: XRV
      real    :: YRV
      real    :: ZRV
      integer :: DAYMV
      integer :: HOURV
      integer :: IRL
      integer :: IX
      integer :: IY
      integer :: IZ
      integer :: MNTHV
      integer :: YEARV

!MSK start
      integer :: ic
      integer :: SECOV
      integer :: MINUV
      real    :: SunHr
      integer :: LTOGV
      integer :: DAYYV
!MSK end

! CLOUV - Cloud cover value
! CV    - Receptor concentration values
! LATIV - Latitude value
! TAIRV - Air temperature value
! QLRDV - Line source to receptor point distance 
! XRV   - Receptor point x-coordinate
! YRV   - Receptor point y-coordinate
! ZRV   - Receptor point z-coordinate
! DAYMV - Day value
! HOURV - Hour value
! IRL   - Line source associated receptor point index
! IX    - Main grid index in x-direction
! IY    - Main grid index in y-direction
! IZ    - Main grid index in z-direction
! MNTHV - Month value
! YEARV - Year value

! Check photochemistry indicator


!MSK      IF (PHOTPS .NE. 1) RETURN

! Initialize latitude and date/time

      LATIV = SITELA

      YEARV = YEAR
      MNTHV = MNTH
      DAYMV = DAYM
      HOURV = HOUR

!MSK start
      DAYYV = DAYY
      MINUV = MINU
      SECOV = SECO
      LTOGV = LTOGMT
!MSK end

! Go through all line source associated receptor points

      DO 100 IRL = 1,NRL

! Get distance from line source to receptor point

          QLRDV = QLRD(IRL)

! If distance is nonpositive then goto next point

          IF (QLRDV .LE. 0.) GOTO 100

! Define receptor point coordinates

          XRV = XRL(IRL)
          YRV = YRL(IRL)
          ZRV = ZRL(IRL)

! Get main grid indices

          CALL GETMGI(1,XRV,YRV,ZRV,IX,IY,IZ)

! Set cloud cover

!MSK start
          IF (TSRADFE) THEN
             if (TSRAD(IX,IY).eq.0.0) then
                ! Nighttime cloud cover not relevant
                CLOUV = 1.0
             else
                SunHr = HOURV + (60.*MINUV + SECOV)/3600.
                CLOUV = cloudfraction(SunHr,DAYMV,LATIV,TSRAD(IX,IY))
             endif
          ELSE
             CLOUV = CLOU(IX,IY)
          ENDIF
!MSK end


! Set air temperature

          TAIRV = TAIR(IX,IY) + CTOK

!MSK start
! Concentration cut off for very small
          do IC=1,NC
             if ( CRL(IC,IRL) .lt. 1.e-20 )   CRL(IC,IRL) = 0.0
          enddo
!MSK end

!MSK start 
!MSK: for receptor points and lines only photochemical balance
!
! Mapping photochemistry indices to compound indices

! PHOTPS = 1 :
! 1 = NO2
! 2 = NO
! 3 = O3

! Copy NO2, NO and O3 concentrations (ug/m3)

          do IC=1,NC
            if(trim(CMPND(IC)) == 'NO2')   CV( 1 )  = CRL(IC,IRL)
            if(trim(CMPND(IC)) == 'NO')    CV( 2 )  = CRL(IC,IRL)
            if(trim(CMPND(IC)) == 'O3')    CV( 3 )  = CRL(IC,IRL)
          enddo
!MSK end


! Calculate photochemical balance between NO2, NO and O3

!MSK          CALL PHOTO(LATIV,YEARV,MNTHV,DAYMV,HOURV,CLOUV,TAIRV,CV)
            call photo(LATIV,LTOGV,YEARV,MNTHV,DAYMV,DAYYV,HOURV,MINUV,SECOV,CLOUV,TAIRV,CV)

! Copy NO2, NO and O3 concentrations back (ug/m3)
!MSK start

          do IC=1,NC
            if(trim(CMPND(IC)) == 'NO2')   CRL(IC,IRL) = CV( 1 )
            if(trim(CMPND(IC)) == 'NO')    CRL(IC,IRL) = CV( 2 )
            if(trim(CMPND(IC)) == 'O3')    CRL(IC,IRL) = CV( 3 )
          enddo

!MSK end


! Next line source associated receptor point

  100 CONTINUE


      RETURN

! End of subroutine CPHOTL

      end subroutine cphotl
