! <cphotr10.f90 - A component of the City-scale
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

      subroutine CPHOTR10(ICV)

!       2016 Matthias Karl, HZG, This is an original CityChem subroutine
!       The subroutine calculates photochemistry for all irregular
!       receptor points. Uses the EMEP10 photochemistry scheme
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    22 Oct 2018  M. Karl: L293++      GenChem compound indices of chemspecs10.inc
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


      implicit none

      integer :: ICV

! ICV - Update concentration indicator

! *** Define 10 chemical species for EMEP10
      integer,parameter :: MSPEC = 10

! Local variables
      double precision  :: CV(MSPEC)
      real              :: DDEP(MSPEC)
      real              :: WDEP(MSPEC)
      real              :: DDEPVV(MSPEC)
      real              :: WDEPSRV(MSPEC)

      real    :: XRV
      real    :: YRV
      real    :: ZRV
      integer :: IR
      integer :: IX
      integer :: IY
      integer :: IZ
      integer :: DAYMV
      integer :: HOURV
      integer :: MNTHV
      integer :: YEARV
      integer :: SECOV
      integer :: MINUV
      integer :: DAYYV
      integer :: IDRY
      integer :: ILAND
      integer :: ic, jc

! CLOUV - Cloud cover value
! CV    - Receptor concentration values
! LATIV - Latitude value
! TAIRV - Air temperature value
! XRV   - Receptor point x-coordinate
! YRV   - Receptor point y-coordinate
! ZRV   - Receptor point z-coordinate
! DAYMV - Day value
! HOURV - Hour value
! IR    - Receptor point index
! IX    - Main grid index in x-direction
! IY    - Main grid index in y-direction
! IZ    - Main grid index in z-direction
! MNTHV - Month value
! YEARV - Year value

      real SunHr
      real LATIV

      real :: DTP
      real :: SITELAV
      real :: SITELOV
      real :: CLOUV
      real :: TAIRV
      real :: DTDZV
      real :: RHUMV
      real :: SHUMV
      real :: PRESV
      real :: PRECV
      real :: MOBULV(0:1)
      real :: USTARV(0:1)
      real :: HINVD
      real :: HINVW

! *** Set site latitude and longitude:

      SITELAV = SITELA
      SITELOV = SITELO
      LATIV   = SITELA

! Time and date
      YEARV = YEAR
      MNTHV = MNTH
      DAYMV = DAYM
      HOURV = HOUR
      MINUV = MINU
      SECOV = SECO
      DAYYV = DAYY

! *** Set photochemistry time step (s):
      DTP = DT


! Go through all receptor points

      DO 100 IR = 1,NR

! Define receptor point coordinates

          XRV = XR(IR)
          YRV = YR(IR)
          ZRV = ZR(IR)

! Get main grid indices

          call getmgi(1,XRV,YRV,ZRV,IX,IY,IZ)


! Set cloud cover

!MSK start
! Estimation of cloud cover:
! If total solar radiation is available, then calculate
! else take from cloud cover file (constant value)

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

! *** Set air temperature

          TAIRV = TAIR(IX,IY) + CTOK

! *** Set air temperature gradient (K/m): Hva benyttes "DTDZV" til??
          DTDZV = DTDZ(IX,IY)

! *** Set Pressure value (mb):
!MSK            PRESV = pres(IX,IY,IZ)
!MSK no meteo input for 3D pressure field
!MSK use standard pressure at ground
!MSK to be revised later
          PRESV = 1013.25

          RHUMV = RHUM(IX,IY)
            
! *** Set precipitation (mm/h):
          PRECV = PREC(IX,IY)

! *** Set Monin-Obukhov lengths (identical?):
          MOBULV(0) = MOBUL(IX,IY)
          MOBULV(1) = MOBUL(IX,IY)

! *** Set friction velocities (m/s):
          USTARV(0) = USTAR(IX,IY)
          USTARV(1) = USTAR(IX,IY)

! *** Set dry deposition indicator (GRIDDDS).
! *** No Dry deposition applied.
          IDRY = 0

! *** Set inverse heights for dry and wet deposition (1/m):
          HINVD = 1./DZ(1)
          HINVW = 1./DZ(1)

! *** Set land/sea indicator (land = 1,sea = 0):
! *** Presently only using land.
          ILAND = 1

! DTP,MSPEC,NSPEC

! Concentration cut off for very small
          do IC=1,NC
            if ( CR(IC,IR) .lt. 1.e-20 )   CR(IC,IR) = 0.0
          enddo

! *** Deposition not considered
          do JC=1,MSPEC
             DDEPVV(jc) = 0.0
             WDEPSRV(jc) = 0.0
          end do

!MSK: for receptor points and lines only photochemical balance
!
! Mapping photochemistry indices to compound indices

! Convert from ug/m3 to molec/cm3
! CP(ic) = C(ic)*((avogad*1.e-12)/cmolw(ic))

! new from chemspecs10.inc
!       OP          =   1  &
!      , OD          =   2  &
!      , OH          =   3  &
!      , HO2         =   4  &
!      , O3          =   5  &
!      , NO          =   6  &
!      , NO2         =   7  &
!      , HNO3        =   8  &
!      , HCHO        =   9  &
!      , CO          =  10



          do IC = 1,NC
             if (trim(cmpnd(ic)) == 'O3'   )  CV( 5) = CR(IC,IR)*((avogad*1.e-12)/cmolw(ic))
             if (trim(cmpnd(ic)) == 'HNO3' )  CV( 8) = CR(IC,IR)*((avogad*1.e-12)/cmolw(ic))
             if (trim(cmpnd(ic)) == 'NO'   )  CV( 6) = CR(IC,IR)*((avogad*1.e-12)/cmolw(ic))
             if (trim(cmpnd(ic)) == 'NO2'  )  CV( 7) = CR(IC,IR)*((avogad*1.e-12)/cmolw(ic))
             if (trim(cmpnd(ic)) == 'CO'   )  CV(10) = CR(IC,IR)*((avogad*1.e-12)/cmolw(ic))
             if (trim(cmpnd(ic)) == 'HCHO' )  CV( 9) = CR(IC,IR)*((avogad*1.e-12)/cmolw(ic))
          enddo
         ! print *,'cphotr10 before emep10',IR,CV(4),CV( 6),CV( 7),CV( 8),CV( 9),CV(10)

! Calculate photochemical balance between NO2, NO and O3

          CALL EMEP10(DTP,MSPEC,NC,CV,DDEP,WDEP,               &
                      SITELAV,SITELOV,IX,IY,IZ,IR,                      &
                      YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV,DAYYV,        &
                      TAIRV,DTDZV,RHUMV,PRECV,CLOUV,MOBULV,USTARV,      &
                      DDEPVV,WDEPSRV,HINVD,HINVW,IDRY,ILAND)
         ! print *,'cphotr10 after emep10',IR,CV(4),CV( 6),CV( 7),CV( 8),CV( 9),CV(10)

! Convert from molec/cm3 to ug/m3
! C(ic) = CP(ic)*cmolw(ic)/(avogad*1.e-12)
          do IC = 1,NC
            if (trim(cmpnd(ic)) == 'O3'    )   CR(IC,IR) = CV( 5)*cmolw(ic)/(avogad*1.e-12)
            if (trim(cmpnd(ic)) == 'HNO3'  )   CR(IC,IR) = CV( 8)*cmolw(ic)/(avogad*1.e-12)
            if (trim(cmpnd(ic)) == 'NO'    )   CR(IC,IR) = CV( 6)*cmolw(ic)/(avogad*1.e-12)
            if (trim(cmpnd(ic)) == 'NO2'   )   CR(IC,IR) = CV( 7)*cmolw(ic)/(avogad*1.e-12)
            if (trim(cmpnd(ic)) == 'CO'    )   CR(IC,IR) = CV(10)*cmolw(ic)/(avogad*1.e-12)
            if (trim(cmpnd(ic)) == 'HCHO'  )   CR(IC,IR) = CV( 9)*cmolw(ic)/(avogad*1.e-12)
          enddo
! Next receptor point


  100 CONTINUE


      RETURN

! End of subroutine CPHOTR10

      end subroutine cphotr10
