! <gmphot.f90 - A component of the City-scale
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

      subroutine gmphot

!       2017 Matthias Karl, HZG, CityChem extension:
!            The subroutine calculates grid model photochemistry.
!            Introduced the call to the grid photochemistry as include file.
!            The call to the new EMEP45 chemistry scheme is in gmemep45.inc.
!            The call to the new EMEP70BIO scheme is in gmemep70.inc
!            The output of 2D surface photochemical and meteorological 
!            parameters is written to netcdf file photomhour.nc
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
!           2016  M. Karl: DChemP, GCCOR and Delta_C from CITYDELTA is not used
!    24 Apr 2018  M. Karl: L847-867    added the call to emep70bio for gridps==4
!                                      emep70bio mechanism is not the same as EMEP68
!    01 Mar 2019  M. Karl: L702        added missing "endif" in EMEP03 part
!                          L718        deleted closing "endif" in EMEP03 part
!                          L674        added PRESV in call to emep03
!    05 Mar 2009  M. Karl: L636        added the check for NSPEC = 3 in EMEP03 part
!
! ----------------------------------------------------------------------------------


      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_grid
!MSK start
      use mod_writenc
!MSK end

      implicit none
! *** External functions:
!MSK      LOGICAL ATTIME

! *** Local variables:

      integer :: MSPEC
      integer :: NSPEC
      integer :: DAYMV
      integer :: DAYYV
      integer :: HOURV
      integer :: IC
      integer :: IDRY
      integer :: ILAND
      integer :: INITV             ! New variable.
      integer :: ITP
      integer :: IX
      integer :: IY
      integer :: IZ
      integer :: MINUV
      integer :: MNTHV
      integer :: SECOV
      integer :: YEARV

      double precision, allocatable :: CP(:)
!MSK not used      REAL, ALLOCATABLE :: DChemP(:)        ! New variable.
!MSK not used      REAL, ALLOCATABLE :: GCCOR(:,:)
      real, allocatable :: DDEP(:)
      real, allocatable :: WDEP(:)
      real, allocatable :: DDEPVV(:)
      real, allocatable :: WDEPSRV(:)

      real :: DTP
      real :: SITELAV
      real :: SITELOV
      real :: TAIRV
      real :: DTDZV
      real :: RHUMV
      real :: SHUMV
      real :: PRESV
      real :: PRECV
      real :: CLOUV
      real :: MOBULV(0:1)
      real :: USTARV(0:1)
      real :: HINVD
      real :: HINVW

!MSK start
      real :: TSRADV
      real :: FXHEV
      real :: FXLAV
      real :: NY_AIR
      real :: ELEV
      real :: Z0V
      real :: UU
      real :: VV
      real :: HMIXV
      real :: TSTARV
      real :: TAIRG
      real :: JNO2V
      real :: JO3V
      real :: RC1V
      real :: RC3V
      real :: RC5V
      real :: RC6V
      real :: RC11V
      real :: RC32V
      real :: RC36V

      integer :: ISL
      integer, parameter :: NSL = 31
      double precision, allocatable :: CPH(:,:,:)
      character(len=10)  :: CPHNAME(NSL)
      character(len=10)  :: CPHUNIT(NSL)
!MSK for netCDF output
      character(len=10)  :: namefield
      character(len=10)  :: unitname
      character(len=23)  :: validity
      integer :: I, J
      integer :: Nhh_in
      integer, dimension(4,1) :: mdate
      logical :: dopacking  = .false.
      logical :: dofloat    = .false.
      logical :: domirror   = .false.
      double precision        :: field2D(NX,NY,1)
!MSk end

! *** MSPEC   - Total number of species in the model (compounds)
! *** NSPEC   - Actual number of species treated chemically (compounds)
! *** DTP     - The timestep (Unit = seconds)
! *** CP      - Concentration values (molecules/cm3)
! *** DChemP  - Delta concentrations (?City-Delta-application?)
! *** GCCOR   - Ground concentration correction factors (?deposition?)
! *** DDEP    - Dry deposition array (1D-vector)
! *** WDEP    - Wet deposition array (1D-vector)
! *** SITELAV - Latitudes  in degrees
! *** SITELOV - Longitudes in degrees
! *** TAIRV   - Temperature used in the chemistry calculations
! *** DTDZV   - Vertical temperature gradient
! *** RHUMV   - Relative humidity used in the chemistry calculations
! *** PRECV   - Precipitation in mm
! *** CLOUV   - Cloud fraction used in the j-values calculations
! *** MOBULV  - Monin-Obukhov length scale
! *** USTARV  - U* value
! *** DDEPVV  - Dry deposition velocities (1D-vector)
! *** WDEPSRV - Wet deposition scavenging ratios (1D-vector)
! *** HINVD   - Inverse height used in dry deposition
! *** HINVW   - Inverse height used in wet deposition
! *** DAYMV   - Day of month
! *** DAYYV   - Day of year
! *** HOURV   - Hour
! *** IC      - Compound index
! *** IDRY    - Dry deposition indicators
! *** ILAND   - Land/sea indicator (1=Land,0=Sea)
! *** INITV   - Initialisation indicator
! *** IX      - Grid model grid index in x-direction
! *** IY      - Grid model grid index in y-direction
! *** IZ      - Grid model grid index in z-direction
! *** SECOV   - Second
! *** MINUV   - Minute
! *** MNTHV   - Month
! *** YEARV   - Year

!MSK start
      integer NS, IR
!MSK end

      !print *,'gmphot GRIDPS',GRIDPS

! *** If INERT or SIMPLIFIED NO2/NO/O3 chemistry is to be applied  ==> RETURN
!MSK      IF (GRIDPS .NE. 2) RETURN
      IF (GRIDPS .LT. 2) RETURN

!_LHS_SOA_June_2007_Start:

! *** Specify the total number of species treated in the model  (MSPEC)
! *** and the number of species treated in the chemistry scheme (NSPEC):

      MSPEC = NC
      NSPEC = MSPEC - n_nochem

      !print *,'gmphot MSPEC NSPEC: ',MSPEC,NSPEC

 !MSK start
      if (GRIDPS == 3) then
        MSPEC = 45
        NSPEC = 45
      endif
      if (GRIDPS == 4) then
        MSPEC = 70
        NSPEC = 70
      endif

      if (GRIDPS >= 3) then
        CPHNAME( 1) = 'OP        ' 
        CPHNAME( 2) = 'OH        ' 
        CPHNAME( 3) = 'HO2       ' 
        CPHNAME( 4) = 'CH3O2     ' 
        CPHNAME( 5) = 'NO3       '
        CPHNAME( 6) = 'oXylOHO2  '
        CPHNAME( 7) = 'CH3COCHO  ' 
        CPHNAME( 8) = 'JNO2      '
        CPHNAME( 9) = 'JO3       '
        CPHNAME(10) = 'kOP_O2    '
        CPHNAME(11) = 'kOP_NO    '
        CPHNAME(12) = 'kO3_NO    '
        CPHNAME(13) = 'kO3_NO2   '
        CPHNAME(14) = 'kNO2_NO3  '
        CPHNAME(15) = 'kOH_CO    '
        CPHNAME(16) = 'kRO3_NO2  '
        CPHNAME(17) = 'TSRAD     '
        CPHNAME(18) = 'CLF       '
        CPHNAME(19) = 'TAIR      '
        CPHNAME(20) = 'RHUM      '
        CPHNAME(21) = 'ELEV      '
        CPHNAME(22) = 'Z0        '
        CPHNAME(23) = 'DTDZ      '
        CPHNAME(24) = 'INVMOL    '
        CPHNAME(25) = 'SHFL      '
        CPHNAME(26) = 'LHFL      '
        CPHNAME(27) = 'USTAR     '
        CPHNAME(28) = 'TSTAR     '
        CPHNAME(29) = 'HMIX      '
        CPHNAME(30) = 'U1        '
        CPHNAME(31) = 'V1        '

        CPHUNIT( 1) = 'molec/cm^3'
        CPHUNIT( 2) = 'molec/cm^3'
        CPHUNIT( 3) = 'molec/cm^3'
        CPHUNIT( 4) = 'molec/cm^3'
        CPHUNIT( 5) = 'molec/cm^3'
        CPHUNIT( 6) = 'molec/cm^3'
        CPHUNIT( 7) = 'molec/cm^3'
        CPHUNIT( 8) = '/s'
        CPHUNIT( 9) = '/s        '
        CPHUNIT(10) = 'cm^3/s    '
        CPHUNIT(11) = 'cm^3/s    '
        CPHUNIT(12) = 'cm^3/s    '
        CPHUNIT(13) = 'cm^3/s    '
        CPHUNIT(14) = 'cm^3/s    '
        CPHUNIT(15) = 'cm^3/s    '
        CPHUNIT(16) = 'cm^3/s    '
        CPHUNIT(17) = 'W/m^2     '
        CPHUNIT(18) = '          '
        CPHUNIT(19) = 'K         '
        CPHUNIT(20) = '          '
        CPHUNIT(21) = 'm         '
        CPHUNIT(22) = 'm         '
        CPHUNIT(23) = 'K/m       '
        CPHUNIT(24) = '/m        '
        CPHUNIT(25) = 'W/m^2     '
        CPHUNIT(26) = 'W/m^2     '
        CPHUNIT(27) = 'm/s       '
        CPHUNIT(28) = 'K         '
        CPHUNIT(29) = 'm         '
        CPHUNIT(30) = 'm/s       '
        CPHUNIT(31) = 'm/s       '
      endif
!MSK end

! *** Allocate delta space:

!MSK not used      IF (.NOT. ALLOCATED (Delta_C)) ALLOCATE (Delta_C(NSPEC,NX,NY,NZ))

!MSK start
      IF (.NOT. ALLOCATED (CPH))     ALLOCATE (CPH(NX,NY,NSL))
!MSK end

! *** NOTE: that the species index cover the 1:NSPEC first species of 
! ***       the C(MSPEC,NX,NY,NZ) array.

! *** Allocate local variables. Only the species treated in the chemistry.

      IF (.NOT. ALLOCATED (CP))      ALLOCATE (CP(NSPEC))
!MSK not used      IF (.NOT. ALLOCATED (DChemP))  ALLOCATE (DChemP(NSPEC))
!MSK not used      IF (.NOT. ALLOCATED (GCCOR))   ALLOCATE (GCCOR(NSPEC,0:2))
      IF (.NOT. ALLOCATED (DDEP))    ALLOCATE (DDEP(NSPEC)) 
      IF (.NOT. ALLOCATED (WDEP))    ALLOCATE (WDEP(NSPEC)) 
 
      IF (.NOT. ALLOCATED (DDEPVV))  ALLOCATE (DDEPVV(NSPEC)) 
      IF (.NOT. ALLOCATED (WDEPSRV)) ALLOCATE (WDEPSRV(NSPEC))

!_LHS_SOA_June_2007_End.

!_LHS_SOA_May_2007_Start:

! *** Set photochemistry time step (s):
! *** Note: SUBROUTINE GMPHOT is called every second timestep.

!     DTP = DT
      DTP = 2.*DT

!_LHS_May_207_End.

! *** Set site latitude and longitude:

      SITELAV = SITELA
      SITELOV = SITELO

! *** Set initialisation indicator "INITV":

      IF (ATTIME(BDAT)) THEN
        INITV = 1
      ELSE
        INITV = 0
      ENDIF

! *** Set time and date data:

      YEARV = YEAR
      MNTHV = MNTH
      DAYMV = DAYM
      HOURV = HOUR
      MINUV = MINU
      SECOV = SECO
      DAYYV = DAYY


      if (messfe) then
       write(messun,'(A,I4)') 'GMPHOT: GRIDDDS = ', GRIDDDS
      endif


! *** Go through all grid squares:

      DO IZ = 1,NZ
        DO IY = 1,NY
          DO IX = 1,NX

! ***       Go through all NSPEC species treated in the chemical scheme:

            DO IC = 1,NSPEC

! ***         Copy concentrations

!MSK              CP(IC) = C(IC,IX,IY,IZ)

!_LHS_SOA_May_2007_Start:
!
! ***         CITY-DELTA-Specific code ?
!
! ***         Copy delta concentrations. DChemP(1:NSPEC) set to zero 
! ***         at simulation start:
!MSK
!MSK              IF (INITV .EQ. 1) THEN
!MSK                DChemP(IC) = 0.
!MSK              ELSE
!MSK                DChemP(IC) = Delta_C(IC,IX,IY,IZ)
!MSK              ENDIF
!MSK
!_LHS_SOA_May_2007_End.

! ***         Define ground concentration correction factors:?

!MSK              GCCOR(IC,0) = 1.
!MSK              GCCOR(IC,1) = 1.
!MSK              GCCOR(IC,2) = 1.

! ***         Initial dry and wet depositions:

              DDEP(IC) = 0.
              WDEP(IC) = 0.


            ENDDO ! IC = 1,NSPEC

! ***       Set dry deposition indicator (GRIDDDS).
! ***       GRIDDDS = 0  ;  No Dry deposition applied.
! ***       GRIDDDS = 1  ;  "Normal" Dry deposition applied. (Used here)

            IDRY = GRIDDDS

! ***       Dry deposition only working within the lowermost layer:

            IF (IZ .GT. 1) IDRY = 0
! ***       Skip Dry Deposition:            
!            IF (IZ .GT. 0) IDRY = 0
            
            !print *,'gmphot idry meteExt: ', IDRY, pimeteexternaldata
!_LHS_SOA_June_2007_Start:
!
! ***       The meteorological input data below should be 3-dimensional.
! ***       Moreover, DTDZ (surf-layer temp gradient is not applied):
!
!_LHS_SOA_June_2007_End.

! ***       Set air temperature (K):
!MSK            if (MeteExternalData > 1) then
            if ( pimeteexternaldata > 1) then
              TAIRV = ins_t(IX,IY,IZ)
            else
              TAIRV = TAIR(IX,IY) + CTOK
            endif
            
! ***       Set air temperature gradient (K/m): Hva benyttes "DTDZV" til??
            DTDZV = DTDZ(IX,IY)

! ***       Set Pressure value (mb):
!MSK            PRESV = pres(IX,IY,IZ)
!MSK no meteo input for 3D pressure field
!MSK use standard pressure at ground
!MSK to be revised later
            PRESV = 1013.25

        
! ***       Set relative humidity (0-1):
!MSK start TAPM rhum3D is not used
!MSK            if (MeteExternalData == 3) then
!MSK            if (pimeteexternaldata == 3) then
!MSK              RHUMV = rhum3D(IX,IY,IZ)
!MSK            elseif (MeteExternalData == 2) then
            if (pimeteexternaldata == 2) then
!MSK end
              SHUMV = shum(IX,IY,IZ)

            else

              RHUMV = RHUM(IX,IY)

            endif
            
! ***       Set precipitation (mm/h):
            PRECV = PREC(IX,IY)

!MSK start
! ***       Set total solar radiation (W/m2)
            TSRADV = TSRAD(IX,IY)

! ***       Set cloud value (0-1):
            CLOUV = CLOU(IX,IY)
            !print *,'gmphot prec clou:', PRECV, CLOUV

! ***       Set Monin-Obukhov lengths (identical?):
            MOBULV(0) = MOBUL(IX,IY)
            MOBULV(1) = MOBUL(IX,IY)

! ***       Set friction velocities (m/s):
            USTARV(0) = USTAR(IX,IY)
            USTARV(1) = USTAR(IX,IY)

! ***       Variables for diagnostic output
            FXHEV  = SHFL(IX,IY)
            FXLAV  = LHFL(IX,IY)
            Z0V    = Z0(IX,IY)
            ELEV   = TOPM(IX,IY)
            HMIXV  = HMIX(IX,IY)
            TSTARV = TSTAR(IX,IY)
            if (IZ .eq. 1) then
              UU = U(IX,IY,IZ)
              VV = V(IX,IY,IZ)
              if ( pimeteexternaldata > 1) then
                TAIRG = ins_t(IX,IY,IZ)
              else
                TAIRG = TAIR(IX,IY) + CTOK
              endif
            endif
!MSK end


!MSK commented DRY DEPOSITION stuff
! ***       Set dry deposition velocities (m/s). DDEPV(IC) is read from the 
! ***       main input file (rconc.for) in cm/s and is ther converted to m/s:
! ***
!_LHS_HARD_CODED_LAND/SEA_DRY_DEPOSITION_VELOCITIES_15June2011_START:
!MSK            do IC = 1,NSPEC
!MSK
!MSK               DDEPVV(IC) = DDEPV(IC)
!MSK
!MSK            enddo
!MSK commented below, has to be specific for each EMEP mechanism
!            if (IDRY > 0) then
!              if (topm(IX,IY) >= 0.5 ) then
! ***           Treating topography heights above 0.5 m as land-points:
!                do IC = 1,NC
!                  if (cmpnd(ic) == 'O3        ')  DDEPVV(ic) = 0.5 * 0.01  ! O3       land in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'NO2       ')  DDEPVV(ic) = 0.1 * 0.01  ! NO2      land in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'HNO3      ')  DDEPVV(ic) = 4.0 * 0.01  ! HNO3     land in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'SO2       ')  DDEPVV(ic) = 0.5 * 0.01  ! SO2      land in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'Sulphate  ')  DDEPVV(ic) = 1.0 * 0.01  ! Sulphate land in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'CH3O2H    ')  DDEPVV(ic) = 0.5 * 0.01  ! CH3O2H   land in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'PAN       ')  DDEPVV(ic) = 0.8 * 0.01  ! PAN      land in Abu Dhabi (cm/s)
!                enddo
!              else
! ***           Treating topography heights below 0.5 m as sea-points:
!                do IC = 1,NC
!                  if (cmpnd(ic) == 'O3        ')  DDEPVV(ic) = 0.07 * 0.01  ! O3       sea in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'H2O2      ')  DDEPVV(ic) = 1.0  * 0.01  ! H2O2     sea in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'NO2       ')  DDEPVV(ic) = 0.02 * 0.01  ! NO2      sea in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'HNO3      ')  DDEPVV(ic) = 1.0  * 0.01  ! HNO3     sea in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'SO2       ')  DDEPVV(ic) = 0.5  * 0.01  ! SO2      sea in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'Sulphate  ')  DDEPVV(ic) = 1.0  * 0.01  ! Sulphate sea in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'CH3O2H    ')  DDEPVV(ic) = 1.0  * 0.01  ! CH3O2H   sea in Abu Dhabi (cm/s)
!                  if (cmpnd(ic) == 'PAN       ')  DDEPVV(ic) = 0.8  * 0.01  ! PAN      sea in Abu Dhabi (cm/s)
!                enddo
!              endif
!            endif
!
!_LHS_HARD_CODED_LAND/SEA_DRY_DEPOSITION_VELOCITIES_15June2011_END.
             


! ***       Set wet deposition scavenging ratios. Also read from main 
! ***       input file in Subroutine RCONC:
!MSK            do IC = 1,NSPEC
!MSK              WDEPSRV(IC) = WDEPSR(IC)
!MSK            ENDDO

! ***       Set inverse heights for dry and wet deposition (1/m):
            HINVD = 1./DZ(1)
            HINVW = 1./DZ(1)

! ***       Set land/sea indicator (land = 1,sea = 0):
! ***       Presently only using land.
            ILAND = 1


! **********************************************************************

! ***        Calculate photochemical reactions:

! **********************************************************************
!_LHS_SOA_May_2007_Start:
!
! ***       In SEWs EPISODE_3.2-version Sam-Erik has here included:
!
!
! MSK: The NO2-NO-O3 photoequilibrium is solved in CPHOTM.FOR if gridps=1 and photps=1


!!DEC$ IF DEFINED (emep03)

           !print *,'gmphot before emep03 exec', GRIDPS

!MSK start
!MSK            IF (GRIDPS == 2 .AND. NSPEC == 3) THEN
           IF (GRIDPS == 2) THEN

!MSK 05.03.2019 Add the check for NSPEC = 3
              IF (NSPEC == 3) THEN

                 do IC = 1,NSPEC
                   CP(IC) = 0.0
                   if (cmpnd(ic)      == 'NO       ' )  DDEPVV( 2) = DDEPV(IC)  
                   if (topm(IX,IY) >= 0.5 ) then
! ***           Treating topography heights above 0.5 m as land-points:
                     if (cmpnd(ic)(1:2) == 'O3' )  DDEPVV( 1) = 0.2 * 0.01  ! O3      land
                     if (cmpnd(ic)(1:3) == 'NO2')  DDEPVV( 3) = 0.1 * 0.01  ! NO2     land
                   else
! ***           Treating topography heights below 0.5 m as sea-points:
                     if (cmpnd(ic)(1:2) == 'O3' )  DDEPVV( 1) = 0.07 * 0.01  ! O3      sea
                     if (cmpnd(ic)(1:3) == 'NO2')  DDEPVV( 3) = 0.02 * 0.01  ! NO2     sea
                   endif

                   if (cmpnd(ic)(1:2) == 'O3' )  WDEPSRV( 1) = WDEPSR(IC)
                   if (cmpnd(ic)      == 'NO       ' )  WDEPSRV( 2) = WDEPSR(IC)
                   if (cmpnd(ic)(1:3) == 'NO2')  WDEPSRV( 3) = WDEPSR(IC)

! Convert from ug/m3 to molec/cm3
! CP(ic) = C(ic)*((avogad*1.e-12)/cmolw(ic))
                   if (cmpnd(ic)(1:2) == 'O3' )  CP( 1) = C(IC,IX+1,IY+1,IZ)*((avogad*1.e-12)/cmolw(ic))
                   if (cmpnd(ic)      == 'NO       ' )  CP( 2) = C(IC,IX+1,IY+1,IZ)*((avogad*1.e-12)/cmolw(ic))
                   if (cmpnd(ic)(1:3) == 'NO2')  CP( 3) = C(IC,IX+1,IY+1,IZ)*((avogad*1.e-12)/cmolw(ic))

                 enddo


!MSK end

             !MSK: some output check
             !print *,'gmphot vars: ',DTP,MSPEC,NSPEC,CP(1),DDEP(1),WDEP(1),      &
             !           SITELAV,SITELOV,IX,IY,IZ ,      &
             !           YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV,DAYYV,      &
             !           TAIRV,DTDZV ,RHUMV, PRESV,   &
             !           PRECV,CLOUV,MOBULV(0),USTARV(0),      &
             !           DDEPVV(1),WDEPSRV(1) ,  HINVD,HINVW,IDRY,ILAND


                 CALL EMEP03(DTP,MSPEC,NSPEC,CP,DDEP,WDEP,                   &
                             SITELAV,SITELOV,IX,IY,IZ, IR,                   &
                             YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV,DAYYV,      &
                             TAIRV,DTDZV,RHUMV,PRESV,                        &
                             PRECV,CLOUV,TSRADV,MOBULV,USTARV,               &
                             DDEPVV,WDEPSRV,HINVD,HINVW,IDRY,ILAND)

!MSK start
! ***         Copy concentrations back:
! compounds may be in different order

! Convert from molec/cm3 to ug/m3
! C(ic) = CP(ic)*cmolw(ic)/(avogad*1.e-12)
                 do IC = 1,NC
                   if (cmpnd(ic)(1:2) == 'O3' )  C(IC,IX+1,IY+1,IZ) = CP( 1)*cmolw(ic)/(avogad*1.e-12)
                   if (cmpnd(ic)      == 'NO       ' )  C(IC,IX+1,IY+1,IZ) = CP( 2)*cmolw(ic)/(avogad*1.e-12)
                   if (cmpnd(ic)(1:3) == 'NO2' ) C(IC,IX+1,IY+1,IZ) = CP( 3)*cmolw(ic)/(avogad*1.e-12)
                 enddo

                 IF (IZ .EQ. 1) THEN
                   do IC = 1, NC
                     if (cmpnd(ic)(1:2) == 'O3' )     DDEPM(IX,IY,IC) = DDEP( 1)
                     if (cmpnd(ic)      == 'NO      ' )     DDEPM(IX,IY,IC) = DDEP( 2)
                     if (cmpnd(ic)(1:3) == 'NO2')     DDEPM(IX,IY,IC) = DDEP( 3)
                   enddo

                   do IC = 1, NC
                     if (cmpnd(ic)(1:2) == 'O3' )     WDEPM(IX,IY,IC) = WDEP( 1)
                     if (cmpnd(ic)      == 'NO      ' )     WDEPM(IX,IY,IC) = WDEP( 2)
                     if (cmpnd(ic)(1:3) == 'NO2')     WDEPM(IX,IY,IC) = WDEP( 3)
                   enddo

!MSK 01.03.2019: missing endif (iz.eq.1)
                 ENDIF

              ELSE

                 print *,'gmphot GRIDPS NSPEC: ',GRIDPS,NSPEC
                 CALL STOPIT('EMEP03 requires that NSPEC is 3!')

! end check NSPEC
              ENDIF

           ENDIF
!MSK end

!_EMEP03	       IF (MESSFE) WRITE (MESSUN,*) 'GMPHOT: Just finished CALL EMEP03.'




!!DEC$ IF DEFINED (emep45)
!
! 
!_LHS_SOA_May_2007_End.
!
! **********************************************************************
!MSK start
! GRIDPS=3 
! EMEP45
! *** CP      - Concentration values (molecules/cm3)
! Grid concentrations C(IC,IX+1,IY+1,IZ) are in ug/m3

           IF (GRIDPS == 3) THEN

               do NS = 1,NSPEC
                   CP(NS)      = 0.0
                   DDEPVV(NS)  = 0.0
                   WDEPSRV(NS) = 0.0
               enddo

! ***  Deposition update is done in gmdepo.f90
! ***  Dry Deposition Mapping
!               do IC = 1,NC
!                   if (cmpnd(ic)      == 'NO      '  ) DDEPVV( 7) = DDEPV(IC)
!                   if (cmpnd(ic)(1:4) == 'N2O5'   )  DDEPVV(10) = DDEPV(IC)
!                   if (cmpnd(ic)(1:4) == 'HCHO'   )  DDEPVV(17) = DDEPV(IC)
!                   if (topm(IX,IY) >= 0.5 ) then
! ***           Treating topography heights above 0.5 m as land-points:
!                     if (cmpnd(ic)(1:2) == 'O3'     )  DDEPVV( 4) = 0.5 * 0.01
!                     if (cmpnd(ic)(1:3) == 'NO2'    )  DDEPVV( 8) = 0.1 * 0.01
!                     if (cmpnd(ic)(1:4) == 'H2O2'   )  DDEPVV( 6) = DDEPV(IC)
!                     if (cmpnd(ic)(1:4) == 'HNO3'   )  DDEPVV(11) = 4.0 * 0.01 
!                     if (cmpnd(ic)(1:3) == 'SO2'    )  DDEPVV(12) = 0.5 * 0.01
!                     if (cmpnd(ic)(1:8) == 'Sulphate'  ) DDEPVV(13) = 1.0 * 0.01
!                     if (cmpnd(ic)(1:6) == 'CH3O2H' )  DDEPVV(19) = 0.5 * 0.01
!                     if (cmpnd(ic)(1:3) == 'PAN'    )  DDEPVV(25) = 0.8 * 0.01
!                   else
! ***           Treating topography heights below 0.5 m as sea-points:
!                     if (cmpnd(ic)(1:2) == 'O3'     )  DDEPVV( 4) = 0.07 * 0.01
!                     if (cmpnd(ic)(1:3) == 'NO2'    )  DDEPVV( 8) = 0.02 * 0.01
!                     if (cmpnd(ic)(1:4) == 'H2O2'   )  DDEPVV( 6) = 1.0  * 0.01
!                     if (cmpnd(ic)(1:4) == 'HNO3'   )  DDEPVV(11) = 1.0  * 0.01
!                     if (cmpnd(ic)(1:3) == 'SO2'    )  DDEPVV(12) = 0.5  * 0.01
!                     if (cmpnd(ic)(1:8) == 'Sulphate'  ) DDEPVV(13) = 1.0  * 0.01
!                     if (cmpnd(ic)(1:6) == 'CH3O2H' )  DDEPVV(19) = 1.0  * 0.01
!                     if (cmpnd(ic)(1:3) == 'PAN'    )  DDEPVV(25) = 0.8  * 0.01
!                   endif

! ***  Wet Deposition Mapping
!                   if (cmpnd(ic)(1:2) == 'O3'     )     WDEPSRV( 4) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:4) == 'H2O2'   )     WDEPSRV( 6) = WDEPSR(IC)
!                   if (cmpnd(ic)      == 'NO      '  )  WDEPSRV( 7) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:3) == 'NO2'    )     WDEPSRV( 8) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:4) == 'N2O5'   )     WDEPSRV(10) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:4) == 'HNO3'   )     WDEPSRV(11) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:3) == 'SO2'    )     WDEPSRV(12) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:8) == 'Sulphate'  )  WDEPSRV(13) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:2) == 'H2'     )     WDEPSRV(14) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:3) == 'CH4'    )     WDEPSRV(15) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:4) == 'HCHO'   )     WDEPSRV(17) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:5) == 'CH3OH'  )     WDEPSRV(18) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:6) == 'CH3O2H' )     WDEPSRV(19) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:2) == 'CO'     )     WDEPSRV(20) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:4) == 'C2H6'   )     WDEPSRV(21) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:6) == 'CH3CHO' )     WDEPSRV(23) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:3) == 'PAN'    )     WDEPSRV(25) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:6) == 'C2H5OH' )     WDEPSRV(26) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:6) == 'nC4H10' )     WDEPSRV(27) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:9) == 'CH3COC2H5' )  WDEPSRV(29) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:4) == 'C2H4'   )     WDEPSRV(32) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:4) == 'C3H6'   )     WDEPSRV(34) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:6) == 'oXylen' )     WDEPSRV(36) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:9) == 'Memaldial' )  WDEPSRV(38) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:8) == 'CH3COCHO'  )  WDEPSRV(39) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:6) == 'HCOCHO' )     WDEPSRV(41) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:8) == 'Isoprene'  )  WDEPSRV(42) = WDEPSR(IC)
!                   if (cmpnd(ic)(1:8) == 'MVKetone'  )  WDEPSRV(44) = WDEPSR(IC)
!               enddo


                include 'gmemep45.inc'

!MSK debug start
!debug              if ( (iz==1).and.(iy==16).and.(ix==1) ) then
!debug                   print *,'gmphot  CO', CP( 41), C(6,IX+1,IY+1,1), CM(1,16,6)
!debug                   print *,'gmphot NO2', CP( 17), C(3,IX+1,IY+1,1), CM(1,16,3)
!debug               endif
!MSK debug end

! ***  Deposition update is done in gmdepo.f90
! *** Copy Dry Deposition back, layer 1
!               IF (IZ .EQ. 1) THEN
!                 do IC = 1,NC
!                   if (cmpnd(ic)(1:2) == 'O3'     )     DDEPM(IX,IY,IC) = DDEP( 4)
!                   if (cmpnd(ic)(1:4) == 'H2O2'   )     DDEPM(IX,IY,IC) = DDEP( 6)
!                   if (cmpnd(ic)      == 'NO      '  )  DDEPM(IX,IY,IC) = DDEP( 7)
!                   if (cmpnd(ic)(1:3) == 'NO2'    )     DDEPM(IX,IY,IC) = DDEP( 8)
!                   if (cmpnd(ic)(1:4) == 'N2O5'   )     DDEPM(IX,IY,IC) = DDEP(10)
!                   if (cmpnd(ic)(1:4) == 'HNO3'   )     DDEPM(IX,IY,IC) = DDEP(11)
!                   if (cmpnd(ic)(1:3) == 'SO2'    )     DDEPM(IX,IY,IC) = DDEP(12)
!                   if (cmpnd(ic)(1:8) == 'Sulphate'  )  DDEPM(IX,IY,IC) = DDEP(13)
!                   if (cmpnd(ic)(1:2) == 'H2'     )     DDEPM(IX,IY,IC) = DDEP(14)
!                   if (cmpnd(ic)(1:3) == 'CH4'    )     DDEPM(IX,IY,IC) = DDEP(15)
!                   if (cmpnd(ic)(1:4) == 'HCHO'   )     DDEPM(IX,IY,IC) = DDEP(17)
!                   if (cmpnd(ic)(1:5) == 'CH3OH'  )     DDEPM(IX,IY,IC) = DDEP(18)
!                   if (cmpnd(ic)(1:6) == 'CH3O2H' )     DDEPM(IX,IY,IC) = DDEP(19)
!                   if (cmpnd(ic)(1:2) == 'CO'     )     DDEPM(IX,IY,IC) = DDEP(20)
!                  if (cmpnd(ic)(1:4) == 'C2H6'   )     DDEPM(IX,IY,IC) = DDEP(21)
!                  if (cmpnd(ic)(1:6) == 'CH3CHO' )     DDEPM(IX,IY,IC) = DDEP(23)
!                   if (cmpnd(ic)(1:3) == 'PAN'    )     DDEPM(IX,IY,IC) = DDEP(25)
!                   if (cmpnd(ic)(1:6) == 'C2H5OH' )     DDEPM(IX,IY,IC) = DDEP(26)
!                   if (cmpnd(ic)(1:6) == 'nC4H10' )     DDEPM(IX,IY,IC) = DDEP(27)
!                   if (cmpnd(ic)(1:9) == 'CH3COC2H5' )  DDEPM(IX,IY,IC) = DDEP(29)
!                   if (cmpnd(ic)(1:4) == 'C2H4'   )     DDEPM(IX,IY,IC) = DDEP(32)
!                   if (cmpnd(ic)(1:4) == 'C3H6'   )     DDEPM(IX,IY,IC) = DDEP(34)
!                   if (cmpnd(ic)(1:6) == 'oXylen' )     DDEPM(IX,IY,IC) = DDEP(36)
!                   if (cmpnd(ic)(1:9) == 'Memaldial' )  DDEPM(IX,IY,IC) = DDEP(38)
!                   if (cmpnd(ic)(1:8) == 'CH3COCHO'  )  DDEPM(IX,IY,IC) = DDEP(39)
!                   if (cmpnd(ic)(1:6) == 'HCOCHO' )     DDEPM(IX,IY,IC) = DDEP(41)
!                   if (cmpnd(ic)(1:8) == 'Isoprene'  )  DDEPM(IX,IY,IC) = DDEP(42)
!                   if (cmpnd(ic)(1:8) == 'MVKetone'  )  DDEPM(IX,IY,IC) = DDEP(44)
!                 enddo
!               ENDIF
!
! *** Copy Wet Deposition back, layer 1
!               IF (IZ .EQ. 1) THEN
!                 do IC = 1,NC
!                   if (cmpnd(ic)(1:2) == 'O3'     )     WDEPM(IX,IY,IC) = WDEP( 4)
!                   if (cmpnd(ic)(1:4) == 'H2O2'   )     WDEPM(IX,IY,IC) = WDEP( 6)
!                   if (cmpnd(ic)      == 'NO      '  )  WDEPM(IX,IY,IC) = WDEP( 7)
!                   if (cmpnd(ic)(1:3) == 'NO2'    )     WDEPM(IX,IY,IC) = WDEP( 8)
!                   if (cmpnd(ic)(1:4) == 'N2O5'   )     WDEPM(IX,IY,IC) = WDEP(10)
!                   if (cmpnd(ic)(1:4) == 'HNO3'   )     WDEPM(IX,IY,IC) = WDEP(11)
!                   if (cmpnd(ic)(1:3) == 'SO2'    )     WDEPM(IX,IY,IC) = WDEP(12)
!                   if (cmpnd(ic)(1:8) == 'Sulphate'  )  WDEPM(IX,IY,IC) = WDEP(13)
!                   if (cmpnd(ic)(1:2) == 'H2'     )     WDEPM(IX,IY,IC) = WDEP(14)
!                   if (cmpnd(ic)(1:3) == 'CH4'    )     WDEPM(IX,IY,IC) = WDEP(15)
!                   if (cmpnd(ic)(1:4) == 'HCHO'   )     WDEPM(IX,IY,IC) = WDEP(17)
!                   if (cmpnd(ic)(1:5) == 'CH3OH'  )     WDEPM(IX,IY,IC) = WDEP(18)
!                   if (cmpnd(ic)(1:6) == 'CH3O2H' )     WDEPM(IX,IY,IC) = WDEP(19)
!                   if (cmpnd(ic)(1:2) == 'CO'     )     WDEPM(IX,IY,IC) = WDEP(20)
!                   if (cmpnd(ic)(1:4) == 'C2H6'   )     WDEPM(IX,IY,IC) = WDEP(21)
!                   if (cmpnd(ic)(1:6) == 'CH3CHO' )     WDEPM(IX,IY,IC) = WDEP(23)
!                   if (cmpnd(ic)(1:3) == 'PAN'    )     WDEPM(IX,IY,IC) = WDEP(25)
!                   if (cmpnd(ic)(1:6) == 'C2H5OH' )     WDEPM(IX,IY,IC) = WDEP(26)
!                   if (cmpnd(ic)(1:6) == 'nC4H10' )     WDEPM(IX,IY,IC) = WDEP(27)
!                   if (cmpnd(ic)(1:9) == 'CH3COC2H5' )  WDEPM(IX,IY,IC) = WDEP(29)
!                   if (cmpnd(ic)(1:4) == 'C2H4'   )     WDEPM(IX,IY,IC) = WDEP(32)
!                   if (cmpnd(ic)(1:4) == 'C3H6'   )     WDEPM(IX,IY,IC) = WDEP(34)
!                   if (cmpnd(ic)(1:6) == 'oXylen' )     WDEPM(IX,IY,IC) = WDEP(36)
!                   if (cmpnd(ic)(1:9) == 'Memaldial' )  WDEPM(IX,IY,IC) = WDEP(38)
!                   if (cmpnd(ic)(1:8) == 'CH3COCHO'  )  WDEPM(IX,IY,IC) = WDEP(39)
!                   if (cmpnd(ic)(1:6) == 'HCOCHO' )     WDEPM(IX,IY,IC) = WDEP(41)
!                   if (cmpnd(ic)(1:8) == 'Isoprene'  )  WDEPM(IX,IY,IC) = WDEP(42)
!                   if (cmpnd(ic)(1:8) == 'MVKetone'  )  WDEPM(IX,IY,IC) = WDEP(44)
!                 enddo
!               ENDIF

              ! Fill CPH with values in first layer at end of simulation hour
                if ( (IZ.EQ.1).AND.(ITS.eq.NTS-1) ) then
                    CPH(IX,IY,7) = CP( 26)       !CH3COCHO=MGLYOX; NO-NO2 conversion
                endif
              
           ENDIF    ! EMEP45

           IF (GRIDPS == 4) THEN

               do NS = 1,NSPEC
                   CP(NS)      = 0.0
                   DDEPVV(NS)  = 0.0
                   WDEPSRV(NS) = 0.0
               enddo

               include 'gmemep70.inc'

              ! Fill CPH with values in first layer at end of simulation hour
               if ( (IZ.EQ.1).AND.(ITS.eq.NTS-1) ) then
                   CPH(IX,IY,7) = CP( 27)       !CH3COCHO=MGLYOX; NO-NO2 conversion
               endif
           
           ENDIF     ! EMEP70BIO

           IF (GRIDPS >= 3) THEN
              ! Fill CPH with values in first layer at end of simulation hour
               IF ( (IZ.EQ.1).AND.(ITS.eq.NTS-1) ) THEN
                   CPH(IX,IY,1) = CP(  1)       ! OP
                   CPH(IX,IY,2) = CP(  3)       ! OH
                   CPH(IX,IY,3) = CP(  4)       ! HO2
                   CPH(IX,IY,4) = CP(  5)       ! CH3O2
                   CPH(IX,IY,5) = CP( 19)       ! NO3    
                   CPH(IX,IY,6) = CP( 11)       !oXylOHO2       ; NO-NO2 conversion
                   !CPH(IX,IY,7) = CP( 26) ! different index
                   CPH(IX,IY,8) = JNO2V         ! DJ(3)
                   CPH(IX,IY,9) = JO3V          ! DJ(1)+DJ(2)
                   CPH(IX,IY,10)= RC1V          ! RC(1)
                   CPH(IX,IY,11)= RC3V          ! RC(3)
                   CPH(IX,IY,12)= RC5V          ! RC(5)
                   CPH(IX,IY,13)= RC6V          ! RC(6)
                   CPH(IX,IY,14)= RC11V         ! RC(11) NO2+NO3
                   CPH(IX,IY,15)= RC32V         ! RC(32) CO+OH
                   CPH(IX,IY,16)= RC36V         ! CH3COO2 + NO2
                   CPH(IX,IY,17)= TSRADV        ! Tot. Solar Rad.
                   CPH(IX,IY,18)= CLOUV         ! Frac.Cloud cover
                   CPH(IX,IY,19)= TAIRG         ! Air Temp. at ground
                   CPH(IX,IY,20)= RHUMV         ! Rel. Humid.
                   CPH(IX,IY,21)= ELEV          ! Height
                   CPH(IX,IY,22)= Z0V           ! Surf. Roughness
                   CPH(IX,IY,23)= DTDZV         ! dT/dz (in K/m)
                   CPH(IX,IY,24)= 1./MOBULV(1)  ! Inverse Obukhov L (m)
                   CPH(IX,IY,25)= FXHEV         ! Sensible heat
                   CPH(IX,IY,26)= FXLAV         ! Latent heat
                   CPH(IX,IY,27)= USTARV(1)     ! U* (m/s)
                   CPH(IX,IY,28)= TSTARV        ! TST (K)
                   CPH(IX,IY,29)= HMIXV         ! Mixing hgt. (m)
                   CPH(IX,IY,30)= UU            ! U-wind (m/s)
                   CPH(IX,IY,31)= VV            ! V-wind (m/s)
                    !print *,'gmphot CPH ',CPH
               ENDIF
           ENDIF

!MSK end


! ***       Next main grid cell.

          ENDDO
        ENDDO
      ENDDO


!MSK start
!     Last timestep in current hour?
!     Write some diagnostic 2-D output for EMEP45

      if (GRIDPS >= 3) then

        !print *,'gmphot its nts dowrite',its,nts

! *** Last timestep in a new hour
! *** and not first hour of simulation
        if (ITS.eq.NTS-1) then
          if (NH1GRIDFE) then

             IF (MESSFE) WRITE (MESSUN,2020)   &
                      MOD(YEAR,100),MNTH,DAYM,HOUR

              ! one time step 
              Nhh_in = 1

              ! current simulation date
              mdate(1,Nhh_in) = YEAR
              mdate(2,Nhh_in) = MNTH
              mdate(3,Nhh_in) = DAYM
              mdate(4,Nhh_in) = HOUR

              validity  = 'instantaneous'
              dopacking = .false.
              dofloat   = .true.
             ! domirror  = .true.
              domirror  = .false.

              do 300 ISL = 1,NSL

                namefield = trim(CPHNAME(ISL))
                unitname  = trim(CPHUNIT(ISL))

                do 310 J = 1,NX
                  do 320 I = 1,NY

                    field2D(J,I,1) = CPH(J,I,ISL)

  320             continue
  310           continue

                !print *,'gmphot write NH1',isl,namefield,field2D(15,15,1)

                call writeconcfield(NH1GRIDFN, namefield,unitname,field2D(1:NX,1:NY,1) ,NX, NY, 1,   &
                                 Nhh_in, mdate, validity, dopacking, dofloat, domirror)


  300         continue

          endif
        endif
      endif
!MSK end

!_LHS_SOA_June_2007_Start:
!
! *** Deallocate local variables:

      IF (ALLOCATED (CP))      DEALLOCATE (CP)
!MSK start
      IF (ALLOCATED (CPH))     DEALLOCATE (CPH)
!MSK      IF (ALLOCATED (DChemP))  DEALLOCATE (DChemP)
!MSK      IF (ALLOCATED (GCCOR))   DEALLOCATE (GCCOR)
!MSK end
      IF (ALLOCATED (DDEP))    DEALLOCATE (DDEP) 
      IF (ALLOCATED (WDEP))    DEALLOCATE (WDEP) 
      IF (ALLOCATED (DDEPVV))  DEALLOCATE (DDEPVV) 
      IF (ALLOCATED (WDEPSRV)) DEALLOCATE (WDEPSRV)

!_LHS_SOA_June_2007_End.

      RETURN
!MSK start
 2020 FORMAT ('GMPHOT: Write main grid diagnostics for time ',  &
             4I2.2)
!MSK end
! *** End of subroutine GMPHOT

      end subroutine gmphot
