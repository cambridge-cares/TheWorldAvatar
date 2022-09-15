! <opmete.f90 - A component of the City-scale
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

      subroutine OPMETE

! The subroutine opens all meteorological data input files.
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

      use mod_main
      use mod_site
      use mod_mete

        implicit none

! Set temperature file unit, file format and open file

      call opifil(TEMPFN,TEMPUN,TEMPFE,TEMPFM)

! Set wind file unit, file format and open file

      call opifil(WINDFN,WINDUN,WINDFE,WINDFM)

! Set turbulence sigma-vw file unit, file format and open file

      call opifil(SDVWFN,SDVWUN,SDVWFE,SDVWFM)

! Set aerodynamic resistance file unit, file format and open file

      call opifil(AEROFN,AEROUN,AEROFE,AEROFM)

! Set mixing height file unit, file format and open file

      call opifil(HMIXFN,HMIXUN,HMIXFE,HMIXFM)

!_LHS_08_Oct_2004_Start:

! Set friction velocity file unit, file format and open file

      call opifil(USTRFN,USTRUN,USTRFE,USTRFM)

! Set sensible heat flux file unit, file format and open file

      call opifil(SHFLFN,SHFLUN,SHFLFE,SHFLFM)

! Set latent heat flux file unit, file format and open file

      call opifil(LHFLFN,LHFLUN,LHFLFE,LHFLFM)

! Set land-use file unit, file format and open file

      call opifil(LANUFN,LANUUN,LANUFE,LANUFM)

!MSK start
! Open MM5 met files

! Set surface moisture flux file unit, file format and open file

      call opifil(MFLXFN,MFLXUN,MFLXFE,MFLXFM)    

! Set surface momentum stress file unit, file format and open file

      call opifil(TAUSFN,TAUSUN,TAUSFE,TAUSFM)

!MSK Eddy diffusivity files are nowhere used
! Set heat diffusivity file unit, file format and open file
!MSK      CALL OPIFIL(KZ_HFN,KZ_HUN,KZ_HFE,KZ_HFM)
! Set momentum diffusivity file unit, file format and open file
!MSK      call opifil(KZ_MFN,KZ_MUN,KZ_MFE,KZ_MFM)

!MSK end

!MSK start TAPM input files

! Set 3D insitu temperature file unit, file format and open file

      call opifil(INS_TFN,INS_TUN,INS_TFE,INS_TFM)

! Set 3D potential temperature file unit, file format and open file

      call opifil(POT_TFN,POT_TUN,POT_TFE,POT_TFM)

! Set TAPM level midpoint file unit, file format and open file

      call opifil(GPOTFN,GPOTUN,GPOTFE,GPOTFM)

! Set potential temperature scale file unit, file format and open file

      call opifil(PTSTRFN,PTSTRUN,PTSTRFE,PTSTRFM)

! Set potential virtual temperature scale file unit, file format and open file

      call opifil(PVSTRFN,PVSTRUN,PVSTRFE,PVSTRFM)

! Set potential convective velocity scale file unit, file format and open file

      call opifil(WSTRFN,WSTRUN,WSTRFE,WSTRFM)

! Set total solar radiation file unit, file format and open file

      call opifil(TSRADFN,TSRADUN,TSRADFE,TSRADFM)

! Set ground temperature file unit, file format and open file

      call opifil(GTMPFN,GTMPUN,GTMPFE,GTMPFM)

!MSK end


! Set precipitation file unit, file format and open file

      call opifil(PRECFN,PRECUN,PRECFE,PRECFM)

! Set relative humidity file unit, file format and open file

      call opifil(RHUMFN,RHUMUN,RHUMFE,RHUMFM)

! Set cloud cover file unit, file format and open file

      call opifil(CLOUFN,CLOUUN,CLOUFE,CLOUFM)

      RETURN

! End of subroutine OPMETE

      end subroutine opmete
