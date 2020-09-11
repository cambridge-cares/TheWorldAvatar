! <citychem.f90 - A component of the City-scale
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
!  PROGRAM: CITYCHEM
!
!          CITY-scale CHEMistry Transport Model
!
!
!  PURPOSE:    Program calculates dispersion of pollutants
!              on an Eulerian grid and in specified
!              receptor points
!
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

      program citychem

! The EPISODE-CityChem urban dispersion model main program.
!
! Extension to EPISODE
!
! ----------------------------------------------------------------------------------
! VERSION 1.2
!
! Changes compared to V1.0a:
!    15 Feb 2018  M. Karl: Activation of point source module in the standard configuration
!    10 Apr 2018  M. Karl: Vertical distribution of domestic heating 90:10 (lay1:2)
!    11 Apr 2018  M. Karl: Vertical distribution of commercial comb. 80:20 (lay1:2)
!    18 Apr 2018  M. Karl: Option to add PM-offset to 3D BCON input file
!    23 Apr 2018  M. Karl: Changed street canyon width and direct contribution
!    04 May 2018  M. Karl: Not used emission rates in EMEP10 set to zero
! Changes in V1.1.1
!    10 Aug 2018  M. Karl: Street canyon: minimum integration path for windward 
!    13 Aug 2018  M. Karl: Call local photochemistry in TSPHOT every hour (Paul Hamer, NILU)
!    14 Aug 2018  M. Karl: Street canyon: extending leeward to 0-180 deg
!    22 Aug 2018  M. Karl: Street canyon: correction of recirculation zone length
!    22 Aug 2018  M. Karl: Street canyon: dilution of contribution at windward
!    22 Oct 2018  M. Karl: In EMEP10-Plume replaced H2O2 by HNO3
!    26 Nov 2018  M. Karl: Commented preprocessor directives in util/ngetarg.f90
! Changes in V1.2.1
!    05 Mar 2019  M. Karl: Fixed EMEP03 call on main grid (GRIDPS=2)
! Changes in V1.3
!    30 Jul 2019  M. Karl: Fixed missing filename issue in util/opifil.f90
!    19 Aug 2019  M. Karl: Ground HONO source (EmChem09-het)
!    19 Aug 2019  M. Karl: Ground HONO source (RCGROUND in emep03/mod_emep03.f90)
!    17 Sep 2019  M. Karl: Variable BCON value for Sulphate
!    17 Sep 2019  M. Karl: Three BCON methods are allowed now (3D, 1D, constant)
!    11 Nov 2019  M. Karl: Commented inquire/close main/iniepi.f90 and main/rmain.f90
!    20 Nov 2019  M. Karl: asrc/rasrcfn.f90 bug fix for not existing area source file
!
! ----------------------------------------------------------------------------------

      !use mod_util
      !use mod_main
      !use mod_site
      use mod_time
      !use mod_mete
      !use mod_conc


! IC     - Index of compound
! AFTIME - If after  given time then true else false
! ATDATE - If at     given date then true else false
! BFTIME - If before given time then true else false

! Initialize the model by reading main input data
      
      call iniepi
      
! Simulation period exceeded?

      if (aftime(edat)) then

! Write finish message, close message file and stop

         call exiepi
         call stopit('CityChem: Start time after end time!')

      endif

! Perform model calculations for the whole simulation period

  100 continue

! Perform calculations for the current simulation subperiod
      
 
      call runepi

! Increment hour by TSIMRES/3600

      call incrtm


! If not end of whole simulation period then goto next
! subperiod

      IF (bftime(edat) .or.                &
         ( atdate(edat) .and. (ish <= nsh) ) ) goto 100
      
! End of program CITYCHEM
      
   end program citychem
