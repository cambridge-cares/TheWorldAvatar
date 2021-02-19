! <radiat.f90 - A component of the City-scale
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

      subroutine radiat(SIGMA,SINPHI,TM,RNN,RKIN,RALB,QSTI)  
! ---------------------------------------------------------------------
!         Radiation scheme according to van Ulden and Holtslag 
!         (1985,JCAM, 24, 1196-1207).
!         
!
!         Input:
!             SIGMA  : Stefan Boltzmanns constant
!             SINPHI : Sine of the solar elevation
!             RNN    : Cloud cover fraction (0-1)   
!             RKIN   : Incoming short wave radiation (W/M2)
!                      (MISSING : -9900. or 0.0)
!             RALB   : Surface Albedo [0.0 - 1.0(total reflection)]   
!
!                      SURFACE      ALBEDO(%)   RALB
!               1  :   DARK SOIL       10       0.10
!               2  :   FOREST          15       0.15
!               3  :   GRASS,LUSH      20       0.20
!               4  :   SAND            25       0.25
!               5  :   ICE,OLD         30       0.30
!               6  :   ICE,NEW         35       0.35
!               7  :   SNOW,OLD        40       0.40
!               8  :   SNOW,NORMAL     60       0.60
!               9  :   SNOW,FRESH      80       0.80
!        
!         Output:
!             QSTI   : Isothermal net radiation, Q*_i. (W/M2)

!    ------------------------------------------------------------------
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

        implicit none

! *** Input variables:
      REAL,INTENT(IN)  :: SIGMA,SINPHI,TM,RNN,RKIN,RALB

! *** Output variables
      REAL,INTENT(OUT) :: QSTI

! *** Local variables:
      REAL :: RKST,RLSTI
	
! *** The values of the constants below are identical with the ones
! *** given in vU&H, 1985. 
      REAL,PARAMETER :: A1    = 990.0 
      REAL,PARAMETER :: A2    = -30.0
      REAL,PARAMETER :: B1    =   0.75
      REAL,PARAMETER :: B2    =   3.4
      REAL,PARAMETER :: C1    =   9.35E-6
      REAL,PARAMETER :: C2    =  60.0
      REAL,PARAMETER :: SPHI0 =   0.03

!     Selection of albedo (input parameter)

      IF(RKIN >= 0.0) THEN
! ***   If measured values of the incoming short wave rad. exists:
          RKST = RKIN*(1.0 - RALB)
      ELSE
!       For PHI < 1.7 degrees : No short wave radiation (vU&H, 1985). 
        IF (SINPHI .LT. SPHI0) THEN
           RKST = 0.0
        ELSE                                      
! ***     The net shortwave radiation, see Eq. (17) in vU&H, 1985:
           RKST = (A1*SINPHI + A2)*(1.0 - B1*RNN**B2)*(1.0 - RALB)    
        ENDIF
      ENDIF

! *** Isothermal longwave radiation, L*_i, see Eq.(21) in vU&H,1985:
      RLSTI = -SIGMA*TM**4*(1.0 - C1*TM**2) + C2*RNN

! *** Isothermal net radiation, Q*_i, see Eq.(28) in vU&H,1985:
      QSTI = RKST + RLSTI

      RETURN

      end subroutine radiat
