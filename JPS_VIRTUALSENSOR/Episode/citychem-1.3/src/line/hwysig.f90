! <hwysig.f90 - A component of the City-scale
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

!MSK      subroutine HWYSIG (X,XY,KST,U,SIGY,SIGZ)
      subroutine HWYSIG (X,XY,KST,U,building_height,canyon,SIGY,SIGZ)
!                    SUBROUTINE HWYSIG (VERSION 80090),
!                     PART OF HIWAY2 AND HIWAY2I.
!       2017 Matthias Karl, HZG, CityChem extension:
!            Simplified Street Canyon Model
!         Street Canyon Dispersion Parameters:
!         adopted from Kono&Ito (AE 1990, part I)
!         Kono, H. and Ito, S. (1990): A micro-scale dispersion model for motor
!           vehicle exhaust gas in urban areas - OMG Volume-Source Model.
!           Atmospheric Environment, Vol. 24B (2), 243-251.
!         and from Kono&Ito (AE 1990, part II)
!         Kono, H. and Ito, S. (1990): A comparison of concentration estimates
!           by the OMG Volume-Source Model with three line source dispersion models
!           Atmospheric Environment, Vol. 24B (2), 253-260.
!         This modification is not used for now!
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

!MSK      DIMENSION SPGZ(3), SPGY(3)
!MSK start 26.09.2017
      implicit none
!MSK end 26.09.2017
      real             :: building_height
      real             :: FY
      real             :: XM
      logical          :: canyon
      REAL SPGZ(3), SPGY(3)
      REAL X,XY,U,SIGY,SIGZ
      REAL SIGYA,SIGYO,SIGZA,SIGZO
      REAL SZO,SZON,SZON_MIN,SZON_MAX
      REAL SYO,SYON,SYON_MIN,SYON_MAX
      REAL U_MIN,U_MAX,TH,XP
      INTEGER KST  
!MSK end
      DATA SPGZ /30.144,12.093,8.698/
      DATA SPGY /52.203,22.612,16.895/
      IF (X.EQ.0.) X=0.0001
      XP=X

!_LHS_Change_19Jan2012_Start:

!_ORG      SYON = 3.0
!_ORG      SZON = 1.5

! *** NOTE that the recommendation from the "User's Guide for HIWAY-2"
! *** is to set: SYON_MIN =  3.0  and  SZON_MIN = 1.5 for U > 3.0 m/s
! ***       and  SYON_MAX = 10.0  and  SZON_MAX = 5.0 for U < 1.0 m/s
! *** with linear interpolation in between.

      SYON_MIN = 3.0
      SYON_MAX = 10.0
      
      SZON_MIN = 1.5
      SZON_MAX = 5.0
      
      U_MAX    = 3.0
      U_MIN    = 1.0 

      IF (U > U_MAX ) THEN
        SYON = SYON_MIN
        SZON = SZON_MIN
      ELSEIF (U < U_MIN ) THEN
        SYON = SYON_MAX
        SZON = SZON_MAX
      ELSE
        SYON = (SYON_MAX - SYON_MIN)*((U - U_MIN)/(U_MAX - U_MIN))
        SYON = SYON_MAX - SYON
        SZON = (SZON_MAX - SZON_MIN)*((U - U_MIN)/(U_MAX - U_MIN))
        SZON = SZON_MAX - SZON        
      ENDIF
      
!_LHS_Change_10Jan2012_End.

      SIGZO=SZON                                                       
      SIGYO=SYON
!BRUCE: test
      !SIGZO=3.0                                                     

!       IF X.LE. 300 METERS USE CURVES AS GIVEN IN THIS SUBROUTINE.
!       IF X.GT. 300 METERS THEN DBTSIG IS CALLED TO COMPUTE
!       THE SIGMAS USING THE DISPERSION AT 300 METERS DUE TO
!       THE ROADWAY AS THE INITIAL DISPERSION.
      IF (X.GT.0.3) X=0.3

!MSK start
!MSK First calculate the initial spread of the traffic plume dispersion
!MSK The initial vertical dispersion depends directly on 
!MSK building_height (Kono&Ito AE 1990, part I).
!MSK See Fig. 1 in Kono&Ito (AE 1990, part II).
!MSK Overwrites the wind-dependence of SIGZO and SIGYO
!MSK defined by LHS_Change_10Jan2012.
!MSK For now this is the only change for street canyon
      if (canyon) then

         SIGZO = 0.8 * building_height
         SIGYO = 2.0 * SIGZO
!MSK         XM    = 1000.0 * X
!MSK Distance XM in meter (X is in km)
      endif

!MSK BELOW IS NOT USED FOR NOW (18.07.2017)
!MSK Calculate SIGZA according to equation (15) in
!MSK Kono&Ito (AE 1990, part I) and the empirical parameters for different
!MSK atmospheric stability given in Table 3 therein.
!MSK For urban area it is assumed: n = 1.
!MSK
!MSK First case: wind speed > 1 m/s
!MSK         if (U.gt.1.0) then
!MSK            IF (KST.LE.3)  SIGZA = SQRT(0.76*0.1*XM*XM)
!MSK            IF (KST.EQ.4)  SIGZA = SQRT(0.27*0.1*XM*XM)
!MSK            IF (KST.GE.5)  SIGZA = SQRT(0.17*0.1*XM*XM)
!MSK         else
!MSK Second case: calm conditions
!MSK            IF (KST.LE.3)  SIGZA = SQRT(1.60*0.1*XM*XM/U)
!MSK            IF (KST.EQ.4)  SIGZA = SQRT(0.33*0.1*XM*XM/U)
!MSK            IF (KST.GE.5)  SIGZA = SQRT(0.31*0.1*XM*XM/U)
!MSK         endif
!MSK         SIGZ=SQRT(SIGZA*SIGZA+SIGZO*SIGZO)
!MSK Calculate SIGYA according to equation (15) in Kono&Ito
!MSK (AE, 1990, part I) and the empirical parameters for different
!MSK atmospheric stability given in Table 3 therein.
!MSK
!MSK Universal function FY from Irwin (1979).
!MSK         FY = 1.0/(1.0+0.031*XM**(0.46))
!MSK First case: wind speed > 1 m/s
!MSK         if (U.gt.1.0) then
!MSK            IF (KST.LE.3)   SIGYA = 0.59*XM*FY
!MSK            IF (KST.EQ.4)   SIGYA = 0.43*XM*FY
!MSK            IF (KST.GE.5)   SIGYA = 0.39*XM*FY
!MSK         else
!MSK Second case: calm conditions
!MSK            SIGYA = 0.56*U**(-0.82)*XM*FY
!MSK         endif
!MSK         SIGY=SQRT(SIGYA*SIGYA+SIGYO*SIGYO)
!MSK not in canyon
!MSK      else
!MSK end

!MSK standard treatment for open roads
         IF (KST.LE.3) SIGZA=110.62*(X**0.93198)
         IF (KST.EQ.4) SIGZA=86.49*(X**0.92332)
         IF (KST.GE.5) SIGZA=61.141*(X**0.91465)
         SIGZ=SQRT(SIGZA*SIGZA+SIGZO*SIGZO)
         IF (KST.LE.3) TH=(18.333-1.8096*ALOG(X))/57.2958
         IF (KST.EQ.4) TH=(14.333-1.7706*ALOG(X))/57.2958
         IF (KST.GE.5) TH=(12.5-1.0857*ALOG(X))/57.2958
         SIGYA=1000.*X*SIN(TH)/(2.15*COS(TH))
         SIGY=SQRT(SIGYA*SIGYA+SIGYO*SIGYO)

      IF (XP.LE.0.3) RETURN
      IF (KST.GE.5) GO TO 20
      IF (KST.EQ.4) GO TO 10

!     SZO and SYO are the initial dispersion due to the roadway.

      SZO=SQRT(SIGZ*SIGZ-SPGZ(1)*SPGZ(1))
      SYO=SQRT(SIGY*SIGY-SPGY(1)*SPGY(1))
      call dbtsig (XP,XP,2,SIGY,SIGZ)
      SIGZ=SQRT(SIGZ*SIGZ+SZO*SZO)
      SIGY=SQRT(SIGY*SIGY+SYO*SYO)
      RETURN
10    CONTINUE
      SZO=SQRT(SIGZ*SIGZ-SPGZ(2)*SPGZ(2))
      SYO=SQRT(SIGY*SIGY-SPGY(2)*SPGY(2))
      call dbtsig (XP,XP,4,SIGY,SIGZ)
      SIGZ=SQRT(SIGZ*SIGZ+SZO*SZO)
      SIGY=SQRT(SIGY*SIGY+SYO*SYO)
      RETURN
20    CONTINUE
      SZO=SQRT(SIGZ*SIGZ-SPGZ(3)*SPGZ(3))
      SYO=SQRT(SIGY*SIGY-SPGY(3)*SPGY(3))
      call dbtsig (XP,XP,5,SIGY,SIGZ)
      SIGZ=SQRT(SIGZ*SIGZ+SZO*SZO)
      SIGY=SQRT(SIGY*SIGY+SYO*SYO)
      RETURN

      end subroutine hwysig
