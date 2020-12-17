! <heff.f90 - A component of the City-scale
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

      subroutine HEFF(ID,HS,TG,widt,diam,BH,BW,Z0,Z2,TA,vz2,UST,CL,HFL,XF,UD,      &
                     hmixin,HNEW,PS,SD,IDH)

!       The subroutine calculates effective emission height
!       2017 Matthias Karl, HZG, CityChem extension:
!            Substantial modification to couple with Sam Erik Walker's WORM
!            preprocessor WMPP to calculate wind speed at stack height, plume height
!            and final plume rise
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

!MSK start
      use mod_mete
!MSK end

      implicit none


      REAL BH
      REAL BW
      REAL CL
      REAL diam
      REAL HFL
      REAL hmixin
      REAL HNEW
      REAL HS
      REAL PS
      REAL SD
      REAL TA
      REAL TG
      REAL UD
      REAL UST
      REAL vz2
      REAL widt
      REAL XF
      REAL Z0
      REAL Z2

      INTEGER ID
      INTEGER IDH

! BH   - Height of building
! BW   - Width of building
! CL   - Monin-Obukhov length
! diam - Stack diameter
! HFL  - Emission height (final value)
! hmixin - Mixing height
! HNEW - Final height after penetration
! HS   - Stack height
! PS   - Degree of penetration
! SD   - Stack downwash height
! TA   - Air temperature
! TG   - Stack gas temperature
! UD   - Wind speed
! UST  - Friction velocity
! vz2  - Wind speed at height Z2
! W    - Stack emission velocity
! XF   - Final distance
! Z0   - Surface roughness
! Z2   - Height for windspeed V
! ID   - Calculated stack downwash indicator
! IDH  - Building cavity zone trap indicator

! Local variables

      REAL BL
      REAL DIFFHT
      REAL DH
      REAL DHB
      REAL DHB1
      REAL DHB2
      REAL DHM
      REAL DHP
      REAL F
      REAL HF
      REAL HP
      REAL RS
      REAL S
      REAL TESTHFL
      REAL TESTHP
      REAL TSD
      REAL TZ0
      REAL UP
      REAL US
      REAL ZP

!MSK from the old WSPROF() implementation:
!no-wmpp      REAL HS2(1), QHMW, TESTHP2(1), UD2(1), UP2(1), US2(1), ZP2(1)

!MSK start
      real :: lmo_inv
      real :: uz_min
      real :: ul
      real :: zul
!MSK end

      INTEGER IWASH
      INTEGER NITER

! BL      - Minimum of building height and width
! DIFFHT  - Test height difference
! DH      - Plume rise
! DHB     - Plume rise due to buoyancy
! DHB1    - Plume rise due to buoyancy
! DHB2    - Plume rise due to buoyancy
! DHM     - Plume rise due to momentum
! DHP     - Plume rise interpolation height
! F       - Plume rise variable
! HF      - Plume height accounting for stack downwash
! HP      - Plume rise height
! QHMW    - Plume rise variable
! RS      - Plume rise variable
! S       - Plume rise variable
! TESTHFL - Test height
! TESTHP  - Test height
! TSD     - Test stack downwash
! TZ0     - Test surface height
! UP      - Windspeed at plume height
! US      - Windspeed at stack height
! ZP      - Test height
! IWASH   - Stack downwash indicator
! NITER   - Number of iterations

! Initializations

      HFL   = 0.
      SD    = 0.
      IWASH = 0
!MSK start
      IDH   = 0
      ID    = 0
! Minimum wind speed >0.0 (used in function uz_f)
      uz_min=0.1
!MSK end

! Minimum for testing building influence

      BL = MIN(BH,BW)

!MSK start
!*** Calculate wind speed at stack height
!***   The EPISODE expression using WSPROF is replaced by the
!***   wind profile defined in S.E. Walker's WORM
!***
!no-wmpp      HS2(1) = HS
!no-wmpp
!no-wmpp      CALL WSPROF(Z0,-9900.,Z2,-9900.,V,UST,CL,1,1,HS2,US2)
!no-wmpp
!no-wmpp      HS = HS2(1)
!no-wmpp      US = US2(1)
!no-wmpp
!***
!***   First we define the wind profile of interest:
!        ul   - wind speed at lower height 
!        zul  - lower height of wind obs. (10m)
!        HS   - stack height
!        US   - from U10 to Stack Height HS
!        UP   - from U10 to Plume Height TESTHP
!     lmo_inv - Inverse Monin-Obukhov length (1/m)

      lmo_inv = 1./CL
      ul      = vz2 
      zul     = Z2
      US      = uz_f(HS,uz_min,Z2,ul,UST,lmo_inv)

!MSK end

! Test parameter for stack induced downwash

      TSD = widt/US

! Momentum rise for all stability classes

      DHM = 3.*diam*widt/US

! Interpolation between momentum rise and stack induced downwash

      IF (TSD .GT. 2.) THEN

! Full momentum

          SD = 0.
          HF = HS + DHM

      ELSE IF (TSD .LT. 1.) THEN

! Full stack downwash

          IWASH = 1
          SD = 2.*(widt/US - 1.5)*diam
          IF (ID .EQ. 1) SD = 0.
          HF = HS + SD

      ELSE

! Partial momentum and downwash, perform interpolation

          DHP = (7.*TSD - 8.)*diam
          IF (DHP .LT. 0.) THEN
              IWASH = 1
              IF (ID .EQ. 1) DHP = 0.
          ENDIF
          HF = HS + DHP

      ENDIF

! No downwash below ground

      IF (HF .LT. 0) HF = 0.

! Evaluation of building influence

      CALL BUILD(BH,BW,HS,vz2,HF,HP,DHM,IDH,IWASH)

! Define new test height
!MSK start
!MSK if TESTHP <= 20m, take wind speed at HS
      if (HP.ge.20.0) then
        TESTHP = HP
      else
        TESTHP = HS
      endif
!MSk end

      NITER = 0
  100 CONTINUE
      NITER = NITER + 1

!MSK start
! Wind speed at plume height
!***   The EPISODE expression using WSPROF is replaced by the
!***   wind profile defined in S.E. Walker's WORM
!***
!no-wmpp
!no-wmpp      TESTHP2(1) = TESTHP
!no-wmpp
!no-wmpp      CALL WSPROF(Z0,-9900.,Z2,-9900.,V,UST,CL,1,1,TESTHP2,UP2)
!no-wmpp
!no-wmpp      TESTHP = TESTHP2(1)
!no-wmpp      UP = UP2(1)
!no-wmpp
!***

       UP  = uz_f(TESTHP,uz_min,Z2,ul,UST,lmo_inv)

!MSK end

      IF (TG .GT. TA) THEN

! Bouyant plumes

          F = 9.81*widt*(diam/2.)**2*(TG - TA)/TG
!MSK          QHMW = 0.11*F

! Plume-rise method (Briggs,1969,1971,1975)

          IF (CL .LT. 0. .OR. CL .GT. 200.) THEN

! Unstable and neutral condition

! Bouyancy rise

              IF (F .LT. 55.) THEN
                  XF  = 0.049*F**(5./8.)
                  DHB = 21.425*F**(3./4.)/UP
              ELSE
                  XF  = 0.119*F**(2./5.)
                  DHB = 38.71*F**(3./5.)/UP
              ENDIF

          ELSE

! Stable condition

              RS = 0.02
              IF (CL .LE. 40.) RS = 0.035

! Calculate stability parameter

              S = 9.81*RS/TA

! Distance to final rise

              XF = 0.0020715*UP*S**(-1./2.)

! Buoyancy rise, calm conditions

              DHB1 = 4.*F**(1./4.)*S**(-3./8.)

! Buoyancy rise, wind

              DHB2 = 2.6*(F/(UP*S))**(1./3.)

! Choose the lower of the two values

              DHB = MIN(DHB1,DHB2)

          ENDIF

      ELSE

! Cold releases

          DHB = 0.

          IF (CL .LT. 0. .OR. CL .GT. 200.) THEN

! Momentum rise, stable for cold releases

              RS = 0.02

! Calculate stability parameter

              S = 9.81*RS/TA

! Calculate momentum rise

              DHM = 1.5*((widt*widt*diam*diam*TA)/(4.*TG*UP))**(1./3.)*S**(-1./6.)

          ENDIF

      ENDIF

! No momentum rise if stack downwash

      IF (IWASH .EQ. 1) DHM = 0.

! Choose the highest value of momentum and buoyancy rise

      DH = MAX(DHB,DHM)

! If momentum rise then zero distance to final height

      IF (DHM .GT. DHB) XF = 0.

! Final distance in meters

      XF = 1000.*XF

! Final plume rise

      IF (IDH .EQ. 3) THEN

! Trapped in the cavity-zone

          HFL = 0.5*BL
          XF  = 0.

      ELSE

          HFL = HP + DH

      ENDIF

!MSK start
      if (HFL .LT. 0.0) then
        print *, "module HEFF: hfl<0, HS HP HFL", HS, HP, HFL
        call STOPIT('HEFF: HFL < 0.0')
      endif  
!MSK end

! Iteration on plume rise

      DIFFHT  = ABS(TESTHP - HFL)
      TESTHFL = 0.05*HFL
      TESTHP  = HFL

      IF (IDH .NE. 3 .AND. DIFFHT .GT. TESTHFL .AND.      &
         NITER .LE. 100) GOTO 100

! Calculate influence of penetration

      CALL PENETR(HS,hmixin,HFL,IDH,HNEW,PS)

! Minimum height for windspeed calculation

      TZ0 = 10.*Z0
      IF (HNEW .LE. TZ0) HNEW = TZ0
      ZP = 0.5*(HS + HNEW)

!MSK start
!***   The EPISODE expression using WSPROF is replaced by the
!***   wind profile defined in S.E. Walker's WORM
!***
!no-wmpp
!no-wmpp     ZP2(1) = ZP
!no-wmpp
!no-wmpp     CALL WSPROF(Z0,-9900.,Z2,-9900.,V,UST,CL,1,1,ZP2,UD2)
!no-wmpp
!no-wmpp     ZP = ZP2(1)
!no-wmpp     UD = UD2(1)
!no-wmpp

       UD  = uz_f(ZP,uz_min,Z2,ul,UST,lmo_inv)
!MSK end

!MSK start debug
!MSK debug: Print calculated wind speeds at stack height (US),
!MSK debug: at plume height (UP), at final plume rise (UD)
!MSK debug     print *,'HEFF wind speeds US UP UD: ',US,UP,UD
!MSK end debug

      RETURN

! End of subroutine HEFF

      end subroutine heff

