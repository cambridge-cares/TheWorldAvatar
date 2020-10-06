! <cphotratej25.f90 - A component of the City-scale
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

      SUBROUTINE cphotratej25(phi,alon,day,SunHr,ZenAng,Cloudv)

!       2018 Matthias Karl, HZG, This is an original CityChem subroutine
!       The subroutine calculates the photolysis rates.
!       The Zenith angle (ZenAng) is calculated with equations from
!       Madronich. The formulae from Paltridge and Platt (1977) are
!       used to calculate the solar height angle as function of
!       date, daytime, latitude and longitude.
!       The parameterization for the calculation of photolysis rates is
!       adopted from Walker et al. (NILU TR 13/2003).
!
!       Paltridge, G.W. and C.M.R. Platt (1977):
!         Radiative processes in meteorology and climatology. 
!         Amsterdam‐Oxford‐New York, Elsevier Scientific Publishing Company, 1976. 
!         Pp. 318+xvii. 103.00 Dfl.
!       Walker, S.-E., Solberg, S., Denby, B. (2003): Development and 
!         implementation of a simplified EMEP photochemistry scheme for
!         urban areas in EPISODE, NILU TR 13/2003. ISBN 82-425-1524-7. 
!
!       All peroxides are treated in the same way as CH3O2H [DJ(16)].
!       The subroutine calculates the photolysis rates
!       jrmax and dj() defined in mod_emep03
!       The cloud correction for j(NO2) needs to be improved!
!*****************************************************************************!
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    24 Apr 2018  M. Karl: created subroutine cphotratej25
!
! ----------------------------------------------------------------------------------

      use mod_main
      use mod_time
      use mod_emep03, only : jrmax, dj
      use mod_emep03, only : OptAir
      
      real    :: phi
      real    :: alon
      integer :: day
      real    :: SunHr
      real    :: ZenAng
      real    :: Cloudv

! phi    - Latitude
! alon   - Longitude
! day    - Day of year
! SunHr  - Hour
! ZenAng - Zenith angle
! Cloudv - Cloud value (0-1)

      REAL Pi
      REAL Rad
      REAL FCT

      PARAMETER (Pi = 3.1415927)
      PARAMETER (Rad = Pi/180.0)
!      PARAMETER (FCT = 50.)

! Pi  - The mathematical constant Pi
! Rad - Degrees to radians

! Local variables

! Calculation of Zenith Angle
      double precision ::  FI
      double precision ::  D
      double precision ::  DEK
      double precision ::  H
      double precision ::  AH
      double precision ::  alpha

! Variables below should also be double precision

      REAL OptAM

! OptAM  - Optical air mass at actual zenith angle

! A and B values for reactions J1 - J16 at clear sky and 340 DU
! DJ(17) = DJ(3)*0.222

      REAL A(jrmax)
      REAL B(jrmax)
      REAL CLOUD1(jrmax)
      REAL CLOUD2(jrmax)
      REAL FAC(jrmax)

      real    :: jhour
      real    :: CLOUDF
      integer :: jday, jmon
      integer :: i

!TEST 14.05.2018: B(3)=0.2 (0.400)

      DATA (A(i),i = 1,jrmax)                                       &
!1-6
      /2.00E-04, 1.23E-03, 1.45E-02, 2.20E-05, 3.00E-06, 5.40E-05,  &
!7-12          
      6.65E-05, 1.35E-05, 2.43E-05, 2.70E-04, 9.72E-05, 5.40E-04,   &
!13-18
      3.53E-02, 8.94E-02, 3.32E-05, 2.27E-05, 3.22E-03, 2.27E-05,   &
!19-24
      2.27E-05, 2.27E-05, 2.27E-05, 2.27E-05, 2.27E-05, 2.27E-05,   &
!25
      2.27E-05/

      DATA (B(i),i = 1,jrmax)                                       &
!1-6
      /1.4,   0.600, 0.400, 0.750, 1.250, 0.790,                    &
!7-12     
      0.6,   0.940, 0.877, 0.790, 0.877, 0.790,                     &
!13-18
      0.081, 0.059, 0.567, 0.620, 0.400, 0.620,                     &
!19-24
      0.620, 0.620, 0.620, 0.620, 0.620, 0.620,                     & 
!25
      0.620/


      DATA CLOUD1/0.86,0.92,0.91,0.88,0.87,0.88,0.89,0.87,           &
                 0.92,0.92,0.92,0.92,0.92,0.92,0.88,0.88,0.91,       &
                 0.88,0.88,0.88,0.88,0.88,0.88,0.88,0.88/

      DATA CLOUD2/0.33,0.41,0.38,0.35,0.33,0.34,0.35,0.33,           &
                 0.41,0.40,0.41,0.41,0.42,0.42,0.35,0.35,0.38,       &
                 0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35/ 


! *** Initialization
         dj(:) = 0.
         FAC(:)= 0.
         OptAM = 0.
         CLOUDF= 0.


! *** Sun time correction for local time
! *** Convert to GMT. Assumes that SunHr = [1,24] !!!!

         jhour = SunHr + LTOGMT

! Last hour of previous day. Reduce JDAY by 1:
         if (jhour < 1) then
           jhour = 24 + jhour
           jday  = day - 1
           if (jday == 0) then
             jday = 365
           endif
! First hour of next day. Increase JDAY by 1:
         elseif (jhour > 24) then
           jhour = jhour - 24
           jday  = day + 1
!       Since the formula for calculating the solar height only
!       apply the expression: D=IMONTH(JMON) + JDAY  we only 
!       need to be concerned with the last day of the year.
           if (jday == 366) then
             if (.not.LEAP(year)) jday = 1
           endif
           if (jday == 367) then
             jday = 1
           endif
         else
           jday = day
         endif


!! MONO32 (Madronich)
! *** Calculate the Zenith angle (ZenAng)
!     FORMULAE FROM PALTRIDGE AND PLATT (1977)
!     CALCULATION OF SOLAR HEIGHT ANGLE
!     GIVEN DATE,TIME (UTC),LAT(deg N),LONG (deg E)

         FI=dble(phi)/57.3
         D = jday *6.2832/365.

! SOLAR FLUX S (W/M2)
!!!!     S=1368.*(1.00011+.034221*COS(D)+.00128*SIN(D)+.000719*COS(2*D))

! DECLINATION ANGLE DEK (RAD)
         DEK=0.006918-.399912*COS(D)+0.070257*SIN(D)-0.006758*COS(2*D)   + &
             0.000907*SIN(2*D)-0.002697*COS(3*D)+0.00148*SIN(3*D)

         H = jhour + dble(alon)*24/360.

         AH = COS(FI)*COS(DEK)*COS((H-12)*6.2832/24)+SIN(DEK)*SIN(FI)

         alpha = min(90.d0,90.d0-DASIN(AH)*57.3)

         ZenAng=real(nint(alpha))

         ! print *,'cphotrate ZenAng ',jday,jhour,AH,alpha,ZenAng

! *** End Calculate Zenith angle


         OptAM = OptAir(ZenAng)


! Cloud fraction goes from 0 to 0.8 (0 = Clear sky,0.8 = Overcast)
!MSK      CLOUDF = CLOUDV*0.8
!MSK cloud fraction input is in fraction already!
         CLOUDF = CLOUDV

      !print *,'emep45_new calcdj1',CLOUDV,CLOUDF,A(3),B(3),OptAM,FAC(3), DJ(3)

! Calculate the photodissociation values

         DO I = 1,jrmax

! Cloud correction

           IF (CLOUDF .LE. 0.2) THEN
             FAC(I) = (1.0 - CLOUDF/0.2) + CLOUD1(I)*CLOUDF/0.2
           ELSE
             FAC(I) = CLOUD1(I) + (CLOUDF - 0.2)*                             &
                      (CLOUD2(I) - CLOUD1(I))/0.6
           ENDIF

           DJ(I) = A(I)*EXP(-B(I)*OptAM)*FAC(I)

         ENDDO

          !print *,'cc70bio calcdj25',CLOUDV,CLOUDF,A(3),B(3),OptAM,FAC(3), DJ(3)

      RETURN



      end subroutine cphotratej25
