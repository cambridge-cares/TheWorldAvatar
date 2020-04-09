! <emep03.f90 - A component of the City-scale
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

      subroutine EMEP03(DTP,MSPEC,NSPEC,CP,DDEP,WDEP,                 &
                       SITELAV,SITELOV,IX,IY,IZ,IR,                   &
                       YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV,DAYYV,     &
                       TAIRV,DTDZV,RHUMV,                             &
                       PRESV,PRECV,CLOUV,TSRADV,MOBULV,USTARV,        &
                       DDEPVV,WDEPSRV,HINVD,HINVW,IDRY,ILAND)




! The subroutine performs photochemical calculations using the
! EMEP 03 compound photochemistry scheme for urban areas
!       2016 Matthias Karl, HZG, CityChem extension:
!            EMEP03 is called from GMPHOT.FOR only and therefore only 
!            available for photochemistry on the main grid.
!            The chemistry solver parts bdf03, dcdt03 and euler03 are
!            placed in include files. Function CalcDJ03 was moved to cphotrate
!            as part of EMEP45_NEW.Function RH2CONC was moved to mod_emep03
!            to be available for other chemistry mechanisms.
!            The subroutine BoxChem03 integrates the photochemistry equations
!            using a variable time step by using a combination of the
!            implicit Euler method and a BDF second order formulation
!            and by using the Gauss-Seidel iterative numerical method
!            for solving the non-linear (implicit) equations.
!            No changes were made to this.
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
!    03 May 2018  M. Karl: L598        DT2, prevent division by zero if ERRW=0
!    01 Mar 2018  M. Karl: L93         added TAIRV,DTDZV,RHUMV in the argument list
!
! ----------------------------------------------------------------------------------

      use mod_emep03
!MSK start
      use mod_conc
      use mod_mete    !PSIH(ETA)
      use mod_phot
!MSK end
      implicit none
 
      INTEGER MSPEC
      REAL DTP
!MSK      REAL CP(MSPEC)
!MSK      REAL GCCOR(MSPEC,0:2)
      double precision, dimension(MSPEC) :: CP
      REAL DDEP(MSPEC)
      REAL WDEP(MSPEC)
      REAL SITELAV
      REAL SITELOV
      REAL TAIRV
      REAL DTDZV
      REAL RHUMV
      REAL PRECV
      REAL CLOUV
      REAL MOBULV(0:1)
      REAL USTARV(0:1)
      REAL DDEPVV(MSPEC)
      REAL WDEPSRV(MSPEC)
      REAL HINVD
      REAL HINVW
!MSK start
      REAL PRESV
      REAL TSRADV
!MSK end

 
      INTEGER NSPEC
      INTEGER IX
      INTEGER IY
      INTEGER IZ
      INTEGER YEARV
      INTEGER MNTHV
      INTEGER DAYMV
      INTEGER HOURV
      INTEGER MINUV
      INTEGER SECOV
      INTEGER DAYYV
      INTEGER IDRY
      INTEGER ILAND
!MSK start
      double precision phidif
      integer NS,IC,IR

!MSK      real PSIH
!MSK end

! CP      - Concentration values (mol/cm3)
! GCCOR   - Ground concentration correction factors
! DDEP    - Dry deposition array
! WDEP    - Wet deposition array
! SITELAV - Latitude (deg)
! SITELOV - Longitude (deg)
! TAIRV   - Air temperature (K)
! DTDZV   - Vertical temperature gradient (K/m)
! RHUMV   - Relative humidity (0-1)
! PRECV   - Precipitation in mm/h
! CLOUV   - Cloud fraction (0-1) 
! HMIXV   - Mixing height value
! MOBULV  - Monin-Obukhov length scale
! USTARV  - Friction velocity
! DDEPVV  - Dry deposition velocities
! WDEPSRV - Wet deposition scavenging ratios
! MSPEC   - Maximum number of photochemistry compounds 
! NSPEC   - Actual  number of photochemistry compounds
! YEARV   - Year
! MNTHV   - Month
! DAYMV   - Day of month
! HOURV   - Hour
! MINUV   - Minute
! SECOV   - Second
! IDRY    - Dry deposition indicators
! ILAND   - Land/sea indicators (land=1/sea=0)

! External functions
!MSK now in mod_emep03
!MSK      REAL RH2CONC

! Local variables

!MSK      REAL ddA
!MSK      REAL ddC
      double precision ddA,ddC

      REAL H2O
      REAL M
      REAL SunHr
      REAL WASH
      REAL ZenAng

! H2O    - Water vapour concentration (cm-3)
! M      - Molecular density of air (molec/cm3)
! SunHr  - Hour
! WASH   - Wet deposition wash value
! ZenAng - Zenith angle

! Set molecular density of air (molec/cm3)

      M = 2.55E19

! Calculate water vapor concentration h2o (cm-3)

!MSK      H2O = RH2CONC(100.*RHUMV,TAIRV)
      H2O = RH2CONC(RHUMV,TAIRV)

! Set photochemistry reaction coefficients

      !PRINT *, 'EMEP03: Next action CALL CalcRC03.'

      CALL CalcRC03(M,H2O,TAIRV)

! Set dissociation reaction coefficients

      SunHr = HOURV + (60.*MINUV + SECOV)/3600.
!MSK start
      if (TSRADFE) then
          if (TSRADV.eq.0.0) then
             ! Nighttime cloud cover not relevant
             CLOUV = 1.0
          else
             CLOUV = cloudfraction(SunHr,DAYMV,SITELAV,TSRADV)
          endif
       endif
! Call new subroutine cphotrate
!MSK      CALL CalcDJ03(SITELAV,DAYYV,SunHr,ZenAng,CLOUV)
      call cphotrate(SITELAV,SITELOV,DAYYV,SunHr,ZenAng,CLOUV)
!MSK end

! Set dry deposition velocities

      IF (IDRY .GT. 0) THEN

!MSK start
!MSK        ddA = DDEPVV( 4)*HINVD
!MSK        ddC = DDEPVV( 8)*HINVD
         ddA = 0.0
         ddC = 0.0
         ddA = DBLE( DDEPVV( 1)*HINVD )
         ddC = DBLE( DDEPVV( 3)*HINVD )
!MSK end


!_CITYDELTA_START:
! CALCULATE EFFECTIVE DRY DEP BASED ON A LOG PROFILE at 1 m
! dd2=dd1/(1+dd1/k/ustar*(ln(z2/z1)-PHI2(z2/z1)+PHI1(z2/z1)))
! HINVD=1/dz1

!MSK   double precision phidif, ddA, ddB
         phidif=(  DBLE( ALOG((1.0/HINVD/2.0)/1.0) )                       &
              - PSIH( (1.0/DBLE( HINVD )/2.0)/DBLE( MOBULV(0) ) )    &
              + PSIH( 1.0/DBLE( MOBULV(0) ))  )

         ddA=ddA/(1.0+(ddA/DBLE( HINVD ))/0.4/DBLE( USTARV(0) )*phidif )
         ddC=ddC/(1.0+(ddC/DBLE( HINVD ))/0.4/DBLE( USTARV(0) )*phidif )

      ! print *,'emep03 ddA,HINVD,USTARV(0),phidif',ddA,HINVD,USTARV(0),phidif
      ! print *,'factor=',1./(1.0+(ddA/HINVD)/0.4/USTARV(0)*phidif)

!_CITYDELTA_END:

      ELSE

        ddA = 0.
        ddC = 0.

      ENDIF

! Set dry deposition reaction coefficients

      CALL DryDep03(ddA,ddC)

!MSK start 
!   Emissions are added in csubgm.for
!      CALL Emissions03(emisNOx)
!      RC(200) = emisNOx ! NO
!      RC(201) = 0.0     ! NO2
      RC(200) = 0.0
      RC(201) = 0.0
!MSK end

! Calculate EMEP model photochemistry

      !PRINT *, 'EMEP03: Next action CALL BoxChem03.'
 
      CALL BoxChem03(DTP,MSPEC,NSPEC,CP,M,H2O,IX,IY,IZ)

      RETURN

! End of subroutine EMEP03

      end subroutine emep03

      subroutine BoxChem03(DTP,MSPEC,NSPEC,CP,Mtot,H2Oconc,IX,IY,IZ)

! The subroutine integrates the photochemistry equations
! using a variable time step by using a combination of the
! implicit Euler method and a BDF second order formulation
! and by using the Gauss-Seidel iterative numerical method
! for solving the non-linear (implicit) equations.

      USE mod_emep03
!MSK start
      use mod_conc
!MSK end

      REAL DTP
      INTEGER MSPEC
      INTEGER NSPEC
!MSK      REAL CP(MSPEC)
      double precision, dimension(MSPEC) :: CP
      REAL Mtot
      REAL H2Oconc

      INTEGER IX
      INTEGER IY
      INTEGER IZ

! DTP     - Total integration time step (s)
! MSPEC   - Maximum number of species
! NSPEC   - Actual number of species
! CP      - Species concentration (mol/cm3)
! Mtot    - Total molecular air density (mol/cm3)
! H2Oconc - Water vapour concentration
! IX      - Main grid cell index in x-direction
! IY      - Main grid cell index in y-direction
! IZ      - Main grid cell index in z-direction

      INTEGER NrGSEul
      INTEGER NrGSBDF

! NrGSEul - Number of Gauss-Seidel iterations for the implicit Euler   method
! NrGSBDF - Number of Gauss-Seidel iterations for the BDF second order method

      INTEGER O3
      INTEGER NO
      INTEGER NO2

! Set compound indices

      PARAMETER (O3 =  1)
      PARAMETER (NO =  2)
      PARAMETER (NO2 = 3)
     
      INTEGER M
      INTEGER O2
      INTEGER H2O

      PARAMETER (M   = 4)
      PARAMETER (O2  = 5)
      PARAMETER (H2O = 6)

!MSK start
!      REAL F(MSPEC)
!      REAL X(MSPEC+3,0:2)
!      REAL W(MSPEC)
!      REAL ERRVEC(MSPEC)
!      REAL ERRW
      double precision F(MSPEC)
      double precision X(MSPEC+3,0:2)
      double precision W(MSPEC)
      double precision ERRVEC(MSPEC)
      double precision ERRW
!MSK end
      REAL ATOL(MSPEC)
      REAL RTOL(MSPEC)

      REAL ATOLF
      REAL DTMin
      REAl DTMax
      REAL DTLeft
!MSK      REAL DT1
!MSK      REAL DT2
      double precision DT1
      double precision DT2
      REAL RDT
      REAL A1
      REAL A2
      REAL A3
      REAL A4
!MSK start
!      REAl Xmin
!      REAL P
!      REAL L
      double precision P,L
      double precision Xmin
!MSK end

      INTEGER INITV
      INTEGER NFAIL

!MSK start
      INTEGER IC,NS,I
      REAL Ldt
!MSK end

! If zero timestep then we need to do nothing

      IF (DTP .LE. 0.) RETURN

! Set number of Gauss-Seidel iterations of the
! implicit Euler and the BDF second order method

      NrGSEul = 2
      NrGSBDF = 2

! Set absolute (mol/cm3) and relative (%/100) error
! tolerances

      DO NS = 1,NSPEC
        RTOL(NS) = 0.10
      ENDDO

      ATOLF = 1.0

!MSK start
!MSK      ATOL( 4) = 5.0E10*ATOLF  ! O3 []
!MSK      ATOL( 7) = 2.5E08*ATOLF  ! NO []
!MSK      ATOL( 8) = 2.5E09*ATOLF  ! NO2 []
      ATOL( 1 ) = 5.0E10*ATOLF  ! O3 []
      ATOL( 2 ) = 2.5E08*ATOLF  ! NO []
      ATOL( 3 ) = 2.5E09*ATOLF  ! NO2 []
!MSK end

! Set minimum and maximum time step (s)

      DTMin = 0.1
      DTMax = DTP

! Set initial concentrations

      DO NS = 1,NSPEC
        X(NS,1) = CP(NS)
!MSK start set small conc to zero
        if ( X(NS,1) .lt. 1.e-20 )   X(NS,1) = 0.0
!MSK end
      ENDDO

! Setting some extra concentrations used by the
! dc/dt, implicit Euler and 2nd order BDF formulations

      X(H2O,1) = H2Oconc
      X(M  ,1) = Mtot
      X(O2 ,1) = 0.2095*Mtot

      X(H2O,2) = H2Oconc
      X(M  ,2) = Mtot
      X(O2 ,2) = 0.2095*Mtot

! Set time step left initially

      DTLeft = DTP

! Main total time step loop

  100 CONTINUE

! Set number of failures to zero

      NFAIL = 0

! Set initialization (or restart) indicator

      INITV = 1

! Calculate W-vector

      DO NS = 1,NSPEC
        W(NS) = ATOL(NS) + RTOL(NS)*X(NS,1)
      ENDDO

! Calculate the tendency vector F = dc/dt
!MSK start initialize tendency vector
      DO NS = 1,NSPEC
        F(NS) = 0.
      ENDDO
!MSK end

      !print *,'emep03: P,L ', DJ(3),X(NO2,1),RC(5), X(NO,1), RC(160)


      INCLUDE 'dcdt03.inc'

! Calculate first time step

      DT1 = DTLeft

      DO NS = 1,NSPEC

!MSK start
        if ( ABS(F(NS)) .lt. 1.e-28 )    F(NS) = 0. 
!MSK end

        IF (F(NS) .NE. 0.) THEN

           DT1 = MIN( DT1,W(NS) / ABS(real(F(NS))) )

        ENDIF
      ENDDO

! Reduce first time step if larger than what is left
! of total time step

      IF (DT1 .GT. DTLeft) THEN
        DT1 = DTLeft
      ENDIF

! Reduce first time step if larger than maximum set

      IF (DT1 .GT. DTMax) THEN
        DT1 = DTMax
      ENDIF

! Increase first time step if smaller than minimum set

      IF (DT1 .LT. DTMin) THEN
        DT1 = DTMin
      ENDIF
 
! OK, now we have the first time step

! Set initial iterate

      DO NS = 1,NSPEC
        X(NS,2) = X(NS,1)
      ENDDO


! Integrate the differential equation over the first time step
! by using the implicit Euler equation and by using a number
! of iterations with the Gauss-Seidel iterative technique

      Ldt = DT1

!MSK start
      if ( ABS(X(NS,2)) .lt. 1.e-28 )  X(NS,2) = 0. 
!MSK end

      !print *,'emep03: before include euler03.f, Ldt', Ldt
 
      DO I = 1,NrGSEul
        INCLUDE 'euler03.inc'
      ENDDO


! OK, found new accepted concentrations

! Update the time step left

      DTLeft = DTLeft - DT1

! Are we finished?

      IF (DTLeft .LT. DTMin) GOTO 999

! No!

  200 CONTINUE

! Tentatively set second time step equal to first

      IF (INITV .EQ. 1) THEN
        DT2 = DT1
      ENDIF

! Reduce second time step if larger than what is left
! of total time step

      IF (DT2 .GT. DTLeft) THEN
        DT2 = DTLeft
      ENDIF

! Reduce second time step if larger than maximum set

      IF (DT2 .GT. DTMax) THEN
        DT2 = DTMax
      ENDIF

! Increase second time step if smaller than minimum set

      IF (DT2 .LT. DTMin) THEN
        DT2 = DTMin
      ENDIF
 
! OK, now we have the second time step

! Update (shift) the concentration array

      IF (NFAIL .EQ. 0) THEN

      DO NS = 1,NSPEC
        X(NS,0) = X(NS,1)
        X(NS,1) = X(NS,2)
      ENDDO

      ENDIF

! Calculate coefficients used by the BDF two-step
! formulation for variable time step

      RDT = DT1/DT2

      A1 = (RDT + 1.)*((RDT + 1.)/RDT)
      A2 = -1./RDT
      A3 = RDT + 1.
      A4 = RDT + 2.

! Calculate initial iterate by simple extrapolation

      DO NS = 1,NSPEC
        X(NS,2) = X(NS,1) + (X(NS,1) - X(NS,0))/RDT
      ENDDO

! Check initial iterate

      DO NS = 1,NSPEC

! If negative concentrations then restart the whole integration
! procedure by using the implicit Euler method which is guaranteed
! to be positive

        IF (X(NS,2) .LT. 0.) GOTO 100

      ENDDO

! Integrate the differential equation over the second time step
! by using the second order BDF equation and by using a number
! of iterations with the Gauss-Seidel iterative technique

      Ldt = DT2

     !print *,'emep03: before include bdf03.f' 
     DO I = 1,NrGSBDF
        INCLUDE 'bdf03.inc'
      ENDDO
     !print *,'emep03: before after bdf03.f' 

! OK, found new concentrations

      !print *,'emep03 xnew NO2 ', X(1,2)

! Find smallest value

      Xmin = X(1,2)
      DO NS = 2,NSPEC
!MSK       Xmin = MIN(Xmin,real( X(NS,2) ))
        Xmin = MIN(Xmin, X(NS,2) )
      ENDDO

! If negative concentrations then restart the whole integration
! procedure by using the implicit Euler method which is guaranteed
! to be positive

      IF (Xmin .LT. 0.) GOTO 100

! Calculate error vector

      DO NS = 1,NSPEC
        ERRVEC(NS) = (2./(RDT*(RDT+1)))*   &
                    (RDT*X(NS,2) - (1. + RDT)*X(NS,1) - X(NS,0))
      ENDDO

! Calculate W-vector

      DO NS = 1,NSPEC
        W(NS) = ATOL(NS) + RTOL(NS)*X(NS,1)
      ENDDO

! Calculate weighted error norm

      ERRW = 0.

      DO NS = 1,NSPEC
!MSK start
        W(NS) = max(W(NS),dble(1.e-28))
!MSK end
        IF (ABS(ERRVEC(NS))/W(NS) .GT. ERRW) THEN
            ERRW = ABS(ERRVEC(NS))/W(NS)
        ENDIF
      ENDDO

! Do we accept this second time step?

      IF (ERRW .LT. 1.0) THEN

! Yes!

! Update the time step left

        DTLeft = DTLeft - DT2

! Are we finished?

        IF (DTLeft .LT. DTMin) GOTO 999

! Calculate new first and second time step

        DT1 = DT2

        if (ERRW.eq.0.0) then
           DT2 = dble(2.0)*DT2
        else
           DT2 = MAX(dble(0.5),MIN(dble(2.0),dble(0.8)/SQRT(ERRW)))*DT2
        endif

! Set initialization (restart) indicator and number of
! failures to zero and go up and perform a new second
! order BDF integration

        INITV = 0
        NFAIL = 0
        GOTO 200

      ELSE

! No!

! Number of failures increased by 1

        NFAIL = NFAIL + 1

! If two failures then restart the whole integration
! procedure by using the implicit Euler method

        IF (NFAIL .EQ. 2) GOTO 100

! Calculate new second time step

        DT2 = MAX(dble(0.5),MIN(dble(2.0),dble(0.8)/SQRT(ERRW)))*DT2

! Set initialization (restart) indicator to zero and
! go up and try a new second order BDF integration

        INITV = 0
        GOTO 200

      ENDIF

! No!

  999 CONTINUE

! Set new concentrations

      DO NS = 1,NSPEC
!MSK start
!MSK        CP(NS) = X(NS,2)
        if ( X(NS,2) .lt. 1.e-28 )  X(NS,2) = 0. 
        CP(NS) = real( X(NS,2) )
!MSK end
      ENDDO

      RETURN

! End of subroutine BoxChem03

      end subroutine BoxChem03

      SUBROUTINE CalcRC03(M,H2O,T)

! The subroutine calculates the photochemistry reaction rates

      USE mod_emep03

!  Total molecular density

      REAL M
      REAL H2O
      REAL T

! M   - Molecular air density (mol/cm3)
! H2O - Water vapour concentration (1% V/V)
! T   - Temperature (K)

! Local variables

      REAL CM
      INTEGER I

 
      DO I = 1,rcmax
        RC(I) = 0.0
      ENDDO

      CM = M*1.E-30

! Inorganic chemistry
      RC(5) = 1.4E-12*EXP(-1310./T) ! IUPAC, 2001

      RETURN

! End of subroutine CalcRC03

      END

! *********************************************************************
!MSK commented subroutine CalcDJ03, now in cphotrate.f90
!      SUBROUTINE CalcDJ03(phi,day,SunHr,ZenAng,Cloudv)
!
! ! The subroutine calculates the photolysis rates
!
!      USE mod_main
!      USE mod_emep03
!
!      REAL phi
!      INTEGER day
!      REAL SunHr
!      REAL ZenAng
!      REAL Cloudv
!
! ! phi    - Latitude
! ! day    - Day of year
! ! SunHr  - Hour
! ! ZenAng - Zenith angle
! ! Cloudv - Cloud value (0-1)
!
!      REAL Pi
!      REAL Rad
!      REAL FCT
!
!      PARAMETER (Pi = 3.1415927)
!      PARAMETER (Rad = Pi/180.0)
!      PARAMETER (FCT = 50.)
!
! ! Pi  - The mathematical constant Pi
! ! Rad - Degrees to radians
!
! ! Local variables
!
!      REAL CosZen
!      REAL DecAng
!      REAL OptAM
!
! ! CosZen - Cosine of zenith angle
! ! DecAng - Declination angle
! ! OptAM  - Optical air mass at actual zenith angle
!
! ! A and B values for reactions J1 - J16 at clear sky and 340 DU
! ! DJ(17) = DJ(3)*0.222
!
!      REAL A(17)
!      REAL B(17)
!      REAL CLOUD1(17)
!      REAL CLOUD2(17)
!      REAL FAC(17)
!
!      real    :: CLOUDF
!      real    :: DN
!      integer :: i
!
!      DATA (A(i),i = 1,17)                                           &
!     /2.00E-04, 1.23E-03, 1.45E-02, 2.20E-05, 3.00E-06, 5.40E-05,   & 
!      6.65E-05, 1.35E-05, 2.43E-05, 2.70E-04, 9.72E-05, 5.40E-04,   &
!      3.53E-02, 8.94E-02, 3.32E-05, 2.27E-05, 3.22E-03/
!
!      DATA (B(i),i = 1,17)                                           &
!     /1.4,   0.600, 0.400, 0.750, 1.250, 0.790,                     &
!      0.6,   0.940, 0.877, 0.790, 0.877, 0.790,                     &
!      0.081, 0.059, 0.567, 0.620, 0.400/
!
!      DATA CLOUD1/0.86,0.92,0.91,0.88,0.87,0.88,0.89,0.87,           &
!                 0.92,0.92,0.92,0.92,0.92,0.92,0.88,0.88,0.91/
!
!      DATA CLOUD2/0.33,0.41,0.38,0.35,0.33,0.34,0.35,0.33,           &
!                 0.41,0.40,0.41,0.41,0.42,0.42,0.35,0.35,0.38/ 
!
! ! Initialization
!      dj(:) = 0.
!      FAC(:)= 0.
!      OptAM = 0.
!      CLOUDF= 0.
!
! ! Calculate the Zenith angle
!
!      DecAng = 23.27*COS(2.0*Pi*(day - 172.0)/365.0)
!      CosZen = COS(Rad*phi)*COS(Rad*DecAng)*                        &
!              COS(2.0*Pi*(SunHr - 12.0)/24.0) +                    &
!              SIN(Rad*phi)*SIN(Rad*DecAng)
!
!      ZenAng = ACos(MAX(0.017,CosZen))/Rad
!
!      OptAM = OptAir(ZenAng)
!
! !MSK DN is a day/night switch to set photolyses to about 0 at night
!      DN = exp(FCT*CosZen)/(exp(-FCT*CosZen)+exp(FCT*CosZen))
!
! ! Cloud fraction goes from 0 to 0.8 (0 = Clear sky,0.8 = Overcast)
! !MSK      CLOUDF = CLOUDV*0.8
! !MSK cloud fraction input is in fraction already!
!      CLOUDF = CLOUDV
!
! ! Calculate the photodissociation values
!
!      DO I = 1,17
!
! ! Cloud correction
!
!        IF (CLOUDF .LE. 0.2) THEN
!          FAC(I) = (1.0 - CLOUDF/0.2) + CLOUD1(I)*CLOUDF/0.2
!        ELSE
!          FAC(I) = CLOUD1(I) + (CLOUDF - 0.2)*                             &
!                  (CLOUD2(I) - CLOUD1(I))/0.6
!        ENDIF
!
! !MSK        DJ(I) = A(I)*EXP(-B(I)*OptAM)*FAC(I)
!        DJ(I) = DN * A(I)*EXP(-B(I)*OptAM)*FAC(I)
!
!      ENDDO
!
!      RETURN
!
! ! End of subroutine CalcDJ03
!      END

      SUBROUTINE Emissions03(emisNOx)

! The subroutine sets the emission reaction coefficients

      USE mod_emep03

! Scalar arguments

      REAL emisNOx

! Local variables

      INTEGER I

      DO I = 200,220
        RC(I) = 0.0
      ENDDO

      RC(200) = emisNOx ! NO
      RC(201) = 0.0     ! NO2

      RETURN

! End of subroutine Emissions03

      END

      SUBROUTINE DryDep03(ddA,ddC)

! The subroutine sets dry deposition reaction rates (s-1)

      USE mod_emep03

!MSK      REAL ddA
!MSK      REAL ddC
      double precision ddA
      double precision ddC

! Set dry deposition reaction rates (s-1)

      RC(160) = ddA ! O3
      RC(162) = ddC ! NO2

      RETURN

! End of subroutine DryDep03

      END

! *********************************************************************
!MSK commented function OptAir, now in mod_emep03.for
!      REAL FUNCTION OptAir(ZenAng)
!
! The subroutine calculates the optical air mass as a function of
! zenith angle
!
!      REAL ZenAng
!
! ZenAng - Zenith angle
!
!      REAL Pi
!      REAL Rad
!      integer i
!
!      PARAMETER (Pi = 3.1415927)
!      PARAMETER (Rad = Pi/180.0)
!
! Pi  - The mathematical constant Pi
! Rad - Degrees to radians
!
! Values of the optical air mass for ZenAng = 60-89
!
!      REAL OptA(60:89)
!
!      DATA (OptA(i), i = 60, 89) &
!       /2.00, 2.06, 2.12, 2.20, 2.27, 2.36, 2.45, 2.55, 2.65, 2.78, &
!        2.90, 3.06, 3.21, 3.40, 3.59, 3.83, 4.07, 4.40, 4.72, 5.16, &
!        5.60, 6.18, 6.88, 7.77, 8.90,10.39,12.44,15.36,19.79,26.96/  
!
!      IF (ZenAng .LT. 60.) THEN
!        OptAir = 1./COS(Rad*ZenAng)
!      ELSEIF (ZenAng .LT. 89.) THEN
!        OptAir = OptA(INT(ZenAng))*(INT(ZenAng) + 1 - ZenAng) +  &
!                OptA(INT(ZenAng) + 1)*(ZenAng - INT(ZenAng))
!      ELSE 
!        OptAir = OptA(89)
!      ENDIF
!
!      RETURN
!
! End of subroutine OptAir
!
!      END
!
!MSK commented function RH2CONC, now in mod_emep03.for
! *********************************************************************
!
!      REAL FUNCTION RH2CONC(RH,T)
!
! The subroutine calculates the concentration of water vapor (molec/cm3)
!C from the relative humidity and temperature
!
! RH - Relative humidity (%)
! T  - Temperature (K)
!      real XZ,ESAT,H2O,RH,T
!
!      XZ   = (597.3 - 0.57*(T - 273.16))*18./1.986*(1./T - 1./273.16)
!      ESAT = 6.1078*EXP(-XZ)
!      H2O  = RH*ESAT/(1.38E-17*T)
!
!      RH2CONC = H2O
!
!      RETURN
!
! End of subroutine RH2CONC
!
!      END
!
!MSK commented function PSIH, use the general one:
!MSK double precision function PSIH in mod_mete.for
! *********************************************************************
!MSK
!MSK      REAL FUNCTION PSIH(ETA)
!MSK
!MSK!   -----------------------------------------------------------------
!MSK!         STABILITY CORRECTION FUNCTION IN THE SURFACE LAYER
!MSK!         TEMPERATURE PROFILE
!MSK!
!MSK!         INPUT:
!MSK!                 ETA   : STABILITY PARAMETER Z/L
!MSK!         OUTPUT:
!MSK!                 PSIH : CORRECTION IN LOGARITHMIC TEMPERATURE PROFILE
!MSK!
!MSK!         THE PRESENT MODEL IS AN EMPIRICAL FIT BY HOLTSLAG AND
!MSK!         DE BRUIN(1987, ......................................)
!MSK!         OF DATA BY HICKS(1976, QUART. J. R. METEOR. SOC., 102,
!MSK!         535-551); SEE ALSO HOLTSLAG(1984, BLM, 29, 225-250)
!MSK!  ------------------------------------------------------------------
!MSK
!MSK      REAL ETA
!MSK
!MSK! Local variables
!MSK
!MSK      REAL Y
!MSK
!MSK! Y - Dummy variable
!MSK
!MSK      IF (ETA .LT. 0.) THEN
!MSK         Y    = SQRT(1 - 16.*ETA)
!MSK         PSIH = 2*ALOG((1 + Y)/2)
!MSK      ELSE
!MSK!_LHS:   Below is the expression of Eq. (2.1.26) from COST 710 Book.         
!MSK         PSIH = - ( (1.0 + 0.6667*ETA)**(1.5) + (0.667*ETA - 9.529)*EXP(-0.35*ETA) + 8.529)
!MSK
!MSK      ENDIF
!MSK
!MSK      RETURN
!MSK! End of real function PSIH
!MSK
!MSK      END FUNCTION PSIH
! *********************************************************************
