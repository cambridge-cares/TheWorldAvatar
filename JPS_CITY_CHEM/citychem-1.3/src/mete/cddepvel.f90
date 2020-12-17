! <cddepvel.f90 - A component of the City-scale
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

      subroutine CDDEPVEL

!     This subroutine calculates Dry-Deposition Velocities based on
!     the usual aerodynamic-, quasi-laminar- and surface- resistance 
!     model concept. For particles the gravitational settling
!     velocity is also included.
! 
!     The Land-Use categories can change from hour to hour
!     and therefore all the Dry-deposition resistances can change
!     accordingly.
!
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
!           2016  M. Karl: Moved function PSIH into mod_mete.f90
!           2017  M. Karl: Modified calculation of Rb_v. Set Rc_v to unity
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo

        implicit none

! External functions

!MSK      REAL PSIH     (is in mod_mete)

!     Local variables:

      integer :: IC,IX,IY,method_part,Rb_method

      real :: z0v,ustarv,mobulv,hmixv,PH

      real :: zref,t_zref,p_zref,ny_air,my_air,rho_air,pr, &
      lambda_air


      real :: rho_p,D_p,Kn,C_c,diff_p,z0_p
      real :: sig_g,lnsig2,Vs_scal,diff_p_scal

      real :: Vs_v,Ra_v,Rb_v,Rc_v,V_drydepv

!MSK start 17.10.2017
      real :: MOLW, DC, M_air, patm
      real :: DIFF_g, z0_g, Pr_g, Sc_g
      real :: topv
!MSK end


!     z0v    - Surface roughness value (for momentum)
!     ustarv - Friction velocity value.
!     mobulv - Monin-Obukhov length value.
!     hmixv  - Mixing height (or PBL height) value.
!     PH     - Psi-h related function value for profile calculation.
!     zref   - Reference height (usually mid-point of first layer)
!
!     t_zref     - Air Temperature (in Celsius) at height zref.
!     p_zref     - Air Pressure (in mb) at height zref.
!     ny_air     - Kinematic viscosity of air.  (m^2 s^-1)
!     my_air     - Dynamic viscosity of air     (kg m^-1 s^-1)
!     rho_air    - (Dry) air density            (kg m^-3)
!     pr         - Moelcular Prandtl number     (dimless)
!     lambda_air - Free molecular path of air   (m)
!
!     diff_g  - Molecular diffusivity of gaseous compound.
!
!     rho_p   - Particle density                    (kg m^-3)
!     D_p     - Particle diameter (given in micrometers = 10^-6 m).
!     Kn      - Non-dimensional Knudsen number (2*Lamda_air/D_p).     
!     C_c     - The Cunningham Slip Flow Correction factor (Dimless).
!     diff_p  - Molecular diffusivity for particles (m^2 s^-1).
!     z0_p    - Roughness height for particles      (m)
!
!     Vs_v    - The Gravitational Settling Velocity (m/s)
!     Ra_v    - The Aerodynamic resistance          (s/m)
!     Rb_v    - The Quasi-Laminar resistance        (s/m)
!     Rc_v    - The Surface (or Canopy) resistance  (s/m)
!
!     V_drydepv - The Dry-Deposition Velocity       (m/s)
!
! --------------------------------------------------------------------
!
!     The reference height is defined as the midpoint of layer 1
!     NB: The height should be adjusted for the coordinate transform.

      zref = DZ(1)/2.
  
      p_zref = 1013.0   ! Reference pressure (in mb) at the zref 
!                       ! height.  Should be read from file.

      rho_p  = 1000.0   ! Particle density kg m^-3.
      
      M_air  = 28.970   ! molar mass of dry air [g/mol]
      patm   = 1.0      ! pressure in atm
      Pr_g   = 0.72     ! Prandtl number

! --------------------------------------------------------------------

!MSK start
      if (.not. allocated(DDEPV2D)) allocate (DDEPV2D(NC,NX,NY))
      if (.not. allocated(AERO))    allocate (AERO(NX,NY))
!MSK end

!     Go through all main gridcells

      DO IY = 1,NY
      DO IX = 1,NX

!       Find reference temperature (in Celsius):

        t_zref  = TAIR(IX,IY)
        ustarv  = USTAR(IX,IY)
        z0v     = Z0(IX,IY)
        mobulv  = MOBUL(IX,IY)
        hmixv   = HMIX(IX,IY)
        topv    = TOPM(IX,IY)

!       Calculate the density, kinematic and dynamic viscosity, and
!       free molecular path for air:
 
        call cairpar(t_zref,p_zref,ny_air,my_air,rho_air,pr,lambda_air)

!	Calculate the Aerodynamic Resistance (s/m) at the reference 
!       height (according to Seinfeld/Pandis,1998 this height is 10 m
!       or less):

        call caero(kappa,z0v,zref,ustarv,mobulv,Ra_v)
        
!MSK start 
! THIS IS THE PLACE WHERE AERO() IS COMPUTED
! aero - Aerodynamic resistance (nx,ny)
         !print *,'CDDEPVEL Ra_v: ', Ra_v
         if (.not. AEROFE) then
            AERO(IX,IY) = Ra_v
         endif
!MSK end


!       Go through all compounds

        DO IC = 1,NC

          IF(TRIM(CMPND(IC)) .NE. 'PM10'  .AND.  &
             TRIM(CMPND(IC)) .NE. 'PM2.5' .AND.  &
             TRIM(CMPND(IC)) .NE. 'EP'    .AND.  &
             TRIM(CMPND(IC)) .NE. 'TSP'        )THEN
	   
!            Gaseous species, either: 'NO2', 'NO', 'NOx', 'O3', 
!                                     'SO2', 'CO', 'CO2' or 'CL'.
!            ***************************************************
!
!MSK 17.10.2017: Aerodynamic Resistance Ra_v=AERO(IX,IY) has been calculated above
!
!MSK 17.10.2017: Now calculate Quasi-Laminar Resistance Rb_v
!
!            Calculate gaseous diffusivity (m^2 s^-1):
!
!            CALL GAS_DIFF(IC, ...., diff_g)
!
!MSK start 17.10.2017 taken from MAFOR
! molecular weight in g/mole
             if (CMOLW(IC) .eq. MISS) then
               MOLW = 40.0
             else
               MOLW = CMOLW(IC)
             end if

! diffusion coefficient (cm^2/s)
! temperature in Kelvin
             DC = 1.e-3/patm*(t_zref+273.)**1.75*SQRT(1.0/M_air + 1.0/MOLW)
             DC = DC/(19.7**(1.0/3.0) + 51.96**(1.0/3.0))**2.0
! diffusion coefficient (m^2/s)
             DIFF_g = DC*1.e-4

!MSK end 17.10.2017
!
!	       Calculate gaseous roughness height z0_g value (m):
!
!	       CALL GAS_Z0(kappa,ustarv,diff_g,z0_g)
!
!	       Calculate Quasi-Laminar Resistance (s/m):
!            Rb_method = 1  ! Calculate Rb based on EMEP Unified and 
!	                      ! Seinfeld & Pandis: eq. (19.17).
!            Rb_method = 2  ! Calculate Rb based on Jacobson eq.(20.14)
!
!            m_gas = CMOLW(IC) 
!	       CALL C_Rb_GAS(Rb_method,kappa,g,pr,ustarv,z0v,z0_g,ny_air,
!     &                    diff_g,Rb_v)
!
!
!MSK start 17.10.2017 taken from MAFOR + EMEP report 01/2003
! Schmidt number
             Sc_g = ny_air/rho_air/DIFF_g
 
             Rb_v = 1.0
             if (topv >= 0.5 ) then
! over land
               Rb_v = (2./(kappa*ustarv)) * (Sc_g/Pr_g)**(2./3.)
             else
! over sea
               Rb_v = (1./(kappa*ustarv)) * LOG(z0v*kappa*ustarv/DIFF_g)
             end if
!MSK end 17.10.2017

!            Calculate Surface or Canopy Resistance (s/m):
!
!            CALL C_Rc(IC, .....  ,Rc_v)

!MSK 17.10.2017: Use a R_low = 1.0 for the Canopy Resistance Rc_v
             Rc_v = 1.0

!MSK 17.10.2017: No settling velocity for Gases, important to set it to zero.
             Vs_v = 0.0

!MSK 17.10.2017: No re-calculation needed
!            Recalculate the Aerodynamic resistance if we want to
!            use the z0_p in the formula:
!
!	       CALL CAERO(kappa,z0_p,zref,ustarv,mobulv,Ra_v)

               ! write(6,*) 'res',IC,topv,z0v,(z0v*kappa*ustarv/DIFF_g),Rb_v

          ELSE
!
!           Particulate species:
!           ********************
! Representative particle diameter (4 um)??

              IF(TRIM(CMPND(IC)) .EQ. 'PM10')THEN
                  D_p  = 4.0   ! Geometric mean D for Number Distribution in um
                  Rc_v = 0.0
                  sig_g = 2.0
              ELSEIF(TRIM(CMPND(IC)) .EQ. 'PM2.5')THEN
                  D_p   = 0.3
                  sig_g = 1.8
                  Rc_v  = 0.0
              ELSEIF(TRIM(CMPND(IC)) .EQ. 'EP')THEN
                  D_p   = 0.3
                  sig_g = 1.8
                  Rc_v  = 0.0
              ELSEIF(TRIM(CMPND(IC)) .EQ. 'TSP')THEN
                  D_p  = 4.0
                  sig_g = 2.0
                  Rc_v = 0.0
             ENDIF

!            Calculate Cunninghams Correction Factor: C_c

             call ccunc(D_p,lambda_air,Kn,C_c)



!	       Calculate particle diffusivity:

!	       method_part = 1  !  Based on Seinfeld & Pandis.

             method_part = 2  ! or Based on Binkowski.

             IF(method_part .EQ. 2)THEN
!              Calculate Log-Normal Distruibution factors for the Binkowski Method.

                 lnsig2      = (LOG(sig_g))**2.0
                 D_p         = D_p*EXP(-3.0*lnsig2)
                 Vs_scal     = EXP(8.0*lnsig2) +  &
                               (1.246 * Kn * EXP(3.5*lnsig2))
                 diff_p_scal = EXP(-2.5*lnsig2) +  &
                               (1.246 * Kn * EXP(-4.0*lnsig2))
             ENDIF



             call part_diff(method_part,D_p,t_zref,my_air,C_c, &
                       diff_p_scal,diff_p)


!	       Calculate particle z0_p value (m):

             call part_z0(kappa,ustarv,diff_p,z0_p)


!	       Calculate Particle Gravitational Settling Velocity (m/s):
!
             call pfallsp(method_part,g,D_p,rho_p,rho_air,my_air,C_c,Kn, &
                     Vs_scal,Vs_v)

!	       Calculate Quasi-Laminar Resistance (s/m):
!            Calculate Rb based on EMEP Unified with either 
!            Seinfeld & Pandis: eq. (19.18) or Binkowski.
             Rb_method = 1

!            Calculate Rb based on Jacobson eq.(20.14)
!            Rb_method = 3 

             call c_rb(Rb_method,kappa,g,pr,ustarv,z0v,z0_p,ny_air, &
                  diff_p,Vs_v,Rb_v)


!            Recalculate the Aerodynamic resistance if we want to
!            use the z0_p in the formula:
!
!	       CALL CAERO(kappa,z0_p,zref,ustarv,mobulv,Ra_v)

          ENDIF 

!         Calculate the Dry-Deposition Velocity (m/s):

             call cdrydepv(Ra_v,Rb_v,Rc_v,Vs_v,V_drydepv)


!MSK start 20.10.2017 Adaption of V_drydepv to 2m height
!
!         If it is necessary, the dry-dep velocity could be 
!         reduced along the MO-adjusted concentration profile: 
!         For example if V_drydepv is valid at 2 m, then
!
!         PH = LOG(zref/2.0) - PSIH(zref/mobulv) + PSIH(2.0/mobulv)
!         V_drydepv = V_drydepv/(1.0 +((V_drydepv*PH)/(kappa*ustarv)))

             V_drydepv = V_drydepv/(V_drydepv*AERO(IX,IY) + 1.0)
!MSK end 20.10.2017

             DDEPV2D(IC,IX,IY) = V_drydepv

         ! print *,'cddepvel: after DDEPV2D', IC, DDEPV2D(IC,IX,IY)

!_LHS_08_Oct_2004_Start:
!_Deposition_TEST
!
!           DDEPV2D(IC,IX,IY) = 0.0
!
!_LHS_08_Oct_2004_End.

!         IF (DDEPV2D(IC,IX,IY) .EQ. MISS) DDEPV2D(IC,IX,IY) = V_drydepv

        ENDDO

      ENDDO
      ENDDO


      IF (MESSFE) WRITE (MESSUN,2000)

      RETURN

 2000 FORMAT ('CDDEPVEL: Calculate Dry-Deposition Velocity')

      RETURN

!     End of subroutine CDDEPVEL
      end subroutine cddepvel
