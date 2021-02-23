! <cdzdt_new.f90 - A component of the City-scale
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

      subroutine cdzdt_new

! **********************************************************************
!      The CDZDT subroutine calculates the 3D vertical exchange
!      field DZDT which is equal to the vertical eddy diffusivity 
!      divided by the vertical distance between neighbouring 
!      concentration values. Thus:
!
!          DZDT(I,J,K) = V_DIFF(I,J,K) / THL
!  
!      where:       THL = 0.5*(DZ(K) + DZ(K+1))
!
!	The vertical turbulent transport (flux) is thereby given by:
!
!	     DZDT(I,J,K) * [ C(I,J,K+1) - C(I,J,K) ]
!
!      Different options are available for the parameterisation 
!      of V_DIF.
!
! **********************************************************************
!       2017 Matthias Karl, HZG, CityChem extension:
!            New option for calculation of vertical eddy diffusivity
!            ALTERNATIVE 6: Use of unstable, neutral and stable regions.
!                           Neutral conditions:  Shir(1973).
!                           Stable conditions:   Businger and Arya(1974).
!                           Unstable conditions: Lamb and Duran (1977).
!            For stable conditions the non-dimensional gradient
!            of temperature (Phi_HL) parameterisation by
!            Beljaars and Holtslag (1991) replaces the parameterisation
!            by Businger et al. (1971)
!            Beljaars, ACM and Holtslag, AAM, 1991:
!            Flux parameterization over land surfaces for atmospheric models
!            J. Appl. Meteorology 40, 327-341.
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

      use mod_util
      use mod_main
      use mod_time
      use mod_site
      use mod_mete

      implicit none

! ... Local variables

      real    :: THL,V_DIFF_CONST,V_DIFF_MIN,V_DIFF_MAX
      real    :: pr_fi,gamma_fi,beta_fi,kappa_fi,zeta_L,zeta_h,fi_zeta_L
      real    :: ustarv,tstarv,mobulv,amobulv,critmobulv,hmixv
      real    :: h_surf,wmixstarv,tmixstarv,ALFALHS,ABS_f_cor   
      real    :: DZ_k,Z_k
      real    :: V_DIFF_CONST_ORIG
      real    :: V_DIFF_SCALING
!MSK start
      real    :: pre,ustarmin
      real    :: b_fi,c_fi,d_fi
      double precision            :: dzeta,reust
      double precision            :: expdzeta,expreust
      double precision, parameter :: EXMIN = -87.3
      double precision, parameter :: EXMAX = 88.7
!MSK end      
      integer :: I,J,K,NILU_METHOD
      integer :: V_DIFF_IMIN,V_DIFF_JMIN,V_DIFF_KMIN
      integer :: V_DIFF_IMAX,V_DIFF_JMAX,V_DIFF_KMAX
      integer :: N_subint,K_k
      
      real,allocatable  :: V_DIFF(:,:,:)

      IF (.NOT. ALLOCATED(V_DIFF))  ALLOCATE (V_DIFF(NX,NY,NZ))
!MSK start
      IF (.NOT. ALLOCATED(DZDT)) ALLOCATE (DZDT(NX,NY,NZ))
!MSk end


! ... V_DIFF(NX,NY,NZ) -  The vertical Eddy Diffusivity
! ... V_DIFF_MIN       -  Minimum value of the vertical Eddy Diffusivity
! ... V_DIFF_MAX       -  Maximum value of the vertical Eddy Diffusivity
! ... V_DIFF_CONST     -  Additiv value for avoiding zero values of the 
!                         Eddy Diffusivity
!                   
!
! ... THL             -  The vertical distance between neighbouring 
!                        concentration values
!
! ... NILU_METHOD     -  Integer value defining which of the available
!                        parameterisations to be used.
! ... I,J,K           -  Main grid indices
! ... V_DIFF_IMIN      -  X-index of the minimum Eddy Diffusivity value
! ... V_DIFF_JMIN      -  Y-index of the minimum Eddy Diffusivity value
! ... V_DIFF_KMIN      -  Z-index of the minimum Eddy Diffusivity value
! ... V_DIFF_IMAX      -  X-index of the maximum Eddy Diffusivity value
! ... V_DIFF_JMAX      -  Y-index of the maximum Eddy Diffusivity value
! ... V_DIFF_KMAX      -  Z-index of the maximum Eddy Diffusivity value

! **********************************************************************

      ABS_f_cor = ABS(f_cor)

!_CITYDELTA_Start:
!
! ... SELECTING the parameterisation for the EDDY DIFFUSIVITY:
!
!     ALTERNATIVE 1: Applying simple Surface layer Monin-Obukhov
!                    similarity theory.
!
!     ALTERNATIVE 2: Only use of neutral and stable regions.
!                    Neutral and unstable conditions:  Shir(1973).
!                    Stable conditions:   Businger and Arya(1974).
!
!     ALTERNATIVE 3: EPA/600/R-99/030, page  7.21 -  7.24  and 
!                                           12.32 - 12.34
!
!     ALTERNATIVE 4: EPA/600/R-99/030, page  7.24 -  7.25 
!                   (Vertically averaging of Alternative 3)
!
!     ALTERNATIVE 5: When predefined values of V_DIFF(I,J,K) is given,
!                    either constant values: V_DIFF = 1.0 or 100.0, or
!                    with diffusivities from UM or other met models.
!
!     ALTERNATIVE 6: As Alt.2 but also treating unstable conditions.
!                    Neutral conditions:  Shir(1973).
!                    Stable conditions:   Businger and Arya(1974).
!MSK                           modified by Beljaars and Holtslag (1991)
!                    Unstable conditions: Lamb and Duran (1977).
!
!      
!        NILU_METHOD = 1
!        NILU_METHOD = 2
!        NILU_METHOD = 3    
!MSK try method 6
!MSK         NILU_METHOD = 4 !BRUCE choice:
!        NILU_METHOD = 5
!MSK start
!        NILU_METHOD = 6
!MSK end
!
! ... End SELECTING the parameterisation for the EDDY DIFFUSIVITY ******
!
! **********************************************************************
!
! ... Defining the Additive Background Eddy Diffusivity:

      V_DIFF_CONST = 0.01
!	V_DIFF_CONST = 0.5
!	V_DIFF_CONST = 1.0
!	V_DIFF_CONST = 5.0

      V_DIFF_CONST_ORIG=V_DIFF_CONST !BRUCE: Included so that it can be set to 0 above the mixing height

      V_DIFF_SCALING=1  !BRUCE: Scales V_DIFF by this amount rather than adding a constant value
      V_DIFF_CONST=V_DIFF_CONST/V_DIFF_SCALING !BRUCE: Reduce this by the scaling to get the same constant offset
      
! ... End Defining the Additive Background Eddy Diffusivity.
!
! **********************************************************************
!
! ... Defining some of the eddy-diffusivity constants:
      pr_fi    = 1.0
      beta_fi  = 4.7
      gamma_fi = 15.0
      kappa_fi = 0.4

! ... End Defining some of the eddy-diffusivity constants.

      V_DIFF_MIN = +1.E35
      V_DIFF_MAX = -1.E35

!MSK start
! ... Set NILU_METHOD based on Model User Input IVERTDIFF
      if (ivertdiff.eq.1) then
        NILU_METHOD = 4
      else if (ivertdiff.eq.2) then
        NILU_METHOD = 6
      else
        NILU_METHOD = 4
      endif
!MSK end

! ... Calculate vertical exchange in the NZ layers:

      DO K = 1,NZ

! ...   Calculate thickness of layer K

        IF (K .LT. NZ) THEN
          THL = 0.5*(DZ(K) + DZ(K+1))
        ELSE
          THL = DZ(NZ)
        ENDIF

! ...   Calculate vertical exchange in layer K

        DO J = 1,NY
         DO I = 1,NX

!MSK start
           V_DIFF(I,J,K) = 0.0
           DZDT(I,J,K)   = 0.0
!MSK end

           ustarv     = USTAR(I,J)
           tstarv     = TSTAR(I,J)
           mobulv     = MOBUL(I,J)
           hmixv      = HMIX(I,J)
           wmixstarv  = WMIXSTAR(I,J)
           tmixstarv  = TMIXSTAR(I,J)

           amobulv = ABS(mobulv)

           h_surf  = 0.1 * hmixv
           zeta_L  = Z(K) / mobulv
           zeta_h  = Z(K) / hmixv

!     BRUCE: zeta_L concept is only valid near the surface and requires a local
!     closure for z>>L for stable conditions. End result is similar to a limit on zeta_L.
!      Maximum allowed value is set to 1 here
            if (zeta_L.gt.1) zeta_L=1

! ...      A more stringent demand would be: |u*/(fL)| < 4 (van Ulden 
! ...                                              and Holtslag, 1985)
           critmobulv = ustarv / (4.0 * ABS_f_cor)

! ...      NOTE: CRITICAL L FOR CHOICE OF FORMULAE (Taken from MEPDIM!)		
!		   critmobulv = 3906.25*UST

! ***      Ref: Seinfeld and Pandis (1998): Table 16.2 (page 863):
!          critmobulv = 1.0E+05

! ...      Estimating the general profile functions of the Surface 
! ...      Layer Monin-Obukhov similarity theory: fi = fi(z/L)
! ...      Ref: Seinfeld and Pandis, page 938 - 942; and
! ...           EPA/600/R-99/030, page 7.21 - 7.24  and 12.32 - 12.34 
!
! ...      See also: Jacobsen, page 213
! ...                Garratt,  page 52, 247-249.  
!
! ----------------------------------------------------------------------

           IF(NILU_METHOD .LE. 2 .OR. NILU_METHOD .EQ. 6)THEN

             IF (zeta_L .LT. 0.0) THEN
! ...          Unstable:
               fi_zeta_L = pr_fi * (1./SQRT(1. - gamma_fi*zeta_L))
             ELSEIF (zeta_L .GE. 0.0) THEN
! ...          Stable:
               fi_zeta_L = pr_fi * ( 1. + beta_fi*zeta_L )
             ENDIF
! ----------------------------------------------------------------------

           ELSEIF(NILU_METHOD .EQ. 3)THEN

             IF (zeta_L .LE. 0.0) THEN
! ...          Unstable:
               fi_zeta_L = pr_fi * (1./SQRT(1. - gamma_fi*zeta_L))
             ELSEIF (zeta_L .GT. 0.0 .AND. zeta_L .LE. 1.0) THEN
! ....         Moderately Stable:
               fi_zeta_L = pr_fi * ( 1. + beta_fi*zeta_L )
             ELSEIF (zeta_L .GT. 1.0) THEN
! ...          Very Stable
               fi_zeta_L = pr_fi * ( beta_fi + zeta_L )
             ENDIF

           ENDIF

! **********************************************************************

           IF(NILU_METHOD .EQ. 1)THEN
! ...        ALTERNATIVE 1: Applying simple Surface layer Monin-Obukhov
! ...                       similarity theory:
!MSK start
             V_DIFF(I,J,K) = 0.0
             DZDT(I,J,K)   = 0.0
!MSK end
             V_DIFF(I,J,K) = V_DIFF_CONST +                                &
                             ( (kappa_fi * ustarv * Z(K) ) / fi_zeta_L )
             DZDT(I,J,K)  = V_DIFF(I,J,K) / THL

! ----------------------------------------------------------------------

           ELSEIF(NILU_METHOD .EQ. 2)THEN
! ...        ALTERNATIVE 2: Only use of neutral and stable regions.
! ...                       Neutral and unstable conditions:  Shir(1973).
! ...                       Stable conditions:   Businger and Arya(1974).
!   
! ...        We apply an additional V_DIFF_CONST which is due to 
! ...        the increased turbulence by the urban roughness/heat
! ...        island. This additional value simply imply that
! ...        there always exists an urban turbulent mixing with
! ...        an apparent eddy diffusivity of:
!
! ...        K_0 = ((2*20.0)**2)/3600  for USTAR > 0.2 m/s and 
!
! ...        K_0 = ((20.0)**2)/3600    for USTAR < 0.1 m/s and 
!
! ...        with a linear change in between.; i.e. a diffusion 
! ...        length scale of either two times the original value 
! ...        of DZ(1) = 20 m within a time span of one hour, or
! ...        only half this value.

             IF (ustarv .GE. 0.2) THEN
               ALFALHS = 4.0
             ELSEIF (ustarv .LE. 0.1) THEN
               ALFALHS = 1.0
             ELSE
               ALFALHS = (30.0*ustarv) - 2.0
             ENDIF
             V_DIFF_CONST = (ALFALHS*20.*20.)/(3600.0)
!_LHS_March-2008_TEST_Start:
!             V_DIFF_CONST = 1.0
!_LHS_March-2008_TEST_End.

!_LHS           IF( zeta_L .LT. 0.0 .OR. mobulv .GT. critmobulv)THEN
            IF( zeta_L .LE. 0.0 )THEN
! ...         Neutral: Shir, 1973.
!			IF(Z(K) .LE. h_surf)THEN
! ...           Within the surface layer:
!		      V_DIFF(I,J,K) =  V_DIFF_CONST + 
!     &                        ( kappa_fi * ustarv * Z(K) /
!     &                          fi_zeta_L )          
!		      DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!			ELSE
! ...           Above the surface layer:
!MSK start
                      V_DIFF(I,J,K) = 0.0
                      DZDT(I,J,K)   = 0.0
!MSK end
                      V_DIFF(I,J,K) = V_DIFF_CONST +                   &
                              ( kappa_fi * ustarv * Z(K) *             &
                              EXP(-8.0*ABS_f_cor*Z(K)/ustarv) ) 

                      DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!			ENDIF

!_LHS		  ELSEIF( mobulv .GE. 0.0 .AND. mobulv .LE. critmobulv)THEN
                 ELSEIF( mobulv .GT. 0.0 )THEN

! ...         Stable: Businger and Arya, 1974.
!			IF(Z(K) .LE. h_surf)THEN
! ...           Within the surface layer:
!		      V_DIFF(I,J,K) =  V_DIFF_CONST + 
!     &                        ( kappa_fi * ustarv * Z(K) /
!     &                          fi_zeta_L )          
!		      DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!			ELSE
! ...           Above the surface layer:
                      V_DIFF(I,J,K) = V_DIFF_CONST +                    &
                              (kappa_fi * ustarv * Z(K) *         &
                              (EXP(-8.0*ABS_f_cor*Z(K)/ustarv)) /       &
                              (0.74 + beta_fi*zeta_L) )

                      DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!			ENDIF

                ENDIF

! ----------------------------------------------------------------------

                ELSEIF(NILU_METHOD .EQ. 3)THEN
! ...       ALTERNATIVE 3: EPA/600/R-99/030, page  7.21 -  7.24  and 
! ...                                             12.32 - 12.34
!MSK start
                      V_DIFF(I,J,K) = 0.0
                      DZDT(I,J,K)   = 0.0
!MSK end
            IF ( zeta_h .GE. 1.0) THEN
! ...         Above the estimated mixing height:
                 V_DIFF(I,J,K) = V_DIFF_CONST
                 DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
            ELSE
! ...         Below the estimated mixing height:
                IF (zeta_L .LE. 0.) THEN
! ...           Unstable conditions:
!			  IF(Z(K) .LE. h_surf)THEN
! ...             Within the surface layer:
!		        V_DIFF(I,J,K) =  V_DIFF_CONST + 
!     &                         ( kappa_fi * ustarv * Z(K) /
!     &                           fi_zeta_L )          
!		        DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!			  ELSE
! ...             Above the surface layer:
                       V_DIFF(I,J,K) =  V_DIFF_CONST +               &
                                 ( kappa_fi * wmixstarv * Z(K) *       &
                                 (1.0 - zeta_h) )
               
                       DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!			  ENDIF

                 ELSEIF (zeta_L .GT. 0.0) THEN
! ...          Moderately Stable and Very Stable conditions:
!			 IF(Z(K) .LE. h_surf)THEN
! ...            Within the surface layer:
!		       V_DIFF(I,J,K) =  V_DIFF_CONST + 
!     &                         ( kappa_fi * ustarv * Z(K) /
!     &                           fi_zeta_L )          
!		       DZDT(I,J,K)  = V_DIFF(I,J,K) / THL			
!			 ELSE
! ...            Above the surface layer:
                       V_DIFF(I,J,K) = V_DIFF_CONST +                   &
                               (kappa_fi * ustarv * Z(K) *        &
                               (1.0 - zeta_h)**1.5 ) /                  &
                               fi_zeta_L

                       DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!			 ENDIF

                   ENDIF

              ENDIF

! ----------------------------------------------------------------------

            ELSEIF(NILU_METHOD .EQ. 4)THEN

! ...      ALTERNATIVE 4: EPA/600/R-99/030, page  7.24 -  7.25
         !BRUCE: Only apply the addition of constant below hmixv
         IF ( zeta_h .GT. 1.0) THEN 
            V_DIFF_CONST=0
            !V_DIFF_CONST=V_DIFF_CONST_ORIG
         ELSE
            V_DIFF_CONST=V_DIFF_CONST_ORIG
         ENDIF
         
              V_DIFF(I,J,K) = 0.0 
              DZDT(I,J,K)   = 0.0
              N_subint = 5
! ...      With the value 1 below NILU_METHOD = 3 or 4 are identical.
!		 N_subint = 1
              DZ_k = THL / FLOAT(N_subint)
              Z_k  = Z(K) - (0.5 * DZ(K))
           DO K_k = 1,N_subint
              Z_k  = Z_k + (0.5 * DZ_k)
              zeta_L = Z_k / mobulv !min(Z_k / mobulv,2.0)!BRUCE: minimum mobulv is set in turb_profile.for not here!
              zeta_h = Z_k / hmixv !max(hmixv,100.0)!BRUCE: minimum hmix is 100. Removed as this is set elsewhere
! ........................................................................
             IF (zeta_L .LE. 0.0) THEN
! ...          Unstable:
               fi_zeta_L = pr_fi * (1./SQRT(1. - gamma_fi*zeta_L))
             ELSEIF (zeta_L .GT. 0.0 .AND. zeta_L .LE. 1.0) THEN
! ...          Moderately Stable:
               fi_zeta_L = pr_fi * ( 1. + beta_fi*zeta_L )
               !fi_zeta_L = pr_fi !BRUCE
             ELSEIF (zeta_L .GT. 1.0) THEN
! ...          Very Stable
               fi_zeta_L = pr_fi * ( beta_fi + zeta_L )
               !fi_zeta_L = pr_fi !BRUCE
             ENDIF
! ........................................................................
             IF ( zeta_h .GE. 1.0) THEN
! ...          Above the estimated mixing height:
                    V_DIFF(I,J,K) = V_DIFF(I,J,K) +                   &
                                  ((DZ_k/THL) * V_DIFF_CONST)
             ELSE
! ...          Below the estimated mixing height:
                 IF (zeta_L .LE. 0.) THEN
! ...            Unstable conditions:
!		  	   IF(Z_k .LE. h_surf)THEN
! ...              Within the surface layer:
!		         V_DIFF(I,J,K) =  V_DIFF(I,J,K) +
!     &                             (DZ_k/THL*(V_DIFF_CONST + 
!     &                              (kappa_fi * ustarv * Z_k /
!     &                               fi_zeta_L ) ) )
!			   ELSE
! ...              Above the surface layer:
                        V_DIFF(I,J,K) = V_DIFF(I,J,K) +                 &
                                   (DZ_k/THL * (V_DIFF_CONST +           &
                                   ( kappa_fi * wmixstarv * Z_k *        &
                                   (1.0 - zeta_h) ) ) )
!			   ENDIF

                  ELSEIF (zeta_L .GT. 0.0) THEN
! ...            Moderately Stable and Very Stable conditions:
!			   IF(Z_k .LE. h_surf)THEN
! ...              Within the surface layer:
!		         V_DIFF(I,J,K) = V_DIFF(I,J,K) +
!     &                            (DZ_k/THL * (V_DIFF_CONST + 
!     &                            ( kappa_fi * ustarv * Z_k /
!     &                              fi_zeta_L ) ) )			
!			   ELSE
! ...              Above the surface layer:
                         V_DIFF(I,J,K) = V_DIFF(I,J,K) +                 &
                                   (DZ_k/THL * (V_DIFF_CONST +           &
                                          (kappa_fi * ustarv * Z_k *     &
                                          ((1.0 - zeta_h)**1.5) ) /      &
                                          fi_zeta_L ) )
!			   ENDIF

                   ENDIF

                  ENDIF

                 ENDDO

                 DZDT(I,J,K)  = V_DIFF(I,J,K) / THL

! -----------------------------------------------------------------------

               ELSEIF(NILU_METHOD .EQ. 5)THEN

!MSK start
                      V_DIFF(I,J,K) = 0.0
                      DZDT(I,J,K)   = 0.0
!MSK end

! ...       ALTERNATIVE 5: When predefined values of V_DIF(I,J,K) is 
! ...                      preferred, for example V_DIFF = 1.0 or 100.0,
! ...                      or values from UM. 
                  IF (ustarv .GE. 0.2) THEN
                       ALFALHS = 4.0
                  ELSEIF (ustarv .LE. 0.1) THEN
                       ALFALHS = 1.0
                  ELSE
                       ALFALHS = (30.0*ustarv) - 2.0
                  ENDIF

             V_DIFF_CONST = (ALFALHS*20.*20.)/(3600.0)

!_NEST_Start: TEST 311 (Applying 1.0 below).
!	      DZDT(I,J,K) = 1.0 / THL
!	      DZDT(I,J,K) = 100.0 / THL
!_NEST_End.  TEST311.

!_LHS_04Jan2005_Start:

! ...       UM Heat diffusivities:
!          V_DIFF(I,J,K) = KZ_H(I,J,K)
!           V_DIFF(I,J,K) = mecf(I,J,K)
!           V_DIFF(I,J,K) = K_q(I,J,K)
            V_DIFF(I,J,K) = K_h(I,J,K) + V_DIFF_CONST
! ...       UM Momentum diffusivities:
!           V_DIFF(I,J,K) = KZ_M(I,J,K)

!_LHS_04Jan2005_End.

! ...       Constant value:
!            V_DIFF(I,J,K) = 100.0  

            DZDT(I,J,K) = V_DIFF(I,J,K) / THL



           ELSEIF(NILU_METHOD .EQ. 6)THEN

! ...        ALTERNATIVE 6: Use of unstable, neutral and stable regions.
! ...                       Neutral conditions:  Shir(1973).
! ...                       Stable conditions:   Businger and Arya(1974).
! ...                       Unstable conditions: Lamb and Duran (1977).
!   
! ...        We apply an additional V_DIFF_CONST which is due to 
! ...        the increased turbulence by the urban roughness/heat
! ...        island. This additional value simply imply that
! ...        there always exists an urban turbulent mixing with
! ...        an apparent eddy diffusivity of:
!
! ...        K_0 = ((2*20.0)**2)/3600  for USTAR > 0.2 m/s and 
!
! ...        K_0 = ((20.0)**2)/3600    for USTAR < 0.1 m/s and 
!
! ...        with a linear change in between.; i.e. a diffusion 
! ...        length scale of either two times the original value 
! ...        of DZ(1) = 20 m within a time span of one hour, or
! ...        only half this value.

!MSK start
!MSK         For stable conditions the non-dimensional gradient
!MSK         of temperature (Phi_HL) parameterisation by
!MSK         Beljaars and Holtslag (1991) replaces the parameterisation
!MSK         by Businger et al. (1971)
!MSK         Beljaars, ACM and Holtslag, AAM, 1991:
!MSK         Flux parameterization over land surfaces for atmospheric models
!MSK         J. Appl. Meteorology 40, 327-341.

                V_DIFF(I,J,K) = 0.0
                DZDT(I,J,K)   = 0.0
                pre           = -8.0
                ustarmin      = 1.e-5
!MSK         Beljaars and Holtslag (1991) empirical constants
                b_fi          = 0.6666666
                c_fi          = 5.0
                d_fi          = -0.35
!MSK end

             IF (ustarv .GE. 0.2) THEN
               ALFALHS = 4.0
             ELSEIF (ustarv .LE. 0.1) THEN
               ALFALHS = 1.0
             ELSE
               ALFALHS = (30.0*ustarv) - 2.0
             ENDIF

             V_DIFF_CONST = (ALFALHS*DZ(1)*DZ(1))/(3600.0)

!MSK start Scaling for option 6
!MSK       no sacling applied (status: 06.06.2017)
             V_DIFF_SCALING=1.
             !BRUCE: Reduce this by the scaling to get the same constant offset
             V_DIFF_CONST=V_DIFF_CONST/V_DIFF_SCALING

!MSK    replaced EXP() in expression below with robust proxy
             reust = pre*ABS_f_cor*Z(K)/max(ustarv,ustarmin)
             if (reust .gt. EXMIN .and. reust.lt. EXMAX) then
                expreust = EXP(reust)
             elseif (reust .le. EXMIN) then
                expreust = 0.
             else
                expreust = EXP(EXMAX)
             endif
!MSK end
!MSK start
!MSK    replaced EXP() in expression below with robust proxy
             dzeta = d_fi*zeta_L
             if (dzeta .gt. EXMIN .and. dzeta.lt. EXMAX) then
                expdzeta = EXP(dzeta)
             elseif (dzeta .le. EXMIN) then
                expdzeta = 0.
             else
               expdzeta = EXP(EXMAX)
             endif                
!MSK end

             IF(amobulv .GT. critmobulv)THEN
! ...          Neutral: Shir, 1973.
!              IF(Z(K) .LE. h_surf)THEN
! ...            Within the surface layer:
!                V_DIFF(I,J,K) =  V_DIFF_CONST + 
!     &                        ( kappa_fi * ustarv * Z(K) /
!     &                          fi_zeta_L )          
!                DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!              ELSE
! ...            Above the surface layer:
                 V_DIFF(I,J,K) = V_DIFF_CONST +                              &
                               ( kappa_fi * ustarv * Z(K) *                  &
!MSK start
!                               EXP(-8.0*ABS_f_cor*Z(K)/ustarv) )
                               EXP(expreust) )
!MSK end

                 DZDT(I,J,K)  = V_DIFF(I,J,K) / THL                
!              ENDIF
             ELSEIF( mobulv .GE. 0.0 .AND. mobulv .LE. critmobulv)THEN
! ...	         Stable: Businger and Arya, 1974.
!              IF(Z(K) .LE. h_surf)THEN
! ...            Within the surface layer:
!                V_DIFF(I,J,K) =  V_DIFF_CONST + 
!     &                        ( kappa_fi * ustarv * Z(K) /
!     &                          fi_zeta_L )          
!                DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!              ELSE
! ...            Above the surface layer:

                 V_DIFF(I,J,K) = V_DIFF_CONST +                              &
                                       (kappa_fi * ustarv * Z(K) *           &
!MSK start
!MSK replaced                   (EXP(-8.0*ABS_f_cor*Z(K)/ustarv)) /           &
!MSK replaced                       (0.74 + beta_fi*zeta_L) )
!MSK            Temperature gradient parameterisation by Businger et al. (1971)
!MSK            replaced by parameterisation by Beljaars and Holtslag (1991)
!MSK            for the stable boundary layer.
!MSK            Scaled by 0.8 to be in better agreement with Basu & Porte-Agel
!MSK            LES simulations (J.Atm.Sci. 2005). Used Eq. (29) therein.
                              (EXP(expreust)) /                                   &
                              (0.8*(1.0+zeta_L*(1.0*sqrt(1.0+2.0*zeta_L/3.0) +    &
                               b_fi*real(expdzeta)*(1.0+c_fi+d_fi*zeta_L))))  )
!MSK end

                 DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!              ENDIF
             ELSE
! ...          Unstable: Lamb and Duran (1977).

!              IF(Z(K) .LE. h_surf)THEN
! ...            Within the surface layer:
!                V_DIFF(I,J,K) =  V_DIFF_CONST + 
!     &                         ( kappa_fi * ustarv * Z(K) /
!     &                           fi_zeta_L )          
!                DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!              ELSE
! ...            Above the surface layer:

                 IF(zeta_h .LT. 0.05)THEN

                   V_DIFF(I,J,K) = (2.5*((kappa_fi * zeta_h)**(4./3.)))             &
                                 * ((1.0 - (gamma_fi*zeta_L))**0.25)
	          
                 ELSEIF( zeta_h .GE. 0.05 .AND. zeta_h .LE. 0.6)THEN

                   V_DIFF(I,J,K) =  0.021 + (0.408 * zeta_h)                        &
                                 + (1.351 * zeta_h * zeta_h)                        &
                                 - (4.096 * zeta_h * zeta_h * zeta_h)               &
                                 + (2.560 * zeta_h * zeta_h * zeta_h * zeta_h)

                 ELSEIF( zeta_h .GT. 0.6 .AND. zeta_h .LE. 1.1)THEN

                   V_DIFF(I,J,K) =  0.2 * EXP(6.0 - (10.0 * zeta_h))

                 ELSE

                   V_DIFF(I,J,K) =  0.0013

                 ENDIF

                 V_DIFF(I,J,K) = V_DIFF_CONST +                                   &
                                 ( wmixstarv * hmixv * V_DIFF(I,J,K) )

                 DZDT(I,J,K)  = V_DIFF(I,J,K) / THL
!              ENDIF

             ENDIF

           ENDIF

!MSK debug start pnc check
!debug       if((k.eq.1).and.(i.eq.12).and.(j.eq.10))then
!debug          print *,'dzdt (1)',zeta_L,mobulv,zeta_h,wmixstarv,hmixv,dzeta,expdzeta,reust,expreust, &
!debug                             pre,ABS_f_cor,Z(1),ustarv,ustarmin
!debug       endif
!MSK debug

            !BRUCE: Scale DZDT
            DZDT(I,J,K)=DZDT(I,J,K)*V_DIFF_SCALING
            
! ...      Calculating Minimum and Maximum value of V_DIFF(I,J,K):
           IF(V_DIFF(I,J,K) .LT. V_DIFF_MIN)THEN
             V_DIFF_MIN  = V_DIFF(I,J,K)
             V_DIFF_IMIN = I
             V_DIFF_JMIN = J
             V_DIFF_KMIN = K
           ENDIF

           IF(V_DIFF(I,J,K) .GT. V_DIFF_MAX)THEN
             V_DIFF_MAX  = V_DIFF(I,J,K)
             V_DIFF_IMAX = I
             V_DIFF_JMAX = J
             V_DIFF_KMAX = K
           ENDIF

!_SIGMA_Start:
           DZDT(I,J,K)=DZDT(I,J,K)*(MOD_H/DEPTHM(I,J))*(MOD_H/DEPTHM(I,J))
!_SIGMA_End.

         ENDDO
        ENDDO

      ENDDO

! ***     Writing test data to the log-file:
      IF (MESSFE) THEN

        WRITE (MESSUN,*)
        WRITE (MESSUN,'(A34,I2)')                                 &
           'CDZDT_NEW: Applied VDIFF_method = ',NILU_METHOD
        WRITE (MESSUN,*)
!        WRITE (MESSUN,'(A51)') 
!     &    ' CDZDT_NEW: Estimated profile of Eddy-diffusivity: '
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,1) = ',V_DIFF(14,12,1),
!     &             '    mecf(14,12,1) = ',mecf(14,12,1)
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,2) = ',V_DIFF(14,12,2),
!     &             '    mecf(14,12,2) = ',mecf(14,12,2)
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,3) = ',V_DIFF(14,12,3),
!     &             '    mecf(14,12,3) = ',mecf(14,12,3)
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,4) = ',V_DIFF(14,12,4),
!     &             '    mecf(14,12,4) = ',mecf(14,12,4)
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,5) = ',V_DIFF(14,12,5),
!     &             '    mecf(14,12,5) = ',mecf(14,12,5)
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,6) = ',V_DIFF(14,12,6),
!     &             '    mecf(14,12,6) = ',mecf(14,12,6)
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,7) = ',V_DIFF(14,12,7),
!     &             '    mecf(14,12,7) = ',mecf(14,12,7)
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,8) = ',V_DIFF(14,12,8),
!     &             '    mecf(14,12,8) = ',mecf(14,12,8)
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,9) = ',V_DIFF(14,12,9),
!     &             '    mecf(14,12,9) = ',mecf(14,12,9)
!        WRITE (MESSUN,'(A27,F12.5,A20,F12.5)') 
!     &    'CDZDT_NEW: V_DIFF(14,12,10)= ',V_DIFF(14,12,10),
!     &             '    mecf(14,12,10)= ',mecf(14,12,10)
!
!        WRITE (MESSUN,*)
!
      ENDIF  ! IF (MESSFE)


      IF (MESSFE) THEN
        WRITE (MESSUN,'(A29,1P,2E12.4)')                                 &
          'CDZDT_NEW: V_DIFF_MIN,V_DIFF_MAX = ',V_DIFF_MIN,V_DIFF_MAX
        WRITE (MESSUN,'(A48,1P,3I4)')                                    &
          'CDZDT_NEW: V_DIFF_IMIN,V_DIFF_JMIN,V_DIFF_KMIN = ',           &
                  V_DIFF_IMIN,V_DIFF_JMIN,V_DIFF_KMIN
        WRITE (MESSUN,'(A48,1P,3I4)')                                    &
          'CDZDT_NEW: V_DIFF_IMAX,V_DIFF_JMAX,V_DIFF_KMAX = ',           &
                  V_DIFF_IMAX,V_DIFF_JMAX,V_DIFF_KMAX
      ENDIF


! ... Deallocate memory

      IF (ALLOCATED(V_DIFF)) DEALLOCATE (V_DIFF)


      RETURN

! ... End of subroutine CDZDT_NEW

      end subroutine cdzdt_new
