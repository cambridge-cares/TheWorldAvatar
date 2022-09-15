! <module_mc_wind_adjust.for - A component of the City-scale
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
!DEC$ FIXEDFORMLINESIZE: 132
!***********************************************************************

      module module_mc_wind_adjust


!     This module contains the declaration of the variables applied in
!     the adjustment process.

! Modifications:
!    29 Jan 2019  M. Karl: replaced real*8 by double precision
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!***********************************************************************
!     Logical parameter set in the run_file, which decide whether just
!     the first guess field is stored or if the adjustment is made and
!     the fully divergence free velocity field is stored.
      logical :: adjust

! Integer parameter defining the adjustment method of the wind field.
! Default method is: method_adjust = 1 in order to extract a totally
! divergence free wind field. For test purposes we can also compute
! the wind field where also omega is calculetd directly from the
! lambda values: method_adjust = 2.
      integer :: method_adjust

! Variables used in the estimation of the stability parameter "alfa":
      integer :: method_alfa

      integer :: m1_initial_wfields
      integer :: m2_tested_alfas

      integer :: n_tol_not_reached

      real    :: alfa,constant_alfa

!     The variables below are declared here to provide to export
!     this information to file.
      real    :: max_u,max_u0
      real    :: max_v,max_v0
      real    :: max_w,max_w0
      real    :: max_omega,max_omega0
      real    :: max_top_omega

      real    :: min_u,min_u0
      real    :: min_v,min_v0
      real    :: min_w,min_w0
      real    :: min_omega,min_omega0
      real    :: min_top_omega

      real, allocatable :: pre_defined_alfa(:)
      real, allocatable :: station_error(:,:)
      real, allocatable :: err_average(:)

!     The relaxation parameter for the SOR-algorithm:
      integer :: max_iter
      integer :: lim_iter
      real    :: rel_par
      real    :: lim_rel_par
      real    :: tolerance
      real    :: lim_tolerance
      real    :: rel_tolerance
      real    :: lim_rel_tolerance

!     Applied 1D-arrays (vectors):
      double precision, allocatable :: ak(:)
      double precision, allocatable :: bk(:)
      double precision, allocatable :: ck(:)
      double precision, allocatable :: pk(:)
      double precision, allocatable :: qk(:)
      double precision, allocatable :: rk(:)

!     Applied 3D-arrays:
      double precision, allocatable :: f1st(:,:,:)
      double precision, allocatable :: f1(:,:,:)
      double precision, allocatable :: f2(:,:,:)
      double precision, allocatable :: f3(:,:,:)
      double precision, allocatable :: f4(:,:,:)

      double precision, allocatable :: h1(:,:,:)
      double precision, allocatable :: h2(:,:,:)
      double precision, allocatable :: h3st(:,:,:)
      double precision, allocatable :: h3(:,:,:)
      double precision, allocatable :: h4(:,:,:)
      double precision, allocatable :: h5(:,:,:)
      double precision, allocatable :: h6(:,:,:)
      double precision, allocatable :: h7(:,:,:)
      double precision, allocatable :: h8st(:,:,:)
      double precision, allocatable :: h8(:,:,:)
      double precision, allocatable :: h9(:,:,:)
      double precision, allocatable :: h10(:,:,:)
      double precision, allocatable :: h11(:,:,:)
      double precision, allocatable :: h12(:,:,:)
      double precision, allocatable :: h13st(:,:,:)
      double precision, allocatable :: h13(:,:,:)
      double precision, allocatable :: h14(:,:,:)
      double precision, allocatable :: h15(:,:,:)

      double precision, allocatable :: g_rhs(:,:,:)
! LHS_TEST_START:
!	real*8, allocatable :: bcg(:,:)
! LHS_TEST_END.

      double precision, allocatable :: lambda(:,:,:)

      double precision, allocatable :: div0_3D(:,:,:)
      double precision, allocatable :: div_3D(:,:,:)

      contains

! *** ADJUST MODULE INTERFACE ROUTINES

!*****************************************************************************************************

      subroutine call_adjust_wfield

      implicit none

      call adjust_wfield

      return

      end subroutine call_adjust_wfield

!*****************************************************************************************************





!****************************************************************************************

      SUBROUTINE SendAdjustAdvanced(bv_adjust, &
                               iv_method_alfa, &
                               rv_constant_alfa, &
                               iv_max_iter, &
                               iv_lim_iter, &
                               rv_rel_par, &
                               rv_tolerance, &
                               rv_rel_tolerance, &
                               rv_lim_rel_par, &
                               rv_lim_tolerance, &
                               rv_lim_rel_tolerance, &
                               iv_method_adjust)


!----------------------------------------------------------------------------------------

      IMPLICIT NONE

      logical :: bv_adjust

      integer :: iv_method_alfa
      integer :: iv_max_iter
      integer :: iv_lim_iter
      integer :: iv_method_adjust

      real    :: rv_constant_alfa
      real    :: rv_rel_par
      real    :: rv_tolerance
      real    :: rv_rel_tolerance
      real    :: rv_lim_rel_par
      real    :: rv_lim_tolerance
      real    :: rv_lim_rel_tolerance

! *** Variable explanation and Default values:
! ***
! *** bv_adjust = .true.           ! Deciding whether adjustment of the first guess field
! ***                              ! is to be performed.
! ***
! *** iv_method_alfa = 2           ! Selection of the applied alfa-values:
! ***                              ! 1 = Apply values as defined in MATHEW.
! ***                              ! 2 = Apply values as defined in WINDS.
! ***                              ! 4 = Apply constant alfa-value.
! ***
! *** rv_constant_alfa = 1.0       ! The constant Alfa-value applied if "iv_method_alfa" 
! ***                              ! is equal to 4.
! ***
! *** iv_max_iter          = 500   ! The maximum number iterations applied for the 
! ***                              ! iterative SOR-procedure.
! *** iv_lim_iter          = 300   ! The number of iterations that are applied with the
! ***                              ! most restrictive absolute tolerance, rv_tolerance.
! *** rv_rel_par           = 1.78  ! Value of the SOR relaxation parameter for the first
! ***                              ! "iv_lim_iter" iterations.
! *** rv_tolerance         = 0.01  ! Abs tolerance for the first "iv_lim_iter" iterations.
! *** rv_rel_tolerance     = 0.01  ! Rel tolerance for the first "iv_lim_iter" iterations.
! *** rv_lim_rel_par       = 1.0   ! Value of the SOR relaxation parameter after 
! ***                              ! "iv_lim_iter" iterations.
! *** rv_lim_tolerance     = 0.02  ! Abs tolerance after "iv_lim_iter" iterations.
! *** rv_lim_rel_tolerance = 0.02  ! Rel tolerance after "iv_lim_iter" iterations.

! *** iv_method_adjust = 1         ! Deciding wheter the vertical OMEGA-values are to 
! ***                              ! calculated from the continuity equation or directly 
! ***                              ! from the LAMBDA-values:
! ***                              ! 1 = from the continuity eq.
! ***                              ! 2 = from the LAMBDA-values.


! *** Initializing the variables declared above in: "module_mc_adjust_domain"

      adjust            = bv_adjust
      method_alfa       = iv_method_alfa
      constant_alfa     = rv_constant_alfa
      max_iter          = iv_max_iter
      lim_iter          = iv_lim_iter
      rel_par           = rv_rel_par
      tolerance         = rv_tolerance
      rel_tolerance     = rv_rel_tolerance
      lim_rel_par       = rv_lim_rel_par
      lim_tolerance     = rv_lim_tolerance
      lim_rel_tolerance = rv_lim_rel_tolerance
      method_adjust     = iv_method_adjust

      END SUBROUTINE SendAdjustAdvanced

!****************************************************************************************

!     End of module module_mc_wind_adjust
      end module module_mc_wind_adjust
