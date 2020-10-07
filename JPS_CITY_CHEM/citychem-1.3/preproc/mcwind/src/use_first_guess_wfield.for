! <use_first_guess_wfield.for - A component of the City-scale
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

      subroutine use_first_guess_wfield

!MSK !DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'use_first_guess_wfield' :: use_first_guess_wfield

!**********************************************************************
!
! This routine inserts the first guess 3D wind field, that is:
! u0(0:im,jm,km) v0(im,0:jm,km), w0(im,jm,0:km) and omega0(im,jm,0:km), 
! directly into the final 3D wind field arrays, i.e.,
!   u(0:im,jm,km) v(im,0:jm,km), w(im,jm,0:km) and omega(im,jm,0:km).

! Modifications:
!     21 Jan 2019  M. Karl: Commented pre-processor directive ATTRIBUTES
!     29 Jan 2019  M. Karl: replace TAB by spaces
!
!***********************************************************************
! If needed, insert the necessary MODULE calls:

      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_winds
      use module_mc_wind_adjust

!***********************************************************************

! We require the use of IMPLICIT NONE:

      implicit none

! Global declarations:


! Local declarations:

      integer :: i,j,k

! Content of the routine:

! Initialize arrays to zero:

!      u     = u0
!      v     = v0
!      w     = w0
!	omega = omega0


      do k = 1,km
        do j = 1,jm
          do i = 0,im

            u(i,j,k) = u0(i,j,k)

          end do
        end do
      end do

      do k = 1,km
        do j = 0,jm
          do i = 1,im

            v(i,j,k) = v0(i,j,k)

          end do
        end do
      end do

      do k = 0,km
        do j = 1,jm
          do i = 1,im

            w(i,j,k)     = w0(i,j,k)
            omega(i,j,k) = omega0(i,j,k)

          end do
        end do
      end do


      if (fe_log) then
        max_u0     = MAXVAL(u0)
        max_v0     = MAXVAL(v0)
        max_w0     = MAXVAL(w0)
        max_omega0 = MAXVAL(omega0)

        write(funit_log,*)
        write(funit_log,'(28X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max u0: ',max_u0,MAXLOC(u0)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max v0: ',max_v0,MAXLOC(v0)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max w0: ',max_w0,MAXLOC(w0)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max omega0: ',max_omega0,MAXLOC(omega0)

        min_u0     = MINVAL(u0)
        min_v0     = MINVAL(v0)
        min_w0     = MINVAL(w0)
        min_omega0 = MINVAL(omega0)

        write(funit_log,*)
        write(funit_log,'(28X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min u0: ',min_u0,MINLOC(u0)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min v0: ',min_v0,MINLOC(v0)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min w0: ',min_w0,MINLOC(w0)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min omega0: ',min_omega0,MINLOC(omega0)

        max_u         = MAXVAL(u)
        max_v         = MAXVAL(v)
        max_w         = MAXVAL(w)
        max_omega     = MAXVAL(omega)

        write(funit_log,*)
        write(funit_log,'(28X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max u : ',max_u,MAXLOC(u)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max v : ',max_v,MAXLOC(v)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max w : ',max_w,MAXLOC(w)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max omega : ',max_omega,MAXLOC(omega)

        min_u         = MINVAL(u)
        min_v         = MINVAL(v)
        min_w         = MINVAL(w)
        min_omega     = MINVAL(omega)

        write(funit_log,*)
        write(funit_log,'(28X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min u : ',min_u,MINLOC(u)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min v : ',min_v,MINLOC(v)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min w : ',min_w,MINLOC(w)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min omega : ',min_omega,MINLOC(omega)

! Calculate and print out the maximum and minimum divergence in
! the applied initial (first guess) wind field:

        call calculate_3D_divergence(im,jm,km,dx,dy,delta_sigma, &
                                depth,depthu,depthv,u0,v0, &
                                omega0,div0_3d)

        write(funit_log,*)
        write(funit_log,'(40X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A28,E10.4,3I4)') &
         'Max. initial 3D-divergence: ', &
         MAXVAL(div0_3D),MAXLOC(div0_3D)
        write(funit_log,'(1X,A28,E10.4,3I4)') &
         'Min. initial 3D-divergence: ', &
         MINVAL(div0_3D),MINLOC(div0_3D)

! Calculate and print out the maximum and minimum divergence in
! the final adjusted wind field:

        call calculate_3D_divergence(im,jm,km,dx,dy,delta_sigma, &
                                depth,depthu,depthv,u,v, &
                                omega,div_3d)

        write(funit_log,*)
        write(funit_log,'(40X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A28,E10.4,3I4)') &
         'Max. final 3D-divergence: ', &
         MAXVAL(div_3D),MAXLOC(div_3D)
        write(funit_log,'(1X,A28,E10.4,3I4)') &
         'Min. final 3D-divergence: ', &
         MINVAL(div_3D),MINLOC(div_3D)
        write(funit_log,*)
        write(funit_log,'(1X,A,E10.4)') &
         'Final 3D-divergence(11,9,1): ',div_3D(11,9,1)
        write(funit_log,*)
      end if  ! (fe_log)

      return
      end subroutine use_first_guess_wfield
