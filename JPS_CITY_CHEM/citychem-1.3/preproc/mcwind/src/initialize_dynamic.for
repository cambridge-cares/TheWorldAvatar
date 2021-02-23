! <initialize_dynamic.for - A component of the City-scale
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
!**********************************************************************

      subroutine initialize_dynamic

!     This routine computes the original right hand side of the 
!     Poisson equation, g(i,j,k), and calculates the SOR-coefficient 
!     that depends on the dynamic stability parameter "alfa".
!     Moreover, the lambda-values, "lambda(0:im+1,0:jm+1,0:km+1)",
!     are initialized to zero at the end of this routine.

! Modifications:
!    29 Jan 2019  M. Karl: replaced real*8 by double precision
!
!**********************************************************************

      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_winds
      use module_mc_wind_adjust

!     We require the use of IMPLICIT NONE:
      implicit none

!     Local declarations:
      integer :: i,j,k
      double precision  :: t1,t2,t3

!     Content of the routine:
      do k = 1,km
        do j = 1,jm
          do i = 1,im

            t1 = (omega0(i,j,k) - omega0(i,j,k-1))/delta_sigma(k)

            t2 = u0(i,j,k)*depthu(i,j) - u0(i-1,j,k)*depthu(i-1,j)
            t2 = t2/(dx*depth(i,j))

            t3 = v0(i,j,k)*depthv(i,j) - v0(i,j-1,k)*depthv(i,j-1)
            t3 = t3/(dy*depth(i,j))

            g_rhs(i,j,k) =  - t1 - t2 - t3

          end do
        end do
      end do

!     Calculate the coefficient that depends on the dynamic variable
!     'alfa': NOTE that we are utilizing the ground and top boundary
!     condition for k = 1 and k = km, respectively:

      k = 1
      do j = 1,jm
        do i = 1,im

!         Version 1:
!          f1(i,j,k) = ((alfa*h0/depth(i,j))**2) + f1st(i,j,k)
!
!         Version 2:   
          h3(i,j,k)  =  h3st(i,j,k)
          h8(i,j,k)  =  - qk(k)*((alfa*h0/depth(i,j))**2) + h8st(i,j,k)
          h13(i,j,k) =  rk(k)*((alfa*h0/depth(i,j))**2) + h13st(i,j,k)

        end do
      end do

      do k = 2,km-1
        do j = 1,jm
          do i = 1,im

!           Version 1:
!            f1(i,j,k) = ((alfa*h0/depth(i,j))**2) + f1st(i,j,k)
!
!           Version 2:
           
            h3(i,j,k)  =  pk(k)*((alfa*h0/depth(i,j))**2) + h3st(i,j,k)
            h8(i,j,k)  = - qk(k)*((alfa*h0/depth(i,j))**2) + h8st(i,j,k)
            h13(i,j,k) =  rk(k)*((alfa*h0/depth(i,j))**2) + h13st(i,j,k)

          end do
        end do
      end do


      k = km
      do j = 1,jm
        do i = 1,im

!         Version 1:
!          f1(i,j,k) = ((alfa*h0/depth(i,j))**2) + f1st(i,j,k)
!
!         Version 2:        
          h3(i,j,k)  =   pk(k)*((alfa*h0/depth(i,j))**2) + h3st(i,j,k)
          h8(i,j,k)  = - qk(k)*((alfa*h0/depth(i,j))**2) + h8st(i,j,k)
          h13(i,j,k) =   h13st(i,j,k)

        end do
      end do


!=======================================================================
!
!     Initializing the lambda-values for the iterative procedure.
!     Note also that the applied boundary condition on the open
!     lateral boundaries are that the lambdas is set equal to zero.
! 
!      call initialize_lambda(....)

      lambda     = 0.0

!=======================================================================

!     Write the applied value of "alfa" to the log-file:
      if (fe_log) then
        write(funit_log,*)
        write(funit_log,'(1X,A,F8.4)') &
    'The stability dependent parameter: alfa = ',alfa
        write(funit_log,*)

!        write(funit_log,*) 'f1(20,16,1) = ',f1(20,16,1)

!        write(funit_log,*) 'h3(20,16,1)     = ',h3(20,16,1)
!        write(funit_log,*) 'h8(20,16,1)     = ',h8(20,16,1)
!        write(funit_log,*) 'h13(20,16,1)    = ',h13(20,16,1)       
!        write(funit_log,*) 'h3(20,16,km)    = ',h3(20,16,km)
!        write(funit_log,*) 'h8(20,16,km)    = ',h8(20,16,km)
!        write(funit_log,*) 'h13(20,16,km)   = ',h13(20,16,km)
!        write(funit_log,*) 'g_rhs(20,16,1)  = ',g_rhs(20,16,1)
!        write(funit_log,*) 'g_rhs(20,16,km) = ',g_rhs(20,16,km)      

      end if

      return
      end subroutine initialize_dynamic
