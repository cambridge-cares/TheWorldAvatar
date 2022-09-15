! <csublr.f90 - A component of the City-scale
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

      subroutine CSUBLR(ICV,IDV,IWV)

! The subroutine calculates integrated line source concentrations and
! dry and wet deposition in all irregular receptor points.
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
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_lsrc

      implicit none

      INTEGER ICV
      INTEGER IDV
      INTEGER IWV

! ICV - Update concentration  indicator
! IDV - Update dry deposition indicator
! IWV - Update wet deposition indicator

! Local variables:

!_LHS_TEST_14Dec2011_Start:
      REAL :: sc_lsrc_contr = 1.0
!       REAL :: sc_lsrc_contr = 0.5      
!_LHS_TEST_14Dec2011_End.      

      REAL,ALLOCATABLE :: CRV(:)
      REAL,ALLOCATABLE :: DDRV(:)
      REAL DM
      REAL RMAXRV
      REAL,ALLOCATABLE :: WDRV(:)
      REAL XM
      REAL XRV
      REAL X1
      REAL X2
      REAL YM
      REAL YRV
      REAL Y1
      REAL Y2
      REAL ZRV

!MSK start
      real  :: dxl,dyl
      real  :: PDM,PDM1,PDM2,PDM3
      real  :: XML1,XML2,XML3
      real  :: YML1,YML2,YML3
!MSK end

      INTEGER IC
      INTEGER IQL
      INTEGER IR

! CRV    - Receptor concentrations
! DDRV   - Receptor dry depositions
! DM     - Minimum distance receptor to line source
! RMAXRV - Line source influence maximum distance
! WDRV   - Receptor wet depositions
! XM     - Line source minimum distance x-coordinate
! XRV    - Receptor x-coordinate
! X1     - Line source end point x-coordinate
! X2     - Line source end point x-coordinate
! YM     - Line source minimum distance y-coordinate
! YRV    - Receptor y-coordinate
! Y1     - Line source end point y-coordinate
! Y2     - Line source end point y-coordinate
! ZRV    - Receptor z-coordinate
! IC     - Compound index
! IQL    - Line source index
! IR     - Receptor point index

! Allocate memory

      IF (.NOT. ALLOCATED(CRV))  ALLOCATE(CRV(MC))
      IF (.NOT. ALLOCATED(DDRV)) ALLOCATE(DDRV(MC))
      IF (.NOT. ALLOCATED(WDRV)) ALLOCATE(WDRV(MC))

! Go through all line sources
      !print *,'csublr: Loop NQL x NR is: ', NQL * NR

      DO 100 IQL = 1,NQL

! Get line source end coordinates

      X1 = QLX(IQL,1)
      Y1 = QLY(IQL,1)
      X2 = QLX(IQL,2)
      Y2 = QLY(IQL,2)

! Get line source maximum influence distance

      RMAXRV = QLRMAX(IQL)

! Go through all irregular receptor points

      DO 110 IR = 1, NR    ! 10, 10 (68HB)

! Get receptor point coordinates

          XRV = XR(IR)
          YRV = YR(IR)
          ZRV = ZR(IR)

! Calculate minimum distance from receptor point to line source

          call distrl(XRV,YRV,X1,Y1,X2,Y2,XM,YM,DM)

        !  if (DM.lt.300.0) then
        !    print *,'perp distance to line',IR,IQL,XRV,X1,X2,YRV,Y1,Y2,DM
        !  endif

!MSK start debugging
!MSK_debug primitive method to calculate the distance of the point to the line:
!MSK_debug          dxl= ABS(X2-X1)
!MSK_debug          dyl= ABS(Y2-Y1)
!MSK_debug          if ((dxl.eq.0.).and.(dyl.eq.0.)) then
!MSK_debug            PDM = -999.
!MSK_debug          else
!MSK_debug Find mid-point of line source
!MSK_debug            XML1 = X1 + 0.5*(X2-X1)
!MSK_debug            YML1 = Y1 + 0.5*(Y2-Y1)
!MSK_debug And two other points
!MSK_debug            XML2 = X1 + 0.25*(X2-X1)
!MSK_debug            YML2 = Y1 + 0.25*(Y2-Y1)
!MSK_debug            XML3 = X1 + 0.75*(X2-X1)
!MSK_debug            YML3 = Y1 + 0.75*(Y2-Y1)
!MSK_debug Find distance between receptor and line points
!MSK_debug            PDM1 = SQRT( (XML1-XRV)*(XML1-XRV) + (YML1-YRV)*(YML1-YRV) )
!MSK_debug            PDM2 = SQRT( (XML2-XRV)*(XML2-XRV) + (YML2-YRV)*(YML2-YRV) )
!MSK_debug            PDM3 = SQRT( (XML3-XRV)*(XML3-XRV) + (YML3-YRV)*(YML3-YRV) )
!MSK_debug Chose the minimum of the three distances
!MSK_debug            PDM  = MIN(PDM1,PDM2)
!MSK_debug            PDM  = MIN(PDM ,PDM3)
!MSK_debug          endif
!MSK_debug          if ((PDM.gt.0.).and.(PDM.lt.300.0)) then
!MSK_debug            print *,'perp distance to line',IR,IQL,XRV,X1,X2,XML1,YRV,Y1,Y2,YML1,PDM,DM
!MSK_debug          endif
!MSK end debugging

! If minimum distance is too large then goto next receptor point

          IF (DM .GT. RMAXRV) GOTO 110

! Calculate concentration and dry and wet deposition

          call csubl(IQL,XRV,YRV,ZRV,NC,MC,CRV,DDRV,WDRV)


! Go through all compounds

          DO 120 IC = 1,NC

! Add concentration  to current receptor point

!_LHS_TEST_14Dec2011_Start:

          !print *,'csublr: ',IC,ICV,CR(IC,IR),CRV(IC)

          
          IF (ICV .EQ. 1)  CR(IC,IR) =    CR(IC,IR) +         &
                          (sc_lsrc_contr * CRV(IC))

!_LHS_TEST_14Dec2011_End.

! Add dry deposition to current receptor point

          IF (IDV .EQ. 1) DDEPR(IC,IR) = DDEPR(IC,IR) + DDRV(IC)

! Add wet deposition to current receptor point

          IF (IWV .EQ. 1) WDEPR(IC,IR) = WDEPR(IC,IR) + WDRV(IC)

! Next compound

  120 CONTINUE

! Next irregular receptor point

  110 CONTINUE

! Next line source

  100 CONTINUE


! Deallocate memory

      IF (ALLOCATED(CRV))  DEALLOCATE(CRV)
      IF (ALLOCATED(DDRV)) DEALLOCATE(DDRV)
      IF (ALLOCATED(WDRV)) DEALLOCATE(WDRV)

      RETURN

! End of subroutine CSUBLR

      end subroutine csublr
