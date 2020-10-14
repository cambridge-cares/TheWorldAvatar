! <csubpr.f90 - A component of the City-scale
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

      subroutine CSUBPR(ICV,IDV,IWV,TSV)

! The subroutine calculates subgrid scale plume segment concentrations
! and dry and wet depositions in all irregular receptor points.
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
      use mod_psrc

      implicit none

      real :: TSV
      integer :: ICV
      integer :: IDV
      integer :: IWV

! TSV - Timestep value
! ICV - Update concentration  indicator
! IDV - Update dry deposition indicator
! IWV - Update wet deposition indicator

! Local variables

      real,allocatable :: CRV(:)
      real,allocatable :: DDRV(:)
      real,allocatable :: WDRV(:)
      real :: XRV
      real :: YRV
      real :: ZRV

      integer :: IC
      integer :: IP
      integer :: IR

! CRV  - Receptor  concentrations
! DDRV - Receptor dry depositions
! WDRV - Receptor wet depositions
! XRV  - Receptor x-coordinate
! YRV  - Receptor y-coordinate
! ZRV  - Receptor z-coordinate
! IC   - Compound index
! IP   - Plume segment index
! IR   - Irregular receptor point index

! Allocate memory

      IF (.NOT. ALLOCATED(CRV))  ALLOCATE(CRV(MC))
      IF (.NOT. ALLOCATED(DDRV)) ALLOCATE(DDRV(MC))
      IF (.NOT. ALLOCATED(WDRV)) ALLOCATE(WDRV(MC))

! Go through all plume segments

      DO 100 IP = 1,NP

! Go through all irregular receptor points

      DO 110 IR = 1,NR

! Set receptor coordinates

          XRV = XR(IR)
          YRV = YR(IR)
          ZRV = ZR(IR)

! Calculate concentration and dry and wet deposition

          call csubp(IP,XRV,YRV,ZRV,TSV,NC,MC,CRV,DDRV,WDRV)

! Go through all compounds

          DO 120 IC = 1,NC

! Add concentration  to current receptor point


!!!===============================================
!!! for averaged receptor concentration, by Kang Dec.23,2019
!!! need to be double check 
!!!===============================================
!! orig          IF (ICV .EQ. 1)    CR(IC,IR) =    CR(IC,IR) +  CRV(IC)
            if(averaged_output) then
              IF (ICV .EQ. 1)    CR(IC,IR) =    CR(IC,IR) + (CRV(IC)/FLOAT(NTS))
            else
              IF (ICV .EQ. 1)    CR(IC,IR) =    CR(IC,IR) +  CRV(IC)
            endif
!!!===============================================            
! Add dry deposition to current receptor point

          IF (IDV .EQ. 1) DDEPR(IC,IR) = DDEPR(IC,IR) + DDRV(IC)

! Add wet deposition to current receptor point

          IF (IWV .EQ. 1) WDEPR(IC,IR) = WDEPR(IC,IR) + WDRV(IC)

! Next compound

  120 CONTINUE

! Next irregular receptor point

  110 CONTINUE

! Next plume segment

  100 CONTINUE

! Deallocate memory

      IF (ALLOCATED(CRV))  DEALLOCATE(CRV)
      IF (ALLOCATED(DDRV)) DEALLOCATE(DDRV)
      IF (ALLOCATED(WDRV)) DEALLOCATE(WDRV)

      RETURN

! End of subroutine CSUBPR

      end subroutine csubpr
