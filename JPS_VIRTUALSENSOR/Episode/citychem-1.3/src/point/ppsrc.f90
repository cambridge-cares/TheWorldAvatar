! <ppsrc.f90 - A component of the City-scale
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

      subroutine PPSRC(IQ,QIDV,QXV,QYV,QZV,QHSV,QDIV,QHBV,QWBV,QTEV,      &
                      QTGV,QVGV,MQEM,NQEM,QEMVEC)

! The subroutine performs processing on point source data.
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
!           2016  M. Karl: conversion from num/s to g/s  (num = number of particles)
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_psrc

      implicit none

      integer :: IQ
      integer :: MQEM
      integer :: NQEM

!MSK      CHARACTER(len=256) QIDV
      character(len=10) QIDV

      real :: QXV
      real :: QYV
      real :: QZV
      real :: QHSV
      real :: QDIV
      real :: QHBV
      real :: QWBV
      real :: QTEV
      real :: QTGV
      real :: QVGV
      real :: QEMVEC(MQEM)

! Local variables
 
      real :: LON0
      real :: QEMV
      real :: QHEV
      real :: QPSV
      real :: QSYV
      real :: QSZV
      real :: UTME
      real :: UTMN
      real :: QXVV
      real :: QYVV

      integer :: I
      integer :: I1
      integer :: I2
      integer :: IC
      integer :: J
      integer :: NPOS

      logical :: LEOF

! Comments out by RuO - 10.01.2008
! Rotate point source main grid coordinates (m)

!      QXVV = +COS(ANGLE*RAD)*(QXV - SITEX0) +
!     .        SIN(ANGLE*RAD)*(QYV - SITEY0) + SITEX0
!      QYVV = -SIN(ANGLE*RAD)*(QXV - SITEX0) +
!     .        COS(ANGLE*RAD)*(QYV - SITEY0) + SITEY0
!
!      QXV = QXVV
!      QYV = QYVV

! If the diameter or velocity is missing then the point source is
! converted to a volume source

      IF (QDIV .LE. 0.0 .OR. QVGV .LE. 0.0) THEN

! If building height is missing use stack height

          IF (QHBV .LE. 0.0) QHBV = QHSV

! If building width is missing use building height

          IF (QWBV .LE. 0.0) QWBV = QHBV

! Calculate stack height and effective emission height

!          QHSV = QHBV/2.0
!          QHEV = QHBV/2.0

!	BRUCE 10032005
!	SET THESE TO THEIR ORIGINAL VALUE DUE TO INPUFF PROBLEMS

          QHSV = QHSV
          QHEV = QHSV

! Calculate plume segment initial sigma-y and sigma-z

          QSYV = QWBV/2.0
          QSZV = QHBV/2.0

      ELSE

! Tentative value of final emission height

          QHEV = QHSV

! Calculate plume segment initial sigma-y and sigma-z

          QSYV = QDIV
          QSZV = QDIV

      ENDIF


! Calculate gas temperature (K)

      IF (QTGV .NE. MISS .AND. IQT .EQ. 1) QTGV = QTGV + CTOK

! Set point source identification
      
      QID(IQ) = QIDV
!_23May2011:      IF (MESSFE) WRITE (MESSUN,2060) IQ,(QIDV(J:J),J=1,30)
!_18Oct2011:      IF (MESSFE) WRITE (MESSUN,2060) IQ,(QIDV(J:J),J=1,10)
      
! Set point source main grid coordinates (m)

      QX(IQ) = QXV
      QY(IQ) = QYV
      QZ(IQ) = QZV

! Set point source height (m) and diameter

      QHS(IQ) = QHSV
      QDI(IQ) = QDIV

! Set point source building height (m) and width (m)

      QHB(IQ) = QHBV
      QWB(IQ) = QWBV
      
! Set point source gas temperature (K) and velocity (m/s)
      
      QTG(IQ) = QTGV
      QVG(IQ) = QVGV
      
! Set point source thermal energy (MW)
      
      QTE(IQ) = 0.

! Set point source initial sigma-y and sigma-z

      QSY(IQ) = QSYV
      QSZ(IQ) = QSZV

! Set point source tentative emission height. For point sources
! that are not volume sources, this will be recalculated later to
! become final emission height taking into account temperature and
! velocity of the gas.

      QHE(IQ) = QHEV

! Set point source tentative degree of penetration. For point
! sources that are not volume sources, this will be recalculated
! later to become final degree of penetration.

      QPS(IQ) = 0.

      !print *,'ppsrc: qhev',IQC(2),QEMVEC(IQC(2))

! Calculate and set point source emission rates (g/s)

      DO 260 IC = 1,NC

          IF (IQC(IC) .GT. 0) THEN
              QEMV = QEMVEC(IQC(IC))
          ELSE
              QEMV = 0.
          ENDIF

!MSK start HERE IS THE PLACE FOR UNIT CONVERSION OF EMISSIONS
! iqu    - Emission rate ..... unit 1: g/s, 2: kg/h, 3: t/a, 4: num/s

!MSK comment:   if iqu==1 nothing to do because emission in psrc file is in g/s 

          IF (IQU .EQ. 2) QEMV = QEMV/3.6
          IF (IQU .EQ. 3) QEMV = (1.0E+6/(365.25*24.*3600.))*QEMV

! conversion from num/s to g/s   (num = number of particles)
! emission rate in num/s has to be derived from g/s in the pointsrc input
!                dens_p = 1 g(particles)/cm3
!                V_p    = 4.2e-21 m3    (d_p = 200 nm = 2.e-7 m)
!                m_p    = 4.2e-15 g(particles)   = dens_p*V_p
!                Rate   = 1 g(particles)/s
!                num_p  = 1 g / m_p = 2.39e14 num
! how much particles/s are emitted?
! scale factor = num/s *F = g/s;   F = 4.2e-15
!
! MSK new comment 23.01.2017
! Conversion from num/s to g/s   (num = number of particles)
! Number emission has the right time unit.
! We only have to take care that concentration CRV is not ug/m^3
! but number/m^3. That is in csubp.f90
          IF ((IQU .EQ. 4).or.(CUNIT(IC)(1:3) == 'num') ) then
            QEMV = QEMV * 1.0
          ENDIF
!MSK end

          QEM(IQ,IC) = QEMV

          !if ( (iq==1).and.(ic==2) ) then
          !    print *,'ppsrc qem iq,ic',iq,ic,IQU,QEM(IQ,IC),QEMV
          !endif

  260 CONTINUE  

      RETURN

 2060 format ('PPSRC: Included point source number ',I3,      &
           ' with ID: ',256A1)

! End of subroutine PPSRC

      end subroutine ppsrc
