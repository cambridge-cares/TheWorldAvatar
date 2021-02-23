! <mod_psrc.f90 - A component of the City-scale
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

      module mod_psrc

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
!           2016  M. Karl: Function ftop from Episode-routine ftop.for included
!
! ----------------------------------------------------------------------------------

      integer mq
      integer mp
      integer mqvec

! mq    - Maximum number of point sources
! mp    - Maximum number of plume segments
! mqvec - Maximum number of plume compounds with emission

      character(len=256) :: newpfn
      character(len=256) :: oldpfn
      character(len=256) :: psrcfn

! newpfn - New plume segments data filename
! oldpfn - Old plume segments data filename
! psrcfn - Point sources filename

      integer :: newpun
      integer :: oldpun
      integer :: psrcun

! newpun - New plume segments data fileunit
! oldpun - Old plume segments data fileunit
! psrcun - Point sources fileunit

      logical :: newpfe
      logical :: oldpfe
      logical :: psrcfe

! newpfe - New plume segments data file exists
! oldpfe - Old plume segments data file exists
! psrcfe - Point sources file exists

      real :: newpfv
      real :: oldpfv
      real :: psrcfv

! newpfv - New plume segments data file value
! oldpfv - Old plume segments data file value
! psrcfv - Point sources file value

      integer :: newpfm
      integer :: oldpfm
      integer :: psrcfm

! newpfm - New plume segments data file format (index)
! oldpfm - Old plume segments data file format (index)
! psrcfm - Point sources file format (index)

      real,allocatable :: qdi(:)
      real,allocatable :: qem(:,:)
      real,allocatable :: qhb(:)
      real,allocatable :: qhe(:)
      real,allocatable :: qhs(:)
      real,allocatable :: qps(:)
      real,allocatable :: qsy(:)
      real,allocatable :: qsz(:)
      real,allocatable :: qte(:)
      real,allocatable :: qtg(:)
      real,allocatable :: qvg(:)
      real,allocatable :: qwb(:)
      real,allocatable :: qx(:)
      real,allocatable :: qy(:)
      real,allocatable :: qz(:)
      
!!!===============================================
!!!  for moving point source, by Kang Dec.10,2019
!!   further changed for circular moving route @ Jan 21, 2020 by Kang  
!! 
      real,allocatable :: qx1(:)   !! used for storing real point source location
      real,allocatable :: qy1(:)   
      real,allocatable :: vel_point(:)  !!! point source moving velocity (m/s)
      real,allocatable :: dd_point(:)         !!! point source moving direction (deg).  0-360.  0, moving north, 180, moving south
      real,allocatable :: t_mpoint1(:)  !!! point source start to moving (s) in current hour.  (0-3600)
      real,allocatable :: t_mpoint2(:)  !!! point source stop to moving (s)

      real :: t_point           !!! current simluation time in current hour (0-3600)
      logical :: point_moving   !!! if any point source will move.  True: will move.

      real,allocatable :: cir_ang(:)   !! used for moving point source at circular route (deg).  0-360
!!!===============================================
!!       
! qdi - Stack diameter (m) (nq)
! qhb - Stack height of building (nq)
! qem - Stack emission rate (g/s or kg/h) (nq,nc)
! qhe - Stack emission height (including plume rise) (m) (nq)
! qhs - Stack physical height (above ground level) (m) (nq)
! qps - Stack degree of penetration (0-1) (nq)
! qsy - Stack plume segment initial sigma-y (m) (nq)
! qsz - Stack plume segment initial sigma-z (m) (nq)
! qte - Stack thermal power (MW) (nq)
! qtg - Stack gas temperature (C or K) (nq)
! qvg - Stack gas velocity (m/s) (nq)
! qwb - Stack width of building (nq)
! qx  - Stack x-coordinate (nq)
! qy  - Stack y-coordinate (nq)
! qz  - Stack z-coordinate (height above sea level) (m) (nq)

      
      integer :: nq

! nq  - Number of stacks

!MSK      character(len=256),allocatable :: qid(:)
      character(len=10),allocatable :: qid(:)

! QID - Point source identifier (nq)

      integer,allocatable :: change(:)
      integer             :: chanq
      integer,allocatable :: iqc(:)
      integer             :: iqt
      integer             :: iqu
      integer,allocatable :: jvec(:)
      integer             :: mjvec
      integer             :: nqvec

! change - Point source emission change vector (nq)
! chanq  - Point source emission change numbers
! iqc    - Index of emission column (nc)
! iqt    - Emission temperatur unit 1: C  , 2: K
! iqu    - Emission rate ..... unit 1: g/s, 2: kg/h, 3: t/a
! jvec   - Contains the compound indices (nqvec)
! mjvec  - Maximum index value used in JVEC
! nqvec  - Number of point source compounds

      integer :: chtime(6)
      logical :: ronce

! chtime - Time for next change of point source data
! ronce  - If point source file is read once then true else false

      real,allocatable :: pu(:,:)

! pu  1        - Plume segment x-coordinate in main grid (m) (10+2*nc,np)
! pu  2        - Plume segment y-coordinate in main grid (m) (10+2*nc,np)
! pu  3        - Plume segment sigma-y (m) (10+2*nc,np)
! pu  4        - Plume segment sigma-z (m) (10+2*nc,np)
! pu  5        - Plume segment time since released (s) (10+2*nc,np)
! pu  6        - Plume segment length (m) (10+2*nc,np) (10+2*nc,np)
! pu  7        - Plume segment height above ground (m) (10+2*nc,np)
! pu  8        - Plume segment wind direction (deg) (10+2*nc,np)
! pu  9        - Plume segment index of point source (10+2*nc,np)
! pu 10        - Plume segment index of vertical layer (10+2*nc,np)
! pu 11,13,... - Plume segment emission rates (g/s) (10+2*nc,np)
! pu 12,14,... - Plume segment masses (g) (10+2*nc,np)

      integer :: np

! np - Number of plume segments

      real             :: rdl
      real             :: ylf
      real,allocatable :: zlf(:)

! rdl - Plume segment redirection limit (deg)
! ylf - Plume segment Y limit fraction of min(DX,DY)
! zlf - Plume segment Z limit fraction of DZ(1:MZ) (nz)

      integer :: psrcmtype

! psrcmtype - Point sources subgrid model type

      integer :: psrcai

! psrcai - Point sources subgrid model add to results indicator

      real :: psrcffmin

! psrcffmin - Point sources subgrid model minimum windspeed (m/s)

      integer :: psrcacciq
      integer :: psrcaccbdat(6)
      integer :: psrcaccedat(6)

      real,allocatable :: psrcaccemi(:)

! psrcacciq   - Accident index of point source
! psrcaccbdat - Accident begin date and time (year,month,day,hour,minute,second)
! psrcaccedat - Accident end   date and time (year,month,day,hour,minute,second)
! psrcaccemi  - Accident emissions (g/s)

!MSK      character(len=256),allocatable :: cmpvec(:)
      character(len=10),allocatable :: cmpvec(:)

! cmpvec - Compound identification vector (nqvec)

      real qhaifa(99,99)
      character(len=256) qhaifafn

! qhaifa   - Haifa emission data read in from file
! qhaifafn - Haifa emission data filename

      contains

!MSK included function ftop from ftop.for

      REAL FUNCTION FTOP(DIST)

! The function calculates a topography height scaling factor

! Scalar arguments

        REAL DIST

! DIST - distance relative to point source stack height

! Local variables

        REAL DVAL
        REAL DVEC(7)
        REAL FVEC(7)
        REAL W

        INTEGER I
        INTEGER J

        DATA DVEC/0.0,2.0,4.0,5.0,10.0,20.0,30.0/
        DATA FVEC/0.8,0.8,0.6,0.5, 0.3, 0.1, 0.0/

! DVAL - Distance
! DVEC - Distance vector
! FVEC - Function vector
! W    - Weight
! I    - Index
! J    - Index

! If too far from source then no topographical influence

        IF (DIST .GE. DVEC(7)) THEN
            FTOP = 0.
            RETURN
        ENDIF

! Find interval

        DO 100 I = 2,7
            IF (DIST .LE. DVEC(I)) THEN
                J = I
                GOTO 110
            ENDIF
  100   CONTINUE

  110   CONTINUE

! Calculate topographical height scaling factor

        W = (DIST - DVEC(J - 1))/(DVEC(J) - DVEC(J - 1))
        DVAL = (1. - W)*DVEC(J - 1) + W*DVEC(J)
        FTOP = (1. - W)*FVEC(J - 1) + W*FVEC(J)

        RETURN

! End of real function FTOP

      END FUNCTION



      subroutine FreePsrcMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreePsrcMemory' :: FreePsrcMemory

      implicit none

! Local variables

      integer i
      integer j

      if (allocated(qdi))        deallocate(qdi)
      if (allocated(qem))        deallocate(qem)
      if (allocated(qhb))        deallocate(qhb)
      if (allocated(qhe))        deallocate(qhe)
      if (allocated(qhs))        deallocate(qhs)
      if (allocated(qps))        deallocate(qps)
      if (allocated(qsy))        deallocate(qsy)
      if (allocated(qsz))        deallocate(qsz)
      if (allocated(qte))        deallocate(qte)
      if (allocated(qtg))        deallocate(qtg)
      if (allocated(qvg))        deallocate(qvg)
      if (allocated(qwb))        deallocate(qwb)
      if (allocated(qx))         deallocate(qx)
      if (allocated(qy))         deallocate(qy)
      if (allocated(qz))         deallocate(qz)
     

      if (allocated(qid))        deallocate(qid)
      
      if (allocated(change))     deallocate(change)
      if (allocated(iqc))        deallocate(iqc)
      if (allocated(jvec))       deallocate(jvec)
      
      if (allocated(pu))         deallocate(pu)
      
      if (allocated(zlf))        deallocate(zlf)
      
      if (allocated(psrcaccemi)) deallocate(psrcaccemi)
      
      if (allocated(cmpvec))     deallocate(cmpvec)



!!!===============================================
!!!  for moving point source, by Kang Dec.10,2019
!! 
          IF (.NOT. ALLOCATED(QX1))     deallocate(QX1)
          IF (.NOT. ALLOCATED(QY1))     deallocate(QY1)
          IF (.NOT. ALLOCATED(vel_point))     deallocate(vel_point)
          IF (.NOT. ALLOCATED(dd_point))     deallocate(dd_point)
          IF (.NOT. ALLOCATED(t_mpoint1))     deallocate(t_mpoint1)
          IF (.NOT. ALLOCATED(t_mpoint2))     deallocate(t_mpoint2)  

          IF (.NOT. ALLOCATED(cir_ang))     deallocate(cir_ang) 
!!!===============================================

      mq = 0
      mp = 0
      mqvec = 0
      newpfn = ' '
      oldpfn = ' '
      psrcfn = ' '
      newpun = 0
      oldpun = 0
      psrcun = 0
      newpfe = .false.
      oldpfe = .false.
      psrcfe = .false.
      newpfv = 0.
      oldpfv = 0.
      psrcfv = 0.
      newpfm = 0
      oldpfm = 0
      psrcfm = 0
      nq = 0
      chanq = 0
      iqt = 0
      iqu = 0
      mjvec = 0
      nqvec = 0
      chtime = 0
      ronce = .false.
      np = 0
      rdl = 0.
      ylf = 0.
      psrcmtype = 0
      psrcai = 0
      psrcffmin = 0.
      psrcacciq = 0
      psrcaccbdat = 0
      psrcaccedat = 0

      do j = 1,99
      do i = 1,99
        qhaifa(i,j) = 0.
      enddo
      enddo

      qhaifafn = ' '

!!!===============================================
!!!  for moving point source, by Kang Dec.10,2019
      point_moving = .False. 
!!!===============================================

! End of subroutine FreePsrcMemory

      end subroutine

      end module mod_psrc
