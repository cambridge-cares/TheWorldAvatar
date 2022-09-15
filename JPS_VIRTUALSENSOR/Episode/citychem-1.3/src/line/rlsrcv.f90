! <rlsrcv.f90 - A component of the City-scale
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

      subroutine RLSRCV

! The subroutine reads line sources data from file for the current
! simulation period.
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
!           2016  M. Karl: NOTE - unit of line source emission is g/(s*m) or num/(s*m)
!                 i.e: amount per second and road meter
!                 TAPM lse files use unit g/s. Therefore, the length of the road link is
!                 calculated and LSRC emissions are divided by the road link length.
!                 Assume that both lanes have same emission value QL1V*0.5
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_lsrc
      use mod_phot

      implicit none

! Local variables

      real, allocatable :: QL1V(:)
!MSK      REAL,ALLOCATABLE :: QL2V(:)
      real :: SW0V

      integer :: IC
      integer :: IQLV
      integer :: IQLVV
      integer :: NQLV
      integer :: YEARV
      integer :: MNTHV
      integer :: DAYMV
      integer :: HOURV

      logical :: LEOF

!MSK start
      character(len=256) :: TXTSTR
!MSK end

! QL1V  - Line source emission on lane 1
! QL2V  - Line source emission on lane 2
! SW0V  - Line source sigma-w
! IC    - Compound index
! IQLV  - Line source index
! IQLVV - Line source index
! NQLV  - Number of line sources    = NQL
! YEARV - Year
! MNTHV - Month
! DAYMV - Day
! HOURV - Hour
! LEOF  - If end of file then true else false

! Allocate memory

      IF (.NOT. ALLOCATED(QL1V)) ALLOCATE(QL1V(NC))
!MSK start
!MSK      IF (.NOT. ALLOCATED(QL2V)) ALLOCATE(QL2V(NC))
      if (NQL .gt. 0) then
        IF (.NOT. ALLOCATED(QL)) ALLOCATE(QL(NC,NQLV,NQLL))
      endif
!MSK end


!MSK start
      IF (LONCE) THEN

          GOTO 50

      ENDIF

        DO 60 IC = 1,NC

         IF (LSRVFE(IC)) THEN
! Read header from line source emission data

           CALL NXTDAT(LSRVUN(IC),LEOF)
           READ (LSRVUN(IC),*) TXTSTR

         ENDIF 
  60    CONTINUE


! Finished reading file heading

      LONCE = .TRUE.

  50  CONTINUE
!MSK end

! Read line sources data

        DO 100 IC = 1,NC

        IF (LSRVFE(IC)) THEN

          DO 110 IQLV = 1,NQL

              CALL NXTDAT(LSRVUN(IC),LEOF)
!MSK              READ (LSRVUN,*,END = 999) IQLVV,SW0V,       &
!MSK                                       (QL1V(IC),QL2V(IC),IC=1,NC)
!MSK one file for each compound
!MSK              READ (LSRVUN(IC),*,END = 999) IQLVV,SW0V,QL1V(IC),QL2V(IC)
!MSK line source files contain only one value = QL1V
              READ (LSRVUN(IC),*,END = 999) QL1V(IC)

!MSK start
!MSK here would be the place to scale all line source emissions equally
              if ( (CMPND(IC) == 'NO        ').or.(CMPND(IC) == 'NO2       ') ) then 
                QL1V(IC) = QL1V(IC) * LSRCSCALE
              endif
!MSK end
             
! Define line source emissions

!MSK: line source emission values QL should be in g/(s*m) or num/(s*m)
!MSK: but the TAPM lse files have unit g/s. The unit of QL values is corrected
!MSK: in lsgrid.f90 and in csubl.f90
!MSK: assume that both lanes have same emission value QL1V*0.5
              QL(IC,IQLV,1) = QL1V(IC) *0.5
              QL(IC,IQLV,2) = QL1V(IC) *0.5

!MSK start debug
!MSK debug            if (IC==2) then 
!MSK debug              print *,'rlsrcv NO ic,iql',IC,IQLV, QL1V(IC),QL(IC,IQLV,1)
!MSK debug            endif
!MSK end debug
              
! Define line source turbulence sigma-w

!MSK start
! SW0V is not included in the line source emission file
! use zero as default value (m) initial vertical turbulence: sigma-y0 = 3.0m
              SW0V = 3.00
!MSK end
              QLSW0(IQLV) = SW0V

! Define line source wet reduction factor

              QLWREDFAC(IQLV) = 0.

! Next line source

  110      CONTINUE    ! iqlv-loop


      ELSE

!MSK start  no ic-file means no lsrc emission for ic
          do IQLV = 1,NQL
              QL(IC,IQLV,1) = 0.0
              QL(IC,IQLV,2) = 0.0
              QLSW0(IQLV)   = 3.0
              QLWREDFAC(IQLV) = 0.
          enddo
!MSK end


      ENDIF


  100    CONTINUE    ! ic-loop

  
         IF (MESSFE) WRITE (MESSUN,2000)       &
                    MOD(YEAR,100),MNTH,DAYM,HOUR

! Deallocate memory

         IF (ALLOCATED(QL1V)) DEALLOCATE(QL1V)
!MSK          IF (ALLOCATED(QL2V)) DEALLOCATE(QL2V)

         RETURN

         
  999    CONTINUE
      IF (MESSFE) WRITE (MESSUN,2010)
      CALL STOPIT('RLSRCV: End of line sources variable file!')

 1000 format(A256)
 2000 format('RLSRCV: Read line sources emis. from file for time ',       &
            4I2.2)
 2010 format('RLSRCV: End of line sources variable file!')

! End of subroutine RLSRCV

      end subroutine rlsrcv
