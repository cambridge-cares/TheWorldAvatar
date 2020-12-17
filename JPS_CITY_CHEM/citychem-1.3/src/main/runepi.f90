! <runepi.f90 - A component of the City-scale
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

      subroutine RUNEPI

!     The subroutine performs model calculations for one hour.
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

      use mod_asrc
      use mod_conc
      use mod_depo
      use mod_emep03
      use mod_grid
      use mod_lsrc
      use mod_main
      use mod_mete
      use mod_phot
      use mod_psrc
      use mod_site
      use mod_stat
      use mod_time
      use mod_util

      implicit none
!MSK
      integer IC
      real cpu_time_now

!_LHS_New_episode_dll_October_2008_Start:

! Arguments

!MSK      INTEGER picallcnt

!_LHS_New_episode_dll_October_2008_End.

!     Local variables:

!MSK      LOGICAL ATDATE

!     ATDATE - If at given date then true else false

!     Update global call counter

!MSK      callcnt = picallcnt

!     Initially OK to run model

      RUNOK = .TRUE.

!     Go through all timesteps

      ITS = 0
!MSK start
! TSTIME needs DT
      if (ITS .EQ. 0) then
        call CALCDT
      endif
!MSK end



  100 continue

!     Perform one timestep with module time:

      call TSTIME
      !print *,'runepi.for: after TSTIME'

!     Write message to user terminal


!     If at first timestep of current simulation period then
!     write message to the screen

      IF (ITS .EQ. 1) THEN
        WRITE (*,2000) TXTDAY(DAYW),DAYM,TXTMON(MNTH),YEAR,HOUR,     &
                      MINU,SECO,(CMPND(IC)(1:6),IC=1,NC)
        call cpu_time(cpu_time_now)
        write(6,*) 'CPU time start hour timestep', cpu_time_now
      ENDIF


!     Start PREPROCESSING:
!      print *,'runepi.for: start PREPROCESSING'
!      call cpu_time(cpu_time_now)
!      write(6,*) 'CPU time before preprocessing', cpu_time_now
      
!     Perform one preprocessing timestep with module mete:
      call TSMETE(0)
      !print *,'runepi.for: after TSMETE'

!     First timestep of current hour?

      if (ITS .EQ. 1) then

!       Calculate timestep:
        call CALCDT

      endif

!     Perform one preprocessing timestep with module stat:
!MSK call to module stat must be after module lsrc
!MSK      CALL TSSTAT(0)

!     Perform one preprocessing timestep with module conc:
      call TSCONC(0)
      !print *,'runepi.for: after TSCONC0'

!     Perform one preprocessing timestep with module depo:
      call TSDEPO(0)
      !print *,'runepi.for: after TSDEPO0'

!     Perform one preprocessing timestep with module phot:
      call TSPHOT(0)
      !print *,'runepi.for: after TSPHOT0'

!     Perform one preprocessing timestep with module asrc:
      call TSASRC(0)
      !print *,'runepi.for: after TSASRC0'

!     Perform one preprocessing timestep with module psrc:
      call TSPSRC(0)
      !print *,'runepi.for: after TSPSRC0'

!     Perform one preprocessing timestep with module lsrc:
      call TSLSRC(0)
      !print *,'runepi.for: after TSLSRC0'

!     Perform one preprocessing timestep with module stat:
      call TSSTAT(0)
      !print *,'runepi.for: after TSSTAT0'
!     Perform one preprocessing timestep with module grid:
      call TSGRID(0)
      !print *,'runepi.for: after TSGRID0'
      !print *,'runepi.for: start POSTPROCESSING'

!     Start POSTPROCESSING:

!     Perform one postprocessing timestep with module asrc:
      call TSASRC(1)
      !print *,'runepi.for: after TSASRC1'
      !call cpu_time(cpu_time_now)
      !write(6,*) 'CPU time after tsasrc', cpu_time_now

!     Perform one postprocessing timestep with module psrc:
      call TSPSRC(1)
      !print *,'runepi.for: after TSPSRC1'
      !call cpu_time(cpu_time_now)
      !write(6,*) 'CPU time after tspsrc', cpu_time_now

!     Perform one postprocessing timestep with module lsrc:
      call TSLSRC(1)
      !print *,'runepi.for: after TSLSRC1'
      !call cpu_time(cpu_time_now)
      !write(6,*) 'CPU time after tslsrc', cpu_time_now

!     Perform one postprocessing timestep with module grid:
      call TSGRID(1)
      !print *,'runepi.for: after TSGRID1'
      !call cpu_time(cpu_time_now)
      !write(6,*) 'CPU time after tsgrid', cpu_time_now

!     Perform one postprocessing timestep with module phot:
      call TSPHOT(1)
      !print *,'runepi.for: after TSPHOT1'
      !call cpu_time(cpu_time_now)
      !write(6,*) 'CPU time after tsphot', cpu_time_now

      !if (ITS .eq. NTS)  print *,'runepi: HOUR ITS', HOUR, ITS

!     Perform one postprocessing timestep with module conc:
      call TSCONC(1)
      !print *,'runepi.for: after TSCONC1'
      !call cpu_time(cpu_time_now)
      !write(6,*) 'CPU time after tsasrc', cpu_time_now

!     Perform one postprocessing timestep with module depo:
      call TSDEPO(1)
      !print *,'runepi.for: after TSDEPO1'
      !call cpu_time(cpu_time_now)
      !write(6,*) 'CPU time after tsdepo', cpu_time_now

!     Perform one postprocessing timestep with module stat:
      call TSSTAT(1)
      !print *,'runepi.for: after TSSTAT1'
      !call cpu_time(cpu_time_now)
      !write(6,*) 'CPU time after tsstat', cpu_time_now

!     Perform one postprocessing timestep with module mete:
      call TSMETE(1)
      !print *,'runepi.for: after TSMETE1'
      !call cpu_time(cpu_time_now)
      !write(6,*) 'CPU time after tsmete', cpu_time_now

!     If not last timestep then goto next timestep:
      if (ITS .LT. NTS) goto 100

      call cpu_time(cpu_time_now)
      write(6,*) 'CPU time end hour timestep', cpu_time_now

!     Write finish message and close message file:
      if (ATDATE(EDAT) .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS)  then
          print *,'runepi: before EXIEPI'

          call EXIEPI
      endif

      RETURN

 2000 format ('Model calc. for ',A3,1X,I2,1X,A3,1X,I4,1X,I2,'h',     &
             1X,I2,'m',1X,I2,'s',' for ',99(5A10,/,T49))


!     End of subroutine RUNEPI
      end subroutine RUNEPI
