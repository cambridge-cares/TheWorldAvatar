! <sysdat.f90 - A component of the City-scale
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

      subroutine SYSDAT(YEAR,MNTH,DAYM,HOUR,MINU,SECO)

! The subroutine returns the current machine/OS system datetime.
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
!           2016  M. Karl: Replaced system call by DATE_AND_TIME intrinsic defined
!                          by the Fortran 95 standard. Removed preprocessor directives
!
! ----------------------------------------------------------------------------------

      integer YEAR
      integer MNTH
      integer DAYM
      integer HOUR
      integer MINU
      integer SECO

! YEAR - Current machine/OS system year
! MNTH - Current machine/OS system month
! DAYM - Current machine/OS system day
! HOUR - Current machine/OS system hour
! MINU - Current machine/OS system minute
! SECO - Current machine/OS system seconds
    
! MSK start
! definitions for date_and_time
      character(8)  :: datum
      character(10) :: time
      character(4)  :: yearstr
      character(2)  :: monistr,dayistr,hourstr,minustr,secustr
      integer       :: stat1
! MSK end

! MSK start
! MSK Replaced by DATE_AND_TIME intrinsic defined by the Fortran 95 standard.
! DATE has format  ccyymmdd
! TIME has format  hhmmss.sss

      call date_and_time(DATE=datum)
      call date_and_time(TIME=time)

      yearstr = datum(1:4)
      monistr = datum(5:6)
      dayistr = datum(7:8)
      hourstr = time(1:2)
      minustr = time(3:4)
      secustr = time(5:6)
  
      !print *,'sysdat',datum,time
  
      read(yearstr,*,iostat=stat1)  YEAR
      read(monistr,*,iostat=stat1)  MNTH
      read(dayistr,*,iostat=stat1)  DAYM
      read(hourstr,*,iostat=stat1)  HOUR
      read(minustr,*,iostat=stat1)  MINU
      read(secustr,*,iostat=stat1)  SECO
  
      !print *,'sysdat',YEAR,MNTH,DAYM,HOUR,MINU,SECO

!MSK end

!!DEC$ IF DEFINED (sun4)

! Local variables
!
!      integer IDAT(3)
!      integer ITIM(3)  
!
! IDAT - Date array (month,day,year)
! ITIM - Time array (hour,minute,second)
!
! Get SunOS system date (day,month,year)
!
!      CALL IDATE(IDAT)
!
!      YEAR = IDAT(3)
!      IF ( 0 .LE. YEAR .AND. YEAR .LT. 50) YEAR = YEAR + 2000
!      IF (50 .LE. YEAR .AND. YEAR .LE. 99) YEAR = YEAR + 1900
!      MNTH = IDAT(2)
!      DAYM = IDAT(1)
!
! Get SunOS system time (hour,minute,second)
!
!      CALL ITIME(ITIM)
!
!      HOUR = ITIM(1)
!      MINU = ITIM(2)
!      SECO = ITIM(3)
!
!!DEC$ ENDIF
!
!!DEC$ IF DEFINED (hp)
! Local variables
!
!      INTEGER IDAT(3),ITIM(3)
!
! IDAT - Date array (month,day,year)
! ITIM - Time array (hour,minute,second)
!
! Get HP-UX system time (day,month,year)
!
!      CALL IDATE(IDAT(1),IDAT(2),IDAT(3))
!
!      YEAR = IDAT(3)
!
! The IDATE routine always returns the number of years since 1900
! It should be replaced by the new intrinsic routine date_and_time
!
!      YEAR = YEAR + 1900
!      MNTH = IDAT(1)
!      DAYM = IDAT(2)
!
! Get HP-UX system time (hour,minute,second)
!
!      CALL ITIME(ITIM)
!
!      HOUR = ITIM(1)
!      MINU = ITIM(2)
!      SECO = ITIM(3)
!
!!DEC$ ENDIF
!
!!DEC$ IF DEFINED (winnt)
!
! Local variables
!
!      INTEGER IARR(8)
!       CHARACTER DSTR*8,TSTR*10,ZSTR*5
!
! IARR - Date and time array
! DSTR - Date string (not used)
! TSTR - Time string (not used)
! ZSTR - Zone string (not used)
!
! Get Windows NT system time (day, month, year, hour, minute, second)
!
!      CALL DATE_AND_TIME(DSTR,TSTR,ZSTR,IARR)
!
!        YEAR = IARR(1)
!	MNTH = IARR(2)
!	DAYM = IARR(3)
!	HOUR = IARR(5)
!	MINU = IARR(6)
!	SECO = IARR(7)
!
!!DEC$ ENDIF

       return

! End of subroutine SYSDAT

      end subroutine SYSDAT
