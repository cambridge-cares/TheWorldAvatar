! <get_calendar_day.for - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*
!*  Unless explicitly acquired and licensed from Licensor under another license,
!*  the contents of this file are subject to the Reciprocal Public License ("RPL")
!*  Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!*  allowed by the RPL, and You may not copy or use this file in either source code
!*  or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!*  All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!*  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!*  DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!*  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!*  See the RPL for specific language governing rights and limitations under the RPL.
!*
!*****************************************************************************!
!
!***********************************************************************
!***
!***      UECT
!***      Urban Emission Conversion Tool
!***
!***********************************************************************

      subroutine get_calendar_day

!***********************************************************************
!***  Subroutine get_calendar day calculates the calendar day
!***  based on a date in YYYY,MM,DD format
!***********************************************************************

!***  NOTE: THE META INFORMATION MAY BE EXTENDED FOR METEO AND BCON

!     Declarations of variables by using the MODULES feature:

      use module_uect_io
      use module_uect_exe
      use module_uect_time


      implicit none

!***********************************************************************

!     Local declarations:


      
!***********************************************************************
!     Content of subroutine:

! Define name of weekdays

      TXTDAY(1) = 'Mon'
      TXTDAY(2) = 'Tue'
      TXTDAY(3) = 'Wed'
      TXTDAY(4) = 'Thu'
      TXTDAY(5) = 'Fri'
      TXTDAY(6) = 'Sat'
      TXTDAY(7) = 'Sun'

! Define name of months

      TXTMON( 1) = 'Jan'
      TXTMON( 2) = 'Feb'
      TXTMON( 3) = 'Mar'
      TXTMON( 4) = 'Apr'
      TXTMON( 5) = 'May'
      TXTMON( 6) = 'Jun'
      TXTMON( 7) = 'Jul'
      TXTMON( 8) = 'Aug'
      TXTMON( 9) = 'Sep'
      TXTMON(10) = 'Oct'
      TXTMON(11) = 'Nov'
      TXTMON(12) = 'Dec'

! Define number of days in each month

      NDAY( 1) = 31
      NDAY( 2) = 28
      NDAY( 3) = 31
      NDAY( 4) = 30
      NDAY( 5) = 31
      NDAY( 6) = 30
      NDAY( 7) = 31
      NDAY( 8) = 31
      NDAY( 9) = 30
      NDAY(10) = 31
      NDAY(11) = 30
      NDAY(12) = 31

! Check for leap year

      if (leap(year)) nday(2) = 29

! Calculate day of week

      call cdayweek(year,mnth,daym,dayweek)

! Calculate day of year

      call cdayyear(year,mnth,daym,dayyear)



      return
      end subroutine get_calendar_day
