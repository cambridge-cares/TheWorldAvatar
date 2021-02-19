! <module_uect_time.for - A component of the City-scale
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
! ----------------------------------------------------------------------------------
! Original source code of the routines cdayweek, cdayyear, LEAP, 
! are written by Sam-Erik Walker (NILU)
!
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
! ----------------------------------------------------------------------------------


      module module_uect_time

!***********************************************************************
!***  Module module_uect_exe declares variables and parameters
!***  for calendar date and time in UECT
!***********************************************************************

      implicit none

!***********************************************************************

!     Declarations:

      integer             :: bdat(6)
      integer             :: edat(6)

      integer             :: nday(12)

      character(len=3)    :: txtday(7)
      character(len=3)    :: txtmon(12)

      integer             :: year
      integer             :: mnth
      integer             :: daym
      integer             :: hour
      integer             :: minu
      integer             :: seco
      integer             :: dayweek
      integer             :: dayyear



!     Routines and Functions:

! ************
      contains

!**********************************************************************

      subroutine cdayweek(year,mnth,daym,dayweek)

! The subroutine calculates the day of week given current datetime,
! where DAYW = 1 corresponds to Monday and DAYW = 7 to Sunday. The
! algorithm is based on the tables in "Hvem Hva Hvor 1971" (p. 121)
! and is valid for all years from 1800 to infinity!

!***********************************************************************

      implicit none

      integer, intent(in)   :: year
      integer, intent(in)   :: mnth
      integer, intent(in)   :: daym
      integer, intent(out)  :: dayweek

! Local variables

      integer               :: JM(12)
      integer               :: IR
      integer               :: JC
      integer               :: NT
      integer               :: JK
      integer               :: J4
      integer               :: J100
      integer               :: J400


! JM   - Column number for each month
! IR   - Row    in HHH table for day of month
! JC   - Column in HHH table for month
! NT   - Number in HHH table for row IR and column JC
! JK   - Column in HHH table for year
! J4   - Adjustment value for leap year
! J100 - Adjustment value for leap year
! J400 - Adjustment value for leap year
! LEAP - If leap year then true else false

! Set data in table JM

      DATA JM/1,5,5,2,7,4,2,6,3,1,5,3/

! Calculate row    number for day of month

      IR = MOD(daym - 1,7) + 1

! Calculate column number for month

      JC = JM(mnth)
      IF (LEAP(year) .AND. (mnth .EQ. 1 .OR. mnth .EQ. 2)) JC = JC + 1

! Calculate "number" in HHH table with row IR and column JC

      NT = MOD(IR + 7 - JC,7) + 1

! Calculate column number for year (adjusting for leap years)

      J4   = (year - 1800)/4
      J100 = (year - 1800)/100
      J400 = (year - 1600)/400

      JK = MOD(year - 1800 + J4 - J100 + J400 + 3 - 1,7) + 1

! Calculate day of week

      dayweek = MOD(JK - 1 + NT - 1,7) + 1

      return

! End of subroutine cdayweek

      end subroutine cdayweek

!**********************************************************************

      subroutine cdayyear(year,mnth,daym,dayyear)

! The subroutine calculates the day of year given current datetime.

!***********************************************************************

      implicit none

      integer, intent(in)   :: year
      integer, intent(in)   :: mnth
      integer, intent(in)   :: daym
      integer, intent(out)  :: dayyear

! Local variables

      integer I

! I - Month index

! Calculate day of year

      dayyear = 0

      do 100 I = 1,mnth - 1
          dayyear = dayyear + nday(I)
  100 continue

      dayyear = dayyear + daym

       return

! End of subroutine cdayyear

      end subroutine cdayyear

!**********************************************************************

      subroutine scale_isop(day,ntim,ntim_max,scalef,lat_start,dlat,idayy)
      !subroutine scale_isop(day,ntim,ntim_max,scalef,nlat,lat_start,dlat,idayy)
    !
    ! routine calculates factors to be multiplied with an 
    ! emission field (e.g. isop) in order to simulate a diurnal cycle 
    ! in emissions (caused by solar dependency)
    ! e.g.   rm(i,j,1,isop) = rm(i,j,1,isop) + e_isop(i,j)*scalef(ipos,j)*dt
    ! with ipos depending on time and longitude.
    ! the scalefactor is calculated for -180 longitude and the mean value
    ! for ntim timesteps during a day is scaled to 1.
    ! The routine should be called every day, 
    ! since the position relative to the sun changes.
    ! The scaling is based on the cos(zenith) which is 1 for 
    ! overhead sun and 0 at sunset.
    ! For the polar night, the values are set to 1.0 (even distribution)
    ! One thing is sure afterwards: we know that e_isop kg/s is added.

!***********************************************************************

    implicit none

      ! in/out
      integer, intent(in) :: day       !day number based on 365 days a year
      integer, intent(in) :: ntim      !number of timesteps during one day
      integer, intent(in) :: ntim_max  !maximum, used to declare array 
     !integer, intent(in) :: nlat      !number of latitudes
      integer, intent(in) :: lat_start !southern edge of the domain (integer!)
      real,    intent(in) :: dlat      !increment   (real!)
      integer, intent(in)  :: idayy     !# days this year
      real,dimension(ntim_max),intent(out) :: scalef  

      ! local
      real              :: pi,piby,obliq,deday,delta,dt,lon,hr,lat,houra,smm
      integer           :: i

 
         pi = acos(-1.0)
         piby = pi/180.0
         obliq = 23.45 * piby
         deday = 4.88 + 2*pi/idayy  * day
         delta = asin(sin(obliq)*sin(deday))
         dt = 24./ntim   !timestep in hours
         lon = -180.0*piby       !calculate for dateline
         !do j=1,nlat   !over latitudes

            hr =  - 0.5*dt   !shift half a interval back
           !lat = (lat_start + (j-0.5)*dlat)*piby
           lat = (lat_start + (1.0-0.5)*dlat)*piby
            smm = 0.0
            do i=1,ntim
               hr = hr + dt
               houra = lon - pi + hr * (2.*pi/24.)
               scalef(i) = &
                   max((sin(delta)*sin(lat)  + cos(delta)*cos(lat)*cos(houra)),0.0)
               smm = smm+scalef(i)/ntim
            end do
            if ( smm > 1e-5 ) then 
              scalef(1:ntim) = scalef(1:ntim)/smm 
            else 
              scalef(1:ntim) = 1.0
            end if

         !end do

       return

! End of logical function LEAP
      end subroutine scale_isop

!**********************************************************************

      logical function LEAP(yearin)
! The function returns true if leap year, else false.
! Scalar arguments

!***********************************************************************

      integer, intent(in) :: yearin

! YEAR - Year
      LEAP = .FALSE.
      IF (MOD(yearin,  4) .EQ. 0 .AND. .NOT. (MOD(yearin,100) .EQ. 0 .AND.     &
         MOD(yearin,400) .NE. 0)) LEAP = .TRUE. 
      
          return

! End of logical function LEAP
      end function 

!**********************************************************************


! End of module module_uect_time

      end module module_uect_time
