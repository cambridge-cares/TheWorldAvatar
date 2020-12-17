! <casrc.f90 - A component of the City-scale
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

      subroutine CASRC

! *** This subroutine distributes the area sources in the model grid.
!
! *** NOTE:   The standard unit for emissions from AirQUIS is [g/s].
!
! *** INPUT:  real qaorig(nc,nx,ny,1:na) ! "na" is indicating the type of area emission field.
! ***                                    ! Most often "na" is 2
! 
! ***         "qaorig(nc,nx,ny,1)"       ! Specifies emissions from domestic heating and small industry. 
! ***         "qaorig(nc,nx,ny,2)"       ! Specifies emissions from road traffic.
!
! ***         These 2-D fields are distributed vertically into 3_D emission
! ***         fields in the array variable: "qa(nc,nx,ny,nz)"
!
! *** For the runs of 'KPIZ_Large_photochem' and 'KPIZ_Large_photoeq' i.e., the 100 km * 100 km domain
! ***                 (with DX = DY = 5 km) we also specify the temporal distribution  in this routine.
! ***                 This is sort of "hard-coded" below.                 
!
! *** OUTPUT: real qa(nc,nx,ny,nz)  ! Contains the gridded emission values in units of [g/s]. 
! 
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
!           2017  M. Karl: Default is attributing the vertical distribution to four sectors:
!                          Domestic heating, Traffic, Other Shipping.
!                          Commented all cases except "Rotterdam"
!    10 Apr 2018  M. Karl: L454-458 Vertical distribution Sector1 (domestic heating): 90:10
!    11 Apr 2018  M. Karl: L469-473 Vertical distribution Sector3 (commercial comb.): 80:20
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
!      use mod_mete     ! Is this used?  HAS TO BE INVOKED IF MET-DEPENDENT GRID EMISSIONS!
      use mod_conc
      use mod_asrc
      use mod_psrc, only : psrcmtype

      implicit none
      
! *** Function type declaration:
!      integer ILAY    ! This util-Function should be used in order to find the layer index.

! *** Local variables:
      integer :: ia, ic
      integer :: ix, iy, iz
      integer :: ih
      integer :: iiday, iiweek
      integer :: siteid_len

      real    :: tv_qaorig
      real    :: rmax_qa
      real    :: rmin_qa
      real    :: rmax_qaorig_resident, rmax_qaorig_traffic
      real    :: rsum_qaorig_resident, rsum_qaorig_traffic      
      real    :: avg_hour_we_tv, avg_hour_wd_tv, avg_day_tv, avg_week_tv
      
      real    :: test_variable
      real, allocatable :: resident_emis(:,:)
      real, allocatable :: traffic_emis(:,:)
      integer :: dim4_loc(4)

!MSK      logical :: ATTIME
!
! *** ic     - Compound index
! *** ix     - X-direction index
! *** iy     - Y-direction index
! *** iz     - Vertical layer index
! *** ATTIME - If at given date and time then true else false

! *** The "const_scaling(na)" array is to be applied if we need a constant scaling 
! *** of some  compound or sector:
! *** NB: This scaling treatment should be moved to: "Subroutine GetAsrcTimeDependentInputData"

      real :: scale_residential = 1.0   ! Sector 1: "Residential":  1.0
      real :: scale_industry    = 1.0   ! Sector 2: "Industry":     3.0
      real :: scale_power       = 1.0   ! Sector 3: "Power":        1.0
      real :: scale_solvent     = 1.0   ! Sector 4: "Solvent":      1.0
      real :: scale_shipping    = 1.0   ! Sector 5: "Shipping":     0.25
      real :: scale_waste       = 1.0   ! Sector 6: "Waste":        1.0
      real :: scale_traffic     = 1.0   ! Sector 7: "Traffic":      3.0
       
! *** Only scaling the KPIZ_small_domain
!      integer :: ix_select_min = 9
!      integer :: ix_select_max = 12
!      integer :: iy_select_min = 9
!      integer :: iy_select_max = 12
      
! *** Scaling all of the KPIZ_Large_domain:
      integer :: ix_select_min = 1
      integer :: ix_select_max = 20
      integer :: iy_select_min = 1
      integer :: iy_select_max = 20
      
      !BRUCE variable for setting the maximum height of the residential distribution
      real :: zdistr_max
      real :: zdistr_min
      
! *** Zeroing the "mod_conc" array: qa(nc,nx,ny,nz):
      qa = 0.0

      siteid_len = LEN_TRIM(siteid)
      
! *** Write out som general CASRC information: 
      if (messfe) then
        write(messun,*)
        write(messun,'(2A)') 'CASRC: Simulation site name = ',siteid(1:siteid_len)
        write(messun,*)
        write(messun,'(A,I5)') 'CASRC: Hour         = hour = ',hour
        write(messun,'(A,I5)') 'CASRC: Day of week  = dayw = ',dayw
        write(messun,'(A,I5)') 'CASRC: Day of month = daym = ',daym
        write(messun,'(A,I5)') 'CASRC: Day of year  = dayy = ',dayy
        write(messun,'(A,I5)') 'CASRC: Month        = mnth = ',mnth
        write(messun,'(A,I5)') 'CASRC: Year         = year = ',year
        write(messun,*)
        allocate (resident_emis(nx,ny))
        allocate (traffic_emis(nx,ny))
        do ic = 1,nc
          resident_emis = qaorig(ic,:,:,1)
          traffic_emis  = qaorig(ic,:,:,2)
          rsum_qaorig_resident = SUM(resident_emis)
          rsum_qaorig_traffic  = SUM(traffic_emis) 
          rmax_qaorig_resident = MAXVAL(resident_emis)
          rmax_qaorig_traffic  = MAXVAL(traffic_emis)
          write(messun,'(A,E12.5,A)') &
       'CASRC-0: Domain sum QAORIG_resident: ',rsum_qaorig_resident,' g/s  '
          write(messun,'(A,E12.5,A)') &
       'CASRC-0: Domain sum QAORIG_traffic:  ',rsum_qaorig_traffic,' g/s  '
          write(messun,*)              
          write(messun,'(49X,3A4)') ' IC ','  I ','  J '
          write(messun,'(A,E12.5,A,3I4)') &
       'CASRC-0: Max QAORIG_resident: ',rmax_qaorig_resident,' g/s  ',ic,MAXLOC(resident_emis)
          write(messun,'(A,E12.5,A,4I4)') &
       'CASRC-0: Max QAORIG_traffic:  ',rmax_qaorig_traffic,' g/s  ',ic,MAXLOC(traffic_emis)
          write(messun,*)
        enddo
        deallocate (resident_emis)
        deallocate (traffic_emis)
      endif

! *** START defining the Vertical Distribution of the Asrc Emissions at simulation beginning:
      if (ATTIME(BDAT) .AND. ISH == 1 .AND. ITS == 1) then

! ***   As long as we do not apply the interface SUNROUTINE SendAsrcNewNumberOfAreaSources(pinz,pina)
! ***   (but apply SUNROUTINE SendAsrcNumberOfAreaSources(pina)), we must allocate the arrays below:
        if (.NOT. allocated(hourf))         allocate(hourf(24,2,na))
        if (.NOT. allocated(daywf))         allocate(daywf(7,na))
        if (.NOT. allocated(weekyearf))     allocate(weekyearf(52,na))
      
        if (.NOT. allocated(tdistr))        allocate(tdistr(na))
        if (.NOT. allocated(asrc_tv))       allocate(asrc_tv(na))

        if (.NOT. allocated(vdistr))        allocate(vdistr(nz,na))

        if (.not. allocated(scale_sector))  allocate(scale_sector(na))
        if (.not. allocated(const_scaling)) allocate(const_scaling(nc,na))

! ***   Default setting:
        const_scaling = 1.0   ! NB: Should be applied in: "subroutine GetAsrcTimeDependentInputData"
        scale_sector  = 1.0   ! NB: Should be applied in: "subroutine GetAsrcTimeDependentInputData"
        vdistr        = 0.0   ! Zeroing the 2D vertical distribution array: "vdistr(nz,na)"

! ***   Initialize the time variation to: Non-varying with time, i.e.:
        tdistr = 1.0

! *************************************************************************************************

! ***   Start: TV-arrays default setting:

!MSK comment: only default time variation is used
        hourf(:,:,:)   = 1.0/24.0
        daywf(:,:)     = 1.0/7.0 
        weekyearf(:,:) = 1.0/52.0

!MSK start
!MSK commented KPIZ simulation for more clarity
!
!        if (siteid(1:siteid_len) == 'KPIZ_Large_photochem' .OR. siteid(1:siteid_len) == 'KPIZ_Large_photoeq') then
!
!! ***     Start: TV-arrays for the "TRAFFIC" sector: **************************************************
!         
!! ***     Houly (24 values) time variation "weekdays" = All but Friday  Traffic emissions:		
!          hourf(:,1,na) = (/0.009, 0.005, 0.003, 0.004, 0.009, 0.029, 0.066, 0.055, 0.054, 0.059, &
!                       0.063, 0.060, 0.061, 0.058, 0.054, 0.051, 0.055, 0.060, 0.059, 0.058, &
!                       0.052, 0.039, 0.025, 0.012/)
!
!          test_variable    = SUM(hourf(1:23,1,na))
!!          hourf(24,1,na) = 1.0 - SUM(hourf(1:23,1,na))
!          hourf(24,1,na) = 1.0 - test_variable      
!          do ih = 1,24
!            hourf(ih,1,na) = 24.0 * hourf(ih,1,na)
!          enddo
!      
!! ***     Houly (24 values) time variation "Weekend"=Friday  Traffic emissions:	
!          hourf(:,2,na) = (/0.013, 0.006, 0.005, 0.008, 0.010, 0.021, 0.028, 0.034, 0.041, 0.048, &
!                       0.051, 0.038, 0.039, 0.045, 0.051, 0.055, 0.071, 0.078, 0.075, 0.076, &
!                       0.073, 0.062, 0.045, 0.026/)
!
!          test_variable    = SUM(hourf(1:23,2,na))
!!          hourf(24,2,na) = 1.0 - SUM(hourf(1:23,2,na))
!          hourf(24,2,na) = 1.0 - test_variable 
!          do ih = 1,24
!            hourf(ih,2,na) = 24.0 * hourf(ih,2,na)
!          enddo
!      
!! ***     Daily (7 values) time variation    Monday - Sunday  Traffic emissions:
!          daywf(:,na) = (/0.155, 0.165, 0.150, 0.140, 0.110, 0.130, 0.150/)
!      
!          test_variable = SUM(daywf(1:6,na))
!!          daywf(7,na) =  1.0 -  SUM(daywf(1:6,na))
!          daywf(7,na) =  1.0 -  test_variable
!          do iiday = 1,7
!            daywf(iiday,na) =  7.0 * daywf(iiday,na)
!          enddo
!      
!! ***     Week number (52 values) time variation Traffic emissions:
!          weekyearf(:,na) = (/0.020, 0.021, 0.021, 0.020, 0.020, 0.020, 0.020, 0.020, 0.020, 0.020,  &
!                         0.020, 0.019, 0.019, 0.019, 0.019, 0.019, 0.019, 0.019, 0.019, 0.019, &
!                         0.019, 0.019, 0.019, 0.019, 0.019, 0.019, 0.019, 0.019, 0.019, 0.018, &
!                         0.017, 0.016, 0.016, 0.016, 0.017, 0.018, 0.019, 0.019, 0.019, 0.019, &
!                         0.019, 0.019, 0.019, 0.020, 0.020, 0.020, 0.020, 0.020, 0.020, 0.020, &
!                         0.020, 0.020/)
!
!          test_variable    = SUM(weekyearf(1:51,na))
!!          weekyearf(52,na) = 1.0 - SUM(weekyearf(1:51,na))
!          weekyearf(52,na) = 1.0 - test_variable 
!          do iiweek = 1,52
!            weekyearf(iiweek,na) = 52.0 * weekyearf(iiweek,na)  
!          enddo
!
!          do ia = 1,na
!            if(ia == 1) scale_sector(ia) = scale_residential ! Sector 1: "Residential"
!            if(ia == 2) scale_sector(ia) = scale_industry    ! Sector 2: "Industry"
!            if(ia == 3) scale_sector(ia) = scale_power       ! Sector 3: "Power"
!            if(ia == 4) scale_sector(ia) = scale_solvent     ! Sector 4: "Solvent"
!            if(ia == 5) scale_sector(ia) = scale_shipping    ! Sector 5: "Shipping"
!            if(ia == 6) scale_sector(ia) = scale_waste       ! Sector 6: "Waste"
!            if(ia == 7) scale_sector(ia) = scale_traffic     ! Sector 7: "Traffic"
!          enddo
!          
!! ***     Specifying the sectors that applies TIME VARIATION in "CASRC":
!      
!! ***     asrc_tv(1)      = .FALSE.  ! Sector 1: "Residential"
!! ***     asrc_tv(2)      = .FALSE.  ! Sector 2: "Industry"
!! ***     asrc_tv(3)      = .FALSE.  ! Sector 3: "Power"
!! ***     asrc_tv(4)      = .FALSE.  ! Sector 4: "Solvent"      
!! ***     asrc_tv(5)      = .FALSE.  ! Sector 2: "Shipping"
!! ***     asrc_tv(6)      = .FALSE.  ! Sector 3: "Waste"
!! ***     asrc_tv(7=na)   = .TRUE.   ! Sector 7: "Traffic"         
!
!          asrc_tv     = .false.
!          asrc_tv(na) = .true.
!  
!! ***     Start: "KPIZ_Large_photochem" vertical distribution:
!          do ia = 1,na  ! Go through all the sectors:
!            if (ia == 1) then
!! ***         Sector 1: "Residential"
!              vdistr( 1,ia) = 0.60        !  60% in Layer  1: DZ(1)  = 17.5 m.
!              vdistr( 2,ia) = 0.40        !  40% in Layer  2: DZ(2)  = 20 m.
!            elseif (ia == 2) then
!! ***         Sector 2: "Industry"
!              vdistr( 2,ia) = 0.05        !   5% in Layer  2: DZ(2)  = 20 m.
!              vdistr( 3,ia) = 0.05        !   5% in Layer  3: DZ(3)  = 25 m.
!              vdistr( 4,ia) = 0.05        !   5% in Layer  4: DZ(4)  = 25 m.
!              vdistr( 5,ia) = 0.05        !   5% in Layer  5: DZ(5)  = 37.5 m.
!              vdistr( 6,ia) = 0.10        !  10% in Layer  6: DZ(6)  = 50 m.
!              vdistr( 7,ia) = 0.20        !  20% in Layer  7: DZ(7)  = 50 m.
!              vdistr( 8,ia) = 0.20        !  20% in Layer  8: DZ(8)  = 50 m.
!              vdistr( 9,ia) = 0.10        !  10% in Layer  9: DZ(9)  = 50 m.
!              vdistr(10,ia) = 0.10        !  10% in Layer 10: DZ(10) = 50 m.
!              vdistr(11,ia) = 0.05        !   5% in Layer 11: DZ(11) = 50 m.
!              vdistr(12,ia) = 0.05        !   5% in Layer 12: DZ(12) = 50 m.
!            elseif (ia == 3) then
!! ***         Sector 3: "Power"
!              vdistr( 6,ia) = 0.10        !  25% in Layer 10: DZ(6) = 50 m.
!              vdistr( 7,ia) = 0.15        !  50% in Layer 11: DZ(7) = 50 m.
!              vdistr( 8,ia) = 0.27        !  25% in Layer 12: DZ(8) = 50 m.
!              vdistr( 9,ia) = 0.35        !  50% in Layer 11: DZ(9) = 50 m.
!              vdistr(10,ia) = 0.13        !  13% in Layer 12: DZ(10) = 50 m.
!            elseif (ia == 4) then
!! ***         Sector 4: "Solvent"
!              vdistr( 1,ia) = 0.50        !  50% in Layer  1: DZ(1)  = 17.5 m.
!              vdistr( 2,ia) = 0.40        !  40% in Layer  2: DZ(2)  = 20 m.
!              vdistr( 3,ia) = 0.10        !  10% in Layer  3: DZ(3)  = 25 m.     
!            elseif (ia == 5) then
!! ***         Sector 5: "Shipping"
!              vdistr( 1,ia) = 0.20        !  20% in Layer  1: DZ(1)  = 17.5 m.
!              vdistr( 2,ia) = 0.50        !  50% in Layer  2: DZ(2)  = 20 m.
!              vdistr( 3,ia) = 0.30        !  30% in Layer  3: DZ(3)  = 25 m.
!            elseif (ia == 6) then
!! ***         Sector 6: "Waste"
!              vdistr( 1,ia) = 1.0          ! 100% in Layer  1: DZ(1) = 17.5 m.
!            elseif (ia == 7) then
!! ***         Sector 7: "Traffic"
!              vdistr( 1,ia) = 1.0          ! 100% in Layer  1: DZ(1) = 17.5 m.
!            endif
!          enddo  !  do ia = 1,na
!          
!MSK        elseif (siteid(1:siteid_len) == 'Rotterdam') then
!MSK end
          if (siteid(1:siteid_len) == 'Rotterdam') then

! ***     Specifying the sectors that applies TIME VARIATION in "CASRC":
      
! ***     asrc_tv(1)      = .FALSE.  ! Sector 1: "Other_than_Traffic"
! ***     asrc_tv(2)      = .FALSE.  ! Sector 2: "Traffic"
        
          asrc_tv       = .false.        
                
! ***     Start: "Rotterdam" vertical distribution: (NOTE: At the moment equal to the "Default" setting below.)
          do ia = 1,na  ! Go through all the sectors:
            if (ia == 1) then
! ***         Sector 1: "Residential":
              vdistr(2, ia) = 1.0         !  100% in Layer  2: DZ(2)  = 20 m.
!_SEW_UncertWeb_120913_Start
              vdistr(2,ia) = DZ(2)/(DZ(2) + DZ(3) + DZ(4))
              vdistr(3,ia) = DZ(3)/(DZ(2) + DZ(3) + DZ(4))
              vdistr(4,ia) = DZ(4)/(DZ(2) + DZ(3) + DZ(4))
!SEW_UncertWeb_120913_End
            elseif (ia == 2) then
! ***         Sector 2: "Traffic":
              vdistr(1,ia) = 1.0          ! 100% in Layer  1: DZ(1) = 17.5 m.
            endif
          enddo ! do ia = 1,na      

!MSK start
!MSK commented Bedre Byluft simulation for more clarity
!        elseif (siteid(1:7) == 'BB_UM20' .OR. siteid(1:7) == 'BB_HM20') then
!! ***     BEDRE BYLUFT Application:        
!! ***     Specifying the sectors that applies TIME VARIATION in "CASRC":
!      
!! ***     asrc_tv(1)      = .FALSE.  ! Sector 1: "Other_than_Traffic"
!! ***     asrc_tv(2)      = .FALSE.  ! Sector 2: "Traffic"
!        
!          asrc_tv       = .false.          
!        
!! ***     Start: "Default" VERTICAL DISTRIBUTION:
!          do ia = 1,na  ! Go through all the sectors:
!            if (ia == 1) then
!! ***         Sector 1: "Residential":
!              if ((icmpnd(1) == 5 .AND. cmpnd(1)(1:3) == 'NO2') .OR. &
!             (icmpnd(1) == 7 .AND. cmpnd(1)(1:3) == 'NOx')) then
!! ***             NO2/NO/O3 simulation: "Residential" NO2 and NO emissions equally distributed
!! ***                                   in the two lowermost layers: 0 - 22 m.
!!                  vdistr(1,ia) = DZ(1)/(DZ(1)+DZ(2))  ! Layer 1:  0 - 5m.
!!                  vdistr(2,ia) = DZ(2)/(DZ(1)+DZ(2))  ! Layer 2:  5 - 22m.
!! ***                                   in layer 2 and 3: 5 - 45m. 
!!                   vdistr(2,ia) = 1  ! Layer 2:  5 - 22m.              
!                  vdistr(2,ia) = DZ(2)/(DZ(2)+DZ(3))  ! Layer 2:  5 - 22m.
!                  vdistr(3,ia) = DZ(3)/(DZ(2)+DZ(3))  ! Layer 3: 22 - 45m.
!              elseif((icmpnd(1) == 9  .AND. cmpnd(1)(1:4) == 'PM10') .OR.  &
!                (icmpnd(1) == 10 .AND. cmpnd(1)(1:5) == 'PM2.5')) then
!! ***                PM2.5 or PM10 simulation: "Residential" PM2.5 and PM10 emissions equally distributed
!! ***                                          in layer 2 and 3: 5 - 45 m.
!                  vdistr(2,ia) = 1  ! Layer 2:  5 - 22m.              
!!                vdistr(2,ia) = DZ(2)/(DZ(2)+DZ(3))  ! Layer 2:  5 - 22m.              
!!                vdistr(3,ia) = DZ(3)/(DZ(2)+DZ(3))  ! Layer 3: 22 - 45m.
!! ***                                          in layer 3 and 4: 
!                ! vdistr(3,ia) = DZ(3)/(DZ(3)+DZ(4))  ! Layer 3: 22 - 45m.
!                ! vdistr(4,ia) = DZ(4)/(DZ(3)+DZ(4))  ! Layer 4: 45 - 75m.
!              else
!                if (messfe) then
!                  write(messun,*)
!                  write(messun,'(A)') 'CASRC: This is a BEDRE BYLUFT APPLCATION. '
!                 write(messun,'(A)') 'CASRC: Can only be run for NO2 (Test NOx), PM10 and PM2.5 '
!                 write(messun,'(A)') 'CASRC: PROGRAM TERMINATES! '
!                endif
!                STOP
!              endif
!            elseif (ia == 2) then
!! ***         Sector 2: "Traffic":
!              vdistr(1,ia) = 1.0          ! 100% in Layer 1
!            endif
!          enddo ! do ia = 1,na
!MSK end
                     
          else    ! Default Distribution

! ***     Specifying the sectors that applies TIME VARIATION in "CASRC":
      
! ***     asrc_tv(1)      = .FALSE.  ! Sector 1: "Other_than_Traffic"
! ***     asrc_tv(2)      = .FALSE.  ! Sector 2: "Traffic"
        
          asrc_tv       = .false.          
        
! ***     Start: "Default" VERTICAL DISTRIBUTION: BRUCE changed this not quite finished zdistr_min only works when equivalent to a layer height
          do ia = 1,na  ! Go through all the sectors:
            if (ia == 1) then
! ***         Sector 1: "Upper": BRUCE   (domestic heating)
                zdistr_max=max(35.0,Z(1))

                zdistr_min=Z(1)

!MSK start: this was used for pnc. Layer 1 has 0%
!MSK                do iz=1,nz-1
!MSK                  if (Z(iz).le.zdistr_max.and.Z(iz).gt.zdistr_min) then
!MSK                    vdistr(iz,ia) = DZ(iz)/(zdistr_max-zdistr_min)
!MSK                  elseif (Z(iz).gt.zdistr_max.and.Z(iz).gt.zdistr_min) then
!MSK                    vdistr(iz,ia) = max(dble(0.0),(zdistr_max-Z(iz)+DZ(iz)))/(zdistr_max-zdistr_min)
!MSK                  else
!MSK                    vdistr(iz,ia) = 0
!MSK                  endif
!MSK                enddo
!MSK end
!MSK start
!MSK for now put all in first layer to get max impact
!!!              vdistr(1,ia) = 1.0          ! 100% in Layer 1
!MSK              vdistr(1,ia) = 0.7          ! 70% in Layer 1
!MSK              vdistr(2,ia) = 0.3          ! 30% in Layer 2
              vdistr(1,ia) = 0.9          ! 90% in Layer 1
              vdistr(2,ia) = 0.1          ! 10% in Layer 2
!MSK end
            elseif (ia == 2) then
! ***         Sector 2: "Traffic":
!MSK: sector not used, because all traffic is line source
              vdistr(1,ia) = 1.0          ! 100% in Layer 1

            elseif (ia == 3) then
! ***         Sector 3: "Other"   "lower":BRUCE
!!!             vdistr(1,ia) = 1.0          ! 100% in Layer 1
!MSK start
!MSK              vdistr(1,ia) = 0.7          ! 70% in Layer 1
!MSK              vdistr(2,ia) = 0.3          ! 30% in Layer 2
              vdistr(1,ia) = 0.8          ! 80% in Layer 1
              vdistr(2,ia) = 0.2          ! 20% in Layer 2
            elseif (ia == 4) then
! ***         Sector 4: "Shipping"
! ***              Default "50/50": (100% below 37.5m)
!              vdistr(1,ia) = 0.5          ! 50% in Layer 1
!              vdistr(2,ia) = 0.5          ! 50% in Layer 2
! ***              TEST CMAQ-distribution
              vdistr(1,ia) = 0.25         ! 25% in Layer 1
              vdistr(2,ia) = 0.25         ! 25% in Layer 2
              vdistr(3,ia) = 0.25         ! 25% in Layer 1
              vdistr(4,ia) = 0.25         ! 25% in Layer 2
            endif
!MSK end

          enddo ! do ia = 1,na
             
        endif

! ***   Write out the applied vertical distribution at the start of the simulation:
        if (messfe) then  
          write(messun,*)
          write(messun,'(A)') 'CASRC: The following vertical distribution of area sources is now applied: '
          write(messun,'(A)') 'CASRC: (NB: All other "vdistr(iz,ia)" values are equal to zero. '
          write(messun,*)
          do iz = 1,nz
            do ia = 1,na
              if (vdistr(iz,ia) /= 0.) then
                write(messun,'(A,F8.4,2(A,I3))')  &
                'CASRC: "vdistr(iz,ia)" = ',vdistr(iz,ia),' for "iz" = ',iz,' and "ia" = ',ia
              endif
            enddo
          enddo
          
          write(messun,*)
          do ia = 1,na
            write(messun,'(A,F8.4,A,I3)')   'CASRC: "scale_sector(ia)"    = ',scale_sector(ia),' for "ia" = ',ia
          enddo
          write(messun,*)

        endif  ! if (messfe)
        
      endif  ! if (ATTIME(BDAT) .AND. ISH == 1 .AND. ITS == 1 .AND. messfe) then
      
! *** End defining the Vertical Distribution of the Asrc Emissions. Done at start of simulation.
! ****************************************************************************************************************

! *** START TIME VARIATION Initialisation and update of the week-number:

!MSK start
!MSK commented KPIZ simulation for more clarity
!! *** Start: "KPIZ_Large_photochem" application SPECIFIC CODE for September 2010:
!      if (siteid(1:siteid_len) == 'KPIZ_Large_photochem' .OR. siteid(1:siteid_len) == 'KPIZ_Large_photoeq') then
!
!! ***   HARD-CODING:      
!        if (dayy == 244) then
!          iweeknr = 35      ! The start value.
!          if (messfe) then
!            write(messun,'(A,I5)') 'CASRC: Week number start at: ',iweeknr 
!          endif
!        endif  ! if (dayy == 244)
!        if (hour == 0 .AND. dayw == 1) then
!          iweeknr = iweeknr + 1
!          if (messfe) then
!            write(messun,'(A,I5)') 'CASRC: Week number increased to: ',iweeknr 
!          endif
!        endif  ! if (hour == 0 .AND. dayw == 1)
!
!        if (dayw == 4) then    ! Friday (= weekend in Abu Dhabi).
!          if (messfe) then
!            write(messun,'(A,I5)') 'CASRC: dayw is now equal to ',dayw
!          endif
!        endif  ! if (dayw == 4)
!
!      endif  ! if (siteid(1:siteid_len) == 'KPIZ_Large_photochem' .OR. siteid(1:siteid_len) == 'KPIZ_Large_photoeq') 

! *** END TIME VARIATION Initialisation and update of the week-number.


! *** Presently the "KPIZ_Large_photochem" or "KPIZ_Large_photoeq" applications for September 2010 
! *** only apply time variations for the "Traffic" sector, i.e. ia = 7.
!MSK comment: in the Default Application the default (=uniform equal) time factors are applied

!MSK start 26.09.2017 QA zero emission value before emission adding:
      qa(:,:,:,:) = 0.0

      do ia = 1,na
      
!MSK start 26.09.2017      
!MSK        if (messfe .and. asrc_tv(ia) ) then
!MSK          write(messun,'(A,I4)') 'CASRC: asrc_tv(ia) = .true. for ia = ',ia 
!MSK        endif
!MSK
!MSK ! ***   Only apply variable time variation for the selected sectors:
!MSK        if (asrc_tv(ia)) then
!MSK
!MSK ! ***     Find if "weekend", i.e. in this case just Friday:
!MSK          if (dayw == 5) then
!MSK            tdistr(ia) = hourf(hour+1,2,ia) * daywf(dayw,ia) * weekyearf(iweeknr,ia)
!MSK          else
!MSK            tdistr(ia) = hourf(hour+1,1,ia) * daywf(dayw,ia) * weekyearf(iweeknr,ia)
!MSK          endif
!MSK        else
        
! ***     Now no timevariation should be applied, and "tdistr(ia)" should now be identical equal to 1.0        
          tdistr(ia) = 1.0
!MSK              
!MSK        endif  ! if (asrc_tv(ia) == .false.)
!MSK end 26.09.2017

        if (messfe) then
          write(messun,'(A,F8.4,A,I3)')   'CASRC: "tdistr(ia)"    = ',tdistr(ia),' for "ia" = ',ia
        endif
        
! ***   Calculate the GRID DISTRIBUTED EMISSIONS:

! ***   Go through all the species:
        do ic = 1,nc
! ***     Apply the time-variation on the "2D" qaorig-array for Sector "ia":          
          do ix = 1,nx
            do iy = 1,ny

!MSK start 26.09.2017
!MSK               if (ix >= ix_select_min .AND. ix <= ix_select_max .AND. &
!MSK              iy >= iy_select_min .AND. iy <= iy_select_max) then
!MSK                  const_scaling(ic,ia) = scale_sector(ia)  ! Scale each sector separately
!MSK               else
!MSK                  const_scaling(ic,ia) = 1.0
!MSK               endif
!MSK end 26.09.2017

!_WITH NO TIMEVARIATION:              tv_qaorig = qaorig(ic,ix,iy,ia)
!MSK: The standard is that emissions already have their time variation
!MSK: so tdistr(ia) = 1.0 and const_scaling(ic,ia) = 1.0
!MSK start
                const_scaling(ic,ia) = 1.0
!MSK end  
                tv_qaorig = qaorig(ic,ix,iy,ia) * tdistr(ia) * const_scaling(ic,ia)

! ***         Then apply the vertical distribution for this Sector:
              do iz = 1,nz    ! Here we could limit ourself to: iz = 1,12       
                 qa(ic,ix,iy,iz) = qa(ic,ix,iy,iz) + ( vdistr(iz,ia) * tv_qaorig)
              enddo  ! do iz = 1,nz
            enddo  ! do iy = 1,ny 
          enddo  !  do ix = 1,nx
        enddo  ! do ic = 1,nc
        
      enddo  ! do ia = 1,na

! Check ship emissions of NO
!     do ix = 15,15
!        do iy = 1,ny
!          do iz = 1,6
!            print *,'casrc ship-NO qa',ix,iy, qa(2,ix,iy,1) 
!                    , vdistr(iz,1),  &
!                    qaorig(3,ix,iy,1),qaorig(3,ix,iy,2),qaorig(3,ix,iy,3),qaorig(3,ix,iy,4)
!          enddo
!        enddo
!     enddo

      if (messfe) then
        write(messun,*)       
        write(messun,'(A)') 'CASRC: Have now included all ASRC emissions in units of "g/s":'
          rmax_qa     = MAXVAL(qa(2,:,:,:))
        write(messun,'(42X,3A4)') '  I ','  J ','  K '
        write(messun,'(A,E12.5,A,3I4)') &
          'CASRC-1: Max QA(IC=2, NO): ',rmax_qa,' g/s. ',MAXLOC(qa(2,:,:,:))
        write(messun,*)
        if(nc == 3) then
            rmax_qa     = MAXVAL(qa(2,:,:,:))
          write(messun,'(42X,3A4)') '  I ','  J ','  K '
          write(messun,'(A,E12.5,A,3I4)') &
            'CASRC-1: Max QA(IC=2): ',rmax_qa,' g/s. ',MAXLOC(qa(2,:,:,:))
          write(messun,*)        
           rmax_qa     = MAXVAL(qa(3,:,:,:))
          write(messun,'(42X,3A4)') '  I ','  J ','  K '
          write(messun,'(A,E12.5,A,3I4)') &
            'CASRC-1: Max QA(IC=3): ',rmax_qa,' g/s. ',MAXLOC(qa(3,:,:,:))
          write(messun,*)  
        endif
      endif

! *** Add Point Source emissions to Grid:
      if (psrcmtype < 1) then
        CALL PSRCtoASRC
        
        if (messfe) then
          write(messun,*)       
          write(messun,'(A)') 'CASRC: Have now added the PSRC Emissions to the Area Sources:'
          rmax_QA     = MAXVAL(qa(1,:,:,:))
          write(messun,'(42X,3A4)') '  I ','  J ','  K '
          write(messun,'(A,E12.5,A,3I4)') &
          'CASRC-2: Max QA(IC=1): ',rmax_qa,' g/s. ',MAXLOC(qa(1,:,:,:))
          write(messun,*)
          if(nc == 3)then
            rmax_QA     = MAXVAL(qa(2,:,:,:))
            write(messun,'(42X,3A4)') '  I ','  J ','  K '
            write(messun,'(A,E12.5,A,3I4)') &
            'CASRC-2: Max QA(IC=2): ',rmax_qa,' g/s. ',MAXLOC(qa(2,:,:,:))
            write(messun,*)
            rmax_QA     = MAXVAL(qa(3,:,:,:))
            write(messun,'(42X,3A4)') '  I ','  J ','  K '
            write(messun,'(A,E12.5,A,3I4)') &
            'CASRC-2: Max QA(IC=3): ',rmax_qa,' g/s. ',MAXLOC(qa(3,:,:,:))
            write(messun,*)            
          endif
        endif  ! if (messfe)
         
      endif  ! if (psrcmtype < 1)

      if ((siteid(1:siteid_len) == 'KPIZ_Large_photochem' .OR. siteid(1:siteid_len) == 'KPIZ_Large_photoeq')  &
     .AND. messfe) then
        avg_hour_wd_tv = (SUM(hourf(:,1,na)))/24.0
        avg_hour_we_tv = (SUM(hourf(:,2,na)))/24.0
        avg_day_tv     = (SUM(daywf(:,na)))/7.0
        avg_week_tv    = (SUM(weekyearf(:,na)))/52.0
        write(messun,*)
        write(messun,'(A,F8.4,A,I3)') 'CASRC: avg_hour_wd_tv  = ',avg_hour_wd_tv,' for Sector: ',na
        write(messun,'(A,F8.4,A,I3)') 'CASRC: avg_hour_we_tv  = ',avg_hour_we_tv,' for Sector: ',na
        write(messun,'(A,F8.4,A,I3)') 'CASRC: avg_day_tv      = ',avg_day_tv,' for Sector: ',na
        write(messun,'(A,F8.4,A,I3)') 'CASRC: avg_week_tv     = ',avg_week_tv,' for Sector: ',na
      endif

      RETURN

!     End of subroutine CASRC

      end subroutine casrc
