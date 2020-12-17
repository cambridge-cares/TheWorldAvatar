! <module_uect_emis.for - A component of the City-scale
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

      module module_uect_emis

!***********************************************************************
!***  Module module_uect_emis declares variables and parameters
!***  for handling emission conversion
!***********************************************************************

      implicit none

!***********************************************************************

!     Declarations:

! SNAP Categories
!     SNAP1:  Combustion in energy and transformation energy
!              (power plants, refineries)
!     SNAP2:  Non-industrial combustion plants
!              (commercial & residential plants)
!     SNAP3:  Combustion in manufacturting industry
!              (industrial combustion)
!     SNAP4:  Production processes
!              (industrial processes)
!     SNAP5:  Extraction and distribution of fossil fuels
!              (oil extraction)
!     SNAP6:  Solvent and other product use
!              (industrial & residential solvent use)
!     SNAP7:  Road transport
!              (vehicular emissions and road abrasion)
!     SNAP8:  Other mobile sources and machinery
!              (shipping, airplanes & off-road traffic)
!     SNAP9:  Water collection, treatment and disposal activities
!              (municipal waste incineration)
!     SNAP10: Agriculture and farming
!              (agriculture)
!     SNAP11: Natural emissions, for now Biogenic Volatile
!             Organic compunds from vegetation


!     All sources
      ! SNAP 8 "Shipping" usually not treated with UECT
      integer,parameter  :: n_snap       = 11

!     Missing value
      real,parameter     :: missval      = -99.0

!     Parameters
! kg/yr --> g/s
      real, parameter    :: pse_funit    = 1.e3/(365*24*3600)
! g/s --> g/s
      real, parameter    :: lse_funit    = 1.0
! kg/yr --> g/s
      real, parameter    :: ase_funit    = 1.e3/(365*24*3600)
! VOC --> RSMOG
      real, parameter    :: voc2rsmog    = 0.0067
! Isoprene C5H8 kgC --> kg
      real, parameter    :: fctoisop     = 68.12/(5.*12.0)

! Ratio NO/NOx for line sources
!MSK 12.08.2019      real, parameter    :: f_no_lse     = 0.70
      real, parameter    :: f_no_lse     = 0.68
      real, parameter    :: f_hono_lse   = 0.02
      real, parameter    :: f_no_pse     = 0.95

! Heating degree days constants
      real, parameter    :: hddmean      = 6.0
      real, parameter    :: fcb          = 0.18
      
!     Fields
      real, dimension(n_snap,24)     :: hourfw
      real, dimension(n_snap,24)     :: hourfe
      real, dimension(24)            :: hourfws
      real, dimension(n_snap,7)      :: daywf
      real, dimension(n_snap,12)     :: mnthf

! We could also hardwire re-occuring festivity/holiday days such as 01.01. and 01.05. to make them dayweek=7

!     Tables

!     Stack Parameters Default Values
!     According to Pregger & Friedrich, Environ. Pollution 2009, Table 4
!     SNAP2 and SNAP6 treated as combustion source within first model layer
!     SNAP7, SNAP8, SNAP10 practically treated as area source

      real,dimension(n_snap),parameter :: stack_HS_default = (/ 44.4, 10.0, 28.1, 24.9, 29.0, 10.0,1.0, 1.0,56.0,1.0,1.0 /)
      real,dimension(n_snap),parameter :: stack_RA_default = (/ 0.67, 0.50, 0.70, 0.39, 0.74, 0.50,0.20,0.20,0.59,0.20,0.20 /)
      real,dimension(n_snap),parameter :: stack_ET_default = (/166.6, 50.0,161.4,124.9,200.0, 50.0,5.0,5.0,140.0,5.0,5.0 /)
      real,dimension(n_snap),parameter :: stack_EV_default = (/ 2.44, 1.00, 4.50, 3.98, 2.55, 1.00,0.10,0.10,5.60,0.1,0.1 /)


!     VOC Table

!*  1 RSMOG   2 HCHO   3 C2H6   4 CH3CHO   5 nC4H10  6 CH3COC2H5   7 C2H4   8 C3H6   9 oXylene
!*
!* Percentage VOC-Split from EMEP
!*   C2H6   NC4H10   C2H4    C3H6    C5H8   OXYL    HCHO    CH3CHO  MEK(=CH3COC2H5)
!1  12.559  14.836   2.406   4.376   0.000   9.479  55.691   0.034  0.620
!2  12.589  39.790   8.174  10.767   0.000  18.632   5.586   0.207  0.089
!3  4.996   35.610   9.044   2.089   0.000  18.323  24.134   0.059  1.347
!4  2.652   34.519   5.458   4.257   0.142  13.380   0.077   0.978  1.608
!5  17.842  79.895   0.018   1.569   0.008   0.505   0.078   0.000  0.000
!6  0.444   44.052   0.244   0.678   0.008  17.904   0.011   0.000  9.965
!7  4.832   36.698   6.796  10.896   0.000  35.051   2.700   2.606  0.421
!8  3.775   47.416   6.636  10.608   0.000  24.676   3.115   3.261  0.235
!9  25.718  36.778   5.237   1.830   1.153   7.881  16.060   0.000  0.093
!*

      real, dimension(n_snap), parameter  :: voc2hcho = (/ 0.557,0.056,0.241,0.001,0.001,0.000,0.027,0.031,0.161,0.000,0.000 /)  !HCHO
      real, dimension(n_snap), parameter  :: voc2etha = (/ 0.126,0.126,0.050,0.027,0.178,0.004,0.048,0.038,0.257,0.000,0.000 /)  !C2H6
      real, dimension(n_snap), parameter  :: voc2aldh = (/ 0.000,0.002,0.001,0.010,0.000,0.000,0.026,0.033,0.000,0.000,0.000 /)  !CH3CHO
      real, dimension(n_snap), parameter  :: voc2buta = (/ 0.148,0.398,0.356,0.345,0.799,0.441,0.367,0.474,0.368,0.000,0.000 /)  !NC4H10
      real, dimension(n_snap), parameter  :: voc2meke = (/ 0.006,0.001,0.013,0.016,0.000,0.100,0.004,0.002,0.001,0.000,0.000 /)  !CH3COC2H5
      real, dimension(n_snap), parameter  :: voc2ethe = (/ 0.024,0.082,0.090,0.055,0.000,0.002,0.068,0.066,0.052,0.000,0.000 /)  !C2H4
      real, dimension(n_snap), parameter  :: voc2prop = (/ 0.044,0.108,0.021,0.044,0.157,0.007,0.109,0.106,0.018,0.000,0.000 /)  !C3H6
      real, dimension(n_snap), parameter  :: voc2oxyl = (/ 0.095,0.186,0.183,0.134,0.005,0.179,0.351,0.247,0.079,0.000,0.000 /)  !OXYL
      real, dimension(n_snap), parameter  :: voc2isop = (/ 0.000,0.000,0.000,0.0014,0.00,0.000,0.000,0.000,0.012,0.000,0.000 /)  !C5H8



!     MONFAC Table (SMOKE-EU; originally from LOTUS-EURO)

      real, dimension(12), parameter :: mnf01 = (/ 1.200,1.150,1.050,1.000,0.900,0.850,0.800,0.870,0.950,1.000,1.080,1.150 /)  !snap 1
      real, dimension(12), parameter :: mnf02 = (/ 1.700,1.500,1.300,1.000,0.700,0.400,0.200,0.400,0.700,1.050,1.400,1.650 /)  !snap 2
      real, dimension(12), parameter :: mnf03 = (/ 1.100,1.080,1.050,1.000,0.950,0.900,0.930,0.950,0.970,1.000,1.020,1.050 /)  !snap 3
      real, dimension(12), parameter :: mnf04 = (/ 1.020,1.020,1.020,1.020,1.020,1.020,1.000,0.880,1.020,1.020,1.020,0.940 /)  !snap 4
      real, dimension(12), parameter :: mnf05 = (/ 1.200,1.200,1.200,0.800,0.800,0.800,0.800,0.800,0.800,1.200,1.200,1.200 /)  !snap 5
      real, dimension(12), parameter :: mnf06 = (/ 0.950,0.960,1.020,1.000,1.010,1.030,1.030,1.010,1.040,1.030,1.010,0.910 /)  !snap 6
      real, dimension(12), parameter :: mnf07 = (/ 0.880,0.920,0.980,1.030,1.050,1.060,1.010,1.020,1.060,1.050,1.010,0.930 /)  !snap 7
      real, dimension(12), parameter :: mnf08 = (/ 0.880,0.920,0.980,1.030,1.050,1.060,1.010,1.020,1.060,1.050,1.010,0.930 /)  !snap 8
      real, dimension(12), parameter :: mnf09 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000 /)  !snap 9
      real, dimension(12), parameter :: mnf10 = (/ 0.450,1.300,2.350,1.700,0.850,0.850,0.850,1.000,1.100,0.650,0.450,0.450 /)  !snap 10
      real, dimension(12), parameter :: mnf11 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000 /)  !snap 11

!     DAYFAC Table (SMOKE-EU; originally from LOTUS-EURO)

!  The two different vehicles classes in SNAP 7 [see Batterman et al., Atmos. Environ. 107, 351-363, 2015]
!                     are 7.0 non-comercial = passenger vehicles
!                     and 7.1 commercial    = trucks and buses (Assume a fix fleet contribution of 10 %)

      real, dimension( 7), parameter :: dyf01 = (/ 1.060,1.060,1.060,1.060,1.060,0.850,0.850 /)  !snap 1
      real, dimension( 7), parameter :: dyf02 = (/ 1.080,1.080,1.080,1.080,1.080,0.800,0.800 /)  !snap 2
      real, dimension( 7), parameter :: dyf03 = (/ 1.080,1.080,1.080,1.080,1.080,0.800,0.800 /)  !snap 3
      real, dimension( 7), parameter :: dyf04 = (/ 1.020,1.020,1.020,1.020,1.020,1.020,0.880 /)  !snap 4
      real, dimension( 7), parameter :: dyf05 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000 /)  !snap 5
      real, dimension( 7), parameter :: dyf06 = (/ 1.200,1.200,1.200,1.200,1.200,0.500,0.500 /)  !snap 6
      real, dimension( 7), parameter :: dyf070= (/ 1.020,1.060,1.080,1.100,1.140,0.810,0.790 /)  !snap 7.0
      real, dimension( 7), parameter :: dyf071= (/ 1.200,1.100,1.100,1.100,1.200,1.200,0.100 /)  !snap 7.1
      real, dimension( 7), parameter :: dyf08 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000 /)  !snap 8
      real, dimension( 7), parameter :: dyf09 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000 /)  !snap 9
      real, dimension( 7), parameter :: dyf10 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000 /)  !snap 10
      real, dimension( 7), parameter :: dyf11 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000 /)  !snap 11

!     HOURFAC Table  "weekday profile" (SMOKE-EU; originally from LOTUS-EURO)

      real, dimension(24), parameter :: hrfw01 = (/ 0.790,0.720,0.720,0.710,0.740,0.800,0.920,1.080,1.190,1.220,  &
                                                    1.210,1.210,1.170,1.150,1.140,1.130,1.100,1.070,1.040,1.020,  &
                                                    1.020,1.010,0.960,0.880                                      /) ! snap1
      real, dimension(24), parameter :: hrfw02 = (/ 0.380,0.360,0.360,0.360,0.370,0.500,1.190,1.530,1.570,1.560,  &
                                                    1.350,1.160,1.070,1.060,1.000,0.980,0.990,1.120,1.410,1.520,  &
                                                    1.390,1.350,1.000,0.420                                      /) ! snap2
      real, dimension(24), parameter :: hrfw03 = (/ 0.750,0.750,0.780,0.820,0.880,0.950,1.020,1.090,1.160,1.220,  &
                                                    1.280,1.300,1.220,1.240,1.250,1.160,1.080,1.010,0.950,0.900,  &
                                                    0.850,0.810,0.780,0.750                                      /) ! snap3
      real, dimension(24), parameter :: hrfw04 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap4
      real, dimension(24), parameter :: hrfw05 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap5
      real, dimension(24), parameter :: hrfw06 = (/ 0.500,0.350,0.200,0.100,0.100,0.200,0.750,1.250,1.400,1.500,  &
                                                    1.500,1.500,1.500,1.500,1.500,1.500,1.500,1.400,1.250,1.100,  &
                                                    1.000,0.900,0.800,0.700                                      /) ! snap6
      real, dimension(24), parameter :: hrfw07 = (/ 0.190,0.090,0.060,0.050,0.090,0.220,0.860,1.840,1.860,1.410,  &
                                                    1.240,1.200,1.320,1.440,1.450,1.590,2.030,2.080,1.510,1.060,  &
                                                    0.740,0.620,0.610,0.440                                      /) ! snap7
      real, dimension(24), parameter :: hrfw08 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap8
      real, dimension(24), parameter :: hrfw09 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap9
      real, dimension(24), parameter :: hrfw10 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap10
      real, dimension(24), parameter :: hrfw11 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap11

!     HOURFAC Table  "summer weekday profile" (special, valid from May-July for traffic)
      real, dimension(24), parameter :: hrfws07= (/ 0.060,0.050,0.090,0.220,0.860,1.840,1.860,1.410,1.240,1.200,  &
                                                    1.320,1.440,1.450,1.590,2.030,2.080,1.510,1.060,0.740,0.620,  &
                                                    0.610,0.440,0.190,0.090                                      /) ! snap7 summer


!     HOURFAC Table  "weekend profile" (SMOKE-EU; originally from LOTUS-EURO)

      real, dimension(24), parameter :: hrfe01 = (/ 0.790,0.720,0.720,0.710,0.740,0.800,0.920,1.080,1.190,1.220,  &
                                                    1.210,1.210,1.170,1.150,1.140,1.130,1.100,1.070,1.040,1.020,  &
                                                    1.020,1.010,0.960,0.880                                      /) ! snap1
      real, dimension(24), parameter :: hrfe02 = (/ 0.380,0.360,0.360,0.360,0.370,0.500,1.190,1.530,1.570,1.560,  &
                                                    1.350,1.160,1.070,1.060,1.000,0.980,0.990,1.120,1.410,1.520,  &
                                                    1.390,1.350,1.000,0.420                                      /) ! snap2
      real, dimension(24), parameter :: hrfe03 = (/ 0.750,0.750,0.780,0.820,0.880,0.950,1.020,1.090,1.160,1.220,  &
                                                    1.280,1.300,1.220,1.240,1.250,1.160,1.080,1.010,0.950,0.900,  &
                                                    0.850,0.810,0.780,0.750                                      /) ! snap3
      real, dimension(24), parameter :: hrfe04 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap4
      real, dimension(24), parameter :: hrfe05 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap5
      real, dimension(24), parameter :: hrfe06 = (/ 0.500,0.350,0.200,0.100,0.100,0.200,0.750,1.250,1.400,1.500,  &
                                                    1.500,1.500,1.500,1.500,1.500,1.500,1.500,1.400,1.250,1.100,  &
                                                    1.000,0.900,0.800,0.700                                      /) ! snap6
      real, dimension(24), parameter :: hrfe07 = (/ 0.190,0.090,0.060,0.050,0.090,0.220,0.860,1.840,1.860,1.410,  &
                                                    1.240,1.200,1.320,1.440,1.450,1.590,2.030,2.080,1.510,1.060,  &
                                                    0.740,0.620,0.610,0.440                                      /) ! snap7
      real, dimension(24), parameter :: hrfe08 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap8
      real, dimension(24), parameter :: hrfe09 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap9
      real, dimension(24), parameter :: hrfe10 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap10
      real, dimension(24), parameter :: hrfe11 = (/ 1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,  &
                                                    1.000,1.000,1.000,1.000                                      /) ! snap11

!     Routines and Functions:

! ************
      contains

!**********************************************************************

      subroutine get_time_table

! The subroutine prepares 2-dim. time varation factor tables

!***********************************************************************

      implicit none

! *** Set up time variation tables for all SNAP sectors

         hourfw( 1,:) = hrfw01(:)
         hourfw( 2,:) = hrfw02(:)
         hourfw( 3,:) = hrfw03(:)
         hourfw( 4,:) = hrfw04(:)
         hourfw( 5,:) = hrfw05(:)
         hourfw( 6,:) = hrfw06(:)
         hourfw( 7,:) = hrfw07(:)
         hourfws(:)   = hrfws07(:)  !snap7 in summer
         hourfw( 8,:) = hrfw08(:)
         hourfw( 9,:) = hrfw09(:)
         hourfw(10,:) = hrfw10(:)
         hourfw(11,:) = hrfw11(:)

         hourfe( 1,:) = hrfe01(:)
         hourfe( 2,:) = hrfe02(:)
         hourfe( 3,:) = hrfe03(:)
         hourfe( 4,:) = hrfe04(:)
         hourfe( 5,:) = hrfe05(:)
         hourfe( 6,:) = hrfe06(:)
         hourfe( 7,:) = hrfe07(:)
         hourfe( 8,:) = hrfe08(:)
         hourfe( 9,:) = hrfe09(:)
         hourfe(10,:) = hrfe10(:)
         hourfe(11,:) = hrfe11(:)

         daywf( 1,:) = dyf01(:)
         daywf( 2,:) = dyf02(:)
         daywf( 3,:) = dyf03(:)
         daywf( 4,:) = dyf04(:)
         daywf( 5,:) = dyf05(:)
         daywf( 6,:) = dyf06(:)
! MSK         daywf( 7,:) = (0.95*dyf070(:) + 0.05*dyf071(:))
! MSK changed commercial vehicle fraction to 10 %
         daywf( 7,:) = (0.90*dyf070(:) + 0.10*dyf071(:))
         daywf( 8,:) = dyf08(:)
         daywf( 9,:) = dyf09(:)
         daywf(10,:) = dyf10(:)
         daywf(11,:) = dyf11(:)

         mnthf( 1,:) = mnf01(:)
         mnthf( 2,:) = mnf02(:)
         mnthf( 3,:) = mnf03(:)
         mnthf( 4,:) = mnf04(:)
         mnthf( 5,:) = mnf05(:)
         mnthf( 6,:) = mnf06(:)
         mnthf( 7,:) = mnf07(:)
         mnthf( 8,:) = mnf08(:)
         mnthf( 9,:) = mnf09(:)
         mnthf(10,:) = mnf10(:)
         mnthf(11,:) = mnf11(:)

! End of subroutine get_time_table

      end subroutine get_time_table

!**********************************************************************

      subroutine str2int(str,int,stat)

! The subroutine converts a string into an integer

!***********************************************************************

      implicit none

      ! Arguments
      character(len=*),intent(in) :: str
      integer,intent(out)         :: int
      integer,intent(out)         :: stat

      read(str,*,iostat=stat)  int

! End of subroutine str2int

      end subroutine str2int


!**********************************************************************

      subroutine utm2ll(ISONE,UTMN,UTME,LAT,LON)

! The subroutine converts UTM north- and east-coordinates to latitude
! and longitude

!***********************************************************************

      implicit none

      integer, intent(in)   :: ISONE
      real, intent(in)      :: UTMN
      real, intent(in)      :: UTME
      real, intent(out)     :: LAT
      real, intent(out)     :: LON

! ISONE - UTM sone
! UTMN  - UTM north-coordinate (X) (meter from equator)
! UTME  - UTM  east-coordinate (Y) (meter from west border)
! LAT   - Latitude  in decimal degrees
! LON   - Longitude in decimal degrees

! Local variables

      double precision :: A,BB0,DEAST,E,E2,F,FI,LA0,M,N,PI,SCAL,X,Y

! A     - Store halvakse
! BB0   - Intermediate value
! DEAST - \stforskyvning UTM
! E     - Intermediate value 
! E2    - Intermediate value
! F     - Flattrykning
! FI    - Intermediate value
! LA0   - Tangeringsmeridian
! M     - Intermediate value
! N     - Intermediate value
! PI    - The mathematical constant Pi
! SCALE - Scale UTM
! X     - Scaled north-coordinate
! Y     - Scaled  east-coordinate

! input line
      character(len=1024) :: arg(3)
      integer :: ia


! Define constants

      A     = 6378388.
      DEAST = 500000.
      F     = 1./297.
      PI    = 3.1415927
      SCAL = 0.9996

!*******************************************************************************
! Scale coordinates

      X   = UTMN/SCAL
      Y   = (UTME - DEAST)/SCAL
      LA0 = (ISONE - 30)*6. - 3.

! Calculate some intermediate quantities

      E2  = F*(2. - F)
      BB0 = (1. - F/2. + F*F/16. + F*F*F/32.)*A

      FI  = X/BB0 +                      &
           (3.*F/4. + 3.*F*F/8. + 21.*F*F*F/256.)*DSIN(2.*X/BB0) +          &
           (21.*F*F/64. + 21.*F*F*F/64.)*DSIN(4.*X/BB0) +          &
           (151.*F*F*F/768.)*DSIN(6.*X/BB0)

      N   = A/DSQRT(1. - E2*DSIN(FI)*DSIN(FI))
      E   = DSQRT(E2*DCOS(FI)*DCOS(FI)/(1. - E2))
      M   = N/(1. + E*E)

! Calculate latitude and longitude in radians

      LAT = FI - (Y*Y*DTAN(FI))/(2.*M*N) +          &
           (Y*Y*Y*Y*DTAN(FI))/(24.*M*N*N*N)*          &
           (5. + 3.*DTAN(FI)*DTAN(FI) + E*E -          & 
            9.*E*E*DTAN(FI)*DTAN(FI) - 4.*E*E*E*E) -          &
           (Y*Y*Y*Y*Y*Y*DTAN(FI))/(720.*M*N*N*N*N*N)*          &
           (61 + 90*DTAN(FI)*DTAN(FI) +          &
            45*DTAN(FI)*DTAN(FI)*DTAN(FI)*DTAN(FI))

      LON = Y/(N*DCOS(FI)) -          &
           (Y*Y*Y*(1. + 2.*DTAN(FI)*DTAN(FI) + E*E))/          &
           (6.*N*N*N*DCOS(FI)) +          &
           (Y*Y*Y*Y*Y*(5. + 28.*DTAN(FI)*DTAN(FI) +           &
           24.*DTAN(FI)*DTAN(FI)*DTAN(FI)*DTAN(FI)))/          &
           (120.*N*N*N*N*N*DCOS(FI)) + LA0*PI/180.

! Convert from radians to degrees

      LAT = LAT*180./PI
      LON = LON*180./PI

!        write(6,      '(F9.5)')  LAT
!        write(6,      '(F9.5)')  LON

! End of subroutine utm2ll

      end subroutine utm2ll


! End of module module_uect_emis

      end module module_uect_emis
