    module onsite
!=========================================================================================================
!   MODULE ONSITE
!   THIS MODULE CONTAINS VARIABLES AND SUBROUTINES NEEDED TO PROCESS ONSITE OR PROGNOSTIC DATA.
!
!   MODIFIED MAY 5, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   USED BY:        MODULE READ_INPUT, MOD_REPORTS, MOD_MISC
!
!   Integer variables
!   osstart:        onsite or PROG extraction start date (YYYMMDD)
!                   start hour assumed to be 1
!   osend:          onsite or PROG extract end date (YYYYMMDD)
!                   end hour assumed to be 24
!   osgmt2lst:      conversion from Greenwich Mean Time (GMT) to
!                   local standard time (LST).
!                   example:  GMT to eastern time zone (EST) is 5
!                   do not have to account for daylight savings time
!   delta_t_ind:    Number of DELTA_TEMP indices
!   noshts:         Number of heights read from OSHEIGHTS keyword(s)
!   nlevel:         Number of vertical levels read from DATA file
!   nobs_hr         Number of observations/hour
!   min_obs         Number of mininum number of observations/hour allowed 
!                   used to compare number of calms and missing to determine if hour is 
!                   calm or missing
!   min_obs_hr:     Minimum number of observations/hr for QA (1)
!   max_obs_hr      Maximum number of observations/hour allowed (12)
!   nread:          Number of read statements encountered in stage 1 runstream file when in correct order
!   nformat:        Number of format statements encountered in stage 1 runstream file when in correct order
!   nosvars:        number of onsite or PROG variables
!   maxread:        maximum number of READ statements allowed per observation
!                   period (set to 50)
!   maxvars:        maximum number of variables on one data record (set to 50)
!   maxlevel:       maximum number of levels for onsite/PROG data( set to 50)
!   maxdtind:       maximum number of indices for DELTA_TEMP values (set to 3)
!   varmap1:        temporary integer array mapping the onsite variables to 
!                   read onsite variables. 1st dimension is the max number of reads
!                   second dimension is nosvars
!                   3rd dimension is 2.  this array will be deallocated after
!                   reading the runstream file
!   varmap:         integer array mapping the onsite variables to 
!                   read onsite variables. 1st dimension is the number of reads
!                   second dimension is nosvars
!                   3rd dimension is 2.  this array will be allocated after
!                   reading the runstream file and filled from varmap1
!   noskeys:        number of ONSITE keywords
!   nreadvars:      maximum number of variables to read among the READ statements
!   osdates:        2x3 array of start month, day, year and end month,
!                   day and year.  1st dim is the start (1) or end (2)
!                   2nd dim is year (1), month (2), and day (3)
!   os_date:        temporary array of onsite dates (MMDDYYY) in LST
!                   this array is used to fill in the dates for
!                   osdate in the derived data type os_info
!   obs_per_hr:     temporary array of # of minutes per hour
!   nos:            5-element array denoting the total number of obs read from
!                   file:
!                   1:  total obs read (not used)
!                   2:  total obs read in data window
!                   3:  maximum number of obs/day (not used)
!                   4:  duplicate observations
!                   5:  sub-hourly values not used
!   nbadhts:		number of bad heights read from data
!   nosdays:        number of days in data period
!   os_windstats:   2x3 array of wind stats;  1st dimension is hourly (1) or sub-hourly (2)
!                   2nd dimension is type of stat. 1=calms, 2=have wind direction but no wind speed (not read in)
!                   3=have wind speed but no wind direction (not read in)
!   os_tempstat:	2-d array of number of obs where temperature < dewpoint (1=hourly, 2=sub-hourly)
!   nos_audit_vars: number of audited variables
!
!   the following variables are used to denote the location of the site-specific meterological variables
!   in osvars.  The variables are used instead of hard-coded values to allow for the flexibilty to add
!   new variables in the future.
!   for example, heat flux is #1 in the osvars list so hflux_var=1 in onsite_init.  If a new variable were to be added
!   before heat flux, then hflux_var is set to 2 in onsite_init without the code having to be modified to change it.
!   this eliminates potential errors in the future due to coding mistakes or oversights.
!   hflux_var:      sensible heat flux
!   Lflux_var:      latent heat flux
!   ustar_var:      u*
!   mix_var:        mixing height
!   zo_var:         surface roughness
!   inso_var:	    insolation
!   nrad_var:       net radiation
!   dt01_var:       delta-temp 1
!   vptg_var:       theta lapse rate
!   l_var:          Monin-Obukhov length
!   wstar_var:      w*
!   clht_var:       ceiling height
!   height_var:     height
!   wind_vars(2):   wind speed and wind direction
!   prcp_vars(2):   precipitation
!   std_vars(2):    standard deviation variables (first and last one)
!   temp_var:       temperature
!   dewpt_var:      dewpoint
!   cloud_var:      cloud cover
!   slp_var:        sea level pressure
!   pres_var:       station pressure
!   rh_var:         relative humidity
!   sa_var:         sigma-theta
!   se_var:         std. dev. of vertical wind direction
!   sv_var:         sigma-v
!   sw_var:         sigma-w
!   su_var:         sigma-u
!   vv_var:         w (vertical wind speed)
!   alb_var:        albedo
!   bo_var:         Bowen ratio
!
!   Real variables
!   osdt_hts:       array of heights used for delta T temperatures for later
!                   use in Bulk Richardson processing
!                   1st dimension is an index from 1 to 3, meaning up to
!                   three delta-t levels
!                   note, only the index = 1 is used for processing
!                   if others found, issue warning
!                   for now keeping an upper limit of 3 in case
!                   future values used.
!                   2nd dimension is the level (1=lower, 2=upper)
!                   values specified by DELTA_TEMP keyword
!   osheights1      temporary array of heights read from the OSHEIGHTS keyword
!                   its size is set by maxlevel
!   os_heights:     final array of heights read from OSHEIGHTS keyword
!                   its size is set by noshts
!   threshspeed:    wind speed associated with THRESHOLD keyword
!   minthresh:      minimum wind speed for THRESHOLD, set to 0
!   minthresh:      minimum wind speed for THRESHOLD for PROGnostic data, set to 0
!   warnthresh:     wind speed to produce a warning message
!                   when threshspeed exceeds this  number (0.5 m/s)
!   maxthresh       wind speed to produce an error when
!                   threshspeed exceeds the speed (1.0 m/s)
!   oslat:          latitude of onsite station or PROG grid cell
!   oslon:          longitude of onsite station or PROG grid cell
!   oselev:         surface onsite station or PROG grid cell
!   xdir:           running total of x-component of wind direction for averaging
!   ydir:           running total of y-component of wind direction for averaging
!
!   Logical variables
!   loskeywords:    array denoting if keywords found
!   1:              denotes that DATA keyword (ikey=4) found for onsite or PROG data. keyword
!                   is mandatory
!   2:              denotes NO_MISSING keyword (ikey=5) found
!   3:              denotes FORMAT keyword found (ikey=6)
!   4:              denotes that QAOUT keyword (ikey=8) found.  If processing stages 1
!                   and 2 in the same run, this keyword is optional.
!                   If processing stage 1 only, this is mandatory.
!   5:              denotes XDATES keyword found (ikey=9)
!   6:              denotes LOCATION keyword found (ikey=10)
!   7:              denotes RANGE keyword found (ikey=12)
!   8:              denotes AUDIT keyword found (ikey=13)
!   9:              denotes DELTA_TEMP keyword found (ikey=14)
!   10:             denotes OSHEIGHTS keyword found (ikey=15)
!   11:             denotes THRESHOLD keyword found (ikey=16)
!   12:             denotes READ keyword found (ikey=17)
!   13:             denotes OBS/HR keyword found (ikey=27)
!
!   luseoshts:      denotes OSHEIGHTS will be used instead of heights (HT) read from
!                   data file
!   lvector:        denotes that wind speed and direction will be vector averages
!                   instead of default scalar averages
!                   are not written past level 1.
!   lgmt2lst:       variable denoting if GMT to LST found (true if so)
!   loselev:        denotes if user entered elevation on LOCATION keyword (true if entered)
!   overland:       denotes if prognostic data is overland (true) or over water (false).
!   checktempdew:   compare dewpoint and temperature if both read in.
!   laudit_all:     audit all variables
!   losobs:         have at least one obs for a day
!   nomisshr:       given hour for a given day has an observation
!   lcalm_obs:      a minute is calm or not for given level
!   lcalm_obs_hr:   calm hour for given level
!   osstage1:       denotes if stage 1 being processed for ONSITE or PROG data
!
!   Character variables
!   osid:           surface station ID
!   os_format1      temporary array of format statements read from input file
!                   number of elements is maxread
!                   this array will be deallocated after
!                   reading the runstream file
!   os_format       array of format statements read from input file
!                   number of elements is nformat
!                   this array will be assigned from os_format1
!   os_reads        array of read statements read from input file
!                   used for QA purposes
!   qaout_vars:     list of variable names of variables being audited
!   dattype:        indicates whether onsite or PROG data used
!
!   data type ovars (osvars)     
!   varname:        variable name, i.e. PRES=pressure; see subroutine onsite_init for definitions
!   lread:          logical variable denoting that the variable name was found on the READ
!                   keyword and this variable will be processed in AERMET.
!   laudit:         logical variable denoting if variable will be audited. Initial value is 
!                   false, no audit, except for dry bulb temperature (TMPD), wind speed (WDIR),
!                   and wind speed (WSPD) which are automatically tracked
!   lmod:           logical variable denoting if variable has already been listed with the
!                   RANGE keyword.  If listed more than once, user is warned and latest
!                   values used.
!   lnomiss:        logical variable denoting to skip (true) reporting hourly missing values
!                   of variable to MESSAGE file.  False means to report the value 
!                   (initial default value) when missing in MESSAGE file.
!   luse:           logical variable denoting that variable is used (true) by AERMET or
!                   not used (false) by AERMET
!                   user warned if variable listed. variables such as heat flux (HFLUX)
!                   are not used.
!   lincbound:      logical variable denoting if < or <= used for comparing to bounds.
!                   lincbound=.true. means to not include the upper and lower bounds in the range
!                   i.e. logic is lower < value < upper
!                   lincbound=.false. means to include the upper and lower bounds in the range
!                   i.e. logic is lower <= value <= upper
!   readvar:		location of variable in osdata1
!   misscount:      integer value of # of missing hours
!   missval:        default missing value
!   lowbound:       lower accetable bound
!   upbound:        upper accetable bound
!   firstlev:       first level for variable
!   nlevels:        number of height levels read for the variable
!                   for single level variables, this should only be 1
!                   multi-level data can be up to maxlevel (see definition above)
!
!   data type osinfo (os_info)
!   osdate:         integer date of observation
!   have_obs:       logical variable denoting hour has an obs (based on nomisshr)
!   sobs:           have at least one observation for the day
!=========================================================================================================
!   variables/subroutines to use from main1 and file_units
!   ipath, nfield, lbad, r8, writeunit, and pathid used in a majority subroutines, keep here
    use main1, only: ipath,nfield,lbad,r8,writeunit,pathid,msg_form
    
!   msg_unit used in a majority of subroutines, keep here 
    use file_units, only: msg_unit
      
    implicit none
      
    integer(kind=8) :: osstart=0
    integer(kind=8) :: osend=0
    integer(kind=4) :: osgmt2lst=-9
    integer(kind=4) :: delta_t_ind=1
    integer(kind=4) :: noshts=0
    integer(kind=4) :: nlevel=1
    integer(kind=4) :: nobs_hr=1
    integer(kind=4) :: nread=0
    integer(kind=4) :: nformat=0
    integer(kind=4) :: nreadvars=0
    integer(kind=4) :: nbadhts=0
    integer(kind=4) :: min_obs=0 
    integer(kind=4) :: osdates(2,3)=0
    integer(kind=4) :: nosdays=0
    integer(kind=4) :: os_windstats(2,3)=0
    integer(kind=4) :: os_tempstat(2)=0
    integer(kind=4) :: nos_audit_vars=0 
    
!   the following are based on onsite_init
    integer(kind=4) :: hflux_var
    integer(kind=4) :: lflux_var
    integer(kind=4) :: ustar_var
    integer(kind=4) :: mix_var
    integer(kind=4) :: zo_var
    integer(kind=4) :: inso_var
    integer(kind=4) :: nrad_var
    integer(kind=4) :: dt01_var
    integer(kind=4) :: vptg_var
    integer(kind=4) :: l_var
    integer(kind=4) :: wstar_var
    integer(kind=4) :: clht_var
    integer(kind=4) :: height_var
    integer(kind=4) :: wind_vars(2)
    integer(kind=4) :: prcp_vars(2)
    integer(kind=4) :: std_vars(2)
    integer(kind=4) :: temp_var
    integer(kind=4) :: dewpt_var
    integer(kind=4) :: cloud_var
    integer(kind=4) :: slp_var
    integer(kind=4) :: pres_var
    integer(kind=4) :: rh_var
    integer(kind=4) :: sa_var
    integer(kind=4) :: se_var
    integer(kind=4) :: sv_var
    integer(kind=4) :: sw_var
    integer(kind=4) :: su_var
    integer(kind=4) :: vv_var
    integer(kind=4) :: alb_var
    integer(kind=4) :: bo_var
     
!   end variables set in onsite_init
    
    integer(kind=4), parameter :: nosvars=42 !40
    integer(kind=4), parameter :: maxread=50
    integer(kind=4), parameter :: maxvars=50
    integer(kind=4), parameter :: maxlevel=50
    integer(kind=4), parameter :: maxdtind=3
    integer(kind=4), parameter :: min_obs_hr=1
    integer(kind=4), parameter ::max_obs_hr=12
    integer(kind=4), parameter :: noskeys=13
    integer(kind=4), allocatable, dimension(:,:,:) :: varmap1
    integer(kind=4), allocatable, dimension(:,:,:) :: varmap
    integer(kind=4), allocatable, dimension(:,:) :: os_audit_index
    integer(kind=4), allocatable, dimension(:,:,:,:) :: os_audit_counts
    integer(kind=4), allocatable, dimension(:) :: read_index !list of variables that will be read in the order of osvars
    integer(kind=4), allocatable, dimension(:) :: ext_index !list of variables that will be read from QAOUT file
    integer(kind=4) :: nos(5)=0
    integer(kind=4) :: nvars=0 !(2)=0 !number of variables (1 for single, 2 for multilevel)
    
    integer(kind=8), allocatable, dimension(:) :: os_date
    integer(kind=4), allocatable, dimension(:,:,:,:) :: obs_per_hr 
    
    real(kind=r8), allocatable, dimension(:,:) :: osdt_hts
    real(kind=r8), allocatable, dimension(:) :: osheights1
    real(kind=r8), allocatable, dimension(:) :: os_heights
    real(kind=r8), allocatable, dimension(:) :: os_subhr_vals(:,:,:,:,:) !multi level sub-hourly data
    real(kind=r8), allocatable, dimension(:) :: osdata1(:,:,:,:) !post QA
    real(kind=r8), allocatable, dimension(:) :: osdata1a(:,:,:,:) !pre qa
    real(kind=r8), allocatable, dimension(:) :: missvals
    real(kind=r8), allocatable, dimension(:) :: lowbound
    real(kind=r8), allocatable, dimension(:) :: upbound
    
    real(kind=r8) :: threshspeed=0.0_r8
    real(kind=r8), parameter :: minthresh=0.0_r8
    real(kind=r8), parameter :: progthresh=0.0_r8
    real(kind=r8) :: warnthresh=0.5_r8 
    real(kind=r8), parameter ::maxthresh=1.0_r8
    real(kind=r8) :: oslat=0.0_r8
    real(kind=r8) :: oslon=0.0_r8
    real(kind=r8) :: oselev=0.0_r8
    real(kind=r8) :: xdir=0.0_r8
    real(kind=r8) :: ydir=0.0_r8
    real(kind=r8) :: dirsum=0.0_r8
  
!   onsite or PROG variables
    type ovars
        character(len=4) :: varname
        logical :: lread,laudit,lmod,lnomiss,luse,lincbound
        integer(kind=4) :: readvar,misscount,missval,lowbound,upbound,firstlev,nlevels
        real(kind=r8) :: conv
    end type ovars
      
    type(ovars) :: osvars(nosvars)
      
    logical :: loskeywords(noskeys)=.false.
    logical :: luseoshts=.false.
    logical :: lvector=.false.
    logical :: lbados=.false.
    logical :: checktempdew=.false.
    logical :: laudit_all=.false.
    logical :: lgmt2lst=.false.
    logical :: loselev=.false.
    logical :: overland=.true.
    logical :: osstage1=.false.
    logical, allocatable, dimension(:) :: losobs
    logical, allocatable, dimension(:,:) :: nomisshr
    logical, allocatable, dimension(:,:,:,:) :: lcalm_obs !each minute is listed as calm or not
    logical, allocatable, dimension(:,:,:) :: lcalm_obs_hr 
    
    character(len=8) :: osid=''
    character(len=100), allocatable, dimension(:) :: os_format1(:)
    character(len=100), allocatable, dimension(:) :: os_format(:)
    character(len=300), allocatable, dimension(:) :: os_reads(:)
    character(len=4), allocatable, dimension(:) :: qaout_vars
    character(len=6) :: dattype(2)
    
    
    type osinfo
        integer(kind=8) :: osdate
        logical :: sobs,have_obs(24)
    end type osinfo
    type(osinfo), dimension(:),allocatable:: os_info
    
    data dattype /'onsite','PROG'/
    
    contains 
!*********************************************************************************************************
      
    subroutine onsite_init
!=========================================================================================================
!   SUBROUTINE ONSITE_INIT
!   THIS SUBROUTINE INITIALIZES THE DATA TYPE OSVARS
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PATH), MODULE PBL (PBL_PROC)  
!
!   Integer variables
!   ivar:           variable loop counter
!=========================================================================================================
    implicit none
    integer(kind=4) :: ivar
!   note missing, lower and upper bounds scaled
!   by multipliers, e.g. pressure is milibars*10, not milibars
!   1012 mb=10120
!   with exception of name, all values can be reset by user
!   using NO_MISSING for lnomiss, and RANGE for less than, missval, lowbound, and upbound
!
!   variables 1-24 are single level variables; variables 25-39 are multi-level 
!   values for variables 6-39 are read in as real
!   values for variables 1-5 (date and time variables) are read in as integers
!
!   lread is initialized to false
!   laudit is initialized to false except for temperature, wind speed and direction, which are initialized to true
!   since they are automatically audited
!   lmod is initialized to false
!   lnomis is initialized to false
!   luse is initialized to true except for those variables denoted as not being used by AERMET below in which case
!   they are initialized to false
!   lincbound is initialized to false for variables that do not include the bounds, and true for those that do
!   include the bounds.  See Table B-4 and B-5 in AERMET User's guide to see which variables include/do not include 
!   the bounds
!   readvar is initialized to 0. it will be set in os_proc when checking to see what variables are read
!   missval, lowbound, and upbound are initialized to the values listed in Tables B-4 and B-5 in AERMET User's guide
!   nlevels is initialized to 0

!   initialize onsite variables

!   if a new variable is added, be sure to update the following subroutines
!   os_list, os_vars, and os_proc, avg_hr, os_test

!                   name    lread laudit lmod   lnomis luse  lincbound readvar misscount missval lowbound upbound firstlev nlevels conv
    osvars(1)=ovars('OSDY',.false.,.false.,.false.,.false.,.true.,.true.,0,0,-9,1,31,0,0,1.0_r8) !day of month
    osvars(2)=ovars('OSMO',.false.,.false.,.false.,.false.,.true.,.true.,0,0,-9,1,12,0,0,1.0_r8) !month of year (1 - 12)
    osvars(3)=ovars('OSYR',.false.,.false.,.false.,.false.,.true.,.true.,0,0,-9,0,9999,0,0,1.0_r8) !4-digit year
    osvars(4)=ovars('OSHR',.false.,.false.,.false.,.false.,.true.,.true.,0,0,-9,0,24,0,0,1.0_r8) !hour of day 0-23 or 1-24
    osvars(5)=ovars('OSMN',.false.,.false.,.false.,.false.,.true.,.true.,0,0,-9,0,60,0,0,1.0_r8) !minute of hour 0-60
      
    osvars(6)=ovars('HFLX',.false.,.false.,.false.,.false.,.true.,.false.,0,0,-999,-100,800,0,0,1.0_r8) !sensible heat flux (W/m^2) (not used by AERMET)
    osvars(7)=ovars('LFLX',.false.,.false.,.false.,.false.,.true.,.false.,0,0,-999,-100,800,0,0,1.0_r8) !latent heat flux (W/m^2) (not used by AERMET)
    
    osvars(8)=ovars('USTR',.false.,.false.,.false.,.false.,.true.,.false.,0,0,-9,0,2,0,0,1.0_r8) !surface friction velocity, u* (m/s)
    osvars(9)=ovars('MHGT',.false.,.false.,.false.,.false.,.true.,.false.,0,0,9999,0,4000,0,0,1.0_r8) !mixing heights
    osvars(10)=ovars('ZOHT',.false.,.false.,.false.,.false.,.true.,.false.,0,0,999,0,2,0,0,1.0_r8) !surface roughness length (m) (not used by AERMET)
    osvars(11)=ovars('SAMT',.false.,.false.,.false.,.false.,.false.,.true.,0,0,999,0,250,0,0,1.0_r8) !snow amount (cm) (not used by AERMET)
    osvars(12)=ovars('PAMT',.false.,.false.,.false.,.false.,.true.,.true.,0,0,999,+0,25,0,0,1.0_r8) !precipitation amount (cm)
    osvars(13)=ovars('INSO',.false.,.false.,.false.,.false.,.true.,.true.,0,0,+9999,0,1250,0,0,1.0_r8) !solar insolation (W/m^2)
    osvars(14)=ovars('NRAD',.false.,.false.,.false.,.false.,.true.,.false.,0,0,+999,-100,800,0,0,1.0_r8) !net radiation (W/m^2)
    osvars(15)=ovars('DT01',.false.,.false.,.false.,.false.,.true.,.false.,0,0,9,-2,5,0,0,1.0_r8) !temperature difference between two levels (upper level - lower level) (Celcius)
    osvars(16)=ovars('DT02',.false.,.false.,.false.,.false.,.false.,.false.,0,0,+9,-2,5,0,0,1.0_r8) !temperature difference between two levels (upper level - lower level) (Celcius) (not used by AERMET)
    osvars(17)=ovars('DT03',.false.,.false.,.false.,.false.,.false.,.false.,0,0,9,-2,5,0,0,1.0_r8) !temperature difference between two levels (upper level - lower level) (Celcius) (not used by AERMET)
    osvars(18)=ovars('VPTG',.false.,.false.,.false.,.false.,.true.,.true.,0,0,-9,0,5,0,0,0.001_r8) !potential temperature lapse rate (C/m)    
    osvars(19)=ovars('MOBL',.false.,.false.,.false.,.false.,.true.,.true.,0,0,-99999,-8888,8888,0,0,1.0_r8) !Monin-Obukov length (m)
    osvars(20)=ovars('WSTR',.false.,.false.,.false.,.false.,.true.,.false.,0,0,-9,0,2,0,0,1.0_r8) !convective velocity scale (m/s)
    osvars(21)=ovars('ALBD',.false.,.false.,.false.,.false.,.true.,.true.,0,0,999,0,1,0,0,1.0_r8) !albedo
    osvars(22)=ovars('BOWN',.false.,.false.,.false.,.false.,.true.,.true.,0,0,999,-10,10,0,0,1.0_r8) !Bowen ratio
    !osvars(22)=ovars('US03',.false.,.false.,.false.,.false.,.false.,.false.,0,999,0,100,0,0,1.0_r8) !user's scalar #3 (user's units) (not used by AERMET)
      
    osvars(23)=ovars('PRCP',.false.,.false.,.false.,.false.,.true.,.true.,0,0,-9,0,25400,0,0,0.01_r8) !precipitation (mm*100)
    osvars(24)=ovars('SLVP',.false.,.false.,.false.,.false.,.true.,.true.,0,0,99999,9000,10999,0,0,0.1_r8) !sea level pressure (mmb*10)
    osvars(25)=ovars('PRES',.false.,.false.,.false.,.false.,.true.,.false.,0,0,99999,9000,10999,0,0,0.1_r8) !station pressure (mb*10)
    osvars(26)=ovars('CLHT',.false.,.false.,.false.,.false.,.false.,.true.,0,0,999,0,300,0,0,1.0_r8) !ceiling height (km*10) (not used by AERMET)
    osvars(27)=ovars('TSKC',.false.,.false.,.false.,.false.,.true.,.true.,0,0,99,0,10,0,0,1.0_r8) !total/opaque sky cover (tenths)
    !osvars(25)=ovars('TSKC',.false.,.false.,.false.,.false.,.true.,.true.,0,9999,0,10,0,0,1.0_r8)  
      
!   the following are multi level data and use NN
!   as the last 2-characters, but the actual values listed in the AERMET input file will
!   have a number, i.e. HTNN is height, but the input file will have HT01, HTO2, etc.
    osvars(28)=ovars('HTNN',.false.,.false.,.false.,.false.,.true.,.false.,0,0,9999,0,4000,0,0,1.0_r8) !height (m)
    osvars(29)=ovars('SANN',.false.,.false.,.false.,.false.,.true.,.false.,0,0,99,0,35,0,0,1.0_r8) !standard deviation of horizontal wind (degrees)
    osvars(30)=ovars('SENN',.false.,.false.,.false.,.false.,.false.,.false.,0,0,+99,0,25,0,0,1.0_r8) !standard deviation of vertical wind (degrees) (not used by AERMET)
    osvars(31)=ovars('SVNN',.false.,.false.,.false.,.false.,.false.,.false.,0,0,99,0,3,0,0,1.0_r8) !standard deviation of v-component of the wind (m/s) (not used by AERMET)
    osvars(32)=ovars('SWNN',.false.,.false.,.false.,.false.,.true.,.false.,0,0,99,0,3,0,0,1.0_r8) !standard deviation of w-component of the wind (m/s)
    osvars(33)=ovars('SUNN',.false.,.false.,.false.,.false.,.false.,.false.,0,0,+99,0,3,0,0,1.0_r8) !standard deviation of u-component of the wind (m/s) (not used by AERMET)
    osvars(34)=ovars('TTNN',.false.,.true.,.false.,.false.,.true.,.false.,0,0,99,-30,40,0,0,1.0_r8) !temperature (degrees C)
    osvars(35)=ovars('WDNN',.false.,.true.,.false.,.false.,.true.,.true.,0,0,999,0,360,0,0,1.0_r8) !wind direction (degrees from North)
    osvars(36)=ovars('WSNN',.false.,.true.,.false.,.false.,.true.,.true.,0,0,99,0,50,0,0,1.0_r8) !wind speed (m/s)
    osvars(37)=ovars('VVNN',.false.,.false.,.false.,.false.,.false.,.false.,0,0,999,0,5,0,0,1.0_r8) !vertical wind component (m/s) (not used by AERMET)
    osvars(38)=ovars('DPNN',.false.,.false.,.false.,.false.,.true.,.false.,0,0,99,-65,35,0,0,1.0_r8) !dew point temperature (degrees C) 
    osvars(39)=ovars('RHNN',.false.,.false.,.false.,.false.,.true.,.true.,0,0,999,0,100,0,0,1.0_r8) !relative humidity (whole percent) 
    osvars(40)=ovars('V1NN',.false.,.false.,.false.,.false.,.false.,.false.,0,0,999,0,100,0,0,1.0_r8) !user's vector #1 (user's units) (not used by AERMET)
    osvars(41)=ovars('V2NN',.false.,.false.,.false.,.false.,.false.,.false.,0,0,999,0,100,0,0,1.0_r8) !user's vector #2 (user's units) (not used by AERMET)
    osvars(42)=ovars('V2NN',.false.,.false.,.false.,.false.,.false.,.false.,0,0,999,0,100,0,0,1.0_r8) !user's vector #3 (user's units) (not used by AERMET)
    
!   loop through the variables and set parameter variable wind_vars, prcp_vars, std_vars, temp_var, dewpt_var, etc. 
!   These variables are used in the onsite module as well as the pbl module.  These variables tell AERMET
!   whether these key variables have been read from an onsite data file.
!   The variables are used instead of hard-coded values to allow for the flexibilty to add
!   new variables in the future.
!   for example, heat flux is #1 in the osvars list so hflux_var=1.  If a new variable were to be added
!   before heat flux, then hflux_var is set to 2 in onsite_init without the code having to be modified to change it.
!   this eliminates potential errors in the future due to coding mistakes or oversights.    
    
    l1: do ivar=1,nosvars
        if (trim(adjustl(osvars(ivar)%varname)) == 'HFLX') hflux_var=ivar !sensible heat flux
        if (trim(adjustl(osvars(ivar)%varname)) == 'LFLX') lflux_var=ivar !sensible heat flux
        if (trim(adjustl(osvars(ivar)%varname)) == 'USTR') ustar_var=ivar !u*
        if (trim(adjustl(osvars(ivar)%varname)) == 'MHGT') mix_var=ivar !mixing height
        if (trim(adjustl(osvars(ivar)%varname)) == 'ZOHT') zo_var=ivar !zo
        if (trim(adjustl(osvars(ivar)%varname)) == 'PAMT') prcp_vars(1)=ivar !PAMT
        if (trim(adjustl(osvars(ivar)%varname)) == 'INSO') inso_var=ivar !insolation
        if (trim(adjustl(osvars(ivar)%varname)) == 'NRAD') nrad_var=ivar !net radiation
        if (trim(adjustl(osvars(ivar)%varname)) == 'DT01') dt01_var=ivar !delt-T 1
        if (trim(adjustl(osvars(ivar)%varname)) == 'VPTG') vptg_var=ivar !theta lapse rate
        if (trim(adjustl(osvars(ivar)%varname)) == 'MOBL') l_var=ivar !Monin-Obukhov length
        if (trim(adjustl(osvars(ivar)%varname)) == 'WSTR') wstar_var=ivar !w*
        if (trim(adjustl(osvars(ivar)%varname)) == 'ALBD') alb_var=ivar !albedo
        if (trim(adjustl(osvars(ivar)%varname)) == 'BOWN') bo_var=ivar !Bowen ratio
        if (trim(adjustl(osvars(ivar)%varname)) == 'PRCP') prcp_vars(2)=ivar !PAMT
        if (trim(adjustl(osvars(ivar)%varname)) == 'SLVP') slp_var=ivar !sea level pressure
        if (trim(adjustl(osvars(ivar)%varname)) == 'PRES') pres_var=ivar !station pressure
        if (trim(adjustl(osvars(ivar)%varname)) == 'CLHT') clht_var=ivar !ceiling height
        if (trim(adjustl(osvars(ivar)%varname)) == 'TSKC') cloud_var=ivar !cloud cover
        if (trim(adjustl(osvars(ivar)%varname)) == 'HTNN') height_var=ivar !height
        if (trim(adjustl(osvars(ivar)%varname)) == 'SANN') then !sigma-theta first std dev. variable
            std_vars(1)=ivar 
            sa_var=ivar
        endif
        if (trim(adjustl(osvars(ivar)%varname)) == 'SENN') se_var=ivar !sigma-theta for vertical wind
        if (trim(adjustl(osvars(ivar)%varname)) == 'SVNN') sv_var=ivar !std dev for v-wind
        if (trim(adjustl(osvars(ivar)%varname)) == 'SWNN') sw_var=ivar !std dev for w-wind
        if (trim(adjustl(osvars(ivar)%varname)) == 'SUNN') then !sigma-u, last std dev. variable
            std_vars(2)=ivar 
            su_var=ivar
        endif
        if (trim(adjustl(osvars(ivar)%varname)) == 'TTNN') temp_var=ivar !temperature
        if (trim(adjustl(osvars(ivar)%varname)) == 'WDNN') wind_vars(1)=ivar !wind direction
        if (trim(adjustl(osvars(ivar)%varname)) == 'WSNN') wind_vars(2)=ivar !wind speed
        if (trim(adjustl(osvars(ivar)%varname)) == 'DPNN') dewpt_var=ivar !dewpoint
        if (trim(adjustl(osvars(ivar)%varname)) == 'RHNN') rh_var=ivar !temperature
        if (trim(adjustl(osvars(ivar)%varname)) == 'VVNN') vv_var=ivar !vertical velocity
    enddo l1
    
    
    return
    end subroutine onsite_init
!*********************************************************************************************************
      
    subroutine os_path
!=========================================================================================================
!   SUBROUTINE OS_PATH
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE ONSITE OR PROG PATHWAY.  THIS INCLUDES: DATA, EXTRACT, QAOUT, RANGE, AUDIT, NO_MISSING,
!   KEYWORDS XDATES, LOCATION, READ, FORMAT, THRESHOLD, AND OBS/HOUR KEYWORDS
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE READ_INPUT (READINP)
!
!   Variable definitions
!      
!   Integer variables
!   i:              index value of keyword
!   i1:             length of keyword with trailing blanks removed
!   jj:             counter for lupkeywords based on ikey value             
!
!   Logical variables
!   lgood:          variable denoting if a filename is okay
!
!   Character variables
!   form1:          format to read filename
!   landflag:       character string denoting grid cell for prognostic data is overland (OL) or
!                   overwater (OW)
!   fileform:       character string of DATA file format to compare against formats array
!   formstr:        format for messages
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: inpline,inpline1,ikey,ilen,keywrd,checkfile,dataline,getdates,getloc,fileind,formind,getunit  
    use file_units, only: flength,os_data_unit,os_inpfile,os_qaout_unit,os_qafile
    implicit none
      
    integer(kind=4):: i,i1,jj
    logical :: lgood
      
    character(len=60) :: formstr(2)
    character(len=6) :: form1,landflag
    !character(len=10) :: fileform
    character(len=10) :: modnam='OS_PATH'
      
!   initialize
    i=0
    i1=0
    jj=0
    lgood=.true.
      
!   message formats
!   1.  invalid or duplicate entry of keyword or invalid overland/water flag
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'

!   2.  invalid number of fields
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'i3,2(1x,a))'  

!   form1 is the format to use to read the message or report file
!   format string is (a300)
    write(form1,'(a2,i3,a1)')'(a',flength,')'

!   get file unit for messages, either msg_unit or standard output
    call getunit
      
    lbad=.false.
    
!   ikey = 0 means text string ONSITE or PROG found on input line
!   set the logical variables for keywords below to false
!   meaning they have not been detected yet
!   except audit
!   initialize onsite variables
    if (ikey == 0) then
        call onsite_init !initialize variables
    else
        i=index(inpline1,trim(adjustl(keywrd(ikey))))
        i1=len_trim(keywrd(ikey))
!       if keyword is DATA, NO_MISSING, FORMAT, QAOUT, XDATES, LOCATION, OBS/HR, RANGE, AUDIT, DELTA_TEMP, OSHEIGHTS, THRESHOLD,
!       or READ, then set jj for loskeywords.
          if ((ikey >=4 .and. ikey <= 6) .or. (ikey >= 8 .and. ikey <= 10) .or. ikey == 27 .or. (ikey >= 12 .and. ikey <= 17)) then
            if (ikey >=4 .and. ikey <=6) then
                jj=ikey-3
            elseif (ikey >=8 .and. ikey <=10) then
                jj=ikey-4
            elseif (ikey==27) then
                jj=ikey-14
            else !ikey ranges from 12 to 17
                jj=ikey-5
            endif
            if (.not. loskeywords(jj)) then
                loskeywords(jj)=.true.
            else
!               DATA, QAOUT, XDATES, LOCATION, THRESHOLD, OBS/HR cannot be duplicated
                if (ikey == 4 .or. (ikey >=8 .and. ikey <= 10) .or. ikey == 16 .or. ikey == 27) then
                    write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,'DUPLICATE ENTRY OF KEYWORD:',&
                        trim(adjustl(keywrd(ikey)))
                    lbad=.true.
                    return
                endif
            endif
        else !invalid keyword for this path 
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E01', modnam,'INVALID KEYWORD',trim(adjustl(keywrd(ikey)))
            lbad=.true.
            return
        endif
              
        if (ikey == 4) then !data file
            osstage1=.true.
!           if not 2 or 3 fields, then there is an error
            if (nfield /= 2 .and. nfield /= 3) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else                      
!               read DATA line to get input file 
!               and positions in input line to read
!               see subroutine dataline for details
                call dataline(i,i1)
                
!               read the original line to get the filename
                read(inpline(fileind(1):fileind(2)),'(a)')os_inpfile
                      
!               read the uppercase version of the line to get the land/water flag
!               if no flag, assume over land
                if (nfield == 3) then
                    read(inpline1(formind(1):formind(2)),'(a)')landflag
                    if (trim(adjustl(landflag)) == 'OW') then
                        overland=.false.
                    elseif (trim(adjustl(landflag)) /= 'OW' .and. trim(adjustl(landflag)) /= 'OL') then
                        lbad=.true.
                        write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,'INVALID OVERLAND/WATER FLAG:',&
                            trim(adjustl(landflag))
                    endif
                 endif   
                
!               get the DATA filename
                call checkfile(os_data_unit,os_inpfile,1,lgood)
            endif
      

        else if (ikey == 5) then !no_missing
            call os_list(i+i1)
              
        else if (ikey == 6) then !FORMAT keyword
            if (nfield /=3) then !account for presence of FORMAT and FORMAT # (1, 2, etc.) and format
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
!               get the format
                call os_form !(i1+i)
            endif             

        else if (ikey == 8) then !qaout
!           if number of fields is not 2, then line is bad
            if (nfield /= 2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                read(inpline(i+i1+1:ilen),form1)os_qafile
!               get the QAOUT filename
                call checkfile(os_qaout_unit,os_qafile,4,lgood)
            endif

        else if (ikey == 9) then !xdates
!           incorrect number of fields, line is bad                  
            if (nfield /= 3 .and. nfield /= 4 .and. nfield /= 7 .and. nfield /= 8) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else  
                call getdates(i+i1,osstart,osend,osdates)
            endif
                 
        else if (ikey == 10) then !location
!           incorrect number of fields, line is bad                  
            if (nfield < 4 .or. nfield > 6) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call getloc(i+i1,osid,oslat,oslon,osgmt2lst,oselev,lgmt2lst,loselev)
            endif

        else if (ikey == 12) then !range
!			incorrect number of fields, line is bad
            if (nfield /= 6) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call os_range(i+i1)
            endif
              
        else if (ikey == 13) then !audit
!           call os_list to get any other variables to audit
            call os_list(i+i1)
            if (index(inpline1,' ALL') > 0)laudit_all=.true.
              
        else if (ikey == 14) then !delta_temp keyword
            if (nfield /= 4) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
!               allocate delta t heights array
                if (.not. allocated(osdt_hts)) then
                    allocate(osdt_hts(maxdtind,2))
!                   initialize to missing values
                    osdt_hts=-9.0
                endif
                call delta_t_ht(i+i1)
            endif
          
        else if (ikey == 15) then !OSHEIGHTS keyword
            if (nfield < 2 .or. nfield > maxlevel+1) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call osheights(i1+i)
            endif

        else if (ikey == 16) then !threshold
!           incorrect number of fields                  
            if (nfield /=2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call os_thresh(i1+i)
            endif
                  
        else if (ikey == 17) then !READ keyword
            if (nfield < 3 .or. nfield > maxvars+2) then !account for presence of READ and READ # (1, 2, etc.)
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call os_vars(i1+i)
            endif
      
        else !ikey = 27 OBS/HOUR keyword
!           incorrect number of fields                  
            if (nfield /=2 .and. nfield /=3) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call os_obsnum(i1+i)
            endif
        endif
    endif

    return
      
    end subroutine os_path
!*********************************************************************************************************

    subroutine os_list(i1)
!=========================================================================================================
!   SUBROUTINE OS_LIST
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE NO_MISSING OR AUDIT KEYWORD FOR ONSITE OR PROG DATA.
!   THIS SUBROUTINE IS ANALOGOUS TO UP_LIST
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PATH)
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!
!   NOTE: nosvars is the number of onsite variables
!
!   Logical variables
!   oslogical:      Array of values for logical check for missing values (true=skip in message file,
!                   false=report, this is default) or audit (true=audit,false=don't audit)
!
!   Character variables
!   osvar:          array of onsite variable names
!   formstr:		formats for messages
!   modnam:         Subroutine name
!=========================================================================================================   
      
    use main1, only : var_list,inpline1,ikey
    implicit none
    integer(kind=4), intent(in):: i1
    integer(kind=4) :: nfield1,i
    character(len=4):: osvar(nosvars)
    character(len=60) :: formstr
    logical :: oslogical(nosvars)
    character(len=10) :: modnam='OS_LIST'
    
!   initialize
    i=0
    oslogical=.false.
    nfield1=nfield-1
     
!   format for warning messages about duplicates
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
    
!   initialize osvar and oslogical to pass to var_list
    l1: do i=1,nosvars
        if (i < height_var) then
            osvar(i)=osvars(i)%varname
        else
!           change the NN to maxlevel for comparison against the variables
!           read from the input runstream file
            write(osvar(i),'(a2,i2)')osvars(i)%varname(1:2),maxlevel
        endif
!       set oslogical to appropriate values based on keyword
        if (ikey == 5) then
            oslogical(i)=osvars(i)%lnomiss
        else
            oslogical(i)=osvars(i)%laudit
        endif
    enddo l1

!   check the string of variables for keyword
    call var_list(i1,nfield1,osvar,oslogical,nosvars)

!   reset the values of the logical variables of whether to track missing values or audit variables
!   issue warning if variable has already been found for the keyword
    l2: do i=1,nosvars
        if (ikey == 5) then
            if (osvars(i)%lnomiss)write(writeunit,formstr)adjustl(pathid(ipath)),'W02',modnam,&
                'DUPLICATING LISTING OF VARIABLE FOR',trim(adjustl(osvars(i)%varname)),'KEYWORD:','NO_MISSING'
            osvars(i)%lnomiss=oslogical(i)
        else
!           issue warning if variable is already being audited
!           check for keyword on the AUDIT line; this is so if TMPD, WSPD, and WDIR
!           are not listed, then a message won't be written since they are always being audited.
            if (osvars(i)%laudit  .and. index(inpline1,osvars(i)%varname) > 0 .and. (i < temp_var .or. i > wind_vars(2))) then
                write(writeunit,formstr)adjustl(pathid(ipath)),'W02',modnam,'DUPLICATING LISTING OF VARIABLE FOR',&
                    trim(adjustl(osvars(i)%varname)),'KEYWORD:','AUDIT'
            else
                if (oslogical(i)) then
!                   if variable is not a variable that is processed, reset the logical variable to false and
!                   don't add to list.
                    if (.not. osvars(i)%luse) oslogical(i)=.false.
                endif      
                osvars(i)%laudit=oslogical(i)
            endif
        endif
    enddo l2
      
    return 
    end subroutine os_list
!*********************************************************************************************************

    subroutine os_range(i1)
!=========================================================================================================
!   SUBROUTINE OS_RANGE
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE RANGE KEYWORD FOR ONSITE OR PROG DATA TO MODIFY THE RANGES OF VALID DATA.
!   THIS SUBROUTINE IS ANALAGOUS TO UPPER_RANGE AND SURF_RANGE
!
!   MODIFIED DECEMBER 2, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PATH, OS_STAGE2)
!     
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!   lowval:         numeric lower bound read from v1
!   hival:          numeric upper bound read from v3
!   missval:        numeric missing value read from v4
!
!   NOTE: nosvars is the number of onsite variables
!
!   Logical variables
!   lfound:         logical variable denoting variable name found in surface list
!   linc:           logical value to include lower and upper bounds (true)
!                   or exclude (false)
!                   value is based on the value of v2
!   lgood:          logical variable denoting if all values are valid (true)
!                   or not (false).  this variable is used to decide whether
!                   to write the new values to the surface air arrays.
!
!   Character variables
!   varnam1:        data for RANGE keyword read from input line
!   formstr:		formats for messages
!   modnam:         Subroutine name
!=========================================================================================================   
    use main1, only : range_mod,getfields  
    implicit none
    integer(kind=4), intent(in):: i1
    integer(kind=4) :: nfield1,i,lowval,hival,missval
    character(len=100),allocatable, dimension(:) :: varnam1  !for getfields
    logical :: lfound,linc,lgood
    character(len=60) :: formstr
    character(len=10) :: modnam='OS_RANGE'  
    
!   initialize
    i=1
    lowval=-9
    hival=-9
    missval=-9
    lfound=.false.
    linc=.false.
    lgood=.true.
    nfield1=nfield-1
      
!   format for warning messages about invalid variable name or duplicates
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
    allocate(varnam1(nfield1))
    varnam1='0000000000000000000000000'
!   read input line to get variable name, the inclusion indicator, lower bound, and upper bound
    call getfields(i1,nfield1,varnam1)
    
!    read varnam1 to see if in the surface variable list
!   if variable is 2 characters, then could be
!   a multi-level data variable
!   if so, add 'NN' to the end of it to match the osvars variable names
    if (len_trim(varnam1(1)) == 2) write(varnam1(1),'(2(2a))')trim(adjustl(varnam1(1))),'NN'
    do while(i <= nosvars .and. .not. lfound)
        if (trim(adjustl(varnam1(1))) == trim(adjustl(osvars(i)%varname))) then
            lfound=.true.
        else
            i=i+1
        endif
    enddo
      
!   if the variable from the input line is in the onsite array
!   then get the new ranges and/or missing data value.
    if (lfound) then
        call range_mod(varnam1(2),varnam1(3),varnam1(4),varnam1(5),linc,lowval,hival,missval,lgood)
        if (lgood) then
!           if variable has already been modified then warn user but still reset values
            if (osvars(i)%lmod) write(writeunit,formstr)adjustl(pathid(ipath)), 'W02',modnam,&
                'DUPLICATING LISTING OF VARIABLE FOR RANGE KEYWORD:',trim(adjustl(osvars(i)%varname))
!           even if variable modified previously, use the latest values
            osvars(i)%lincbound=linc
            osvars(i)%lowbound=lowval
            osvars(i)%upbound=hival
            osvars(i)%missval=missval
            if (.not. osvars(i)%lmod) osvars(i)%lmod=.true.
        endif
    else
        write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID VARIABLE NAME FOR RANGE KEYWORD:',&
            trim(adjustl(varnam1(1)))
        lbad=.true.
    endif
      
    deallocate(varnam1)
      
    return 
    end subroutine os_range
!*********************************************************************************************************      

    subroutine delta_t_ht(i1)
!=========================================================================================================
!   SUBROUTINE DELTA_T_HT
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE DELTA_TEMP KEYWORD FOR ONSITE OR PROG DATA TO SPECIFY LEVELS FOR DELTA TEMPERATURE CALCULATIONS.
!
!   MODIFIED DECEMBER 2, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PATH, OS_STAGE2)
!     
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!   ht_index:       height index read from input line
!   iflag:          3-element array of IO flags
!
!   Real variables
!   ht:             2-element array of input heights
!
!   Character variables
!   datafield:      data for DELTA_TEMP keyword read from input line
!   formstr:        formats for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: getfields
    implicit none
    integer(kind=4), intent(in):: i1
    integer(kind=4) :: i,nfield1,ht_index,iflag(3)
    real(kind=r8) :: ht(2)
    character(len=60) :: formstr(5)
    character(len=100), allocatable, dimension(:) :: datafield
    character(len=10) :: modnam='DELTA_T_HT'
    
!   initialize
    i=0
    ht_index=0
    iflag=0
    ht=-9.0_r8
    nfield1=nfield-1
   
!   formats for messages
!   1. invalid height index or height
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'

    
!   2.  height index not used, duplicate listing, height 1 > height 2
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),a)'
    
    allocate(datafield(nfield1))
      
    datafield='0000000000000000000000000'

!   read input line to get variable name, the height index, lower height, and upper height
    call getfields(i1,nfield1,datafield)      
      
!   get the height index, can only be between 1 and 3
!   read heights
    read(datafield(1),*,iostat=iflag(1)) ht_index
    read(datafield(2),*,iostat=iflag(2)) ht(1)
    read(datafield(3),*,iostat=iflag(3)) ht(2)
      
!   check the index
    if (iflag(1) == 0) then
        if (ht_index < 1 .or. ht_index > maxdtind) then
!           invalid value
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'INVALID HEIGHT INDEX FOR DELTA_TEMP:',&
                trim(adjustl(datafield(1)))
            lbad=.true.
        elseif (ht_index > 1) then
!           index of 2 or 3 not used
            write(writeunit,formstr(2))adjustl(pathid(ipath)),'W64',modnam,'HEIGHT INDEX',trim(adjustl(datafield(1))),'NOT USED'
!            delta_t_ind=ht_index !commented out but keep for possible future use
        else
!           check to see if this index already filled in
!           if so, issue warning and use entered values, if valid
            delta_t_ind=ht_index
            if (osdt_hts(ht_index,1) /= -9.0 .and. osdt_hts(ht_index,2) /= -9.0) write(writeunit,formstr(2))&
                adjustl(pathid(ipath)),'W02',modnam,'DUPLICATING LISTING OF HEIGHT INDEX',trim(adjustl(datafield(1))),&
                'FOR DELTA_TEMP KEYWORD'
        endif
    else
!       invalid string for number        
        write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'INVALID HEIGHT INDEX FOR DELTA_TEMP:'&
            ,trim(adjustl(datafield(1))) 
        lbad=.true.
    endif

!   check the heights
    l1: do i=1,2
        if (iflag(i+1) /= 0 .or. ht(i) < 0.0) then 
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'INVALID HEIGHT LISTED FOR DELTA_TEMP:',&
                trim(adjustl(datafield(i+1)))
            lbad=.true.
        endif
    enddo l1
          
!   if all 3 values of iflag are good, make sure the first
!   height is below the second height, and if so, assign values to
!   array
!   include the fact that lbad must be false because iflag(1) can be good
!   but the number was not valid, i.e. negative or greater than 3.
    if (iflag(1) == 0 .and. iflag(2) == 0 .and. iflag(3) == 0 .and. .not. lbad) then
        if (ht(1) < ht(2)) then
    l2:     do i=1,2
                osdt_hts(ht_index,i)=ht(i)
            enddo l2
        else
            write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'HEIGHT 1 ',trim(adjustl(datafield(2))),'>= HEIGHT 2 ',&
                trim(adjustl(datafield(3)))
            lbad=.true.
        endif
    endif
              
    deallocate(datafield)
     
    return
    end subroutine delta_t_ht
!*********************************************************************************************************

    subroutine os_thresh(i1)
!=========================================================================================================
!   SUBROUTINE OS_THRESH
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE THREHSOLD KEYWORD FOR ONSITE OR PROG DATA TO SPECIFY MINIMUM WIND SPEED.
!
!   MODIFIED DECEMBER 2, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PATH, OS_STAGE2)
!     
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   Variable definitions
!      
!   Integer variables
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!   iflag:          IO flag
!
!   Character variables
!   datafield:      data for OS_THRESH keyword read from input line
!   formstr:            formats for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: ikey,keywrd,getfields,lpath,eps
    implicit none
    integer(kind=4), intent(in):: i1
    integer(kind=4) :: nfield1,iflag
    character(len=60) :: formstr(2)
    character(len=100),allocatable, dimension(:) :: datafield
    character(len=10) :: modnam='OS_THRESH'
    
!   initialize
    nfield1=0
    iflag=0
    nfield1=nfield-1
      
!   formats for messages
!   1.  invalid value for keyword
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'2(a),1x,a)'
    
!   2.  speed exceeds threshold
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),f5.3,1x,a,1x,f3.1,1x,a)'

    allocate(datafield(nfield1))
      
    datafield='0000000000000000000000000'
    
!   read input line to get threshold
    call getfields(i1,nfield1,datafield)  

    read(datafield(1),*,iostat=iflag)threshspeed

    if (iflag == 0) then
        if (threshspeed < minthresh) then !wind speed < minimum speed
            write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(keywrd(ikey))),'SPEED',threshspeed,&
                ' M/S IS BELOW',minthresh,'M/S'
            lbad=.true.
        elseif (threshspeed > maxthresh) then !wind speed > maxspeed
            write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(keywrd(ikey))),'SPEED',threshspeed,&
                ' M/S EXCEEDS',maxthresh,'M/S'
            lbad=.true.
        elseif (dabs(threshspeed-progthresh) > eps .and. lpath(5)) then 
!               threshold is not equal to recommended prognostic threshold
!               see MMIF guidance for AERMOD for latest recommended value.
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'W65',modnam,&
                trim(adjustl(keywrd(ikey))),'SPEED',threshspeed,' M/S IS NOT EQUAL TO PROG THRESHOLD',progthresh,'M/S'
        else
            if (threshspeed > warnthresh) write(writeunit,formstr(2))adjustl(pathid(ipath)),'W65',modnam,&
                trim(adjustl(keywrd(ikey))),'SPEED',threshspeed,' M/S EXCEEDS',warnthresh,'M/S'
        endif
    else !invalid entry
        write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR KEYWORD',trim(adjustl(keywrd(ikey))),&
            ':',trim(adjustl(datafield(1)))
        lbad=.true.
    endif
      
    deallocate(datafield)
    
    return
    end subroutine os_thresh
!*********************************************************************************************************

    subroutine osheights(i1)
!=========================================================================================================
!   SUBROUTINE OSHEIGHTS
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE OSHEIGHTS KEYWORD FOR ONSITE OR PROG DATA TO SPECIFY LEVELS FOR DATA.
!
!   MODIFIED DECEMBER 2, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PATH, OS_TEST)
!     
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!   iflag:          IO flag
!
!   Real variables
!   ht:             input height read from datafield
!
!   Character variables
!   datafield:      data for OSHEIGHTS keyword read from input line
!   formstr:		formats for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: getfields
    implicit none
    integer(kind=4), intent(in):: i1
    integer(kind=4) :: i,nfield1,iflag
    real(kind=r8) :: ht
    character(len=60) :: formstr
    character(len=100), allocatable, dimension(:) :: datafield(:)
    character(len=10) :: modnam='OSHEIGHTS'  

!   format for message
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   initialize
    i=0
    nfield1=0
    iflag=0
    ht=0.0_r8
    nfield1=nfield-1
      
    allocate(datafield(nfield1))
      
    datafield='0000000000000000000000000'
     
!   read input line to heights
    call getfields(i1,nfield1,datafield)  
      
!   allocate osheights1 array if not allocated
    if (.not. allocated(osheights1)) then
        allocate(osheights1(maxlevel))
        osheights1=0.0
    endif
      
    l1: do i=1,nfield1
        read(datafield(i),*,iostat=iflag)ht
        if (iflag == 0 .and. ht > 0.0) then
            noshts=noshts+1
            osheights1(noshts)=ht
        else
            write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID HEIGHT LISTED FOR OSHEIGHTS:',&
                trim(adjustl(datafield(i)))
            lbad=.true.
        endif
    enddo l1
      
    deallocate(datafield)
      
    return
    end subroutine osheights
!*********************************************************************************************************
      
    subroutine os_vars(i1)
!=========================================================================================================
!   SUBROUTINE OS_VARS
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE READ KEYWORD FOR ONSITE OR PROG DATA TO SPECIFY LEVELS AND VARIABLES TO READ FROM DATA.
!
!   MODIFIED DECEMBER 2, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PATH)
!     
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!   iflag:          IO flag
!   ifield:         loop counter
!   ilev:           integer level number; used to determine if a possible multi-level variable
!   varlen:         length of character string var1 (see below)
!   ir:             index of READ in READ statement
!   il:             letter loop counter
!   add1:           number of elements to skip in varmap if reading multiple lines per READ
!
!   Real variables
!   lfound:         logical variable denoting var1 = var2
!   lletter:        letter found on inpline1
!
!   Character variables
!   formstr:		formats for messages
!   datafield:      data for READ keyword read from input line
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: inpline1,getfields
    implicit none
    integer(kind=4), intent(in):: i1
    integer(kind=4) :: i,nfield1,iread,iflag,ifield,ilev,varlen,ir,il,add1
    logical :: lfound,lletter
    character(len=100), allocatable, dimension(:) :: datafield
    character(len=60) :: formstr(5)
    character(len=10) :: modnam='OS_VARS'
    
!   initialize
    i=0
    nfield1=nfield-1
    iread=0
    iflag=0
    ifield=0
    ilev=0
    varlen=0
    lfound=.false.
    add1=0
    
!   formats for messages
!   1.  read index exceed max number of read lines
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),i2)'

!   2.  read index < 0 or invalid variable name or variable not used by AERMET
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),a)'

!   3.  read index equals current number of read indices
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i2,1x,a,1x,i2)'
    
!   4.  read index exceeds current # of read indices +1
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,i2,1x,a,1x,i2,a)'
    
!   5.  invalid value of read index
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
    allocate(datafield(nfield1))

    datafield='0000000000000000000000000'
    
!   read input line to get variables to read
    call getfields(i1,nfield1,datafield) 
      
!   allocate varmap1
    if (.not. allocated(varmap1)) then
        allocate(varmap1(maxread,nosvars,2))
        varmap1=0
    endif
      
!   allocate os_reads
    if (.not. allocated(os_reads)) then
        allocate(os_reads(maxread))
        os_reads='*'
    endif
      
!   get the read number (1, 2, or 3)
    read(datafield(1),*,iostat=iflag)iread
    if (iflag == 0) then
        if (iread > maxread) then
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'READ INDEX',trim(adjustl(datafield(1))),&
                'EXCEEDS MAXIMUMN NUMBER OF READ LINES',maxread
            lbad=.true.
        elseif (iread <= 0) then
            write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'READ INDEX',trim(adjustl(datafield(1))),&
                'IS LESS THAN ZERO'
            lbad=.true.
        elseif (iread == nread) then
!           may be a continuation of previous read
            ir=index(inpline1,'READ')
!           find first letter after read because the number after READ is the read number
            il=ir+4
            lletter=.false.
            do while (il <= len_trim(inpline1) .and. .not. lletter)
                if (ichar(inpline1(il:il)) >=65 .and. ichar(inpline1(il:il)) <=90) then
                    lletter=.true.
                else
                    il=il+1
                endif
            enddo
!           if a continuation, find how many elements of varmap1 have been filled in, find first 0
            lfound=.false.
            add1=1
            do while (add1 <= nosvars .and. .not. lfound)
                if (varmap1(iread,add1,1)==0) then
                    lfound=.true.
                else
                    add1=add1+1
                endif
            enddo
!           take 1 away to go back to last entry
            add1=add1-1
    
!           concatenate current os_reads value with input line, minus READ and read number
            write(os_reads(iread),'(a,1x,a)')trim(adjustl(os_reads(iread))),trim(adjustl(inpline1(il:len_trim(inpline1))))
        elseif (iread > nread+1) then
            write(writeunit,formstr(4))adjustl(pathid(ipath)),'E05',modnam,'READ INDEX',iread,&
                'EXCEEDS CURRENT NUMBER OF READ INDICES +1',nread,'; CHECK READ ORDER'
            lbad=.true.
        else
            nread=iread
            add1=0
            os_reads(iread)=inpline1
        endif
    else
        write(writeunit,formstr(5))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(datafield(1))),&
            'IS AN INVALID VALUE FOR READ INDEX'
        lbad=.true.
    endif
      
    if (.not. lbad) then
!       now check the rest of the data fields to match to variable names
!       initialize ilev
        ilev=0
    l2: do ifield=2,nfield1
            i=1
            lfound=.false.
            ilev=0
            varlen=len_trim(datafield(ifield))
            if (varlen == 4) then
                do while (i <= nosvars .and. .not. lfound)
                    iflag=0
                    if (trim(adjustl(datafield(ifield))) == trim(adjustl(osvars(i)%varname))) then
                        lfound=.true.
                    else
!                       see if the first 2 characters match, could be multi-level data variable, i.e. HT01, HT02, TT01, etc.
                        if (i >= height_var .and. datafield(ifield)(1:2) == osvars(i)%varname(1:2)) then
                            read(datafield(ifield)(3:4),*,iostat=iflag)ilev
                            if (iflag == 0 .and. ilev <= maxlevel) then !potential valid number
                                lfound=.true.
                            else
                                i=i+1
                            endif
                        else
                            i=i+1
                        endif
                    endif
                enddo
            endif
            if (lfound) then
!               warn user if variable is not one used by AERMET
!               it will still go into the varmap1 and varmap arrays
!               since it will still be read in from the data file.
                if (.not. osvars(i)%luse)write(writeunit,formstr(2))adjustl(pathid(ipath)),'W64',modnam,'VARIABLE',&
                    trim(adjustl(osvars(i)%varname)),'IS NOT USED BY AERMET AND WILL BE READ BUT NOT BE PROCESSED'
               
!               set lread for variable to true
                osvars(i)%lread=.true.
!               map the variable to varmap1
!               for 3rd dimension, index 1 is the variable number in osvars
!               index 2 is ilev, which is 0 or a number >= 1
                varmap1(iread,ifield-1+add1,1)=i !variable #
                varmap1(iread,ifield-1+add1,2)=ilev !level
!               if a multi-level variable, reset the variable nlevels in osvars for the particular variable
!               this will control processing later in stage 1 and 2
                if (ilev > 0) then
                    if (ilev > osvars(i)%nlevels) osvars(i)%nlevels=ilev
                    if (osvars(i)%firstlev == 0) osvars(i)%firstlev=ilev !set first level being read for variable
                endif
            else
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'INVALID VARIABLE NAME',&
                    trim(adjustl(datafield(ifield))),'FOR READ STATEMENT'
                lbad=.true.
            endif
        enddo l2
    endif
    deallocate(datafield)
    
    return
    end subroutine os_vars
!*********************************************************************************************************
      
    subroutine os_form
!=========================================================================================================
!   SUBROUTINE OS_FORMAT
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE FORMAT KEYWORD FOR ONSITE OR PROG DATA TO SPECIFY FORMAT FOR THE READ STATEMENTS.
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   MODIFIED DECEMBER 2, 2021
!   CALLED BY:      MODULE ONSITE (OS_PATH, OS_TEST)
!     
!   Variable definitions
!      
!   Integer variables
!   iread:			FORMAT number
!   iflag:          IO flag
!   iform:			index value of iread used to read FORMAT from inpline1
!
!   Character variables
!   formstr:		formats for messages
!   keywrd1:        FORMAT keyword read from input line
!   aread:          2-digit character string of iread
!   modnam:		    Subroutine name
!========================================================================================================= 
    use main1, only: getfields,inpline1,ilen
    implicit none
    integer(kind=4) :: iread,iflag,iform
    character(len=12) :: keywrd1
    character(len=2) :: aread
    character(len=60) :: formstr(5)
    character(len=10) :: modnam='OS_FORM'
    
!   initialize
    iread=0
    iflag=0

!   formats for messages
!   1.  format index exceeds max number of format lines
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,i4,1x,a,1x,i2)'  

!   2.  format index < 0
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,i4,1x,a)'
    
!   3.  format index = current number of format indices
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i2,1x,a,1x,i2)'
   
!   4.  read index exceeds current # of read indices +1
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,i2,1x,a,1x,i2,a)'
    
!   5.  invalid value for format index
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   allocate os_format1
    if (.not. allocated(os_format1)) then
        allocate(os_format1(maxread))
        os_format1='*'
    endif

!   read the keyword and iread
    read(inpline1,*,iostat=iflag)keywrd1,iread
!   check iread 
    if (iflag == 0) then
        if (iread > maxread) then
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'FORMAT INDEX',iread,&
                'EXCEEDS MAXIMUMN NUMBER OF FORMAT LINES',maxread
            lbad=.true.
        elseif (iread <= 0) then
            write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'FORMAT INDEX',iread,'IS LESS THAN ZERO'
            lbad=.true.
        elseif (iread == nformat) then
            write(writeunit,formstr(3))adjustl(pathid(ipath)),'E05',modnam,'FORMAT INDEX',iread,&
                'EQUALS CURRENT NUMBER OF FORMAT INDICES',nformat
            lbad=.true.
        elseif (iread > nformat+1) then !possible skip of a format index
            write(writeunit,formstr(4))adjustl(pathid(ipath)),'E05',modnam,'FORMAT INDEX',iread,&
                'IS GREATER THAN NUMBER OF FORMAT INDICES + 1',nformat,'; CHECK FORMAT ORDER'
            lbad=.true.
        else
            nformat=iread
        endif
    else
        write(writeunit,formstr(5))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(inpline1)),&
            'IS AN INVALID VALUE FOR FORMAT INDEX'
        lbad=.true.
    endif
!   if iread okay, now get the format, the format will be checked for syntax in osformread
    if (.not. lbad) then
!       iread is 1 or 2 digits
        write(aread,'(i2)')iread
        iform=index(inpline1,aread)
!       get format statement, account for blank or comma delimiter between iread and format statement
        if (iread < 10) then
            os_format1(iread)=inpline1(iform+2:ilen)
        else
            os_format1(iread)=inpline1(iform+3:ilen)
        endif
    endif

    return
    end   subroutine os_form
!*********************************************************************************************************

    subroutine os_obsnum(i1)
!=========================================================================================================
!   SUBROUTINE OS_OBSNUM
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE OBS/HOUR KEYWORD FOR ONSITE OR PROG DATA TO SPECIFY NUMBER OF OBSERVATIONS PER HOUR.
!   AND OPTIONAL VECTOR AVERAGING KEYWORD FOR WINDS.
!   MIN_OBS IS ALSO CALCULATED.
!
!   MODIFIED DECEMBER 2, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PATH)
!     
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   Variable definitions
!      
!   Integer variables
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!   iflag:          IO flag
!
!   Character variables
!   formstr:        format for messages
!   datafield:      data for OBS/HOUR keyword read from input line
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: ikey,keywrd,getfields
    implicit none
    integer(kind=4), intent(in):: i1
    integer(kind=4) :: nfield1,iflag
    character(len=100),allocatable, dimension(:) :: datafield
    character(len=60) :: formstr(3)
    character(len=10) :: modnam='OS_OBSNUM'
    
!   initialize
    nfield1=nfield-1
    iflag=0      
      
!   1.  less (more) than minimum (maximum) # obs allowed
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,1x,a,1x,i2)'

!   2.  invalid value for keyword
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,2(a),1x,a)'
    
!   3.   invalid value for wind averaging method
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
    
    allocate(datafield(nfield1))
      
    datafield='0000000000000000000000000'
      
!   read input line to get # of observations
    call getfields(i1,nfield1,datafield)  

    read(datafield(1),*,iostat=iflag)nobs_hr

    if (iflag == 0) then
        if (nobs_hr < min_obs_hr) then !less than mininum of obs allowed
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(keywrd(ikey))),nobs_hr,'IS LESS THAN',&
                min_obs_hr
            lbad=.true.
        elseif (nobs_hr > max_obs_hr) then !more than maximum number of obs allowed
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(keywrd(ikey))),nobs_hr,'EXCEEDS',&
                max_obs_hr
            lbad=.true.
        else !okay, calculate min_obs
            if (mod(nobs_hr,2) == 0) then !even
                min_obs=nobs_hr/2
            else !odd
                min_obs=(nobs_hr+1)/2
            endif
        endif
    else !invalid entry
        write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR KEYWORD',trim(adjustl(keywrd(ikey))),&
            ':',trim(adjustl(datafield(1)))
        lbad=.true.
    endif
      
!   check to see if optional VECTOR or SCALAR keywords listed
!   if so, then set lvector accordingly. if keyword not listed
!   then set lvector to false, meaning to do scalar average.
!   if something other than VECTOR or SCALAR are listed, issue
!   error
    if (nfield1 == 2) then
        if (trim(adjustl(datafield(2))) == 'VECTOR') then
            lvector=.true.
        elseif (trim(adjustl(datafield(2))) == 'SCALAR') then
            lvector=.false.
        else
            lvector=.false.
            lbad=.true.
            write(writeunit,formstr(3))adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR WIND AVERAGING METHOD:',&
                trim(adjustl(datafield(2))),'FOR KEYWORD',trim(adjustl(keywrd(ikey)))
        endif
    else
        lvector=.false.
    endif
      
    deallocate(datafield)
      
    return
    end subroutine os_obsnum
!*********************************************************************************************************

    subroutine os_test
!=========================================================================================================
!   SUBROUTINE OS_TEST
!   THIS SUBROUTINE CHECKS THAT MANDATORY KEYWORDS HAVE BEEN INCLUDED
!   THIS IS NOT A CHECK ON THE SYNTAX OR IF INCLUDED FILENAMES EXIST
!   THAT HAS BEEN DONE EARLIER IN OS_PATH
!
!   MODIFIED MAY 5, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE READ_INPUT (READINP)
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   j:              loop counter
!   k:              loop counter
!   ivar:           variable counter
!
!   Logical variables
!   lbad2:          denotes if READ and FORMAT statements are consistent
!
!   Character variables
!   formstr:        format for messages 
!   modnam:         Subroutine name
!=========================================================================================================   
    use main1, only: keywrd,lstage,lpath
    implicit none
    integer(kind=4) :: i,j,k,ivar
    logical :: lbad2
    character(len=60) :: formstr(10)
    character(len=10) :: modnam='OS_TEST'
      
!   initialize
    i=0
    j=0
    k=0
    lbad2=.false.
    
!   1.  missing mandatory keyword
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2.  XDATES missing; OSMN present with OBS/HR =1; 
!       wind speed and direction not processed together; 
!       invalid overwater flag for onsite data
!       OSHEIGHTS and HTNN specified
!       no delta_temp heights listed for DT01
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a)'
    
!   3.  number of READ statements not equal to number of FORMAT statements
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i2,1x,a,1x,i2)'

!   4.  OBS/HOUR keyword with/without OSMN or OBS/HOUR keyword not present with OSMN
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,a,1x,a,1x,a)'

!   5.  DELTA_TEMP heights
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,2(1x,f5.1,1x,a))'
    
!   6.  temperatures will be used for  DELTA_TEMP heights
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'a,1x,2(1x,f5.1,1x,a),1x,a)'   

!   7.  onsite variable audited but not read in
    write(formstr(7),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),a)'
    
!   8.  number of OSHEIGHTS not equal to number of HTNN levels
    write(formstr(8),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,1x,a,1x,i3)'
    
!   9.  first level of wind direction not same as first level for wind speed
!       or number of levels for wind direction not equal to number of levels for wind speed
    write(formstr(9),'(2(a))')trim(adjustl(msg_form)),'a,1x,i2,1x,a,1x,i2)'
    
!   10. variable will be read in but not used in stage 2
    write(formstr(10),'(2(a))')trim(adjustl(msg_form)),'a,1x,a4,1x,a)'
    
!   note that when OS_TEST is called, msg_unit has been set
!   no longer need to write messages to the value from getunit
!   write to msg_unit
      
!   mandatory DATA keyword if stage 1 only
!   not needed if running stage 2 without stage 1
    if (osstage1 .and. .not. loskeywords(1)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(4)))
        lbad=.true.
    endif
          
!   do some checks on FORMAT and READ
!   first check to see if either is missing
          
!   check FORMAT if stage 1 only
    if (osstage1 .and. .not. loskeywords(3)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(6)))
        lbad=.true.
    endif
          
!   check READ if stage 1 only
    if (osstage1 .and. .not. loskeywords(12)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(17)))
        lbad=.true.
    endif
      
!   if both FORMAT and READ present, make sure both have the same # of occurrences
!   and format matches the read statements
!   only check if stage 1
    if (osstage1 .and. loskeywords(3) .and. loskeywords(12)) then 
        if (nread /= nformat) then
            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'E50',modnam,'NUMBER OF VALID READ STATEMENTS',nread,&
                'IS NOT EQUAL TO NUMBER OF VALID FORMAT STATEMENTS',nformat
            lbad=.true.
        else
!           check the read and format statements for consistency
            call osformread(lbad2)
            if (lbad2) then
                lbad=.true.
            else
!               allocate varmap and assign values from varmap1
!               allocate os_format and assign values from os_format1
!               nread should be equal to nformat
                allocate(varmap(nread,nreadvars,2))
                allocate(os_format(nformat))
                varmap=0
    l1:         do i=1,nread
                    os_format(i)=os_format1(i)
    l2:             do j=1,nreadvars
    l3:                 do k=1,2
                            varmap(i,j,k)=varmap1(i,j,k)
                        enddo l3
                    enddo l2
                enddo l1
                deallocate(varmap1)
                deallocate(os_format1)
            endif
        endif
    endif
     

!   check QAOUT if stage 2 only
!   if stage 1 or stage 1 and 2, then it's optional
    if (.not. osstage1 .and. lstage(2) .and. .not. loskeywords(4)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(7)))
        lbad=.true.
    endif
      
!   check XDATES
!   must be there if stage 1 and not stage 2
!   if stage 1 and 2 and no XDATES, then issue warning that
!   xdates will come from METPREP XDATES
    if (osstage1) then
        if (.not. loskeywords(5)) then
            if (lstage(2)) then !issue warning that METPREP XDATES will be used
                write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W03',modnam,&
                    'XDATES KEYWORD MISSING; READ XDATES FROM METPREP XDATES'
            else !error
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(9)))
                lbad=.true.
            endif
        endif
    endif
      
!   check LOCATION if stage 1 and not stage 2
!   not needed for stage 2
    if (osstage1 .and. .not. lstage(2) .and. .not. loskeywords(6)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(10)))
        lbad=.true.
    endif

!   check THRESHOLD if stage 1 and not stage 2
!   not needed for stage 2
!   threshold is only required if wind speed being read in.

    if (osstage1 .and. osvars(wind_vars(2))%lread .and. .not. loskeywords(11)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(16)))
        lbad=.true.
    endif
     
!   if obs/hr keyword found, check to see if OSMN was included in the READ statement
!   if OSMN not found and obs/hr keyword found and > 1, issue error
!   if OSMN found and obs/hr = 1 then issue warning that OSMN will be ignored
!   issue error if OSMN found and obs/hr is not found
    if (osstage1) then
        if (loskeywords(13) .and. nobs_hr == 1 .and. osvars(5)%lread) then
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W66',modnam,'OBS/HOUR = 1 WITH OSMN; IGNORE OSMN'
        elseif (loskeywords(13) .and. nobs_hr > min_obs_hr .and. nobs_hr <= max_obs_hr .and. .not. osvars(5)%lread) then
            write(msg_unit,formstr(4))adjustl(pathid(ipath)),'E51',modnam,'OBS/HOUR KEYWORD','PRESENT','WITHOUT','OSMN'
            lbad=.true.
        elseif (.not. loskeywords(13) .and. osvars(5)%lread) then
            write(msg_unit,formstr(4))adjustl(pathid(ipath)),'E51',modnam,'OBS/HOUR KEYWORD','NOT PRESENT','WITH','OSMN'
            lbad=.true.
        endif
    endif
   
!   check overland overwater flag and ONSITE or PROG
!   if ONSITE and overwater, issue error
    if (.not. overland .and. lpath(4)) then
        lbad=.true.
         write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,'OVERWATER FLAG INVALID FOR ONSITE DATA'
    endif

!   if overland and certain variables read in, alert
!   user that those variables will not be used in Stage 2 processing
    if (overland .and. osstage1) then
    l4: do ivar=1,nosvars
            if ((ivar == hflux_var .or. ivar == lflux_var .or. ivar == zo_var .or. ivar == vptg_var .or. ivar == l_var .or. &
                ivar == wstar_var .or. ivar == alb_var .or. ivar == bo_var) .and. osvars(ivar)%lread) &
                write(msg_unit,formstr(10))adjustl(pathid(ipath)),'I67',modnam,'LOCATION IS OVERLAND; VARIABLE',&
                    trim(adjustl(osvars(ivar)%varname)),'READ IN STAGE 1 BUT WILL NOT BE USED IN STAGE 2'
            
!           post 21DRF, ignore u* and cloud cover if overland and PROGnostic; allow for use with ONSITE
            if ((ivar == ustar_var .or. ivar == cloud_var) .and. osvars(ivar)%lread .and. lpath(5)) &
                write(msg_unit,formstr(10))adjustl(pathid(ipath)),'I67',modnam,'LOCATION IS OVERLAND; PROGNOSTIC VARIABLE',&
                    trim(adjustl(osvars(ivar)%varname)),'READ IN STAGE 1 BUT WILL NOT BE USED IN STAGE 2'
        enddo l4
    endif
    
!   if OSHEIGHTS included, write osheights1 to os_heights  
!   also, check to see if OSHEIGHTS included with HT variables
!   if so, then issue warning HT variables ignored and OSHEIGHTS used

    if (osstage1 .and. loskeywords(10)) then
        allocate(os_heights(noshts))
    l5: do i=1,noshts
            os_heights(i)=osheights1(i)
        enddo l5
        deallocate(osheights1)
      
        if (osvars(height_var)%lread) then
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W67',modnam,&
                'OSHEIGHTS AND HTNN BOTH SPECIFIED. OSHEIGHTS KEYWORD WILL BE USED AND HTnn VARIABLES IGNORED'
            
!           if number of OSHEIGHTS is not equal to heights from data, then issue error
            if (osvars(height_var)%nlevels /= noshts) then
                write(msg_unit,formstr(8))adjustl(pathid(ipath)),'E57',modnam,'NUMBER OF OSHEIGHTS',noshts,&
                    'NOT EQUAL TO NUMBER OF HTNN LEVELS',osvars(height_var)%nlevels
                lbad=.true.
            endif
        endif
        luseoshts=.true.
    endif
          
!   check DELTA_TEMP and DT01
!   if DELTA_TEMP being used and DT01 being read in, issue informational message
!   if DELTA_TEMP being used and DT01 not being read in, do the following:
!       1) if temperatures available for both heights, issue warning and calculate
!           DT01 from temperatures
!       2) if temperatures not available, issue error and stop
!   if not using DELTA_TEMP but DT01 read in, issue error and stop
    if (osstage1) then
        if (loskeywords(9)) then !have DELTA_TEMP
            if (osvars(dt01_var)%lread) then
!               only use height index 1, since only to use
                write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I61',modnam,'DELTA_TEMP HEIGHTS:',osdt_hts(1,1),'M',&
                    osdt_hts(1,2),'M'
            else
!               issue warning that temperatures may be used
                write(msg_unit,formstr(6))adjustl(pathid(ipath)),'W69',modnam,'TEMPERATURES FOR DELTA TEMP HEIGHTS:',&
                    osdt_hts(1,1),'M',osdt_hts(1,2),'M','WILL BE USED IF AVAILABLE'
            endif
        else
            if (osvars(dt01_var)%lread) then
!               issue error
                 write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E54',modnam,'NO DELTA_TEMP HEIGHTS LISTED FOR DT01'
                lbad=.true.
            endif
        endif
    endif
          
!   check that variables to be audited were also read in
!   if audited variable not read in, issue warning and 
!   reset audit variable for the variable to false.
!   only issue warning with ALL is not specified on AUDIT keyword   
!   also add to nos_audit_vars if temperature and winds are read in
    if (osstage1) then
    l6: do i=6,nosvars
        if (osvars(i)%laudit .and. osvars(i)%luse .and. .not. osvars(i)%lread) then
            if (.not. laudit_all) write(msg_unit,formstr(7))adjustl(pathid(ipath)),'W68',modnam,'ONSITE VARIABLE',&
                trim(adjustl(osvars(i)%varname)),'AUDITED BUT NOT READ IN, RESET TO NOT AUDIT'
            osvars(i)%laudit=.false.
        endif
        enddo l6
    endif

!   have to have both direction and speed if one is read, if not issue error
    ! code modified by GMM
	! original line:-
	! if (osvars(wind_vars(1))%lread /= osvars(wind_vars(2))%lread) then
    if (osvars(wind_vars(1))%lread .neqv. osvars(wind_vars(2))%lread) then
        write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E60',modnam,'WIND SPEED AND DIRECTION ARE NOT PROCESSED TOGETHER'
        lbad=.true.
    endif
    
!   check to see if wind speed and direction have matching levels (if both read in)
!   if don't match then issue warning
    if (osvars(wind_vars(1))%lread .and. osvars(wind_vars(2))%lread .and. (osvars(wind_vars(1))%firstlev /=  &
    osvars(wind_vars(2))%firstlev .or. osvars(wind_vars(1))%nlevels/= osvars(wind_vars(2))%nlevels)) then
        if (osvars(wind_vars(1))%firstlev /= osvars(wind_vars(2))%firstlev) write(msg_unit,formstr(9))adjustl(pathid(ipath)),&
        'W61',modnam,'FIRST LEVEL FOR WD',osvars(wind_vars(1))%firstlev,'NOT EQUAL TO FIRST LEVEL FOR WS',&
            osvars(wind_vars(2))%firstlev
        
        if (osvars(wind_vars(1))%nlevels /= osvars(wind_vars(2))%nlevels) write(msg_unit,formstr(9))adjustl(pathid(ipath)),'W61',&
        modnam,'NUMBER OF LEVELS FOR WD',osvars(wind_vars(1))%nlevels,'NOT EQUAL TO NUMBER OF LEVELS FOR WS',&
            osvars(wind_vars(2))%nlevels
    endif
    
    return
    end subroutine os_test
!*********************************************************************************************************

    subroutine osformread(lbad2)
!=========================================================================================================
!   SUBROUTINE OSFORMREAD
!   THIS SUBROUTINE CHECKS THAT THE FIRST READ STATMENT HAS THE DATE/TIME AS THE FIRST SET OF VARIABLES
!   AND THE FORMAT STATEMENTS DON'T HAVE SYNTAX ERRORS
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_TEST)
!
!   OUTPUT VARIABLES:
!
!   LBAD2:          DENOTES BAD READ
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   j:              loop counter
!   timevars:       number of date/time variables in read statement
!                   if OSMN is present then value is 5, year, month, day, hour, minute
!                   otherwise value is 4; year, month, day, hour
!   nparenth:       2-element array of number of left parenthesis (element 1)
!                   and right parenthesis (element 2)
!
!   Logical variables
!   lbad2:          denotes if problem with READ and/or FORMAT statements 
!   lend:           denotes that last variable read from varmap1 for particular read
!
!   Character variables
!   formstr:        format for messages    
!   modnam:         Subroutine name
!=========================================================================================================   
    implicit none
    integer(kind=4) :: i,j,timevars,nparenth(2) 
    logical, intent(out) :: lbad2
    logical :: lend
    character(len=60) :: formstr(4)
    character(len=10) :: modnam='OSFORMREAD'
      
!   initialize
    i=0
    j=0
    timevars=0
    nparenth=0
    lbad2=.false.
    lend=.false.
  
!   formats for messages
!   1.  first read statement does not include date/time 
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'3(a))'
    
!   2.  READ statement includes date/time but is not first set
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i2,1x,3(a))'
    
!   3.  number of left parentheses does not match number of right parentheses
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'2(a,1x,i2,1x),a)'

!   4.  first and/or last characters not parentheses
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'5(a,1x),i2,1x,a)'
    
    l1: do i=1,nread
        if (i==1) then 
!           check to make sure first statement has date/time variables first         
!           if minute variable not included first four variables must be
!           date/time
!           if minute variable included then first five variables must be date/time
            if (osvars(5)%lread) then
                timevars=5
            else
                timevars=4
            endif
    l2:     do j=1,timevars
                if (varmap1(i,j,1) > 5)lbad2=.true.
            enddo l2
            if (lbad2)write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E52',modnam,'FIRST READ STATEMENT "',&
                trim(adjustl(os_reads(i))),'" DOES NOT INCLUDE DATE/TIME VARIABLES AS FIRST SET OF VARIABLES' 
        else
!           check to see if any of the date/time variables are listed on the other read lines
!           if so, check to see if they are the first variables, if not, then issue error
            
            if (index(os_reads(i),'OSYR') > 0 .or. index(os_reads(i),'OSMO') > 0 .or. index(os_reads(i),'OSDY') > 0 &
            .or. index(os_reads(i),'OSHR') > 0 .or. index(os_reads(i),'OSMN') > 0) then
    l3:         do j=1,timevars
                    if (varmap1(i,j,1) > 5)lbad2=.true.
                enddo l3
                if (lbad2)write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E52',modnam,'READ STATEMENT',i,'"',&
                    trim(adjustl(os_reads(i))),'" INCLUDES DATE/TIME VARIABLES BUT THEY ARE NOT FIRST SET'
                
            endif
            
        endif
!       get the maximum number of variables read for each read statement
!       this will be used for allocation of varmap
        j=1
        lend=.false.
        do while(j <= nosvars .and. .not. lend)
            if (varmap1(i,j,1) == 0) then
                lend=.true.
            else
                j=j+1
            endif
        enddo
!       subtract 1 from j to account for the last variable check was the first variable not read in
        j=j-1
        if (j > nreadvars) nreadvars=j
!       if format statement is not FREE
!       check the format statement to see if left and right parentheses
!       totals match
        if (trim(adjustl(os_format1(i))) /= 'FREE') then
!           first character should be ( and last character should be )
            if (os_format1(i)(1:1) /= '(' .and. os_format1(i)(len_trim(os_format1(i)):len_trim(os_format1(i))) /= ')') then
                lbad2=.true.
                write(msg_unit,formstr(4))adjustl(pathid(ipath)),'E53',modnam,'FIRST CHARACTER',os_format1(i)(1:1),&
                    'AND/OR LAST CHARACTER',os_format1(i)(len_trim(os_format1(i)):len_trim(os_format1(i))),' IN FORMAT ',i,&
                    'ARE NOT PARENTHESES'
            endif
            nparenth=0
    l4:     do j=1,len_trim(os_format1(i))
                if (os_format1(i)(j:j) == '(') then
                    nparenth(1)=nparenth(1)+1
                endif
                if (os_format1(i)(j:j) == ')') then
                    nparenth(2)=nparenth(2)+1
                endif
            enddo l4
!           if left and right parentheses counts don't match then
!           issue error
            if (nparenth(1) /=nparenth(2)) then
                write(msg_unit,formstr(3))adjustl(pathid(ipath)),'E53',modnam,'NUMBER OF LEFT PARENTHESES',nparenth(1),&
                    'NOT EQUAL TO NUMBER OF RIGHT PARENTHESES',nparenth(2),'FOR FORMAT KEYWORD'
                
                if (.not. lbad2) lbad2=.true.
            endif
        endif
    enddo l1
      
!   deallocate os_reads, no longer needed
    deallocate(os_reads)
    
      return
    end subroutine osformread
!*********************************************************************************************************

    subroutine os_proc
!=========================================================================================================
!   SUBROUTINE OS_PROC
!   THIS SUBROUTINE CONTROLS THE READING AND QA OF ONSITE DATA FOR STAGE 1
!
!   MODIFIED MAY 5, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      AERMET
!
!   Variable definitions
!      
!   Integer variables
!   d:              day loop counter
!   h:              hour loop counter
!   idattim:        8-element array to get current date/time from date_and_time subroutine
!   y:              year loop counter
!   m:              month loop counter
!   imon:           start and end month for a year in the data period
!   idy:            start day and end day for a month in the data period
!   i:              single level variable counter
!   j:              multi-level variable counter
!   ilev:           level loop counter
!   ivar:           variable loop counter across osvars
!   ivar1:          variable loop counter across variables read in
!   iday1:          counter of number of days in data period for arrays
!   addvar:         addition to nvar to account for heights in osdata1 if OSHEIGHTS used
!                   instead of HT
!
!   Logical variables
!   leap:           variable denoting if year is a leap year
!
!   Character variables
!   formstr:		formats for messages
!   cdate:          date output from data_and_time function
!   data_form:      format for writing data to QAOUT file
!   modnam:         Subroutine name
!=========================================================================================================   
    use main1, only: lstage,days,leapyr,numdays,istage
    use file_units, only: os_qaout_unit
    implicit none
    integer(kind=4) :: idattim(8),y,m,imon(2),idy(2),i,j,addvar,ivar,ivar1,d,h,ilev,iday1
    !integer k !temp
    logical :: leap
    character(len=8) :: cdate
    character(len=60) :: formstr(4)
    character(len=10) :: modnam='OS_PROC'
    character(len=200) :: data_form
    
!   formats for messages
    
!   1. extraction message
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a8,1x,2(i2.2,a1),i2.2)'
    
!   2.  number of extracted, duplicate observations, sub-hourly observations not used, or bad height levels
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8)'
    
!   3. no extracted data, check dates
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a1,i8,a)'
    
!   4. first level multi-level variable
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,a2,1x,a,1x,i2)'

!	initialize
    d=0
    h=0
    y=0
    m=0
    imon=0
    idy=0
    ivar1=0
    lbad=.false.
    addvar=0
    
!   set value for nos_audit_vars
    vv: do ivar=1,nosvars
        if (osvars(ivar)%laudit) nos_audit_vars=nos_audit_vars+1
    enddo vv
    
!   get the number of days in the data period
    nosdays=numdays(osstart,osend)
        
!   if stage 1, get maximum number of levels based on the max
!   of varmap and nlevel.  if scalars only are read
!   varmap=0 and nlevel=1.  If multi-level data read
!   varmap could be 1 or greater and nlevel=1.
    if (istage == 1 .and. osstage1) nlevel=max(nlevel,maxval(varmap(:,:,2)))
    
!   get # of variables and which ones are QA'd
!   don't include time variables
    l1: do ivar=6,nosvars
            if (osvars(ivar)%lread .and. ivar > 5) nvars=nvars+1
    enddo l1

!   allocate arrays

    allocate(osdata1a(nvars,nlevel,24,nosdays))
    allocate(read_index(nvars))
    
    read_index=0
    allocate(os_date(nosdays))
    if (osvars(5)%lread) then
        allocate(obs_per_hr(nvars,nlevel,nosdays,24))
        allocate(os_subhr_vals(nobs_hr,nvars,nlevel,24,nosdays))
        if (osvars(wind_vars(2))%lread) then
!           allocate calm obs array if reading wind speed
            allocate(lcalm_obs(nobs_hr,nlevel,24,nosdays))
            lcalm_obs=.false.
        endif
        obs_per_hr=0
    endif
    
    if (osvars(wind_vars(2))%lread) then
!       allocate calm obs array if reading wind speed
        allocate(lcalm_obs_hr(nlevel,24,nosdays))
        lcalm_obs_hr=.false.
    endif
    
    allocate(nomisshr(nosdays,24))
    allocate(losobs(nosdays))
    
    
!   initialize arrays to missing
!   order of variables will be in the order of the osvars data type, not
!   the order variables are read in from the data file.
!   also get location of heights, wind direction, and wind speed if read
!   heights will be reset to first element of osdata1 later
    j=0
  l2: do ivar=1,nosvars
        if (osvars(ivar)%lread .and. ivar > 5) then
            j=j+1
            read_index(j)=ivar
            if (ivar == height_var) osvars(height_var)%readvar=j
            if (ivar == wind_vars(1)) osvars(wind_vars(1))%readvar=j
            if (ivar == wind_vars(2)) osvars(wind_vars(2))%readvar=j
            osdata1a(j,:,:,:)=real(osvars(ivar)%missval,r8)
            if (osvars(5)%lread) os_subhr_vals(:,j,:,:,:)=real(osvars(ivar)%missval,r8)
        endif
    enddo l2
    
!   initialize os_date to have all dates within the data period
!   even if a day may not have observations
    iday1=0
    l3: do y=osdates(1,1),osdates(2,1)
!       check to see if year is leap year
        call leapyr(y,leap)
!       set initial values of imonth(1) and imonth(2) to
!       1 and 12
        imon(1)=1
        imon(2)=12
!       reset imonth(1) and imonth(2) to first and last
!       month if first or last year of data period
        if (y == osdates(1,1)) imon(1)=osdates(1,2)
        if (y == osdates(2,1)) imon(2)=osdates(2,2)
          
    l4: do m=imon(1),imon(2)
!           initialize iday(1) to first day of month and
!           iday(2) to last day of the month
            idy(1)=1
            idy(2)=days(m)
!           reset iday(2) to 29 if February and leap year
            if (m == 2 .and. leap) idy(2)=days(m)+1   
!           set iday(1) and iday(2) to first and last days
!           if first month of first year and last month
!           of last year
            if (m == imon(1) .and. y == osdates(1,1)) idy(1)=osdates(1,3)
            if (m == imon(2) .and. y == osdates(2,1))idy(2)=osdates(2,3)
    l5:     do d=idy(1),idy(2)
                iday1=iday1+1
                os_date(iday1)=y*10000+m*100+d
            enddo l5
        enddo l4         
    enddo l3
    
    losobs=.false.
    nomisshr=.false.  !if hour is not missing nomisshr will be true, if missing, then false
    
!   read the input data
!   get time time before extraction
    call date_and_time(date=cdate,values=idattim)
    
    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I60',modnam,'BEGIN:',trim(adjustl(pathid(ipath))),'EXTRACTION',cdate,&
    idattim(5),':',idattim(6),':',idattim(7)
  
!   read the site-specific or PROG datas
!   if stage 1, read the DATA file
!   if stage 2, read the QAOUT file from previous stage 1
    if (istage == 1 .and. osstage1) then
        call read_os
    else
        call read_ext
    endif
    
!   write summary of extractions
!   extracted observations
    write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I63',modnam,'NUMBER OF EXTRACTED OBSERVATIONS:',nos(2)
    
!   duplicate observations
    write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I64',modnam,'NUMBER OF DUPLICATE OBSERVATIONS:',nos(4)
    
!   sub-hourly observations not used
    if (osvars(5)%lread) write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I65',modnam,&
        'NUMBER OF SUB-HOURLY OBSERVATIONS NOT USED:',nos(5)
    
!   number of bad height levels
    if (osvars(height_var)%lread) write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I66',modnam,'NUMBER OF BAD HEIGHT LEVELS: ',&
        nbadhts
    
    call date_and_time(date=cdate,values=idattim)  
        
    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I60',modnam,'END:',trim(adjustl(pathid(ipath))),'EXTRACTION',cdate,&
    idattim(5),':',idattim(6),':',idattim(7)
    
!   now check that heights are in correct order 
    if (.not. lbados) then
        if (luseoshts .or. osvars(height_var)%lread) call check_hts
    endif
    
!   if no error, proceed with hourly averaging (if applicable) and auditing (if applicabe) and filling in osdata array
    if (.not. lbados) then
!       if no valid data, alert user, may be outside XDATES
        if (nos(2) == 0) then
            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W46',modnam,'NO EXTRACTED DATA BETWEEN',osstart,'-',osend,&
                '; CHECK XDATES'
        else
            allocate(os_info(nosdays))
!           fill in arrays
    l7:     do d=1,nosdays
                os_info(d)%osdate=os_date(d)
                os_info(d)%sobs=losobs(d)
    l8:         do h=1,24
                    os_info(d)%have_obs(h)=nomisshr(d,h)
                enddo l8
            enddo l7

!           check to see if wind speed is being read for hourly data
!           if so, then check the wind speed against the threshold and
!           reset wind direction (if read) and standard deviations (if read)
!           this is only done for data that is read in as hourly data
!           if read in as sub-hourly data, then the sub-hourly subroutines will take
!           care of the checks and resets.

            if (osvars(wind_vars(2))%lread .and. .not. osvars(5)%lread .and. istage==1 .and. osstage1) call check_wind
            

 !          allocate the audit arrays auditing performed for stage 1
            if (istage == 1 .and. osstage1 .and. (loskeywords(8) .or. osvars(temp_var)%lread .or. osvars(wind_vars(1))%lread .or. &
                osvars(wind_vars(2))%lread)) then
                i=0
                j=0
                allocate(os_audit_index(nos_audit_vars,2))

                os_audit_index=0
                allocate(missvals(nos_audit_vars))
                allocate(lowbound(nos_audit_vars))
                allocate(upbound(nos_audit_vars))
                if (osvars(5)%lread) then
                    allocate(os_audit_counts(nos_audit_vars,nlevel,2,4))
                else
                    allocate(os_audit_counts(nos_audit_vars,nlevel,1,4))
                endif
                os_audit_counts=0
!               fill in os_audit_index array
    l9:         do ivar=6,nosvars
                    if (osvars(ivar)%lread) then
                        j=j+1 !count of variables being read
!                       set indices for variable reads
                        if (ivar == hflux_var)osvars(hflux_var)%readvar=j
                        if (ivar == lflux_var)osvars(lflux_var)%readvar=j
                        if (ivar == ustar_var)osvars(ustar_var)%readvar=j
                        if (ivar == mix_var) osvars(mix_var)%readvar=j
                        if (ivar == zo_var) osvars(zo_var)%readvar=j
                        if (ivar == inso_var) osvars(inso_var)%readvar=j
                        if (ivar == nrad_var) osvars(nrad_var)%readvar=j
                        if (ivar == dt01_var) osvars(dt01_var)%readvar=j
                        if (ivar == vptg_var) osvars(vptg_var)%readvar=j
                        if (ivar == l_var) osvars(l_var)%readvar=j
                        if (ivar == wstar_var) osvars(wstar_var)%readvar=j
                        if (ivar == alb_var) osvars(alb_var)%readvar=j
                        if (ivar == bo_var) osvars(bo_var)%readvar=j
                        if (ivar == clht_var)osvars(clht_var)%readvar=j
                        if (ivar == temp_var) osvars(temp_var)%readvar=j
                        if (ivar == dewpt_var) osvars(dewpt_var)%readvar=j
                        if (ivar == cloud_var) osvars(cloud_var)%readvar=j
                        if (ivar == slp_var) osvars(slp_var)%readvar=j
                        if (ivar == pres_var) osvars(pres_var)%readvar=j
                        if (ivar == rh_var) osvars(rh_var)%readvar=j
                        if (ivar == prcp_vars(1))osvars(prcp_vars(1))%readvar=j
                        if (ivar == prcp_vars(2))osvars(prcp_vars(2))%readvar=j
                        if (ivar == sa_var) osvars(sa_var)%readvar=j
                        if (ivar == se_var) osvars(se_var)%readvar=j
                        if (ivar == sv_var) osvars(sv_var)%readvar=j
                        if (ivar == sw_var) osvars(sw_var)%readvar=j
                        if (ivar == su_var) osvars(su_var)%readvar=j
                        if (ivar == vv_var) osvars(vv_var)%readvar=j
                        if (osvars(ivar)%laudit) then
                            if (osvars(ivar)%firstlev > 1) write(msg_unit,formstr(4))adjustl(pathid(ipath)),'Q60',modnam,&
                            'FIRST LEVEL FOR',osvars(ivar)%varname(1:2),'IS LEVEL',osvars(ivar)%firstlev
                            
                            i=i+1 !count of variables being audited
                            os_audit_index(i,1)=j!order in osdata1a 
                            os_audit_index(i,2)=ivar !order in osvars
                            missvals(i)=real(osvars(ivar)%missval,r8)
                            lowbound(i)=real(osvars(ivar)%lowbound,r8)*osvars(ivar)%conv
                            upbound(i)=real(osvars(ivar)%upbound,r8)*osvars(ivar)%conv
                        endif
                    endif
                enddo l9
                if (osvars(temp_var)%lread .and. osvars(dewpt_var)%lread) checktempdew=.true.
            endif
            if (.not. osstage1 .and. lstage(2)) then
!               get index values for different variables
                i=0
                j=0
    l10:        do ivar=6,nosvars
                    if (osvars(ivar)%lread) then
                        j=j+1 !count of variables being read
!                       set indices for variable reads 
                        if (ivar == hflux_var)osvars(hflux_var)%readvar=j
                        if (ivar == Lflux_var)osvars(lflux_var)%readvar=j
                        if (ivar == ustar_var)osvars(ustar_var)%readvar=j
                        if (ivar == mix_var) osvars(mix_var)%readvar=j
                        if (ivar == zo_var) osvars(zo_var)%readvar=j
                        if (ivar == inso_var) osvars(inso_var)%readvar=j
                        if (ivar == nrad_var) osvars(nrad_var)%readvar=j
                        if (ivar == dt01_var) osvars(dt01_var)%readvar=j
                        if (ivar == vptg_var) osvars(vptg_var)%readvar=j
                        if (ivar == l_var) osvars(l_var)%readvar=j
                        if (ivar == wstar_var) osvars(wstar_var)%readvar=j
                        if (ivar == alb_var) osvars(alb_var)%readvar=j
                        if (ivar == bo_var) osvars(bo_var)%readvar=j
                        if (ivar == clht_var)osvars(clht_var)%readvar=j
                        if (ivar == temp_var) osvars(temp_var)%readvar=j
                        if (ivar == dewpt_var) osvars(dewpt_var)%readvar=j
                        if (ivar == cloud_var) osvars(cloud_var)%readvar=j
                        if (ivar == slp_var) osvars(slp_var)%readvar=j
                        if (ivar == pres_var) osvars(pres_var)%readvar=j
                        if (ivar == rh_var) osvars(rh_var)%readvar=j
                        if (ivar == prcp_vars(1))osvars(prcp_vars(1))%readvar=j
                        if (ivar == prcp_vars(2))osvars(prcp_vars(2))%readvar=j
                        if (ivar == sa_var) osvars(sa_var)%readvar=j
                        if (ivar == se_var) osvars(se_var)%readvar=j
                        if (ivar == sv_var) osvars(sv_var)%readvar=j
                        if (ivar == sw_var) osvars(sw_var)%readvar=j
                        if (ivar == su_var) osvars(su_var)%readvar=j
                        if (ivar == vv_var) osvars(vv_var)%readvar=j
                    endif
                enddo l10
            endif
!           if sub-hourly data, average to hourly data; QA will also take place
!           for any audited variables

            if (istage == 1 .and. osstage1 .and. osvars(5)%lread) call avg_hr
        
!           audit hourly data
            if (istage == 1 .and. osstage1 .and. (loskeywords(8) .or. osvars(temp_var)%lread .or. osvars(wind_vars(1))%lread .or. &
            osvars(wind_vars(2))%lread)) call os_audithr
        
!           allocate osdata array
!           for stage 1 and if heights are not read in and OSHEIGHTS used, then
!           add 1 to nvars to include a height variable

            if (osvars(height_var)%readvar == 0 .and. luseoshts) then
                allocate(osdata1(nvars+1,nlevel,24,nosdays))
                if (istage == 1 .and. osstage1) allocate(qaout_vars(nvars+1))
                addvar=1
!               add 1 to direction, speed, temperature, dewpoint, and cloud cover variable indices if read
!               this info will also be used in stage 2
                if (osvars(hflux_var)%lread) osvars(hflux_var)%readvar=osvars(hflux_var)%readvar+1
                if (osvars(lflux_var)%lread) osvars(lflux_var)%readvar=osvars(lflux_var)%readvar+1
                if (osvars(ustar_var)%lread) osvars(ustar_var)%readvar=osvars(ustar_var)%readvar+1
                if (osvars(mix_var)%lread) osvars(mix_var)%readvar=osvars(mix_var)%readvar+1
                if (osvars(zo_var)%lread) osvars(zo_var)%readvar=osvars(zo_var)%readvar+1
                if (osvars(inso_var)%lread) osvars(inso_var)%readvar=osvars(inso_var)%readvar+1
                if (osvars(nrad_var)%lread) osvars(nrad_var)%readvar=osvars(nrad_var)%readvar+1
                if (osvars(dt01_var)%lread) osvars(dt01_var)%readvar=osvars(dt01_var)%readvar+1
                if (osvars(vptg_var)%lread) osvars(vptg_var)%readvar=osvars(vptg_var)%readvar+1
                if (osvars(l_var)%lread) osvars(l_var)%readvar=osvars(l_var)%readvar+1
                if (osvars(wstar_var)%lread) osvars(wstar_var)%readvar=osvars(wstar_var)%readvar+1
                if (osvars(clht_var)%lread) osvars(clht_var)%readvar=osvars(clht_var)%readvar+1
                if (osvars(wind_vars(1))%lread) osvars(wind_vars(1))%readvar=osvars(wind_vars(1))%readvar+1
                if (osvars(wind_vars(2))%lread) osvars(wind_vars(2))%readvar=osvars(wind_vars(2))%readvar+1
                if (osvars(temp_var)%lread) osvars(temp_var)%readvar=osvars(temp_var)%readvar+1
                if (osvars(dewpt_var)%lread) osvars(dewpt_var)%readvar=osvars(dewpt_var)%readvar+1
                if (osvars(cloud_var)%lread) osvars(cloud_var)%readvar=osvars(cloud_var)%readvar+1
                if (osvars(slp_var)%lread) osvars(slp_var)%readvar=osvars(slp_var)%readvar+1
                if (osvars(pres_var)%lread) osvars(pres_var)%readvar=osvars(pres_var)%readvar+1
                if (osvars(rh_var)%lread) osvars(rh_var)%readvar=osvars(rh_var)%readvar+1
                if (osvars(sa_var)%lread) osvars(sa_var)%readvar=osvars(sa_var)%readvar+1
                if (osvars(se_var)%lread) osvars(se_var)%readvar=osvars(se_var)%readvar+1
                if (osvars(sv_var)%lread) osvars(sv_var)%readvar=osvars(sv_var)%readvar+1
                if (osvars(sw_var)%lread) osvars(sw_var)%readvar=osvars(sw_var)%readvar+1
                if (osvars(su_var)%lread) osvars(su_var)%readvar=osvars(su_var)%readvar+1
                if (osvars(vv_var)%lread) osvars(vv_var)%readvar=osvars(vv_var)%readvar+1
                if (osvars(prcp_vars(1))%lread) osvars(prcp_vars(1))%readvar=osvars(prcp_vars(1))%readvar+1
                if (osvars(prcp_vars(2))%lread) osvars(prcp_vars(2))%readvar=osvars(prcp_vars(2))%readvar+1
            else !covers stage 2 as well
!               reset scalars because ht is being moved to the front so the scalars move over 1
!               multi-level data doesn't shift because height is the first multi-level variable
                if (osvars(hflux_var)%lread) osvars(hflux_var)%readvar=osvars(hflux_var)%readvar+1
                if (osvars(lflux_var)%lread) osvars(lflux_var)%readvar=osvars(lflux_var)%readvar+1
                if (osvars(ustar_var)%lread) osvars(ustar_var)%readvar=osvars(ustar_var)%readvar+1
                if (osvars(mix_var)%lread) osvars(mix_var)%readvar=osvars(mix_var)%readvar+1
                if (osvars(zo_var)%lread) osvars(zo_var)%readvar=osvars(zo_var)%readvar+1
                if (osvars(inso_var)%lread) osvars(inso_var)%readvar=osvars(inso_var)%readvar+1
                if (osvars(nrad_var)%lread) osvars(nrad_var)%readvar=osvars(nrad_var)%readvar+1
                if (osvars(dt01_var)%lread) osvars(dt01_var)%readvar=osvars(dt01_var)%readvar+1
                if (osvars(vptg_var)%lread) osvars(vptg_var)%readvar=osvars(vptg_var)%readvar+1
                if (osvars(l_var)%lread) osvars(l_var)%readvar=osvars(l_var)%readvar+1
                if (osvars(wstar_var)%lread) osvars(wstar_var)%readvar=osvars(wstar_var)%readvar+1
                if (osvars(clht_var)%lread) osvars(clht_var)%readvar=osvars(clht_var)%readvar+1
                if (osvars(cloud_var)%lread) osvars(cloud_var)%readvar=osvars(cloud_var)%readvar+1
                if (osvars(slp_var)%lread) osvars(slp_var)%readvar=osvars(slp_var)%readvar+1
                if (osvars(pres_var)%lread) osvars(pres_var)%readvar=osvars(pres_var)%readvar+1
                if (osvars(prcp_vars(1))%lread) osvars(prcp_vars(1))%readvar=osvars(prcp_vars(1))%readvar+1
                if (osvars(prcp_vars(2))%lread) osvars(prcp_vars(2))%readvar=osvars(prcp_vars(2))%readvar+1
                allocate(osdata1(nvars,nlevel,24,nosdays))
                if (istage == 1 .and. osstage1) allocate(qaout_vars(nvars))
                addvar=0
            endif
!           fill in data array and output variable name array
!           if heights are read in and OSHEIGHTS used as well, reset
!           heights to the OSHEIGHT values
!           if heights are not read in but OSHEIGHTS are read in
!       
!           also make heights the first variable in osdata
!           this means any scalar variables such as PRCP, PRES, etc will be
!           shifted by 1 from osdata1a to osdata
!           multi-level variables after heights keep the same location in the arrays
!           this is because height is always the first multi-level variable
!           example 
!                       1     2      3    4     5     6
!           osdata1a:  PRCP, PRES, HTNN, TTNN, WDNN, WSNN

!                       1   2     3     4     5     6
!           osdata:  HTNN, PRCP, PRES, TTNN, WDNN, WSNN

!           assign osheights to first variable or HT to first variable if read
            if (luseoshts) then
                if (istage == 1 .and. osstage1) qaout_vars(1)=osvars(height_var)%varname
    ll1:        do ilev=1,nlevel
                    osdata1(1,ilev,:,:)=os_heights(ilev) 
                enddo ll1
            elseif (osvars(height_var)%readvar /= 0) then
                osdata1(1,:,:,:)=osdata1a(osvars(height_var)%readvar,:,:,:)
                if (istage == 1 .and. osstage1) qaout_vars(1)=osvars(height_var)%varname
            endif

    v1:     do ivar1=1,nvars
                if (read_index(ivar1) == height_var) cycle v1
                if ((read_index(ivar1) < height_var .and. (osvars(height_var)%readvar /= 0 .or. luseoshts)) .or. &
                    (read_index(ivar1) > height_var .and. osvars(height_var)%readvar == 0)) then
                    osdata1(ivar1+1,:,:,:)=osdata1a(ivar1,:,:,:)
                    if (istage == 1 .and. osstage1) qaout_vars(ivar1+1)=osvars(read_index(ivar1))%varname
                else
                    osdata1(ivar1,:,:,:)=osdata1a(ivar1,:,:,:)
                    if (istage == 1 .and. osstage1) qaout_vars(ivar1)=osvars(read_index(ivar1))%varname
                endif
            enddo v1

!           write out the data if QAOUT requested
            if (istage == 1 .and. osstage1 .and. loskeywords(4)) then
                call header(nvars+addvar)

!               data format
                !write(data_form,'(a,i2,a)')'(i8.8,3x,i2.2,5x,i3,',nvars+addvar,'(1x,f11.4))'
                write(data_form,'(a,i2,a)')'(i8.8,3x,i2.2,5x,i3,',nvars+addvar,'(1x,f12.5))'
                
    d1:         do d=1,nosdays
                    if (os_info(d)%sobs) then
    h1:                 do h=1,24
    !                       only write if the hour is not missing
                            if (.not. os_info(d)%have_obs(h)) cycle h1
    ll2:                    do ilev=1,nlevel
                                
                                write(os_qaout_unit,data_form)os_info(d)%osdate,h,&
                                ilev,(osdata1(ivar1,ilev,h,d),ivar1=1,nvars+addvar)
                            enddo ll2
                        enddo h1
                    endif
                enddo d1
            endif
        endif
    endif
    
!   deallocate temporary arrays
    if (allocated(osdata1a)) deallocate(osdata1a)
    if (allocated(ext_index)) deallocate(ext_index)
    if (allocated(varmap)) deallocate(varmap)
    if (allocated(qaout_vars)) deallocate(qaout_vars)
    if (allocated(os_date)) deallocate(os_date)
    if (allocated(obs_per_hr)) deallocate(obs_per_hr)
    if (allocated(os_subhr_vals)) deallocate(os_subhr_vals)
    if (allocated(losobs)) deallocate(losobs)
    if (allocated(lcalm_obs)) deallocate(lcalm_obs)
    if (allocated(nomisshr)) deallocate(nomisshr)
    if (allocated(missvals)) deallocate(missvals)
    if (allocated(upbound)) deallocate(upbound)
    if (allocated(lowbound)) deallocate(lowbound)
    if (allocated(os_format)) deallocate(os_format)
    if (allocated(read_index)) deallocate(read_index)
    
    return
    end subroutine os_proc
!*********************************************************************************************************

    subroutine header(nwritevar)
!=========================================================================================================
!   SUBROUTINE HEADER
!   THIS SUBROUTINE WRITES THE HEADER FOR THE QAOUT FILE
!
!   MODIFIED JANUARY 20, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PROC)
!
!   INPUT ARGUMENTS
!   
!   NWRITEVAR:      NUMBER OF VARIABLES TO WRITE        
!
!   Variable definitions
!      
!   Integer variables
!   nwritevar:      number of variables to write
!   ivar:           loop counter
!
!   Character variables
!   bndstr:			character string of < or <=
!   celev:          character string of user entered elevation (oselev)
!   alat:           character string of latitude with N or S descriptor
!   alon:           character string of longitude with E or W descriptor
!   cgmt2lst:       character string of time adjustment (osgmt2lst)
!   olow_str:       OVERLAND or OVERWATER character string
!   header_form:	header format
!   modnam:         Subroutine name
!=========================================================================================================   
    use main1, only: versn
    use file_units, only: os_qaout_unit
    implicit none
    integer(kind=4), intent(in) :: nwritevar
    integer(kind=4) :: ivar
    character(len=2) :: bndstr
    character(len=10) :: celev=''
    character(len=3) :: cgmt2lst=''
    character(len=10) :: alat,alon,olow_str
    character(len=10) :: modnam='OS_PROC'
    character(len=15) :: header_form
    
    if (lgmt2lst) write(cgmt2lst,'(i3)')osgmt2lst
    
    if (loselev) write(celev,'(f10.3)')oselev
    
    if (overland) then
        olow_str='OVERLAND'
    else
        olow_str='OVERWATER'
    endif
                
!   set values for alat/alon
    if (oslat < 0.0_r8) then
        write(alat,'(f7.3,a1)')dabs(oslat),'S'
    else
        write(alat,'(f7.3,a1)')oslat,'N'
    endif
    if (oslon < 0.0_r8) then
        write(alon,'(f8.3,a1)')dabs(oslon),'W'
    else
        write(alon,'(f8.3,a1)')oslon,'E'
    endif  
    
!   begin writing header
    
!   version
    write(os_qaout_unit,'(2(a))')'AERMET VERSION ',versn
    
!   location
    write(os_qaout_unit,'(a,a8,2(1x,a10),2(1x,a))')'LOCATION ',osid,alat,alon,trim(adjustl(cgmt2lst)),trim(adjustl(celev))
    
!   file type
    write(os_qaout_unit,'(a)')'FILE TYPE: QAOUT'

!   threshold
    if (loskeywords(11)) write(os_qaout_unit,'(a,1x,f6.3)')'THRESHOLD',threshspeed
    
!   delta temps if used
    if (loskeywords(9)) write(os_qaout_unit,'(a,1x,i1,2(1x,f8.3))')'DELTA_TEMP',delta_t_ind,osdt_hts(1,1),osdt_hts(1,2)
    
!   overland/overwater identifier
    write(os_qaout_unit,'(a)')trim(adjustl(olow_str))
    
!   dates
    write(os_qaout_unit,'(a,i4,2(1x,i2.2),1x,i4,2(1x,i2.2))')'DATES ',osdates(1,1),osdates(1,2),osdates(1,3),osdates(2,1),&
        osdates(2,2),osdates(2,3)
    
!   RANGE of modified variables
    v1: do ivar=6,nosvars !nwritevar
        if (osvars(ivar)%lread .and. osvars(ivar)%luse .and. osvars(ivar)%lmod) then
            if (osvars(ivar)%lincbound) then
                bndstr='<='
            else
                bndstr='<'
            endif
            write(os_qaout_unit,'(a,1x,a4,i6,1x,a2,1x,2(1x,i6))')'RANGE',osvars(ivar)%varname,osvars(ivar)%lowbound,bndstr,&
                osvars(ivar)%upbound,osvars(ivar)%missval
        endif

    enddo v1
    
!   first and last levels for multi-level variables
    v2: do ivar=height_var,nosvars !nwritevar
        if (osvars(ivar)%lread .and. osvars(ivar)%luse) then
            write(os_qaout_unit,'(a4,1x,a,1x,2(i3,1x))')osvars(ivar)%varname,'FIRST-LAST LEVELS',osvars(ivar)%firstlev,&
                osvars(ivar)%nlevels
        endif

    enddo v2    
!   data header
    !write(header_form,'(a,i2,a)')'(a,',nwritevar,'(8x,a4))'
    write(header_form,'(a,i2,a)')'(a,',nwritevar,'(9x,a4))'
    write(os_qaout_unit,header_form)'DATE       HR     LEV',(trim(adjustl(qaout_vars(ivar))),ivar=1,nwritevar)
    
    return
    end subroutine header
!*********************************************************************************************************

    subroutine read_os
!=========================================================================================================
!   SUBROUTINE READ_OS
!   THIS SUBROUTINE READS THE DATA FILE FOR ONSITE OR PROG DATA
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   eof:            end of file indicator
!   centuries:      centuries of start and end year of data period
!   iyear2:         2-digit year of iyear
!   last_year:      previous record's value for iyear2
!   nintvar:        number integer variables read in
!   nrealvar:       number of real variables read in
!   icent:          number of centuries in data set (checking for cross-over)
!   i:              loop counter
!   j:              loop counter
!   iflag:          I/O flag
!   iread:          READ loop counter
!   ilev:           level loop counter
!   svar:           variable counter in varmap
!   irec1:          record counter
!   ivar:           variable counter across osvars
!   ivar1:          variable counter across variables read in
!   imin2:          number of sub-hourly obs read for each hour to compare
!                   against nobs_hr
!   imin1:          minute index (up to 12)
!   imin:           actual minute of hour
!   iday1:          counter of number of days in data period for arrays
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!   ihr:            integer observation hour in GMT or LST
!                   will be reset to LST hours 1-24 if GMT
!   savehr:         previous observation's hour
!   savemin:        previous observation's minute
!   sdate:          integer observation date (YYYYMMDD)
!   savedate:       previous observation's date (YYYYMMDD)
!   nbad:           number of bad data reads
!   maxbad:         maximum number of bad reads allowed
!   intvars:        values of integer variables
!
!   Logical variables
!   leap:           variable denoting if year is a leap year
!   lstop:          variable to stop loop when checking for number of integer
!                   and real data variables
!   crossover:      denotes data spans two centuries
!   lgo:            date of observation is within data window set by osstart and osend
!   lfound:         variable used to stop do while loop checking to see if a variable is read in
!
!   Character variables
!   datline:		data line
!   adate:          character string of date (YYYYMMDD)
!   formstr:        format for messages
!   date_hr_str:    date and hour text string for error messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: data_dates,numdays,noprint,eps
    use file_units, only: os_data_unit
    implicit none

    integer(kind=4) :: eof,centuries(2),last_year,iyear2,nintvar,nintvar1,nrealvar,iread,i,j,icent,svar,&
    irec1,savemin,imin2,iflag,ilev,iday1,iyear,imonth,iday,ihr,savehr,ivar,ivar1,imin1,imin
    integer(kind=4) :: nbad=0
    integer(kind=8) :: sdate
    integer(kind=8) :: savedate=0
    integer(kind=4), parameter :: maxbad=3
    integer(kind=4), allocatable, dimension(:) :: intvars 
    real(kind=r8), allocatable, dimension(:) :: realvars 
    logical :: lstop,crossover,lgo,lfound 
    character(len=5000) :: datline
    character(len=60) :: formstr(8)
    character(len=20) :: date_hr_str
    character(len=8) :: adate=''
    character(len=10) :: modnam='READ_OS'

!   formats for messages
    
!   1.  message to screen to write out processing day
    write(formstr(1),'(a)')'(1x,a,1x,a,1x,a,1x,2(a2,a1),a4,1x,a)'
    
!   2.   duplicate observation
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20)'
    
    
!   3.  error reading data line
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i6,1x,a,1x,i2,1x,a)'
    
!   4.  minute not used
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,i2,1x,a,1x,a20)'
    
!   5.  duplicate observation for the minute
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20,1x,a,1x,i2.2)'
    
!   6.  number of obs/hour exeeds input OBS/HR value
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'2(a,1x,i3,1x),a20,1x,a,1x,i2.2)'

!   7.  current date is prior to previous date in file
    write(formstr(7),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,i8)'

!   8.  exceed number of read errors
    write(formstr(8),'(2(a))')trim(adjustl(msg_form)),'a,1x,i1,1x,a,1x,i1)'
    
!   get the centuries of the start and end dates in case years in data file are 2-digit
!   get 2-digit years
    l1: do i=1,2
        if (i==1) then
            write(adate,'(i8)')osstart
        else
            write(adate,'(i8)')osend
        endif
        read(adate(1:2),'(i2)')centuries(i) !2-digit century, i.e. 19, 20
        centuries(i)=centuries(i)*100 !make century 1900 or 2000
    enddo l1
    
    iread=1
    eof=0
    sdate=0
    crossover=.false.
    last_year=0
    icent=0
    irec1=0
    savemin=0
    imin2=0
    
!   allocate integer variables, i.e. dates
!   number will be 4 if hourly data, 5 if sub-hourly data
!   will be the same for all lines
    if (osvars(5)%lread) then
        nintvar1=5
    else
        nintvar1=4
    endif
    
    allocate(intvars(nintvar1))
    
    do while (eof == 0 .and. .not. lbad .and. sdate <= osend)
        read(os_data_unit,'(a5000)',iostat=eof)datline
        irec1=irec1+1
        if (eof == 0) then
!           get number of real variables on line and check for integer variables
            nrealvar=0
            nintvar=0
            ivar1=1
            lstop=.false.
            do while(ivar1 <= nreadvars .and. .not. lstop)
                if (varmap(iread,ivar1,1) > 0) then
                    if (varmap(iread,ivar1,1) > 5) then 
                        nrealvar=nrealvar+1 
                    else
                        nintvar=nintvar+1
                    endif
                else
                    lstop=.true.
                endif
                ivar1=ivar1+1
            enddo 
!           allocate real variables based on number of reals on line
            allocate(realvars(nrealvar))
!           do not include format statement if FREE format
            if (trim(adjustl(os_format(iread))) == 'FREE') then
                read(datline,*,iostat=iflag)(intvars(i),i=1,nintvar),(realvars(j),j=1,nrealvar)
            else
                read(datline,os_format(iread),iostat=iflag)(intvars(i),i=1,nintvar),(realvars(j),j=1,nrealvar)
            endif

            if (iflag == 0) then
                nos(1)=nos(1)+1 !total obs read
!               get the date/time if read in.  If not read on the dataline, value
!               from last line with date is used.
                if (nintvar > 0) then
    l2:             do ivar1=1,nintvar
                        if (varmap(iread,ivar1,1)==1) then
                            iday=intvars(ivar1)
                        elseif (varmap(iread,ivar1,1)==2) then
                            imonth=intvars(ivar1)
                        elseif (varmap(iread,ivar1,1)==3) then
                            iyear=intvars(ivar1)
                        elseif(varmap(iread,ivar1,1)==4) then
                            ihr=intvars(ivar1)
                        else
                            imin=intvars(ivar1)
                        endif
                    enddo l2
                
!                   check year if 2-digit and reset to 4 digit
                    if (iyear < 100) then
                        iyear2=iyear !keep 2-digit year
!                       determine if data file has possibly crossed a century
                        if (iyear < last_year) then !implies a century crossover, e.g. 1999 to 2000
                            crossover=.true. 
                            icent=icent+1
                        endif
                
                        if (crossover) then
                            iyear=iyear+centuries(1)+icent*100
                        else
                            iyear=iyear+centuries(1)
                        endif
                        last_year=iyear2
                    endif

!                   if sub-hourly data being used, minutes other than 0
!                   should be incremented by 1 hour (-1)
                    if (osvars(5)%lread .and. imin /=0 ) call data_dates(ihr,iday,imonth,iyear,-1,osstart,osend,51,&
                    sdate,lgo)
!                   check to see if in data window                    
                    call data_dates(ihr,iday,imonth,iyear,osgmt2lst,osstart,osend,51,sdate,lgo)
                    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
                    
                    if (osvars(5)%lread .and. (savedate /= sdate .or. ihr /= savehr)) then
                        imin2=0 !reset minute counter
                    endif
                    
!                   check the minute to see if fits obs/hour
!                   for example, if there are 5 obs/hour, then that means
!                   minutes 12, 24, 36, 48, and 60 (or 0) are valid
!                   but minutes 1, 5, etc. are not valid; instead of an error, issue warning for
!                   those minutes and skip.
                    if (osvars(5)%lread .and. lgo) then
!                       60/nob_hr is minute interval between valid obs. if nobs_hr=5 then there
!                       are 12 minutes between obs.  If nobs_hr=3, then are 20 minutes
!                       if the mod of the current minute and 60/nobs_hr is not 0, then not a valid
!                       minute reading, skip.
                        if (mod(imin,60/nobs_hr) /=0) then
                            lgo=.false.
                            nos(5)=nos(5)+1
                        else
                            imin1=imin/(60/nobs_hr)
                            lgo=.true.
                        endif
                        imin2=imin2+1
                        if (imin2 > nobs_hr) then
                            lbad=.true.
                            write(msg_unit,formstr(6))adjustl(pathid(ipath)),'E56',modnam,'NUMBER OF OBS/HR',imin2,&
                                'EXCEEDS INPUT OBS/HR',nobs_hr,date_hr_str,'MIN',imin
                        endif
                    endif
                endif
                if (lgo) then
                    if (iread == 1) nos(2)=nos(2)+1 !extracted obs in window
                   
                    write(adate,'(i8.8)')sdate
                    if (sdate == savedate) then
                        if (iread == 1 .and. ((ihr == savehr .and. .not. osvars(5)%lread) .or. (ihr == savehr .and. imin==savemin &
                        .and. osvars(5)%lread))) then
                            nos(4)=nos(4)+1
                          
                            if (osvars(5)%lread) then
                                write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I62',modnam,&
                                    'POTENTIAL DUPLICATE OBSERVATION FOR',date_hr_str,'MIN',imin
                            else
                                write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I62',modnam,&
                                    'POTENTIAL DUPLICATE OBSERVATION FOR',date_hr_str
                            endif
                        else
                            nos(3)=nos(3)+1 !increment only if a different hour, this will eliminate duplicates in the final counts
                            nomisshr(iday1,ihr)=.true. !hour has an observation
                        endif
                    else
                        if (sdate < savedate)write(msg_unit,formstr(7))adjustl(pathid(ipath)),'W62',modnam,'CURRENT DATE',sdate,&
                            'IS PRIOR TO PREVIOUS DATE IN DATA FILE',savedate
!                       write to screen the date and hour being processed if NOPRINT not specfied on JOB pathway
!                       if sub-hourly data, only write when imin1=1 (first obs for hour)
                        if (.not. noprint .and. (.not. osvars(5)%lread .or. (osvars(5)%lread .and. imin1 == 1))) &
                        write(output_unit,formstr(1))'Stage 1: Extracting',trim(adjustl(dattype(ipath-3))),&
                            'data for month/day/year',adate(5:6),'/',adate(7:8),'/',adate(1:4),'LST' !,ihr
                        nos(3)=1
                        
                        iday1=numdays(osstart,sdate)!increment day counter for updata1
                        nomisshr(iday1,ihr)=.true. !hour has an observation
                        losobs(iday1)=.true.
                    endif 
                        
!                   map the real variables to the data arrays
    l3:             do ivar1=1,nrealvar
!                       get the location in the main data array and which data array to write to
!                       add the value of nintvar (number of integer variables) to 2nd dimension
!                       of varmap array to offset for presence of integer variables, i.e. dates
!                       find location of variable in main data array
!                       for single level variables, ilev is 0, but will be written to level 1
!                       in the osdata1 array

                        svar=varmap(iread,ivar1+nintvar,1)
                        ilev=varmap(iread,ivar1+nintvar,2)

!                       get position variable is in the main data array
                        ivar=6
                        lfound=.false.
                        i=0

                        do while(ivar <= nosvars .and. .not. lfound)
                            if (osvars(ivar)%lread)  i=i+1
                            if (ivar == svar) lfound=.true.
                            ivar=ivar+1
                        enddo
                        
!                       reset ilev to 1 if 0 for writing purposes
                        if (ilev == 0) ilev=1

!                       assign to arrays
                        if (osvars(5)%lread) then
                            if (dabs(realvars(ivar1)-real(osvars(svar)%missval,r8)) > eps) &
                            os_subhr_vals(imin2,i,ilev,ihr,iday1)=realvars(ivar1)*osvars(svar)%conv
                        else
                            if (dabs(realvars(ivar1)-real(osvars(svar)%missval,r8)) > eps) &
                            osdata1a(i,ilev,ihr,iday1)=realvars(ivar1)*osvars(svar)%conv
                        endif
                    enddo l3                    
                endif
                savedate=sdate
                savehr=ihr
                if (osvars(5)%lread)savemin=imin
            else
                write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W63',modnam,'ERROR READING DATA LINE: ',irec1,' CHECK FORMAT',&
                    iread,trim(adjustl(os_format(iread)))
                nbad=nbad+1
!               number of bad records, which could be headers, exceeds max number allowed, issue error
                if (nbad > maxbad) then
                    lbad=.true.
                    write(msg_unit,formstr(8))adjustl(pathid(ipath)),'E55',modnam,'NUMBER OF DATA READ ERRORS',nbad,&
                        'EXCEEDS MAXIMUM NUMBER ALLOWED',maxbad
                endif
            endif
            
        endif
        if (allocated(realvars)) deallocate(realvars)
        iread=iread+1
        
        if (iread > nread) iread=1 !bottom of loop
    enddo
    
    if (.not. lbados .and. lbad)lbados=.true.
    deallocate(intvars)
    
    return
    end subroutine read_os
!*********************************************************************************************************

    subroutine avg_hr
!=========================================================================================================
!   SUBROUTINE AVG_HR
!   THIS SUBROUTINE CALCULATES HOURLY AVERAGES FROM SUB-HOURLY DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   nobs_hr1:       number of valid obs/hour 
!   icalms:         number of calms/hour
!   dircount:       number of wind directions/hour
!   h_minus_1:      original hour of observation; for processing purposes hour is 
!                   is incremented by 1, e.g. hour 1 minute 10 is assigned to hour 2
!                   h_minus_1 used for debugging
!   ivar:           variable counter across osvars
!   ivar1:          variable counter across variables read in
!   d:              day counter
!   h:              hour counter
!   imin1:          minute index (up to 12)
!   imin:           actual minute of hour
!   ilev:           level loop counter
!   iv:             index in os_audit_index 
!   iswitch:        indicator of hourly data (1) or sub-hourly data(2) for counts
!   orig_date:		original date of observations
!
!   Real variables
!   hrval:          hourly average of variable at level
!
!   Logical variables
!   lfound:         logical variable denoting variable found
!   havedir:        level has wind direction
!   havespeed:      level has wind speed
!   writelev:       write mesage for a level. this is so that single level variables such as PRCP
!
!   Character variables
!   formstr:		formats for messages
!   debuform:       formats for debug messages
!   athresh:        indicates wind speed for minute set to 1/2 threshold
!   vname:          variable name. based on osvars%varname, but if a multi-level data, replaces
!                   NN with level index, i.e. for level 1, WDNN becomes WD01.
!   adate:          character string of date (YYYYMMDD)
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: noprint,eps,debug
    use file_units, only: debug_unit
    implicit none
    integer(kind=4) :: nobs_hr1,icalms,dircount,h_minus_1,ivar,ivar1,d,h,imin1,imin,ilev,iswitch,iv
    integer(kind=8) :: orig_date
    
    real(kind=r8) :: hrval
    logical lfound,havedir,havespeed,writelev
    character(len=1) :: athresh
    character(len=4) :: vname
    character(len=70) :: formstr(4), debugform(9)
    character(len=8) :: adate=''
    character(len=10) :: modnam='AVG_HR'

!   formats for messages
!   1.  message to screen to write out processing day
    write(formstr(1),'(a)')'(1x,a,1x,a,1x,a,1x,2(a2,a1),a4,1x,a)' 
    
!   2. not enough minutes for averaging scalar variable; set to missing
!       or number of minutes < min number of minutes but keep total
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,1x,a,1x,2(a,1x,i2,1x),a)'   
    
!   3. not enough minutes for averaging multi-level variable for a level; set to missing
!       if winds, set to calm
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,1x,a,1x,3(a,1x,i2,1x),a)'  
    
!   4.  temperature < dewpoint
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,2(1x,a,1x,i2.2),1x,a,1x,i2)'
    
!   debug formats
!   1. header/date
    write(debugform(1),'(a)')'(2(/1x,a),1x,i8.8//)'
    
!   2.  header for scalar variable
    write(debugform(2),'(a)')'(/t5,a,1x,a4,/t5,a)'
    
!   3.  header for multilevel variable for level NN
!       or wind speed and direction
    write(debugform(3),'(a)')'(/t5,a,1x,a4,1x,a,1x,i2,/t5,a)'
        
!   4.  format for writing debug data
    write(debugform(4),'(a)')'(t6,i8,1x,i2.2,t25,i2.2,t32,g13.6,t48,g13.6)'

!   5. no average calculated for scalar 
    write(debugform(5),'(a)')'(/t5,2(a,1x,i2,1x),a)'

!   6. no average calculated for multi-level variable
!       or wind set to calm
    write(debugform(6),'(a)')'(/t5,3(a,1x,i2,1x),a)'

!   7.  hourly average
    write(debugform(7),'(a)')'(/t5,a,1x,i2,1x,a,1x,g13.6)'

!   8.  format for writing data
    write(debugform(8),'(a)')'(t5,i8,1x,i2.2,t25,i2.2,1x,g13.6,1x,a1,1x,g13.6)'
    
!   9.  data
    write(debugform(9),'(a)')'(t6,i8,1x,i2.2,t25,i2.2,t32,f7.3,a1)'

!   set iswitch
    iswitch=2
    icalms=0
    if (debug)write(debug_unit,'(3(a))')' STAGE 1 ',trim(adjustl(pathid(ipath))),' SUB-HOURLY AVERAGING'
        
    d1: do d=1,nosdays
        if (.not. os_info(d)%sobs) cycle d1 !cycle d1 if no obs for the day
        write(adate,'(i8.8)')os_info(d)%osdate
        if (.not. noprint) write(output_unit,formstr(1))'Stage 1: Averaging sub-hourly',trim(adjustl(dattype(ipath-3))),&
            'data for month/day/year',adate(5:6),'/',adate(7:8),'/',adate(1:4),'LST'
        
        if (debug) write(debug_unit,debugform(1))'*****************************************************************************',&
            'DATE:',os_info(d)%osdate
        
    h1: do h=1,24
            if (.not. os_info(d)%have_obs(h)) cycle h1 !cycle h1 if no obs for the hour
            
            if (debug)then
!               get original date and hour for minutes
                write(debug_unit,'(/t5,a4,1x,i2.2)')'HOUR',h
                h_minus_1=h-1
                if (h_minus_1==0 .and. d > 1) then
                    h_minus_1=24
                    orig_date=os_info(d-1)%osdate
                else
                    orig_date=os_info(d)%osdate
                endif
            endif
            
    lv1:    do ilev=1,nlevel
!               process all variables in this loop except wind speed, wind direction, and standard deviations
!               wind speed will be processed first, then wind direction, then standard deviations
!               these are done separately because wind direction and standard deviations are dependent
!               on whether the wind speed is calm (below threshold).
!               also wind direction can be a scalar or vector mean and standard deviations are sums of squares
      v1:       do ivar1=1,nvars
                    ivar=read_index(ivar1)
              
!                   don't average below levels that are below the first level of a variable
!                   or above the variable's top level or is a standard deviation, wind direction, or wind speed
                    if ((ilev > osvars(ivar)%nlevels .and. osvars(ivar)%nlevels > 0) .or. &
                    (ilev > 1 .and. osvars(ivar)%nlevels == 0) .or. osvars(ivar)%firstlev > ilev .or. &
                    (ivar >= std_vars(1) .and. ivar <=std_vars(2)) .or. ivar == wind_vars(1) .or. ivar == wind_vars(2)) cycle v1
                    
                    
                    if (osvars(ivar)%nlevels == 0) then
                        writelev=.false. !don't write levels for single level data (PRCP, SLVP, etc.)
                    else
                        writelev=.true.
                    endif
!                   create a vname variable that for multi-level data, NN is replaced with level
!                   i.e. HTNN becomes HT01, HT02, etc.
!                   otherwise, vname is the full variable name        
                    if (osvars(ivar)%varname(3:4) == 'NN') then
                        write(vname,'(a2,i2.2)')osvars(ivar)%varname(1:2),ilev
                    else
                        vname=osvars(ivar)%varname
                    endif   
!					write variable name
                    if (debug) then
                        if (.not. writelev) then
                            write(debug_unit,debugform(2))'VARIABLE:',vname,&
                                'ACTUAL DATE/HOUR  MINUTE      VALUE        RUNNING TOTAL'
                        else
                            write(debug_unit,debugform(3))'VARIABLE:',vname,'LEVEL:',ilev,&
                                'ACTUAL DATE/HOUR  MINUTE      VALUE        RUNNING TOTAL'
                        endif
                        
                    endif
                        
!                   initialize values for nobs_hr1, and hrval
                    hrval=0.0_r8
                    nobs_hr1=nobs_hr
     m1:            do imin1=1,nobs_hr !minute loop
!                       calculate actual minute of hour
                        imin=(60/nobs_hr)*imin1
                        if (osvars(ivar)%laudit) then
                            iv=findvar(ivar)
                            call audit(os_subhr_vals(imin1,ivar1,ilev,h,d),ivar,d,h,imin,ilev,iv,iswitch) !audit
                        endif
!                       if missing, do not include for hourly average; take 1 away from nobs_hr1
!                       otherwise, add to running total variable, hrval
                        if (dabs(os_subhr_vals(imin1,ivar1,ilev,h,d)-real(osvars(ivar)%missval,r8)) < eps) then
                            nobs_hr1=nobs_hr1-1
                        else
                            hrval=hrval+os_subhr_vals(imin1,ivar1,ilev,h,d)
                        endif
                        if (debug)write(debug_unit,debugform(4))orig_date,h_minus_1,imin,os_subhr_vals(imin1,ivar1,ilev,h,d),hrval
!                       if reading dewpoint compare against temperature if applicable
                        if (checktempdew) then
                            if (ivar1 == osvars(dewpt_var)%readvar .and. dabs(os_subhr_vals(imin1,ivar1,ilev,h,d)-& 
                            real(osvars(ivar)%missval,r8)) >=eps .and. &
                            dabs(os_subhr_vals(imin1,osvars(temp_var)%readvar,ilev,h,d)-real(osvars(temp_var)%missval,r8)) >= &
                            eps .and. os_subhr_vals(imin1,osvars(temp_var)%readvar,ilev,h,d) < &
                            os_subhr_vals(imin1,ivar1,ilev,h,d)) os_tempstat(iswitch)=os_tempstat(iswitch)+1
                        endif
                    enddo m1
!                   now calculate hourly average if enough values
!                   if PRCP, SAMT, or PAMT, keep the total but issue a message about
!                   number of obs.  other variables set to missing
                    if (nobs_hr1 < min_obs) then !not enough values; write message
                        if (.not. writelev) then
                            if (ivar == 10 .or. ivar == 11 .or. ivar == 21 .and. nobs_hr1 > 0) then !keep total but issue message about # of nobs
                                
                                write(msg_unit,formstr(2))adjustl(pathid(ipath)),'Q70',modnam,os_info(d)%osdate,'HR',h,&
                                trim(adjustl(vname)),'# OF MINUTES',nobs_hr1,'<',min_obs,'MINUTES'
                                
                                osdata1a(ivar1,ilev,h,d)=hrval !set to total
                            else
                                write(msg_unit,formstr(2))adjustl(pathid(ipath)),'Q68',modnam,os_info(d)%osdate,'HR',h,&
                                trim(adjustl(vname)),'# OF MINUTES',nobs_hr1,'<',min_obs,'MINUTES FOR AVG; SET TO MISSING'
                                
                            endif
                            if (debug) write(debug_unit,debugform(5))'NUMBER OF NON-MISSING OBS',nobs_hr1,&
                                '< MINIMUM NUMBER OF OBS',min_obs,'NO AVERAGE CALCULATED'
                            
                        else
                            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q68',modnam,os_info(d)%osdate,'HR',h,&
                            trim(adjustl(vname)),'# OF MINUTES',nobs_hr1,'<',min_obs,'MINUTES FOR AVG LEVEL',ilev,'SET TO MISSING'
                           
                            if (debug) write(debug_unit,debugform(6))'NUMBER OF NON-MISSING OBS',nobs_hr1,&
                                '< MINIMUM NUMBER OF OBS',min_obs,'FOR LEVEL',ilev,'NO AVERAGE CALCULATED'
                            
                        endif
                    else !calculate average and assign to osdata1a
                        osdata1a(ivar1,ilev,h,d)=hrval/real(nobs_hr1,r8)
                        if (debug)write(debug_unit,debugform(7))'NUMBER OF NON-MISSING OBS ',nobs_hr1,' HOURLY AVERAGE ',&
                            osdata1a(ivar1,ilev,h,d)
                        
                    endif
                    

                enddo v1
                havedir=.false.
                havespeed=.false.
!               check wind speed, direction, and standard deviations
!               check wind speed first because if wind speed is less than
!               or equal to the threshold then direction and standard devations
!               for the minute are set to missing
!*************  WIND SPEED ******************************************     
 !              now check wind speed if read from data file
                if (osvars(wind_vars(2))%lread) then
                    ivar=wind_vars(2)
                    ivar1=osvars(wind_vars(2))%readvar
                    nobs_hr1=nobs_hr
                    hrval=0.0_r8
                    icalms=0
!                   only process if a valid level for this variable
                    if (osvars(ivar)%firstlev <= ilev .and. ilev <= osvars(ivar)%nlevels) then
                        havespeed=.true.
                        write(vname,'(a2,i2.2)')osvars(ivar)%varname(1:2),ilev
                        
                        if (debug)then
                            if (lvector)write(debug_unit,'(/t5,a)')'VECTOR AVERAGING USED'
                            write(debug_unit,debugform(3))'VARIABLE: ',vname,'LEVEL: ',ilev,&
                                'ACTUAL DATE/HOUR  MINUTE      VALUE        RUNNING TOTAL'
                            
                        endif
    m2:                 do imin1=1,nobs_hr
                            imin=(60/nobs_hr)*imin1
                            iv=findvar(ivar)
                            call audit(os_subhr_vals(imin1,ivar1,ilev,h,d),ivar,d,h,imin,ilev,iv,iswitch) !audit, winds are always audited
    !                       check for winds below the threshold
    !                       if below the threshhold speed, reset to half the threshold, set the minute to calm and increment
    !                       calms counter for level/hour
    !                       adjusted speed may be used for hourly average
                            athresh=''
                            if (dabs(os_subhr_vals(imin1,ivar1,ilev,h,d)-real(osvars(ivar)%missval,r8)) > eps) then 
                                if (os_subhr_vals(imin1,ivar1,ilev,h,d) < threshspeed) then
                                    os_windstats(iswitch,1)=os_windstats(iswitch,1)+1
                                    lcalm_obs(imin1,ilev,h,d)=.true.
                                    icalms=icalms+1
                                    os_subhr_vals(imin1,ivar1,ilev,h,d)=threshspeed/2.0_r8
                                    athresh='*'
                                endif
                                hrval=hrval+os_subhr_vals(imin1,ivar1,ilev,h,d)
                            else
                                nobs_hr1=nobs_hr1-1
                            endif 
                            if (debug)write(debug_unit,debugform(8))orig_date,h_minus_1,imin,os_subhr_vals(imin1,ivar1,ilev,h,d),&
                                athresh,hrval
                            
                        enddo m2
                        if (debug)write(debug_unit,'(/t5,a,f4.2)')'*=wind speed set to 1/2 speed threshold ',threshspeed
 !                      if too many calms, issue message and 
 !                      set the hourly wind speed to 0 and set the hourly calm flag to true   
                        if (icalms >= min_obs) then !not enough values
!                           initially calculate wind speed. in avg_wind, will check to see if need to reset to calm
!                           based on missing wind directions and number of calms
                            osdata1a(ivar1,ilev,h,d)=hrval/real(nobs_hr1,r8)
                            if (debug) then
                                write(debug_unit,'(/t5,a)')&
                                    'INITIAL WIND SPEED, MAY BE RESET TO CALM AFTER WIND DIRECTION CALCULATIONS'
                                write(debug_unit,debugform(7))'NUMBER OF NON-MISSING OBS',nobs_hr1,'HOURLY AVERAGE',&
                                    osdata1a(ivar1,ilev,h,d)
        
                            endif
!                       if missing issue message
                        elseif  (nobs_hr1 < min_obs) then !missing data
!                           note that missing counts for the hour will be done in os_audit
!                           and a message will be issued that hour is missing
                            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q68',modnam,os_info(d)%osdate,'HR',h,&
                            trim(adjustl(vname)),'# OF MINUTES',nobs_hr1,'<',min_obs,'MINUTES FOR AVG LEVEL',ilev,'SET TO MISSING'
                            
                            if (debug) write(debug_unit,debugform(6))'NUMBER OF NON-MISSING OBS',nobs_hr1,&
                                '< MINIMUM NUMBER OF OBS',min_obs,'FOR LEVEL',ilev,'NO AVERAGE CALCULATED'
                            
                        else !calculate hourly average if not vector averaging; if vector averaging will be calculated in avg_wind
                            if (.not. lvector) then
                                osdata1a(ivar1,ilev,h,d)=hrval/real(nobs_hr1,r8)
                                if (debug)write(debug_unit,debugform(7))'NUMBER OF NON-MISSING OBS',nobs_hr1,'HOURLY AVERAGE',&
                                    osdata1a(ivar1,ilev,h,d)
                                
                            endif
                        endif
!                       set lcalm_obs_hr to true if below threshold; needed if processing stage 2
!                       and stage 1 together
                        if (osdata1a(ivar1,ilev,h,d) < threshspeed) lcalm_obs_hr(ilev,h,d)=.true.
                    endif
                endif
!*************  WIND SPEED ****************************************** 
!*************  WIND DIRECTION ****************************************** 
 !              now check wind direction if read from data file
                if (osvars(wind_vars(1))%lread) then
                    ivar=wind_vars(1)
                    ivar1=osvars(wind_vars(1))%readvar
                    nobs_hr1=nobs_hr
                    hrval=0.0_r8
                    dircount=0
                    xdir=0.0_r8
                    ydir=0.0_r8
                    dirsum=0.0_r8
                  
                    if (lcalm_obs_hr(ilev,h,d)) osdata1a(ivar1,ilev,h,d)=0.0_r8 !set to 0 if a calm hour
                    if (osvars(ivar)%firstlev <= ilev .and. ilev <= osvars(ivar)%nlevels) havedir=.true.
                    if (osvars(ivar)%firstlev <= ilev .and. ilev <= osvars(ivar)%nlevels) then
                        
                        write(vname,'(a2,i2.2)')osvars(ivar)%varname(1:2),ilev
                        if (debug)then
                            if (lvector) then
                                write(debug_unit,'(/t5,a)')'VECTOR AVERAGING USED; X AND Y COMPONENTS IN RADIANS'
                                write(debug_unit,debugform(3))'VARIABLE:',vname,'LEVEL:',ilev,&
                                    'ACTUAL DATE/HOUR  MINUTE   SPEED    DIR       X-DIR   X-DIR RUNNING   Y-DIR   Y-DIR RUNNING'
                            else
                                write(debug_unit,debugform(3))'VARIABLE:',vname,'LEVEL:',ilev,&
                              'ACTUAL DATE/HOUR  MINUTE     DIR     PREV DIR (MINUTE-1)    DIFFERENCE   CORRECTION  RUNNING TOTAL'
                            endif
                        endif
    m3:                 do imin1=1,nobs_hr
                            athresh=''
                            imin=(60/nobs_hr)*imin1
!                           set wind direction to missing if calm minute
                            if (lcalm_obs(imin1,ilev,h,d)) then
                                os_subhr_vals(imin1,ivar1,ilev,h,d)=real(osvars(ivar)%missval,r8)
                                athresh='*'
                                if (debug)write(debug_unit,debugform(9))orig_date,h_minus_1,imin,&
                                    os_subhr_vals(imin1,ivar1,ilev,h,d),athresh
                            endif
                            iv=findvar(ivar)
                            call audit(os_subhr_vals(imin1,ivar1,ilev,h,d),ivar,d,h,imin,ilev,iv,iswitch) !audit, winds are always audited
                            if (dabs(os_subhr_vals(imin1,ivar1,ilev,h,d)- real(osvars(ivar)%missval,r8)) < eps) then
                                nobs_hr1=nobs_hr1-1
                            else
                                dircount=dircount+1
                                call calc_wind(dircount,d,h,imin1,ilev)
                            endif
                        enddo m3
                        if (debug)write(debug_unit,'(/t5,a,f4.2)')'*=direction set to missing for calms'
!                       if missing issue message
                        if  (nobs_hr1 < min_obs) then !missing data
!                           note that missing counts for the hour will be done in os_audit
!                           and a message will be issued that hour is missing
                            if (icalms >= min_obs) then !treat hour as calm
!                               set wind speed to 0 and issue message
                                osdata1a(osvars(wind_vars(2))%readvar,ilev,h,d)=0.0_r8
                                lcalm_obs_hr(ilev,h,d)=.true.
                                write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q65',modnam,os_info(d)%osdate,'HR',h,&
                                trim(adjustl(vname)),' # OF MINUTES',nobs_hr-icalms,'<',min_obs,'MINUTES FOR AVG LEVEL',ilev,&
                                    'RESET TO CALM (0)'
!                               set direction to 0
                                osdata1a(ivar1,ilev,h,d)=0.0_r8
                                if (debug)write(debug_unit,'(/t5,a)')'WIND DIRECTION AND SPEED SET TO CALM (0,0)'
                            else
                                write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q66',modnam,os_info(d)%osdate,'HR',h,&
                                trim(adjustl(vname)),' # OF MINUTES',nobs_hr1,'<',min_obs,'MINUTES FOR AVG LEVEL',ilev,&
                                    'SET TO MISSING'
                                
                                if (debug) write(debug_unit,debugform(6))'NUMBER OF NON-MISSING OBS',nobs_hr1,&
                                    '< MINIMUM NUMBER OF OBS',min_obs,'FOR LEVEL',ilev,'NO AVERAGE CALCULATED'
                            endif
                        else !calculate hourly average
                            call avg_wind(dircount,d,h,ilev)
                        endif
                    endif !end processing
                endif !end wind direction
!*************  WIND DIRECTION ****************************************** 
!               check to see if this level has both wind speed and direction
!               if both are read.  This is not the same as if wind speed
!               is a missing value and wind direction is a non-missing value, or vice
!               versa.  This is a check to see if one of the wind variables was read
!               for the level and the other is not, i.e. they are not paired
!               if one is present but the other is not, then issue a warning
                if (osvars(wind_vars(1))%lread .and. osvars(wind_vars(2))%lread .and. ((havespeed .and. .not. havedir) .or. &
                (.not. havespeed .and. havedir))) then
                    if (havespeed .and. .not. havedir) then
                        os_windstats(iswitch,3)=os_windstats(iswitch,3)+1
                    else
                        os_windstats(iswitch,2)=os_windstats(iswitch,2)+1
                    endif
                endif
!*************  STANDARD DEVIATIONS**************************************
!               now for standard deviations
    v2:         do ivar=std_vars(1),std_vars(2)
                    if (.not. osvars(ivar)%lread) cycle v2
!                   find the location in os_subhr_vals
                    ivar1=1
                    lfound=.false.
                    nobs_hr1=nobs_hr
                    hrval=0.0_r8
                    do while (ivar1 <= nvars .and. .not. lfound)
                        if (ivar == read_index(ivar1)) then
                            lfound=.true.
                        else
                            ivar1=ivar1+1
                        endif
                    enddo

                    if (ilev > osvars(ivar)%nlevels .or. osvars(ivar)%firstlev > ilev) cycle v2
                    write(vname,'(a2,i2.2)')osvars(ivar)%varname(1:2),ilev
                    if (debug)write(debug_unit,debugform(3))'VARIABLE:',vname,'LEVEL:',ilev,&
                        'ACTUAL DATE/HOUR  MINUTE      VALUE        RUNNING TOTAL'
    m4:             do imin1=1,nobs_hr
                        athresh=''
                        imin=(60/nobs_hr)*imin1
!                       set standard deviation to missing if calm minute
                        if (lcalm_obs(imin1,ilev,h,d))then
                            os_subhr_vals(imin1,ivar1,ilev,h,d)=real(osvars(ivar)%missval,r8)
                            athresh='*'
                        endif
                        if (osvars(ivar)%laudit) then
                            iv=findvar(ivar)
                            call audit(os_subhr_vals(imin1,ivar1,ilev,h,d),ivar,d,h,imin,ilev,iv,iswitch) !audit
                        endif
                        if (dabs(os_subhr_vals(imin1,ivar1,ilev,h,d)-real(osvars(ivar)%missval,r8)) < eps) then
                            nobs_hr1=nobs_hr1-1
                        else
                            hrval=hrval+os_subhr_vals(imin1,ivar1,ilev,h,d)*os_subhr_vals(imin1,ivar1,ilev,h,d)
                        endif
                        if (debug)write(debug_unit,debugform(8))orig_date,h_minus_1,imin,os_subhr_vals(imin1,ivar1,ilev,h,d),&
                            athresh,hrval
                    enddo m4
                    if (debug)write(debug_unit,'(/t5,a,f4.2)')'*=set to missing for calm winds'	
!                   if missing issue message
                    if  (nobs_hr1 < min_obs) then !missing data
!                           note that missing counts for the hour will be done in os_audit
!                           and a message will be issued that hour is missing
                        write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q68',modnam,os_info(d)%osdate,'HR',h,&
                                trim(adjustl(vname)),' # OF MINUTES',nobs_hr1,'<',min_obs,'MINUTES FOR AVG LEVEL',ilev,&
                                    'SET TO MISSING'
                    else !calculate hourly average
                        osdata1a(ivar1,ilev,h,d)=sqrt(hrval/real(nobs_hr1,r8))
                        if (debug)write(debug_unit,debugform(7))'NUMBER OF NON-MISSING OBS',nobs_hr1,'HOURLY AVERAGE',&
                            osdata1a(ivar1,ilev,h,d)
                    endif
                enddo v2
!*************  STANDARD DEVIATIONS**************************************
            enddo lv1
        enddo h1
    enddo d1
    

    return
    end subroutine avg_hr
!*********************************************************************************************************

    subroutine audit(rvar,ivar,d,h,imin,ilev,iv,iswitch)
!=========================================================================================================
!   SUBROUTINE QA
!   THIS SUBROUTINE AUDITS SUB-HOURLY OR HOURLY DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (AVG_HR, OS_AUDITHR)
!
!   INPUT ARGUMENTS
!
!   RVAR:           REAL VARIABLE BEING TESTED
!   IVAR:           VARIABLE INDEX IN OSVARS
!   D:              DAY INDEX
!   H:              HOUR
!   IMIN:           ACTUAL MINUTE OF HOUR
!   ILEV:           LEVEL INDEX
!   IV:             INDEX IN OS_AUDIT_INDEX 
!   ISWITCH:        INDICATOR OF HOURLY DATA (1) OR 
!                   SUB-HOURLY DATA(2) FOR COUNTS
!
!   Variable definitions
!      
!   Integer variables
!   orig_hr:        original hour for sub-hourly data
!   orig_date:      original date for sub-houry data
!   ivar:           variable index in osvars
!   imin:           actual minute of hour
!   ilev:           level index
!   d:              day index
!   h:              hour
!   iv:             index in os_audit_index 
!   iswitch:        indicator of hourly data (1) or 
!                   sub-hourly data(2) for counts
!   iyear:          year of observation
!   imonth:         month of observation
!   iday:           day of observation
!
!   Real variables
!   rvar:           real variable being tested
!
!   Logical variables
!   lgo:            logical dummy variable used for data_dates
!   lowviolate:     indicates variable violates lower BC
!   upviolate:      indicates variable violates upper BC
!   writemiss:      write that variable is missing for a level/hour
!   writelev:       write mesage for a level. this is so that single level variables such as PRCP
!
!   Character variables
!   vname:          variable name. based on osvars%varname, but if a multi-level data, replaces
!                   NN with level index, i.e. for level 1, WDNN becomes WD01.
!   formstr:        format for messages
!   lstr:           2-element character array for lower BC violations
!                   1='LB' 2='<' or '<='
!   ustr:           2-element character array for upper BC violations
!                   1='UB' 2='>' or '>='
!   lstr1:          value of lstr(2) with 'MUCH' or blank
!   ustr1:          value of ustr(2) with 'MUCH' or blank    
!   datestr:        date string for reporting; is original date for sub-hourly date
!                   or current date for hourly data
!   timestr:        time/minute string for sub-hourly data; hour for hourly data
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: data_dates,eps
    implicit none
    integer(kind=4), intent(in) :: iswitch,ivar,imin,ilev,d,h,iv
    integer(kind=4) :: orig_hr,iyear,imonth,iday
    integer(kind=8) :: orig_date
    real(kind=r8),intent(in) :: rvar
    logical :: lgo,lowviolate,upviolate,writemiss,writelev
    character(len=10) :: modnam='AUDIT'
    character(len=4) :: vname
    character(len=7) :: lstr(2),ustr(2)
    character(len=15) :: lstr1,ustr1
    character(len=70) :: formstr(4)
    character(len=8) :: adate=''
    character(len=30) :: datestr,timestr

    lstr(1)='LB'
    lstr(2)=''
    ustr(1)='UB'
    ustr(2)=''
    lowviolate=.false.
    upviolate=.false.
    writemiss=.false.
    
    if (osvars(ivar)%varname(3:4) == 'NN') then
        write(vname,'(a2,i2.2)')osvars(ivar)%varname(1:2),ilev
    else
        vname=osvars(ivar)%varname
    endif 
    
!   formats for messages
!   1.  missing for multi-level variable
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'4(a,1x),i2)'
    
!   2.  missing for scalar variable
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'

!   3. violation for multi-level variable
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a2,f9.1,1x,a,f9.1,1x,a,1x,i2)'

!   4.  violation for scalar variable
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a2,f9.1,1x,a,f9.1)'

!   calculate actual minute of hour
!    imin=(60/nobs_hr)*imin1
    
!   create a date and time string based on if hourly data or sub-hourly data
!   for writing to message file if missing, or bounds violations.
    if (iswitch == 1) then
        write(datestr,'(i8)')os_info(d)%osdate
        write(timestr,'(a,i2)')'HR ',h
    else
!       if any data is missing or exceeds bounds for sub-hourly data, report the original date
!       so that the user can check the original file easier.
        write(adate,'(i8)')os_info(d)%osdate
        read(adate,'(i4,3(i2))')iyear,imonth,iday
        orig_hr=h
        call data_dates(orig_hr,iday,imonth,iyear,1,osstart,osend,42,orig_date,lgo)
        write(datestr,'(a,i8)')'ORIGINAL DATE ',orig_date
        write(timestr,'(a,i2.2,1x,a,i2.2)')'HR ',orig_hr,'MINUTE ',imin
    endif

!   set default values for lstr and ustr
    if (osvars(ivar)%lincbound) then
        lstr(2)='<'
        ustr(2)='>'
    else
        lstr(2)='<='
        ustr(2)='>='
    endif
    
    if (osvars(ivar)%nlevels == 0) then
        writelev=.false.
    else
        writelev=.true.
    endif
    
!   increment total counter
    os_audit_counts(iv,ilev,iswitch,1)=os_audit_counts(iv,ilev,iswitch,1)+1
    
    if (dabs(rvar-missvals(iv)) < eps) then
        os_audit_counts(iv,ilev,iswitch,2)=os_audit_counts(iv,ilev,iswitch,2)+1
!       set writemiss; if sub-hourly data set to false,
!       otherwise, based on the variable
        if (iswitch == 1 .and. .not. osvars(ivar)%lnomiss) then
            writemiss=.true.
        else
            writemiss=.false.
        endif        
    else !check bounds, include a buffer of 0.01
        if (osvars(ivar)%lincbound) then !bounds are acceptable, included in range
            if (rvar < lowbound(iv) .and. &
            dabs(rvar-lowbound(iv)) > eps) lowviolate=.true.
            if (rvar > upbound(iv) .and. &
            dabs(rvar-upbound(iv)) > eps) upviolate=.true.
                            
        else !bounds are not acceptable
            if (rvar <=lowbound(iv)) lowviolate=.true.
            if (rvar >= upbound(iv)) upviolate=.true.
            
        endif      
    endif
    
    if (writemiss) then
        if (writelev) then
            write(msg_unit,formstr(1))adjustl(pathid(ipath)),'Q61',modnam,trim(adjustl(datestr)),trim(adjustl(timestr)),&
            trim(adjustl(vname)),'MISSING LEVEL',ilev
        else
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),'Q61',modnam,trim(adjustl(datestr)),trim(adjustl(timestr)),&
            trim(adjustl(vname)),'MISSING'
        endif
    endif
                                
    if (lowviolate) then !lower violation
        os_audit_counts(iv,ilev,iswitch,3)=os_audit_counts(iv,ilev,iswitch,3)+1
!       adjust lstr(2) if much lower than lower bound
        if (rvar < lowbound(iv)-0.5_r8*dabs(upbound(iv)-lowbound(iv))) then
            write(lstr1,'(a,1x,a)')'MUCH',trim(adjustl(lstr(2)))
        else
            lstr1=trim(adjustl(lstr(2)))
        endif
        if (writelev) then
             write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q62',modnam,trim(adjustl(datestr)),trim(adjustl(timestr)),&
            trim(adjustl(vname)),lstr(1),rvar,trim(adjustl(lstr1)),lowbound(iv),'LEVEL',ilev
        else
             write(msg_unit,formstr(4))adjustl(pathid(ipath)),'Q62',modnam,trim(adjustl(datestr)),trim(adjustl(timestr)),&
             trim(adjustl(vname)),lstr(1),rvar,trim(adjustl(lstr1)),lowbound(iv)
        endif
    endif !lower violation
    
    if (upviolate) then !upper violation
        os_audit_counts(iv,ilev,iswitch,4)=os_audit_counts(iv,ilev,iswitch,4)+1
!       adjust ustr(2) if much bigger than upper bound
        if (rvar > upbound(iv)+0.5_r8*dabs(upbound(iv)-lowbound(iv))) then
            write(ustr1,'(a,1x,a)')'MUCH',trim(adjustl(ustr(2)))
        else
            ustr1=trim(adjustl(ustr(2)))
        endif
        
        if (writelev) then
            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q63',modnam,trim(adjustl(datestr)),trim(adjustl(timestr)),&
            trim(adjustl(vname)),ustr(1),rvar,trim(adjustl(ustr1)),upbound(iv),'LEVEL',ilev
        else
            write(msg_unit,formstr(4))adjustl(pathid(ipath)),'Q63',modnam,trim(adjustl(datestr)),trim(adjustl(timestr)),&
            trim(adjustl(vname)),ustr(1),rvar,trim(adjustl(ustr1)),upbound(iv)
        endif
    endif !upper violation 

    return
    end subroutine audit
!*********************************************************************************************************

    subroutine calc_wind(dircount,d,h,imin1,ilev)
!=========================================================================================================
!   SUBROUTINE CALC_WIND
!   THIS SUBROUTINE ADDS TO RUNNING TOTAL OF SUMMED WIND DIRECTIONS, EITHER VECTOR OR SCALAR AVERAGING
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (AVG_HR)
!
!   INPUT ARGUMENTS 
!
!   DIRCOUNT:       DIRECTION NUMBER COUNTER
!   D:              DAY COUNTER
!   H:              HOUR COUNTER
!   IMIN1:          MINUTE INDEX (UP TO 12)
!   ILEV:           LEVEL INDEX
!
!   Variable definitions
!      
!   Integer variables
!   dircount:       direction number counter
!   ivar1:          wind direction read index 
!   d:              day counter
!   h:              hour counter
!   imin1:          minute index (up to 12)
!   imin:           actual minute of hour
!   ilev:           level index
!   h_minus_1:      original hour of observation; for processing purposes hour is 
!                   is incremented by 1, e.g. hour 1 minute 10 is assigned to hour 2
!                   h_minus_1 used for debugging
!   ivar1:          variable counter across variables read in
!   orig_date:      original date of observation
!
!   Real variables
!   xd:             x-component of wind direction (radians)
!   yd:             y-component of wind direction (radians)
!   diff:           difference between current direction and previous direction
!   corr:           correction factor
!   lastdir:        last observation's direction+direction+correction
!
!   Character variables
!   debugform:      debug message formats
!   modnam:         Function name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: d2r,debug
    use file_units, only: debug_unit
    implicit none
    integer(kind=4),intent(in) :: dircount,d,h,ilev,imin1
    integer(kind=4) :: imin,h_minus_1,ivar1
    integer(kind=8) :: orig_date
    real(kind=r8) ::xd,yd,diff,corr
    real(kind=r8) :: lastdir=0.0_r8
    character(len=80) :: debugform(3)
    character(len=10) :: modnam='CALC_WIND'
    save lastdir
    
    ivar1=osvars(wind_vars(1))%readvar
    
!   debug formats
!   1.  vector averaging
    write(debugform(1),'(a)')'(t6,i8,1x,i2.2,t25,i2.2,t32,f6.2,t40,f7.3,t50,f8.5,t61,f8.3,t74,f8.5,t85,f8.3)'
    
!   2.  scalar averaging with one value
    write(debugform(2),'(a)')'(t6,i8,1x,i2.2,t25,i2.2,t32,f7.3,t48,f7.3,t91,f10.4)'

!   3.  scalar averaging
    write(debugform(3),'(a)')'(t6,i8,1x,i2.2,t25,i2.2,t32,f7.3,t47,f8.3,t66,f9.4,t80,f8.3,t91,f10.4)'

!   for debugging
    h_minus_1=h-1
    if (h_minus_1==0 .and. d > 1) then
        h_minus_1=24
        orig_date=os_info(d-1)%osdate
    else
        orig_date=os_info(d)%osdate
    endif
    imin=(60/nobs_hr)*imin1

    if (lvector) then !calculate vector components based on speed and direction
        xd=-os_subhr_vals(imin1,osvars(wind_vars(2))%readvar,ilev,h,d)*dsin(os_subhr_vals(imin1,ivar1,ilev,h,d)*d2r)
        xdir=xdir+xd
        yd=-os_subhr_vals(imin1,osvars(wind_vars(2))%readvar,ilev,h,d)*dcos(os_subhr_vals(imin1,ivar1,ilev,h,d)*d2r)
        ydir=ydir+yd

        if (debug)write(debug_unit,debugform(1))orig_date,h_minus_1,imin,os_subhr_vals(imin1,ivar1,ilev,h,d),xd,xdir,yd,ydir

    else !use current method
        if (dircount == 1) then
            dirsum=os_subhr_vals(imin1,ivar1,ilev,h,d)
            lastdir=os_subhr_vals(imin1,ivar1,ilev,h,d)
            if (debug)write(debug_unit,debugform(2))orig_date,h_minus_1,imin,os_subhr_vals(imin1,ivar1,ilev,h,d),dirsum
        else
            diff=os_subhr_vals(imin1,ivar1,ilev,h,d)-lastdir
            if (diff < -180._r8) then
                corr=360.0_r8
            elseif (diff > 180._r8) then
                corr=-360._r8
            else
                corr=0.0_r8
            endif
            lastdir=lastdir+diff+corr
            dirsum=dirsum+lastdir
            if (debug)write(debug_unit,debugform(3))orig_date,h_minus_1,imin,os_subhr_vals(imin1,ivar1,ilev,h,d),lastdir,diff,&
                corr,dirsum
        endif
    endif    
    
    return
    end subroutine calc_wind
!*********************************************************************************************************

    SUBROUTINE avg_wind(dircount,d,h,ilev)
!=========================================================================================================
!   SUBROUTINE AVG_WIND
!   THIS SUBROUTINE CALCULATES THE SCALAR OR VECTOR AVERAGE OF WIND DIRECTION
!   IF A VECTOR AVERAGE, CALCULATE THE AVERAGE WIND SPEED TO VECTOR AVERAGE SPEED
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (AVG_HR)
!
!   INPUT ARGUMENTS 
!
!   DIRCOUNT:       DIRECTION NUMBER COUNTER
!   D:              DAY COUNTER
!   H:              HOUR COUNTER
!   ILEV:           LEVEL INDEX
!
!   Variable definitions
!      
!   Integer variables
!   dircount:       direction number counter
!   ivar1:          index of wind direction in data
!   d:              day counter
!   h:              hour counter
!   ilev:           level index
!   ivar1:          variable counter across variables read in
!
!   Real variables
!   xd:             x-component of wind direction (radians)
!   yd:             y-component of wind direction (radians)
!   dir1:           hourly average wind direction based on scalars
!   corr:           correction factor
!
!   Character variables
!   debugform:      debug message formats
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: d2r,debug
    use file_units, only: debug_unit
    implicit none
    integer(kind=4),intent(in) :: dircount,d,h,ilev
    integer(kind=4) :: ivar1
    real(kind=r8) ::xd,yd,dir1,corr
    character(len=80) :: debugform(2)
    character(len=10) :: modnam='AVG_WIND'
   
!   debug formats
!   1.  vector averaging
    write(debugform(1),'(a)')'(/t5,a,/t7,f8.3,t22,f8.3,t38,f7.3,t52,i2,t66,f7.3,t76,f6.2)'

!   2.  scalar averaging
    write(debugform(2),'(a)')'(/t5,a/t8,i2,t20,f7.3)'
    
    ivar1=osvars(wind_vars(1))%readvar
    
    corr=0.0_r8
    if (lvector) then !calculate average direction and speed
        xd=xdir/real(dircount,r8)
        yd=ydir/real(dircount,r8)
        if ((xd > 0.0_r8 .and. yd > 0.0_r8) .or. (xd < 0.0_r8 .and. yd > 0.0_r8))corr=180._r8
        if (xd < 0.0_r8 .and. yd < 0.0_r8) corr=0.0_r8
        if (xd > 0.0_r8 .and. yd < 0.0_r8) corr=360.0_r8
!       account for exactly east, west, north, or south winds
!       north or south wind
        if (xd == 0) then
            if (yd > 0.0_r8) then
                osdata1a(ivar1,ilev,h,d)=360.0_r8 !north
            else
                osdata1a(ivar1,ilev,h,d)=180.0_r8 !south
            endif
        endif
!       east or west wind
        if (yd == 0) then
            if (xd > 0.0_r8) then
                osdata1a(ivar1,ilev,h,d)=270.0_r8
            else
                osdata1a(ivar1,ilev,h,d)=90.0_r8
            endif
        endif
!       calculate wind direction, not directly north/south or east/west
        if (xd /= 0.0_r8 .and. yd /= 0.0_r8) osdata1a(ivar1,ilev,h,d)=datan(xd/yd)/d2r+corr
!       calculate wind speed
        osdata1a(osvars(wind_vars(2))%readvar,ilev,h,d)=sqrt((xd*xd)+(yd*yd))
        if (debug)write(debug_unit,debugform(1))'AVERAGE X-DIR   AVERAGE Y-DIR  CORRECTION  NUMBER OF OBS    DIRECTION  SPEED',&
            xd,yd,corr,dircount,osdata1a(ivar1,ilev,h,d),osdata1a(osvars(wind_vars(2))%readvar,ilev,h,d)
            
    else !use current method
        dir1=dirsum/real(dircount,r8)
        do while (dir1 > 360.0_r8 .or. dir1 < 0.0) 
            if (dir1 > 360.0_r8) then
                dir1=dir1-360.0_r8
            elseif (dir1 < 0.0_r8) then
                dir1=dir1+360.0_r8
            endif
        enddo
        osdata1a(ivar1,ilev,h,d)=dir1
        if (debug)write(debug_unit,debugform(2))'NUMBER OF OBS    DIR1',dircount,dir1
    endif   
    if (osdata1a(ivar1,ilev,h,d) < 0.05_r8 .and. osdata1a(ivar1,ilev,h,d) >= 0.0_r8) then
        osdata1a(ivar1,ilev,h,d)=360._r8
        if (debug)write(debug_unit,'(/t5,a)')'RESET WIND DIRECTION TO 360'
    elseif (osdata1a(ivar1,ilev,h,d) < 0.0_r8) then
        if (debug)write(debug_unit,'(/t5,a,1x,g13.6)')'NEGATIVE WIND DIRECTION',osdata1a(ivar1,ilev,h,d)
        osdata1a(ivar1,ilev,h,d)=360._r8+osdata1a(ivar1,ilev,h,d)
        if (debug)write(debug_unit,'(t5,a,1x,g13.6)')'ADD 360; NEW WIND DIRECTION:',osdata1a(ivar1,ilev,h,d)
    elseif (osdata1a(ivar1,ilev,h,d) > 360.0_r8) then
        if (debug)write(debug_unit,'(/t5,a,1x,g13.6)')'WIND DIRECTION > 360',osdata1a(ivar1,ilev,h,d)
        osdata1a(ivar1,ilev,h,d)=osdata1a(ivar1,ilev,h,d)-360._r8
        if (debug)write(debug_unit,'(t5,a,1x,g13.6)')'SUBTRACT 360; NEW WIND DIRECTION:',osdata1a(ivar1,ilev,h,d)
    endif
    
    return
    end subroutine avg_wind
!*********************************************************************************************************

    subroutine check_wind
!=========================================================================================================
!   SUBROUTINE CHECK_WIND
!   THIS SUBROUTINE CHECKS HOURLY WIND SPEEDS AGAINST THE THRESHOLD SPEED AND RESETS WIND DIRECTION
!   AND ANY STANDARD DEVIATIONS OR CHECKS FOR WIND DIRECTIONS ARE ZERO AND WIND SPEED ABOVE THRESHOLD.
!   THIS SUBROUTINE IS ONLY USED FOR HOURLY DATA THAT IS READ IN AS HOURLY DATA.  SUB-HOURLY WIND
!   CHECKS ARE DONE IN SUBROUTINE AVG_HR AND THIS SUBROUTINE IS NOT USED FOR THE SUBSEQUENT HOURLY
!   AVERAGES
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   ivar:		loop counter
!   ivar1:		variable index in data
!   d:			day counter
!   h:			hour counter
!   ilev:		level loop counter
!   iswitch:        indicator of hourly data (1) or sub-hourly data(2) for counts
!	
!   Logical variables
!   lfound:     logical variable denoting standard deviation
!               found in read_index
!   havespeed:  logical variable denoting that speed is read
!               for a level
!   havedir:    logical variable denoting that direction is
!               read for a level
!
!   Character variables
!   formstr:        format for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: eps
    implicit none
    integer(kind=4) :: ivar,ivar1,d,h,ilev,iswitch
    logical lfound,havespeed,havedir
    character(len=80) :: formstr
    character(len=10) :: modnam='CHECK_WIND'
    
!   format for message 
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,1x,a,1x,f4.2,1x,a,1x,i2)'
    
    iswitch=1
!   first do a check to see if there is any need to do hourly checks
!   check the minimum wind speed over all levels and hours
!   if the minimum value is below the threshold, then proceed
!   also check if there are wind directions of near-zero and wind speed above threshold
!   wind directions near zero and wind speed < threshold will be caught by the check for speed < threshold

    if (minval(osdata1a(osvars(wind_vars(2))%readvar,:,:,:)) < threshspeed .or. &
        (minval(osdata1a(osvars(wind_vars(1))%readvar,:,:,:)) < 0.05_r8 .and. &
        minval(osdata1a(osvars(wind_vars(2))%readvar,:,:,:)) >= threshspeed)) then
    d1: do d=1,nosdays
            if (.not. os_info(d)%sobs) cycle d1 
!           at least one obs for the day
    h1:     do h=1,24
                if (.not. os_info(d)%have_obs(h)) cycle h1
!               hour has an observation
    l1:         do ilev=1,nlevel
                    havespeed=.false.
                    havedir=.false.
                    if (ilev >= osvars(wind_vars(2))%firstlev .and. ilev <= osvars(wind_vars(2))%nlevels) then
!                       level has a wind speed 
                        havespeed=.true.
                        if (osdata1a(osvars(wind_vars(2))%readvar,ilev,h,d) >= 0.0_r8 .and. &
                            osdata1a(osvars(wind_vars(2))%readvar,ilev,h,d) < threshspeed .and. &
                            dabs(osdata1a(osvars(wind_vars(2))%readvar,ilev,h,d)-real(osvars(wind_vars(2))%missval,r8)) > eps) &
                            then
                            os_windstats(iswitch,1)=os_windstats(iswitch,1)+1
                            osdata1a(osvars(wind_vars(2))%readvar,ilev,h,d)=threshspeed/2.0_r8
                            lcalm_obs_hr(ilev,h,d)=.true.
                            write(msg_unit,formstr)adjustl(pathid(ipath)),'Q64',modnam,os_info(d)%osdate,'HR',h,'WIND SPEED <',&
                                threshspeed,'LEVEL',ilev
                        endif
                    endif
!                   now check direction
                    if (osvars(wind_vars(1))%lread .and. ilev >= osvars(wind_vars(1))%firstlev .and. ilev <= &
                        osvars(wind_vars(2))%nlevels) then
!                       level has a wind direction 
                        havedir=.true.
                        if (lcalm_obs_hr(ilev,h,d)) then
                            osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d) = real(osvars(wind_vars(1))%missval,r8)
                        else
                            if (dabs(osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d)-real(osvars(wind_vars(1))%missval,r8)) > eps) &
                                then
                                if (osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d) <= 0.05_r8 .and. &
                                    osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d) >= 0.0_r8) then
                                    osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d)=360.0_r8 !reset from 0 to 360
                                elseif (osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d)  < 0.0_r8) then !negative, add 360
                                    osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d)=&
                                        osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d)+360.0_r8  
                                elseif (osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d) > 360.0_r8) then ! > 360, subtract 360
                                    osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d)=&
                                        osdata1a(osvars(wind_vars(1))%readvar,ilev,h,d)-360._r8
                                endif
                            endif
                        endif
                    endif
!                   check to see if this level has both wind speed and direction
!                   if both are read.  This is not the same as if wind speed
!                   is a missing value and wind direction is a non-missing value, or vice
!                   versa.  This is a check to see if one of the wind variables was read
!                   for the level and the other is not, i.e. they are not paired
!                   if one is present but the other is not, then issue a warning
                    if (osvars(wind_vars(1))%lread .and. osvars(wind_vars(2))%lread .and. ((havespeed .and. .not. havedir) .or. &
                    (.not. havespeed .and. havedir))) then
                        
                        if (havespeed .and. .not. havedir) then
                            os_windstats(iswitch,3)=os_windstats(iswitch,3)+1
                        else
                            os_windstats(iswitch,2)=os_windstats(iswitch,2)+1
                        endif
                    endif
!                   now reset the standard deviations if read
    v1:             do ivar=std_vars(1),std_vars(2)
                        if (.not. osvars(ivar)%lread) cycle v1
                        
                        if (lcalm_obs_hr(ilev,h,d)) then
!                           find the location in osdata1a
                            ivar1=1
                            lfound=.false.
                            do while (ivar1 <= nvars .and. .not. lfound)
                                if (ivar == read_index(ivar1)) then
                                    lfound=.true.
                                else
                                    ivar1=ivar1+1
                                endif
                            enddo
!                           reset to missing
                            osdata1a(ivar1,ilev,h,d)=real(osvars(ivar)%missval,r8) 
                        endif
                    enddo v1
                enddo l1
            enddo h1
        enddo d1
    endif
  
    return
    end subroutine check_wind
!*********************************************************************************************************
    
    subroutine os_audithr
!=========================================================================================================
!   SUBROUTINE OS_AUDITHR
!   THIS SUBROUTINE AUDITS HOURLY DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   ivar:           variable index in osvars
!   ivar1:          variable index in osdata1a
!   d:              day counter
!   h:              hour counter
!   imin:           actual minute of hour (not needed)
!   ilev:			level loop counter
!   iv:             variable index in osdata1a of variable in os_audit_index
!   iswitch:        indicator of hourly data (1) or sub-hourly data(2) for counts
!
!   Logical variables
!   writelev:       write mesage for a level. this is so that single level variables such as PRCP
!
!   Character variables
!   adate:          character string of date (YYYYMMDD)
!   vname:          variable name. based on osvars%varname, but if a multi-level data, replaces
!                   NN with level index, i.e. for level 1, WDNN becomes WD01.
!   formstr:		formats for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit
    use main1, only: noprint
    implicit none
    integer(kind=4) :: ivar,ivar1,d,h,imin,ilev,iswitch,iv
    logical :: writelev
    character(len=8) :: adate=''
    character(len=4) :: vname
    character(len=70) :: formstr(2)
    character(len=10) :: modnam='OS_AUDITHR'
    
    iswitch=1
    
!   formats for messages
!   1.  message to screen to write out processing day
    write(formstr(1),'(a)')'(1x,a,1x,a,1x,a,1x,2(a2,a1),a4,1x,a)' 

!   2.  temperature < dewpoint
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,1x,a,1x,i2)'


    d1: do d=1,nosdays
        if (.not. os_info(d)%sobs) cycle d1 
!       day has at least one obs
        write(adate,'(i8.8)')os_info(d)%osdate
        if (.not. noprint)write(output_unit,formstr(1))'Stage 1: QA''ing',trim(adjustl(dattype(ipath-3))),&
            'data for month/day/year',adate(5:6),'/',adate(7:8),'/',adate(1:4),'LST'
            
    h1: do h=1,24
            if (.not. os_info(d)%have_obs(h)) cycle h1
!           hour has obs
    lv1:    do ilev=1,nlevel
      v:        do iv=1,nos_audit_vars
                    ivar1=os_audit_index(iv,1) !location in osdata1a
                    ivar=os_audit_index(iv,2) !location in osvars
                
!                   don't perform operations in variable loop for variables that are not
!                   multi-level, i.e. PRCP, SLVP, etc. or ilev exceeds the variables # of levels
!                   also don't perform calculations if level is below first level for variable
                    if ((ilev > osvars(ivar)%nlevels .and. osvars(ivar)%nlevels > 0) .or. &
                    (ilev > 1 .and. osvars(ivar)%nlevels==0) .or. ilev < osvars(ivar)%firstlev) cycle v

                    if (osvars(ivar)%nlevels == 0) then
                        writelev=.false.
                    else
                        writelev=.true.
                    endif
                            
        !           create a vname variable that for multi-level data, NN is replaced with level
        !           i.e. HTNN becomes HT01, HT02, etc.
                    if (osvars(ivar)%varname(3:4) == 'NN') then
                        write(vname,'(a2,i2.2)')osvars(ivar)%varname(1:2),ilev
                    else
                        vname=osvars(ivar)%varname
                    endif  
                    imin=0 !dummy for audit
                    call audit(osdata1a(ivar1,ilev,h,d),ivar,d,h,imin,ilev,iv,iswitch) !audit                               
                enddo v
!               if dewpoint read in and temperature read in, then compare
                if (checktempdew)then
                    if (osdata1a(osvars(temp_var)%readvar,ilev,h,d) < osdata1a(osvars(dewpt_var)%readvar,ilev,h,d)) then
                        os_tempstat(iswitch)=os_tempstat(iswitch)+1
                        write(msg_unit,formstr(2))adjustl(pathid(ipath)),'Q67',modnam,os_info(d)%osdate,'HR',h,'TT < DP LEVEL',ilev
                    endif
                endif
            enddo lv1
        enddo h1
    enddo d1
     
    return
    end subroutine os_audithr
!*********************************************************************************************************
      
    subroutine os_stage2
!=========================================================================================================
!   SUBROUTINE OS_STAGE2
!   THIS SUBROUTINE PERFORMS STAGE 2 PROCESSING FOR ONSITE DATA WHEN READING QAOUT DATA
!   WITHOUT STAGE 1
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE READ_INPUT (READINP)
!
!   Variable definitions
!   
!   Integer variables
!   iline:          line number
!   eof:            end of file indicator
!   iflag1:         I/O flag
!   nfields:	    number of fields in data line
!   nn:             index of string LEVELS in input line
!   ilevs1:         first level of multi-level variable
!   ilevs2:         top level of multi-level variable
!   i:              variable counter
!   i1:             variable counter
!   ivar:           variable index in osvars
!   ivar1:          variable index in osdata1a
!   ilev:           level index
!   ihr:            integer observation hour in GMT or LST
!                   will be reset to LST hours 1-24 if GMT
!   sdate:          integer observation date (YYYYMMDD)
!   minlev:         minimum first level allowed
!
!   Real variables
!   missvals:       missing values of audited variables
!   ht:             calculated ht
!   tv1,tv2:        virtual temperatures
!   dz:             ht at level ilev - ht at level ilev-1
!   delta:          temperature or speed difference
!   diff_dir:       wind direction differences
!   dew_dev:        dewpoint deviation
!
!   Logical variables
!   ldata:          indicates data header record found
!   lfound:         indicates variable found for do while loop 
!
!   Character variables
!   formstr:        format for messages
!   vlist:          variable list
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only : linelen,inpline1,ikey,ilen,keywrd,getdates,getloc,lpath
    use file_units, only: os_qaout_unit
    implicit none
    
    integer(kind=4) :: i,i1,eof,iflag1,iline,nfields,nn,ilevs1,ilevs2,ivar,ivar1,ilev,ihr
    integer(kind=4), parameter :: minlev=1
    integer(kind=8) :: sdate
    logical :: ldata,lfound
    character(len=10) :: modnam='OS_STAGE2'
    character(len=26) :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=26) :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=60) :: formstr(6)
    character(len=4), allocatable, dimension(:):: vlist(:)
    character(len=6) :: form1
    
!   1.  invalid number of fields for keyword
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),',i3,2(1x,a))'

!   2.  invalid date/time variable or data variable
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
    
!   3.  error reading data line
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i6)'
  
!   4.  error with first/last levels below 1 or exceeding max level
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a4,1x,a,1x,i2,1x,a,i3)'
  
!   5.  error reading first/last levels or invalid variable for first-last levels
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,a4)'

!   6.  overwater flag for onsite data
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'a)'
 
    write(form1,'(a2,i3,a1)')'(a',linelen,')'
    
!   read in the header from the QAOUT file to get station information and dates
    eof=0
    lbad=.false.
    ldata=.false.
    lfound=.false.
    iline=0
    do while (eof == 0 .and. .not. lbad)
        read(os_qaout_unit,form1,iostat=eof)inpline1
        iline=iline+1
        ilen=len_trim(inpline1)
        if (eof == 0) then
    l1:     do i1=1,len_trim(inpline1)
                i=index(lower,inpline1(i1:i1))
                if (i > 0) inpline1(i1:i1)=upper(i:i)
            enddo l1
            if (index(inpline1,'LOCATION') > 0) then !station information
                nfield=0
!               get the number of fields because it can vary depending on if GMT to LST or elevation included
    l2:         do i=1,len_trim(inpline1)
                    if (ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32) nfield=nfield+1
                enddo l2
                i=index(inpline1,'LOCATION')
                call getloc(i+8,osid,oslat,oslon,osgmt2lst,oselev,lgmt2lst,loselev)
            endif
!           set if delta temp heights listed
            if (index(inpline1,'DELTA_TEMP') > 0) then
                nfield=0
!               get the number of fields 
    l3:         do i=1,len_trim(inpline1)
                    if (ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32) nfield=nfield+1
                enddo l3
                if (nfield /= 4) then
                    write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                        'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                    lbad=.true.
                else
!                   allocate delta t heights array
                    if (.not. allocated(osdt_hts)) allocate(osdt_hts(maxdtind,2))
!                   initialize to missing values
                    osdt_hts=-9.0
                    i=index(inpline1,'DELTA_TEMP')
                    call delta_t_ht(i+10)
                endif
            endif
            if (index(inpline1,'THRESHOLD') > 0) then
                nfield=0
!               get the number of fields 
    l4:         do i=1,len_trim(inpline1)
                    if (ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32) nfield=nfield+1
                enddo l4
                if (nfield /=2) then
                    write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                        'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                    lbad=.true.
                else
                    i=index(inpline1,'THRESHOLD')      
                    call os_thresh(i+10)
                endif
            endif
            if (index(inpline1,'OVERWATER') > 0) then
                overland=.false.
                if (lpath(4)) then
                    lbad=.true.
                    write(writeunit,formstr(6))adjustl(pathid(ipath)),'E03',modnam,'OVERWATER FLAG INVALID FOR ONSITE DATA'
                endif
            endif
            if (index(inpline1,'OVERLAND') > 0) overland=.true.
            
            if (index(inpline1,'DATES') > 0) then
                i=index(inpline1,'DATES')
                nfield=7
                call getdates(i+5,osstart,osend,osdates)
            endif
            if (index(inpline1,'RANGE') > 0) then
                i=index(inpline1,'RANGE')
                nfield=6
                call os_range(i+5)
            endif
            
!           get the first and last levels of the multi-level variables
            if (index(inpline1,'FIRST-LAST LEVELS') > 0) then
                nn=index(inpline1,'LEVELS')
!               start with height
                ivar=height_var
                lfound=.false.
                do while (ivar <= nosvars .and. .not. lfound)
                    if (trim(adjustl(inpline1(1:4))) == trim(adjustl(osvars(ivar)%varname))) then
                        lfound=.true.
!                       get first and last levels
                        read(inpline1(nn+6:ilen),*,iostat=iflag1)ilevs1,ilevs2
                        if (iflag1 == 0) then
                            if (ilevs1 < 1) then
                                write(writeunit,formstr(4))adjustl(pathid(ipath)),'E05',modnam,osvars(ivar)%varname,'FIRST',&
                                    ilevs1,'<',minlev
                                lbad=.true.
                            elseif (ilevs1 > maxlevel) then
                                write(writeunit,formstr(4))adjustl(pathid(ipath)),'E05',modnam,osvars(ivar)%varname,'FIRST',&
                                    ilevs1,'>',maxlevel
                                lbad=.true.
                            else
                                osvars(ivar)%firstlev=ilevs1
                            endif
                            if (ilevs2 < 1) then
                                write(writeunit,formstr(4))adjustl(pathid(ipath)),'E05',modnam,osvars(ivar)%varname,'LAST',&
                                    ilevs2,'<',minlev
                                lbad=.true.
                            elseif (ilevs2 > maxlevel) then
                                write(writeunit,formstr(4))adjustl(pathid(ipath)),'E05',modnam,osvars(ivar)%varname,'LAST',&
                                    ilevs2,'>',maxlevel
                                lbad=.true.
                            else
                                osvars(ivar)%nlevels=ilevs2
                            endif
                        else
                            write(writeunit,formstr(5))adjustl(pathid(ipath)),'E05',modnam,&
                                'ERROR READING FIRST AND/OR LAST LEVELS FOR',osvars(ivar)%varname
                        endif
                    else
                        ivar=ivar+1
                    endif
                enddo
                if (.not. lfound) then
                    write(writeunit,formstr(5))adjustl(pathid(ipath)),'E05',modnam,'INVALID VARIABLE FOR FIRST-LAST LINE',&
                        inpline1(1:4)
                        lbad=.true.
                endif
            endif
            
!           get the variables that are being read
!           should be in the order as osvars, if not, issue an
!           error, with exception of HT which is first if heights read in stage 1
            if (index(inpline1,'DATE       HR     LEV') > 0) then !variable list
                ldata=.true.
!               get the number of fields 
                nfields=0
    l5:         do i=1,len_trim(inpline1)
                    if (ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32) nfields=nfields+1
                enddo l5
                allocate(vlist(nfields-3))
                
                read(inpline1(22:len_trim(inpline1)),*)(vlist(ivar),ivar=1,nfields-3)
            
!               check the input variables for valid variables
!               dates/times are not valid
                allocate(ext_index(nfields-3))
    v1:         do ivar1=1,nfields-3
                    ivar=1
                    lfound=.false.
                    do while (ivar <= nosvars .and. .not. lfound)
                        if (trim(adjustl(vlist(ivar1))) == trim(adjustl(osvars(ivar)%varname))) then
                            lfound=.true.
                            osvars(ivar)%lread=.true.
                            ext_index(ivar1)=ivar
                        else
                            ivar=ivar+1
                        endif
                    enddo
!                   date/time found or invalid variable name
                    if (lfound .and. ivar <= 5) then
                        write(writeunit,formstr(2))adjustl(pathid(ipath)),'E59',modnam,'INVALID DATE/TIME VARIABLE IN',&
                            trim(adjustl(dattype(ipath-3))),'QAOUT FILE:',trim(adjustl(osvars(ivar)%varname))
                        lbad=.true.
                    elseif (.not. lfound) then
                         write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'INVALID VARIABLE IN',&
                             trim(adjustl(dattype(ipath-3))),'QAOUT FILE:',trim(adjustl(vlist(ivar1)))
                        lbad=.true.
                    endif
                enddo v1
            endif
!           get max number of levels, nlevel will be set here for stage 2, stage 1 will
!           be set in os_proc
            if (ldata .and. index(inpline1,'DATE       HR     LEV') == 0) then !this is data
                read(inpline1,*,iostat=iflag1)sdate,ihr,ilev
                if (iflag1 == 0) then
                    if (ilev > nlevel) nlevel=ilev
                else
                    lbad=.true.
                    write(msg_unit,formstr(3))adjustl(pathid(ipath)),'E55',modnam,'ERROR READING DATA LINE:',iline
                endif
            endif
        endif
    enddo
    rewind(os_qaout_unit)
    if (lbad) lbados=.true.
    if (allocated(vlist)) deallocate(vlist)
    

    return
    end subroutine os_stage2
!*********************************************************************************************************
      
    subroutine read_ext
!=========================================================================================================
!   SUBROUTINE READ_EXT
!   THIS SUBROUTINE READS THE QAOUT FILE FOR ONSITE OR PROG DATA
!   WITHOUT STAGE 1
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PROC)
!
!   Variable definitions
!   
!   Integer variables
!   i:              variable counter
!   i1:             variable counter
!   eof:            end of file indicator
!   iflag1:         I/O flag
!   iline:          line counter
!   savelen:        previous level
!   iv1:            variable index in upvar of variable in up_audit_index
!   ivar2:          variable index for osdata1a
!   ivar:           variable index in osvars
!   ivar1:          variable index in osdata1a
!   ilev:           level index
!   iday1:          counter of number of days in data period for arrays
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!   ihr:            integer observation hour in GMT or LST
!                   will be reset to LST hours 1-24 if GMT
!   savehr:         previous observation's hour
!   sdate:          integer observation date (YYYYMMDD)
!   savedate:       previous observation's date (YYYYMMDD)
!
!   Real variables
!   rosvars:        real data varaibles read in
!
!   Logical variables
!   ldata:          indicates data header record found
!   lfound:         indicates variable found for do while loop 
!   lgo:            data within data window set by osstart and osend
!
!   Character variables
!   lower:          lower case letter string
!   upper:          upper case letter string
!   formstr:        format for messages
!   form1:          format for reading header lines
!   adate:          character string of date (YYYYMMDD)
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only : linelen,inpline1,ilen,data_dates,numdays,noprint,istage,eps
    use file_units, only: os_qaout_unit
    implicit none
    integer(kind=4) :: i,i1,eof,ivar2,iflag1,iline,savelev,ivar,ivar1,ilev,iday1,iyear,imonth,iday,ihr,savehr !,nfields
    integer(kind=8) :: sdate
    integer(kind=8) :: savedate=0
    real(kind=r8) :: rosvars(nvars)
    logical :: ldata,lfound,lgo !,ldup

    character(len=10) :: modnam='READ_EXT'
    character(len=26) :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=26) :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!    character(len=4) :: vlist(nvars)
    character(len=6) :: form1
    character(len=60) :: formstr(3)
    character(len=8) :: adate
    
!   formats for messages
!   1.  message to screen to write out processing day
    write(formstr(1),'(a)')'(1x,a,1x,i1,3(a,1x),2(a2,a1),a4,1x,a)' 
  
!   2.  potential duplicate
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,i2.2)'
    

!   3.  error reading line
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i6)'

    write(form1,'(a2,i3,a1)')'(a',linelen,')'
    
!   read in the header from the QAOUT file to get station information and dates
    eof=0
    lbad=.false.
    iline=0
    lfound=.false.
    ldata=.false.
    iday1=0
    
    do while (eof == 0 .and. .not. lbad)
        read(os_qaout_unit,form1,iostat=eof)inpline1
        ilen=len_trim(inpline1)
        iline=iline+1
        if (eof == 0) then
!           once reading numerical data skip the loop to capitalize to save time
            if (.not. ldata) then
    l1:         do i1=1,len_trim(inpline1)
                    i=index(lower,inpline1(i1:i1))
                    if (i > 0) inpline1(i1:i1)=upper(i:i)
                enddo l1
            endif
!           get the variables that are being read
            if (index(inpline1,'DATE       HR     LEV') > 0) then
                ldata=.true.
!               get the number of fields 
!                nfields=0
    l5:         do i=1,len_trim(inpline1)
                    if (ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32) nfield=nfield+1
                enddo l5
                
            endif
                
!           read in data
            if (ldata .and. index(inpline1,'DATE       HR     LEV') == 0) then !this is data
                read(inpline1,*,iostat=iflag1)adate,ihr,ilev,(rosvars(ivar1),ivar1=1,nvars)
                if (iflag1 == 0) then
                    nos(1)=nos(1)+1
!                   check to see if in the data window
                    read(adate,'(i4,2(i2))')iyear,imonth,iday
                    call data_dates(ihr,iday,imonth,iyear,osgmt2lst,osstart,osend,51,sdate,lgo)
                    if (lgo) then 
                        if (ilev == 1) then
                            if (sdate /= savedate) iday1=iday1+1
                            nos(2)=nos(2)+1 !extracted obs in window; only increment for 1st level
                        endif
                        losobs(iday1)=.true.
                        if (sdate == savedate) then
                            if (ihr == savehr .and. ilev == savelev) then
                                nos(4)=nos(4)+1
                                write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I62',modnam,&
                                    'POTENTIAL DUPLICATE OBSERVATION FOR DATE:',sdate,'HR',ihr
                            else
!                               write to screen the date and hour being processed if NOPRINT not specfied on JOB pathway
                                nos(3)=nos(3)+1 !increment only if a different hour, this will eliminate duplicates in the final counts
                                nomisshr(iday1,ihr)=.true. !hour has an observation
                            endif
                        else
!                           write to screen the date and hour being processed if NOPRINT not specfied on JOB pathway
                            if (.not. noprint) write(output_unit,formstr(1))'Stage',istage,': Extracting',&
                                trim(adjustl(dattype(ipath-3))),'data for month/day/year',adate(5:6),'/',adate(7:8),'/',&
                                adate(1:4),'LST' !,ihr
!                           new day, nos(3)=1
                            nos(3)=1
                            iday1=numdays(osstart,sdate)!increment day counter for updata1
                            nomisshr(iday1,ihr)=.true. !hour has an observation
                        endif                         
                        
!                       assign to the osdata1a array
    v2:                 do ivar1=1,nvars
                            ivar=ext_index(ivar1)
                            lfound=.false.
                    
!                           now find the variable's index for osdata1a
!                           don't need to worry about lfound being true because
!                           os_stage2 would catch invalid variable names and 
!                           AERMET would have aborted before now.
                            ivar2=1
                            lfound=.false.
                            do while (ivar2 <= nvars .and. .not. lfound)
                                if (read_index(ivar2) == ivar) then
                                    lfound=.true.
                                else
                                    ivar2=ivar2+1
                                endif
                            enddo
                            osdata1a(ivar2,ilev,ihr,iday1)=rosvars(ivar1)
!                           if wind speed is read, set lcalm_obs_hr to true for this observation  
!                           account for fact missing indicator for wind speed could be negative
                            if (ivar==wind_vars(2) .and. osdata1a(ivar2,ilev,ihr,iday1) < threshspeed .and. &
                                dabs(osdata1a(ivar2,ilev,ihr,iday1)-real(osvars(wind_vars(2))%missval,r8)) > eps)&
                                lcalm_obs_hr(ilev,ihr,iday1)=.true.
                        enddo v2
                    endif
                    savehr=ihr
                    savedate=sdate
                    savelev=ilev
                else
                    lbad=.true.
                    write(msg_unit,formstr(3))adjustl(pathid(ipath)),'E55',modnam,'ERROR READING DATA LINE:',iline
                endif
            endif
        endif
    enddo

    if (lbad) lbados=.true.
    

    return
    end subroutine read_ext
!*********************************************************************************************************
      
    subroutine check_hts
!=========================================================================================================
!   SUBROUTINE CHECK_HTS
!   THIS SUBROUTINE READS OSDATA1A AND CHECKS THAT HEIGHTS ARE IN INCREASING VALUE WITH INCREASING
!   LEVELS
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (OS_PROC)
!
!   Variable definitions
!
!   Integer variables
!   htvar:      value for readvar for heights.
!   d:              day counter
!   h:              hour counter
!   imin1:          minute index (up to 12)
!   imin:           actual minute of hour
!   ilev:           level loop counter
!
!   Character variables
!   formstr:        formats for messages
!   timestr:        time/date string
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: eps
    implicit none
    integer(kind=4) :: htvar,d,h,ilev,imin,imin1
    character(len=12) :: timestr
    character(len=80) :: formstr(2)
    character(len=10) :: modnam='CHECK_HTS'
    
!   formats for messages
!   1.  heights not in order
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,2(1x,a,1x,i2,1x,f8.3),1x,a)'

!   2.  reached limit of bad height checks
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a)'

!   after reading the data, and heights are read, make
!   sure the heights increase with increasing levels
!   if not, issue an error and abort. do this check
!   even if the osheights keyword is used as it could
!   indicate bad data or bad read/format statements.
    htvar=osvars(height_var)%readvar
    
    if (.not. lbad .and. osvars(height_var)%lread .and. nos(2) > 0) then
    d1: do d=1,nosdays
            if(.not. losobs(d)) cycle d1
    h1:     do h=1,24
                if (.not. nomisshr(d,h))cycle h1
    lv1:        do ilev=2,nlevel
                    if (ilev < osvars(height_var)%firstlev .or. ilev > osvars(height_var)%nlevels) cycle lv1
                    if (osvars(5)%lread) then !sub-hourly data
    m1:                 do imin1=1,nobs_hr
                            if (dabs(os_subhr_vals(imin1,htvar,ilev,h,d)-real(osvars(height_var)%missval,r8)) > eps .and. &
                            os_subhr_vals(imin1,htvar,ilev,h,d) <= os_subhr_vals(imin1,htvar,ilev-1,h,d)) then
                                nbadhts=nbadhts+1
                                lbad=.true.
                                if (nbadhts <= 100) then
                                    imin=(60/nobs_hr)*imin1
                                    write(timestr,'(a2,1x,i2.2,1x,a3,1x,i2.2)')'HR',h,'MIN',imin
                                    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E58',modnam,'DATE',os_date(d),&
                                    trim(adjustl(timestr)),'HT LEVEL',ilev,os_subhr_vals(imin1,htvar,ilev,h,d),'M <=  HT LEVEL ',&
                                        ilev-1,os_subhr_vals(imin1,htvar,ilev-1,h,d),'M'
                                    
                                elseif (nbadhts == 101) then
                                   write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W60',modnam,&
                                       'REACHED LIMIT OF BAD HEIGHT CHECKS'
                                endif
                            endif
                        enddo m1
                    else !hourly
                        if (dabs(osdata1a(htvar,ilev,h,d)-real(osvars(height_var)%missval,r8)) > eps .and. &
                        osdata1a(htvar,ilev,h,d) <= osdata1a(htvar,ilev-1,h,d)) then
                            nbadhts=nbadhts+1
                            lbad=.true.
                            if (nbadhts <= 100) then
                                write(timestr,'(a2,1x,i2.2)')'HR',h
                                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E58',modnam,'DATE',os_date(d),&
                                trim(adjustl(timestr)),'HT LEVEL',ilev,osdata1a(htvar,ilev,h,d),'M <=  HT LEVEL ',&
                                    ilev-1,osdata1a(htvar,ilev-1,h,d),'M'
                            elseif (nbadhts == 101) then
                                write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W60',modnam,'REACHED LIMIT OF BAD HEIGHT CHECKS'
                            endif
                        endif
                    endif
                enddo lv1
            enddo h1
        enddo d1
    endif    
    
    if (lbad) lbados=.true.
    
    return
    end subroutine check_hts
!*********************************************************************************************************
      
    integer(kind=4) function findvar(ivar)
!=========================================================================================================
!   FUNCTION FINDVAR
!   THIS FUNCTION FINDS THE INDEX IN OS_AUDIT_INDEX OF VARIABLE IVAR
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE ONSITE (AVG_HR)
!
!   INPUT ARGUMENTS 
!
!   IVAR:			INDEX OF VARIABLE IVAR IN OSVARS
!
!   OUTPUT
!
!   FINDVAR:        INDEX OF IVAR IN OS_AUDIT_INDEX
!
!   Variable definitions
!
!   Integer variables
!   ivar:			index of variable ivar in osvars
!
!   Logical variables
!   lfound:         ivar found
!   Character variables
!   formstr:        formats for messages
!   timestr:        time/date string
!   modnam:         Subroutine name
!========================================================================================================= 
    implicit none
    integer(kind=4), intent(in) :: ivar
    logical :: lfound
    
    lfound=.false.
    findvar=1
    do while (findvar < nos_audit_vars .and. .not. lfound)
        if (ivar == os_audit_index(findvar,2)) then
            lfound=.true.
        else
            findvar=findvar+1
        endif
    enddo
    return
    end function findvar
!*********************************************************************************************************
    end module onsite