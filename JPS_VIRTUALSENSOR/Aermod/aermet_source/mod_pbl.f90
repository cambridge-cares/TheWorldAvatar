    module pbl
!=========================================================================================================
!   MODULE PBL
!   THIS MODULE CONTAINS COMMON VARIABLES AND SUBROUTINES NEEDED THROUGOUT AERMET PROCESSING 
!   TO PROCESS STAGE 2 INPUTS AND CALCULATE PBL PARAMETERS (METPREP PATHWAY)
!
!   MODIFIED MAY 31, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   USED BY:        AERMET, MODULES READ_INPUT, REPORTS
!
!   Variable definitions
!
!   Integer variables
!   pblstart:       processing start date (YYYMMDD)
!                   start hour assumed to be 1
!   pblend:         processing end date (YYYYMMDD)
!                   end hour assumed to be 24
!   pbldates:       2x3 array of start month, day, year and end month,
!                   day and year.  1st dim is the start (1) or end (2)
!                   2nd dim is year (1), month (2), and day (3)
!   npbldays:       number of days in processing period
!   pblgmt2lst:     conversion from Greenwich Mean Time (GMT) to
!                   local standard time (LST) for source.
!                   example:  GMT to eastern time zone (EST) is 5
!                   do not have to account for daylight savings time
!                   needed when processing onsite data with mixing heights
!                   and no upper air sounding data included.
!   irnd:           matrix of 24x366 of random numbers to add to NWS
!                   standard hourly obs.
!   irnd1:			loop counter for irnd variable data setup
!   my_sounding:    sounding time to use, set in pbl_proc
!   target_hr_gmt:  target time for sounding to use in GMT
!   snd_hr_gmt:     times in GMT before and after target hour to find closest
!                   sounding to target hour.
!   nsectors1:      number of surface roughness sectors for the primary site
!   nfreq1:         frequency of surface characteristics for the primary site
!                   this can be 1 for annual, 4 for seasonal and 12 for monthly
!   nsectors2:      number of surface roughness sectors for the secondary site
!   nfreq2:         frequency of surface characteristics for the secondary site
!                   this can be 1 for annual, 4 for seasonal and 12 for monthly  
!   snding_win:     2-element array to select hours before and after the preferred
!                   sounding. default is -1 and 1 hour (1 hour before and after)
!                   when UAWINDOW is used with UASELECT SUNRISE option, these
!                   are the number of hours before and after sunrise.
!   sectors1:       array of surface roughness sectors for primary site
!   sectors2:       array of surface roughness sectors for secondary site
!   npblkeys:       number of keywords needed for METPREP
!   minsec:         minimum number of sectors allowed for surface characteristics (1)
!   maxsec:         maximum number of sectors allowed for surface characteristics (12)
!   nsc_years:      number of surface characteristics years based on pblstart and pblend
!   nsc_files1:		number of surface characteristics files for primary site.
!   nsc_files2:		number of surface characteristics files for secondary site.
!   sc_years:       Years for surface characteristics
!   nfreq1:         frequency of surface characteristics by year for primary site
!   nsectors1:      number of sectors for surface charactersitics by year for primary site
!   nfreq2:         frequency of surface characteristics by year for secondary site
!   nsectors2:      number of sectors for surface charactersitics by year for secondary site
!   pfl_nlevels:	number of profile levels for each hour within the data period; can vary depending on if
!                   hour is site-specific (prognostic) or NWS
!   windsrc:        source of wind data for each hour (0=no data,1=site-specific or prognostic, 2= NWS)
!   tempsrc:        source of surface temperature data for each hour 
!                   (0=no data,1=site-specific or prognostic, 2=NWS,3=substituted or interpolated)
!   cloudsrc:		source of cloud cover data for each hour 
!                   (0=no data,1=site-specific or prognostic, 2=NWS,3=substituted or interpolated)
!   iup:            day index in upper air data for given day in pbl processing; iup may not have
!                   same value as day  index of pbl processing if data periods for upper air and
!                   pbl processing are not exactly the same period.
!   isnd:           index of sounding to use from upper air data	
!   isf:            day index in surface data for given day in pbl processing; isf may not have
!                   same value as day  index of pbl processing if data periods for surface and
!                   pbl processing are not exactly the same period.
!   isf1:			day index in AERMINUTE data for given day in pbl processing; isf1 may not have
!                   same value as day  index of pbl processing if data periods for AERMINUTE and
!                   pbl processing are not exactly the same period.
!   ios:            day index in onsite(prognostic) data for given day in pbl processing; ios may not have
!                   same value as day  index of pbl processing if data periods for onsite(prognostic) and
!                   pbl processing are not exactly the same period.
!   pfl_levels:     number of levels for PROFILE data; initialized to 1 for NWS but reset later
!   nlevs:          number of levels to process for sounding data
!   nbplvars:       number of PBL variables
!   nplkeys:        number of PBL keywords
!   minsec:         minimum number of surface characteristics sectors allowed
!   maxsec:         maximum number of surface characteristics sectors allowed
!   ihem:           2-element array (1=primary, 2=secondary)) of hemisphere based on 
!                   latitude of primary or secondary site (1=north, 2=south); is initialized
!                   to 1 (northern hemisphere) and reset in pbl_test
!   pbl_windstats:  3-element array reporting:
!                   total # of calms encountered in winds subroutine
!                   number of calms due to ASOS threshold
!                   number of variable winds
!   n_cloud_sub:    number of hours with cloud cover substitution (interpolation); this is
!                   is not the cloud cover substitution based on SUBNWS
!   n_temp_sub:     number of hours with temperature substitution (interpoloation)
!   n_conv_miss:    number of days with no convective heights
!   pbl_obs:        number of 1: upper air obs, 2: NWS, 3: ONSITE/PROG, 
!                   4: ASOS 1-minute obs per day for dates covered by pbldates
!
!   Real variables
!   pbllat:         latitude of source
!   pbllon:         longitude of source
!   angle:          solar angle for given hour
!   crit_ang:       critical angle for determining stability
!   inso:           insolation for given hour
!   nrad:           net radiation for given hour
!   cp:             specific heat of air at constant pressure (1004)
!   pi:             3.14159
!   vonk:           von-Karmen constant(0.4)
!   mech_max:       maximum mechanical mixing height allowed
!   conv_max:       maximum convective mixingh height allowed
!   sectors1:		beginning and ending sectors for each sector by year for primary site
!   sectors2:		beginning and ending sectors for each sector by year for secondary site	
!   tsectors:       temporary beginning and ending sectors for each sector by year
!   talbedo:        temporary array of albedo by sector by year
!   tbowen:         temporary array of Bowen ratio by sector by year
!   tzo:            temporary array of surface roughness by sector by year
!   up_sunrise:     sunrise times at upper air location by day
!   up_sunset:      sunset times at upper air location by day
!   local_sunrise:	sunrise times at LOCATION coordinates by day
!   local_sunset:	sunset times at LOCATION coordinates by day
!   sun_ang:		sun angle for each hour of the day
!   rho:            density for each hour of the day
!   theta:          potential temperature at each sounding level for sounding of the day
!   tempk:          temperature at each sounding level for sounding of the day
!   pres:           pressure at each sounding level for sounding of the day
!   ht:             height at each sounding level for sounding of the day
!
!   Logical variables
!   lpblkeywords:      array denoting if keywords found
!   note ikey below is the index of the keyword in the keywrd array in module main1
!   1:              denotes XDATES keyword found (ikey=9)
!   2:              denotes LOCATION keyword found (ikey=10)
!   3:              denotes FREQ_SECT keyword found (ikey=18)
!   4:              denotes SITE_CHAR keyword found (ikey=19)
!   5:              denotes SECTOR keyword found (ikey=20)
!   6:              denotes OUTPUT keyword found (ikey=21)
!   7:              denotes MODEL keyword found (ikey=22)
!   8:              denotes METHOD keyword found (ikey=23)
!   9:              denotes UAWINDOW keyword found (ikey=24)
!   10:             denotes PROFILE keyword found (ikey=26)
!   11:             denotes NWS_HGT keyword found (ikey=28)
!   12:             denotes FREQ_SECT2 keyword found (ikey=29)
!   13:             denotes SITE_CHAR2 keyword found (ikey=30)
!   14:             denotes SECTOR2 keyword found (ikey=31)
!   15:             denotes AERSURF keyword found (ikey=33)
!   16:             denotes AERSURF2 keyword found (ikey=34)
!   17:             denotes THRESH_1MIN keyword found (ikey=35)
!   18:             denotes DATA keyword found (ikey=4)
!                   this keyword is now obsolete but the user is warned that it is there
!                   in a future update to AERMET, this will not be recognized and if present
!                   will be an error.
!
!   litem:          denotes item or process if found associated with METHOD keyword
!   1:              denotes WIND_DIR item found
!   2:              denotes REFLEVEL item found
!   3:              denotes STABLEBL item found
!   4:              denotes ASOS_ADJ item found
!   5:              denotes UASELECT item found
!
!   laction:        denotes if action or parameter is found associated with item
!                     associated with METHOD keyword
!   1:              denotes RANDOM action found
!   2:              denotes NORAND action found
!   3:              denotes SUBNWS action found
!   4:              denotes BULKRN action found
!   5:              denotes NO_ADJ action found
!   6:              denotes SUNRIS action found
!   7:              denotes ADJ_U* action found
!   8:              denotes SUB_CC action found
!   9:              denotes NO_SUB action found
!   10:             denotes SUB_TT action found
!   11:             denotes NOTSUB action found
!   12:             denotes NOPERS action found
!
!   linstr:         denotes instrument found with NWS_HGT
!   1:              denotes WIND found
!
!   sec_sc:         denotes if secondary site characteristics are input
!                   either by AERSURF2 or SECTOR2, FREQ_SECT2, SITE_CHAR2
!   lgmt2lst:       variable denoting if GMT to LST found (true if so)
!   lelev:          denotes if user entered elevation on LOCATION keyword (true if entered)
!   lbadpbl:		denotes if errors occurred (true) during pbl processing
!   subtemp:        substitute temperatures
!   subcloud:       substitute cloud cover
!   havepress:      have pressure for further calculations
!   adjustar:       perform adjustments to u* (true)
!   onustar:        have valid onsite/prognostic u* for hour
!   onmol:          have valid onsite/prognostic Monin-Obukhov length for hour
!   onhflux:        have valid onsite/prognostic sensible heat flux for hour
!   onlflux:        have valid onsite/prognostic latent heat flux for hour
!   onwstar:		have valid onsite/prognostic w* for hour
!   onmix:          have valid onsite/prognostic mixing height for hour
!   onnrad:         have valid onsite/prognostic net radiation for hour
!   oninso:         have valid onsite/prognostic insolation for hour
!   onvptg:         have valid onsite/prognostic theta lapse rate for hour
!   oncloud:        have valid onsite/prognostic cloud cover for hour
!   day_obs:        have at least one observation for the day to use
!   up_obs:         have upper air sounding to use for the day
!   asos_hr:        NWS data is ASOS for the hour
!   nws_obs:	    have NWS observation for the hour
!   os_obs:         have an onsite/prognostic observation for the hour
!   one_min_obs:	have an AERMINUTE observation for the hour
!   cbl:            hour is convective (true) or stable (false)
!   lextend:        extend sounding to 5 km
!   lextended:      sounding extended to 5 km
!   lmmif_version:  have detected MMIF version for prognostic data
!     
!   Character variables
!   sites:          character string identifying site as primary or secondary
!					site for surface characteristics processing and messaging
!   pblid:          site id from LOCATION keywrod
!   yrstr1:         string of years for primary surface characteristics read with AERSURF or FREQ_SECT keyword
!   yrstr2:         string of years for primary surface characteristics read with AERSURF2 or FREQ_SECT2 keyword
!   
!
!   data type sc1 (sfc_char1)
!   albedo:         albedo for primary site
!   bowen:          bowen ratio for primary site
!   zo:             surface roughness for primary site
!
!   data type sc2 (sfc_char2)
!   albedo:         albedo for secondary site
!   bowen:          bowen ratio for secondary site
!   zo:             surface roughness for secondary site
!
!   data type sfdat1 (sfc_data)
!   sfcdate:        date (YYYYMMDD)
!   ipcode:         hourly precipitation code
!   ccvr:           hourly cloud cover
!   hflux:          hourly sensible heat flux
!   ustar:          hourly u*
!   wstar:          hourly w*  
!   vptg:           hourly theta lapse rate
!   zic:            hourly convective mixing height
!   zim:		    hourly mechanical mixing height
!   mol:            hourly Monin-Obukhov length
!   zo:             hourly surface roughness
!   bowen:          hourly Bowen ratio
!   albedo:         hourly albedo
!   wspd:			hourly reference wind speed
!   wdir:           hourly wind direction
!   wind_ht:        anemometer height
!   airtemp:        hourly reference air temperaure (K)
!   temp_ht:		temperature height
!   pamt:           hourly precipitation
!   rh:             hourly relative humidity
!   pres:           hourly station presssure
!   theta_star:     hourly theta-star (currently not used)
!   windsrcflag:    source of wind data for each hour
!   subflag:        substitution flag for temperature and/or cloud cover
!
!   data type pfldat (pfl_data)
!   pfldate:        date (YYYYMMDD)
!   ht:             height of level
!   speed:          wind speed
!   dir:            wind direction
!   airtemp:        air temperature (Celcius)
!   sigma_a:        standard deviation of wind direction (sigma-theta)
!   sigma_w:        standard deviation of vertical wind speed
!=========================================================================================================
!   these variables are in several subroutines so set here
    use main1, only: ipath,pathid,lbad,r8,eps,debug,noprint,nactions,nitems,lstage
    use file_units, only: debug_unit,flength
    use onsite, only: osvars,osdata1
        
    implicit none

    integer(kind=8) :: pblstart=0
    integer(kind=8) :: pblend=0
    integer(kind=4) :: pbldates(2,3)=0
    integer(kind=4) :: npbldays=0
    integer(kind=4) :: pblgmt2lst=-9
    integer(kind=4) :: irnd(24,366) 
    integer(kind=4) :: my_sounding=0
    integer(kind=4) :: target_hr_gmt=0
    integer(kind=4) :: snd_hr_gmt(2)=0
    integer(kind=4) :: snding_win(2)
    integer(kind=4) :: nsc_years=0
    integer(kind=4) :: nsc_files1=0
    integer(kind=4) :: nsc_files2=0
    integer(kind=4) :: n_cloud_sub=0
    integer(kind=4) :: n_temp_sub=0
    integer(kind=4) :: n_conv_miss=0
    integer(kind=4) :: pfl_levels=1 !at least one level, if NWS
    integer(kind=4) :: irnd1 !looping variable for random wind direction
    integer(kind=4) :: os_ipath=0  !value used to select pathid
    integer(kind=4) :: nlevs=0
    integer(kind=4) :: ihem(2)=1 !initially set to northern hemisphere
    integer(kind=4) :: pbl_windstats(3)=0
    
    integer(kind=4), allocatable, dimension(:) :: sc_years
    integer(kind=4), allocatable, dimension(:) :: nfreq1
    integer(kind=4), allocatable, dimension(:) :: nsectors1
    integer(kind=4), allocatable, dimension(:) :: nfreq2
    integer(kind=4), allocatable, dimension(:) :: nsectors2
    integer(kind=4), allocatable, dimension(:,:) :: pfl_nlevels
    integer(kind=4), allocatable, dimension(:,:) :: windsrc
    integer(kind=4), allocatable, dimension(:,:) :: tempsrc
    integer(kind=4), allocatable, dimension(:,:) :: cloudsrc
    integer(kind=4), allocatable, dimension(:) :: iup !day of sounding in upper air data
    integer(kind=4), allocatable, dimension(:) :: isnd !sounding # for each day to use in upper air data
    integer(kind=4), allocatable, dimension(:) :: ios !day of obs in onsite/PROG data
    integer(kind=4), allocatable, dimension(:) :: isf !day of obs in NWS data
    integer(kind=4), allocatable, dimension(:) :: isf1 !day of obs in AERMINUTE
    integer(kind=4), allocatable, dimension(:,:) :: pbl_obs
    
    integer(kind=4), parameter :: npblvars=12
    integer(kind=4), parameter :: npblkeys=18 !17
    integer(kind=4), parameter :: minsec=1
    integer(kind=4), parameter :: maxsec=12
      
      
    real(kind=r8) :: pbllat=0.0
    real(kind=r8) :: pbllon=0.0
    real(kind=r8) :: pblelev=0.0_r8
    real(kind=r8) :: angle
    real(kind=r8) :: crit_angle
    real(kind=r8) :: inso
    real(kind=r8) :: nrad
    real(kind=r8),parameter :: cp=1004.0_r8
    real(kind=r8), parameter :: pi=3.14159_r8
    real(kind=r8), parameter :: vonk=0.4_r8
    real(kind=r8), parameter :: mech_max=4000._r8 !max mechanical mixing height
    real(kind=r8), parameter :: conv_max=4000._r8 !max convective mixing height
    real(kind=r8), allocatable, dimension(:,:,:) :: sectors1
    real(kind=r8), allocatable, dimension(:,:,:) :: sectors2
    real(kind=r8), allocatable, dimension(:,:,:) :: tsectors
    
    real(kind=r8), allocatable, dimension(:,:,:) :: talbedo
    real(kind=r8), allocatable, dimension(:,:,:) :: tbowen
    real(kind=r8), allocatable, dimension(:,:,:) :: tzo
    
    real(kind=r8), allocatable, dimension(:) :: up_sunrise
    real(kind=r8), allocatable, dimension(:) :: up_sunset
    real(kind=r8), allocatable, dimension(:) :: local_sunrise
    real(kind=r8), allocatable, dimension(:) :: local_sunset
    real(kind=r8), allocatable, dimension(:,:) :: sun_ang
    real(kind=r8), allocatable, dimension(:,:) :: rho !density
    real(kind=r8), allocatable, dimension(:) :: theta
    real(kind=r8), allocatable, dimension(:) :: tempk
    real(kind=r8), allocatable, dimension(:) :: pres
    real(kind=r8), allocatable, dimension(:) :: ht
    
    logical :: lpblkeywords(npblkeys)
    logical :: lactions(nactions)
    logical :: litems(nitems)
    logical :: sec_sc=.false.
    logical :: lgmt2lst=.false.
    logical :: lelev=.false.
    logical :: lbadpbl=.false.
    logical :: subtemp=.true.
    logical :: subcloud=.true.
    logical :: havepress=.false.
    logical :: adjustar=.false.
    logical :: onustar !have a valid onsite u* for hour
    logical :: onmol !have a valid onsite Monin-Obukhov length
    logical :: onhflux !have a valid onsite sensible heat flux
    logical :: onlflux !have a valid onsite latent heat flux
    logical :: onwstar !have valid onsite w*
    logical :: onmix !valid onsite mixing height
    logical :: onnrad !valid onsite net radiation
    logical :: oninso !valid onsite insolation
    logical :: onvptg !valid onsite lapse rate
    logical :: oncloud !valid cloud cover
    logical, allocatable, dimension(:) :: day_obs
    logical, allocatable, dimension(:) :: up_obs
    logical, allocatable, dimension(:,:) :: asos_hr
    logical, allocatable, dimension(:,:) :: nws_obs
    logical, allocatable, dimension(:,:) :: os_obs
    logical, allocatable, dimension(:,:) :: one_min_obs
    logical, allocatable, dimension(:,:) :: got_eq_ccvr
    logical, allocatable, dimension(:,:) :: cbl
    logical :: lextend=.false.
    logical :: lextended=.false.
    logical :: lmmif_versn=.false.
    
    character(len=9) :: sites(2)
    character(len=8) :: pblid='-9'
    character(len=20) :: mmif_versn='UNKNOWN'
    
    character(len=flength), allocatable, dimension(:) :: yrstr1
    character(len=flength), allocatable, dimension(:) :: yrstr2
    
!   primary site surface characteristics
    type sc1
        real(kind=r8) :: albedo,bowen,zo
    end type sc1
      
    type(sc1), dimension(:,:,:),allocatable:: sfc_char1
      
!   secondary site surface characteristics
    type sc2
        real(kind=r8) :: albedo,bowen,zo
    end type sc2
    type(sc2), dimension(:,:,:),allocatable:: sfc_char2
    
!   variables for surface file
    type sfdat1
        integer(kind=8) :: sfcdate
        integer(kind=4) :: ipcode(24),ccvr(24),jday !,windsrc(24),tempsrc(24),cloudsrc(24)
        real(kind=r8) :: hflux(24),ustar(24),wstar(24),vptg(24),zic(24),zim(24),mol(24),zo(24),bowen(24),albedo(24),&
            wspd(24),wdir(24),wind_ht(24),airtemp(24),temp_ht(24),pamt(24),rh(24),pres(24),theta_star(24) !,sun_ang(24),&
            !up_sunrise,up_sunset,local_sunrise,local_sunset
        character(len=7) :: windsrcflag(24)
        character(len=9) :: subflag(24)
            
    end type sfdat1

    type(sfdat1),dimension(:),allocatable:: sfc_data
    
    type pfldat
        integer(kind=8) :: pfldate
        real(kind=r8), allocatable, dimension(:) :: ht,speed,dir,airtemp,sigma_a,sigma_w
    end type pfldat
    
    type(pfldat),dimension(:,:),allocatable :: pfl_data
    
    data sites /'PRIMARY','SECONDARY'/
   
!   initialize snding_win, may be reset later in up_window
    data snding_win /-1,1/
     
!   initialize irnd
    data (irnd(irnd1,1),irnd1=1,24)/5,2,8,7,7,6,9,7,1,5,8,0,7,3,6,8,5,1,8,1,4,6,4,4/
    data (irnd(irnd1,2),irnd1=1,24)/0,6,6,4,0,1,3,0,4,5,9,5,3,1,4,8,2,5,6,9,8,9,1,2/
    data (irnd(irnd1,3),irnd1=1,24)/4,0,8,4,1,3,0,3,2,8,6,2,5,6,1,4,3,5,1,2,1,3,5,1/
    data (irnd(irnd1,4),irnd1=1,24)/0,0,2,3,5,6,0,2,0,1,1,7,9,6,5,4,7,3,1,3,1,3,6,2/
    data (irnd(irnd1,5),irnd1=1,24)/9,8,1,4,6,9,6,9,3,2,2,3,2,4,9,2,3,0,1,1,6,4,2,6/
    data (irnd(irnd1,6),irnd1=1,24)/0,7,9,7,6,0,2,6,2,2,9,0,5,9,6,6,6,9,2,5,6,0,6,0/
    data (irnd(irnd1,7),irnd1=1,24)/2,7,4,9,7,0,3,8,4,8,7,6,5,0,1,9,8,8,5,6,2,6,5,9/
    data (irnd(irnd1,8),irnd1=1,24)/3,5,2,9,1,9,9,5,4,4,3,1,3,8,6,3,2,4,7,6,7,2,4,8/
    data (irnd(irnd1,9),irnd1=1,24)/9,0,0,8,0,3,7,8,4,8,5,1,8,9,4,7,5,8,0,5,3,4,8,9/
    data (irnd(irnd1,10),irnd1=1,24)/5,7,4,9,2,9,8,8,0,6,5,7,9,0,0,2,2,1,7,3,9,6,2,5/
    data (irnd(irnd1,11),irnd1=1,24)/4,8,7,8,7,8,6,5,6,9,1,4,9,8,1,5,7,0,3,8,0,0,5,3/
    data (irnd(irnd1,12),irnd1=1,24)/2,3,8,0,1,2,2,6,7,5,2,3,2,2,5,3,0,3,6,9,0,1,9,4/
    data (irnd(irnd1,13),irnd1=1,24)/3,2,7,3,8,4,1,7,5,7,6,5,4,1,5,0,1,8,4,0,2,5,4,2/
    data (irnd(irnd1,14),irnd1=1,24)/8,7,2,4,6,0,6,8,4,4,2,1,7,5,1,0,6,0,9,7,0,1,1,4/
    data (irnd(irnd1,15),irnd1=1,24)/4,6,2,4,9,4,3,6,1,1,6,4,8,4,7,0,7,1,5,6,8,7,3,4/
    data (irnd(irnd1,16),irnd1=1,24)/0,3,1,3,8,6,6,2,6,1,5,6,6,4,5,9,0,5,3,4,0,0,3,6/
    data (irnd(irnd1,17),irnd1=1,24)/1,2,3,8,7,1,4,5,8,6,0,9,0,2,8,6,2,3,3,1,2,2,5,3/
    data (irnd(irnd1,18),irnd1=1,24)/4,3,3,7,9,6,3,2,7,6,1,1,5,9,5,9,3,3,9,4,3,5,7,0/
    data (irnd(irnd1,19),irnd1=1,24)/1,1,9,0,7,2,6,6,1,5,8,3,4,4,5,7,8,2,4,8,1,5,2,0/
    data (irnd(irnd1,20),irnd1=1,24)/3,7,1,8,1,7,9,2,4,9,2,0,7,4,6,7,5,3,1,9,3,8,3,2/
    data (irnd(irnd1,21),irnd1=1,24)/3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3/
    data (irnd(irnd1,22),irnd1=1,24)/6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7/
    data (irnd(irnd1,23),irnd1=1,24)/4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4/
    data (irnd(irnd1,24),irnd1=1,24)/0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3/
    data (irnd(irnd1,25),irnd1=1,24)/4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4/
    data (irnd(irnd1,26),irnd1=1,24)/6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2/
    data (irnd(irnd1,27),irnd1=1,24)/5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0/
    data (irnd(irnd1,28),irnd1=1,24)/1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2/
    data (irnd(irnd1,29),irnd1=1,24)/1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2/
    data (irnd(irnd1,30),irnd1=1,24)/9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2/
    data (irnd(irnd1,31),irnd1=1,24)/8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7/
    data (irnd(irnd1,32),irnd1=1,24)/7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6/
    data (irnd(irnd1,33),irnd1=1,24)/6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7/
    data (irnd(irnd1,34),irnd1=1,24)/5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9/
    data (irnd(irnd1,35),irnd1=1,24)/1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2/
    data (irnd(irnd1,36),irnd1=1,24)/0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1/
    data (irnd(irnd1,37),irnd1=1,24)/2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1/
    data (irnd(irnd1,38),irnd1=1,24)/2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8/
    data (irnd(irnd1,39),irnd1=1,24)/4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6/
    data (irnd(irnd1,40),irnd1=1,24)/3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5/
    data (irnd(irnd1,41),irnd1=1,24)/9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5/
    data (irnd(irnd1,42),irnd1=1,24)/0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8/
    data (irnd(irnd1,43),irnd1=1,24)/3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9/
    data (irnd(irnd1,44),irnd1=1,24)/8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0/
    data (irnd(irnd1,45),irnd1=1,24)/4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7/
    data (irnd(irnd1,46),irnd1=1,24)/7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8/
    data (irnd(irnd1,47),irnd1=1,24)/9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8/
    data (irnd(irnd1,48),irnd1=1,24)/0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0/
    data (irnd(irnd1,49),irnd1=1,24)/7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9/
    data (irnd(irnd1,50),irnd1=1,24)/3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2/
    data (irnd(irnd1,51),irnd1=1,24)/2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4/
    data (irnd(irnd1,52),irnd1=1,24)/4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0/
    data (irnd(irnd1,53),irnd1=1,24)/0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1/
    data (irnd(irnd1,54),irnd1=1,24)/0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7/
    data (irnd(irnd1,55),irnd1=1,24)/7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1/
    data (irnd(irnd1,56),irnd1=1,24)/8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8/
    data (irnd(irnd1,57),irnd1=1,24)/5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8/
    data (irnd(irnd1,58),irnd1=1,24)/2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1/
    data (irnd(irnd1,59),irnd1=1,24)/3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9/
    data (irnd(irnd1,60),irnd1=1,24)/3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4/
    data (irnd(irnd1,61),irnd1=1,24)/9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3/
    data (irnd(irnd1,62),irnd1=1,24)/3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9/
    data (irnd(irnd1,63),irnd1=1,24)/4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1/
    data (irnd(irnd1,64),irnd1=1,24)/4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1/
    data (irnd(irnd1,65),irnd1=1,24)/0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4/
    data (irnd(irnd1,66),irnd1=1,24)/8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4/
    data (irnd(irnd1,67),irnd1=1,24)/6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8/
    data (irnd(irnd1,68),irnd1=1,24)/3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2/
    data (irnd(irnd1,69),irnd1=1,24)/3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3/
    data (irnd(irnd1,70),irnd1=1,24)/6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7/
    data (irnd(irnd1,71),irnd1=1,24)/4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4/
    data (irnd(irnd1,72),irnd1=1,24)/0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3/
    data (irnd(irnd1,73),irnd1=1,24)/4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4/
    data (irnd(irnd1,74),irnd1=1,24)/6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2/
    data (irnd(irnd1,75),irnd1=1,24)/5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0/
    data (irnd(irnd1,76),irnd1=1,24)/1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2/
    data (irnd(irnd1,77),irnd1=1,24)/1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2/
    data (irnd(irnd1,78),irnd1=1,24)/9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2/
    data (irnd(irnd1,79),irnd1=1,24)/8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7/
    data (irnd(irnd1,80),irnd1=1,24)/7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6/
    data (irnd(irnd1,81),irnd1=1,24)/6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7/
    data (irnd(irnd1,82),irnd1=1,24)/5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9/
    data (irnd(irnd1,83),irnd1=1,24)/1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2/
    data (irnd(irnd1,84),irnd1=1,24)/0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1/
    data (irnd(irnd1,85),irnd1=1,24)/2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1/
    data (irnd(irnd1,86),irnd1=1,24)/2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8/
    data (irnd(irnd1,87),irnd1=1,24)/4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6/
    data (irnd(irnd1,88),irnd1=1,24)/3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5/
    data (irnd(irnd1,89),irnd1=1,24)/9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5/
    data (irnd(irnd1,90),irnd1=1,24)/0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8/
    data (irnd(irnd1,91),irnd1=1,24)/3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9/
    data (irnd(irnd1,92),irnd1=1,24)/8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0/
    data (irnd(irnd1,93),irnd1=1,24)/4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7/
    data (irnd(irnd1,94),irnd1=1,24)/7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8/
    data (irnd(irnd1,95),irnd1=1,24)/9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8/
    data (irnd(irnd1,96),irnd1=1,24)/0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0/
    data (irnd(irnd1,97),irnd1=1,24)/7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9/
    data (irnd(irnd1,98),irnd1=1,24)/3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2/
    data (irnd(irnd1,99),irnd1=1,24)/2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4/
    data (irnd(irnd1,100),irnd1=1,24)/4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0/
    data (irnd(irnd1,101),irnd1=1,24)/0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1/
    data (irnd(irnd1,102),irnd1=1,24)/0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7/
    data (irnd(irnd1,103),irnd1=1,24)/7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1/
    data (irnd(irnd1,104),irnd1=1,24)/8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8/
    data (irnd(irnd1,105),irnd1=1,24)/5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8/
    data (irnd(irnd1,106),irnd1=1,24)/2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1/
    data (irnd(irnd1,107),irnd1=1,24)/3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9/
    data (irnd(irnd1,108),irnd1=1,24)/3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4/
    data (irnd(irnd1,109),irnd1=1,24)/9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3/
    data (irnd(irnd1,110),irnd1=1,24)/3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9/
    data (irnd(irnd1,111),irnd1=1,24)/4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1/
    data (irnd(irnd1,112),irnd1=1,24)/4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1/
    data (irnd(irnd1,113),irnd1=1,24)/0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4/
    data (irnd(irnd1,114),irnd1=1,24)/8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4/
    data (irnd(irnd1,115),irnd1=1,24)/6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8/
    data (irnd(irnd1,116),irnd1=1,24)/3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2/
    data (irnd(irnd1,117),irnd1=1,24)/3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3/
    data (irnd(irnd1,118),irnd1=1,24)/6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7/
    data (irnd(irnd1,119),irnd1=1,24)/4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4/
    data (irnd(irnd1,120),irnd1=1,24)/0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3/
    data (irnd(irnd1,121),irnd1=1,24)/4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4/
    data (irnd(irnd1,122),irnd1=1,24)/6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2/
    data (irnd(irnd1,123),irnd1=1,24)/5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0/
    data (irnd(irnd1,124),irnd1=1,24)/1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2/
    data (irnd(irnd1,125),irnd1=1,24)/1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2/
    data (irnd(irnd1,126),irnd1=1,24)/9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2/
    data (irnd(irnd1,127),irnd1=1,24)/8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7/
    data (irnd(irnd1,128),irnd1=1,24)/7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6/
    data (irnd(irnd1,129),irnd1=1,24)/6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7/
    data (irnd(irnd1,130),irnd1=1,24)/5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9/
    data (irnd(irnd1,131),irnd1=1,24)/1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2/
    data (irnd(irnd1,132),irnd1=1,24)/0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1/
    data (irnd(irnd1,133),irnd1=1,24)/2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1/
    data (irnd(irnd1,134),irnd1=1,24)/2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8/
    data (irnd(irnd1,135),irnd1=1,24)/4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6/
    data (irnd(irnd1,136),irnd1=1,24)/3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5/
    data (irnd(irnd1,137),irnd1=1,24)/9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5/
    data (irnd(irnd1,138),irnd1=1,24)/0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8/
    data (irnd(irnd1,139),irnd1=1,24)/3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9/
    data (irnd(irnd1,140),irnd1=1,24)/8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0/
    data (irnd(irnd1,141),irnd1=1,24)/4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7/
    data (irnd(irnd1,142),irnd1=1,24)/7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8/
    data (irnd(irnd1,143),irnd1=1,24)/9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8/
    data (irnd(irnd1,144),irnd1=1,24)/0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0/
    data (irnd(irnd1,145),irnd1=1,24)/7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9/
    data (irnd(irnd1,146),irnd1=1,24)/3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2/
    data (irnd(irnd1,147),irnd1=1,24)/2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4/
    data (irnd(irnd1,148),irnd1=1,24)/4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0/
    data (irnd(irnd1,149),irnd1=1,24)/0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1/
    data (irnd(irnd1,150),irnd1=1,24)/0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7/
    data (irnd(irnd1,151),irnd1=1,24)/7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1/
    data (irnd(irnd1,152),irnd1=1,24)/8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8/
    data (irnd(irnd1,153),irnd1=1,24)/5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8/
    data (irnd(irnd1,154),irnd1=1,24)/2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1/
    data (irnd(irnd1,155),irnd1=1,24)/3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9/
    data (irnd(irnd1,156),irnd1=1,24)/3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4/
    data (irnd(irnd1,157),irnd1=1,24)/9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3/
    data (irnd(irnd1,158),irnd1=1,24)/3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9/
    data (irnd(irnd1,159),irnd1=1,24)/4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1/
    data (irnd(irnd1,160),irnd1=1,24)/4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1/
    data (irnd(irnd1,161),irnd1=1,24)/0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4/
    data (irnd(irnd1,162),irnd1=1,24)/8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4/
    data (irnd(irnd1,163),irnd1=1,24)/6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8/
    data (irnd(irnd1,164),irnd1=1,24)/3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2/
    data (irnd(irnd1,165),irnd1=1,24)/3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3/
    data (irnd(irnd1,166),irnd1=1,24)/6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7/
    data (irnd(irnd1,167),irnd1=1,24)/4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4/
    data (irnd(irnd1,168),irnd1=1,24)/0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3/
    data (irnd(irnd1,169),irnd1=1,24)/4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4/
    data (irnd(irnd1,170),irnd1=1,24)/6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2/
    data (irnd(irnd1,171),irnd1=1,24)/5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0/
    data (irnd(irnd1,172),irnd1=1,24)/1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2/
    data (irnd(irnd1,173),irnd1=1,24)/1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2/
    data (irnd(irnd1,174),irnd1=1,24)/9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2/
    data (irnd(irnd1,175),irnd1=1,24)/8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7/
    data (irnd(irnd1,176),irnd1=1,24)/7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6/
    data (irnd(irnd1,177),irnd1=1,24)/6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7/
    data (irnd(irnd1,178),irnd1=1,24)/5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9/
    data (irnd(irnd1,179),irnd1=1,24)/1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2/
    data (irnd(irnd1,180),irnd1=1,24)/0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1/
    data (irnd(irnd1,181),irnd1=1,24)/2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1/
    data (irnd(irnd1,182),irnd1=1,24)/2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8/
    data (irnd(irnd1,183),irnd1=1,24)/4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6/
    data (irnd(irnd1,184),irnd1=1,24)/3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5/
    data (irnd(irnd1,185),irnd1=1,24)/9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5/
    data (irnd(irnd1,186),irnd1=1,24)/0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8/
    data (irnd(irnd1,187),irnd1=1,24)/3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9/
    data (irnd(irnd1,188),irnd1=1,24)/8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0/
    data (irnd(irnd1,189),irnd1=1,24)/4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7/
    data (irnd(irnd1,190),irnd1=1,24)/7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8/
    data (irnd(irnd1,191),irnd1=1,24)/9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8/
    data (irnd(irnd1,192),irnd1=1,24)/0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0/
    data (irnd(irnd1,193),irnd1=1,24)/7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9/
    data (irnd(irnd1,194),irnd1=1,24)/3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2/
    data (irnd(irnd1,195),irnd1=1,24)/2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4/
    data (irnd(irnd1,196),irnd1=1,24)/4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0/
    data (irnd(irnd1,197),irnd1=1,24)/0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1/
    data (irnd(irnd1,198),irnd1=1,24)/0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7/
    data (irnd(irnd1,199),irnd1=1,24)/7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1/
    data (irnd(irnd1,200),irnd1=1,24)/8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8/
    data (irnd(irnd1,201),irnd1=1,24)/5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8/
    data (irnd(irnd1,202),irnd1=1,24)/2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1/
    data (irnd(irnd1,203),irnd1=1,24)/3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9/
    data (irnd(irnd1,204),irnd1=1,24)/3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4/
    data (irnd(irnd1,205),irnd1=1,24)/9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3/
    data (irnd(irnd1,206),irnd1=1,24)/3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9/
    data (irnd(irnd1,207),irnd1=1,24)/4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1/
    data (irnd(irnd1,208),irnd1=1,24)/4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1/
    data (irnd(irnd1,209),irnd1=1,24)/0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4/
    data (irnd(irnd1,210),irnd1=1,24)/8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4/
    data (irnd(irnd1,211),irnd1=1,24)/6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8/
    data (irnd(irnd1,212),irnd1=1,24)/3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2/
    data (irnd(irnd1,213),irnd1=1,24)/3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3/
    data (irnd(irnd1,214),irnd1=1,24)/6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7/
    data (irnd(irnd1,215),irnd1=1,24)/4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4/
    data (irnd(irnd1,216),irnd1=1,24)/0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3/
    data (irnd(irnd1,217),irnd1=1,24)/4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4/
    data (irnd(irnd1,218),irnd1=1,24)/6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2/
    data (irnd(irnd1,219),irnd1=1,24)/5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0/
    data (irnd(irnd1,220),irnd1=1,24)/1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2/
    data (irnd(irnd1,221),irnd1=1,24)/1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2/
    data (irnd(irnd1,222),irnd1=1,24)/9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2/
    data (irnd(irnd1,223),irnd1=1,24)/8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7/
    data (irnd(irnd1,224),irnd1=1,24)/7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6/
    data (irnd(irnd1,225),irnd1=1,24)/6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7/
    data (irnd(irnd1,226),irnd1=1,24)/5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9/
    data (irnd(irnd1,227),irnd1=1,24)/1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2/
    data (irnd(irnd1,228),irnd1=1,24)/0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1/
    data (irnd(irnd1,229),irnd1=1,24)/2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1/
    data (irnd(irnd1,230),irnd1=1,24)/2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8/
    data (irnd(irnd1,231),irnd1=1,24)/4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6/
    data (irnd(irnd1,232),irnd1=1,24)/3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5/
    data (irnd(irnd1,233),irnd1=1,24)/9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5/
    data (irnd(irnd1,234),irnd1=1,24)/0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8/
    data (irnd(irnd1,235),irnd1=1,24)/3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9/
    data (irnd(irnd1,236),irnd1=1,24)/8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0/
    data (irnd(irnd1,237),irnd1=1,24)/4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7/
    data (irnd(irnd1,238),irnd1=1,24)/7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8/
    data (irnd(irnd1,239),irnd1=1,24)/9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8/
    data (irnd(irnd1,240),irnd1=1,24)/0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0/
    data (irnd(irnd1,241),irnd1=1,24)/7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9/
    data (irnd(irnd1,242),irnd1=1,24)/3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2/
    data (irnd(irnd1,243),irnd1=1,24)/2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4/
    data (irnd(irnd1,244),irnd1=1,24)/4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0/
    data (irnd(irnd1,245),irnd1=1,24)/0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1/
    data (irnd(irnd1,246),irnd1=1,24)/0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7/
    data (irnd(irnd1,247),irnd1=1,24)/7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1/
    data (irnd(irnd1,248),irnd1=1,24)/8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8/
    data (irnd(irnd1,249),irnd1=1,24)/5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8/
    data (irnd(irnd1,250),irnd1=1,24)/2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1/
    data (irnd(irnd1,251),irnd1=1,24)/3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9/
    data (irnd(irnd1,252),irnd1=1,24)/3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4/
    data (irnd(irnd1,253),irnd1=1,24)/9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3/
    data (irnd(irnd1,254),irnd1=1,24)/3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9/
    data (irnd(irnd1,255),irnd1=1,24)/4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1/
    data (irnd(irnd1,256),irnd1=1,24)/4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1/
    data (irnd(irnd1,257),irnd1=1,24)/0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4/
    data (irnd(irnd1,258),irnd1=1,24)/8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4/
    data (irnd(irnd1,259),irnd1=1,24)/6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8/
    data (irnd(irnd1,260),irnd1=1,24)/3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2/
    data (irnd(irnd1,261),irnd1=1,24)/3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3/
    data (irnd(irnd1,262),irnd1=1,24)/6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7/
    data (irnd(irnd1,263),irnd1=1,24)/4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4/
    data (irnd(irnd1,264),irnd1=1,24)/0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3/
    data (irnd(irnd1,265),irnd1=1,24)/4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4/
    data (irnd(irnd1,266),irnd1=1,24)/6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2/
    data (irnd(irnd1,267),irnd1=1,24)/5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0/
    data (irnd(irnd1,268),irnd1=1,24)/1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2/
    data (irnd(irnd1,269),irnd1=1,24)/1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2/
    data (irnd(irnd1,270),irnd1=1,24)/9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2/
    data (irnd(irnd1,271),irnd1=1,24)/8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7/
    data (irnd(irnd1,272),irnd1=1,24)/7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6/
    data (irnd(irnd1,273),irnd1=1,24)/6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7/
    data (irnd(irnd1,274),irnd1=1,24)/5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9/
    data (irnd(irnd1,275),irnd1=1,24)/1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2/
    data (irnd(irnd1,276),irnd1=1,24)/0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1/
    data (irnd(irnd1,277),irnd1=1,24)/2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1/
    data (irnd(irnd1,278),irnd1=1,24)/2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8/
    data (irnd(irnd1,279),irnd1=1,24)/4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6/
    data (irnd(irnd1,280),irnd1=1,24)/3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5/
    data (irnd(irnd1,281),irnd1=1,24)/9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5/
    data (irnd(irnd1,282),irnd1=1,24)/0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8/
    data (irnd(irnd1,283),irnd1=1,24)/3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9/
    data (irnd(irnd1,284),irnd1=1,24)/8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0/
    data (irnd(irnd1,285),irnd1=1,24)/4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7/
    data (irnd(irnd1,286),irnd1=1,24)/7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8/
    data (irnd(irnd1,287),irnd1=1,24)/9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8/
    data (irnd(irnd1,288),irnd1=1,24)/0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0/
    data (irnd(irnd1,289),irnd1=1,24)/7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9/
    data (irnd(irnd1,290),irnd1=1,24)/3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2/
    data (irnd(irnd1,291),irnd1=1,24)/2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4/
    data (irnd(irnd1,292),irnd1=1,24)/4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0/
    data (irnd(irnd1,293),irnd1=1,24)/0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1/
    data (irnd(irnd1,294),irnd1=1,24)/0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7/
    data (irnd(irnd1,295),irnd1=1,24)/7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1/
    data (irnd(irnd1,296),irnd1=1,24)/8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8/
    data (irnd(irnd1,297),irnd1=1,24)/5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8/
    data (irnd(irnd1,298),irnd1=1,24)/2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1/
    data (irnd(irnd1,299),irnd1=1,24)/3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9/
    data (irnd(irnd1,300),irnd1=1,24)/3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4/
    data (irnd(irnd1,301),irnd1=1,24)/9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3/
    data (irnd(irnd1,302),irnd1=1,24)/3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9/
    data (irnd(irnd1,303),irnd1=1,24)/4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1/
    data (irnd(irnd1,304),irnd1=1,24)/4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1/
    data (irnd(irnd1,305),irnd1=1,24)/0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4/
    data (irnd(irnd1,306),irnd1=1,24)/8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4/
    data (irnd(irnd1,307),irnd1=1,24)/6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8/
    data (irnd(irnd1,308),irnd1=1,24)/3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2/
    data (irnd(irnd1,309),irnd1=1,24)/3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3/
    data (irnd(irnd1,310),irnd1=1,24)/6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7/
    data (irnd(irnd1,311),irnd1=1,24)/4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4/
    data (irnd(irnd1,312),irnd1=1,24)/0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3/
    data (irnd(irnd1,313),irnd1=1,24)/4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4/
    data (irnd(irnd1,314),irnd1=1,24)/6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2/
    data (irnd(irnd1,315),irnd1=1,24)/5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0/
    data (irnd(irnd1,316),irnd1=1,24)/1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2/
    data (irnd(irnd1,317),irnd1=1,24)/1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2/
    data (irnd(irnd1,318),irnd1=1,24)/9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2/
    data (irnd(irnd1,319),irnd1=1,24)/8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7/
    data (irnd(irnd1,320),irnd1=1,24)/7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6/
    data (irnd(irnd1,321),irnd1=1,24)/6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7/
    data (irnd(irnd1,322),irnd1=1,24)/5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9/
    data (irnd(irnd1,323),irnd1=1,24)/1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2/
    data (irnd(irnd1,324),irnd1=1,24)/0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1/
    data (irnd(irnd1,325),irnd1=1,24)/2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1/
    data (irnd(irnd1,326),irnd1=1,24)/2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8/
    data (irnd(irnd1,327),irnd1=1,24)/4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6/
    data (irnd(irnd1,328),irnd1=1,24)/3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5/
    data (irnd(irnd1,329),irnd1=1,24)/9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5/
    data (irnd(irnd1,330),irnd1=1,24)/0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8/
    data (irnd(irnd1,331),irnd1=1,24)/3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9/
    data (irnd(irnd1,332),irnd1=1,24)/8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0/
    data (irnd(irnd1,333),irnd1=1,24)/4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7/
    data (irnd(irnd1,334),irnd1=1,24)/7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8/
    data (irnd(irnd1,335),irnd1=1,24)/9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8/
    data (irnd(irnd1,336),irnd1=1,24)/0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0/
    data (irnd(irnd1,337),irnd1=1,24)/7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9/
    data (irnd(irnd1,338),irnd1=1,24)/3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2/
    data (irnd(irnd1,339),irnd1=1,24)/2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4/
    data (irnd(irnd1,340),irnd1=1,24)/4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0/
    data (irnd(irnd1,341),irnd1=1,24)/0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1/
    data (irnd(irnd1,342),irnd1=1,24)/0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7/
    data (irnd(irnd1,343),irnd1=1,24)/7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1/
    data (irnd(irnd1,344),irnd1=1,24)/8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8/
    data (irnd(irnd1,345),irnd1=1,24)/5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8/
    data (irnd(irnd1,346),irnd1=1,24)/2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1/
    data (irnd(irnd1,347),irnd1=1,24)/3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9/
    data (irnd(irnd1,348),irnd1=1,24)/3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4/
    data (irnd(irnd1,349),irnd1=1,24)/9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3/
    data (irnd(irnd1,350),irnd1=1,24)/3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9/
    data (irnd(irnd1,351),irnd1=1,24)/4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1/
    data (irnd(irnd1,352),irnd1=1,24)/4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1/
    data (irnd(irnd1,353),irnd1=1,24)/0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4/
    data (irnd(irnd1,354),irnd1=1,24)/8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4/
    data (irnd(irnd1,355),irnd1=1,24)/6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8/
    data (irnd(irnd1,356),irnd1=1,24)/3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2/
    data (irnd(irnd1,357),irnd1=1,24)/3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3/
    data (irnd(irnd1,358),irnd1=1,24)/6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7/
    data (irnd(irnd1,359),irnd1=1,24)/4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4/
    data (irnd(irnd1,360),irnd1=1,24)/0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3/
    data (irnd(irnd1,361),irnd1=1,24)/4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4/
    data (irnd(irnd1,362),irnd1=1,24)/6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2/
    data (irnd(irnd1,363),irnd1=1,24)/5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0/
    data (irnd(irnd1,364),irnd1=1,24)/1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2/
    data (irnd(irnd1,365),irnd1=1,24)/1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2/
    data (irnd(irnd1,366),irnd1=1,24)/9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2/

      
    contains
!********************************************************************************************************* 
      
    subroutine pbl_path
!=========================================================================================================
!   SUBROUTINE PBL_PATH
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE METPREP PATHWAY.  THIS INCLUDES: DATA, EXTRACT, QAOUT, RANGE, AUDIT, NO_MISSING,
!   KEYWORDS XDATES, AND LOCATION KEYWORDS
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE READ_INPUT (READINP)
!
!   Variable definitions
!      
!   Integer variables
!   i:              index value of keyword
!   i1:             length of keyword with trailing blanks removed
!   jj:             counter for lpblkeywords based on ikey value
!   isite:          integer indicator of site type for surface characteristics
!                   processing and message (1=primary, 2=secondary)
!
!   Real variables
!   pblelev:        source elevation on LOCATION keyword
!                   this value will not be on the keyword but
!                   needed for calling subroutine GETLOC
!   Logical variables
!   lgood:          variable denoting if a filename is okay
!
!   Character variables
!   formstr:        format for messages
!   form1:          format to read filename
!   pblid:          source ID on LOCATION keyword (not used)
!   tempfil:        temporary filename for AERSURF or AERSURF2 file
!   yrstr:          text string of years associated with FREQ_SECT(2) or AERSURF(2)
!   modnam:         Subroutine name
!=========================================================================================================
    use file_units, only: inpfile,sfc_out_unit,pfl_out_unit,sfc_outfile,pfl_outfile,sfc_char_unit,sfc_charfile1,&
        sfc_charfile2  

    use main1, only: inpline,inpline1,ikey,nfield,ilen,keywrd,checkfile,dataline,getdates,getloc,fileind,formind,writeunit,&
        getunit,msg_form
    use surface, only: nws_hgts,sf_thresh
    implicit none
      
    integer(kind=4) :: i,i1,jj
    integer(kind=4) :: isite=0
    real(kind=r8) :: pblelev=0.0
    logical :: lgood
    character(len=6) :: form1
    character(len=8) :: pblid
    character(len=60) :: formstr(3)

    character(len=flength) :: tempfil=''
    character(len=flength) :: yrstr='-9'
    character(len=10) :: modnam='PBL_PATH'
    
!   1.  invalid keyword or duplicate entry of keyword, option has been listed before
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),',a,1x,a)'
    
!   2.  invalid number of fields for keyword
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),',i3,2(1x,a))'
    
!   3.  option listed before or extra field for keyword
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),',2(a,1x),a)'

    
!   form1 is the format to use to read the message or report file
!   format string is (a300)
    write(form1,'(a2,i3,a1)')'(a',flength,')'
      
!   get file unit for messages
    call getunit
    
    lbad=.false.
!   ikey = 0 means text string METPREP found on input line
!   set the logical variables for keywords below to false
!   meaning they have not been detected yet
!   initialize upper air variables
    if (ikey == 0) then
        lpblkeywords=.false.
        litems=.false.
        lactions=.false.
    else
        i=index(inpline1,trim(adjustl(keywrd(ikey))))
        i1=len_trim(keywrd(ikey))
          
!       set the logical variable for the keyword to true
        if ((ikey == 4 .or. ikey == 9 .or. ikey == 10) .or. (ikey >=18 .and. ikey <=24) .or. (ikey >= 28 .and. ikey <= 31) .or. &
        (ikey >=33 .and. ikey <=35) .or. ikey == 26) then
            if (ikey==4) then
                jj=18
            elseif (ikey <=10) then
                jj=ikey-8
            elseif (ikey >=18 .and. ikey <=24) then
                jj=ikey-15
            elseif (ikey >=28 .and. ikey <=31) then
                jj=ikey-17
            elseif (ikey >=33 .and. ikey <=35) then
                jj=ikey-18
            else
                jj=ikey-16
            endif
            if (ikey >=18 .and. ikey <=20 .or. ikey==33) isite=1
            if (ikey >=29 .and. ikey <=31 .or. ikey==34) isite=2
              
            if (.not. lpblkeywords(jj)) then
                lpblkeywords(jj)=.true.
            else
!               issue error message that keyword has already been found
                if ((ikey /= 4 .and. ikey <=10) .or. ikey == 21 .or. ikey == 22 .or. ikey == 24 .or. ikey == 26) then
!                    ikey == 34) then
                    write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,'DUPLICATE ENTRY OF KEYWORD:',&
                        trim(adjustl(keywrd(ikey)))
                    lbad=.true.
                    return
                endif
            endif
        else !invalid keyword for this path
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E01',modnam,'INVALID KEYWORD:',trim(adjustl(keywrd(ikey)))
            lbad=.true.
            return
        endif
     
        if (ikey == 9) then !xdates
!           incorrect number of fields, line is bad   
            if (nfield /= 3 .and. nfield /= 4 .and. nfield /= 7 .and. nfield /= 8) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else  
                call getdates(i+i1,pblstart,pblend,pbldates)
            endif
                  
        else if (ikey == 10) then !location
!             incorrect number of fields                  
            if (nfield < 4 .or. nfield > 6) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                if (nfield==6) write(writeunit,formstr(3))adjustl(pathid(ipath)),'W70',modnam,'EXTRA FIELD FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey))),'IGNORING EXTRA FIELD'
                call getloc(i+i1,pblid,pbllat,pbllon,pblgmt2lst,pblelev,lgmt2lst,lelev)
            endif
              
        else if (ikey == 18 .or. ikey == 29) then ! freq_sect or freq_sect2
!            if (nfield /= 3) then
            if (nfield < 3) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
!               allocate sc_years1 if not allocated and allocate sc_years2 if not allocated and AERSURF2 found
                if (isite==1) then
                    if (.not. allocated(sfc_charfile1)) then
                        allocate(sfc_charfile1(20))
                        allocate(yrstr1(20)) !20 files
                        yrstr1=''
                    endif
                    nsc_files1=nsc_files1+1 !increment file counter
                    sfc_charfile1(nsc_files1)=inpfile
                    
                    if (nfield > 3) then
                        call year_str(i1,yrstr1(nsc_files1))
                    else
                        yrstr1(nsc_files1)=yrstr
                    endif
                else
                    if (.not. allocated(sfc_charfile2)) then
                        allocate(sfc_charfile2(20))
                        allocate(yrstr2(20)) !20 files
                        yrstr2=''
                    endif
                    nsc_files2=nsc_files2+1 !increment file counter
                    sfc_charfile2(nsc_files2)=inpfile
                    if (nfield > 3) then
                        call year_str(i1,yrstr2(nsc_files2))
                    else
                        yrstr2(nsc_files2)=yrstr
                    endif
                endif
!               call freq_sec(i+i1,inpline1)
            endif
              
        else if (ikey == 19 .or. ikey == 30) then !SITE_CHAR or SITE_CHAR2  
            if (nfield /=6) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            elseif (.not. lpblkeywords(jj-1)) then  !FREQ_SECT or FREQ_SECT2 has not been defined, this is an error
                write(writeunit,formstr(3))adjustl(pathid(ipath)),'E76',modnam,trim(adjustl(keywrd(ikey))),&
                    'HAS BEEN LISTED BEFORE',trim(adjustl(keywrd(ikey-1)))
                lbad=.true.
            endif
              
        else if (ikey == 20 .or. ikey == 31) then !SECTOR or SECTOR2
            if (nfield /=4) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            elseif (.not. lpblkeywords(jj-2)) then  !FREQ_SECT or FREQ_SECT2 has not been defined, this is an error
                write(writeunit,formstr(3))adjustl(pathid(ipath)),'E76',modnam,trim(adjustl(keywrd(ikey))),&
                    'HAS BEEN LISTED BEFORE',trim(adjustl(keywrd(ikey-2)))
                lbad=.true.
            endif
          
        else if (ikey == 21) then !output file, i.e. surface file
!           if number of fields is not 2, then line is bad
            if (nfield /= 2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                read(inpline(i+i1+1:ilen),form1)sfc_outfile
!               get the OUTPUT filename
                call checkfile(sfc_out_unit,sfc_outfile,4,lgood)
            endif
        else if (ikey == 22) then !model not used
        
        else if (ikey == 23) then !method keyword
            if (nfield /= 3) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call methods(i+i1)
            endif
              
        else if (ikey == 24) then !uawindow
            if (nfield /= 3) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call up_window(i+i1)
            endif
                
        else if (ikey == 26) then !profile file
!           if number of fields is not 2, then line is bad
            if (nfield /= 2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                read(inpline(i+i1+1:ilen),form1)pfl_outfile
!               get the PROFILE filename
                call checkfile(pfl_out_unit,pfl_outfile,4,lgood)
            endif
              
        else if (ikey == 28) then !NWS_HGT
            if (nfield /= 3) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call nws_hgts(i1)
            endif
              
        else if (ikey <=34 .and. ikey /= 4 .and. ikey /=22 .and. ikey /= 25) then !AERSURF or AERSURF2
 !           if (nfield /= 2) then
            if (nfield < 2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                isite=ikey-32

!               allocate sc_years1 if not allocated and allocate sc_years2 if not allocated and AERSURF2 found
                if (isite==1) then
                    if (.not. allocated(sfc_charfile1)) then
                        allocate(sfc_charfile1(20))
                        allocate(yrstr1(20)) !20 files
                        yrstr1=''
                    endif
                    nsc_files1=nsc_files1+1 !increment file counter
                else
                    if (.not. allocated(sfc_charfile2)) then
                        allocate(sfc_charfile2(20))
                        allocate(yrstr2(20)) !20 files
                        yrstr2=''
                    endif
                    nsc_files2=nsc_files2+1 !increment file counter
                endif
                
!               use the subroutine DATALINE to get the AERSURFACE filename
                call dataline(i,i1)
                !code modified by GMM
				!original code:-
				!read(inpline(fileind(1):fileind(2)),'(a)'),tempfil
				read(inpline(fileind(1):fileind(2)),'(a)') tempfil
                if (nfield > 2) read(inpline1(formind(1):ilen),'(a)')yrstr
!               get the AERSURF or AERSURF2 filename
                if (isite == 1) then
                    sfc_charfile1(nsc_files1)=tempfil !AERSURFACE file
                    call checkfile(sfc_char_unit(isite),sfc_charfile1(nsc_files1),1,lgood)
                    yrstr1(nsc_files1)=yrstr
                else
                    sfc_charfile2(nsc_files2)=tempfil !AERSURFACE file
                    call checkfile(sfc_char_unit(isite),sfc_charfile2(nsc_files2),1,lgood)
                    yrstr2(nsc_files2)=yrstr
                endif
                close(sfc_char_unit(isite))
            endif
        elseif (ikey==35) then !ASOS threshold
            if (nfield /=2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call sf_thresh(i1+i)
            endif
        else !ikey = 22 (MODEL keyword) do nothing at this time; DEBUG
!			also ignore the DATA keyword
            if (ikey /= 4 .and. ikey /=22 .and. ikey /= 25) write(writeunit,formstr(1))adjustl(pathid(ipath)),'E01',modnam,&
                'INVALID KEYWORD:',trim(adjustl(keywrd(ikey)))
        endif
    endif
    

    return
      
    end subroutine pbl_path
!*********************************************************************************************************

    subroutine year_str(i1,ystr)
!=========================================================================================================
!   SUBROUTINE YEAR_STR
!   THIS SUBROUTINE GETS A STRING OF YEARS FOR THE FREQ_SECT KEYWORD IN THE RUNSTREAM FILE
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PATH)
!
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   OUPUT ARGUMENT(S):
!   
!   YSTR:           TEXT STRING OF YEARS
!
!   Variable definitions
!   Integers:
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!   ifield:         field counter
!
!   Character variables
!   modnam:         Subroutine name
!   ystr:           text string of years
!   str:            year string as array of years
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: nfield,getfields
    !use file_units, only: flength
    implicit none
    integer(kind=4) :: i1,nfield1,ifield
    !integer(kind=4) :: i,j !temp
    character(len=10) :: modnam='YEAR_STR'
    character(len=flength) :: ystr
    character(len=100), allocatable, dimension(:) :: str
    
    
    nfield1=nfield-1
    allocate(str(nfield1))
    str='0000000000'
    call getfields(i1,nfield1,str)
   
!   start past field that contains the # of sectors and build year string
    l1: do ifield=3,nfield1
        if (ifield == 3) then
            write(ystr,'(a)')trim(adjustl(str(ifield)))
        else
            write(ystr,'(a,1x,a)')trim(adjustl(ystr)),trim(adjustl(str(ifield)))
        endif
    enddo l1
    deallocate(str)
    return
    end subroutine year_str
!*********************************************************************************************************

    subroutine pbl_proc
!=========================================================================================================
!   SUBROUTINE PBL_PROC
!   THIS SUBROUTINE PERFORMS THE PBL CALCULATIONS
!
!   MODIFIED MAY 31, 2022
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY AERMET
!
!   Variable definitions
!
!   Integer variables
!   nsite:          number of surface characteristics sites, 1 if primary only; 2 if both primary and secondary sites
!   my_zone:        sounding time zone to use (see sdg2use)
!   d:              day loop counter
!   h:              hour loop counter
!   idattim:        date/time used for notifying user of start/end processing
!   y:              year loop counter
!   m:              month loop counter
!   imon:           first (1) and last (2) month to process for each year
!   idy:            first (1) and last (2) day to process for each month
!   iday1:          day index
!   iw1:            used to calculate Julian day
!   iw2:            used to calculate Julian day
!   iw3:            used to calculate Julian day
!   iswitch:        switch to indicate when to check upper air data and nws, onsite/prog, and aerminute
!                   iswitch=1, check for nws, onsite/prog, and aerminute
!                   iswitch=2, check for upper air       
!   isite:          integer indicator of site type for surface characteristics
!                   processing and message (1=primary, 2=secondary
!   cblhr1:         first convective hour
!   v:              variable counter
!   ilev:           level counter
!   sdg2use:        array of sounding times to use (negative is day before current day)
!
!   Real variables
!   rhomin:         minimum density
!   csubg:          constant to calculate heat flux
!   homin:          minimum heat flux
!   skyfract:       sky fraction   
!   ustar_ratio:    ratio of original site-specific u* to recalculate u* for hours
!                   when wind speed is reset; added post 21drf
!   orig_wind_speed:    original wind speed before reset (if needed); added post 21drf
!   temp_ustar:     recalculated u* (1=based on original wind speed, 2=based on reset wind speed) for calculating ustar_ratio; 
!                   added post 21drf
!   temp_l:         temporary Monin-Obukhov length output by calc_ustar when resetting u*; added post 21drf
!   temp_thetastar: temporary thetastar output by calc_ustar when resetting u*; added post 21drf
!
!   Logical variables
!   leap:           year is leap year (true)
!   calcflux:       calculate heat flux
!   stable_rate:    true if lapse rate is stable
!   bulkri:         invoke Bulk Richardson routine
!   reset:          reset u* and theta*
!   calc_conv:      calculate convective mixing heights
!   firstcbl:       first convective hour found
!   posflux:        positive heat flux found
!   stablecalcs:    perform stable calculations
!   lcycle:         cycle l3 loop
!   ostemp:         substituted temperature is on temperature
!   lbadsc:         have bad surface characterstics
!   wind_reset:     wind has been reset from its original value to minimum wind speed; added post 21drf
!
!   Character variables
!   formstr:        format for messages
!   debugform:      format for debug messages
!   date_hr_str:	date/hour text string
!   cdate:          output of date-time argument
!   adate:          character string of date
!   modnam:         Subroutine name
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: lpath,numdays,leapyr,lkey,days,r,g,msg_form
    use file_units, only: msg_unit,sfc_charfile1,sfc_charfile2
    use upperair, only: upgmt2lst,uplat,uplon,updata,sound_info,nsnd,upstage1
    use surface, only: nws_hgt,sfvars,sfc_init,nsfc
    use onsite, only: nlevel,temp_var,inso_var,nrad_var,ustar_var,l_var,hflux_var,lflux_var,rh_var,dewpt_var,prcp_vars,osdt_hts,&
        dt01_var,wstar_var,mix_var,vptg_var,zo_var,bo_var,alb_var,sa_var,sw_var,wind_vars,temp_var,nosvars,overland,onsite_init,&
        nos,osstage1,cloud_var
    
    implicit none
    integer(kind=4) :: nsite=1 !at least 1 site
    integer(kind=4) :: my_zone=0
    integer(kind=4) ::d,h,idattim(8),y,m,imon(2),idy(2),iday1,iw1,iw2,iw3,iswitch,cblhr1,v,ilev,isite!,iup,ios,isf,isf1,isnd,isite
    integer(kind=4) :: sdg2use(-12:12)
    real(kind=r8) :: rhomin,skyfract,ustar_ratio,temp_ustar(2),temp_l,temp_thetastar
    real(kind=r8), parameter :: csubg=0.1_r8
    real(kind=r8), parameter :: homin=0.0001_r8
    real(kind=r8), allocatable, dimension(:,:) :: orig_wind_speed
    
    logical :: leap,calcflux,stable_rate,bulkri,reset,calc_conv,firstcbl,posflux,stablecalcs,lcycle,ostemp,lbadsc
    logical, allocatable, dimension(:,:) :: wind_reset
    character(len=10) :: modnam='PBL_PROC'
    character(len=20) :: date_hr_str
    character(len=80) :: formstr(12)
    character(len=100) :: debugform(13)
    character(len=8) :: cdate,adate

    !   initialize sdg2use
    data sdg2use /-12,12,12,12,12,12,12,12,12,0,0,0,0,0,0,0,0,0,0,0,-12,-12,-12,-12,-12/
    
!   formats for messages
!   1.  message to screen to write out processing day
    write(formstr(1),'(a)')'(1x,a,1x,2(a2,a1),a4,1x,a)'
    
!   2.  format for start/stop PBL calculations
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,a8,1x,2(i2.2,a1),i2.2)'	
5	format(1x,a10,1x,a3,5x,a10,1x,'PBL CALCULATIONS ',a,a8,1x,i2.2,':',i2.2,':',i2.2)  
    
!   3.  air temperature exceeds bounds or
!       heat flux < 0 for convective hour
!       net radiation > 0 for stable hour
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a20,1x,a,1x,f6.2,1x,a)'

!   4.  density exceeds bounds
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a20,1x,a,2(1x,f5.2,1x,a))'
    
!   5.  calm wind or cloud cover missing or temperature missing, skip calculations
!		or w* set to 0.001 m/s
!       concvective or mechanical mixing height is 0
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a20,1x,a)'

!   6.  no sounding for the day
!       heat flux conditions not met for the day
!       or only 1 level in sounding
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a)'
    
!   7.  Prognostic data variable will not be read but calculated
    write(formstr(7),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),a)'
    
!   8. Bulk Ri not used
    write(formstr(8),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20,1x,a)'
    
!   9. no valid data, abort processing
    write(formstr(9),'(2(a))')trim(adjustl(msg_form)),'a)'
    
!   10. convective mixing height set to conv_max
    write(formstr(10),'(2(a))')trim(adjustl(msg_form)),'a,1x,f6.1,1x,a,1x,a20)'
    
!   11. notify user of stability approach
    write(formstr(11),'(2(a))')trim(adjustl(msg_form)),'a)'

!   12. adjust u* not applied
    write(formstr(12),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20)'
!   debug formats
!   1. header
    write(debugform(1),'(a)')'(2(/1x,a),1x,i8.8/)'
    
!   2.  wind information
    write(debugform(2),'(a)')'(/t10,a,1x,3(1x,f5.1),1x,i1,1x,f9.5)'
    
!   3. temperature information
    write(debugform(3),'(a)')'(/t10,a,1x,f5.1,1x,f6.1,1x,i1)'

!   4.  cloud cover
    write(debugform(4),'(a)')'(/t10,a,1x,i2,1x,i2)'	

!   5.  albedo and bowen ratio
    write(debugform(5),'(a)')'(/t10,a,1x,2(1x,f6.2))'
    
!   6.  value
    write(debugform(6),'(a)')'(/t10,3(a,1x),g13.6)'
    
!   7.  heat flux calculations
    write(debugform(7),'(a)')'(2(/t10,a),/t11,f3.1,t18,f4.2,t24,g13.6,1x,g13.6)'

!   8. L calculation
    write(debugform(8),'(a)')'(/t10,a,/t16,a,/t11,f3.1,1x,f7.5,t23,f8.6,t33,f6.2,t41,g13.6,2(1x,g13.6))'

!   9.  lapse rate
    write(debugform(9),'(a)')'(/t10,a,/t53,g13.6,2(1x,g13.6))'
    
!   10. heat flux calculation
    write(debugform(10),'(a)')'(2(/t10,a)/t10,f6.4,t18,f6.1,3(1x,g13.6))'
    
!   11. upper air sounding window
    write(debugform(11),'(a)')'(2(/t10,a)/t10,f8.3,t24,i3,t34,i3,t49,i2.2,1x,a1,i3,a1,,t69,i2.2,t87,i2.2,1x,a1,i3,a1)'

!   12. density calculation
    write(debugform(12),'(a)')'(2(t10,a)/t15,f7.0,t28,f6.2,t38,f8.4,t51,f8.5)'

!   13. density value exceed bounds
    write(debugform(13),'(a)')'(t10,a,2(1x,f5.2,1x,a))'

    call date_and_time(date=cdate,values=idattim)
    
!   if no valid observations for any data, issue error and abort
    if (((upstage1 .and. nsnd(4)-nsnd(5) == 0) .or. (.not. upstage1 .and. lstage(2) .and. nsnd(2) == 0)) .and. nsfc(4) == 0 .and. &
        nos(2) == 0) then
        write(msg_unit,formstr(9))adjustl(pathid(ipath)),'E70',modnam,'NO VALID DATA, ABORT PROCESSING'
        lbadpbl=.true.
        return
    endif

    write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I70',modnam,'PBL CALCULATIONS BEGIN: ',cdate,idattim(5),':',idattim(6),&
        ':',idattim(7)
    
!   22112 this is now done in pbl_test
!    if (.not. lpath(4) .and. .not. lpath(5)) call onsite_init
    
!   write stage 2 header
    if (debug) then
        write(debug_unit,'(a)')' STAGE 2 DEBUG PROCESSING START'
!       if ONSITE or PROG used, set os_ipath for later debugging statements
        if (lpath(4) .or. lpath(5)) then
            if (lpath(4)) then
                os_ipath=4 !1
            else
                os_ipath=5 !2
            endif
        endif
    endif
    
!   alert user that certain variables won't be used if overwater variables are read in
!   but location is indicated to be overland
!   only issue message if performing stage 2 without stage 1
!   if performing stage 1 and 2 in same run, the stage 1 messages will indicate the 
!   variable will not be used in stage 2
    if (overland .and. .not. osstage1) then
    v1: do v=5,nosvars
            if (osvars(v)%lread .and. (v == l_var .or. v == hflux_var .or. v == wstar_var .or. v == vptg_var .or. v == zo_var &
            .or. v == bo_var .or. v == alb_var .or. v == lflux_var)) then
                write(msg_unit,formstr(7))adjustl(pathid(ipath)),'I80',modnam,'LOCATION IS OVERLAND; PROGNOSTIC VARIABLE',&
                    trim(adjustl(osvars(v)%varname)),'WILL NOT BE USED FROM STAGE 1 BUT CALCULATED IN STAGE 2'
            endif
        enddo v1
    endif
    
!   alert user if Monin-Obukhov length will be used for stability determination
!   can only be used if overwater
    if (osvars(l_var)%lread .and. .not. overland) then
        write(msg_unit,formstr(11))adjustl(pathid(ipath)),'I84',modnam,&
            'MONIN-OBUKHOV LENGTH WILL BE USED FOR STABILITY DETERMINATION WHEN L IS NOT MISSING'
    else
!       L not read in or overland; use standard solar angle approach
        write(msg_unit,formstr(11))adjustl(pathid(ipath)),'I84',modnam,&
            'SOLAR ANGLE APPROACH WILL BE USED FOR STABILITY DETERMINATION'
    endif
!   initialize sfc_date to have all dates within the data period
!   even if a day may not have observations
    npbldays=numdays(pblstart,pblend)
    
!   allocate arrays
    allocate(sfc_data(npbldays))
    allocate(pbl_obs(4,npbldays)) !post 21DRF
    allocate(wind_reset(24,npbldays))
    allocate(orig_wind_speed(24,npbldays))
    allocate(windsrc(24,npbldays))
    allocate(tempsrc(24,npbldays))
    allocate(cloudsrc(24,npbldays))
    allocate(up_sunrise(npbldays))
    allocate(up_sunset(npbldays))
    allocate(local_sunrise(npbldays))
    allocate(local_sunset(npbldays))
    allocate(sun_ang(24,npbldays))
    allocate(day_obs(npbldays))
    allocate(up_obs(npbldays))
    allocate(asos_hr(24,npbldays))
    allocate(nws_obs(24,npbldays))
    allocate(os_obs(24,npbldays))
    allocate(one_min_obs(24,npbldays))
    allocate(got_eq_ccvr(24,npbldays))
    allocate(cbl(24,npbldays))
    allocate(rho(24,npbldays))
    if (lpath(2)) then
        allocate(iup(npbldays))
        allocate(isnd(npbldays))
        iup=1
        isnd=1
    endif
    if (lpath(3)) then
        allocate(isf(npbldays))
        if (lkey(32)) then
            allocate(isf1(npbldays))
            isf1=1
        endif
        isf=1
    endif
    if (lpath(4) .or. lpath(5)) then
        allocate(ios(npbldays))
        ios=1
    endif
    
    windsrc=0
    tempsrc=0
    cloudsrc=0
    pbl_obs=0 !post 21DRF
    up_sunrise=-999.0_r8
    up_sunset=-999.0_r8
    local_sunrise=-999.0_r8
    local_sunset=-999.0_r8
    sun_ang=-999.0_r8
    day_obs=.false.
    up_obs=.false.
    asos_hr=.false.
    nws_obs=.false.
    os_obs=.false.
    one_min_obs=.false.
    got_eq_ccvr=.false.
    cbl=.false.
    rho=-9.0_r8
    calcflux=.false.
    stable_rate=.false.
    bulkri=.false.
    
    nrad=real(osvars(nrad_var)%missval,r8)!*osvars(nrad_var)%conv
    inso=real(osvars(inso_var)%missval,r8)!*osvars(inso_var)%conv
    
!   set pfl_levels based on the presence of ONSITE or PROG pathway
!   if not present pfl_levels initialized to 1
    if (lpath(4) .or. lpath(5)) then
        pfl_levels=nlevel
    else
!       call onsite_init to assign values to the variable indices for profile data
        call onsite_init
    endif
    
    allocate(pfl_data(24,npbldays))
    
    allocate(pfl_nlevels(24,npbldays))
    
    pfl_nlevels=pfl_levels
    
!   initialize the hourly variables in sfc_data and pfl_data to missing or zero
    h1: do h=1,24
        sfc_data%hflux(h)=-999.0_r8  
        sfc_data%ustar(h)=-9.0_r8
        sfc_data%wstar(h)=-9.0_r8
        sfc_data%vptg(h)=-9.0_r8
        sfc_data%zic(h)=-999.0_r8
        sfc_data%zim(h)=-999.0_r8
        sfc_data%mol(h)=-99999.0_r8
        sfc_data%zo(h)=-9.0_r8
        sfc_data%bowen(h)=-9.0_r8
        sfc_data%albedo(h)=-9.0_r8
        sfc_data%wspd(h)=999.0_r8
        sfc_data%wdir(h)=999.0_r8
        sfc_data%wind_ht(h)=-9.0_r8
        sfc_data%airtemp(h)=999.0_r8
        sfc_data%temp_ht(h)=-9.0_r8
        sfc_data%pamt(h)=-9._r8
        sfc_data%rh(h)=999.0_r8
        sfc_data%pres(h)=99999.0_r8
        sfc_data%theta_star(h)=-9.0_r8
        sfc_data%ipcode(h)=9999
        sfc_data%ccvr(h)=99
        sfc_data%windsrcflag(h)='NAD'
        sfc_data%subflag(h)='NoSubs'
    enddo h1
    
!   get the sounding times to use if not using the SUNRISE option for sounding selection
!   these are independent of day, so only need to to do this calculation once.
    if (.not. lactions(6)) then
!       determine zone to determine appropriate reference sounding based on upper air station longitude
!       keep same myzone calculations as MPPBL in old AERMET, so multiply longitudes by -1
!       because longitudes in old AERMET are opposite of real world.
        if (uplon < 0.0_r8) then  !western hemisphere
            my_zone=-int(-uplon/15._r8+0.5_r8)
        elseif (uplon > 0.0_r8) then !eastern hemisphere
            my_zone=-int(-uplon/15._r8-0.5_r8)
        else
            my_zone=0 !prime meridian
        endif
        my_sounding=sdg2use(my_zone)
        if (my_sounding < 0) then
            target_hr_gmt=-my_sounding
        else
            target_hr_gmt=my_sounding
        endif
!       sounding windows
        snd_hr_gmt(1)=my_sounding+snding_win(1)
        snd_hr_gmt(2)=my_sounding+snding_win(2)
!       debug information 
        if (debug)write(debug_unit,debugform(11))'UPPER AIR SOUNDING WINDOW INFORMATION',&
            'UPPER LON    MY_ZONE  MY_SOUNDING    WINDOW BEGIN      TARGET HOUR         WINDOW END',&
            uplon,my_zone,my_sounding,snd_hr_gmt(1),'(',snding_win(1),')',target_hr_gmt,snd_hr_gmt(2),&
            '(',snding_win(2),')'
        
    endif
    
    iday1=0
    y1: do y=pbldates(1,1),pbldates(2,1)
!       check to see if year is leap year
        call leapyr(y,leap)
!       set initial values of imonth(1) and imonth(2) to
!       1 and 12
        imon(1)=1
        imon(2)=12
!       reset imonth(1) and imonth(2) to first and last
!       month if first or last year of data period
        if (y == pbldates(1,1)) imon(1)=pbldates(1,2)
        if (y == pbldates(2,1)) imon(2)=pbldates(2,2)
    m1: do m=imon(1),imon(2)
!           initialize iday(1) to first day of month and
!           iday(2) to last day of the month
            idy(1)=1
            idy(2)=days(m)
!           reset iday(2) to 29 if February and leap year
            if (m == 2 .and. leap) idy(2)=days(m)+1   
!           set iday(1) and iday(2) to first and last days
!           if first month of first year and last month
!           of last year
            if (m == imon(1) .and. y == pbldates(1,1)) idy(1)=pbldates(1,3)
            if (m == imon(2) .and. y == pbldates(2,1))idy(2)=pbldates(2,3)
    d1:     do d=idy(1),idy(2)
                iday1=iday1+1
                sfc_data(iday1)%sfcdate=y*10000+m*100+d
                pfl_data(:,iday1)%pfldate=y*10000+m*100+d
    hh1:        do h=1,24
                    allocate(pfl_data(h,iday1)%ht(pfl_levels))
                    allocate(pfl_data(h,iday1)%speed(pfl_levels))
                    allocate(pfl_data(h,iday1)%dir(pfl_levels))
                    allocate(pfl_data(h,iday1)%airtemp(pfl_levels))
                    allocate(pfl_data(h,iday1)%sigma_a(pfl_levels))
                    allocate(pfl_data(h,iday1)%sigma_w(pfl_levels))
!                   initialize pfl_data based on presence of onsite/prognostic data
    lv1:            do ilev=1,pfl_levels
                        pfl_data(h,iday1)%sigma_a(ilev)=osvars(sa_var)%missval*osvars(sa_var)%conv
                        pfl_data(h,iday1)%sigma_w(ilev)=osvars(sw_var)%missval*osvars(sw_var)%conv
                        if ((lpath(4) .or. lpath(5)) .and. nos(2) > 0) then
                            pfl_data(h,iday1)%speed(ilev)=osvars(wind_vars(2))%missval*osvars(wind_vars(2))%conv
                            pfl_data(h,iday1)%dir(ilev)=osvars(wind_vars(1))%missval*osvars(wind_vars(1))%conv
                            pfl_data(h,iday1)%airtemp(ilev)=osvars(temp_var)%missval*osvars(temp_var)%conv
                            pfl_data(h,iday1)%ht(ilev)=osdata1(1,ilev,1,1)
                        else
                            pfl_data(h,iday1)%speed(ilev)=sfvars(11)%missval!*sfvars(11)%conv
                            pfl_data(h,iday1)%dir(ilev)=sfvars(10)%missval!*sfvars(10)%conv
                            pfl_data(h,iday1)%airtemp(ilev)=sfvars(7)%missval*sfvars(7)%conv
                            pfl_data(h,iday1)%ht(ilev)=nws_hgt
                        endif
                        
                    enddo lv1
                enddo hh1
!               calculate julian day (based on function julian in older AERMET)
                iw1=mod((m+9),12)
                iw2=(iw1*153+2)/5+d+58
                if (leap) then
                    iw3=366
                    iw2=iw2+1
                else
                    iw3=365
                endif
                sfc_data(iday1)%jday=1+mod(iw2,iw3)
!               calculate angles and local sunrise/sunset
                 
                call sundat(pbllat,pbllon,pblgmt2lst,sfc_data(iday1)%jday,local_sunrise(iday1),local_sunset(iday1),&
                    sun_ang(:,iday1))
                
!               upper air sunrise/sunset
                call sundat(uplat,uplon,upgmt2lst,sfc_data(iday1)%jday,up_sunrise(iday1),up_sunset(iday1))
                
!               check to see if there are  NWS data or ONSITE/PROG data
                iswitch=1
                call check_obs(iswitch,iday1)
                
            enddo d1
        enddo m1         
    enddo y1
    
!   get the surface characteristics
!   check to see if secondary SC used, reset nsite to 2, otherwise leave nsite as 1
!   also set sec_sc to true indicating secondary site
    if (sec_sc)nsite=2
    s1: do isite=1,nsite
!       allocate the frequency and sectors arrays for each site
        if (isite == 1) then
            allocate(nfreq1(pbldates(2,1)-pbldates(1,1)+1))
            allocate(nsectors1(pbldates(2,1)-pbldates(1,1)+1))
            nfreq1=0
            nsectors1=0
        else
            allocate(nfreq2(pbldates(2,1)-pbldates(1,1)+1))
            allocate(nsectors2(pbldates(2,1)-pbldates(1,1)+1))
            nfreq2=0
            nsectors2=0
        endif
        lbadsc=.false.
        call aersurf(2,isite,lbadsc)
    enddo s1
    
    if (allocated(sfc_charfile1)) deallocate(sfc_charfile1)     
    if (allocated(yrstr1)) deallocate(yrstr1)
    if (allocated(sfc_charfile2)) deallocate(sfc_charfile2)
    if (allocated(yrstr2)) deallocate(yrstr2)
    
    if (.not. lbadpbl) then
!       write surface file header
        call sfc_header
!       assign hourly winds, initial temperatures (no substitution yet), cloud cover, surface characteristics
!       do not interpolate missing temperatures or cloud cover yet
    d2: do d=1,npbldays
        
!           write message to screen for day
            write(adate,'(i8)')sfc_data(d)%sfcdate
            if (.not. noprint)write(output_unit,formstr(1))'Stage 2: Initial data assignments for month/day/year',adate(5:6),'/',&
                adate(7:8),'/',adate(1:4),'LST'
!           get location of the day in the upper air, onsite and surface data
             if (.not. day_obs(d)) cycle d2
             
    h2:     do h=1,24

!               reset nws_obs to true if no standard obs but there is a one minute obs
!               this allows the missing temperature to be interpolated if other hours not missing
                write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sfc_data(d)%sfcdate,'HR',h
                
                if (.not. nws_obs(h,d) .and. one_min_obs(h,d)) nws_obs(h,d)=one_min_obs(h,d)
!                call winds(d,h)
!               post 21DRF; pass wind_reset and original wind speed as output arguments
                call winds(d,h,wind_reset(h,d),orig_wind_speed(h,d))
!               assign initial temperatures either onsite or NWS; interpolation for missing hours takes place below
!               in subroutine substitute
                call temps(d,h)

!               assign cloud cover either onsite or NWS; interpolation for missing hours takes place below
!               in subroutine substitute
!               post 21DRF, now check cloud cover
                call have_data(d,h,cloud_var,oncloud)
                if (oncloud .and. overland .and. lpath(5)) oncloud=.false.
                
                call clouds(d,h)

            enddo h2
    enddo d2
    
!       after initial temperature and cloud assignments, perform substitution (if requested)
!       and also calculate pressure
    
    d3: do d=1,npbldays
            
!           write message to screen for day
            write(adate,'(i8)')sfc_data(d)%sfcdate
            if (.not. noprint)write(output_unit,formstr(1))'Stage 2: PBL calculations for month/day/year',adate(5:6),'/',&
                adate(7:8),'/',adate(1:4),'LST'
            
            if (.not. day_obs(d)) then
                call writefiles(d)
                cycle d3
            endif
            
!           set calc_conv to false initially.  This will be used to tell AERMET if conv_ht needs to be
!           called to calculate convective mixing heights for the day.  If at least 1 convective hour
!           doesn't have onsite mixing heights, then convective heights will be calculated
            calc_conv=.false.
!           first convective hour
            cblhr1=1
!           at least one convective hour is positive
            posflux=.false.
!           set firstcbl, denoting first convective hour
            firstcbl=.false.
!           write debug info for the day, sunrise/sunset times
            if (debug) then
                write(debug_unit,debugform(1))'*****************************************************************************',&
                    'DATE:',sfc_data(d)%sfcdate
                call write_srss(d)
            endif

!           get sounding for day
            iswitch=2
            call check_obs(iswitch,d)
            if (up_obs(d)) then
                if (sound_info(iup(d))%nlevels(isnd(d)) == 1 .and. updata(2,1,isnd(d),iup(d)) == 0.0_r8) then
!					reset to having no sounding for the day
                    up_obs(d)=.false.
!					if only 1 level in the sounding and it is the surface, issue message and skip
!					post 21DRF, only issue the message if mixing heights not read
                    if (.not. osvars(mix_var)%lread)write(msg_unit,formstr(6))adjustl(pathid(ipath)),'I78',modnam,&
                        'SOUNDING FOR DATE',sfc_data(d)%sfcdate,&
                        'HAS ONLY ONE LEVEL AT SURFACE, NO CONVECTIVE PARAMETERS WILL BE CALCULATED'
                else
                        call read_sound(d)
                endif
            else
!               write to message file, no sounding
!               post 21DRF, only write message if no mixing heights
                if (.not. osvars(mix_var)%lread)write(msg_unit,formstr(6))adjustl(pathid(ipath)),'I78',modnam,&
                    'NO SOUNDING AVAILABLE FOR DATE',sfc_data(d)%sfcdate,'NO CONVECTIVE PARAMETERS WILL BE CALCULATED'
            endif   
            if (.not. up_obs(d) .and. .not. osvars(mix_var)%lread) n_conv_miss=n_conv_miss+1
            
    h3:     do h=1,24   
                write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sfc_data(d)%sfcdate,'HR',h
                
                if (debug)write(debug_unit,'(/t5,a4,1x,i2.2)')'HOUR',h
                if (.not. nws_obs(h,d) .and. .not. os_obs(h,d) .and. .not. one_min_obs(h,d)) then
                    if (debug)write(debug_unit,'(t5,a)')'NO VALID OBSERVATIONS'
                    cycle h3
                endif
!               reset nrad and inso
                nrad=real(osvars(nrad_var)%missval,r8)
                inso=real(osvars(inso_var)%missval,r8)

                if (debug) then
                    write(debug_unit,debugform(2))'WIND SPEED/DIRECTION/HT/SOURCE/Z0:',sfc_data(d)%wspd(h),sfc_data(d)%wdir(h),&
                    sfc_data(d)%wind_ht(h),windsrc(h,d),sfc_data(d)%zo(h)
                    
                    write(debug_unit,debugform(5))'INITIAL ALBEDO/BOWEN RATIO:',sfc_data(d)%albedo(h),sfc_data(d)%bowen(h)
                    
                    if (tempsrc(h,d) > 0) write(debug_unit,debugform(3))'TEMPERATURE/HT/SOURCE:',sfc_data(d)%airtemp(h),&
                        sfc_data(d)%temp_ht(h),tempsrc(h,d)
                    
                    if (cloudsrc(h,d) > 0) write(debug_unit,debugform(4))'INITIAL CLOUD COVER/SOURCE:',sfc_data(d)%ccvr(h),&
                        cloudsrc(h,d)
                endif   
!               substitute temperature and/or clouds if not assigned, but only for the following hours:
!               only hour 1 when the day is greater than the first day (no substitutions for first day for hours 1 and 2)
!               only hour 24 when the day is less than the last day (no substitutions for last day)
!               all other hours and days

!               initialize ostemp to false; may be reset to true in substitute if both interpolating hours are onsite
!               ostemp will be used in press subroutine when calculating station pressure
                ostemp=.false. 

                if (((subtemp .and. tempsrc(h,d) == 0) .or. (subcloud .and. cloudsrc(h,d) == 0)) .and. &
                    ((h ==1 .and. d > 1) .or. (h > 1 .and. h < 24) .or. (h == 24 .and. d < npbldays)))&
                    call substitute(d,h,ostemp)
                
!               assign data to pfl_data 
                call profile(d,h)
                
!               assign/calculate pressure   
                call press(d,h,ostemp)    
!               assign/calculate dewpoint and RH
                if (nws_obs(h,d) .or. (os_obs(h,d) .and. (osvars(rh_var)%lread .or. (osvars(temp_var)%lread .and. &
                     osvars(dewpt_var)%lread))))call rh(d,h)
               
!               assign precip if reading NWS obs or precip read from onsite data
                if (nws_obs(h,d) .or. (os_obs(h,d) .and. (osvars(prcp_vars(1))%lread .or. osvars(prcp_vars(2))%lread))) &
                    call precip(d,h)
                
!			    assign values for certain logical variables that will be used later
!			    some will be set to false if overland
!			    also set some variables regardless of stability; others will be set for the
!			    appropriate stability
                
!			    u*
                call have_data(d,h,ustar_var,onustar)
!               post 21DRF, if overland and prognostic reset
                if (onustar .and. overland .and. lpath(5)) onustar=.false.
                
!				L
                call have_data(d,h,l_var,onmol)
                if (onmol .and. overland)onmol=.false. !reset if overland
                
!				sensible heat flux
                call have_data(d,h,hflux_var,onhflux)
                if (onhflux .and. overland)onhflux=.false. !reset if overland
                
!				latent heat flux
                call have_data(d,h,lflux_var,onlflux)
                if (onlflux .and. overland)onlflux=.false. !reset if overland
                
!				w*; set wstar in sfc_data in the convective stability section of code below
                call have_data(d,h,wstar_var,onwstar)
                if (onwstar .and. overland)onwstar=.false. !reset if overland
                
!				mixing height; set zic and zim if sfc_data depending on stabililty below
                call have_data(d,h,mix_var,onmix)
                
!				vptg; set vptg in sfc_data in the convective stability section of code below
                call have_data(d,h,vptg_var,onvptg)
                if (onvptg .and. overland)onvptg=.false. !reset if overland
                
!				!net radiation
                call have_data(d,h,nrad_var,onnrad)
                
!				insolation
                call have_data(d,h,inso_var,oninso)  
                
!               Calculate the solar elevation at which the net radiation is
!               theoritically zero (CRIT_ANGLE) and compare this to the actual
!               solar elevation; we assign the hour to the CBL bin if the
!               solar elevation is .GE. CRIT_ANGLE, otherwise the hour is assigned to
!               the SBL bin.  As necessary, missing values for cloud cover and
!               temperature are replaced locally with fixed values (5 and 288,
!               respectively); the missing value flags are retained in the global
!               variables.
                call stability(d,h)
                
!               determine first convective hour, this will be used later to determine
!               if convective mixing heights will be calculated later
                if (.not. firstcbl .and. cbl(h,d)) then
                    firstcbl=.true.
                    cblhr1=h
                endif
                
!               need to have a valid temperature and pressure to proceed
                if (tempsrc(h,d) == 0 .or. .not. havepress) then
                    write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I72',modnam,date_hr_str,&
                        'TEMPERATURE AND/OR PRESSURE MISSING, SKIP CALCULATIONS'
                    
                    cycle h3
                endif
!               check temperature bounds
                if (tempsrc(h,d) > 0 .and. (sfc_data(d)%airtemp(h) < 208._r8 .or. sfc_data(d)%airtemp(h) > 348._r8)) &
                    write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W75',modnam,date_hr_str,'AIR TEMPERATURE',&
                    sfc_data(d)%airtemp(h),'< 208 K OR > 348 K'
                    
!               calculate density (kg/m^3)
                rho(h,d)=sfc_data(d)%pres(h)*100._r8/(r*sfc_data(d)%airtemp(h))
                if (debug) write(debug_unit,debugform(12))'DENSITY CALCULATION',&
                    'PRESSURE*100     AIR TEMP      R          DENSITY',sfc_data(d)%pres(h)*100._r8,sfc_data(d)%airtemp(h),r,&
                    rho(h,d)
                
!               check bounds on density
                if (sfc_data(d)%pres(h) > 800._r8) then
                    rhomin=0.90_r8
                else
                    rhomin=0.75_r8
                endif
                
                if (rho(h,d) < rhomin .or. rho(h,d) > 1.5_r8) then
                    write(msg_unit,formstr(4))adjustl(pathid(ipath)),'W75',modnam,date_hr_str,'DENSITY',rho(h,d),'<',rhomin,&
                        'OR > 1.5'
                    if (debug)write(debug_unit,debugform(13))'DENSITY',rho(h,d),'<',rhomin,'OR > 1.5'
                endif
                
!               assign values for certain  variables that will be used later
!               also set some variables regardless of stability; others will be set for the
!               appropriate stability
!               u*
                
                if (onustar) then
!                   if a stable hour and wind was reset, rescale u*
                    if (wind_reset(h,d) .and. .not. cbl(h,d)) then
                        write(msg_unit,formstr(8))adjustl(pathid(ipath)),'I85',modnam,'VALID ONSITE U* FOR',&
                            date_hr_str,'WILL BE RECALCULATED BASED ON RESET WIND SPEED'
!                       calculate u* based on original wind speed
                        if (debug)then
                            if (debug)write(debug_unit,'(/t10,a)')'SCALING U*'
                            write(debug_unit,'(/t10,a)')'CALCULATING U* BASED ON ORIGINAL WIND SPEED'
                        endif
                        call calc_ustar(d,h,wind_reset(h,d),orig_wind_speed(h,d),temp_ustar(1),temp_l,temp_thetastar)
!                       calculate u* based on reset wind speed
                        if (debug) write(debug_unit,'(/t10,a)')'CALCULATING U* BASED ON RESET WIND SPEED'
                        call calc_ustar(d,h,wind_reset(h,d),sfc_data(d)%wspd(h),temp_ustar(2),temp_l,temp_thetastar)
                        
                        if (temp_ustar(1) == 0.0_r8 .or. temp_ustar(1) == 0.0_r8) then
                            ustar_ratio=1.0_r8
                            if (debug) write(debug_unit,'(/t10,a)')'U* RATIO = 1; DO NOT RESET U*'
                            sfc_data(d)%ustar(h)=ustar_ratio*osdata1(osvars(ustar_var)%readvar,1,h,ios(d))
                        else
                            ustar_ratio=temp_ustar(2)/temp_ustar(1)
                            sfc_data(d)%ustar(h)=ustar_ratio*osdata1(osvars(ustar_var)%readvar,1,h,ios(d))
                            if (debug) then
                                write(debug_unit,'(/t10,a/t10,f10.5,1x,f10.5)')'ORIGINAL U*    ORIGINAL WIND SPEED',&
                                    osdata1(osvars(ustar_var)%readvar,1,h,ios(d)),orig_wind_speed(h,d)
                                write(debug_unit,'(/t10,a,1x,f10.5)')'RECALCULATED U* (ORIG WIND SPEED)',temp_ustar(1)
                                write(debug_unit,'(/t10,a,1x,f10.5)')'RECALCULATED U* (RESET WIND SPEED)',temp_ustar(2)
                                write(debug_unit,'(/t10,a,1x,g13.6)')'U* RATIO (RESET SPEED/ORIG SPEED)',ustar_ratio
                                write(debug_unit,'(/t10,a,1x,f10.5)')'SCALED U*',sfc_data(d)%ustar(h)
                                if (debug)write(debug_unit,'(/t10,a)')'END U* SCALING'
                            endif
                        endif					
                    elseif (.not. wind_reset(h,d)) then !wind not reset, stability doesn't matter
                        sfc_data(d)%ustar(h)=osdata1(osvars(ustar_var)%readvar,1,h,ios(d))
                        if (debug) then
                            write(debug_unit,debugform(6))'U* SET TO ',trim(adjustl(pathid(os_ipath))),'VALUE:',&
                            sfc_data(d)%ustar(h)
                            if (adjustar .and. .not. cbl(h,d)) write(msg_unit,formstr(8))adjustl(pathid(ipath)),&
                                'I85',modnam,'VALID ONSITE U* FOR',date_hr_str,'ADJUST U* NOT APPLIED'
                        endif
                    endif
                endif
 
!               L
                if (onmol) then
                    if (wind_reset(h,d) .and. onustar .and. .not. cbl(h,d)) then
!                       post 21DRF; scale L based on u* ratio (cubed of ratio); only for stable hours
                        write(msg_unit,formstr(8))adjustl(pathid(ipath)),&
                                'I86',modnam,'VALID ONSITE L FOR',date_hr_str,&
                                'WILL BE SCALED BASED ON CUBED RATIO OF NEW/ORIGINAL U*'
                        sfc_data(d)%mol(h)=osdata1(osvars(l_var)%readvar,1,h,ios(d))*ustar_ratio*ustar_ratio*ustar_ratio
                        if (debug) then
                            write(debug_unit,'(/t10,a)')'L SCALED BY CUBED RATIO OF NEW/ORIGINAL U* RATIO'
                            write(debug_unit,'(/t10,a/t10,f10.5,1x,f10.5,1x,g13.6)')'ORIGINAL L     U* RATIO    NEW  L',&
                                osdata1(osvars(l_var)%readvar,1,h,ios(d)),ustar_ratio,sfc_data(d)%mol(h)
                        endif
                    elseif (.not. wind_reset(h,d)) then
                        sfc_data(d)%mol(h)=osdata1(osvars(l_var)%readvar,1,h,ios(d))
                        if (debug) write(debug_unit,debugform(6))'L SET TO ',trim(adjustl(pathid(os_ipath))),'VALUE:',&
                        sfc_data(d)%mol(h)	
                    endif
                endif
!               heat flux
                
                if (onhflux) then
                    sfc_data(d)%hflux(h)=osdata1(osvars(hflux_var)%readvar,1,h,ios(d))
                    if (debug) write(debug_unit,debugform(6))'HEAT FLUX SET TO ',trim(adjustl(pathid(os_ipath))),'VALUE:',&
                        sfc_data(d)%hflux(h)
                endif

!               !net radiation
                
                if (onnrad) then
                    nrad=osdata1(osvars(nrad_var)%readvar,1,h,ios(d))
                    if (debug) write(debug_unit,debugform(6))'NET RADIATION SET TO ',trim(adjustl(pathid(os_ipath))),'VALUE:',nrad
                    !   for stable hour, check to see if positive
                    if (.not. cbl(h,d)) then
                        !if (dabs(nrad-real(osvars(nrad_var)%missval,r8)*osvars(nrad_var)%conv) > eps .and. nrad > 0.0_r8) &
                        if (dabs(nrad-real(osvars(nrad_var)%missval,r8)) > eps .and. nrad > 0.0_r8) &
                        write(msg_unit,formstr(3))adjustl(pathid(ipath)),'I76',modnam,date_hr_str,'NET RADIATION',nrad,&
                            '> 0 FOR STABLE HOUR'
                    endif
                endif
!               insolation
                
                if (oninso) then
                    inso=osdata1(osvars(inso_var)%readvar,1,h,ios(d))
                    if (debug) write(debug_unit,debugform(6))'INSOLATION SET TO ',trim(adjustl(pathid(os_ipath))),'VALUE:',inso
                endif    
                
!               determine first convective hour, this will be used later to determine
!               if convective mixing heights will be calculated later
    !            if (.not. firstcbl .and. cbl(h,d)) then
    !                firstcbl=.true.
    !                cblhr1=h
                !endif
                
                if (cbl(h,d)) then
                
!                   proceed with convective boundary layer calculations
!                   assign or calculate net_radiation
                    lcycle=.false.
                    if (.not. onnrad) call netrad(d,h,lcycle)
                    if (lcycle) cycle h3
!                   calculate heat flux if not read in or missing from onsite data
!                   calculate heat flux from net radiation
                    if (dabs(nrad-real(osvars(nrad_var)%missval,r8)) > eps .and. .not. onhflux) then
                        sfc_data(d)%hflux(h)=( (1.0_r8-csubg)*nrad)/(1.0_r8+(1.0_r8/sfc_data(d)%bowen(h)))
                        if (debug)write(debug_unit,debugform(7))'HEAT FLUX CALCULATED','CSUBG   BOWEN       NRAD      HEAT FLUX',&
                            csubg,sfc_data(d)%bowen(h),nrad,sfc_data(d)%hflux(h)
                        
                    endif
                 
!                   check to make sure heat flux is positive for convective hour; if not issue message and reset to 0.1
                    if (sfc_data(d)%hflux(h) <= 0.0_r8 .and. dabs(sfc_data(d)%hflux(h)-(-999.0_r8)) > eps) then
                        write(msg_unit,formstr(3))adjustl(pathid(ipath)),'I81',modnam,date_hr_str,' HEAT FLUX',&
                            sfc_data(d)%hflux(h),'<= 0 FOR CONVECTIVE HOUR; RESET TO 0.1'
                        sfc_data(d)%hflux(h)=0.1_r8

                        if (debug)write(debug_unit,'(t10,a)')'HEAT FLUX <= 0, SET TO 0.1'
                    endif
                    
                    if (sfc_data(d)%hflux(h) > 0.0_r8) posflux=.true.
                    
!                   if the wind has been reset and onsite u*, recalculate u* based on original wind speed and then
!                   reset wind speed, take ratio, and apply to original u* to calculate updated u*
                    if (onustar .and. wind_reset(h,d)) then
                        write(msg_unit,formstr(8))adjustl(pathid(ipath)),'I86',modnam,'VALID ONSITE U* FOR',&
                            date_hr_str,'WILL BE RECALCULATED BASED ON RESET WIND SPEED'
!                       calculate u* based on original wind speed
                        if (debug)then
                            if (debug)write(debug_unit,'(/t10,a)')'SCALING U*'
                            write(debug_unit,'(/t10,a)')'CALCULATING U* BASED ON ORIGINAL WIND SPEED'
                        endif
                        call calc_ustar(d,h,wind_reset(h,d),orig_wind_speed(h,d),temp_ustar(1),temp_l,temp_thetastar)
!                       calculate u* based on reset wind speed
                        if (debug) write(debug_unit,'(/t10,a)')'CALCULATING U* BASED ON RESET WIND SPEED'
                        call calc_ustar(d,h,wind_reset(h,d),sfc_data(d)%wspd(h),temp_ustar(2),temp_l,temp_thetastar)
                        
                        if (temp_ustar(1) == 0.0_r8 .or. temp_ustar(1) == 0.0_r8) then
                            ustar_ratio=1.0_r8
                            if (debug) write(debug_unit,'(/t10,a)')'U* RATIO = 1; DO NOT RESET U*'
                            sfc_data(d)%ustar(h)=ustar_ratio*osdata1(osvars(ustar_var)%readvar,1,h,ios(d))
                        else
                            ustar_ratio=temp_ustar(2)/temp_ustar(1)
                            sfc_data(d)%ustar(h)=ustar_ratio*osdata1(osvars(ustar_var)%readvar,1,h,ios(d))
                            if (debug) then
                                write(debug_unit,'(/t10,a/t10,f10.5,1x,f10.5)')'ORIGINAL U*	   ORIGINAL WIND SPEED',&
                                    osdata1(osvars(ustar_var)%readvar,1,h,ios(d)),orig_wind_speed(h,d)
                                write(debug_unit,'(/t10,a,1x,f10.5)')'RECALCULATED U* (ORIG WIND SPEED)',temp_ustar(1)
                                write(debug_unit,'(/t10,a,1x,f10.5)')'RECALCULATED U* (RESET WIND SPEED)',temp_ustar(2)
                                write(debug_unit,'(/t10,a,1x,g13.6)')'U* RATIO (RESET SPEED/ORIG SPEED)',ustar_ratio
                                write(debug_unit,'(/t10,a,1x,f10.5)')'SCALED U*',sfc_data(d)%ustar(h)
                                if (debug)write(debug_unit,'(/t10,a)')'END U* SCALING'
                            endif
                        endif
                    endif
                    
!                   if first convective hour, check to see if > homin and if not, reset firstcbl to false.
                    if (firstcbl .and. h==cblhr1 .and. sfc_data(d)%hflux(h) < homin) then
                        firstcbl=.false.
                        cblhr1=1
                    endif
!                   if there are onsite mixing heights go ahead and assign convective . if not, they will be assigned outside the
!                   if/else for processing for convective vs. stable hours
!                   apply check for convective mixing height to compare against conv_max
                    if (onmix) then
                        if (sfc_data(d)%zic(h) > conv_max) write(msg_unit,formstr(10))adjustl(pathid(ipath)),'I79',modnam,&
                            'CBL HT SET TO',conv_max,'FOR',date_hr_str
                            
                        sfc_data(d)%zic(h)=min(osdata1(osvars(mix_var)%readvar,1,h,ios(d)),conv_max)
                      
                        if (debug) then
                            write(debug_unit,'(t10,2(a),f10.4,a,f6.1)')'CONVECTIVE MIXING HEIGHT SET TO MINIMUM OF ',&
                            trim(adjustl(pathid(os_ipath))),osdata1(osvars(mix_var)%readvar,1,h,ios(d)),&
                                ' AND MAX CONVECTIVE HEIGHT ',conv_max
                        endif
                        
!                       post 21DRF				
!                       check the mechanical mixing heights for zero
!                       convective mixing height
                        
                        if (sfc_data(d)%zic(h) == 0.0_r8) write(msg_unit,formstr(5))adjustl(pathid(ipath)),'W75',modnam,&
                            date_hr_str,'CONVECTIVE MIXING HEIGHT = 0 M'

!                       have onsite/PROG u* set mechanical mixing height
                        if (onustar) call mech_ht(d,h)
                    else
!                       if onmix is false, then set calc_conv to true for the day
                        calc_conv=.true.
!                       post 21DRF, if mixing height read in but missing, issue message that it will be calculated
                        if (osvars(mix_var)%lread)  write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I87',modnam,&
                            date_hr_str,'CONVECTIVE MIXING HEIGHT MISSING; WILL BE CALCULATED IN CONV_HT'
                    endif   
                    
!                   if no wind, skip further processing
                    if (windsrc(h,d) == 0 .or. (sfc_data(d)%wspd(h) < eps .and. sfc_data(d)%wdir(h) < eps) .or. &
                        (sfc_data(d)%wspd(h) > 900._r8 .and. sfc_data(d)%wdir(h) > 900.0_r8)) then
!                       winds are missing or calm, skip hour
                        write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I72',modnam,date_hr_str,&
                            'WINDS CALM OR MISSING, SKIP CALCULATIONS'
                        cycle h3 
                    endif

!                   if have onsite L with onsite u* and wind was reset then rescale
                    if (onmol .and. wind_reset(h,d) .and. onustar) then
!                       post 21DRF; scale L based on u* ratio (cubed of ratio)
                        write(msg_unit,formstr(8))adjustl(pathid(ipath)),&
                                'I86',modnam,'VALID ONSITE L FOR',date_hr_str,&
                                'WILL BE SCALED BASED ON CUBED RATIO OF NEW/ORIGINAL U*'
                        sfc_data(d)%mol(h)=osdata1(osvars(l_var)%readvar,1,h,ios(d))*ustar_ratio*ustar_ratio*ustar_ratio
                        if (debug) then
                            write(debug_unit,'(/t10,a)')'L SCALED BY CUBED RATIO OF NEW/ORIGINAL U* RATIO'
                            write(debug_unit,'(/t10,a/t10,f10.5,1x,f10.5,1x,g13.6)')'ORIGINAL L     U* RATIO    NEW  L',&
                                osdata1(osvars(l_var)%readvar,1,h,ios(d)),ustar_ratio,sfc_data(d)%mol(h)
                        endif
                    endif
                    
!                   u* and Monin-Obukhov length      
!                   don't need a separate check for L since it will be handled in calc_ustar 
!                   calculate u*; if L is also missing it will be calculated as well
                    if (.not. onustar) then
                        if (onmol) then
!                           output L to temp_l (not used) but calculate u*
                            call calc_ustar(d,h,wind_reset(h,d),sfc_data(d)%wspd(h),sfc_data(d)%ustar(h),temp_l,temp_thetastar)
                        else
!                           output L to sfc_data value of L
                            call calc_ustar(d,h,wind_reset(h,d),sfc_data(d)%wspd(h),sfc_data(d)%ustar(h),sfc_data(d)%mol(h),&
                                temp_thetastar)
                        endif
                    endif
                    
!					now have u*, get mechanical mixing height if in onsite/PROG data
                    if (onmix .and. .not. onustar) call mech_ht(d,h)
!                   
!                   assign Monin-Obukhov to array if needed
                    if (.not. onmol) then
 !                      calculate L if onsite u* read in and not missing; if u* is missing or not read,
!                       L will have been calculated in calc_ustar
                        if (onustar) then
                            sfc_data(d)%mol(h)=-rho(h,d)*cp*sfc_data(d)%airtemp(h)*sfc_data(d)%ustar(h)**3/&
                                (vonk*g*sfc_data(d)%hflux(h))  
                            if (debug)write(debug_unit,debugform(8))'L CALCULATED FROM U*, DENSITY, TEMPERATURE AND HEAT FLUX',&
                                'k     G      DENSITY   TEMP         U*         HFLUX           L',vonk,g,rho(h,d),&
                                sfc_data(d)%airtemp(h),sfc_data(d)%ustar(h),sfc_data(d)%hflux(h),sfc_data(d)%mol(h)
                
                        endif
                    endif
!                   calculate w* if needed and necessary variables are available; if mixing height is to be calculated
!                   w* will be calculated subroutine conv_ht   
                    if (onwstar) then
                        sfc_data(d)%wstar(h)=osdata1(osvars(wstar_var)%readvar,1,h,ios(d))
                        if (debug) write(debug_unit,debugform(6))'W* SET TO ',trim(adjustl(pathid(os_ipath))),'VALUE:',&
                            sfc_data(d)%wstar(h)
                    
!                       reset w* if < 0.001
                        if (sfc_data(d)%wstar(h) < 0.001_r8) then
                            sfc_data(d)%wstar(h)=0.001_r8
                            write(msg_unit,formstr(5))adjustl(pathid(ipath)),'W78',modnam,date_hr_str,'W* < 0.001; RESET TO 0.001'
                            if (debug)write(debug_unit,'(/t10,a)')'W* < 0.001; RESET TO 0.001'
                        endif
                    else
                        if (onmix .and. sfc_data(d)%mol(h) < 0.0_r8 .and. dabs(sfc_data(d)%mol(h)-(-99999._r8)) > &
                        eps .and. rho(h,d) > 0.0_r8 .and. sfc_data(d)%airtemp(h) > 0.0_r8 .and. sfc_data(d)%airtemp(h) < 900.0_r8 &
                        .and. sfc_data(d)%hflux(h) > -900._r8 .and. dabs(sfc_data(d)%hflux(h)-(-777.0_r8)) > eps .and. &
                        sfc_data(d)%zic(h) > 0.0) sfc_data(d)%wstar(h)=wstar(d,h)       
                    endif
                    
 !                  calculate vptg if needed and necessary variables are available; if mixing height is to be calculated
!                   vptg will be calculated in subroutine conv_ht  
                    if (onvptg) then
                        sfc_data(d)%vptg(h)=dmax1(osdata1(osvars(vptg_var)%readvar,1,h,ios(d)),0.005_r8)
                        if (debug)write(debug_unit,'(/t10,a,g13.6,a)')'VPTG SET TO MAX OF ',&
                            osdata1(osvars(vptg_var)%readvar,1,h,ios(d)),' AND 0.005'
                    else
                        if (onmix .and. sfc_data(d)%mol(h) < 0.0_r8 .and. dabs(sfc_data(d)%mol(h)-(-99999._r8)) > eps .and. &
                        rho(h,d) > 0.0_r8 .and. sfc_data(d)%airtemp(h) > 0.0_r8 .and. sfc_data(d)%airtemp(h) < 900.0_r8 &
                        .and. sfc_data(d)%hflux(h) > -900._r8 .and. dabs(sfc_data(d)%hflux(h)-(-777.0_r8)) > eps .and. &
                        sfc_data(d)%zic(h) > 0.0_r8) then
                            if (.not. up_obs(d)) then
!                               if no sounding
                                sfc_data(d)%vptg(h)=0.005_r8	!set to default value          
                            else
                                sfc_data(d)%vptg(h)=vptg(d,h)
                            endif
                        endif
                    endif                           
                    
                else 
!                   have onsite/PROG u* and mixing height, go ahead and set mechanical mixing height
!                   even if further processing is skipped.
                    if (onmix .and. onustar) call mech_ht(d,h)
!                   if no wind, skip further processing
                    if (windsrc(h,d) == 0 .or. (sfc_data(d)%wspd(h) < eps .and. sfc_data(d)%wdir(h) < eps) .or. &
                        (sfc_data(d)%wspd(h) > 900._r8 .and. sfc_data(d)%wdir(h) > 900.0_r8)) then
!                       winds are missing or calm, skip hour
                        write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I72',modnam,date_hr_str,&
                            'WINDS CALM OR MISSING, SKIP CALCULATIONS'
                        cycle h3 
                    endif

!                   stable
                    stablecalcs=.true.
!                   if have u*, L, cloud cover, and heat flux, can skip most calculations, just need to calculate theta-star
!                   note if onsite u* not available, it will be calculated later
                    if (onustar) then
!                       if cloud cover available use that first
!                       if cloud cover not available but L is, use L
!                       use heat flux if neither avaialable and if none of the three are
!                       available, skip
                        if (cloudsrc(h,d) > 0) then
!                           calculate theta-star from cloud cover 
!                           equation 16 of AERMOD MFED, Section 3.3.1 page 18
                            sfc_data(d)%theta_star(h)=0.09_r8*(1.0_r8-0.5_r8*((real(sfc_data(d)%ccvr(h),r8)/10._r8)**2))
                            if (debug) write(debug_unit,'(t10,a,g13.6,a,i2)')'THETA STAR ',sfc_data(d)%theta_star(h),&
                                ' CALCULATED FROM CLOUD COVER ',sfc_data(d)%ccvr(h)
                            stablecalcs=.false.
                        elseif (onmol .and. tempsrc(h,d) > 0) then
!                           calculate from L, temperature and u*
!                           solve for theta-star, equation 11 of AERMOD MFED, Section 3.3.1 page 16
                            sfc_data(d)%theta_star(h)=(sfc_data(d)%airtemp(h)/(vonk*g*sfc_data(d)%mol(h)))*sfc_data(d)%ustar(h)*&
                                sfc_data(d)%ustar(h)
                            if (debug) write(debug_unit,'(t10,a,g13.6,a,1x,f7.2)')'THETA STAR ',sfc_data(d)%theta_star(h),&
                                ' CALCULATED FROM TEMPERATURE ',sfc_data(d)%airtemp(h)
                            stablecalcs=.false.
                        elseif(onhflux) then
!                           calculate from heat flux and u*
!                           equation 10 of AERMOD MFED, Section 3.3.1 page 16
                            sfc_data(d)%theta_star(h)=-sfc_data(d)%hflux(h)/(rho(h,d)*cp*sfc_data(d)%ustar(h))
                            stablecalcs=.false.
                        endif
                    endif    
                        
!                   apply Bulk-Richarson number if have onsite winds > 0 and don't have onsite u* and L, a stable lapse rate
!                   or do not have cloud cover or only have equivalent cloud cover
                    if (lactions(4)) then
!                       post 21DRF, set bulkri to false if onsite u* and L read in
                        if (.not. stablecalcs .or. (onustar .and. onmol)) then
                            bulkri=.false.
                        else
                            bulkri=.true.
                        endif
                    else
                        bulkri=.false.
                    endif
                        
                    if (bulkri .and. osvars(dt01_var)%lread .and. windsrc(h,d)==1 .and. sfc_data(d)%wspd(h) > 0.0_r8) then

!                       Bulk-Richardson requested and can be used and we have DT01
                       
                        if (dabs(osdata1(osvars(dt01_var)%readvar,1,h,ios(d))-real(osvars(dt01_var)%missval,r8)) > eps) then
!                           check lapse rate
                            if (osdata1(osvars(dt01_var)%readvar,1,h,ios(d))+(osdt_hts(1,2)-osdt_hts(1,1))*0.0098_r8 > 0.0_r8) then
                                stable_rate=.true.
                            else
                                stable_rate=.false.
!                               use neutral limit and calculate u* (if needed), theta*, and L
!                               post 21DRF							
                                if (.not. onustar) &
                                    sfc_data(d)%ustar(h)=vonk*sfc_data(d)%wspd(h)/dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))
                                sfc_data(d)%theta_star(h)=0.00001_r8
                                if (.not. onmol) sfc_data(d)%mol(h)=8888._r8
                                if (debug)write(debug_unit,debugform(9))&
                                    'UNSTABLE LAPSE RATE, SET TO NEUTRAL LIMITS       U*        THETASTAR         L',&
                                    sfc_data(d)%ustar(h),sfc_data(d)%theta_star(h),sfc_data(d)%mol(h)
                                write(msg_unit,formstr(8))adjustl(pathid(ipath)),'I82',modnam,'BULK RI NOT USED FOR',&
                                    date_hr_str,'UNSTABLE OR NEUTRAL LAPSE RATE'
                            endif
                        else
                            write(msg_unit,formstr(8))adjustl(pathid(ipath)),'I82',modnam,'BULK RI NOT USED FOR',date_hr_str,&
                                'MISSING LAPSE RATE'  
                            stable_rate=.false. !set to false if missing delta-t
                        endif
                        if (stable_rate .or. cloudsrc(h,d) == 0 .or. got_eq_ccvr(h,d)) then
                            bulkri=.true.
                        elseif (cloudsrc(h,d) /= 0 .or. .not. got_eq_ccvr(h,d)) then
                            write(msg_unit,formstr(8))adjustl(pathid(ipath)),'I82',modnam,'BULK RI NOT USED FOR',date_hr_str,&
                            'CC NOT MISSING OR DO NOT HAVE EQUIVALENT CC'
                            bulkri=.false.
                        endif
                    else
                        stable_rate=.false.
                        if (bulkri) then
                            if (windsrc(h,d)/=1) write(msg_unit,formstr(8))adjustl(pathid(ipath)),'I82',modnam,&
                                'BULK RI NOT USED FOR',date_hr_str,'MISSING ONSITE WINDS'
                            bulkri=.false. !reset
                        endif
                    endif
                    
                    if (bulkri) then
!                       apply Bulk Richardson approach
                        call bulk_rich(d,h)
                    else
                        if (cloudsrc(h,d) == 0 .and. stablecalcs) then
!                           missing cloud cover, cycle
                            write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I75',modnam,date_hr_str,&
                                'CLOUD COVER MISSING, SKIP CALCULATIONS'
                            if (debug)write(debug_unit,'(/t10,a)')'MISSING CLOUD COVER, U*, AND L, CYCLE HOUR LOOP'
                            cycle h3
                        endif
                        if (.not. onustar) call calc_ustar(d,h,wind_reset(h,d),sfc_data(d)%wspd(h),sfc_data(d)%ustar(h),temp_l,&
                            sfc_data(d)%theta_star(h)) !calculate u* and theta-star
                    endif
                    
!                   now check u* and theta* to make sure heat flux isn't less than -64 W/m**2
!                   these checks are done even if onsite u*, etc. are available.
                    if (sfc_data(d)%ustar(h) > 0.0_r8 .and. sfc_data(d)%theta_star(h) > 0.0_r8) call check_flux(d,h,reset)
                    
!                   have u*, calculate mechanical mixing height if have onsite/PROG mixing heights
                    if (onmix) call mech_ht(d,h)
                    
!                   calculate equivalent cloud cover from theta star if missing for BULKRN. will be used for deposition
                    if (bulkri .and. (cloudsrc(h,d)==0 .or. got_eq_ccvr(h,d))) then
                        if (sfc_data(d)%theta_star(h) >= 0.09_r8) then
                            sfc_data(d)%ccvr(h)=0
                            if (debug)write(debug_unit,'(/t10,a,g13.6,a)')'THETA-STAR ',sfc_data(d)%theta_star(h),&
                                ' >= 0.09, SET CLOUD COVER TO 0'
                        else
            !               calculate skyfract based on equation 16 in AERMOD MFED (page 18), solving for fraction
                            skyfract=dsqrt((1.0_r8-sfc_data(d)%theta_star(h)/0.09_r8)/0.5_r8)
                            if (debug)write(debug_unit,'(/t10,2(a,g13.6))')'SKYFRACT = ',skyfract,&
                                ' BASED ON EQUATION 16 AERMOD MFED WITH THETA-STAR = ',sfc_data(d)%theta_star(h)
                            if (skyfract <= 0.0_r8) then
                                sfc_data(d)%ccvr(h)=0
                                if (debug)write(debug_unit,'(/t10,a,g13.6,a)')'SKYFRACT = ',skyfract,' <= 0, SET CLOUD COVER TO 0'
                            elseif (skyfract >= 1.0_r8) then
                                sfc_data(d)%ccvr(h)=10
                                if (debug)write(debug_unit,'(/t10,a,g13.6,a)')'SKYFRACT = ',skyfract,' >= 1, SET CLOUD COVER TO 10'
                            else
                                sfc_data(d)%ccvr(h)=nint(skyfract*10._r8)
                                if (debug)write(debug_unit,'(/t10,a,g13.6,a,i2)')'SKYFRACT = ',skyfract,&
                                ' CLOUD COVER = SKYFRAC*10 ',sfc_data(d)%ccvr(h)
                            endif
                        endif
                        cloudsrc(h,d)=5
                        
                    endif
!                   calculate Monin-Obukhov length and heat flux if necessary
                   
                    if (.not. onmol) then
                        if (sfc_data(d)%ustar(h) > 0.0_r8 .and. sfc_data(d)%theta_star(h) > 0.0_r8) then
                            if (adjustar) then
                                if (bulkri) then
                                   
                                    sfc_data(d)%mol(h)=sfc_data(d)%airtemp(h)*sfc_data(d)%ustar(h)*sfc_data(d)%ustar(h)/&
                                    (vonk*g*sfc_data(d)%theta_star(h))
                                    if (debug)write(debug_unit,'(/t10,a,g13.6)')'L SET TO: ',sfc_data(d)%mol(h)
                                    !endif
                                    
                                else
                        !           use L=1100*ustar*ustar and normal L, take max
                                    sfc_data(d)%mol(h)=max((1100._r8*sfc_data(d)%ustar(h)*sfc_data(d)%ustar(h)),&
                                        sfc_data(d)%airtemp(h)*sfc_data(d)%ustar(h)*sfc_data(d)%ustar(h)/&
                                        (vonk*g*sfc_data(d)%theta_star(h)))
                                    if (debug)write(debug_unit,'(/t10,a,g13.6)')'L SET TO: ',sfc_data(d)%mol(h)
                                endif
                            else
                              
                                sfc_data(d)%mol(h)=sfc_data(d)%airtemp(h)*sfc_data(d)%ustar(h)*sfc_data(d)%ustar(h)/&
                                    (vonk*g*sfc_data(d)%theta_star(h))
                                if (debug)write(debug_unit,'(/t10,a,g13.6)')'L SET TO: ',sfc_data(d)%mol(h)
                                
                            endif
                        endif
                    endif
                    if (.not. onhflux .and. sfc_data(d)%ustar(h) > 0.0_r8 .and. sfc_data(d)%theta_star(h) > 0.0_r8) then
                        sfc_data(d)%hflux(h)=-sfc_data(d)%theta_star(h)*rho(h,d)*cp*sfc_data(d)%ustar(h)
                        if (debug)write(debug_unit,debugform(10))'HEAT FLUX CALCULATED FROM DENSITY, Cp, U*, and THETA-STAR',&
                            'DENSITY   Cp        U*        THETA-STAR    HEAT FLUX',rho(h,d),cp,sfc_data(d)%ustar(h),&
                            sfc_data(d)%theta_star(h),sfc_data(d)%hflux(h)  
                    endif
!                   back calculate net radiation
!                   assign or calculate net radiation
                    lcycle=.false.
                    if (.not. onnrad)call netrad(d,h,lcycle)
                    if (lcycle) cycle h3

                endif !end convective/stable if/then logic
                
!               reset large absolute values of L to 8888. indicates near neutral conditions. also
!               reset to absolute value of 1.0 if low.
        
                if (dabs(sfc_data(d)%mol(h)-(-99999._r8)) > eps) then
!                   reset absolute values to 8888 for stable hours, -8888 for convective hours
!                   sign is based on current sign of L
                    if (dabs(sfc_data(d)%mol(h)) > 8888.0_r8)then
                        sfc_data(d)%mol(h)=8888.0_r8*dsign(1.0_r8,sfc_data(d)%mol(h))
                        if (debug)write(debug_unit,'(/t10,a,f7.1)')'LARGE ABSOLUTE VALUE OF L; L RESET TO: ',sfc_data(d)%mol(h)
                    endif
!                   reset values with absolute values < 1.0
                    if (dabs(sfc_data(d)%mol(h)) < 1.0_r8)then
                        sfc_data(d)%mol(h)=1.0_r8*dsign(1.0_r8,sfc_data(d)%mol(h))
                        if (debug)write(debug_unit,'(/t10,a,f4.1)')'SMALL ABSOLUTE VALUE OF L; L RESET TO: ',sfc_data(d)%mol(h)
                    endif
                endif
                
!               calculate mechanical mixing height if no onsite mixing heights
!               only calculate if u* not missing.  mechanical height has already been set to missing in sfc_data initialization
!               so if u* missing then no further processing needed on mechanical heights.
                if (.not. onmix .and. dabs(sfc_data(d)%ustar(h)-(-9.0_r8)) > eps) call mech_ht(d,h)
                
            enddo h3

 
!           check to see if heat flux conditions are met to calculate mixing heights
!           need at least one hour of positive heat flux and first convective hour is not missing
!           if first convective hour is missing, all hours are missing heat flux, or no positive heat
!           flux for the day, don't calculate mixing heights
    
            if (calc_conv .and. (.not. posflux .or. dabs(sfc_data(d)%hflux(cblhr1)-(-999.0_r8)) < eps .or. &
                maxval(sfc_data(d)%hflux) < homin)) then
                calc_conv=.false. 
                write(msg_unit,formstr(6))adjustl(pathid(ipath)),'I77',modnam,'HEAT FLUX CONDITIONS NOT MET FOR DATE:',&
                    sfc_data(d)%sfcdate,' NO CONVECTIVE PARAMETERS WILL BE CALCULATED'
            endif
!           begin convective mixing height calculations, if at least 1 convective hour doesn't have onsite mixing heights.
!           if there are onsite mixing heights, convective heights are handled in the h3 loop
            
            if (calc_conv) then
                if (up_obs(d)) call conv_ht(d,cblhr1) !then
            endif
            
!           write to profile and surface file
            call writefiles(d)
            
            if (allocated(tempk)) deallocate(tempk)
            if (allocated(theta)) deallocate(theta)
            if (allocated(pres)) deallocate(pres)
            if (allocated(ht)) deallocate(ht)
            
        enddo d3
    endif
    
    deallocate(windsrc)
    deallocate(tempsrc)
    deallocate(cloudsrc)
    deallocate(up_sunrise)
    deallocate(up_sunset)
    deallocate(local_sunrise)
    deallocate(local_sunset)
    deallocate(sun_ang)
    deallocate(day_obs)
    deallocate(up_obs)
    deallocate(asos_hr)
    deallocate(nws_obs)
    deallocate(os_obs)
    deallocate(one_min_obs)
    deallocate(got_eq_ccvr)
    deallocate(cbl)
    deallocate(rho)
    deallocate(pfl_nlevels)
    if (allocated(iup)) deallocate(iup)
    if (allocated(isnd)) deallocate(isnd)
    if (allocated(isf)) deallocate(isf)
    if (allocated(isf1)) deallocate(isf1)
    if (allocated(ios)) deallocate(ios)
    
    return
    end subroutine pbl_proc
!*********************************************************************************************************
    
    subroutine freq_sec(i1,isite,nfreq,nsec,lskip)
!=========================================================================================================
!   SUBROUTINE FREQ_SEC
!   THIS SUBROUTINE PROCESSES THE LINE FREQ_SECT OR FREQ_SECT FROM THE RUNSTREAM INPUT FILE
!   OR EXTERNAL FILE CONTAINING SURFACE CHARACTERISTICS
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (AERSURF)
!
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!   ISITE:          INTEGER INDICATOR OF SITE TYPE FOR SURFACE CHARACTERISTICS
!                   PROCESSING AND MESSAGE (1=PRIMARY, 2=SECONDARY)
! 
!   OUTPUT ARGUMENTS
!
!   NFREQ:          TEMPORAL FREQUENCY OF SURFACE CHARACTERISTICS
!   NSEC:           NUMBER OF SECTORS FOR SURFACE CHARACTERISTICS
!   LSKIP:          DENOTES TO SKIP PROCESSING BECAUSE THESE CHARACTERISTICS
!                   ARE NOT FOR THE YEAR(S) BEING PROCESSED
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   i1:             starting index to read from input line
!   isite:          integer indicator of site type for surface characteristics
!                   processing and message (1=primary, 2=secondary)
!   ilen1:          length of line after removing trailing blanks
!   istart:         first non-blank character in the input line
!   nfield1:        number fields on line
!   ifreq:          frequency of surface characteristics
!   isec:           number of surface sectors
!   iflag:          IO flag   
!   nfreq:          temporal frequency of surface characteristics
!   nsec:           number of sectors for surface characteristics
!   iyr1:           first year in a range of years if found
!   iyr2:           second year in a range of years if found
!   ifield:         field counter
!   idash:          index of - in string of years
!
!   Logical variables
!   lskip:          denotes to skip processing because these characteristics
!                   are not for the year(s) being processed
!   lfound:         denotes if first year found in list of years
!
!   Character variables
!   formstr:        format for messages
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: inpline1,nfield,getfields,writeunit,msg_form
    implicit none
    integer(kind=4), intent(in) :: i1,isite
    integer(kind=4), intent(out) :: nfreq,nsec
    integer(kind=4) :: i,ilen1,istart,nfield1,ifreq,isec,iflag,iyr1,iyr,idash,yr1,yr2,ifield
   
    logical, intent(out) :: lskip
    logical :: lfound
    character(len=60) :: formstr(3)
    character(len=10) :: modnam='FREQ_SEC'
      
    character(len=100),allocatable, dimension(:) :: datafield
      
!   formats for messages
!   1.  invalid number of fields
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'i3,1x,a)'

!   2.  invalid frequency or sector number
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,3(1x,a))'

!   3.  number of sectors exceed bounds
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),i3,1x,a,i3)'

    isec=0
    ifreq=0
    lskip=.true.
    nfreq=0
    nsec=0
!   because this subroutine reads the line from either the runstream file
!   or external file of surface characteristics need to get number of fields
!   for reading from the runstream file, this normally has already been done
!   but since can possibly read from an external file, need to get fields

!   find first non-blank character to account for indented text
!   read through line until non-blank found
    ilen1=len_trim(inpline1)
    i=1
    istart=0
    do while(i <= ilen1 .and. istart == 0)
        if (ichar(inpline1(i:i)) /= 32) istart=i
        i=i+1
    enddo
    nfield1=1 
      
!   get # of fields in line by looking for blank characters between non-blank characters
    l1: do i=istart,ilen1-1
        if (ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32) nfield1=nfield1+1
    enddo l1
    
!   check to make sure number of nfields=3
!   note if reading from the runstream file, this has already been done
!   so shouldn't get an error here
    
    if (nfield1 < 3) then
        write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,nfield,&
            'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD FREQ_SECT'
        lbad=.true.
    else
        nfield1=nfield1-1 !subtract 1 to account for keyword
        allocate(datafield(nfield1))
        datafield='0000000000000000000000000'
!        inpline1=line !set inpline1 to line since getfields uses inpline1
          
!       read input line to sector type (ANNUAL, SEASONAL, or MONTHLY) and number of sectors
        call getfields(i1,nfield1,datafield)
     
        if (nfield1 > 2) then
!           we have year assignments.  because multiple years could be reading the same
!           file, i.e. there are two years and the two different surface characteristics
!           are defined in the same file, AERMET needs to check to make sure that the
!           years being processed for this particular instance of the file are in fact
!           the years being currently read in.
            lfound=.false.
            ifield=3
!           search to see if first year is in the list of years for this particular instance of FREQ_SECT
            do while(ifield <= nfield1 .and. .not. lfound)
                if (index(datafield(ifield),'-') > 0) then
!                   checks for correct syntax and that first year < 2nd year done in surf_years
                    idash=index(datafield(ifield),'-')
                    read(datafield(ifield)(idash-4:idash-1),*)yr1
                    read(datafield(ifield)(idash+1:idash+4),*)yr2
!                   check to see if first year associated with the file containing this FREQ_SECT is 
!                   part of this range
                    if (yr1 <= sc_years(1) .and. sc_years(2) <= yr2) lfound=.true.
                else
                    read(datafield(ifield),*)yr1
                    if (sc_years(1) == yr1) lfound=.true.
                endif
                if (.not. lfound)ifield=ifield+1
            enddo 
            if (lfound) then
                lskip=.false.
            else
                lskip=.true.
            endif
        else
            lskip=.false.
        endif
        
        if (.not. lskip) then
!           this block is for the year or years being processed for this set of 
!           characteristics
            if (trim(adjustl(datafield(1))) == 'ANNUAL') then
                ifreq=1
            elseif (trim(adjustl(datafield(1))) == 'MONTHLY') then
                ifreq=12
            elseif (trim(adjustl(datafield(1))) == 'SEASONAL') then
                ifreq=4
            else
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E75',modnam,'INVALID FREQUENCY FOR',&
                    trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',trim(adjustl(datafield(1)))
                lbad=.true.
            endif
      
!           get number of sectors
            read(datafield(2),*,iostat=iflag)isec
            if (iflag == 0) then
                if (isec < minsec .or. isec > maxsec) then
                    if (isec < minsec) then
                        write(writeunit,formstr(3))adjustl(pathid(ipath)),'E75',modnam,trim(adjustl(sites(isite))),&
                            'SITE NUMBER OF SECTORS:',isec,'<',minsec
                    else
                        write(writeunit,formstr(3))adjustl(pathid(ipath)),'E75',modnam,trim(adjustl(sites(isite))),&
                            'SITE NUMBER OF SECTORS:',isec,'>',maxsec
                    endif
                    lbad=.true.
                endif
            else
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E75',modnam,'INVALID SECTOR NUMBER FOR',&
                    trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',trim(adjustl(datafield(2)))
                lbad=.true.
            endif
      
!       assign ifreq and isec to appropriate frequency and sector variables; allocate arrays
            if (.not. lbad) then
                nfreq=ifreq
                nsec=isec
    l2:         do iyr=1,nsc_years
                    iyr1=sc_years(iyr)-pbldates(1,1)+1
!                   only process if within the data processing period
                    if (iyr1 > 0 .and. iyr <= (pbldates(2,1)-pbldates(1,1)+1)) then
!                       update nfreq1 and nsectors1 or nfreq2 and nsectors2
                        if (isite==1) then
                            nfreq1(iyr1)=ifreq
                            nsectors1(iyr1)=isec
                            
                        else
                            nfreq2(iyr1)=ifreq
                            nsectors2(iyr1)=isec
                        endif
                    endif
                enddo l2
            endif
        endif
    endif
     
    if (allocated(datafield)) deallocate(datafield)

    return
    end subroutine freq_sec
      
!*********************************************************************************************************
      
    subroutine site_char(i1,isite,nfreq,nsec)
!=========================================================================================================
!   SUBROUTINE SITE_CHAR
!   THIS SUBROUTINE PROCESSES THE LINE SITE_CHAR OR SITE_CHAR2 FROM THE RUNSTREAM INPUT FILE
!   OR EXTERNAL FILE CONTAINING SURFACE CHARACTERISTICS
!
!   MODIFIED DECEMBER 3, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL_PROC (AERSURF)
!
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!   ISITE:          INTEGER INDICATOR OF SITE TYPE FOR SURFACE CHARACTERISTICS
!                   PROCESSING AND MESSAGE (1=PRIMARY, 2=SECONDARY)
!   NFREQ:          TEMPORAL FREQUENCY OF SURFACE CHARACTERISTICS
!   NSEC:           NUMBER OF SECTORS FOR SURFACE CHARACTERISTICS
!        
!   Variable definitions
!      
!   Integer variables
!   isite:          integer indicator of site type for surface characteristics
!                   processing and message (1=primary, 2=secondary)
!   i:              loop counter
!   i1:             starting index to read from input line
!   ilen1:          length of line after removing trailing blanks
!   istart:         first non-blank character in the input line
!   nfield1:        number fields on line
!   ifreq:          frequency of surface characteristics
!   isec:           number of surface sectors
!   iflag:          IO flag   
!   nfreq:          temporal frequency of surface characteristics
!   nsec:           number of sectors for surface characteristics
!   iyr:            year loop counter
!   iyr1:           number of years since first year of data period
!
!   Real variables
!   albedo:         albedo
!   bowen:          Bowen ratio
!   zo:             surface roughness
!
!   Logical variables
!   goodfreq:       genotes frequency index is good (true) or bad (false)
!   goodsec:        denotes if sector index is good (true) or bad (false)
!
!   Character variables
!   formstr:        formats for messages
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: inpline1,nfield,getfields,writeunit,msg_form

    implicit none
    integer(kind=4), intent(in) :: i1,isite,nfreq,nsec
    integer(kind=4) :: i,ilen1,istart,nfield1,ifreq,isec,iflag,iyr,iyr1
    real(kind=r8) :: albedo=-999.0
    real(kind=r8) :: bowen=-999.0
    real(kind=r8) :: zo=-999.0
      
    logical :: goodfreq=.false.
    logical :: goodsec=.false.
    character(len=60) :: formstr(6)
    character(len=10) :: modnam='SITE_CHAR'
      
    character(len=100), allocatable, dimension(:) :: datafield
  
!   formats for messages
!   1.  invalid number of fields
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'i3,1x,a)'
    
!   2.  invalid frequency (numeric) or sector number (numeric)
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),i3)'
    
!   3.  invalid frequency (non-numeric) or sector number (non-numeric)
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,3(1x,a))'

!   4.  invalid numeric value for surface characteristic or suspect Bowen ratio
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,a,1x,a,1x,f8.2)'

!   5.  invalid non-numeric value for surface characteristic
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,3(1x,a))'
!   6.  zo < 0.0001 or suspect
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),f8.6,1x,a)'

    isec=0
    ifreq=0
      
!   because this subroutine reads the line from either the runstream file
!   or external file of surface characteristics need to get number of fields
!   for reading from the runstream file, this normally has already been done
!   but since can possibly read from an external file, need to get fields

!   find first non-blank character to account for indented text
!   read through line until non-blank found
    ilen1=len_trim(inpline1)
    i=1
    istart=0
    do while(i <= ilen1 .and. istart == 0)
        if (ichar(inpline1(i:i)) /= 32) istart=i
        i=i+1
    enddo
    nfield1=1 
      
!   get # of fields in line by looking for blank characters between non-blank characters
    l1: do i=istart,ilen1-1
        if (ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32) nfield1=nfield1+1
    enddo l1
      
!   check to make sure number of nfields=3
!   note if reading from the runstream file, this has already been done
!   so shouldn't get an error here
    if (nfield1 /=6) then
        write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,nfield,&
            'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD SITE_CHAR'
        lbad=.true.
    else
        nfield1=nfield1-1 !subtract 1 to account for keyword
        allocate(datafield(nfield1))
        datafield='0000000000000000000000000'
!        inpline1=line !set inpline1 to line since getfields uses inpline1
        call getfields(i1,nfield1,datafield)     
!       get frequency
        read(datafield(1),*,iostat=iflag)ifreq
        if (iflag == 0) then
            if (ifreq < 1 .or. ifreq > nfreq) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E75',modnam,'INVALID FREQUENCY FOR',&
                    trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',ifreq
                lbad=.true.
            else
                goodfreq=.true.
            endif
        else
            write(writeunit,formstr(3))adjustl(pathid(ipath)),'E75',modnam,'INVALID FREQUENCY FOR',&
                    trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',trim(adjustl(datafield(1)))
            lbad=.true.
        endif
          
!       get sector number
        read(datafield(2),*,iostat=iflag)isec
        if (iflag == 0) then
            !if (isec < minsec .or. isec > nsectors) then
            if (isec < minsec .or. isec > nsec) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E75',modnam,'INVALID SECTOR NUMBER FOR',&
                    trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',isec
                lbad=.true.
            else
                goodsec=.true.
            endif
        else
            write(writeunit,formstr(3))adjustl(pathid(ipath)),'E75',modnam,'INVALID SECTOR NUMBER FOR',&
                    trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',trim(adjustl(datafield(2)))
            lbad=.true.
        endif  
          
!       get albedo
        read(datafield(3),*,iostat=iflag)albedo
        if (iflag == 0) then
            if (albedo < 0.0 .or. albedo > 1.0) then
                write(writeunit,formstr(4))adjustl(pathid(ipath)),'E75',modnam,'INVALID ALBEDO FOR',trim(adjustl(sites(isite))),&
                    'SITE SURFACE CHARACTERISTICS:',albedo
                lbad=.true.
            endif
        else
            write(writeunit,formstr(5))adjustl(pathid(ipath)),'E75',modnam,'INVALID ALBEDO FOR',trim(adjustl(sites(isite))),&
                    'SITE SURFACE CHARACTERISTICS:',trim(adjustl(datafield(3)))
            lbad=.true.
        endif 
          
!       get bowen ratio
        read(datafield(4),*,iostat=iflag)bowen
        if (iflag == 0) then
!           warn user if bowen ratio < -10 or bowen ratio > 10 and not missing
            if (dabs(bowen) > 10.0_r8 .and. bowen .ne. -999.0_r8) then
                write(writeunit,formstr(4))adjustl(pathid(ipath)),'W75',modnam,'BOWEN RATIO FOR',trim(adjustl(sites(isite))),&
                    'MAY BE SUSPECT:',bowen
            elseif (bowen == -999.0) then
                write(writeunit,formstr(4))adjustl(pathid(ipath)),'E75',modnam,'INVALID BOWEN RATIO FOR',&
                    trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',bowen
                lbad=.true.
            endif
        else
            write(writeunit,formstr(5))adjustl(pathid(ipath)),'E75',modnam,'INVALID BOWEN RATIO FOR',trim(adjustl(sites(isite))),&
                    'SITE SURFACE CHARACTERISTICS:',trim(adjustl(datafield(4)))
            lbad=.true.
        endif 
          
!       get surface roughness
        read(datafield(5),*,iostat=iflag)zo
        if (iflag == 0) then
            if (zo <= 0.0) then
                write(writeunit,formstr(4))adjustl(pathid(ipath)),'E75',modnam,'INVALID Z0 FOR',&
                    trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',zo
                lbad=.true.
            elseif (zo < 0.0001_r8) then
                write(writeunit,formstr(6))adjustl(pathid(ipath)),'W75',modnam,'Z0 FOR',trim(adjustl(sites(isite))),' < 0.0001:',&
                    zo,'RESET TO 0.0001'
                
                zo=0.0001_r8
            elseif (zo < 0.001_r8) then
                write(writeunit,formstr(6))adjustl(pathid(ipath)),'W75',modnam,'Z0 FOR',trim(adjustl(sites(isite))),' < 0.001:',&
                    zo,'MAY BE SUSPECT'
            endif
            
        else
            write(writeunit,formstr(5))adjustl(pathid(ipath)),'E75',modnam,'INVALID Z0 FOR',trim(adjustl(sites(isite))),&
                    'SITE SURFACE CHARACTERISTICS:',trim(adjustl(datafield(5)))
            lbad=.true.
        endif 
        if (.not. lbad .and. goodsec .and. goodfreq) then !write to arrays
    l2:     do iyr=1,nsc_years 
                iyr1=sc_years(iyr)-pbldates(1,1)+1
!               only process if within the data processing period
                if (iyr1 > 0 .and. iyr <= (pbldates(2,1)-pbldates(1,1)+1)) then
!                   assign values to temporary arrays
                    talbedo(isec,ifreq,iyr1)=albedo
                    tbowen(isec,ifreq,iyr1)=bowen
                    tzo(isec,ifreq,iyr1)=zo
                endif
            enddo l2
        endif
    endif
       
    if (allocated(datafield)) deallocate(datafield)
      
    return
    end subroutine site_char
      
!********************************************************************************************************
      
    subroutine sectors(i1,isite,nsec)
!=========================================================================================================
!   SUBROUTINE SECTORS
!   THIS SUBROUTINE PROCESSES THE LINE SECTOR OR SECTOR2 FROM THE RUNSTREAM INPUT FILE
!   OR EXTERNAL FILE CONTAINING SURFACE CHARACTERISTICS
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (SFC_CHARS, AERSURF)
!
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!   ISITE:          INTEGER INDICATOR OF SITE TYPE FOR SURFACE CHARACTERISTICS
!                   PROCESSING AND MESSAGE (1=PRIMARY, 2=SECONDARY)
!   NSEC:           NUMBER OF SECTORS FOR SURFACE CHARACTERISTICS
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   isite:          integer indicator of site type for surface characteristics
!                   processing and message (1=primary, 2=secondary)
!   i1:             starting index to read from input line
!   ilen1:          length of line after removing trailing blanks
!   istart:         first non-blank character in the input line
!   nfield1:        number fields on line
!   isec:           surface sector being read
!   iflag:          IO flag   
!   dir:            2-element array of starting and ending direction read with SECTOR keyword
!   nsec:           number of sectors for surface characteristics
!   iyr:            year loop counter
!   iyr1:           number of years since first year of data period
!
!   Logical variables
!   goodsec:        denotes if sector index is good (true) or bad (false)
!
!   Character variables
!   formstr:        format for messages
!   datafield:      data for FREQ_SECT or FREQ_SECT2 keyword read from input line
!   dirs:           character string denoting if starting or ending direction for sector messaging
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: inpline1,nfield,getfields,writeunit,msg_form
    implicit none
    integer(kind=4), intent(in) :: i1,isite,nsec
    integer(kind=4) :: i,ilen1,istart,nfield1,isec,iflag,dir(2),iyr,iyr1

    logical :: goodsec=.false.
    character(len=8) :: dirs(2)
    character(len=10) :: modnam='SECTORS'
    character(len=60) :: formstr(5)
    character(len=100), allocatable, dimension(:) :: datafield
 
      
    data dirs /'STARTING','ENDING'/
    
!   formats for messages
!   1.  invalid number of fields
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'i3,1x,a)'
    
!   2.  invalid numeric sector number
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),i3)'

!   3.  invalid non-numeric sector number
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,3(1x,a))'

    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'5(a,1x),i4)'   
!   4.  invalid numeric wind direction

!   5.  invalid non-numeric wind direction
   write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,5(1x,a))'
     
    isec=0
    dir=-999
      
!   because this subroutine reads the line from either the runstream file
!   or external file of surface characteristics need to get number of fields
!   for reading from the runstream file, this normally has already been done
!   but since can possibly read from an external file, need to get fields

!   find first non-blank character to account for indented text
!   read through line until non-blank found
    ilen1=len_trim(inpline1)
    i=1
    istart=0
    do while(i <= ilen1 .and. istart == 0)
        if (ichar(inpline1(i:i)) /= 32) istart=i
        i=i+1
    enddo
    nfield1=1 
      
!   get # of fields in line by looking for blank characters between non-blank characters
    l1: do i=istart,ilen1-1
        if (ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32) nfield1=nfield1+1
    enddo l1
      
!   check to make sure number of nfields=3
!   note if reading from the runstream file, this has already been done
!   so shouldn't get an error here
    if (nfield1 /=4) then
        write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD SECTOR'
        lbad=.true.
    else
        nfield1=nfield1-1 !subtract 1 to account for keyword
        allocate(datafield(nfield1))
        datafield='0000000000000000000000000'
        call getfields(i1,nfield1,datafield)              
!       get sector number
        read(datafield(1),*,iostat=iflag)isec
        if (iflag == 0) then
            !if (isec < minsec .or. isec > nsectors) then
            if (isec < minsec .or. isec > nsec) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E75',modnam,'INVALID SECTOR NUMBER FOR',&
                    trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',isec
                lbad=.true.
            else
                goodsec=.true.
            endif
        else
            write(writeunit,formstr(3))adjustl(pathid(ipath)),'E75',modnam,'INVALID SECTOR NUMBER FOR',&
                trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',trim(adjustl(datafield(1)))
            lbad=.true.
        endif  
          
!       get directions

        l2: do i=1,2
            
            read(datafield(i+1),*,iostat=iflag)dir(i)
            if (iflag == 0) then
                if (dir(i) < 0 .or. dir(i) > 360) then
                    write(writeunit,formstr(4))adjustl(pathid(ipath)),'E75',modnam,'INVALID',trim(adjustl(dirs(i))),&
                    'DIRECTION FOR',trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',dir(i)
                    lbad=.true.
                else
                    if (goodsec) then
    l3:                 do iyr=1,nsc_years
                            iyr1=sc_years(iyr)-pbldates(1,1)+1
!                           only process if within the data processing period
                            if (iyr1 > 0 .and. iyr <= (pbldates(2,1)-pbldates(1,1)+1)) then
!                               set to the temporary tsectors array
                                tsectors(i,isec,iyr1)=real(dir(i),r8)
                              
                            endif
                        enddo l3
                    endif
                endif
            else
                write(writeunit,formstr(5))adjustl(pathid(ipath)),'E75',modnam,'INVALID',trim(adjustl(dirs(i))),&
                    'DIRECTION FOR',trim(adjustl(sites(isite))),'SITE SURFACE CHARACTERISTICS:',trim(adjustl(datafield(i+1)))
                
                lbad=.true.
            endif 
        enddo l2
    endif
          
    if (allocated(datafield)) deallocate(datafield)
            
    return
    end subroutine sectors
!*********************************************************************************************************
      
    subroutine methods(i1)
!=========================================================================================================
!   SUBROUTINE METHODS
!   THIS SUBROUTINE PROCESSES THE LINE METHOD FROM THE RUNSTREAM INPUT FILE
!
!   MODIFIED DECEMBER 3, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PATH)
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
!   item:           item number
!   iaction:        action number
!   iword1:         word number to use for word1 for messages
!   iword2:         word number to use for word2 for messages
!
!   Logical variables
!   lfound:         logical variable denoting variable name found in surface listc
!   gooditem:       logical variable denoting if ITEM is okay
!   goodaction:     logical variable denoting if ACTION is okay
!
!   Character variables
!   datafield:      data for DELTA_TEMP keyword read from input line
!   word1:          first text string for messages
!   word2:          second text string for messages
!   formstr:        format for messages
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: nfield,keywrd,getfields,writeunit,items,actions,ikey,msg_form
    implicit none
    integer(kind=4), intent(in) :: i1
    integer(kind=4) :: nfield1,item,iaction,iword1,iword2
    logical lfound,gooditem,goodaction
    character(len=10) :: modnam='METHODS'
    character(len=60) :: formstr(3)
    character(len=30) :: word1(3),word2(3)
    character(len=100), allocatable, dimension(:) :: datafield
      
!   text strings for messages
    data word1 /'DUPLICATE ENTRY OF KEYWORD','INVALID PROCESS','INVALID PARAMETER'/
    data word2 /'PROCESS','FOR KEYWORD','PARAMETER'/
    
!   formats for messages
!   1.	duplicate entry, invalid entry
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,3(1x,a))'
    
!   2. invalid combination
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,2(1x,a))'
    
!   3.  obsolete parameter
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),2(a))'
    
    nfield1=nfield-1
      
    allocate(datafield(nfield1))
    datafield='0000000000000000000000000'
      
    call getfields(i1,nfield1,datafield)   


!   get the process or item from datafield(1)
    item=1
    lfound=.false.
    do while (item <=nitems .and. .not. lfound)
        if (trim(adjustl(datafield(1))) == trim(adjustl(items(item)))) then
            lfound=.true.
        else
            item=item+1
        endif
    enddo
      
!   check to see if process has been already found
!   STABLEBL is allowed twice since it can be used for
!   the ADJ_U* and BULKRN options
    if (lfound) then
        if (litems(item) .and. item /=3) then
            iword1=1
            iword2=1
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,trim(adjustl(word1(iword1))),&
                trim(adjustl(keywrd(ikey))),trim(adjustl(word2(iword2))),trim(adjustl(items(item)))
            lbad=.true.
            gooditem=.false.
        else
            litems(item)=.true.
            gooditem=.true.
        endif
    else
        iword1=1
        iword2=2
        write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(word1(iword1))),trim(adjustl(datafield(1))),&
            trim(adjustl(word2(iword2))),trim(adjustl(keywrd(ikey)))
        lbad=.true.
        gooditem=.false.
    endif
      
!   get the action from datafield(2)
    iaction=1
    lfound=.false.
    do while (iaction <=nactions .and. .not. lfound)
        if (trim(adjustl(datafield(2))) == trim(adjustl(actions(iaction)))) then
            lfound=.true.
        else
            iaction=iaction+1
        endif
    enddo
!   check to see if SUNRIS listed instead of SUNRISE
    if (.not. lfound .and. trim(adjustl(datafield(2))) == 'SUNRIS') then
        lfound=.true.
        iaction=6
    endif
    

!   check to see if parameter has already been found
!   ignore duplicate error if action is NOPERS (not used)
    if (lfound) then
        if (iaction /= nactions) then
            if (lactions(iaction)) then
                iword1=1
                iword2=3
                write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,trim(adjustl(word1(iword1))),&
                    trim(adjustl(keywrd(ikey))),trim(adjustl(word2(iword2))),trim(adjustl(actions(iaction)))
                lbad=.true.
                goodaction=.false.
            else
                lactions(iaction)=.true.
                goodaction=.true.
            endif
        else
!           if NOPERS listed, issue warning to user that it will not be used since persistence no longer used
            write(writeunit,formstr(3))adjustl(pathid(ipath)),'W81',modnam,'OBSOLETE PARAMETER',trim(adjustl(actions(iaction))),&
                trim(adjustl(word2(2))),trim(adjustl(keywrd(ikey))),'; IGNORE'
        endif
    else
        iword1=3
        iword2=2
        write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(word1(iword1))),trim(adjustl(datafield(2))),&
            trim(adjustl(word2(iword2))),trim(adjustl(keywrd(ikey)))
        lbad=.true.
        goodaction=.false.
    endif
    
!   now check a valid combination of process and parameter
!   valid pairings
!   item        action
!   1           1,2
!   2           3
!   3           4,7
!   4           5
!   5           6
!   6           8,9,12
!   7           10,11,12
    
    if (gooditem .and. goodaction) then
        if ((item==1 .and. iaction > 2) .or. ((item == 2 .or. item == 4  .or. item == 5) .and.  iaction /= item +1).or. &
            (item == 3 .and. iaction /=4 .and. iaction /= 7) .or. (item == 6 .and. iaction /=8 .and. iaction /= 9 .and. &
            iaction /=12) .or. (item == 7 .and. iaction /=10 .and. iaction /=11 .and. iaction /=12)) then
            write(writeunit,formstr(2))adjustl(pathid(ipath)),'E77',modnam,'INVALID COMBINATION OF',trim(adjustl(items(item))),&
                trim(adjustl(actions(iaction)))
            lbad=.true.
        endif
    endif

    deallocate(datafield)

    return
    end subroutine methods
!*********************************************************************************************************
      
    subroutine up_window(i1)
!=========================================================================================================
!   SUBROUTINE UP_WINDOW
!   THIS SUBROUTINE PROCESSES THE LINE UAWINDOW FROM THE RUNSTREAM INPUT FILE
!
!   MODIFIED DECEMBER 3, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PATH)
!
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   Variable definitions
!      
!   Integer variables
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   i:              loop counter
!   nfield1:        number fields on line minus 1 to account for keyword
!   iflag:          IO flag
!   times:          2-element array of hours before and after to check for sounding
!
!   Character variables
!   datafield:      data for DELTA_TEMP keyword read from input line
!   formstr:        formats for messages
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: nfield,getfields,writeunit,msg_form
    implicit none
    integer(kind=4), intent(in):: i1
    integer(kind=4) :: i,nfield1,iflag,times(2)
    character(len=10) :: modnam='UP_WINDOW'
    character(len=60) :: formstr(3)
    character(len=100), allocatable, dimension(:) :: datafield
    
      
!   formats for mesages
!   1.  window exeeds 12 hours or beginning hour positive or negative
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,1x,a)'

!   2.  invalid value
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'

!   3.  beginning hour > ending hour
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,1x,a,1x,i3)'

    nfield1=nfield-1
    times=0
      
    allocate(datafield(nfield1))
    datafield='0000000000000000000000000'
      
    call getfields(i1,nfield1,datafield)   

!   get hours
!   beginning hour
    read(datafield(1),*,iostat=iflag)times(1)
    if (iflag == 0) then
        if (times(1) < -12) then
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'UAWINDOW HOUR',times(1),'EXCEEDS 12 HRS'
            lbad=.true.
        elseif (times(1) > 0) then
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'UAWINDOW BEGINNING HOUR',times(1),'IS POSITIVE'
            lbad=.true.
        endif
    else
        write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR UAWINDOW HOUR:',&
            trim(adjustl(datafield(1)))
        lbad=.true.
    endif
      
!   ending hour
    read(datafield(2),*,iostat=iflag)times(2)
    if (iflag == 0) then
        if (times(2) > 12) then
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'UAWINDOW HOUR',times(2),'EXCEEDS 12 HRS'
            lbad=.true.
        elseif (times(2) < 0) then
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'UAWINDOW ENDING HOUR',times(1),'IS NEGATIVE'
            lbad=.true.
        elseif (times(2) < times(1)) then
            write(writeunit,formstr(3))adjustl(pathid(ipath)),'E78',modnam,'UAWINDOW BEGINNING HOUR',times(1),&
                'IS GREATER THAN ENDING HOUR',times(2)
            lbad=.true.
        endif
    else
        write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR UAWINDOW HOUR:',&
            trim(adjustl(datafield(2)))
        lbad=.true.
    endif
    if (.not. lbad) then
    l1:    do i=1,2
            snding_win(i)=times(i)
        enddo l1
    endif
      
    deallocate(datafield)
    
    return
    end subroutine up_window
!*********************************************************************************************************
      
    subroutine aersurf(iswitch,isite,lbadsc)
!=========================================================================================================
!   SUBROUTINE AERSURF
!   THIS SUBROUTINE READS THE AERSURF OR AERSURF2 FILE TO GET SURFACE CHARACTERISTICS
!   IF AERSURF OR AERSURF2 NOT USED, THEN READ THE RUNSTREAM FILE
!
!   MODIFIED MARCH 9, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC, PBL_TEST)
!
!   INPUT ARGUMENTS 
!
!   ISWITCH:    SWITCH TO INDICATE WHETHER TO JUST CHECK YEARS OR PROCEED WITH FULL SURFACE CHARACTERISTICS
!               ASSIGNMENT
!               ISWITCH=1, CHECK FOR DUPLICATE YEARS ONLY
!               OTHERWISE, FULL CALCULATIONS
!   ISITE:          INTEGER INDICATOR OF SITE TYPE FOR SURFACE CHARACTERISTICS
!                   PROCESSING AND MESSAGE (1=PRIMARY, 2=SECONDARY)
!   LBADSC:     BAD SURFACE CHARACTERISTICS; DUPLICATE YEARS OR OTHER PROBLEMS
!
!   Variable definitions
!      
!   Integer variables
!   iswitch:        switch to indicate whether to just check years or proceed with full surface characteristics
!                   assignment
!   isite:          integer indicator of site type for surface characteristics
!                   processing and message (1=primary, 2=secondary)
!   i:              index value of keyword
!   i2:             loop counter
!   i3:             index of lower case letter in inpline1 from lower character string
!   ikey1:          keyword index in sc_keys
!   eof:            integer end of file indicator to read runstream input file
!   i3:              value of index function used throughout subroutine
!   eof:            end of file indicator
!   iunit:          file unit number
!   nfiles:         number of files to read
!   ifiles:         file loop counter
!   nfreq:          temporal frequency for a particular set of surface characteristics
!   nsec:           particular number of sectors for a set of surface characteristics
!   iyr:            year value
!   iyr1:           number of years between iyr and pbl start year
!   freq:           max value of nfreq1 or nfreq2
!   sec:            max value of nsectors1 or nsectors2
!   ifreq:          frequency loop counter
!   isec:           sector loop counter
!   k:              SC keyword loop counter
!   id:             starting/ending direction loop counter
!
!   Logical variables
!   lbadsc:         bad surface characteristics; duplicate years or other problems
!   lfound:         keyword found
!   lfound1:        3-element array of the surface characteristics keywords
!                   first one has to be FREQ_SECT keyword is found
!   lskip:          denotes to skip processing because these characteristics
!                   are not for the year(s) being processed
!   lgo:            proceed with processing if years in sc_years is in PBL
!                   processing window
!   lfound2:		logical array denoting that a surface characteristics keyword found for site
!
!   Character variables
!   lower:          character string of lowercase letters
!   upper:          character string of uppercase letters
!   sc_keys:        list of surface characteristics keywords
!   akey:           character string of ikey1
!   fname:          file name
!   form1:          format to line from file
!   formstr:        formats for messages
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: linelen,inpline1,ilen,writeunit,lpath,msg_form
    use file_units, only: inp_unit,sfc_char_unit,sfc_charfile1,sfc_charfile2,msg_unit
    implicit none
    integer(kind=4), intent(in) :: iswitch,isite
    integer(kind=4) ::i,i1,i2,i3,ikey1,eof,iunit,nfiles,ifiles,nfreq,nsec,iyr,iyr1,freq,sec,ifreq,isec,id,k
    logical, intent(inout) :: lbadsc
    logical lfound,lfound1(3),lskip,lgo,lfound2(2,3)
    logical, allocatable,dimension (:) :: have_sc
    character(len=26) :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=26) :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!    character(len=linelen) :: line
    character(len=12) :: sc_keys(2,3)
    character(len=12) :: akey
    character(len=flength) :: fname
    character(len=6) :: form1
    character(len=60) :: formstr(6)
    character(len=10) :: modnam='AERSURF'
 
!   formats for messages
!   1.  invalid line
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2.   FREQ_SECT or FREQ_SECT2 not found
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a)'

!   3.  MMIF found while using ONSITE pathway or MMIF version found
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,2(1x,a))'

!   4.  MMIF version mismatch
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,3(1x,a))'
    
!   5.  a year's SC previously assigned or not assigned
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,i4,2(1x,a))'

!   6. didn't find one or more appropriate keywords for site
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'a,5(1x,a))'

!   allocate and initialize temporary arrays
    if (iswitch == 1) then
        allocate(have_sc(pbldates(2,1)-pbldates(1,1)+1))
        have_sc=.false.
    else
        allocate(talbedo(12,12,pbldates(2,1)-pbldates(1,1)+1))
        allocate(tbowen(12,12,pbldates(2,1)-pbldates(1,1)+1))
        allocate(tzo(12,12,pbldates(2,1)-pbldates(1,1)+1))
        allocate(tsectors(2,12,pbldates(2,1)-pbldates(1,1)+1))
        talbedo=-9.0_r8
        tbowen=-9.0_r8
        tzo=-9.0_r8
        tsectors=-9.0_r8
    endif

    lbadsc=.false.
    lfound2=.false.
    write(form1,'(a2,i3,a1)')'(a',linelen,')'
      
    sc_keys(1,1)='FREQ_SECT'
    sc_keys(1,2)='SECTOR'
    sc_keys(1,3)='SITE_CHAR'
    
    sc_keys(2,1)='FREQ_SECT2'
    sc_keys(2,2)='SECTOR2'
    sc_keys(2,3)='SITE_CHAR2'
    
    lfound1=.false.
    
    if (isite == 1) then
        nfiles=nsc_files1
    else
        nfiles=nsc_files2
    endif
    l1: do ifiles=1,nfiles
!       check to see which file will be used, the AERSURF(2) type file or runstream file
        if (lpblkeywords(isite+14)) then
!           use the AERSURF or AERSURF file, set iunit to the correct unit number
            iunit=sfc_char_unit(isite)
        else
            iunit=inp_unit
        endif
        if (isite == 1) then
            fname=sfc_charfile1(ifiles)
            call surf_years(yrstr1(ifiles))
        else
            fname=sfc_charfile2(ifiles)
            call surf_years(yrstr2(ifiles))
        endif
!       check to see if the years in this occurrence of sc_years is in the data window denoted by
!       METPREP XDATES and if the year already has a surface characteristics listed on another occurence
!       of surface characteristics
        if (iswitch == 1) then
        y1: do iyr1=1,nsc_years
                if (pbldates(1,1) <= sc_years(iyr1) .and. sc_years(iyr1) <= pbldates(2,1)) then
            
                    if (have_sc(sc_years(iyr1)-pbldates(1,1)+1)) then
!                       year already has SC assigned issue error
!                       an error is issued to allow the user to check for syntax in input file
                        lbadsc=.true.
                        write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E74',modnam,'YEAR',sc_years(iyr1),&
                            trim(adjustl(sites(isite))),'SURFACE CHARACTERSTICS PREVIOUSLY ASSIGNED'
                    else
                        have_sc(sc_years(iyr1)-pbldates(1,1)+1)=.true.
                    endif
                endif
            enddo y1
            if (lbadsc)lbad=.true.
        endif
        
!       check to see if the year or years with this file are within the XDATES for METPREP
        iyr1=1
        lgo=.false.
        do while (iyr1 <= nsc_years .and. .not. lgo)
            if (pbldates(1,1) <= sc_years(iyr1) .and. sc_years(iyr1) <= pbldates(2,1)) then
                lgo=.true.
            else
                iyr1=iyr1+1
            endif
        enddo
        
!       skip further processing if the years associated with this file are not in the data window
        if (.not. lgo) then
            deallocate(sc_years)
            cycle l1
        endif
        
        if (.not. lbad .and. iswitch /= 1) then
            open(unit=iunit,file=fname,status='old')
            eof=0
            do while(eof==0)
                read(iunit,form1,iostat=eof)inpline1
                ilen=len_trim(inpline1)
!               get MMIF version
                if (inpline1(1:2) == '**' .and. index(inpline1,'MMIF VERSION') > 0) then
                    if (lpath(4)) then
!						issue warning since ONSITE pathway
                        write(writeunit,formstr(3))adjustl(pathid(ipath)),'W79',modnam,'MMIF VERSION',&
                            trim(adjustl(inpline1(index(inpline1,'MMIF VERSION')+12:ilen))),'FOUND WHILE USING ONSITE PATHWAY'
                    else
!						issue information message since using PROG pathway
                        write(writeunit,formstr(3))adjustl(pathid(ipath)),'I83',modnam,'MMIF VERSION',&
                            trim(adjustl(inpline1(index(inpline1,'MMIF VERSION')+12:ilen))),'FOUND'
                    endif
!					determine MMIF version
                    if (.not. lmmif_versn) then
!						set MMIF version and notify user with message depending on lpath
                        mmif_versn=inpline1(index(inpline1,'MMIF VERSION')+12:ilen)
                        lmmif_versn=.true.
                    else
!						check to see if a different version of MMIF used
                        if (trim(adjustl(inpline1(index(inpline1,'MMIF VERSION')+12:ilen))) /= trim(adjustl(mmif_versn))) &
                            write(writeunit,formstr(4))adjustl(pathid(ipath)),'W80',modnam,'MMIF VERSION ',&
                            trim(adjustl(inpline1(index(inpline1,'MMIF VERSION')+12:ilen))),&
                            'DOES NOT MATCH CURRENT MMIF VERSION',trim(adjustl(mmif_versn))
                        mmif_versn=inpline1(index(inpline1,'MMIF VERSION')+12:ilen)
                    endif
                endif
                
                if (ilen > 0 .and. inpline1(1:2) /= '**') then
!                   convert to upper case
    l2:             do i2=1,ilen
                        i3=index(lower,inpline1(i2:i2))
                        if (i3 > 0) inpline1(i2:i2)=upper(i3:i3)
                    enddo l2
              
!                   find appropriate keyword
!                   since both site types, primary or secondary contain the same
!                   root keywords, don't need to search for FREQ_SECT2, SITE_CHAR2, or SECTOR2 at
!                   this point
!                   also check for correct order.  if the runstream file is being used this has already been done
!                   but hasn't been done for the AERSURF files.
                    if (index(inpline1,'FREQ_SECT') > 0 .or. index(inpline1,'SITE_CHAR') > 0 .or. &
                        index(inpline1,'SECTOR') > 0) then
                        read(inpline1,*)akey
                        ikey1=1
                        lfound=.false.
                        do while (ikey1 <=3 .and. .not. lfound)
                            if (trim(adjustl(akey)) == trim(adjustl(sc_keys(isite,ikey1)))) then
                                lfound=.true.
                                lfound2(isite,ikey1)=.true.
                            else
                                ikey1=ikey1+1
                            endif
                        enddo
                        if (lfound) then
                            lfound1(ikey1)=.true.
                            if (ikey1 >= 2 .and. .not. lfound1(1)) then !error FREQ_SECT should be first
                                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E75',modnam,'FREQ_SECT OR FREQ_SECT2 NOT FOUND'
                                lbad=.true.
                                eof=1
                            endif
                            i=index(inpline1,trim(adjustl(sc_keys(isite,ikey1))))
                            i1=len_trim(sc_keys(isite,ikey1))
                            if (ikey1 == 1) then
                                call freq_sec(i+i1,isite,nfreq,nsec,lskip)
                            elseif (ikey1 == 2) then
                                if (.not. lskip) call sectors(i+i1,isite,nsec)
                            else
                                if (.not. lskip) call site_char(i+i1,isite,nfreq,nsec)
                            endif
                        endif
                    endif
                endif
            enddo
!           check to see if keywords for site found
            if (.not. lfound2(isite,1) .or. .not. lfound2(isite,2) .or. .not. lfound2(isite,3)) then
    l3:         do k=1,3
                    lbadsc=.true.
                    if (.not. lfound2(isite,k)) write(msg_unit,formstr(6)) adjustl(pathid(ipath)),'E73',modnam,'MISSING KEYWORD ',&
                            trim(adjustl(sc_keys(isite,k))),'FOR',trim(adjustl(sites(isite))),&
                            'SURFACE CHARACTERISTICS SITE; FILENAME',trim(adjustl(fname))
                enddo l3
            endif
!           close the file
            close(iunit)
        endif
        
        if (lbad .or. lbadsc) lbadpbl=.true.
        if (allocated(sc_years)) deallocate(sc_years)
    enddo l1
    
!   check to make sure all years have been accounted for after reading the data files
    if (iswitch == 1) then
    y2: do iyr1=pbldates(1,1),pbldates(2,1)
            if (.not. have_sc(iyr1-pbldates(1,1)+1)) then
!               year doesn't have SC assigned, error
                lbadsc=.true.
                write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E74',modnam,'YEAR',iyr1,&
                            trim(adjustl(sites(isite))),'SURFACE CHARACTERSTICS NOT ASSIGNED'

            endif
        enddo y2
        if (lbadsc)then
            lbad=.true.
            lbadpbl=.true.
        endif
    endif
!   proceed with assignments if iswitch is not 1
    if (iswitch /= 1) then
!       now assign the temporary arrays to the final surface characteristics arrays
        if (isite == 1) then
            allocate(sfc_char1(maxval(nsectors1),maxval(nfreq1),pbldates(2,1)-pbldates(1,1)+1))
            allocate(sectors1(2,maxval(nsectors1),pbldates(2,1)-pbldates(1,1)+1))
        else
            allocate(sfc_char2(maxval(nsectors2),maxval(nfreq2),pbldates(2,1)-pbldates(1,1)+1))
            allocate(sectors2(2,maxval(nsectors2),pbldates(2,1)-pbldates(1,1)+1))
        endif
    
!       fill in the sectors1 and sectors2 arrays and the sfc_char1 and sfc_char2 arrays
    y3:     do iyr=pbldates(1,1),pbldates(2,1)
            iyr1=iyr-pbldates(1,1)+1
            if (isite == 1) then
                freq=maxval(nfreq1)
                sec=maxval(nsectors1)
            else
                freq=maxval(nfreq2)
                sec=maxval(nsectors2)
            endif
    f1:     do ifreq=1,freq
    s1:         do isec=1,sec
                    if (isite == 1) then
                        sfc_char1(isec,ifreq,iyr1)%albedo=talbedo(isec,ifreq,iyr1)
                        sfc_char1(isec,ifreq,iyr1)%bowen=tbowen(isec,ifreq,iyr1)
                        sfc_char1(isec,ifreq,iyr1)%zo=tzo(isec,ifreq,iyr1)
                    else
                        sfc_char2(isec,ifreq,iyr1)%albedo=talbedo(isec,ifreq,iyr1)
                        sfc_char2(isec,ifreq,iyr1)%bowen=tbowen(isec,ifreq,iyr1)
                        sfc_char2(isec,ifreq,iyr1)%zo=tzo(isec,ifreq,iyr1)
                    endif
    d1:             do id=1,2
                        if (isite == 1) then
                            sectors1(id,isec,iyr1)=tsectors(id,isec,iyr1)
                        else
                            sectors2(id,isec,iyr1)=tsectors(id,isec,iyr1)
                        endif
                    enddo d1
                enddo s1
            enddo f1
        enddo y3
    
        if (allocated(talbedo)) deallocate(talbedo)
        if (allocated(tbowen) )deallocate(tbowen)
        if (allocated(tzo)) deallocate(tzo)
        if (allocated(tsectors)) deallocate(tsectors)
    endif
    
    if (allocated(have_sc)) deallocate(have_sc)
    
    return
    end subroutine aersurf
!*********************************************************************************************************

    subroutine surf_years(yrstr)
!=========================================================================================================
!   SUBROUTINE SURF_YEARS
!   THIS SUBROUTINE CHECKS TO SEE WHAT YEARS ARE ASSOCIATED WITH A SPECIFIC SET OF SURFACE CHARACTERISTICS
!
!   MODIFIED MARCH 9, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (AERSURF)
!
!   INPUT ARGUMENTS 
!
!   YRSTR:          TEXT STRING OF YEARS ASSOCIATED WITH SURFACE CHARACTERISTICS
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   iyr:            year loop counter
!   nfields:        number of fields in yrstr
!   ifield:         field loop counter
!   yrs:            2-element array of first and last year in yrstr
!   iflags:         I/O flags for yrs
!   nn:             index of dash in yrstr
!
!   Logical variables
!   lbadsc:         indicates issue with years
!   Character variables
!   formstr:        formats for messages
!   yrstr:          text string of years associated with surface characteristics
!   yearfields:     years or range of years read from yrstr
!   modnam:         Subroutine name
!========================================================================================================= 
    !use main1, only: nfield
    use file_units, only: msg_unit
    use main1, only: lbad,msg_form
    implicit none
    integer(kind=4):: i,iyr,nfields,ifield,yrs(2),iflags(2),nn
    logical :: lbadsc
    character(len=flength),intent(in) :: yrstr
    character(len=10) :: modnam='SURF_YEARS'
    character(len=60) :: formstr(2)
    character(len=15),allocatable, dimension(:) :: yearfields
    
!   formats for messages
!   1.  invalid year string or first year of range > second year
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'

!   2.  year is not 4-digit year
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i4,1x,a)'
    
    ifield=0
    nfields=0
    iyr=0
    iflags=0
    lbadsc=.false.
    if (trim(adjustl(yrstr)) == '-9') then
!       all years
        nsc_years=pbldates(2,1)-pbldates(1,1)+1
        allocate(sc_years(nsc_years))
    l1: do iyr=1,nsc_years
            sc_years(iyr)=pbldates(1,1)-1+iyr
        enddo l1
    else
!       get the number of fields 
    l2: do i=1,len_trim(yrstr)
            if (ichar(yrstr(i+1:i+1)) == 32 .and. ichar(yrstr(i:i)) /= 32) nfields=nfields+1
        enddo l2
        allocate(yearfields(nfields))
        read(yrstr,*)(yearfields(ifield),ifield=1,nfields)
        nsc_years=0
    l3: do ifield=1,nfields
            if (index(yearfields(ifield),'-') > 0) then ! range of years
                nn=index(yearfields(ifield),'-')
                read(yearfields(ifield)(1:nn-1),*,iostat=iflags(1))yrs(1)
                read(yearfields(ifield)(nn+1:len_trim(yearfields(ifield))),*,iostat=iflags(2))yrs(2)
!               make sure years are numeric and 4-digit
    l4:         do i=1,2
                    if (iflags(i) /= 0) then
                        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E74',modnam,'INVALID YEAR STRING',&
                            trim(adjustl(yearfields(ifield)))
                        lbadsc=.true.
                    elseif (yrs(i) < 1000) then
                        write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E74',modnam,'YEAR',yrs(i),'IS NOT 4-DIGIT'
                        lbadsc=.true.
                    endif
                enddo l4
                if (.not. lbadsc) then
!                   make sure first year is before second year
                    if (yrs(1) > yrs(2)) then
                        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E74',modnam,'FIRST YEAR OF RANGE > SECOND YEAR ',&
                            trim(adjustl(yearfields(ifield)))
                        lbadsc=.true.
                    else
                        nsc_years=nsc_years+(yrs(2)-yrs(1)+1)
                    endif
               endif
            else
                read(yearfields(ifield),*,iostat=iflags(1))yrs(1)
!               make sure year is numeric and 4-digit
                if (iflags(1) /= 0) then
                    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E74',modnam,'INVALID YEAR STRING',&
                        trim(adjustl(yearfields(ifield)))
                    lbadsc=.true.
                else
                    if (yrs(1) < 1000) then
                        write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E74',modnam,'YEAR',yrs(1),'IS NOT A 4-DIGIT YEAR'
                        lbadsc=.true. 
                    else
                        nsc_years=nsc_years+1
                    endif
                endif
            endif
        enddo l3

        if (.not. lbadsc) then
            allocate(sc_years(nsc_years))
            iyr=0
    l5:     do ifield=1,nfields
                if (index(yearfields(ifield),'-') > 0) then ! range of years
                    nn=index(yearfields(ifield),'-')
                    read(yearfields(ifield)(1:nn-1),*,iostat=iflags(1))yrs(1)
                    read(yearfields(ifield)(nn+1:len_trim(yearfields(ifield))),*,iostat=iflags(2))yrs(2)
    l6:             do i=yrs(1),yrs(2)
                        iyr=iyr+1
                        sc_years(iyr)=i
                    enddo l6
                else
                    read(yearfields(ifield),*,iostat=iflags(1))yrs(1)
                    iyr=iyr+1
                    sc_years(iyr)=yrs(1)
                endif
            enddo l5            
        endif
    endif
    if (allocated(yearfields)) deallocate(yearfields)
    if (lbadsc) lbad=.true.
    return
    end subroutine surf_years
!*********************************************************************************************************

    subroutine pbl_test
!=========================================================================================================
!   SUBROUTINE PBL_TEST
!   THIS SUBROUTINE CHECKS THAT MANDATORY KEYWORDS HAVE BEEN INCLUDED
!   THIS IS NOT A CHECK ON THE SYNTAX OR IF INCLUDED FILENAMES EXIST
!   THAT HAS BEEN DONE EARLIER IN PBL_PATH
!
!   MODIFIED MAY 2, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE READ_INP (READINP)
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   j:              loop counter
!   ikey1:          keyword index for surface characteristics
!   isurf:          lpblkeyword index counter for surface characteristics
!   isite:          integer indicator of site type for surface characteristics
!                   processing and message (1=primary, 2=secondary)
!
!   Logical variables
!   lbadddate:		check to determine if pbl processing dates overlap with other data
!                   periods (upper air, surface, onsite/prognostic)
!   lbadsc:         denotes bad surface characteristics processing
!   usercloud:      user requested cloud interpolation to be done
!   usertemp:       user requested temperature interpolation to be done
!   use_os_ustar:   use the input u* if overland and ONSITE or overwater and PROG
!   use_os_cloud:   use the input cloud cover if overland and ONSITE or overwater and PROG
!   Character variables
!   formstr:        formats for messages
!   str:            text string to denote if ONSITE or PROG
!   scstr:          surface characteristics site string (PRIMARY or SECONDARY)
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: keywrd,writeunit,lpath,lkey,msg_form
    use file_units, only: msg_unit
    use upperair, only: upstart,upend,updates,upgmt2lst,lupkeywords,upstage1
    use surface, only: sfstart,sfend,sfdates,lasos,iasos,asoscommdates,sfid,nws_hgt,sflat,sflon,lsfkeywords,sfstage1
    use onsite, only: osstart,osend,osdates,oslat,oslon,temp_var,cloud_var,inso_var,nrad_var,ustar_var,loskeywords,osstage1,&
        mix_var,overland,onsite_init
    implicit none
    integer(kind=4):: i,j,ikey1,isurf,isite
    !integer(kind=8), allocatable, dimension(:) :: start1
    !integer(kind=8), allocatable, dimension(:) :: end1
    logical lbaddate(3),lbadsc,usercloud,usertemp,use_os_ustar,use_os_cloud
    !character(len=flength) :: fname
    character(len=6) :: str
    character(len=9) :: scstr(2)
    character(len=80) :: formstr(9)
    character(len=10) :: modnam='PBL_TEST'
      
    data scstr /'PRIMARY','SECONDARY'/
    
!   formats for messages
!   1.  mismatch of data periods for one or more pathways
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),i8,a1,i8,3(1x,a),1x,i8,a1,i8)'
    
!   2.location keyword needed or surface characteristics keyword missing
!   no upperair data with onsite or prognostic data and does not include mixing heights
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,2(1x,a))'

!   3.  keyword not needed
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
 
!   4.  NWS_HGT not equal to ASOS anemometer height
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'2(a,1x,f5.2,1x),a,1x,a)'
    
!   5.  NWS_HGHT keyword missing
!       BULKRN requested but can't be used
!       not enough variables for stable calculations
!       no data overlap  
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a)'

!   6. keyword listed with site keywords
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'a,3(1x,a))'

!   7.  dates out of order
    write(formstr(7),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),i8,1x,3(a,1x),i8)'

!   8.  no data overlap
    write(formstr(8),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,a1,i8)'

!   9. variable will be read in but not used in stage 2
    write(formstr(9),'(2(a))')trim(adjustl(msg_form)),'a,1x,a4,1x,a)'
        
    usercloud=.false.
    usertemp=.false.
    use_os_ustar=.true.
    use_os_cloud=.true.
    
    if (.not. lpath(4) .and. .not. lpath(5)) call onsite_init
    
!   post 21DRF reset u* and cloud cover to not being read if overland and prognostic but allow for onsite
!   keep osvars%lread unchanged
    if (lpath(5) .and. overland) then
        if (osvars(ustar_var)%lread) then
!           only write message if stage 1 not done in same AERMET run
            if (.not. osstage1) write(msg_unit,formstr(9))adjustl(pathid(ipath)),'I80',modnam,&
            'LOCATION IS OVERLAND; PROGNOSTIC VARIABLE',trim(adjustl(osvars(ustar_var)%varname)),&
                'READ IN STAGE 1 BUT CALCULATED IN STAGE 2'
            use_os_ustar=.false.
        endif
        if (osvars(cloud_var)%lread) then
!           only write message if stage 1 not done in same AERMET run
            if (.not. osstage1) write(msg_unit,formstr(9))adjustl(pathid(ipath)),'I80',modnam,&
            'LOCATION IS OVERLAND; PROGNOSTIC VARIABLE',trim(adjustl(osvars(cloud_var)%varname)),&
                'READ IN STAGE 1 BUT CALCULATED IN STAGE 2'
            use_os_cloud=.false.
        endif
    endif
    
!   22112 reset use_os_ustar and use_os_cloud to false if ONSITE and the resepective variables
!   are not used
    if (lpath(4)) then
        if (.not. osvars(ustar_var)%lread) use_os_ustar=.false.
        if (.not. osvars(cloud_var)%lread) use_os_cloud=.false.
    endif
    
!   note that when PBL_TEST is called, msg_unit has been set
!   no longer need to write messages to the value from getunit
!   write to msg_unit
      
!   XDATES
!   if XDATES listed, check to see if overlap with the input datasets
!   if overlap then okay, if outside all the input datasets' range issue error
!   if XDATES not listed, check the dates and if they are not the same issue error 
    
!   initialize to true, reset to false if dates overlap
    lbaddate=.true.
    if (lpblkeywords(1)) then
!       check against upper air
        if (lpath(2)) then
            if (pblstart > upend .or. pblend < upstart) then
!               only set lbaddate(1) if not reading onsite mixing heights
                if (osvars(5)%lread) lbaddate(1)=.false.
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W80',modnam,'MISMATCH OF',trim(adjustl(pathid(2))),&
                    'DATA PERIOD',upstart,'-',upend,'AND',trim(adjustl(pathid(6))),'DATA PERIOD',pblstart,'-',pblend
                
!               reset upper air dates
                upstart=pblstart
                upend=pblend
                updates=pbldates
            else
                lbaddate(1)=.false.
                if (pblstart < upstart) then
                    write(msg_unit,formstr(7))adjustl(pathid(ipath)),'W80',modnam,trim(adjustl(pathid(2))),'START DATE',upstart,&
                    'IS AFTER',trim(adjustl(pathid(6))),'START DATE',pblstart
                else
!                   only extract data within METPREP window if stage 2 and not stage 1 or not saving EXTRACT AND QAOUT
                    if (.not. upstage1 .or. (.not. lupkeywords(3) .and. .not. lupkeywords(4))) then
                        upstart=pblstart
                        updates(1,:)=pbldates(1,:)
                    endif
                endif
                if (upend < pblend) then
                    write(msg_unit,formstr(7))adjustl(pathid(ipath)),'W80',modnam,trim(adjustl(pathid(2))),'END DATE',upend,&
                    'IS BEFORE',trim(adjustl(pathid(6))),'END DATE',pblend
                else
!                   only extract data within METPREP window if stage 2 and not stage 1 or not saving EXTRACT AND QAOUT
                    if (.not. upstage1 .or. (.not. lupkeywords(3) .and. .not. lupkeywords(4))) then
                        upend=pblend
                        updates(2,:)=pbldates(2,:)
                    endif
                endif
            endif
        endif
!       check against surface
        if (lpath(3)) then
            if ((pblstart > sfend .or. pblend < sfstart) .and. lpath(3)) then
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W80',modnam,'MISMATCH OF',trim(adjustl(pathid(3))),&
                    'DATA PERIOD',sfstart,'-',sfend,'AND',trim(adjustl(pathid(6))),'DATA PERIOD',pblstart,'-',pblend
!               reset surface air dates
                sfstart=pblstart
                sfend=pblend
                sfdates=pbldates
            else
                lbaddate(2)=.false.
                if (pblstart < sfstart) then
                    write(msg_unit,formstr(7))adjustl(pathid(ipath)),'W80',modnam,trim(adjustl(pathid(3))),'START DATE',sfstart,&
                    'IS AFTER',trim(adjustl(pathid(6))),'START DATE',pblstart
            
                else
!                   only extract data within METPREP window if stage 2 and not stage 1 or not saving EXTRACT AND QAOUT
                    if (.not. sfstage1 .or. (.not. lsfkeywords(3) .and. .not. lsfkeywords(4))) then
                        sfstart=pblstart
                        sfdates(1,:)=pbldates(1,:)
                    endif
                endif
                if (sfend < pblend) then
                    write(msg_unit,formstr(7))adjustl(pathid(ipath)),'W80',modnam,trim(adjustl(pathid(3))),'END DATE',sfend,&
                    'IS BEFORE',trim(adjustl(pathid(6))),'END DATE',pblend
                else
!                   only extract data within METPREP window if stage 2 and not stage 1 or not saving EXTRACT AND QAOUT
                    if (.not. sfstage1 .or. (.not. lsfkeywords(3) .and. .not. lsfkeywords(4))) then
                        sfend=pblend
                        sfdates(2,:)=pbldates(2,:)
                    endif
                endif                
            endif  
        endif
!       onsite or PROG
    l1: do i=4,5
            if (lpath(i)) then
                if ((pblstart > osend .or. pblend < osstart) .and. lpath(i)) then
                    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W80',modnam,'MISMATCH OF',trim(adjustl(pathid(i))),&
                    'DATA PERIOD',osstart,'-',osend,'AND',trim(adjustl(pathid(6))),'DATA PERIOD',pblstart,'-',pblend
                    
!                   reset onsite dates
                    osstart=pblstart
                    osend=pblend
                    osdates=pbldates
                else
                    lbaddate(3)=.false.
                    if (pblstart < osstart) then
                        write(msg_unit,formstr(7))adjustl(pathid(ipath)),'W80',modnam,trim(adjustl(pathid(i))),'START DATE',&
                            osstart,'IS AFTER',trim(adjustl(pathid(6))),'START DATE',pblstart
                    else
    !                   only extract data within METPREP window if stage 2 and not stage 1 or not saving EXTRACT AND QAOUT
                        if (.not. osstage1 .or. .not. loskeywords(4)) then
                            osstart=pblstart
                            osdates(1,:)=pbldates(1,:)
                        endif
                    endif
                    if (osend < pblend) then
                        write(msg_unit,formstr(7))adjustl(pathid(ipath)),'W80',modnam,trim(adjustl(pathid(i))),'END DATE',&
                            osend,'IS BEFORE',trim(adjustl(pathid(6))),'END DATE',pblend
                        
                    else
    !                   only extract data within METPREP window if stage 2 and not stage 1 or not saving EXTRACT AND QAOUT
                        if (.not. osstage1 .or. .not. loskeywords(4)) then
                            osend=pblend
                            osdates(2,:)=pbldates(2,:)
                        endif
                    endif
                endif
            endif
        enddo l1 
!		if all 3 paths are bad, lbad=true
        if (lbaddate(1) .and. lbaddate(2) .and. lbaddate(3)) then
            lbad=.true.
            write(msg_unit,formstr(8))adjustl(pathid(ipath)),'E70',modnam,'NO DATA PERIODS DATES OVERLAP',pblstart,pblend
        endif
        
    else  !need to check the dates of the inputs against each other
!       if upperair and surface data
        if (lpath(2) .and. lpath(3)) then
            if (upstart > sfend .or. upend < sfstart) then
               
!               only set if onsite mixing heights are not being read in
                if (osvars(mix_var)%lread) then
                    lbaddate(1)=.false.
                    lbaddate(2)=.false.
              
                    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W80',modnam,'MISMATCH OF',trim(adjustl(pathid(2))),&
                    'DATA PERIOD',upstart,'-',upend,'AND',trim(adjustl(pathid(3))),'DATA PERIOD',sfstart,'-',sfend
                endif
            else
                lbaddate(1)=.false.
                lbaddate(2)=.false.
            endif
        endif
!       if upperair and onsite if not reading onsite mixing heights
        if (lpath(2) .and. lpath(4)) then
            if (upstart > osend .or. upend < osstart) then
               
                if (osvars(mix_var)%lread) then
                    lbaddate(1)=.false.
                    lbaddate(3)=.false.
                endif
              
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W80',modnam,'MISMATCH OF',trim(adjustl(pathid(2))),&
                    'DATA PERIOD',upstart,'-',upend,'AND',trim(adjustl(pathid(4))),'DATA PERIOD',osstart,'-',osend
            else
                lbaddate(1)=.false.
                lbaddate(3)=.false.
            endif
        endif
!       if upperair and PROG
        if (lpath(2) .and. lpath(5)) then
            if (upstart > osend .or. upend < osstart) then
                
                if (osvars(mix_var)%lread) then
                    lbaddate(1)=.false.
                    lbaddate(3)=.false.
                endif
               
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W80',modnam,'MISMATCH OF',trim(adjustl(pathid(2))),&
                    'DATA PERIOD',upstart,'-',upend,'AND',trim(adjustl(pathid(5))),'DATA PERIOD',osstart,'-',osend
            else
                lbaddate(1)=.false.
                lbaddate(3)=.false.
            endif
        endif
!       if surface and onsite
        if (lpath(3) .and. lpath(4)) then
            if (sfstart > osend .or. sfend < osstart) then
               
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W80',modnam,'MISMATCH OF',trim(adjustl(pathid(3))),&
                    'DATA PERIOD',upstart,'-',upend,'AND',trim(adjustl(pathid(4))),'DATA PERIOD',osstart,'-',osend
            else
                lbaddate(2)=.false.
                lbaddate(3)=.false.
            endif
        endif
       
        if (lbaddate(1) .and. lbaddate(2) .and. lbaddate(3)) then
            lbad=.true.
            write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E70',modnam,'NO DATA PERIODS DATES OVERLAP'
        else
!           use the earliest of the dates and latest of the dates
            if (lpath(2)) then
                pblstart=upstart
                pblend=upend
                pbldates=updates
            endif
            if (lpath(3)) then
                if (pblstart == 0) then
                    pblstart=sfstart
                    pbldates(1,:)=sfdates(1,:)
                else
                    if (sfstart < pblstart) then
                        pblstart=sfstart
                        pbldates(1,:)=sfdates(1,:)
                    endif
                endif
                if (pblend == 0) then
                    pblend=sfend
                    pbldates(2,:)=sfdates(2,:)
                else
                    if (sfend > pblend) then
                        pblend=sfend
                        pbldates(2,:)=sfdates(2,:)
                    endif
                endif
            endif
            if (lpath(4) .or. lpath(5)) then
                if (pblstart == 0) then
                    pblstart=osstart
                    pbldates(1,:)=osdates(1,:)
                else
                    if (osstart < pblstart) then
                        pblstart=osstart
                        pbldates(1,:)=osdates(1,:)
                    endif
                endif
                if (pblend == 0) then
                    pblend=osend
                    pbldates(2,:)=osdates(2,:)
                else
                    if (osend > pblend) then
                        pblend=osend
                        pbldates(2,:)=osdates(2,:)
                    endif
                endif
            endif
        
        endif
    endif
!   check the LOCATION keyword
!   if present with onsite data and upperair data, then issue warning
!   if present with onsite data (with mixing heights) and no upperair data then it is needed
!   otherwise it is not needed and issue a warning
    if (lpblkeywords(2)) then
        if (.not. ((lpath(4) .or. lpath(5)) .and. osvars(8)%lread .and. .not. lpath(2))) then !onsite data with mixing height and without upper air data
            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W70',modnam,trim(adjustl(keywrd(10))),'KEYWORD NOT NEEDED'
!           reset coordinates to surface or onsite (PROG)
            if (lpath(4) .or. lpath(5)) then
                pbllat=oslat
                pbllon=oslon
            else
                pbllat=sflat
                pbllon=sflon
            endif
            pblgmt2lst=upgmt2lst !set pblgmt2lst to the upper air stations GMT-LST 
        endif
    else !keyword not present
        if ((lpath(4) .or. lpath(5)) .and. osvars(8)%lread .and. .not. lpath(2)) then !onsite data without upper air data
            if (lpath(4)) then
                str='ONSITE'
            else
                str='PROG'
            endif
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E71',modnam,'LOCATION KEYWORD MISSING; NEEDED FOR',&
                trim(adjustl(str)),'DATA WITH MIXHTS AND NO UPPERAIR DATA'
            lbad=.true.
        endif
!       set pbl coordinates to onsite or surface
        if (lpath(4) .or. lpath(5)) then
            pbllat=oslat
            pbllon=oslon
        else
            pbllat=sflat
            pbllon=sflon
        endif
        pblgmt2lst=upgmt2lst !set pblgmt2lst to the upper air stations GMT-LST 
    endif
    
!   check the NWS height against ASOS height when SURFACE data being processed
!   and the processing period emcompasses the ASOS commission date
!   NWS height only checked under these conditions because the processing period
!   may be pre-ASOS.
    if (lpblkeywords(11)) then
        !if (lpath(3) .and. (lasos .or. (.not. lasos .and. iasos /=0)) .and. pblend >= asoscommdates(iasos)%commisdate) then
        if (lpath(3) .and. (lasos .or. (.not. lasos .and. iasos /=0))) then
          
            if (pblend >= asoscommdates(iasos)%commisdate .and. dabs(nws_hgt-asoscommdates(iasos)%anem_meters) > 0.1_r8)&
            write(writeunit,formstr(4))adjustl(pathid(ipath)),'W71',modnam,'NWS_HGT',nws_hgt,'M NOT EQUAL TO ASOS HEIGHT',&
                asoscommdates(iasos)%anem_meters,'FOR STATION',trim(adjustl(sfid))
        endif
    else
 !      if NWS_HGT keyword missing and substituting surface data (SUBNWS), issue error
        if (lpath(3) .and. lactions(3)) then
            write(writeunit,formstr(5))adjustl(pathid(ipath)),'E72',modnam,'NWS_HGT KEYWORD MISSING' 
            lbad=.true.
        endif
    endif
!   surface characteristics
    lbadsc=.false.
!   check to make sure that if onsite and surface data processed along with SUBNWS keyword both
!   primary and secondary surface characteristics are input via
!   keywords or AERSURF and AERSURF2
!   also if only one ONSITE (or PROG) or SURFACE is processed then
!   only have primary characteristics
!   also make sure that external file and keywords are not both input

!   external file listed with individual keywords for same site (primary or secondary)
!   allow external file for one type and keywords for the other
!   examples:  AERSURF not allowed with FREQ_SECT, SITE_CHAR, and SECTOR
!   examples:  AERSURF allowed with FREQ_SECT2, SITE_CHAR2, and SECTOR2
!   examples:  AERSURF2 not allowed with FREQ_SECT2, SITE_CHAR2, and SECTOR2
!   examples:  AERSURF2 allowed with FREQ_SECT, SITE_CHAR, and SECTOR
    s1: do isurf=15,16
            if (isurf == 15) then
                j=3 !FREQ_SECT, SITE_CHAR, SECTOR
            else
                j=12 !FREQ_SECT2, SITE_CHAR2, SECTOR2
            endif
            ikey1=isurf+18
            if (lpblkeywords(isurf) .and. (lpblkeywords(j) .or. lpblkeywords(j+1) .or. lpblkeywords(j+2))) then
                lbadsc=.true.
                write(writeunit,formstr(6))adjustl(pathid(ipath)),'E73',modnam,trim(adjustl(keywrd(ikey1))),'KEYWORD LISTED WITH',&
                    trim(adjustl(scstr(i))),'SITE SC KEYWORDS'
            endif
    enddo s1

    if (lpath(3) .neqv. lpath(4)) then
!       only onsite or surface being processed
!       secondary keywords listed, issue warning
        if (lpblkeywords(12) .or. lpblkeywords(13) .or. lpblkeywords(14) .or. lpblkeywords(16)) then
            
            write(writeunit,formstr(5))adjustl(pathid(ipath)),'W74',modnam,'SECONDARY SITE SC KEYWORDS LISTED BUT NOT NEEDED'
        endif
!       FREQ_SECT SITE_CHAR and SECTOR not specified (need all 3) if no file specified
        if ((lpblkeywords(3) .neqv. lpblkeywords(4)) .or. (lpblkeywords(3) .neqv. lpblkeywords(5))) then
            lbadsc=.true.
            write(writeunit,formstr(5))adjustl(pathid(ipath)),'E73',modnam,'ONE OR MORE PRIMARY SITE SC KEYWORDS MISSING'
        endif
    endif

!   both surface and onsite specified and SUBNWS, need characteristics for both
    if (lpath(3) .and. lpath(4) .and. lactions(3)) then
        if (lpblkeywords(15) .neqv. lpblkeywords(16)) then
    i2:     do i=1,2
                if (i == 1) then
                    j=3
                    isurf=15
                else
                    j=12
                    isurf=16
                endif
!               make sure all 3 components or file are there or not
                if ((lpblkeywords(j) .neqv. lpblkeywords(j+1)) .or. (lpblkeywords(j+1) .neqv. lpblkeywords(j+2))) then
                    lbadsc=.true.
                    write(writeunit,formstr(2))adjustl(pathid(ipath)),'E73',modnam,'ONE OR MORE',trim(adjustl(scstr(i))),&
                        'SITE SC KEYWORDS MISSING'
                endif
            enddo i2
        
        elseif (.not. lpblkeywords(15) .and. .not. lpblkeywords(16) .and. .not. lpblkeywords(3) .and. .not. lpblkeywords(4) .and. &
                .not. lpblkeywords(5) .and. .not. lpblkeywords(12) .and. .not. lpblkeywords(13) .and. .not. lpblkeywords(14)) then
!           need to have all 3 components for each type
            lbadsc=.true.
            write(writeunit,formstr(5))adjustl(pathid(ipath)),'E73',modnam,'SURFACE CHARACTERISTICS NOT SPECIFIED'
        endif
    endif
    
!   reset subtemp or subcloud from initialized value of true to false if no substitution requested or
!   set usercloud or usertemp to true if substitution requested
    if (litems(6)) then
        if (lactions(8)) usercloud=.true.
        if (lactions(9)) subcloud=.false.
    endif
    
    if (litems(7)) then
        if (lactions(10)) usertemp=.true.
        if (lactions(11)) subtemp=.false.
    endif
!   reset subtemp and subcloud based on certain conditions
    if ((lpath(4) .or. lpath(5)) .and. lpath(3)) then
!       ONSITE or PROG data along with SURFACE data

!       cloud cover
!       if BULKRN requested or onsite insolation read in set subcloud to false.
        if (lactions(4) .or. osvars(inso_var)%lread) then
            subcloud=.false. 
            if (litems(6) .and. lactions(8)) write(msg_unit,formstr(5))adjustl(pathid(ipath)),'WXX',modnam,&
                'BULKRN REQUESTED OR ONSITE INSO AVAILABLE; RESET CLOUD COVER SUBSTITUION TO NO SUBSTITUTION'
        
!		elseif (osvars(cloud_var)%lread .and. subcloud .and. .not. usercloud) then
!       post 21DRF now use os_use_cloud instead lread
        elseif (use_os_cloud .and. subcloud .and. .not. usercloud) then
!           set subcloud to false if user didn't request substitution and PROG or ONSITE cloud cover available
!           if user didn't request substitution subcloud may be true; if user
!           requested no substitution, subcloud is already false
!           note if there is no PROG or ONSITE cloud cover, then subcloud is already true if the user
!           didn't request no substitution; interpolation will be based on NWS cover;
!           because subcloud is defaulted to true, no need for an elseif for no PROG or ONSITE cover
            subcloud=.false.
        endif
    
!       temperature
!       set subtemp to false since SURFACE data being read and cloud cover is being substituted from NWS data
!       if the user requested temperature substitution then leave subtemp as true if not already set to false
!       if no onsite temperatures leave subtemp as is (true or false) and interpolation will be done from
!       NWS temperatures.
        if (osvars(temp_var)%lread .and. subtemp .and. .not. usertemp) subtemp=.false.

    elseif (lpath(4) .and. lpath(5)) then
!       PROG or ONSITE data with no SURFACE DATA        
!       cloud cover
!       if BULKRN requested or onsite insolation read in set subcloud to false.
        if (lactions(4) .or. osvars(inso_var)%lread) then
            subcloud=.false. 
            if (litems(6) .and. lactions(8)) write(msg_unit,formstr(5))adjustl(pathid(ipath)),'WXX',modnam,&
                'BULKRN REQUESTED OR ONSITE INSO AVAILABLE; RESET CLOUD COVER SUBSTITUION TO NO SUBSTITUTION'
!           no need to address sky cover or temperature. defaults or user requested values take care of it
        endif
!       for the case of no PROG or ONSITE data and SURFACE data, no need to reset subtemp or subcloud
        
!       also check if enough data for stable calculations, if not, issue warning
!       post 21DRF, use use_os_cloud instead of lread
        !if (.not. osvars(cloud_var)%lread .and. .not. osvars(nrad_var)%lread .and. (.not. osvars(inso_var)%lread .or. .not. &
        if (.not. use_os_cloud .and. .not. osvars(nrad_var)%lread .and. (.not. osvars(inso_var)%lread .or. .not. &
            lactions(4))) write(msg_unit,formstr(5))adjustl(pathid(ipath)),'W73',modnam,&
            'NOT ENOUGH VARIABLES FOR STABLE BL CALCULATIONS'
    endif
    
!   set adjust u* flag; alert user that valid onsite u* will not be adjusted
    if (litems(3) .and. lactions(7)) then
        adjustar=.true.
        if (lpath(4) .or. lpath(5)) then
!           post 21DRF use the variable us_os_ustar instead of osvars%lread
            if (use_os_ustar) write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I85',modnam,&
                'ADJUST U* REQUESTED; HOURS WITH VALID U* WILL NOT BE ADJUSTED'
            !if (osvars(ustar_var)%lread) write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I85',modnam,&
            !    'ADJUST U* REQUESTED; HOURS WITH VALID U* WILL NOT BE ADJUSTED'
        endif
    endif
    
!   if SUNRISE option used for sounding selection but UAWINDOW not selected, set default window
!   to -6 and + 2 hours.  if SUNRISE OPTION not selected and UAWINDOW not selected default window is
!   +/- 1 hour, which is the initial value of the window.
    if (lactions(6) .and. .not. lkey(24)) then
        snding_win(1)=-6
        snding_win(2)=2
    endif
    
!   post 21DRF, check to see if upper air data being read in if prognostic or onsite data without mixing heights
    if (.not. lpath(2) .and. ((lpath(4) .or. lpath(5)) .and. .not. osvars(mix_var)%lread)) then
        if (lpath(4)) then
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E81',modnam,&
                'UPPERAIR DATA NOT REQUESTED AND NO INPUT',trim(adjustl(pathid(4))),'MIXING HEIGHTS'
        else
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E81',modnam,&
                'UPPERAIR DATA NOT REQUESTED AND NO INPUT',trim(adjustl(pathid(5))),'MIXING HEIGHTS'
        endif
        lbad=.true.
    endif

!   call aersurf to make sure no problems with duplicate years
    if (lpblkeywords(12) .or. lpblkeywords(16)) sec_sc=.true.
    if (.not. lbadsc) then
!       primary (always present)
        isite=1
        call aersurf(1,isite,lbadsc)
        if (lbadsc)lbad=.true.
!       if secondary
        if (sec_sc) then
            lbadsc=.false.
            isite=2
            call aersurf(1,isite,lbadsc)
            if (lbadsc)lbad=.true.
        endif
    else
        lbad=.true.
    endif

!   assign hemisphere if southern hemisphere. ihem already initialized to 1 (northern hemisphere)
    if (((lpath(4) .or. lpath(5)) .and. oslat < 0.0_r8) .or. (.not. lpath(4) .and. .not. lpath(5) .and. sflat < 0.0_r8)) ihem(1)=2
    if (sec_sc .and. sflat < 0.0_r8) ihem(2)=2
    
!   DATA keyword found. this is obsolete but will not make it an error at this time
!   issue warning; a later version of AERMET will make this a fatal error
    if (lpblkeywords(18)) write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W70',modnam,trim(adjustl(keywrd(4))),&
        'KEYWORD NOT NEEDED'
 
    return
    end subroutine pbl_test
!*********************************************************************************************************

    subroutine sundat(rlat,rlon,tzone,jday,sr,ss,angs)
!=========================================================================================================
!   SUBROUTINE SUNDAT
!   THIS SUBROUTINE CALCULATES SUNRISE AND SUNSET, AND OPTIONALLY WILL CALCULATE THE SOLAR
!   ELEVATION ANGLE FOR ALL HOURS OF THE DAY.
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   RLAT:       LATITUDE
!   RLON:       LONGITUDE
!   TZONE:      TIME ZONE
!   JDAY:       JULIAN DAY
!
!   OUTPUT ARGUMENTS
!
!   SR:         SUNRISE
!   SS:         SUNSET
!   ANGS:       OPTIONAL ELEVATION ANGLES FOR THE DAY
!
!   Variable definitions
!      
!   Integer variables
!   tzone:      time zone
!   jday:       julian day
!   h:          hour loop counter
!
!   Real variables
!   rlat:       latitude
!   rlon:       longitude
!   sr:         sunrise
!   ss:         sunset
!   angs:       optional elevation angles for the day
!   dum:        variable relating longitude to time zone
!   tempz:      variable relating longitude to time zone
!   sinlat:     sine of latitude
!   coslat:     cosine of latitude
!   dayno:      fraction of year for day
!   tdayno:     2X dayno
!   sindayno:   sine of dayno
!   cosdayno:   cosine of dayno
!   sintdayno:  sine of tdayno
!   costdayno:  cosine of tdayno
!   sigma:      accounting of earth's orbit ellipticity
!   decsin:     sine of declination
!   deccos:     cosine of declination
!   amm:        time in hours of meridian passage
!   hcos:       relates latitude and declination
!   h2:         sunrise/sunset solar angle
!   hi:         used to calculate hourly angle
!   alfsn:      used to calculate hourly angle
!
!   Logical variables
!   calc_ang:   logical variable to denote to calculate hourly solar angles
!
!   Character variables
!   modnam:     Subroutine name
!========================================================================================================= 
    use main1, only: d2r
    implicit none
    integer(kind=4),intent(in) :: tzone,jday
    integer(kind=4) :: h
    real(kind=r8),intent(in) :: rlat,rlon
    real(kind=r8),intent(out) :: sr,ss
    real(kind=r8),intent(out),optional :: angs(24)
    real(kind=r8) :: dum,tempz,sinlat,coslat,dayno,tdayno,sindayno,cosdayno,sintdayno,costdayno,sigma,decsin,deccos,amm,hcos,&
        h2,hi,alfsn
    logical :: calcang
    character(len=10) :: modnam='SUNDAT'
    
!   have to convert western(eastern) hemisphere to positive(negative) longitude
    dum=-rlon/15.0_r8-real(tzone,r8)
    tempz=15.0_r8*real(tzone,r8)-(-1.0_r8*rlon)
    sinlat=dsin(rlat*d2r)
    coslat=dcos(rlat*d2r)
    
    calcang=present(angs)
    

!   determine the fraction of year for this date
!   0.0172028=360/(365.242*57.29578)
    dayno=(real(jday,r8)-1.0_r8)*0.0172028_r8
    tdayno=2.0_r8*dayno
    sindayno=dsin(dayno)
    cosdayno=dcos(dayno)
    sintdayno=dsin(tdayno)
    costdayno=dcos(tdayno)
   
!   account for ellipticity of earth's orbit
    sigma=279.9348_r8+(dayno/d2r)+1.914827_r8*sindayno-0.079525_r8*cosdayno+0.019938_r8*sintdayno-0.00162_r8*costdayno
    
!   sine of solar declination
!   0.39785=sin(0.4091720193)=sin(23.44393/57.29578)
    
    decsin=0.39785_r8*dsin(sigma*d2r)
    deccos=dsqrt(1.0_r8-decsin*decsin)
    
!   time in hours of merdian passage
    amm=12.0_r8+0.12357_r8*sindayno-0.004289_r8*cosdayno+0.153809_r8*sintdayno+0.060783_r8*costdayno
    hcos=(-sinlat*decsin)/(coslat*deccos)
    
    
    if (dabs(hcos) < 1.0_r8) then
        if (hcos > -1.0_r8) then
!           determine solar hour angle of sunrise-sunset
            h2 = (datan2(dsqrt(1.0_r8-hcos*hcos),hcos)/15.0_r8)/d2r
!           Time of sunrise(SUNRISE) and time of sunset(SUNSET) are expressed
!           in local standard time since the zone correction has already 
!           been made.  Otherwise they would be in Greenwich Mean Time.
            sr=amm-h2+dum
            ss=amm+h2+dum
        else !sun never sets
            sr=24.0
            ss=24.0
        endif
    else !sun never rises
        sr=12.0_r8
        ss=12.0_r8
    endif
    
!calculate optional angles
    if (calcang) then
!       determine solar hour angle (in radians) for each hour of the day and convert to degrees
    d1: do h=1,24
            hi=(15.0_r8 * (real(h,r8)-amm) + tempz) * d2r
            alfsn = sinlat*decsin + deccos*coslat*dcos(hi)
            angs(h)= datan2(alfsn,sqrt(1.0-alfsn*alfsn))/d2r
        enddo d1
    endif
    return
    end subroutine sundat
!*********************************************************************************************************

    subroutine check_obs(iswitch,iday1)
!=========================================================================================================
!   SUBROUTINE CHECK_OBS
!   THIS SUBROUTINE DETERMINES IF THERE ARE UPPERAIR, SURFACE, OR ONSITE/PROG OBS FOR THE PBL DATE.
!
!   MODIFIED MARCH 31, 2022
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE (PBL) PBL_PROC
!
!   INPUT ARGUMENTS 
!
!   ISWITCH:    SWITCH TO INDICATE WHEN TO CHECK UPPER AIR DATA AND NWS, ONSITE/PROG, AND AERMINUTE
!               ISWITCH=1, CHECK FOR NWS, ONSITE/PROG, AND AERMINUTE
!               ISWITCH=2, CHECK FOR UPPER AIR
!   IDAY1:      DAY COUNTER FOR SFC_DATA
!
!   Variable definitions
!      
!   Integer variables
!   iswitch:    switch to indicate when to check upper air data
!   iday1:      day counter for sfc_data
!   i:          loop counter
!   iyr:        4-digit year for upper air data check
!   imon:       integer month for upper air data check
!   idy:        integer day of month for upper air data check
!   ihr:        integer hour of day for upper air data check
!   snd_hr:     2 element array of the hour of day for beginning and ending
!               window for upper air check (UAWINDOW)
!   target_hr:  target hour of day for a sounding (00Z or 12Z GMT)
!   itargetday: index in upper air data for target date for sounding
!   stophr:     hour to stop looking on the target date
!
!   Logical variables
!   lfound:     logical variable denoting current day found in various data
!               (upperair, surface, onsite/PROG, AERMINUTE output)
!   lfound1:    logical variable denoting if target hour or snd_hr hours found
!               in upper air data
!   lupper:     3 element array indicating if target and window soundings
!               found in upper air data
!   ljunk:      dummy logical variable for subroutine data_dates
!   lgo:        3-element array used in subroutine data_dates to 
!               determine if target date (index 1), and window sounding dates (indices 2 and 3)
!               found in upper air data.
!
!   Character variables
!   adate:      character string of date and is used to get year, month, and day.
!   debugform:  format for debug message for upper air data
!   modnam:     Subroutine name
!========================================================================================================= 
    use main1, only : data_dates,lpath,lkey
    use upperair, only: upstart,upend,upgmt2lst,nupdays,sound_info,have_soundings
    use surface, only: sfstart,sfend,sfc_info,sfstart1min,sfend1min,one_mindat,nsf1mindays,nsfdays,nsfc
    use onsite, only: osstart,osend,os_info,nosdays,nos
    implicit none
    integer(kind=4),intent(in) :: iswitch,iday1
    integer(kind=4) :: i,iyr,imon,idy,ihr,snd_hr(2),target_hr,itargetday,stophr,iupday,h
    integer(kind=8) :: snd_window(2),target_date
    logical lfound,lfound1,lupper(3),ljunk,lgo(3)
    character(len=8) :: adate
    character(len=80) :: debugform
    character(len=10) :: modnam='CHECK_OBS'
    
!   initialize
    lfound=.false.
    lupper=.false.
    itargetday=0
    
    write(debugform,'(a)')'(/t5,a,1x,i8,/t10,a//t13,i8,1x,i2.2,t37,i8,1x,i2.2,t57,i8,1x,i2.2)'
!   ************************************  UPPER AIR CHECK START *****************************************************
!   post 21DRF, check for number of soundings per day when iswitch = 1
    if (lpath(2) .and. iswitch == 1 .and. have_soundings) then
        iupday=1
        lfound=.false.
        do while (iupday <= nupdays .and. .not. lfound)
            if (sfc_data(iday1)%sfcdate == sound_info(iupday)%snddate) then
                lfound=.true.
                if (sound_info(iupday)%sounding) pbl_obs(1,iday1)=sound_info(iupday)%nsnding
            else
                iupday=iupday+1
            endif
        enddo
    endif
    
!   check upper air for target sounding if iswitch is not 1.
    if (lpath(2) .and. iswitch /= 1 .and. have_soundings) then
!       found the day in the upper air data, now determine sounding to use for convective mixing heights
        if (lactions(6)) then
!           set target_hr to upper air sunrise.  This time is already in LST
            target_hr=int(up_sunrise(iday1))
            write(adate,'(i8)')sfc_data(iday1)%sfcdate
            read(adate,'(i4,3(i2))')iyr,imon,idy
            
!           time is already in LST so GMT to LST is 0
            call data_dates(target_hr,idy,imon,iyr,0,upstart,upend,42,target_date,lgo(1))
!           calculate start and end windows for upper air data.  note times are already in LST
    h1:     do i=1,2
                snd_hr(i)=target_hr
                write(adate,'(i8)')sfc_data(iday1)%sfcdate
                read(adate,'(i4,3(i2))')iyr,imon,idy
!               since the beginning window may go into the previous day, treat snding_win as a GMT to LST
!               beginning(ending) window would be similar to going west (east) from prime meridan so
!               treat snding_win as negative.  beginning (ending) window will be positive (negative) which
!               is opposite of their values.  
                call data_dates(snd_hr(i),idy,imon,iyr,-snding_win(i),upstart,upend,42,snd_window(i),lgo(i+1))
            enddo h1
    
        else    
!           use default 00Z or 12Z sounding
            
!           get the date to check as the target sounding date, this could be the day before the day that
!           is being processed or the day being processed
!           see Table 4-1 of the AERMET user's guide for time zone and sounding assignments
!           note that in table, negative (positive) time zone adjustments are western (eastern)
!           hemisphere and are opposite of the AERMET convention where positive (negative) means
!           western (eastern) hemisphere.
            
!           initially, the day will be the day based on GMT
            write(adate,'(i8)')sfc_data(iday1)%sfcdate
            read(adate,'(i4,3(i2))')iyr,imon,idy
            
            if (my_sounding < 0) then                
!               get previous day; use subroutine data_dates from module main1 by assuming hour 1 and a GMT to LST of 2 to make 
!               previous day; remember a positive (negative) GMT to LST is western (eastern) hemisphere so actually subtracting
!               use upstart and upend as the start and end dates
                ihr=1
                call data_dates(ihr,idy,imon,iyr,2,upstart,upend,42,target_date,ljunk) 
                
!               now account for GMT to LST and reset target date to LST since upper air data in LST
                write(adate,'(i8)')target_date
                read(adate,'(i4,3(i2))')iyr,imon,idy
                
!               note target_hr that was set relative to GMT in PBL_PROC is now reset to LST
!               set target_hr to target hour in GMT
                target_hr=target_hr_gmt
                call data_dates(target_hr,idy,imon,iyr,upgmt2lst,upstart,upend,42,target_date,lgo(1))
            else
!               get current day in GMT, with hour 1 and GMT to LST of 0
                ihr=1
                call data_dates(ihr,idy,imon,iyr,0,upstart,upend,42,target_date,ljunk)
                
!               now account for GMT to LST and reset target date
                write(adate,'(i8)')target_date
                read(adate,'(i4,3(i2))')iyr,imon,idy
                
!               note target_hr that was set relative to GMT in PBL_PROC is now reset to LST
!               set target_hr to target hour in GMT
                target_hr=target_hr_gmt
                call data_dates(target_hr,idy,imon,iyr,upgmt2lst,upstart,upend,42,target_date,lgo(1))
            endif            
                
!           calculate the start and end windows for upper air data. note initial time will be in
!           GMT and converted to local time
    l2:     do i=1,2
!               GMT of hour at beginning (i=1) or end (in=2) of window
                snd_hr(i)=my_sounding+snding_win(i)
                
!               determine which days to use for the window.
                write(adate,'(i8)')sfc_data(iday1)%sfcdate
                read(adate,'(i4,3(i2))')iyr,imon,idy
                if (i == 1) then
                    if (my_sounding <= 0) then
!                       get previous day; use subroutine data_dates from module main1 by assuming hour 1  
!                       and a GMT to LST of 2 to make previous day; remember a positive (negative) GMT 
!                       to LST is western (eastern) hemisphere so actually subtracting
                        
!                       use upstart and upend as the start and end dates
                        ihr=1
                        call data_dates(ihr,idy,imon,iyr,2,upstart,upend,42,snd_window(i),ljunk) 
                    else
!                       get current day in GMT, with hour 1 and GMT to LST of 0
                        ihr=1
                        call data_dates(ihr,idy,imon,iyr,0,upstart,upend,42,snd_window(i),ljunk)
                    endif
                    
!                   now account for GMT to LST and reset date to LST since upper air data in LST
!                   snd_hr will be reset from GMT to LST
                    write(adate,'(i8)')snd_window(i)
                    read(adate,'(i4,3(i2))')iyr,imon,idy
                    call data_dates(snd_hr(i),idy,imon,iyr,upgmt2lst,upstart,upend,42,snd_window(i),lgo(2))
                    
                else
                    if (my_sounding < 0) then
!                       get previous day; use subrutine data_dates from module main1 by assuming hour 1  
!                       and a GMT to LST of 2 to make previous day; remember a positive (negative) GMT 
!                       to LST is western (eastern) hemisphere so actually subtracting
                        
!                       use upstart and upend as the start and end dates
                        ihr=1
                        call data_dates(ihr,idy,imon,iyr,2,upstart,upend,42,snd_window(i),ljunk) 
                    else
!                       get current day in GMT, with hour 1 and GMT to LST of 0
                        ihr=1
                        call data_dates(ihr,idy,imon,iyr,0,upstart,upend,42,snd_window(i),ljunk)
                    endif
!                   now account for GMT to LST and reset date
                    write(adate,'(i8)')snd_window(i)
                    read(adate,'(i4,3(i2))')iyr,imon,idy
!                   set snd_hr to snd_hr_gmt and then it will be reset to LST
                    snd_hr(i)=snd_hr_gmt(i)
                    call data_dates(snd_hr(i),idy,imon,iyr,upgmt2lst,upstart,upend,42,snd_window(i),lgo(3))
                    
                endif
                
            enddo l2
        endif  
        
!       if all 3 dates are not in the upper air data window then exit subroutine
        if (.not. lgo(1) .and. .not. lgo(2) .and. .not. lgo(3)) return
            
            
!       find a sounding to use. hiearchy:
!       1.  target date and hour or closest sounding within window ON
!           target date but BEFORE target hour, or
!           closest sounding within window ON target date but AFTER target hour. Note,
!           that before/after dates could be different from target date but first look
!           at all soundings on target date.
!       2.  closest sounding within window BEFORE target date and hr (different day)
!           than target date.
!       3.  closest sounding within window AFTER target date and hr
            
!       1.  target date and hour or a sounding the same day as the target date but a previous hour
!			(search limited to same day as target day.  if no sounding on target day
        if (lgo(1)) then
!           sounding is in upper air data period but need to check to see if 
!           there is a sounding for the day and hour.
!           get the day index of the target day.  it will be in the sound_info
!           data even if there are no soundings
            lfound=.false.
            do while (iup(iday1) <= nupdays .and. .not. lfound) 
                if (target_date == sound_info(iup(iday1))%snddate) then
                    lfound=.true.
                else
                    iup(iday1)=iup(iday1)+1
                endif     
            enddo
            itargetday=iup(iday1)

            if (sound_info(iup(iday1))%sounding) then
!               only proceed if this day has a sounding
!               find sounding that matches target hour and has data
                lfound1=.false.
                do while (isnd(iday1) <= sound_info(iup(iday1))%nsnding .and. .not. lfound1)
                    if (target_hr == sound_info(iup(iday1))%snd_hr(isnd(iday1)) .and. sound_info(iup(iday1))%nlevels(isnd(iday1)) &
                        > 0 ) then
                        lfound1=.true.
                    else
                        isnd(iday1)=isnd(iday1)+1
                    endif
                enddo  
                if (lfound1) then
                    lupper(1)=.true.
                else
!                   now see if any soundings on the target day before the target hour
!                   start at the end of the day and work backwards
                    if (snd_window(1) == target_date) then
!                       beginning is same day, stop at the 
                        stophr=snd_hr(1)
                    else
!                       go to the beginning of the day because the beginning of the window is before the target date                   
                        stophr=1
                    endif
!                   find sounding before target hour that is closest but after the stop hour defined above
                    isnd(iday1) = sound_info(iup(iday1))%nsnding
                    do while (isnd(iday1) >= 1 .and. .not. lfound1)
                        if (sound_info(iup(iday1))%snd_hr(isnd(iday1)) <= target_hr .and. &
                            sound_info(iup(iday1))%snd_hr(isnd(iday1)) >= stophr .and. &
                            sound_info(iup(iday1))%nlevels(isnd(iday1)) > 0) then
!						    sounding is between the window and target hour
                            lfound1=.true.
                        else
                            isnd(iday1)=isnd(iday1)-1
                        endif	
                    enddo
                    
                    if (lfound1) then
                        lupper(1)=.true.
                    else
!                       now look after the target_hour
                        if (snd_window(2) == target_date) then
!                           beginning is same day, stop at the sounding hour 
                            stophr=snd_hr(2)
                        else
!                           go to the end of the day because the beginning of the window is after the target date                   
                            stophr=24
                        endif
                    
                        isnd(iday1)=1 !reset
!                       find sounding before target hour that is closest but before the stop hour defined above
                        do while (isnd(iday1) <= sound_info(iup(iday1))%nsnding .and. .not. lfound1)
                            if (sound_info(iup(iday1))%snd_hr(isnd(iday1)) >= target_hr .and. &
                                sound_info(iup(iday1))%snd_hr(isnd(iday1)) <= stophr .and. &
                                sound_info(iup(iday1))%nlevels(isnd(iday1)) > 0) then
        !						sounding is between the window and target hour
                                lfound1=.true.
                            else
                                isnd(iday1)=isnd(iday1)+1
                            endif
                        enddo
                        if (lfound1)lupper(1)=.true.
                    endif
                endif
            endif
        endif
                
!       2.  closest sounding within window BEFORE target date/hr but not same day as target date

!       no sounding found for target date and hour, look in the window before
        if (lgo(2) .and. .not. lupper(1) .and. snd_window(1) /= target_date) then
!          sounding is in upper air data period but need to check to see if 
!           there is a sounding for the day and hour.
!           get the day index of the target day.  it will be in the sound_info
!           data even if there are no soundings
            lfound=.false.
!           reset iup for the day. if lgo(1) is true but lupper(1) is false, the day
!           was found, so reset iup(iday1) to 2 days before.  if both lgo(1) and lupper(1) are
!           both false, reset to iup(iday1) to 1
            if (lgo(1) .and. iup(iday1) > 3) then
                iup(iday1)=iup(iday1)-2
            else
                iup(iday1)=1
            endif
                
            do while (iup(iday1) <= nupdays .and. .not. lfound) 
                if (snd_window(1) == sound_info(iup(iday1))%snddate) then
                    lfound=.true.
                else
                    iup(iday1)=iup(iday1)+1
                endif     
            enddo
            if (sound_info(iup(iday1))%sounding) then
                lfound1=.false.
!               find sounding that is in the window and closest to target hour
!               count backwards from total # of soundings instead of starting at 1
                isnd(iday1)=sound_info(iup(iday1))%nsnding
                    
                do while (isnd(iday1) >= 1 .and. .not. lfound1)
!					beginning window sounding is on a different day so the hour will be >= than target_hour
                    if (sound_info(iup(iday1))%snd_hr(isnd(iday1)) >= target_hr .and. &
                        sound_info(iup(iday1))%snd_hr(isnd(iday1)) >= snd_hr(1) .and. &
                        sound_info(iup(iday1))%nlevels(isnd(iday1)) > 0) then
!						sounding is between the window and target hour
                        lfound1=.true.
                    else
                        isnd(iday1)=isnd(iday1)-1
                    endif
                enddo  
                if (lfound1) lupper(2)=.true. 
            endif
        endif

            
!       3.  closest sounding within window AFTER target date/hr but not same day
!       no sounding for the target hour or window before, look after the target hour
            
        if (lgo(3) .and. .not. lupper(1) .and. .not. lupper(2) .and. snd_window(2) /= target_date) then

 !          sounding is in upper air data period but need to check to see if 
!           there is a sounding for the day and hour.
!           get the day index of the target day.  it will be in the sound_info
!           data even if there are no soundings
            lfound=.false.
!           reset iup for the day. if lgo(1) is true but lupper(1) is false, the day
!           was found, so reset iup(iday1) to 2 days before.  if both lgo(1) and lupper(1) are
!           both false, reset to iup(iday1) to 1
            if (lgo(1)) then
                iup(iday1)=itargetday
            else
                iup(iday1)=1
            endif
        
            do while (iup(iday1) <= nupdays .and. .not. lfound) 
                if (snd_window(2) == sound_info(iup(iday1))%snddate) then
                    lfound=.true.
                else
                    iup(iday1)=iup(iday1)+1
                endif     
            enddo
            if (sound_info(iup(iday1))%sounding) then
                lfound1=.false.
!               find sounding that is in the window and closest after target hour
                isnd(iday1)=1
                    
                do while (isnd(iday1) <= sound_info(iup(iday1))%nsnding .and. .not. lfound1)
!					beginning window sounding is on a different day so the hour will be <= than target_hour
                    if (sound_info(iup(iday1))%snd_hr(isnd(iday1)) <= target_hr .and. &
                        sound_info(iup(iday1))%snd_hr(isnd(iday1)) <= snd_hr(2) .and. &
                        sound_info(iup(iday1))%nlevels(isnd(iday1)) > 0) then
!						sounding is between the window and target hour
                        lfound1=.true.
                    else
                        isnd(iday1)=isnd(iday1)+1
                    endif
                enddo  
                if (lfound1) lupper(3)=.true. 
            endif       
        endif
        
        if (debug)write(debug_unit,debugform)'SOUNDING WINDOW FOR',sfc_data(iday1)%sfcdate,&
            'BEGIN WINDOW DATE/HR     TARGET DATE/HR    ENDING WINDOW DATE/HR',snd_window(1),snd_hr(1),target_date,target_hr,&
            snd_window(2),snd_hr(2)
        if (lupper(1) .or. lupper(2) .or. lupper(3)) up_obs(iday1)=.true.
                            
    endif
!   ************************************  UPPER AIR CHECK END *******************************************************
    
!   ************************************  SURFACE CHECK START *******************************************************
    if (lpath(3) .and. sfc_data(iday1)%sfcdate >= sfstart .and. sfc_data(iday1)%sfcdate <= sfend .and. iswitch ==1 .and. &
        nsfc(2) > 0) then
        lfound=.false.
        
        do while(sfc_data(iday1)%sfcdate >= sfstart .and. sfc_data(iday1)%sfcdate <= sfend .and. isf(iday1) <= nsfdays .and. &
            .not. lfound)
      
            if (sfc_data(iday1)%sfcdate == sfc_info(isf(iday1))%sfcdate) then
                day_obs(iday1)=sfc_info(isf(iday1))%sobs
                nws_obs(:,iday1)=sfc_info(isf(iday1))%have_obs
                asos_hr(:,iday1)=sfc_info(isf(iday1))%asos_hr
                !endif
                lfound=.true.
            else
                isf(iday1)=isf(iday1)+1
            endif
        enddo
!       post 21DRF, number of hours with an observation
        if (lfound) then
    h2:	    do h=1,24
                if (nws_obs(h,iday1))pbl_obs(2,iday1)=pbl_obs(2,iday1)+1
            enddo h2
        endif
    endif    
!   ************************************  SURFACE CHECK END *********************************************************
    
!   ************************************  AERMINUTE CHECK START *****************************************************

    if (lpath(3) .and. lkey(32) .and. sfc_data(iday1)%sfcdate >= sfstart1min .and. sfc_data(iday1)%sfcdate <= sfend1min .and. &
        iswitch ==1) then
        lfound=.false.
        
        do while(sfc_data(iday1)%sfcdate >= sfstart1min .and. sfc_data(iday1)%sfcdate <= sfend1min .and. isf1(iday1) <= &
            nsf1mindays .and. .not. lfound)
          
            if (sfc_data(iday1)%sfcdate == one_mindat(isf1(iday1))%sf1mindate) then
                one_min_obs(:,iday1)=.true.  !have 1-minute data but it may be missing
                lfound=.true.
            else
                isf1(iday1)=isf1(iday1)+1
            endif
        enddo
!       post 21DRF, number of hours with an observation
        if (lfound) then
    h3:	    do h=1,24
                if (one_min_obs(h,iday1))pbl_obs(4,iday1)=pbl_obs(4,iday1)+1
            enddo h3
        endif
    endif    
!   ************************************  AERMINUTE CHECK END *******************************************************

!   ************************************  ONSITE/PROG CHECK START ****************************************************
    if ((lpath(4) .or. lpath(5)) .and. sfc_data(iday1)%sfcdate >= osstart .and. sfc_data(iday1)%sfcdate <= osend .and. &
        iswitch ==1 .and. nos(2) > 0) then
        lfound=.false.
      
        do while(sfc_data(iday1)%sfcdate >= osstart .and. sfc_data(iday1)%sfcdate <= osend .and. ios(iday1) <= nosdays .and. &
            .not. lfound)
            if (sfc_data(iday1)%sfcdate == os_info(ios(iday1))%osdate) then
!               set day_obs if false, if it is true, leave it as is
                if (.not. day_obs(iday1)) day_obs(iday1)=os_info(ios(iday1))%sobs
                os_obs(:,iday1)=os_info(ios(iday1))%have_obs
                lfound=.true.
            else
                ios(iday1)=ios(iday1)+1
            endif
        enddo
!       post 21DRF, number of hours with an observation
        if (lfound) then
    h4:	    do h=1,24
                if (os_obs(h,iday1))pbl_obs(3,iday1)=pbl_obs(3,iday1)+1
            enddo h4
        endif			
    endif    
!   ************************************  ONSITE/PROG CHECK END *******************************************************
     
    return
    end subroutine check_obs
!*********************************************************************************************************

!    subroutine winds(d,h)
    subroutine winds(d,h,wind_reset,orig_wind_speed)
!=========================================================================================================
!   SUBROUTINE WINDS
!   THIS SUBROUTINE ASSIGNS WINDS FOR THE HOUR AND ASSIGNS PROFILE VALUES FOR THE WINDS
!
!   MODIFIED MARCH 1, 2022
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:          DAY COUNTER
!   H:          HOUR COUNTER
!   OUTPUT ARGUMENTS
!
!   WIND_RESET:         LOGICAL FLAG DENOTING WIND SPEED RESET TO MINIMUM WIND SPEED
!   ORIG_WIND_SPEED:    ORIGINAL WIND SPEED BEFORE RESET (IF NEEDED)
!
!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!   ilev:       level counter
!   i:          loop counter
!   isite:		integer indicator of site type for surface characteristics
!				processing and message (1=primary, 2=secondary)
!
!   Real variables
!   albedo:     albedo from surface characteristics array
!   bowen:      Bowen ratio from surface characteristicsl array
!   zo:         surface roughness from surface characteristics array
!   nws_speed:  wind speed from NWS station
!   nws_dir:    wind direction from NWS station
!   zmax:       maximum height to search for winds in onsite data
!   recoeff:    coefficient to multiply zo by for checking validity of level
!   asosadj:    adjustment factor to add to ASOS wind speed (m/s)
!   minsigv:    minimum sigma-v to compare against for wind speed
!   orig_wind_speed:    original wind speed before reset (if needed)
!
!   Logical variables
!   lgotwind:   have a wind for the hour and level
!   misswind:   logical variable denoting missing wind direction (1) or wind speed (2)
!   missht:     height missing for level
!   lskip:      logical variable denoting to skip if level missing height or winds
!   calmhr:     hour is calm
!   onemin:     1-minute hourly ASOS wind is used
!   varwind:    logical variable denoting wind is a variable wind for standard hourly obs from NWS
!   validos:	have valid onsite/prognostic observation
!   calmht:     have height for calm wind
!   wind_reset: logical variable denoting that wind speed has been reset to minimum wind speed; added post 21DRF
!
!   Character variables
!   date_hr_str:    date and hour string
!   formstr:	formats for messages
!   modnam:     Subroutine name
!========================================================================================================= 
    use main1, only: lpath,msg_form
    use file_units, only: msg_unit
    use surface, only: asos_thresh,nws_hgt,sfdata,one_mindat,sfvars
    use onsite, only: nlevel,lcalm_obs_hr,wind_vars,height_var,alb_var,zo_var,bo_var,sa_var,sw_var,overland
    implicit none
    integer(kind=4),intent(in) :: d,h
    integer(kind=4) :: ilev,i,isite
    real(kind=r8) :: albedo,bowen,zo,nws_speed,nws_dir
    real(kind=r8), intent(out) :: orig_wind_speed
    real(kind=r8),parameter :: zmax=100._r8,refcoeff=7.0_r8, asosadj=0.257_r8,minsigv=0.2_r8
    logical lgotwind,misswind(2),missht,lskip,calmhr,onemin,varwind,validos,calmht
    logical, intent(out) :: wind_reset
    character(len=20) :: date_hr_str
    character(len=60) :: formstr(2)
    character(len=10) :: modnam='WINDS'
    
!   post 21DRF
    wind_reset=.false.
    
!   formats for messages
!   1.  reset winds or zo
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20)'
    
!   2.  wind height < 20*z0
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a20,1x,a,1x,f6.2,1x,a,1x,f4.2)'
 
    calmhr=.false.
    lgotwind=.false.
    onemin=.false.
    varwind=.false.
    validos=.false.
    calmht=.false.
    
     write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sfc_data(d)%sfcdate,'HR',h
     
!   hiearchy of winds
!   if onsite winds, search through levels for first non-calm level below zmax and use that level
!   if no levels, then check the NWS data if applicable and use that if valid
   
!   check onsite first if onsite winds are being processed
    !if (sfc_data(d)%os_obs(h) .and. osvars(wind_vars(1))%lread .and. osvars(wind_vars(2))%lread) then
    if (os_obs(h,d) .and. osvars(wind_vars(1))%lread .and. osvars(wind_vars(2))%lread) then
        isite=1
        
!       find the position in osdata1 to read.  look for direction because speed will be the next
!       variable.
!       check read_index(1), if it is height, then no adjustments needed because the order of data
!       that was in osdata is the same as osdata1 which is used here.
!       if the first variable is not height, then need to adjust where to look in osdata1
        
        
!       find the first non-calm and non-missing level but below zmax (defined above)
        ilev=1
        misswind=.false.
        missht=.false.
        do while(ilev <= nlevel .and. .not. lgotwind)
            misswind=.false.
            missht=.false.
!           check wind direction (i=1) and wind speed(i=2) if not calm hour
            
            if (.not. lcalm_obs_hr(ilev,h,ios(d))) then
                calmhr=.false.
    l1:         do i=1,2
                    
                    if (dabs(osdata1(osvars(wind_vars(i))%readvar,ilev,h,ios(d))-osvars(wind_vars(i))%missval) <= eps) then
                        misswind(i)=.true.
                    else
                        misswind(i)=.false.
                    endif
                    
                enddo l1
              
            else
!               set height to this level temporarily
                if (sfc_data(d)%wind_ht(h) < 0.0_r8) sfc_data(d)%wind_ht(h)=osdata1(1,ilev,h,ios(d))
                calmhr=.true.
                calmht=.true.
                validos=.true.
            endif
!           check for missing height
            if (dabs(osdata1(1,ilev,h,ios(d))-osvars(height_var)%missval) <= eps) then
                missht=.true.
            else
                missht=.false.
            endif
!           set lskip based on missing winds or height
            if (misswind(1) .or. misswind(2) .or. missht) then
                lskip=.true.
            else
                lskip=.false.
            endif
  
!           if not calm and not missing winds and not missing height proceed

            if (.not. lcalm_obs_hr(ilev,h,ios(d)) .and. .not. lskip .and. osdata1(1,ilev,h,ios(d)) <= zmax .and. .not. &
                lgotwind) then
!               get the surface characteristics for this wind direction and check to see if
!               the height is above 7*zo
                
                call sfc_chars(isite,d,osdata1(osvars(wind_vars(1))%readvar,ilev,h,ios(d)),albedo,bowen,zo)
!               assign winds and surface characteristics to the hour if height > refcoeff*zo
                if (osdata1(1,ilev,h,ios(d)) >= refcoeff*zo) then
!                   use hourly zo (if overwater) or value from SITE_CHAR
                    if (osvars(zo_var)%lread .and. .not. overland) then
                     
                        if (dabs(osdata1(osvars(zo_var)%readvar,1,h,ios(d))-real(osvars(zo_var)%missval,r8)) > eps .and. &
                            osdata1(osvars(zo_var)%readvar,1,h,ios(d)) /= 0.0_r8) then
                            sfc_data(d)%zo(h)=osdata1(osvars(zo_var)%readvar,1,h,ios(d))
                            if (sfc_data(d)%zo(h) < 0.0001_r8) then
                                sfc_data(d)%zo(h)=0.0001_r8
                                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W75',modnam,'ZO < 0.0001; RESET TO 0.001 FOR',&
                                    date_hr_str
                            endif
                        else
                            sfc_data(d)%zo(h)=zo
                        endif
                    else
                        sfc_data(d)%zo(h)=zo
                    endif
!                   use hourly albedo (if overwater) or value from SITE_CHAR
                    if (osvars(alb_var)%lread .and. .not. overland) then
                       
                        if (dabs(osdata1(osvars(alb_var)%readvar,1,h,ios(d))-real(osvars(alb_var)%missval,r8)) > eps) then
                            sfc_data(d)%albedo(h)=osdata1(osvars(alb_var)%readvar,1,h,ios(d))
                        else
                            sfc_data(d)%albedo(h)=albedo
                        endif
                    else
                        sfc_data(d)%albedo(h)=albedo
                    endif
!                   use hourly Bowen ratio (if overwater) or value from SITE_CHAR
                    if (osvars(bo_var)%lread .and. .not. overland) then
                        
                        if (dabs(osdata1(osvars(bo_var)%readvar,1,h,ios(d))-real(osvars(bo_var)%missval,r8)) > eps) then
                            sfc_data(d)%bowen(h)=osdata1(osvars(bo_var)%readvar,1,h,ios(d))
                        else
                            sfc_data(d)%bowen(h)=bowen
                        endif
                    else
                        sfc_data(d)%bowen(h)=bowen
                    endif
                    
                    sfc_data(d)%wspd(h)=osdata1(osvars(wind_vars(2))%readvar,ilev,h,ios(d))
                    sfc_data(d)%wdir(h)=osdata1(osvars(wind_vars(1))%readvar,ilev,h,ios(d))
                    sfc_data(d)%wind_ht(h)=osdata1(1,ilev,h,ios(d))
                    
                    lgotwind=.true.
                    validos=.true.
                    windsrc(h,d)=1
                    
!                   issue message if wind height is below 20*zo
                    if (sfc_data(d)%wind_ht(h) < 20.0_r8*sfc_data(d)%zo(h))write(msg_unit,formstr(2))adjustl(pathid(ipath)),&
                        'I73',modnam,date_hr_str,'WIND HEIGHT',sfc_data(d)%wind_ht(h),'< 20X Z0',sfc_data(d)%zo(h)
                    
                else !not above 7zo, keep searching
                    ilev=ilev+1
                endif
            else
                ilev=ilev+1
            endif
        enddo
        
!       if no wind found but there is a calm, set the obs to calm for now and height to first level
        !if (.not. lgotwind .and. .not. lskip) then
        if (.not. lgotwind .and. calmht) then
            sfc_data(d)%wspd(h)=0.0_r8
            sfc_data(d)%wdir(h)=0.0_r8
            
            calmhr=.true.
        endif
!       reset calm winds to missing and sigmas to missing in the profile data
!       set winds and sigma's if not missing/calm 
!       temperature will be handled later in subroutine pbl_proc after subroutine substitute
    l2: do ilev=1,nlevel
            !pfl_data(ilev,d)%ht(h)=osdata1(1,ilev,h,ios(d))
            pfl_data(h,d)%ht(ilev)=osdata1(1,ilev,h,ios(d))
            if (lcalm_obs_hr(ilev,h,ios(d))) then
                pfl_data(h,d)%speed(ilev)=osvars(wind_vars(2))%missval*osvars(wind_vars(2))%conv
                pfl_data(h,d)%dir(ilev)=osvars(wind_vars(1))%missval*osvars(wind_vars(1))%conv
            else
!               wind direction
                if (osvars(wind_vars(1))%lread) then
                    if (dabs(osdata1(osvars(wind_vars(1))%readvar,ilev,h,ios(d))-real(osvars(wind_vars(1))%missval,r8)) > eps) &
                        pfl_data(h,d)%dir(ilev)=osdata1(osvars(wind_vars(1))%readvar,ilev,h,ios(d))
                endif
!               wind speed
                if (osvars(wind_vars(2))%lread) then
                    if (dabs(osdata1(osvars(wind_vars(2))%readvar,ilev,h,ios(d))-real(osvars(wind_vars(2))%missval,r8)) > eps) &
                        pfl_data(h,d)%speed(ilev)=osdata1(osvars(wind_vars(2))%readvar,ilev,h,ios(d))
                endif

            endif
!           sigma-theta
            if (osvars(sa_var)%lread) then
                if (dabs(osdata1(osvars(sa_var)%readvar,ilev,h,ios(d))-real(osvars(sa_var)%missval,r8)) > eps .and. &  
                    osdata1(osvars(sa_var)%readvar,ilev,h,ios(d)) >= 0.0_r8 ) &
                    pfl_data(h,d)%sigma_a(ilev)=osdata1(osvars(sa_var)%readvar,ilev,h,ios(d))
            endif
!           sigma-w
            if (osvars(sw_var)%lread) then
                if (dabs(osdata1(osvars(sw_var)%readvar,ilev,h,ios(d))-real(osvars(sw_var)%missval,r8)) > eps .and. & 
                    osdata1(osvars(sw_var)%readvar,ilev,h,ios(d)) >= 0.0_r8) &
                    pfl_data(h,d)%sigma_w(ilev)=osdata1(osvars(sw_var)%readvar,ilev,h,ios(d))
            endif
        enddo l2
        
    endif
  
!   surface data
!   process if no valid onsite data for the hour or the onsite obs was calm and NWS data substitution requested.
    if ((.not. os_obs(h,d) .or. (os_obs(h,d) .and. .not. lgotwind)) .and. nws_obs(h,d) .and. lactions(3)) then
!       check to see if there is 1-minute average wind
!       if so, use it instead of standard obs
!       do not reset calmhr because 0,0 is allowed for 1-minute data
     
        if (one_min_obs(h,d)) then
            nws_speed=one_mindat(isf1(d))%wspd(h)
            nws_dir=one_mindat(isf1(d))%wdir(h)

!           adjust the wind speed if requested
!           if wind speed or direction is not missing set onemin=true; onemin is set to false at top of subroutine
            if (nws_speed < 900._r8 .and. nws_dir < 900._r8 .and. nws_speed >= 0.0_r8 .and. nws_dir > 0.0_r8 ) onemin=.true.
            if (nws_speed == 0.0_r8 .and. nws_dir == 0.0_r8) calmhr=.true.
         
            if (.not. lactions(5) .and. onemin) then
                nws_speed=nws_speed+asosadj
            endif
            if (lpblkeywords(17) .and. nws_speed < asos_thresh) then
                nws_dir=0.0_r8
                nws_speed=0.0_r8
                calmhr=.true.
!				post 21DRF, increment ASOS threshold counter
                pbl_windstats(2)=pbl_windstats(2)+1
            else
                calmhr=.false.
            endif
        endif
        if (.not. onemin) then !use standard obs if no 1-minute data for the hour
!           both direction and speed are not missing
            nws_dir=sfdata(10,h,isf(d))
            nws_speed=sfdata(11,h,isf(d))
           
            if (nws_dir == 0.0_r8 .or. nws_speed == 0.0_r8) then
                calmhr=.true. !set to calm if 0,0
                nws_dir=0.0_r8
                nws_speed=0.0_r8
            else
                calmhr=.false. !calmhr may have been set to true for 1-minute data
            endif
!           check for variable wind. if wind speed <= 3.1 m/s,wind direction is missing, and date is after 07/01/1996 then it is
!           a variable wind
            if (nws_speed <= 3.1_r8 .and. .not. calmhr .and. dabs(nws_dir-sfvars(10)%missval) <= eps .and. sfc_data(d)%sfcdate >= &
            19960701) varwind=.true.
!           adjust wind speed if ASOS obs and not missing and no adjustment not requested
            if (.not. validos) then
                
            endif
           
            if (asos_hr(h,d) .and. dabs(nws_speed-real(sfvars(11)%missval,r8)) > eps .and. .not. lactions(5) .and. .not. &
                calmhr) nws_speed=nws_speed+asosadj
              
!           randomnize wind direction if requested or default
            !if (.not. lactions(2) .and. nws_dir < sfvars(10)%missval*sfvars(11)%conv) then
            if (.not. lactions(2) .and. nws_dir < sfvars(10)%missval .and. .not. calmhr) then
                nws_dir=nws_dir+real(irnd(h,sfc_data(d)%jday),r8)-4.0_r8
                if (nws_dir > 360._r8) then
                    nws_dir=nws_dir-360._r8
                elseif (nws_dir < 0.0_r8) then
                    nws_dir=nws_dir+360._r8
                endif
            endif

        endif
!       now get the surface characteristics
!       if no onsite data at all, then isite=1 (primary), otherwise, isite=2 (secondary)
!       if onsite data, will use the albedo and Bowen ratio from the onsite surface characteristics
!       and surface roughness from the secondary site (NWS site).
!       if no onsite data, then use primary site (NWS site)
        if (lpath(4)) then !don't need to worry about lpath(5) because can't have PROG and SURFACE data
!           first get albedo and Bowen ratio of primary site using NWS wind direction
            isite=1
            call sfc_chars(isite,d,nws_dir,albedo,bowen,zo)
            sfc_data(d)%bowen(h)=bowen
            sfc_data(d)%albedo(h)=albedo
!           now get zo
            isite=2
            call sfc_chars(isite,d,nws_dir,albedo,bowen,zo)
            sfc_data(d)%zo(h)=zo
        else
            isite=1
            call sfc_chars(isite,d,nws_dir,albedo,bowen,zo)
            sfc_data(d)%zo(h)=zo
            sfc_data(d)%bowen(h)=bowen
            sfc_data(d)%albedo(h)=albedo
        endif
!       set instrument height, if variable wind or both speed and direction not missing then
!       set the height. if both are missing then leave as missing
        if (varwind .or. (dabs(nws_speed-sfvars(11)%missval) > eps .and. dabs(nws_dir-sfvars(10)%missval) > eps)) &
        sfc_data(d)%wind_ht(h)=nws_hgt
        
!       now check to see if wind is below mininum value and reset if so
        if (nws_speed < dsqrt(2.d0)*minsigv .and. .not. calmhr) then
            write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I74',modnam,sfc_data(d)%sfcdate,h
            nws_speed = dsqrt(2.d0)*minsigv
        endif
        if (nws_speed < 900._r8) then
            sfc_data(d)%wspd(h)=nws_speed
            sfc_data(d)%wdir(h)=nws_dir
            lgotwind=.true.            
        else
            lgotwind=.false.
            calmhr=.false.
            nws_dir=999._r8
        endif

!       if no onsite for the hour, set pfl data
        if (.not. os_obs(h,d)) then
            pfl_nlevels(h,d)=1
            pfl_data(h,d)%speed(1)=nws_speed
            pfl_data(h,d)%dir(1)=nws_dir
            pfl_data(h,d)%ht(1)=nws_hgt
        endif
    endif
    
!   set wind flags, default is NAD; if none of these conditions, then will be NAD
   
    if (windsrc(h,d)==1) then
!       valid non-calm, non-missing onsite or prognostic wind obs
        if (lpath(4)) then
            sfc_data(d)%windsrcflag(h)='NAD-OS'
        else
            sfc_data(d)%windsrcflag(h)='PROG-OS'
        endif
    else
        if (validos .and. calmhr) then
!           both onsite and NWS data are calm, keep as onsite
            if (lpath(4)) then
                sfc_data(d)%windsrcflag(h)='NAD-OS'
            else
                sfc_data(d)%windsrcflag(h)='PROG-OS'
            endif
        elseif (lgotwind) then
!           use NWS
            windsrc(h,d)=2
            if (.not. lactions(5)) then
!               adjusted 
                if (onemin) then
                    sfc_data(d)%windsrcflag(h)='ADJ-A1'
                else
                    if (asos_hr(h,d)) then
!                       ASOS station for the hour
                        sfc_data(d)%windsrcflag(h)='ADJ-SFC'
                    else
!                       not an ASOS station for the hour
                        sfc_data(d)%windsrcflag(h)='NAD-SFC'
                    endif
                endif
            else
!               non-adjusted
                if (onemin) then
                    sfc_data(d)%windsrcflag(h)='NAD-A1'
                else
                    sfc_data(d)%windsrcflag(h)='NAD-SFC'
                endif
            endif
        endif
    endif
    
!   reset wind flag 
    if (sfc_data(d)%wspd(h) > 900._r8 .and. sfc_data(d)%wdir(h) > 900._r8) sfc_data(d)%windsrcflag(h)='NAD'    

!   if no wind direction, then need to calculate average zo
    if (.not. lgotwind .or. calmhr .or. varwind) then
        isite=1
        call sfc_chars(isite,d,999.0_r8,albedo,bowen,zo)
        sfc_data(d)%bowen(h)=bowen
        sfc_data(d)%albedo(h)=albedo
!       initially set zo to the primary site, may get reset below
        sfc_data(d)%zo(h)=zo
        if (lpath(4) .and. ((varwind .or. calmhr) .and. windsrc(h,d)==2 .and. .not. validos)) then
!           NWS site used, reset zo
            isite=2
            call sfc_chars(isite,d,999.0_r8,albedo,bowen,zo)
            sfc_data(d)%zo(h)=zo
        endif
    endif
 
!   post 21DRF, if calm hour increment counter
!   if variable wind increment variable wind counter
!   ASOS calm threshold counter set above
    if (calmhr) pbl_windstats(1)=pbl_windstats(1)+1
    if (varwind) pbl_windstats(3)=pbl_windstats(3)+1
!   now check to see if wind is below mininum value and reset if so. note if NWS
!   winds used, it has already been reset. this is more for ONSITE or PROG data
    orig_wind_speed=sfc_data(d)%wspd(h)
    if (sfc_data(d)%wspd(h) < dsqrt(2.d0)*minsigv .and. .not. calmhr) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I74',modnam,'RESET WIND TO 0.282 M/S FOR',date_hr_str
        sfc_data(d)%wspd(h) = dsqrt(2.d0)*minsigv
        wind_reset=.true.
    endif
    

    return
    end subroutine winds
!*********************************************************************************************************

    subroutine temps(d,h)
!=========================================================================================================
!   SUBROUTINE TEMPS
!   THIS SUBROUTINE ASSIGNS TEMPERATURES FOR THE HOUR FOR THE SURFACE AND FILLS IN THE PROFILE TEMPS
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:      day counter
!   h:      hour counter
!   ilev:       level counter
!
!   Real variables
!   zmax:       maximum height to search for temperatures in onsite data
!
!   Logical variables
!   lgottemp:   have a temperature for the hour and level
!   misstemp:   logical variable denoting missing temperature
!   missht:     height missing for level
!   lskip:      logical variable denoting to skip if level missing height or temperature
!
!   Character variables
!   modnam:     Subroutine name
!========================================================================================================= 
    use surface, only: sfdata,sfvars
    use onsite, only: nlevel,height_var,temp_var
    implicit none
    integer(kind=4),intent(in) :: d,h
    integer(kind=4) :: ilev
    real(kind=r8),parameter :: zmax=100._r8
    logical lgottemp,misstemp,missht,lskip
    character(len=10) :: modnam='TEMPS'
    
!   if onsite obs, then use those
    lgottemp=.false.
    
    if (os_obs(h,d) .and. osvars(temp_var)%lread) then
!       find the first temperature level above zo and  below zmax (defined above)
        ilev=1
        
        misstemp=.false.
        missht=.false.
        lgottemp=.false.

        do while(ilev <= nlevel  .and. .not. lgottemp)
!           check temperature
            if (dabs(osdata1(osvars(temp_var)%readvar,ilev,h,ios(d))-real(osvars(temp_var)%missval,r8))<= eps) then
                misstemp=.true.
            else
                misstemp=.false.
            endif
!           check for missing height
            if (dabs(osdata1(1,ilev,h,ios(d))-real(osvars(height_var)%missval,r8)) <= eps) then
                missht=.true.
            else
                missht=.false.
            endif
!           set lskip based on missing temperature or height
            if (misstemp .or. missht) then
                lskip=.true.
            else
                lskip=.false.
            endif

!           if not missing temperature or height proceed
            if (.not. lskip .and. osdata1(1,ilev,h,ios(d)) <= zmax .and. .not. lgottemp) then

!               assign temperature
                if (osdata1(1,ilev,h,ios(d)) >= sfc_data(d)%zo(h)) then
                    sfc_data(d)%airtemp(h)=osdata1(osvars(temp_var)%readvar,ilev,h,ios(d))+273.15_r8
                    sfc_data(d)%temp_ht(h)=osdata1(1,ilev,h,ios(d))
                    lgottemp=.true.
                    tempsrc(h,d)=1
                else !not above zo, keep searching
                    ilev=ilev+1
                endif
            else
                ilev=ilev+1
            endif
        enddo
    endif
    
!   surface data
!   process if no valid onsite data for the hour  and NWS data substitution requested.
    if ((.not. os_obs(h,d) .or. (os_obs(h,d) .and. .not. lgottemp)) .and. nws_obs(h,d) .and. lactions(3)) then
        if (dabs(sfdata(7,h,isf(d))-real(sfvars(7)%missval,r8)) > eps) then
            sfc_data(d)%airtemp(h)=sfdata(7,h,isf(d))+273.15_r8
            sfc_data(d)%temp_ht(h)=2.0_r8
            tempsrc(h,d)=2
        endif
    endif

!   set pfl_data to NWS obs if no onsite data read in

    if (nws_obs(h,d)) then
        if (.not. os_obs(h,d) .and. dabs(sfdata(7,h,isf(d))-real(sfvars(7)%missval,r8)) > eps) &
        pfl_data(h,d)%airtemp(1)=sfdata(7,h,isf(d))
    endif
    return    
    end subroutine temps
!*********************************************************************************************************

    subroutine clouds(d,h)
!=========================================================================================================
!   SUBROUTINE CLOUDS
!   THIS SUBROUTINE ASSIGNS CLOUD COVER FOR THE HOUR
!
!   MODIFIED MARCH 10, 2022
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!   i:          loop counter
!   icloud:     cloud types (total, opaque, ASOS)
!   misscloud:  missing values for total, opaque, and ASOS
!
!   Logical variables
!   lfound:     logical variable indicating if day found
!   lgotcloud:  have clouds for the hour
!
!   Character variables
!   modnam:     Subroutine name
!========================================================================================================= 
    use main1, only: lpath
    use surface, only: sfdata,sfvars
    use onsite, only: cloud_var
    
    implicit none
    integer(kind=4),intent(in) :: d,h 
    integer(kind=4) :: i,icloud(3),misscloud(3)
    logical lfound,lgotcloud
    character(len=10) :: modnam='CLOUDS'
    
!   if onsite obs, then use those
!   post 21DRF, use oncloud logical variable instead of lread
    lgotcloud=.false.
!    if (os_obs(h,d) .and. osvars(cloud_var)%lread) then
    if (os_obs(h,d) .and. oncloud) then
        if (dabs(osdata1(osvars(cloud_var)%readvar,1,h,ios(d))- real(osvars(cloud_var)%missval,r8)) > eps) then
            sfc_data(d)%ccvr(h)=nint(osdata1(osvars(cloud_var)%readvar,1,h,ios(d)))
            cloudsrc(h,d)=1
            lgotcloud=.true.
        endif
    endif
    
!   surface data
    if (lpath(3)) then
!       process if no valid onsite data for the hour and there are surface obs of cloud cover
!       don't need SUBNWS keyword for cloud cover, automatically done
        if ((.not. os_obs(h,d) .or. (os_obs(h,d) .and. .not. lgotcloud)) .and. (nws_obs(h,d) .and. &
            (sfdata(4,h,isf(d)) /= real(sfvars(4)%missval,r8) .or. sfdata(6,h,isf(d)) /= &
            real(sfvars(6)%missval,r8)))) then
!           assign values to icloud (1=opaque, 2=total, 3=ASOS)
            icloud(2)=int(sfdata(4,h,isf(d)))/100 !total
            icloud(1)=int(sfdata(4,h,isf(d)))-icloud(2)*100 !opaque
            icloud(3)=int(sfdata(6,h,isf(d))) !ASOS
            misscloud(2)=sfvars(4)%missval/100
            misscloud(1)=sfvars(4)%missval-misscloud(2)*100
            misscloud(3)=sfvars(6)%missval
            i=1
            lfound=.false.
            do while (i <= 3 .and. .not. lfound)
                if (icloud(i) /= misscloud(i)) then
                    lfound=.true.
                else
                    i=i+1
                endif
            enddo
            if (lfound) then
                sfc_data(d)%ccvr(h)=icloud(i)
                cloudsrc(h,d)=2
            endif
        endif
    endif
    
    return    
    end subroutine clouds
!*********************************************************************************************************

    subroutine substitute(d,h,ostemp)
!=========================================================================================================
!   SUBROUTINE SUBSTITUTE
!   THIS SUBROUTINE SUBSTITUTES TEMPERATURE AND CLOUD COVER FOR MISSING HOURS
!
!   MODIFIED MARCH 2, 2022
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER
!
!   INPUT/OUTPUT ARGUMENTS
!   OSTEMP:     SUBSTITUTED TEMPERATURE IS FROM ONSITE TEMPERATURES
!               NEEDED FOR PRESS SUBROUTINE
!
!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!   ilev:       level counter
!   i:          loop counter
!   d1:         day for first hour to use for interpolation
!   d2:         day for second hour to use for interpolation
!   h1:         first hour to use for interpolation
!   h2:         second hour to use for interpolation
!   delta_h1:   number of hours between hour h and first hour of interpolation
!   delta_h2:   number of hours between hour h and second hour of interpolation
!   cld_delta:  difference in cloud cover between first and second hours used for interpolation
!
!   Real variables
!   delta:       difference in temperature between first and second hours used for interpolation
!
!   Logical variables
!   lsub:       logical variable denoting substitution has occurred
!   ostemp:     substituted temperature is from onsite temperatures
!
!   Character variables
!   debugform:  format for debug statements
!   modnam:     Subroutine name
!========================================================================================================= 

    implicit none
    integer(kind=4),intent(in) :: d,h
    integer(kind=4) :: i,d1(3),d2(3),h1(3),h2(3),delta_h1,delta_h2,cld_delta
    real(kind=r8) :: delta
    logical :: lsub
    logical,intent(inout) :: ostemp
    character(len=100) :: debugform(2)
    character(len=10) :: modnam='SUBSTITUTE'
        
!   debug formats
!   1. temperature
    write(debugform(1),'(a)')'(/2(a),t29,i8,1x,a,1x,i2.2,1x,f6.2,1x,a/t29,i8,1x,a,1x,i2.2,1x,f6.2,1x,i8,1x,a,1x,i2.2,1x,f6.2)'

!   2.  cloud cover
    write(debugform(2),'(a)')'(/2(a),t29,i8,1x,a,1x,i2.2,1x,i2,1x,a/t29,i8,1x,a,1x,i2.2,1x,i2,1x,i8,1x,a,1x,i2.2,1x,i2)'
        
!   only perform substitutions if 
!   hour 1 or 2 and not day 1
!   hour 23 or 24 and not the final day
!   other hours of the day for any day
    if (h == 1) then
        if (d > 1) then
!           pairs for interpolation:
!           hour 24 of day before, hour 2 of current day
!           hour 24 of day before, hour 3 of current day
!           hour 23 of day before, hour 2 of current day
            d1=d-1 !all 3 elements are day before
            h1(1)=24
            h1(2)=24
            h1(3)=23
            d2=d  !all 3 elements are same day
            h2(1)=h+1
            h2(2)=h+2
            h2(3)=h+1
        endif
    elseif (h == 2) then
        if (d > 1) then
!           pairs for interpolation:
!           hour 1 of current day, hour 3 of current day
!           hour 1 of current day, hour 4 of current day
!           hour 24 of day before, hour 3 of current day
            d1(1)=d
            d1(2)=d
            d1(3)=d-1
            h1(1)=h-1
            h1(2)=h-1
            h1(3)=24
            d2=d  !all 3 elements are same day
            h2(1)=h+1
            h2(2)=h+2
            h2(3)=h+1
        else !1st day, only check between hour 1 and 4 (1 and 3, or 1 and 4)
!           pairs for interpolation:
!           hour 1 of current day, hour 3 of current day
!           hour 1 of current day, hour 4 of current day
!           hour 1 of current day, hour 4 of current day (duplicate of 2nd pair)
            d1=d
            h1=h-1
            d2=d
            h2(1)=h+1
            h2(2)=h+2
            h2(3)=h+2
        endif
    elseif (h == 23) then
        if (d < npbldays) then
!           pairs for interpolation:
!           hour 22 of current day, hour 24 of current day
!           hour 22 of current day, hour 1 of next day
!           hour 21 of current day, hour 24 of current day
            d1=d !all 3 elements are same day
            h1(1)=h-1
            h1(2)=h-1
            h1(3)=h-2
            d2(1)=d 
            d2(2)=d+1
            d2(3)=d
            !d2(3)=d+1
            h2(1)=h+1
            h2(2)=1
            h2(3)=h+1
        else !last day, only consider hour 21, 22, and 24 (22 and 24, 21 and 24)
!           pairs for interpolation:
!           hour 22 of current day, hour 24 of current day
!           hour 22 of current day, hour 24 of current day (duplicate of first pair)
!           hour 21 of current day, hour 24 of current day
            d1=d !all 3 elements are same day
            h1(1)=h-1
            h1(2)=h-1
            h1(3)=h-2
            d2=d !all 3 elements are same day
            h2=24 !all 3 elements same hour
        endif 
    elseif (h==24) then
!       pairs for interpolation:
!       hour 23 of current day, hour 1 of next day
!       hour 23 of current day, hour 2 of next day
!       hour 22 of current day, hour 1 of next day
        d1=d !all 3 elements are same day
        h1(1)=h-1
        h1(2)=h-1
        h1(3)=h-2
        d2=d+1 !all 3 elements are same day
        h2(1)=1
        h2(2)=2
        h2(3)=1
    else
!       pairs for interpolation:
!       one hour before current hour of current day, one hour after current hour of current day
!       one hour before current hour of current day, two hours after current hour of current day
!       two hours before current hour of current day, one hour after current hour of current day
        d1=d
        h1(1)=h-1
        h1(2)=h-1
        h1(3)=h-2
        d2=d
        h2(1)=h+1
        h2(2)=h+2
        h2(3)=h+1
    endif

!   first check 1-hour on each side
!   then 1-hour before and 2 hours after
!   then 2-hours before and 1 hour after

!   temperature
!   substitution only occurs if the other hours are not missing, previous hour has not been interpolated already,
!   and the measurement heights are the same
    
    if (tempsrc(h,d) == 0 .and. subtemp) then !missing temperature
        lsub=.false.
        i=1
        do while(i <= 3 .and. .not. lsub)
           
            if (tempsrc(h1(i),d1(i)) > 0 .and. tempsrc(h1(i),d1(i)) < 3 .and. tempsrc(h2(i),d2(i)) > 0 .and. &
                dabs(sfc_data(d1(i))%temp_ht(h1(i)) -sfc_data(d2(i))%temp_ht(h2(i))) <= eps ) then
!               both hours not missing and previous hour is not interpolated and measurement heights the same
                delta_h1=h-h1(i)
                delta_h2=h2(i)-h1(i)
!               account for crossing days
                if (delta_h1 < 0) delta_h1=delta_h1+24
                if (delta_h2 < 0) delta_h2=delta_h2+24
!               temperature difference between hours bookending the missing hour
                delta=sfc_data(d2(i))%airtemp(h2(i))-sfc_data(d1(i))%airtemp(h1(i))
                sfc_data(d)%airtemp(h)=sfc_data(d1(i))%airtemp(h1(i))+delta*(real(delta_h1,r8)/real(delta_h2,r8))
                sfc_data(d)%temp_ht(h)=sfc_data(d2(i))%temp_ht(h2(i))

!               set ostemp to true if both temperatures are onsite
                if (tempsrc(h1(i),d1(i))== 1 .and. tempsrc(h2(i),d2(i))==1) ostemp=.true.
                
!               set tempsrc to 3
                tempsrc(h,d)=3
                lsub=.true.
!               post 21DRF, tally number of hours with temperature substitution
                n_temp_sub=n_temp_sub+1
!               write interpolation information if debugging
                if (debug) write(debug_unit,debugform(1))trim(adjustl(modnam)),': TEMPERATURE',sfc_data(d)%sfcdate,'HR',h,&
                    sfc_data(d)%airtemp(h),'FROM',sfc_data(d1(i))%sfcdate,'HR',h1(i),sfc_data(d1(i))%airtemp(h1(i)),&
                    sfc_data(d2(i))%sfcdate,'HR',h2(i),sfc_data(d2(i))%airtemp(h2(i))
            endif
            i=i+1
        enddo
    endif
    
!   cloud cover
!   substitution only occurs if the other hours are not missing and previous hour has not been interpolated already

    if (cloudsrc(h,d) == 0 .and. subcloud) then !missing cloud    
        lsub=.false.
        i=1
        do while(i <= 3 .and. .not. lsub)
            if (cloudsrc(h1(i),d1(i)) > 0 .and. cloudsrc(h1(i),d1(i)) < 3 .and. cloudsrc(h2(i),d2(i)) > 0) then
!               both hours not missing and previous hour not interpolated already
                delta_h1=h-h1(i)
                delta_h2=h2(i)-h1(i)
!               account for crossing days
                if (delta_h1 < 0) delta_h1=delta_h1+24
                if (delta_h2 < 0) delta_h2=delta_h2+24
!               cloud difference between hours bookending the missing hour
                cld_delta=sfc_data(d2(i))%ccvr(h2(i))-sfc_data(d1(i))%ccvr(h1(i))
                sfc_data(d)%ccvr(h)=nint(real(sfc_data(d1(i))%ccvr(h1(i)),r8)+real(cld_delta,r8)*(real(delta_h1,r8)/&
                    real(delta_h2,r8)))
!               set cloudsrc = 3
                cloudsrc(h,d)=3
                lsub=.true.
!               post 21DRF, tally number of hours with cloud cover substitution
                n_cloud_sub=n_cloud_sub+1
!               write interpolation information if debugging
                if (debug) write(debug_unit,debugform(2))trim(adjustl(modnam)),': CLOUD COVER',sfc_data(d)%sfcdate,'HR',h,&
                    sfc_data(d)%ccvr(h),'FROM',sfc_data(d1(i))%sfcdate,'HR',h1(i),sfc_data(d1(i))%ccvr(h1(i)),&
                    sfc_data(d2(i))%sfcdate,'HR',h2(i),sfc_data(d2(i))%ccvr(h2(i))
            endif
            i=i+1
        enddo
    endif
    
!   set subflag

    if (tempsrc(h,d)==3) then
        if (cloudsrc(h,d)==3) then
            sfc_data(d)%subflag(h)='Sub_CC-TT'
        else
            sfc_data(d)%subflag(h)='Sub_TT'
        endif
    else
        if (cloudsrc(h,d)==3) sfc_data(d)%subflag(h)='Sub_CC'
    endif
    
    return
    end subroutine substitute
!*********************************************************************************************************

    subroutine press(d,h,ostemp)
!=========================================================================================================
!   SUBROUTINE PRESS
!   THIS SUBROUTINE CALCULATES STATION PRESSURE FOR THE HOUR
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER   
!   OSTEMP:     SUBSTITUTED TEMPERATURE IS FROM ONSITE TEMPERATURES
!               NEEDED FOR PRESS SUBROUTINE
!
!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!   popt:       pressure assignment/calculation option
!   slpvar:     value of sea level pressure index in osdata1
!   presvar:    value of station pressure index in osdata1
!
!   Real variables
!   nwselev:    NWS station elevation from LOCATION keyword or SURFACE file
!   tv:         virtual temperature used for pressure calculations (290.0 K)
!   
!   Logical variables
!   ospres:     logical variable denoting if onsite station pressure (1) and sea level pressure (2) non-missing
!   sfpres:     logical variable denoting if NWS station pressure (1) and NWS sea level pressure (2) non-missing
!
!   Character variables
!   src:        indicates if ONSITE or PROGnostic
!   pres_opt:	pressure calculation option
!   modnam:     Subroutine name
!========================================================================================================= 

    use main1, only : pressure,lpath
    use surface, only: lsfelev,user_elev,sfelev,sf_user_elev,sfdata,sfvars
    use onsite, only: pres_var,slp_var,loselev,oselev
    implicit none
    
    integer(kind=4),intent(in) :: d,h 
    integer(kind=4) :: popt,slpvar,presvar
    real(kind=r8) :: nwselev
    real(kind=r8), parameter :: tv=290._r8
    logical ospres(2),sfpres(2)
    logical,intent(in) :: ostemp
    character(len=10) :: modnam='PRESS'
    character(len=10) :: src
    character(len=100) :: pres_opt
    
    
    if (lpath(4)) src=pathid(4)
    if (lpath(5)) src=pathid(5)   
    
    slpvar=osvars(slp_var)%readvar
    presvar=osvars(pres_var)%readvar
    
    ospres=.false.
    sfpres=.false.
    havepress=.false.
    popt=0
!   set the values for onsite pressure logical variable ospres.  ospres(1) is station pressure
!   ospres(2) is sea level pressure.  ospres is true if that variable is being read and not missing
    
    if (osvars(pres_var)%lread .and. os_obs(h,d)) then
        if (dabs(osdata1(presvar,1,h,ios(d))-real(osvars(pres_var)%missval,r8)) > eps) ospres(1)=.true. !station pressure
    endif
    if (osvars(slp_var)%lread .and. os_obs(h,d)) then
        if(dabs(osdata1(slpvar,1,h,ios(d))-real(osvars(slp_var)%missval,r8)) > eps .and. slpvar > 0) ospres(2)=.true. ! sea level pressure      
    endif
    
    if (nws_obs(h,d)) then
!       set the values for NWS pressure logical variable sfpres.  sfpres(1) is station pressure
!       sfpres(2) is sea level pressure.  sfpres is true if that variable is not missing
        if (dabs(sfdata(3,h,isf(d))-real(sfvars(3)%missval,r8)) > eps) sfpres(1)=.true.
        if (dabs(sfdata(2,h,isf(d))-real(sfvars(2)%missval,r8)) > eps) sfpres(2)=.true.
!       set nwselev based on if user elevation or elevation from file if needed
        if (lsfelev) then
            nwselev=sfelev
        elseif (user_elev) then
            nwselev=sf_user_elev
        else
           nwselev=-99999._r8
        endif
    else
        nwselev=-99999._r8
    endif
    
    if (tempsrc(h,d)==1 .or. (tempsrc(h,d)==3 .and. ostemp)) then
        if (ospres(1)) then 
!           use station pressure
            popt=1
            
        elseif (ospres(2)) then 
!           use station sea level pressure; use value of 290 for virtual temperature
            popt=2
            
        elseif (sfpres(1) .and. loselev .and. (user_elev .or. lsfelev)) then
!           use NWS station pressure and elevation with onsite elevation; use value of 290 for virtual temperature 
            popt=3
            
        elseif (sfpres(2) .and. loselev) then
!           calculate station pressure from NWS sea level presssure and onsite elevation 
            popt=4
            
        elseif (sfpres(1)) then
!           use NWS station pressure    
            popt=5
            
        elseif (sfpres(2) .and. (user_elev .or. lsfelev)) then
!           use NWS sea level pressure and NWS elevation)
            popt=6
            
        elseif (loselev) then
!           use onsite elevation and determine station pressure from standard atmosphere
            popt=7
            
        elseif (user_elev .or. lsfelev) then
!           use NWS elevation and determine station pressure from standard atmosphere            
            popt=8
            
        elseif (ospres(2) .and. .not. loselev) then 
!           use onsite sea level pressure, assuming no elevation
            popt=9
            
        elseif (sfpres(2) .and. .not. user_elev .and. .not. lsfelev) then
!           assign NWS sea level pressure to onsite station pressure
            popt=10
            
        else
!           Station pressure is used only to calculate density; rather than
!           skip the computations if pressure is missing, assume a pressure
!           of 1013.25 (sea level pressure) if all variables are missing.
!           This condition should seldom be hit since there should always
!           be a calculation for the hourly surface obs.; the exception is
!           if the on-site data contains all the necessary met. data and
!           the hourly obs are not needed. 
            popt=11
             
        endif
    elseif (tempsrc(h,d)==2 .or. (tempsrc(h,d)==3 .and. .not. ostemp)) then
!       have NWS temperature but not onsite temperature or interpolated temperature
!       is a mix of NWS and ONSITE
        
        if (sfpres(1)) then
!           use NWS station pressure
            popt=5
        elseif (sfpres(2) .and. (user_elev .or. lsfelev)) then
!           use NWS sea level pressure and NWS elevation 
            popt=6
        elseif (ospres(1) .and. (user_elev .or. lsfelev) .and. loselev) then
!           adjust onsite station pressure to surface station elevation
            popt=12
            
        elseif (ospres(2) .and. (user_elev .or. lsfelev)) then
!           adjust onsite sea level pressure to surface station elevation
            popt=13
            
        elseif (ospres(1)) then
!           use onsite station pressure
            popt=1
            write(pres_opt,'(a)')'USE ONSITE STATION PRESSURE'
        elseif (ospres(2) .and. loselev) then
!           use onsite sea level pressure and elevation to convert to station pressure
            popt=2
            write(pres_opt,'(5(a))')'ADJUST ONSITE SEA LEVEL PRESSURE TO STATION PRESSURE USING ONSITE ELEVATION'
        elseif ((user_elev .or. lsfelev) .and. nwselev /= 0.0_r8) then
!           based on surface elevation
            popt=8
            write(pres_opt,'(a)')'USE SURFACE ELEVATION AND STANDARD ATMOSPHERE'
        elseif (loselev) then
!           use onsite elevation and determine station pressure from standard atmosphere
            popt=7
        elseif (sfpres(2) .and. .not. lsfelev .and. .not. user_elev) then
!           assign surface sea level pressure to station pressure, assuming elevation =0
            popt=14
        elseif (ospres(2) .and. .not. loselev) then
!           assign onsite sea level pressure to station pressure, assuming elevation=0
            popt=15
        else
!           Station pressure is used only to calculate density; rather than
!           skip the computations if pressure is missing, assume a pressure
!           of 1013.25 (sea level pressure) if all variables are missing.
!           This condition should seldom be hit since there should always
!           be a calculation for the hourly surface obs.; the exception is
!           if the on-site data contains all the necessary met. data and
!           the hourly obs are not needed.    
            popt=11
        endif
    else
!       no onsite temperature and no NWS temperature or temperature is an interpolation between NWS and onsite data with
!       same height
        if (ospres(1)) then 
!           use station pressure
            popt=1
        elseif (sfpres(1)) then
!           use NWS station pressure
            popt=5
        elseif (loselev) then
!           use onsite elevation and determine station pressure from standard atmosphere 
            popt=7
        elseif (user_elev .or. lsfelev) then
!           use NWS elevation and determine station pressure from standard atmosphere             
            popt=8
        else
!           Station pressure is used only to calculate density; rather than
!           skip the computations if pressure is missing, assume a pressure
!           of 1013.25 (sea level pressure) if all variables are missing.
!           This condition should seldom be hit since there should always
!           be a calculation for the hourly surface obs.; the exception is
!           if the on-site data contains all the necessary met. data and
!           the hourly obs are not needed. 
            popt=11
        endif
    endif
    
!   set havepress to true if popt > 0
    if (popt > 0) havepress=.true.
    
    if (popt == 1) then
!       use station pressure
        sfc_data(d)%pres(h)=osdata1(presvar,1,h,ios(d))  
        write(pres_opt,'(3(a))')'USE ',trim(adjustl(src)),' STATION PRESSURE'
    elseif (popt == 2) then
!       use station sea level pressure; use value of 290 for virtual temperature
        sfc_data(d)%pres(h)=pressure(2,oselev,tv,osdata1(slpvar,1,h,ios(d))) 
        write(pres_opt,'(5(a))')'ADJUST ',trim(adjustl(src)),' SEA LEVEL PRESSURE TO STATION PRESSURE USING ',&
            trim(adjustl(src)),' ELEVATION'
    elseif (popt == 3) then
!       use NWS station pressure and elevation with onsite elevation; use value of 290 for virtual temperature 
        sfc_data(d)%pres(h)=pressure(2,oselev-nwselev,tv,sfdata(3,h,isf(d))) 
        write(pres_opt,'(a)')'ADJUST SURFACE STATION PRESSURE TO ONSITE STATION PRESSURE USING ONSITE ELEVATION'
    elseif (popt == 4) then
!       calculate station pressure from NWS sea level pressure and onsite elevation
        sfc_data(d)%pres(h)=pressure(2,oselev,tv,sfdata(2,h,isf(d)))
        write(pres_opt,'(a)')'ADJUST SURFACE SEA LEVEL PRESSURE TO ONSITE STATION PRESSURE USING ONSITE ELEVATION'
    elseif (popt == 5) then
!       use NWS station pressure
        sfc_data(d)%pres(h)=sfdata(3,h,isf(d))
        write(pres_opt,'(a)')'USE NWS STATION PRESSURE'
    elseif (popt == 6) then
!       use NWS sea level pressure and NWS elevation
        sfc_data(d)%pres(h)=pressure(2,nwselev,tv,sfdata(2,h,isf(d)))  
        write(pres_opt,'(a)')'ADJUST SURFACE SEA LEVEL PRESSURE TO SURFACE STATION PRESSURE USING SURFACE ELEVATION'
    elseif (popt == 7) then
!       use onsite elevation and determine station pressure from standard atmosphere
        sfc_data(d)%pres(h)=1013.25_r8*(1.0_r8 -((0.0065_r8/288.15_r8)*oselev))**5.255_r8
        write(pres_opt,'(3(a))')'USE ',trim(adjustl(src)),' ELEVATION AND STANDARD ATMOSPHERE'
    elseif (popt == 8) then
!       use NWS elevation and determine station pressure from standard atmosphere            
        sfc_data(d)%pres(h)=1013.25_r8*(1.0_r8 -((0.0065_r8/288.15_r8)*nwselev))**5.255_r8
        write(pres_opt,'(a)')'USE SURFACE ELEVATION AND STANDARD ATMOSPHERE'
    elseif (popt == 9) then
!       use onsite sea level pressure, assuming no elevation
        sfc_data(d)%pres(h)=osdata1(slpvar,1,h,ios(d))
        write(pres_opt,'(3(a))')'USE ',trim(adjustl(src)),' SEA LEVEL PRESSURE'
    elseif (popt == 10) then
!       assign NWS sea level pressure to onsite station pressure
        sfc_data(d)%pres(h)=sfdata(2,h,isf(d)) 
        write(pres_opt,'(a)')'USE SURFACE SEA LEVEL PRESSURE'
    elseif (popt == 11) then
!       Station pressure is used only to calculate density; rather than
!       skip the computations if pressure is missing, assume a pressure
!       of 1013.25 (sea level pressure) if all variables are missing.
!       This condition should seldom be hit since there should always
!       be a calculation for the hourly surface obs.; the exception is
!       if the on-site data contains all the necessary met. data and
!       the hourly obs are not needed. 
        sfc_data(d)%pres(h)=1013.25_r8
        write(pres_opt,'(a)')'VARIABLES MISSING; ASSUME 1013.25 MB'
    elseif (popt == 12) then
!       adjust onsite station pressure to surface station elevation
        sfc_data(d)%pres(h)=pressure(2,nwselev-oselev,tv,osdata1(presvar,1,h,ios(d)))
        write(pres_opt,'(a)')'ADJUST ONSITE STATION PRESSURE USING SURFACE STATION ELEVATION'
    elseif (popt == 13) then
!       adjust onsite sea level pressure to surface station elevation
        sfc_data(d)%pres(h)=pressure(2,nwselev,tv,osdata1(slpvar,1,h,ios(d)))   
        write(pres_opt,'(a)')'ADJUST ONSITE SEA LEVEL PRESSURE USING SURFACE STATION ELEVATION'
    elseif (popt == 14) then
!       assign surface sea level pressure to station pressure, assuming elevation =0
        sfc_data(d)%pres(h)=sfdata(2,h,isf(d))
        write(pres_opt,'(a)')'USE SURFACE SEA LEVEL PRESSURE'
    elseif (popt == 15) then
!       assign onsite sea level pressure to station pressure, assuming elevation=0
        sfc_data(d)%pres(h)=osdata1(slpvar,1,h,ios(d))  
        write(pres_opt,'(3(a))')'USE ',trim(adjustl(src)),' SEA LEVEL PRESSURE'
    endif

    if (popt > 0 .and. debug)write(debug_unit,'(t10,1x,2(a),1x,2(a),1x,f6.1)')trim(adjustl(modnam)),':',trim(adjustl(pres_opt)),&
    ':',sfc_data(d)%pres(h)
    return
    end subroutine press
!*********************************************************************************************************

    subroutine rh(d,h)
!=========================================================================================================
!   SUBROUTINE RH
!   THIS SUBROUTINE CALCULATES RELATIVE HUMIDITY FOR THE HOUR
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!   ilev:       level index
!   irh:       integer RH calculated from humidity function
!
!   Real variables
!   dewpt:      dewpoint temperature, either ONSITE or NWS
!   diffd:      difference between dewpoint and missing dewpoint indicator
!   difft:      difference between air temperature and missing air temperature indicator
!   diffht:     difference between onsite height and temperature measurement height
!   
!   Logical variables
!   lfound:     logical variable denoting onsite RH found
!   lfound1:    logical variable denoting onsite dewpoint found corresponding to same level as air temperature
!
!   Character variables
!   modnam:     Subroutine name
!========================================================================================================= 

    use main1, only : lpath,humidity
    use surface, only: sfdata,sfvars
    use onsite, only: nlevel,rh_var,dewpt_var

    implicit none
    
    integer(kind=4),intent(in) :: d,h 
    integer(kind=4) :: ilev,irh
    real(kind=r8) :: dewpt,diffd,difft,diffht
    logical lfound,lfound1
    character(len=10) :: modnam='RH'
    
!   hiearchy:
!   1. onsite rh
!   2. onsite temperature and dewpoint (if both available at same level)
!   3. NWS rh
!   4. rh from NWS temperature and dewpoint (even if there is onsite temperature)
    
!   if none of the above conditions are satisfied, RH is already set to missing
    
    lfound=.false.
    lfound1=.false.
    
    !if (sfc_data(d)%os_obs(h)) then
    if (os_obs(h,d)) then
!       have onsite obs for the hour
        if (osvars(rh_var)%lread) then
!           find first level of non-missing RH
            ilev=1
            do while (ilev <= nlevel .and. .not. lfound)
                if (dabs(osdata1(osvars(rh_var)%readvar,ilev,h,ios(d))-real(osvars(rh_var)%missval,r8)) > eps) then
!                   valid RH, assign to rh variable
                    lfound=.true.
                    sfc_data(d)%rh(h)=osdata1(osvars(rh_var)%readvar,ilev,h,ios(d))
                endif
                ilev=ilev+1
            enddo
        endif
        
!       if no valid RH found or not read in then look for onsite dewpoint and temperature but want to be same level
        if (.not. lfound .or. .not. osvars(rh_var)%lread) then
            if (osvars(dewpt_var)%lread .and. tempsrc(h,d)==1) then
!               find the level corresponding to the air temperature 
                ilev=1
                do while (ilev <= nlevel .and. .not. lfound1)
                    diffht=dabs(sfc_data(d)%temp_ht(h)-osdata1(1,ilev,h,ios(d)))
                    diffd=dabs(osdata1(osvars(dewpt_var)%readvar,ilev,h,ios(d))-real(osvars(dewpt_var)%missval,r8))
                    if (diffht < eps .and. diffd > eps) then 
!                       level matches temperature level and dewpoint is not missing
                        dewpt=osdata1(osvars(dewpt_var)%readvar,ilev,h,ios(d))
                        lfound1=.true.
                    endif
                    ilev=ilev+1
                enddo
                if (lfound1) then
!                   calculate integer humidity and then convert to real
!                   convert air temperature back to Celcius
                    irh=humidity(sfc_data(d)%airtemp(h)-273.15_r8,dewpt)
                    sfc_data(d)%rh(h)=real(irh,r8)
                endif
            endif
        endif
    endif
    
!   no valid onsite obs, use NWS
    
    if (.not. lfound .and. .not. lfound1 .and. lpath(3)) then
!       either don't have dewpoint or temperature, use surface RH if not missing
        if (nws_obs(h,d) .and. dabs(sfdata(9,h,isf(d))-real(sfvars(9)%missval,r8)) > eps) then
!           assign RH
            sfc_data(d)%rh(h)=sfdata(9,h,isf(d))
        else
!           use temperature and dewpoint from surface
            diffd=dabs(sfdata(8,h,isf(d))-real(sfvars(8)%missval,r8))
            difft=dabs(sfdata(7,h,isf(d))-real(sfvars(7)%missval,r8))
!           temperature has already been assigned to the surface data, use that value
            if (diffd > eps .and. difft > eps) then
!               calculate integer humidity and then convert to real
                irh=humidity(sfdata(7,h,isf(d)),sfdata(8,h,isf(d)))
                sfc_data(d)%rh(h)=real(irh,r8)
            endif
        endif
    endif
    
    return
    end subroutine rh
!*********************************************************************************************************

    subroutine precip(d,h)
!=========================================================================================================
!   SUBROUTINE PRECIP
!   THIS SUBROUTINE ASSIGNS PRECIP FOR THE HOUR
!
!   MODIFIED MAY 16, 2022
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!   i:          counter to check if PAMT or PRCP is available
!   
!   Logical variables
!   lfound:     logical variable denoting onsite precip found
!
!   Character variables
!   modnam:     Subroutine name
!========================================================================================================= 

    use surface, only: sfdata
    use onsite, only: prcp_vars
    implicit none
    
    integer(kind=4),intent(in) :: d,h
    integer(kind=4) :: i
    logical lfound
    character(len=10) :: modnam='PRECIP'

!   start with onsite PAMT and then PRCP
    i=1
    lfound=.false.
    
    do while (i <= 2 .and. .not. lfound)
        if (osvars(prcp_vars(i))%lread .and. os_obs(h,d)) then
            if (dabs(osdata1(osvars(prcp_vars(i))%readvar,1,h,ios(d))-real(osvars(prcp_vars(i))%missval,r8)) > eps) then
                lfound=.true.
                if (i==1) then
!                   convert from cm to mm
                    sfc_data(d)%pamt(h)=osdata1(osvars(prcp_vars(i))%readvar,1,h,ios(d))*10.0_r8
                else
                    sfc_data(d)%pamt(h)=osdata1(osvars(prcp_vars(i))%readvar,1,h,ios(d))
                endif
            endif
            
        endif
        i=i+1         
    enddo
    
!   if no onsite precip

    if (.not. lfound .and. nws_obs(h,d)) then
        sfc_data(d)%pamt(h)=sfdata(1,h,isf(d))
        sfc_data(d)%ipcode(h)=sfdata(5,h,isf(d))
        if (sfc_data(d)%ipcode(h) == 1 .or. sfc_data(d)%ipcode(h) == 3) then
            sfc_data(d)%ipcode(h)=11
        elseif (sfc_data(d)%ipcode(h) == 2) then
            sfc_data(d)%ipcode(h)=22
        elseif (sfc_data(d)%ipcode(h) == 9) then
            sfc_data(d)%ipcode(h)=99
        endif
    endif
!   assign code if precip
 
    if (sfc_data(d)%pamt(h) > 0.0_r8) then
        
        if (tempsrc(h,d) > 0) then
            if (sfc_data(d)%airtemp(h) < 273.15_r8) then
                sfc_data(d)%ipcode(h)=22
            else
                sfc_data(d)%ipcode(h)=11
            endif
        else
!           change from 9999 to 99
            if (sfc_data(d)%ipcode(h) > 100_r8) sfc_data(d)%ipcode(h)=99
        endif
    else
        if (sfc_data(d)%ipcode(h) > 100_r8) sfc_data(d)%ipcode(h)=99
    endif
    if (dabs(sfc_data(d)%pamt(h)-0.0_r8) < eps) sfc_data(d)%ipcode(h)=0
    return
    end subroutine precip
!*********************************************************************************************************

    subroutine stability(d,h)
!=========================================================================================================
!   SUBROUTINE STABILITY
!   THIS SUBROUTINE CALCULATES CRITICAL SOLAR ELEVATION ANGLE FOR THE HOUR BASED ON HOURLY ELEVATION ANGLE 
!   AND ASSIGNS STABILITY (CONVECTIVE OR STABLE)
!
!   MODIFIED MARCH 9, 2022
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!   d1:         day counter for solar elevation angle for hour before current hour
!   h1:         hour counter for solar elevation angle for hour before current hour
!   h2:         loop counter for loop hh1
!   nt:         counter for number of hours with valid temperature
!   
!   Real variables
!   qrnot:      clear sky insolation (page 13 of AERMOD MFED; see Ro term just below equation 4)
!   skyfrac:    sky fraction used to calculate cloud cover based on equation 4. solve for n in equation 4
!   tt:         running total of temperature for the day
!   b1:         reset of albedo based on critical angle
!   a1:         constant 990 used to calculate qrnot (page 13 of AERMOD MFED; see Ro term just below equation 4)
!   a2:         constant 30 used to calculate qrnot (page 13 of AERMOD MFED; see Ro term just below equation 4)
!
!   Character variables
!   debugform:      formats for debug messages
!   date_hr_str:	date/hour text string
!   formstr:	    format string for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: d2r,msg_form
    use file_units, only:  msg_unit
    use onsite, only: inso_var,l_var,overland
    implicit none
    
    integer(kind=4),intent(in) :: d,h
    integer(kind=4) :: d1,h1,nt,h2
    real(kind=r8) :: qrnot,skyfrac,tt,b1
    real(kind=r8) :: a1=990.0_r8
    real(kind=r8) :: a2=30.0_r8
    character(len=100) :: debugform(7)
    character(len=20) :: date_hr_str
    character(len=60) :: formstr
    character(len=10) :: modnam='STABILITY'
    
!   format string for alerting user to method of stability determination
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a20,1x,a)'
    
!   debug formats
!   1.  solar angle
    write(debugform(1),'(a)')'(2(/t10,a,1x,i2.2,1x,a,1x,i8,a,1x,f7.3)/t10,a,1x,f7.3)'

!   2.  QRNOT calculation
    write(debugform(2),'(a)')'(/t12,a/t10,f5.1,t18,f4.1,t25,f7.3,t34,g13.6)'	

!   3.  SKYFRAC = 0
    write(debugform(3),'(a)')'(/t10,a,1x,g13.6,1x,a,1x,g13.6/t10,a)'

!   4.  SKYFRAC based on eq. 4
    write(debugform(4),'(a)')'(/t10,a,1x,g13.6,1x,a,1x,g13.6/t10,a/t10,a,g13.6)'

!   5.  QRNOT > 0 or INSO < 0, set SKYFRAC to 1
    write(debugform(5),'(a)')'(/t10,a,1x,g13.6,1x,a,1x,g13.6,1x,a/t10,a)'
    
!   6.  angle, critical angle and stability
    write(debugform(6),'(a)')'(/t10,a/t9,f7.3,2x,a1,2x,f7.3,5x,a/)'
    
!   7.  Monin-Obukhov length and stability
    write(debugform(7),'(a)')'(/t15,a/t9,f8.1,1x,a,t24,a/)'

    if (debug)write(debug_unit,'(/t10,a10)')trim(adjustl(modnam))
    
    nt=0
    
    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sfc_data(d)%sfcdate,'HR',h
     
!   if using L for the stability, alert user when it is not being used
    if (.not. onmol .and. .not. overland) write(msg_unit,formstr)adjustl(pathid(ipath)),'I84',modnam,date_hr_str,&
        'STABILITY BASED ON CRITICAL SOLAR ANGLE'
    
!   Compute the average of the solar elevation angles at the
!   beginning and end of the hour.
    
    if (h == 1) then
        if (d == 1) then
            d1=d
        else
            d1=d-1
        endif
        h1=24
    else
        d1=d
        h1=h-1
    endif
    
!   calculate angle
    angle=(sun_ang(h1,d1)+sun_ang(h,d))/2.0_r8
    if (debug)write(debug_unit,debugform(1))'SOLAR ANGLE HOUR',h1,'DATE',sfc_data(d1)%sfcdate,':',sun_ang(h1,d1),&
        'SOLAR ANGLE HOUR',h,'DATE',sfc_data(d)%sfcdate,':',sun_ang(h,d),'ANGLE:',angle
    
!   if have insolation, calculate an equivalent cloud cover that will replace any cloud cover
    if (oninso) then
        if (osdata1(osvars(inso_var)%readvar,1,h,ios(d)) > 0.0_r8 .and. angle > 0.0_r8) then
!           calculate equivalent cloud cover
            cloudsrc(h,d)=4 !set cloudsrc to 4 (equivalent cover)
!           calculate clear sky insolation (page 13 of AERMOD MFED; see Ro term just below equation 4)
            qrnot=a1*dsin(angle*d2r)-a2
            if (debug)write(debug_unit,debugform(2))'A1		A2     ANGLE       QRNOT',a1,a2,angle,qrnot
!           account for qnto <=0
            if (qrnot <= 0.0_r8) then
!               set skyfrac to 0
                skyfrac=0.0_r8
                if (debug)write(debug_unit,'(/t10,a)')'QRNOT <= 0, SET SKYFRAC = 0'
            elseif (osdata1(osvars(inso_var)%readvar,1,h,ios(d)) > 0.0_r8 .and. osdata1(osvars(inso_var)%readvar,1,h,ios(d)) >= &
            qrnot) then
                skyfrac=0.0_r8
                if (debug)write(debug_unit,debugform(3))'INSO',osdata1(osvars(inso_var)%readvar,1,h,ios(d)),'> 0 AND >= QRNOT',&
                    qrnot,'SET SKYFRAC = 0'
            elseif (osdata1(osvars(inso_var)%readvar,1,h,ios(d)) > 0.0_r8) then
!               solve for n in equation 4 of AERMOD MFED (page 13) to calculate sky cover
                skyfrac=((1.0_r8-osdata1(osvars(inso_var)%readvar,1,h,ios(d))/qrnot)/0.75_r8)**(1.0_r8/3.4_r8)
                if (debug)write(debug_unit,debugform(4))'INSO',osdata1(osvars(inso_var)%readvar,1,h,ios(d)),'> 0 AND < QRNOT',&
                    qrnot,'SET SKYFRAC BASED ON EQUATION 4 AERMOD MFED','SKYFRAC=',skyfrac
            else
                skyfrac=1.0_r8
                if (debug)write(debug_unit,debugform(5))'QRNOT',qrnot,'> 0 OR INSO',osdata1(osvars(inso_var)%readvar,1,h,ios(d)),&
                    ' < 0','SET SKYFRAC = 1.0'
            endif
            if (skyfrac <= 0.0_r8) then
                sfc_data(d)%ccvr(h)=0
                if (debug)write(debug_unit,'(/t10,a,g13.6,a)')'SKYFRAC = ',skyfrac,' <= 0, SET CLOUD COVER TO 0'
            elseif (skyfrac >= 1.0_r8) then
                sfc_data(d)%ccvr(h)=10
                if (debug)write(debug_unit,'(/t10,a,g13.6,a)')'SKYFRAC = ',skyfrac,' >= 1, SET CLOUD COVER TO 10'
            else
                sfc_data(d)%ccvr(h)=nint(skyfrac*10._r8)
                if (debug)write(debug_unit,'(/t10,a,g13.6,a,i2)')'SKYFRAC = ',skyfrac,' CLOUD COVER = SKYFRAC*10 ',&
                    sfc_data(d)%ccvr(h)
            endif
            if (tempsrc(h,d) > 0 .and. sfc_data(d)%airtemp(h) < 900._r8 ) then
                crit_angle=nr_ang(d,h,sfc_data(d)%ccvr(h),sfc_data(d)%airtemp(h))
!               Check for angle < crit_angle, which indicates that EQ_CCVR value
!               may not be appropriate since it's derived from QR data but the
!               hour will be designated as SBL. If BULKRN/DELT option is also 
!               being used, this will allow use EQ_CCVR based on BULKRN option   
                if (angle < crit_angle) got_eq_ccvr(h,d)=.true.
            else
                crit_angle=94.0_r8
            endif
        elseif (tempsrc(h,d) > 0 .and. sfc_data(d)%airtemp(h) < 900._r8 .and. cloudsrc(h,d) > 0 .and. sfc_data(d)%ccvr(h) < 99)&
        then
            crit_angle=nr_ang(d,h,sfc_data(d)%ccvr(h),sfc_data(d)%airtemp(h))
        elseif (cloudsrc(h,d) > 0 .and. sfc_data(d)%ccvr(h) < 99) then
!           cloud cover but no temperature; use average temperature for previous 24 hours with
!           at least 75% valid data
            tt=0
    hh1:    do h2=1,24
                if (tempsrc(h2,d) > 0 .and. sfc_data(d)%airtemp(h2) < 900._r8) then
                    tt=tt+sfc_data(d)%airtemp(h2)
                    nt=nt+1
                endif
            enddo hh1
            if (nt >= 18) then
                tt=tt/real(nt,r8)
            else
                tt=288.0_r8
            endif
            crit_angle=nr_ang(d,h,sfc_data(d)%ccvr(h),tt)
        elseif (tempsrc(h,d) > 0 .and. sfc_data(d)%airtemp(h) < 900._r8 ) then
!           temperature available but no cloud cover; use 5/10
            crit_angle=nr_ang(d,h,5,sfc_data(d)%airtemp(h))
        else
!           both cloud and temperature missing
            crit_angle=nr_ang(d,h,5,288._r8)
        endif 
    elseif (tempsrc(h,d) > 0 .and. sfc_data(d)%airtemp(h) < 900._r8 .and. cloudsrc(h,d) > 0 .and. sfc_data(d)%ccvr(h) < 99) then
        crit_angle=nr_ang(d,h,sfc_data(d)%ccvr(h),sfc_data(d)%airtemp(h))
    elseif (cloudsrc(h,d) > 0 .and. sfc_data(d)%ccvr(h) < 99) then
   
!       cloud cover but no temperature; use average temperature for previous 24 hours with
!       at least 75% valid data
        tt=0
    hh2:    do h2=1,24
!           only include non-missing temperatures
            if (tempsrc(h2,d) > 0 .and. sfc_data(d)%airtemp(h2) < 900._r8) then
                tt=tt+sfc_data(d)%airtemp(h2)
                nt=nt+1
            endif
        enddo hh2
        if (nt >= 18) then
            tt=tt/real(nt,r8)
        else
            tt=288.0_r8
        endif
        crit_angle=nr_ang(d,h,sfc_data(d)%ccvr(h),tt)
    elseif (tempsrc(h,d) > 0 .and. sfc_data(d)%airtemp(h) < 900._r8) then
!       temperature available but no cloud cover; use 5/10
        crit_angle=nr_ang(d,h,5,sfc_data(d)%airtemp(h))
    else
!       both cloud and temperature missing
        crit_angle=nr_ang(d,h,5,288._r8)
    endif
!   reset albedo based on crit_angle
    b1=1-sfc_data(d)%albedo(h)
    if (angle <= 0.0_r8) then
        sfc_data(d)%albedo(h)=1.0_r8
    else
        sfc_data(d)%albedo(h)=sfc_data(d)%albedo(h)+b1*dexp(-0.1_r8*angle+(-0.5_r8*b1*b1))
    endif
    
!   if Monin-Obukhov length read in and used (over water applications only) use L
!   to determine stability; Otherwise use standard solar angle approach
!   calculations in code above still done for albedo
    if (onmol) then
!       have L and if < 0, set cbl to true; cbl is initialized to false so don't need
!       to set to false for the hour
        if (osdata1(osvars(l_var)%readvar,1,h,ios(d)) < 0.0_r8) cbl(h,d)=.true.
    else	
!		use normal solar angle approach
        if (angle >= crit_angle)cbl(h,d)=.true. !convective hour
    endif
    
    if (debug) then
        if (angle <= 0.0_r8) then
            write(debug_unit,'(/t10,a)')'ANGLE < 0, ALBEDO SET TO 1.0'
        else
            write(debug_unit,'(/t10,a,f4.2)')'ALBEDO SET TO ',sfc_data(d)%albedo(h)
        endif
        if (cbl(h,d)) then
            if (onmol) then
                write(debug_unit,debugform(7))'L        STABILITY',osdata1(osvars(l_var)%readvar,1,h,ios(d)),'< 0',&
                    'CONVECTIVE'
            else
                write(debug_unit,debugform(6))'ANGLE     CRIT ANGLE   STABILITY',angle,'>=',crit_angle,'CONVECTIVE'
            endif
        else
            if (onmol) then
                write(debug_unit,debugform(7))'L        STABILITY',osdata1(osvars(l_var)%readvar,1,h,ios(d)),'> 0','STABLE'
            else
                write(debug_unit,debugform(6))'ANGLE     CRIT ANGLE   STABILITY',angle,'<',crit_angle,'STABLE'
            endif
        endif
    endif
    
    return
    end subroutine stability
!*********************************************************************************************************

!    subroutine calc_ustar(d,h)
    subroutine calc_ustar(d,h,wind_reset,wspeed,ustar,mol,thetastar1) !post 21DRF
!=========================================================================================================
!   SUBROUTINE CALC_USTAR
!   THIS SUBROUTINE CALCULATES USTAR AND MONIN-OBUKOV LENGTH FOR STABLE OR CONVECTIVE CONDITIONS
!
!   MODIFIED JANUARY 20, 2022
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER
!   WSPEED: INPUT WIND SPEED; POST 21DRF
!   WIND_RESET: WIND SPEED HAS BEEN RESET TO 0.28 M/S; POST 21DRF
!
!   OUTPUT ARGUMENTS
!   USTAR:      SURFACE FRICTION VELOCITY
!   MOL:        MONIN-OBUKHOV LENGTH
!   THETASTAR1: THETA-STAR
!
!   Variable definitions
!      
!   Integer variables
!   d:      day counter
!   h:      hour counter
!   iter:   iterations of do while loop, used for debugging only
!
!   Real variables
!   psizl:      similiarity function for calculations based on measurement height (zref)
!   psizol:     similiarity function for calculations based on surface roughness length (z0)
!   mu:         (1-16zref/L)**0.25
!   mu0:        (1-16z0/L)**0.25
!   lastl       previous value of L
!   ustar:      surface friction velocity
!   mol:        monin-obukhov length
!   betam:      constant (4.7 for stable adjust u*, 5.0 for stable non-adjust u*)
!   cdn:        drag coefficient
!   unot:       value used to calculate u*
!   thetastar1: theta-star
!   chek:       value used to check for real or imaginary solution for u*
!   ucrit:      critical value of u
!   ustarcrit:  critical value of u*
!
!   Logical variables
!   lcalc:      logical variable denoting to calculate u* and L
!   wind_reset: wind speed has been reset to 0.28 m/s; post 21DRF
!
!   Character variables
!   modnam:     Subroutine name
!========================================================================================================= 
    use main1, only : g
    use onsite, only: l_var
    implicit none
    integer(kind=4),intent(in) :: d,h
    integer(kind=4) :: iter
    real(kind=r8), intent(in) :: wspeed
    real(kind=r8), intent(out) :: ustar,mol,thetastar1
    !real(kind=r8) :: psizl,psizol,lastl,mu,mu0,ustar,mol,betam,cdn,thetastar1,unot,chek,ucrit,ustarcrit
    real(kind=r8) :: psizl,psizol,lastl,mu,mu0,betam,cdn,unot,chek,ucrit,ustarcrit
    logical :: lcalc
    logical, intent(in) :: wind_reset
    character(len=10) :: modnam='CALC_USTAR'

    if (debug)write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' START'

    if (cbl(h,d)) then
 !      first check to see if Monin-Obukhov length is available, if so calculate u* from L
        if (onmol .and. (.not. wind_reset .or. .not. onustar)) then
            lcalc=.false.
        else
!           u* and L will be calculated
            lcalc=.true.
        endif   
        if (lcalc) then
!           first guess for psizl and psizol
            psizl=0.0_r8
            psizol=0.0_r8
            lastl=0.0_r8
            if (wspeed==0.0_r8) then
!               set u* to 0 if wind speed is 0
                ustar=0.0_r8
            else
!               Set up iteration loop over Monin-Obukhov length (MOL) and
!               friction velocity (USTAR).  The first guess for the
!               iteration is with PSIZL and PSIZOL set equal to zero.
!               The next guess is made by reevaluating PSIZL and PSIZOL.
                ustar=vonk*wspeed/(dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))-psizl+psizol)
                mol=-rho(h,d)*cp*sfc_data(d)%airtemp(h)*ustar**3/(vonk*g*sfc_data(d)%hflux(h))
                
                if (debug)write(debug_unit,'(/t10,a,2(1x,g13.6)/t10,a)')'INITIAL VALUE OF U* AND L:',ustar,mol,&
                    'ITERATION        MU          MU0         PSIZL         PSIZOL          U*            L         RHO'
                iter=0
                do while(dabs(mol-lastl) > dabs(eps*mol))
                    lastl=mol
!                   calculate mu, mu0, psizl, and psizol
                    mu=(1.0_r8 - 16.0_r8*(sfc_data(d)%wind_ht(h)/mol))**0.25_r8
                    mu0=(1.0_r8 - 16.0_r8*(sfc_data(d)%zo(h)/mol))**0.25_r8
                    psizl=2.0_r8*dlog((1.0_r8+mu)/2.0_r8)+dlog((1.0_r8+mu*mu)/2.0_r8)-2.0_r8*datan(mu)+pi/2.0_r8 
                    psizol=2.0_r8*dlog((1.0_r8+mu0)/2.0_r8)+dlog((1.0_r8+mu0*mu0)/2.0_r8)-2.0_r8*datan(mu0)+pi/2.0_r8 
!                   recompute u* and MOL
                    ustar=vonk*wspeed/(dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))-psizl+psizol)
                    mol=-rho(h,d)*cp*sfc_data(d)%airtemp(h)*ustar**3/(vonk*g*sfc_data(d)%hflux(h))
                    iter=iter+1
                    if (debug)write(debug_unit,'(t10,i9,7(1x,g13.6))')iter,mu,mu0,psizl,psizol,ustar,mol,rho(h,d)
                enddo
!               set values of ustar and mol
!               post 21DRF do not set sfc_data value
                !sfc_data(d)%ustar(h)=ustar
                !sfc_data(d)%mol(h)=mol
            endif
        else
!           calculate u* from L
            if (debug)write(debug_unit,'(/t10,a)')'CALCULATE U* FROM L'
!			do not set to sfc_data u*, just ustar; post 21DRF
            !sfc_data(d)%ustar(h)=((-osdata1(osvars(l_var)%readvar,1,h,ios(d))*vonk*g*sfc_data(d)%hflux(h))/&
            !    (rho(h,d)*cp*sfc_data(d)%airtemp(h)))**(1.0_r8/3.0_r8)
            ustar=((-osdata1(osvars(l_var)%readvar,1,h,ios(d))*vonk*g*sfc_data(d)%hflux(h))/&
                (rho(h,d)*cp*sfc_data(d)%airtemp(h)))**(1.0_r8/3.0_r8)
            mol=osdata1(osvars(l_var)%readvar,1,h,ios(d))
        endif
        
    else
!       stable
!       set betam based on adjust u*
        if (adjustar) then
            betam=4.7_r8
            cdn=vonk/dlog((sfc_data(d)%wind_ht(h)-5.0_r8*sfc_data(d)%zo(h))/sfc_data(d)%zo(h))
            thetastar1=0.08_r8
            
            if (debug)write(debug_unit,'(/t10,a,g13.6)')'INITIAL THETASTAR: ',thetastar1
            if (debug) write(debug_unit,'(/t15,2(a))')'BETAM        CDN         THETASTAR       CHEK           U*          ',&
            'UCRIT        USTARCRIT     UNOT'
!           adjust thetastar1 for low solar angles
            if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2)
            unot=dsqrt((betam*(sfc_data(d)%wind_ht(h)-5._r8*sfc_data(d)%zo(h)-sfc_data(d)%zo(h))*g*thetastar1)/&
                sfc_data(d)%airtemp(h))
            chek=((2.0_r8*unot)/(dsqrt(cdn)*wspeed))**2
            ustar=(cdn*wspeed/2.0_r8)*((1.0_r8+dexp(-1.0_r8*chek/2.0_r8))/(1.0_r8-dexp(-2.0_r8/dsqrt(chek))))
            ucrit=(2.0_r8*unot)/dsqrt(cdn)
            ustarcrit=cdn*ucrit/4.0_r8
            ustar=max(ustarcrit,ustar)
            if (chek > 1.0_r8) then
!               post 21DRF, set thetastar1
                !sfc_data(d)%theta_star(h)=thetastar1*sfc_data(d)%wspd(h)/ucrit
                thetastar1=thetastar1*wspeed/ucrit
!			else
!                sfc_data(d)%theta_star(h)=thetastar1
            endif
            if (debug)write(debug_unit,'(t10,g13.6,7(1x,g13.6))')betam,cdn,sfc_data(d)%theta_star(h),chek,ustar,ucrit,ustarcrit,&
                unot
        else
            betam=5.0_r8
            cdn=vonk/dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))
            thetastar1=0.09_r8*(1.0_r8-0.5_r8*((real(sfc_data(d)%ccvr(h),r8)/10.0_r8)**2))
            if (debug)write(debug_unit,'(/t10,a,g13.6)')'INITIAL THETA-STAR: ',thetastar1
            if (debug) write(debug_unit,'(/t15,2(a))')'BETAM        CDN         THETASTAR       CHEK           U*          ',&
            'UCRIT        USTARCRIT'
!           adjust thetastar1 for low solar angles
            if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2)
            unot=dsqrt((betam*sfc_data(d)%wind_ht(h)*g*thetastar1)/sfc_data(d)%airtemp(h))
            chek=((2.0_r8*unot)/(dsqrt(cdn)*wspeed))**2
            if (chek <= 1.0_r8) then !real
                ustar=(cdn*wspeed/2.0_r8)*(1.0_r8+dsqrt(1.0_r8-chek))
                !sfc_data(d)%theta_star(h)=thetastar1
                if (debug)write(debug_unit,'(t10,g13.6,5(1x,g13.6))')betam,cdn,thetastar1,chek,ustar
            else!imaginary  
                ucrit=(2.0_r8*unot)/dsqrt(cdn)
                ustarcrit=cdn*ucrit/2.0_r8
                ustar=ustarcrit*wspeed/ucrit
                thetastar1=thetastar1*wspeed/ucrit
                if (debug)write(debug_unit,'(t10,g13.6,7(1x,g13.6))')betam,cdn,thetastar1,chek,&
                    ustar,ucrit,ustarcrit
            endif
            
        endif   
    endif
 
    if (debug)write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' END'
    
    return
    end subroutine calc_ustar
!*********************************************************************************************************

    real(kind=r8) function nr_ang(d,h,ccvrin,tempin)
!=========================================================================================================
!   FUNCTION NR_ANG
!   CALCULATE CRITICAL SOLAR ANGLE
!     
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS
!   D:              DAY COUNTER
!   H:              HOUR COUNTER
!   CCVRIN:         INPUT CLOUD COVER
!   TEMPIN:         INPUT TEMPERATURE
!
!   OUTPUT
!     
!   NR_ANG:       CRITICAL SOLAR ANGLE
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   h:              hour counter
!   ccvrin:         input cloud cover
!   i:              iteration loop counter
!
!   Real variables
!   sky:            real value of ccvrin
!   albedo:         albedo
!   tempin:         input temperature in K
!   acrit1:         critical angle for iteration i
!   lastacrit:      i-1 acrit1 value
!   ba1:            1-albedo
!   term1:          term for calculations
!   term2:          term for calculations
!   sinacrit:       sin of acrit
!   sb:             constant (5.67e-08)
!   c1:             constant (5.31e-13)
!   c2:             constant (60.0)
!   b1:             constant (0.75)
!   b2:             constant (3.4)
!   a1:             constant (990.0)
!   a2:             constant (30.0)
!
!   Character variables
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: d2r
    implicit none
    integer(kind=4), intent(in) :: ccvrin,d,h
    integer(kind=4) i
    real(kind=r8), intent(in) :: tempin
    real(kind=r8):: sky,lastacrit,albedo,ba1,acrit1,term1,term2,sinacrit
    real(kind=r8) :: sb=5.67e-08_r8
    real(kind=r8) :: c1=5.31e-13_r8
    real(kind=r8) :: c2=60.0_r8
    real(kind=r8) :: b1=0.75_r8
    real(kind=r8) :: b2=3.4_r8
    real(kind=r8) :: a1=990._r8
    real(kind=r8) :: a2=30.0_r8
    
    character(len=10) :: modnam='NR_ANG' 
    
    i=0
    
!   adjust the albedo based on solar langle
    ba1=1.0_r8-sfc_data(d)%albedo(h)
    if (angle <= 0.0_r8) then
!       set nighttime albedo to 1.0
        albedo=1.0_r8
    else
        albedo=sfc_data(d)%albedo(h)+ba1*dexp(-0.1*angle+(-0.5_r8*ba1*ba1))
    endif
    
    sky=real(ccvrin,r8)
    
    if (albedo < 1.0_r8 .and. albedo >= 0.0_r8) then
!       initialize acrit1 and lastacrit
        acrit1=angle
        lastacrit=0.0_r8
!       iterate to calculate acrit1 with limit of 20 interations
        do while (dabs(acrit1-lastacrit) > dabs(eps*acrit1) .and. i <=20)
            i=i+1
            lastacrit=acrit1
            term1=sb*tempin**4.0_r8-c1*tempin**6.0_r8 - c2*sky/10.0_r8
            term2=(1.0_r8-b1*(sky/10.0_r8)**b2)*(1.0-albedo)*a1
            sinacrit=term1/term2 + a2/a1
            if (sinacrit <= 0.0_r8) then
                acrit1=0.0_r8
                exit
            elseif (sinacrit > 1.0_r8) then
                acrit1=92.0_r8
                exit
            else
                acrit1=dasin(sinacrit)/d2r !radians to degrees
            endif
!           adjust albedo to current value of acrit1
            if (acrit1 <= 0.0_r8) then
                acrit1=0.0_r8
                exit
            else
                albedo=sfc_data(d)%albedo(h)+ba1*exp(-0.1*acrit1+(-0.5_r8*ba1*ba1))
            endif
        enddo
    elseif (albedo >= 1.0_r8) then
        acrit1=94.0_r8
    endif

    nr_ang=acrit1
    
    return
    end function nr_ang
!*********************************************************************************************************

    subroutine netrad(d,h,lcycle)
!=========================================================================================================
!   FUNCTION NETRAD
!   CALCULATE NET RADIATION
!     
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS
!   D:              DAY COUNTER
!   H:              HOUR COUNTER
!
!   OUTPUT ARGUMENTS
!   LCYCLE:         LOGICAL INDICATOR TO CYCLE THE HOUR LOOP BECAUSE NRAD WASN'T CALCULATED
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   h:              hour counter
!
!   Real variables
!   sky:        cloud cover
!   c1:         constant 5.31e-13
!   c2:         constant 60
!   c3:         1.12
!   sb:         Stefan-Boltzmann constant (5.57e-08)
!
!   logical variables
!   lcycle:     logical indicator to cycle the hour loop because nrad wasn't calculated
!
!   Character variables
!   formstr:        format for messages
!   debugform:      debug message formats
!   date_hr_str:	date/hr text string
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: d2r,msg_form
    use file_units, only: msg_unit
    use onsite, only: nrad_var
    
    implicit none
    integer(kind=4), intent(in) :: d,h
    
    real(kind=r8) :: sb=5.67e-08_r8
    real(kind=r8) :: c1=5.31e-13_r8
    real(kind=r8) :: c2=60.0_r8
    real(kind=r8) :: c3=1.12_r8
    real(kind=r8) :: sky=0.0_r8

    logical, intent(out) :: lcycle
    character(len=20) :: date_hr_str
    character(len=60) :: formstr(2)
    character(len=100) :: debugform(2)
    character(len=10) :: modnam='NETRAD'
    
!   formats for messages
!   1. missing insolation and cloud cover
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a20,1x,a)'
    
!   2.  radiation > 0 for stable hour
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a20,1x,a,1x,g13.6,1x,a)'

!   debug formats
!   1.  net radiation calculations
    write(debugform(1),'(a)')'(/t16,a,/t13,e9.2,1x,f5.2,1x,f4.2,t33,e9.2,t47,f4.2,t55,f7.2,t65,f4.1,t71,g13.6,1x,g13.6)'
    
!   2.  net radiation calculated from heat flux and bowen ratio
    write(debugform(2),'(a)')'(2(/t10,a)/t10,f4.2,t17,g13.6,1x,g13.6)'
    
    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sfc_data(d)%sfcdate,'HR',h
    
    if (debug)write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' START'
    
!   initialize lcycle to false 
    lcycle=.false.
    sky=real(sfc_data(d)%ccvr(h),r8)
    

    if (.not. onnrad) then
        if (cbl(h,d)) then !convective
            if (oninso .and. inso > 0.0_r8 .and. cloudsrc(h,d) > 0) then
!               calculate net radiation if cloud cover and insolation
                nrad=((1.0_r8-sfc_data(d)%albedo(h))*inso+c1*(sfc_data(d)%airtemp(h)**6)-sb*(sfc_data(d)%airtemp(h)**4) + &
                c2*(sky/10.0_r8))/c3
!               write debug information
                if (debug) write(debug_unit,debugform(1))&
                    'C1      C2   C3      SB       ALBEDO   AIR TEMP  CCVR      INSO         NRAD',c1,c2,c3,sb,&
                    sfc_data(d)%albedo(h),sfc_data(d)%airtemp(h),sky,inso,nrad
            elseif (cloudsrc(h,d) > 0) then
!               calculate insolation
!               insolation is 0 if angle < 1.74 (sin(30/990)
                if (angle <= 1.74_r8) then
                    !inso(h,d)=0.0_r8
                    inso=0.0_r8
                else
                    inso=990._r8*dsin(angle*d2r)-30._r8
                    inso=inso*(1._r8-0.75_r8*((sky/10.0_r8)**3.4_r8))
                endif
!               calculate net radiation from cloud cover and calculated insolation
                nrad=((1.0_r8-sfc_data(d)%albedo(h))*inso+c1*(sfc_data(d)%airtemp(h)**6)-sb*(sfc_data(d)%airtemp(h)**4) + &    
                c2*(sky/10.0_r8))/c3
            
!               write debug information
                if (debug) write(debug_unit,debugform(1))&
                    'C1      C2   C3      SB       ALBEDO   AIR TEMP  CCVR      INSO         NRAD',c1,c2,c3,sb,&
                    sfc_data(d)%albedo(h),sfc_data(d)%airtemp(h),sky,inso,nrad
            else
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I71',modnam,date_hr_str,'MISSING INSOLATION AND CLOUD COVER'
                if (debug)write(debug_unit,'(/t10,a)')'NET RADIATION NOT CALCULATED, MISSING INSOLATION AND CLOUD COVER'
                lcycle=.true. !hour loop in pbl_proc will be cycled
               
            endif
        else !stable
!           back calculate net radiation
            if (dabs(sfc_data(d)%hflux(h)-999.0_r8) > eps) then
                nrad=sfc_data(d)%hflux(h)*(1.0_r8+1.0_r8/sfc_data(d)%bowen(h))/0.9_r8
                if (debug)write(debug_unit,debugform(2))'NET RADIATION CALCULATED FROM HEAT FLUX AND BOWEN RATIO',&
                    'BOWEN     HEAT FLUX      NRAD',sfc_data(d)%bowen(h),sfc_data(d)%hflux(h),nrad
            endif 
        endif
        
    endif
    
!   for stable hour, check to see if positive
    if (.not. cbl(h,d)) then
        if (dabs(nrad-real(osvars(nrad_var)%missval,r8)) > eps .and. nrad > 0.0_r8) &
        write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I76',modnam,date_hr_str,'NET RADIATION',nrad,'> 0 FOR STABLE HOUR'
    endif
    
    if (debug)write(debug_unit,'(/t10,2(a)/)')trim(adjustl(modnam)),' END'

    return
    end subroutine netrad
!*********************************************************************************************************

    subroutine sfc_chars(isite,d,wd,albedo,bowen,zo)
!=========================================================================================================
!   SUBROUTINE SFC_CHARS
!   THIS SUBROUTINE ASSIGNS SURFACE CHARACTERISITCS FOR THE HOUR FOR PRIMARY OR SECONDARY SITE
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (WINDS)
!
!   INPUT ARGUMENTS 
!
!   ISITE:	INTEGER INDICATOR OF SITE TYPE FOR SURFACE CHARACTERISTICS
!			PROCESSING AND MESSAGE (1=PRIMARY, 2=SECONDARY)
!   D:      DAY COUNTER
!   WD:     WIND DIRECTION
!
!
!   OUTPUT ARGUMENTS
!
!   ALBEDO:     ALBEDO FOR CURRENT HOUR AND WIND DIRECTION
!   BOWEN:      BOWEN RATIO FOR CURRENT HOUR AND WIND DIRECTION
!   ZO:         SURFACE ROUGHNESS FOR CURRENT HOUR AND WIND DIRECTION
!
!   Variable definitions
!      
!   Integer variables
!   isite:      integer indicator of site type for surface characteristics
!               processing and message (1=primary, 2=secondary)
!   d:          day counter
!   ifreq:      frequency counter
!   isec:       sector counter
!   iyr:        4-digit year of date being processed
!   iyr1:       number of years since start year
!   imon:       month of date being processed
!   nfreq:      frequency of surface characteristics. set to nfreq1 or nfreq2 depending onsite
!   month2seas: month to season default assignments. 2x12 array where first dimension is the
!               hemisphere (1=north,2=south) and second dimension is month  
!
!   Real variables
!   wd:         wind direction
!   albedo:     albedo for current hour and wind direction
!   bowen:      bowen ratio for current hour and wind direction
!   zo:         surface roughness for current hour and wind direction
!
!   Logical variables
!   lfound:     logical variable indicating if day found
!
!   Character variables
!   modnam:     Subroutine name
!   adate:      8-character string of date
!========================================================================================================= 
    use main1, only: eps
    implicit none
    integer(kind=4),intent(in) :: d,isite
    integer(kind=4) :: ifreq,isec,iyr,iyr1,imon,nfreq,month2seas(2,12)
    real(kind=r8), intent(in) :: wd
    real(kind=r8),intent(out) :: albedo,bowen,zo
    logical lfound
    character(len=10) :: modnam='SFC_CHARS'
    character(len=8) :: adate
    
!   assign default seasons to months
!   northern hemisphere
!   1=winter (January, February, December)
!   2=spring (March, April, May)
!   3=summer (June, July, August)
!   4=autumn (September, October, November)
    data (month2seas(1,imon),imon=1,12)/1,1,2,2,2,3,3,3,4,4,4,1/
    
!   southern hemisphere
!   1=winter (June, July, August)
!   2=spring (September, October, November)
!   3=summer (January, February, December)
!   4=autumn (June, July, August)
    data (month2seas(2,imon),imon=1,12)/3,3,4,4,4,1,1,1,2,2,2,3/
    
!   get year and month of date being processed
    write(adate,'(i8)')sfc_data(d)%sfcdate
    read(adate,'(i4,i2)')iyr,imon
    
!   get # of years since pbl start year
    iyr1=iyr-pbldates(1,1)+1
    

!   isite set in winds subroutine
!   assign a value to nfreq
    if (isite ==1) then
        nfreq=nfreq1(iyr1)
    else
        nfreq=nfreq2(iyr1)
    endif
    
!   determine the current frequency counter for the month
!   if monthly characteristics, then ifreq is the month
!   if annual, ifreq is 1 and if seasonal, assign the season
!   based on the month
    if (nfreq == 1) then
        ifreq=1
    elseif (nfreq == 12) then
        ifreq=imon
    else
        ifreq=month2seas(ihem(isite),imon)
    endif
    
    albedo=0.0_r8
    bowen=0.0_r8
    zo=0.0_r8
!   based on wind direction and time of year, select appropriate surface characteristics
!   beginning sector is included, ending sector is excluded
    if (isite == 1) then
!       get average if missing wind direction
        if (wd > 900._r8) then
    l1:     do isec=1,nsectors1(iyr1)
                albedo=albedo+sfc_char1(isec,ifreq,iyr1)%albedo
                bowen=bowen+sfc_char1(isec,ifreq,iyr1)%bowen
                zo=zo+sfc_char1(isec,ifreq,iyr1)%zo
            enddo l1
            albedo=albedo/real(nsectors1(iyr1),r8)
            bowen=bowen/real(nsectors1(iyr1),r8)
            zo=zo/real(nsectors1(iyr1),r8)
        else
            isec=1
            lfound=.false.
            do while (isec <= nsectors1(iyr1) .and. .not. lfound)
                if (sectors1(1,isec,iyr1) < sectors1(2,isec,iyr1)) then !beginning of sector is less than end of sector
!                   account for fact that wind direction could be 360 and ending direction is 360
                    if (wd >= sectors1(1,isec,iyr1) .and. ((wd < sectors1(2,isec,iyr1) .and. sectors1(2,isec,iyr1) /= 360._r8)&
                        .or. (wd <= sectors1(2,isec,iyr1) .and. dabs(sectors1(2,isec,iyr1)- 360._r8) <= eps))) then
                        lfound=.true.
                    else
                        isec=isec+1
                    endif
                else !crossover 360, i.e. the sector is 350 to 20 degrees
                    if ((wd >= sectors1(1,isec,iyr1) .and. wd > sectors1(2,isec,iyr1)) .or. (wd <= sectors1(1,isec,iyr1) .and. &
                        wd < sectors1(2,isec,iyr1))) then
                        lfound=.true.
                    else
                        isec=isec+1
                    endif
                endif
            enddo 
            albedo=sfc_char1(isec,ifreq,iyr1)%albedo
            bowen=sfc_char1(isec,ifreq,iyr1)%bowen
            zo=sfc_char1(isec,ifreq,iyr1)%zo
        endif
    else
!       get average if missing wind direction
        if (wd > 900.0_r8) then
    l2:     do isec=1,nsectors2(iyr1)
                albedo=albedo+sfc_char2(isec,ifreq,iyr1)%albedo
                bowen=bowen+sfc_char2(isec,ifreq,iyr1)%bowen
                zo=zo+sfc_char2(isec,ifreq,iyr1)%zo
            enddo l2
            albedo=albedo/real(nsectors2(iyr1),r8)
            bowen=bowen/real(nsectors2(iyr1),r8)
            zo=zo/real(nsectors2(iyr1),r8)
        else
            
            isec=1
            lfound=.false.
            do while (isec <= nsectors2(iyr1) .and. .not. lfound)
                if (sectors2(1,isec,iyr1) < sectors2(2,isec,iyr1)) then !beginning of sector is less than end of sector
!                   account for fact that wind direction could be 360 and ending direction is 360
                    if (wd >= sectors2(1,isec,iyr1) .and. ((wd < sectors2(2,isec,iyr1) .and. sectors2(2,isec,iyr1) /= 360._r8) &
                        .or. (wd <= sectors2(2,isec,iyr1) .and. dabs(sectors2(2,isec,iyr1) -360._r8) <= eps))) then
                        lfound=.true.
                    else
                        isec=isec+1
                    endif
                else !crossover 360, i.e. the sector is 350 to 20 degrees
                    if ((wd >= sectors2(1,isec,iyr1) .and. wd > sectors2(2,isec,iyr1)) .or. (wd <= sectors2(1,isec,iyr1) .and. &
                        wd < sectors2(2,isec,iyr1))) then
                        lfound=.true.
                    else
                        isec=isec+1
                    endif
                endif
            enddo 
            albedo=sfc_char2(isec,ifreq,iyr1)%albedo
            bowen=sfc_char2(isec,ifreq,iyr1)%bowen
            zo=sfc_char2(isec,ifreq,iyr1)%zo
        endif
    endif
    
    return
    end subroutine sfc_chars
!*********************************************************************************************************

    subroutine write_srss(d)
!=========================================================================================================
!   SUBROUTINE WRITE_SRSS
!   THIS SUBROUTINE WRITES THE LOCAL AND UPPERAIR SUNRISE AND SUNSET TIMES IN HH:MM:SS
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:      day counter
!   i:      time loop counter (1=local, 2=upper air)
!   hms:    2x3 array of hour,minute,second for sunrise or sunset
!           1st dim is (1=hour, 2=minute, 3=second)
!           2nd dim is sunrise or sunset (1=sunrise, 2=sunset)
!
!   Real variables
!   times:      2-element sunrise or sunset (1=sunrise, 2=sunset)
!   wd:         wind direction
!   albedo:     albedo for current hour and wind direction
!   bowen:      bowen ratio for current hour and wind direction
!   zo:         surface roughness for current hour and wind direction
!
!   Logical variables
!   lfound:     logical variable indicating if day found
!
!   Character variables
!   locstr:     location string (local or upper air)
!   modnam:     Subroutine name
!========================================================================================================= 

    implicit none
    integer(kind=4),intent(in) :: d
    integer(kind=4) :: i,j,hms(3,2)
    real(kind=r8) :: times(2)
    character(len=10) :: modnam='WRITE_SRSS'
    character(len=9) :: locstr(2)
    
    data locstr /'LOCAL','UPPER AIR'/
    
    times=0
    hms=0
!   put the local and upperair times into the times array
    l1: do i=1,2
            if (i == 1) then
                times(1)=local_sunrise(d)
                times(2)=local_sunset(d)
            else
                times(1)=up_sunrise(d)
                times(2)=up_sunset(d)
            endif
    l2:     do j=1,2
                hms(1,j)=int(times(j)) !hour
                hms(2,j)=int((times(j)-real(hms(1,j),r8))*60.0_r8) !minute
                hms(3,j)=int((((times(j)-real(hms(1,j),r8))*60.0_r8)-real(hms(2,j),r8))*60.0_r8) !second
            enddo l2
            write(debug_unit,'(t5,2(a),2(1x,i2.2,2(a1,i2.2)))')trim(adjustl(locstr(i))),' SUNRISE/SUNSET: ',hms(1,1),':',&
                hms(2,1),':',hms(3,1),hms(1,2),':',hms(2,2),':',hms(3,2)
    enddo l1

    return
    end subroutine write_srss
!*********************************************************************************************************

    subroutine have_data(d,h,invar1,outvar)
!=========================================================================================================
!   SUBROUTINE HAVE_DATA
!   THIS SUBROUTINE ASSIGNS A VALUE TO A LOGICAL VARIABLE DENOTING IF REQUESTED VARIABLE IS VALID FOR 
!   THE HOUR
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER
!   INVAR1: INDEX OF VARIABLE IN OSVARS ARRAY
!
!   OUTPUT ARGUMENTS
!   
!   OUTVAR: LOGICAL VALUE FOR VARIABLE
!
!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!   invar1:     index of variable in osvars array
!   invar2:     index of variable in osdata1
!
!   Logical variables
!   outvar:     logical value for variable
!
!   Character variables
!   modnam:     Subroutine name
!========================================================================================================= 

    implicit none
    
    integer(kind=4),intent(in) :: d,h,invar1
    integer(kind=4) :: invar2
    logical,intent(out) :: outvar
    character(len=10) :: modnam='HAVE_DATA'
    
!   assign value to invar2
    invar2=osvars(invar1)%readvar
    
    if (os_obs(h,d) .and. osvars(invar1)%lread) then
!       have an onsite/prog observation and the variable is being read in
        if (dabs(osdata1(invar2,1,h,ios(d))-real(osvars(invar1)%missval,r8)) > eps) then
!           variable's value is not missing
            outvar=.true.
        else
!           variable's value is missing
            outvar=.false.
        endif
    else
!       either no valid onsite/prog observation or variable is not read in
        outvar=.false.
    endif

    return
    end subroutine have_data
!*********************************************************************************************************

    subroutine bulk_rich(d,h)
!=========================================================================================================
!   SUBROUTINE PRESS
!   THIS SUBROUTINE CALCULATES U*, L, and THETA* BASED ON BULK-RICHARDSON APPROACH
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER

!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!   rpt:        replicate counter
!   iter:       iteration counter
!   imin:       minimum number of iterations
!   k:          counter
!   counter:    counter
!   j:          counter
!
!   Real variables
!   betam:      constant (5)
!   alpha1:     constant (4)
!   beta1:      constant (0.5)
!   gamma1:     constant (0.3)
!   lastu:      previous value of u* in iteration loop
!   lastmol:    previous value of L in iteration loop
!   lastth:     previous value of theta-star in iteration loop
!   lst:        array of L values (max of 20 values)
!   ust:        array of u* values (max of 20 values)
!   thst:       array of theta-star (max of 20 values)
!   z_over_l:   array of z/L values (max of 20 values) where z is wind speed measurement height
!   z_on_l:     z/L where z is wind speed measurement height
!   z0_on_l:    z0/L where z0 is surface roughness
!   ustarmax:   maximum value of u*
!   dtheta:     potential temperature difference
!   dtdz:       temperature lapse rate
!   ptg:        potential temperature lapse rate
!   ustar1:     u*
!   mol:        Monin-Obukhov length
!   a:          term for initial u* calculation
!   cdn:        drag coefficient
!   thetastar1: theta-star
!   templ:      temporary value of Monin-Obukhov length
!   tempu:      temporary value of u*
!   tempth:     temporary value of theta-star
!   utemp:      sum of u* values
!   ltemp:      sum of L values
!   thtemp:     sum of theta-star values
!   term:       term to calculate u* based on Equation 22 page 449 of Luhar and Rayner (2009)
!   term0:      term to calculate u* based on Equation 22 page 449 of Luhar and Rayner (2009)
!
!   Logical variables
!   test:      logical variable used in calculating u* for adjust u*
!   write1:     logical variable denoting which debug outputs to write
!
!   Character variables
!   debugform:  formats for debug messages
!   str1:       text string for debug messages
!   str2:       text string for debug messages
!   modnam:     Subroutine name
!========================================================================================================= 
    use main1, only : g
    use onsite, only: osdt_hts,dt01_var
    implicit none
    
    integer(kind=4),intent(in) :: d,h 
    integer(kind=4) :: rpt,iter,imin,k,counter,j
    real(kind=r8) :: betam=5.0_r8
    real(kind=r8) :: alpha1=4.0_r8
    real(kind=r8) :: beta1=0.5_r8
    real(kind=r8) :: gamma1=0.3_r8
    real(kind=r8) :: lastu,lastmol,lastth
    real(kind=r8) :: lst(20),ust(20),thst(20),z_over_l(20)
    real(kind=r8) :: z_on_l,z0_on_l,ustarmax,dtheta,dtdz,ptg,ustar1,mol,a,cdn,thetastar1,templ,tempu,tempth,&
        utemp,ltemp,thtemp,term,term0
    
    logical :: test,write1
    character(len=100) :: debugform(11)
    character(len=110) :: str1,str2
    character(len=10) :: modnam='BULK_RICH'
 
!   debug formats
!   1.  initial information
    write(debugform(1),'(a)')'(/t10,a,2(1x,f5.2)//t14,a/t10,g13.6,4(1x,g13.6),//t10,a/t26,g13.6,2(1x,g13.6))'

!   2.  initial values of u*, theta-star, and L
    write(debugform(2),'(a)')'(t10,a,/t26,g13.6,2(1x,g13.6))'

!   3.  constants
    write(debugform(3),'(a)')'(t10,a/t10,f4.1,t20,f3.1,t28,f3.1,t37,f3.1)'

!   4. iterations and values for u*, thetastar, L, z/L, z0/L term, term0
    write(debugform(4),'(a)')'(t10,i2,t21,g13.6,6(1x,g13.6))'

!   5. iterations and values for u*, thetastar, L, z/L
    write(debugform(5),'(a)')'(t10,i2,t21,g13.6,3(1x,g13.6))'

!   6.  iterations and values for u*, thetastar, L
    write(debugform(6),'(a)')'(t10,i2,t21,g13.6,2(1x,g13.6))'

!   7.  final values for u*, theta-star and L
    write(debugform(7),'(a)')'(/t10,a/t24,g13.6,2(1x,g13.6)/)'

!   8. iterations=20
    write(debugform(8),'(a)')'(t10,a,g13.6,1x,a/t10,a/t3,g13.6,7(1x,g13.6))'
        
!   9.  updated u*, theta star, and L
    write(debugform(9),'(a)')'(t10,a,/t10,a/t5,g13.6,2(1x,g13.6))'

!   10. iterations=20; check for ping-pong pattern
    write(debugform(10),'(a)')'(t9,i2,t13,2(1x,g13.6),t44,l1,t49,i2)'

!   11. counter for ping-pong pattern
    write(debugform(11),'(a)')'(t10,i4,t20,i3,t29,g13.6,3(1x,g13.6))'

    rpt=0
    z_on_l=0.0_r8
    lastu=0.0_r8
    lastmol=0.0_r8
    lastth=0.0_r8
    iter=0
    imin=3
    ustarmax=-99.0_r8
    test=.true.
    !ustar=0.0_r8
    mol=0.0_r8
    thetastar1=0.0_r8
    lst=0.0_r8
    ust=0.0_r8
    thst=0.0_r8
    z_over_l=0.0_r8
    write1=.false.
    
    if (debug)write(debug_unit,'(t10,a,1x,a/)')trim(adjustl(modnam)),'START'
    
!   temperature gradient
    dtdz=osdata1(osvars(dt01_var)%readvar,1,h,ios(d))/(osdt_hts(1,2)-osdt_hts(1,1))
!   potential temperature gradient
    ptg=dtdz+0.0098_r8
!   potential temperature difference
    dtheta=osdata1(osvars(dt01_var)%readvar,1,h,ios(d))+(osdt_hts(1,2)-osdt_hts(1,1))*0.0098_r8
    
!   if dtheta is <=0 use neutral limit for thetastar1 to drive mol to limit
!   also exit subroutine
    if (dtheta <= 0.0_r8) then
        dtheta=0.0_r8
        sfc_data(d)%ustar(h)=vonk*sfc_data(d)%wspd(h)/(dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h)))
        sfc_data(d)%mol(h)=8888.
        sfc_data(d)%theta_star(h)=0.00001_r8
        if (debug)then
            write(debug_unit,'(t10,a)')'DTHETA <= 0, USE NEUTRAL LIMIT NO FURTHER PROCESSING'
            str1='INITIAL VALUES        U*        THETA-STAR        L'
            write(debug_unit,debugform(2))trim(adjustl(str1)),sfc_data(d)%ustar(h),sfc_data(d)%theta_star(h),sfc_data(d)%mol(h)
            write(debug_unit,'(t10,a,1x,a/)')trim(adjustl(modnam)),'END'
        endif
        return
    endif
    
    a=betam*osdt_hts(1,2)*g/sfc_data(d)%airtemp(h)

!   calculate drag coefficient
    if (adjustar) then
        cdn=vonk/dlog((sfc_data(d)%wind_ht(h)-betam*sfc_data(d)%zo(h))/sfc_data(d)%zo(h))
    else
!       defined in Section 3.3.1, page 17 of AERMOD MFED  
        cdn=vonk/dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))
    endif
    
!   initial value of Monin-Obukhov length
    mol=sfc_data(d)%wind_ht(h)/2.0_r8
    
!   equation 18, Section 3.3.1, page 18 of AERMOD MFED
    thetastar1=vonk*dtheta/(dlog(osdt_hts(1,2)/osdt_hts(1,1))+betam*(osdt_hts(1,2)-osdt_hts(1,1))/mol)
    
!   adjust thetastar1 for low solar angles
    if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2)
   
!   calculate ustar1
!   ustar1 is 3rd term of equation 14, Section 3.3.1, page 17 of AERMOD MFED
    ustar1=dsqrt(cdn*a*thetastar1)
    
!   revise initial guess of Monin-Obukhov length
!   equation 11, Section 3.3.1, page 16 of AERMOD MFED
    mol=ustar1*ustar1*sfc_data(d)%airtemp(h)/(vonk*g*thetastar1)

!   save initial guesses
    templ=mol
    tempu=ustar1
    tempth=thetastar1
    
!   if debugging write initial values of variables
    if (debug) then
        
!       dtdz,theta gradient,theta difference
        str1='DT/DZ       DTHETA/DZ     DTHETA        CDN             TERM A'
        str2='INITIAL VALUES        U*        THETA-STAR        L'
        write(debug_unit,debugform(1))'DELTA-T HTS',osdt_hts(1,1),osdt_hts(1,2),trim(adjustl(str1)),dtdz,ptg,dtheta,cdn,a,&
            trim(adjustl(str2)),ustar1,thetastar1,mol
    endif
    
!   begin iterations
!   iterate until the difference between L and previous iteration of L, or u* and previous iteration of u*, or
!   difference between theta* and previous iteration of theta* is < 0.01 of the current value, or at least one iteration,
!   and L is >= 1 and less than 20 interations.
    
!   write constants
    if (debug) then
        if (adjustar) then
            write(debug_unit,debugform(3))'ALPHA    BETA    GAMMA    BETAM',alpha1,beta1,gamma1,betam
        else
            write(debug_unit,'(t10,a,1x,f4.1/)')'BETAM',betam
        endif
    endif
    
    do while ((dabs(lastmol-mol) > dabs(eps*mol) .or. dabs(lastu-ustar1) > dabs(eps*ustar1) .or. dabs(lastth-thetastar1) > &
        dabs(eps*thetastar1) .or. imin >= 1) .and. mol >= 1.0_r8 .and. iter < 20) !max of 20 iterations
        imin=max(0,imin-1)
        iter=iter+1
        lastmol=mol
        lastu=ustar1
        lastth=thetastar1
        
        if (debug .and. iter == 1) then
            write(debug_unit,'(/t10,a)')'START ITERATIONS'
            if(debug .and. iter == 1)then
                str1='ITERATION        U*         THETA-STAR       L            z/L          z0/L           term         term0'
                write(debug_unit,'(t10,a)')str1
            endif
        endif
        
        if (adjustar) then
!           employ Luhar-Rayner method
            z_on_l=sfc_data(d)%wind_ht(h)/mol
            if (sfc_data(d)%wind_ht(h) > 0.7_r8*mol) then
!               use low wind equation for z/L > 0.7L
!               ustar1 is based on Equation 22 page 449 of Luhar and Rayner (2009)
                write1=.true.
                z0_on_l=sfc_data(d)%zo(h)/mol
                term=alpha1*z_on_l**beta1*(1.0_r8+gamma1*z_on_l**(1.0_r8-beta1))
                term0=alpha1*z0_on_l**beta1*(1.0_r8+gamma1*z0_on_l**(1.0_r8-beta1))
                ustar1=vonk*sfc_data(d)%wspd(h)/(term-term0)
            else
                write1=.false.
!               use original method
!               equation 12, Section 3.3.1, page 17 of AERMOD MFED
                ustar1=vonk*sfc_data(d)%wspd(h)/(dlog((sfc_data(d)%wind_ht(h)-betam*sfc_data(d)%zo(h))/&
                    sfc_data(d)%zo(h))+betam*z_on_l)
            endif
            
!           thetastar1
!           equation 18, Section 3.3.1, page 18 of AERMOD MFED
            thetastar1=vonk*dtheta/(dlog(osdt_hts(1,2)/osdt_hts(1,1))+betam*(osdt_hts(1,2)-osdt_hts(1,1))/mol)
            
!           adjust thetastar1 for low solar angles
            if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2)
            
            mol=ustar1*ustar1*sfc_data(d)%airtemp(h)/(vonk*g*thetastar1)
            
            if (debug) then
                if (write1) then
                    write(debug_unit,debugform(4))iter,ustar1,thetastar1,mol,z_on_l,z0_on_l,term,term0
                else
                    write(debug_unit,debugform(5))iter,ustar1,thetastar1,mol,z_on_l
                endif
            endif
      
!           save to arrays
            lst(iter)=mol
            ust(iter)=ustar1
            thst(iter)=thetastar1
            z_over_l(iter)=z_on_l
            
        else
!           no adjust u*   
            if (sfc_data(d)%wind_ht(h) > 0.5_r8*mol) then
!               equation 19, Section 3.3.1, page 18 of AERMOD MFED
                ustar1=vonk*sfc_data(d)%wspd(h)/(dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h)) + &
                    7.0_r8*dlog(sfc_data(d)%wind_ht(h)/mol) + 4.25_r8/(sfc_data(d)%wind_ht(h)/mol) - &
                    0.5_r8/((sfc_data(d)%wind_ht(h)/mol)*(sfc_data(d)%wind_ht(h)/mol))+(betam/2.0_r8)-1.648_r8)  
            else
!               equation 12, Section 3.3.1, page 17 of AERMOD MFED
                ustar1=vonk*sfc_data(d)%wspd(h)/(dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))+betam*sfc_data(d)%wind_ht(h)/mol)
            endif
            
!           thetastar1
!           equation 18, Section 3.3.1, page 18 of AERMOD MFED
            thetastar1=vonk*dtheta/(dlog(osdt_hts(1,2)/osdt_hts(1,1))+betam*(osdt_hts(1,2)-osdt_hts(1,1))/mol)
!           adjust thetastar1 for low solar angles
            if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2)
            
!           new guess for L
!           equation 11, Section 3.3.1, page 16 of AERMOD MFED
            mol=ustar1*ustar1*sfc_data(d)%airtemp(h)/(vonk*g*thetastar1)
            
!           write debug information
            if (debug) write(debug_unit,debugform(6))iter,ustar1,thetastar1,mol
            
        endif
    enddo
    if (debug) write(debug_unit,'(t10,a,i2,a)')'END AFTER ',iter,' ITERATIONS'
    
!   thetastar1
!   equation 18, Section 3.3.1, page 18 of AERMOD MFED
    thetastar1=vonk*dtheta/(dlog(osdt_hts(1,2)/osdt_hts(1,1))+betam*(osdt_hts(1,2)-osdt_hts(1,1))/mol)
    if (debug)write(debug_unit,'(t15,a,g13.6)')'THETASTAR BASED ON EQ. 18: ',thetastar1
    
!   adjust thetastar1 for low solar angles
    if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2)  
    if (debug)write(debug_unit,'(t15,a,g13.6)')'THETASTAR AFTER ADJUSTMENT FOR LOW SOLAR ANGLE: ',thetastar1
    
    if (adjustar)then
        if (iter == 20) then !non-converged
            if (z_over_l(20) > 99.0_r8) then !implies diverging values
!               for the diverging values, use non-iterative method
!               ustar1 is based on Equation 22 page 449 of Luhar and Rayner (2009)
                mol=templ
                ustar1=tempu
                thetastar1=tempth
                
                z_on_l=sfc_data(d)%wind_ht(h)/mol
                z0_on_l=sfc_data(d)%zo(h)/mol
                term=alpha1*z_on_l**beta1*(1.0_r8+gamma1*z_on_l**(1.0_r8-beta1))
                term0=alpha1*z0_on_l**beta1*(1.0_r8+gamma1*z0_on_l**(1.0_r8-beta1))
                ustar1=vonk*sfc_data(d)%wspd(h)/(term-term0)
                
!               thetastar1
!               equation 18, Section 3.3.1, page 18 of AERMOD MFED
                thetastar1=vonk*dtheta/(dlog(osdt_hts(1,2)/osdt_hts(1,1))+betam*(osdt_hts(1,2)-osdt_hts(1,1))/mol)
                
!               first calcuate max value of u* based on eq. 25 of Luhar and Rayner (2009), page 452
                ustarmax = 0.6_r8*(sfc_data(d)%wind_ht(h)/(dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))))**0.33333_r8
                
                if (debug)then
                    str1='ITERATIONS = 20, NON-DIVERGING VALUES, Z/L'
                    str2='U*       THETA-STAR       L            z/L          z0/L           term         term0         ustarmax'
                    write(debug_unit,debugform(8))trim(adjustl(str1)),z_over_l(20),'> 99',trim(adjustl(str2)),ustar1,&
                        thetastar1,mol,z_on_l,z0_on_l,term,term0,ustarmax
                endif
!               reset thetastar1
!               apply upper limit on thetastar1 based on eq. 23 of Luhar and Raynor (2009), page 451
                thetastar1 = min(thetastar1,0.1_r8*(1.0_r8-(min((2._r8*ustar1)/ustarmax,1.0_r8)-1.0_r8)**2))
                
!               reset ustar1 to ustarmax if ustarmax <= ustar1
                if (ustarmax <= ustar1) ustar1=ustarmax
                
!               adjust thetastar1 for low solar angles
                if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2)  
                
!               calculate L
                mol=ustar1*ustar1*sfc_data(d)%airtemp(h)/(vonk*g*thetastar1)
                
                z_on_l=sfc_data(d)%wind_ht(h)/mol
                
                if (debug)then
                    str1='UPDATED U*, THETA-STAR, AND L'
                    str2='U*       THETA-STAR       L'
                    write(debug_unit,debugform(9))trim(adjustl(str1)),trim(adjustl(str2)),ustar1,thetastar1,mol
                endif
            else
                k=0
!               check for ping-pong pattern
                do while (test)
                    k=k+1
                    if (k < 20) then
!                       check for convergence, but only if k < 20 to avoid array out of bounds
                        if(dabs(z_over_l(20)-z_over_l(20-k)) < 0.0001_r8)then
                            rpt=k
                            test=.false.
                        endif
                        if (debug) then

                            if (k==1)write(debug_unit,'(t10,a,g13.6,a/t10,a)')'ITERATIONS = 20, Z/L',z_over_l(20),&
                                    ' < 99 CHECK FOR PING-PONG PATTERN, K < 20','K       Z/L(20)     Z/L(20-K)   TEST  RPT'
                            write(debug_unit,debugform(10))k,z_over_l(20),z_over_l(20-k),test,rpt
                        endif
                    else
!                       convergence not reached; apply diverent solution
                        mol=templ
                        ustar1=tempu
                        thetastar1=tempth
                        
!                       ustar1 is based on Equation 22 page 449 of Luhar and Rayner (2009)
                        z_on_l=sfc_data(d)%wind_ht(h)/mol
                        z0_on_l=sfc_data(d)%zo(h)/mol
                        term=alpha1*z_on_l**beta1*(1.0_r8+gamma1*z_on_l**(1.0_r8-beta1))
                        term0=alpha1*z0_on_l**beta1*(1.0_r8+gamma1*z0_on_l**(1.0_r8-beta1))
                        ustar1=vonk*sfc_data(d)%wspd(h)/(term-term0)
                
!                       thetastar1
!                       equation 18, Section 3.3.1, page 18 of AERMOD MFED
                        thetastar1=vonk*dtheta/(dlog(osdt_hts(1,2)/osdt_hts(1,1))+betam*(osdt_hts(1,2)-osdt_hts(1,1))/mol)

                
!                       first calcuate max value of u* based on eq. 25 of Luhar and Rayner (2009), page 452
                        ustarmax = 0.6_r8*(sfc_data(d)%wind_ht(h)/(dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))))**0.33333_r8
 
                        if (debug)then
                            str1='ITERATIONS = 20, NON-DIVERGING VALUES, Z/L'
                            write(str2,'(2(a))')'U*       THETA-STAR       L            z/L          '&
                            'z0/L           term         term0         ustarmax'
                            write(debug_unit,debugform(8))trim(adjustl(str1)),z_over_l(20),'< 99',trim(adjustl(str2)),ustar1,&
                                thetastar1,mol,z_on_l,z0_on_l,term,term0,ustarmax
                        endif
                        
!                       reset thetastar1
!                       apply upper limit on thetastar1 based on eq. 23 of Luhar and Raynor (2009), page 451
                        thetastar1 = min(thetastar1,0.1_r8*(1.0_r8-(min((2._r8*ustar1)/ustarmax,1.0_r8)-1.0_r8)**2))
                        
!                       reset ustar1 to ustarmax if ustarmax <= ustar1                        
                        if (ustarmax <= ustar1) ustar1=ustarmax
                
!                       adjust thetastar1 for low solar angles
                        if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2)  
                
!                       calculate L
                        mol=ustar1*ustar1*sfc_data(d)%airtemp(h)/(vonk*g*thetastar1)
                
                        z_on_l=sfc_data(d)%wind_ht(h)/mol
                        
                        if (debug)then
                            str1='UPDATED U*, THETA-STAR, AND L'
                            str2='U*       THETA-STAR       L'
                            write(debug_unit,debugform(9))trim(adjustl(str1)),trim(adjustl(str2)),ustar1,thetastar1,mol
                        endif
                        
                        
                        exit !exit from loop
                    endif
                enddo
                
                if (debug) write(debug_unit,'(/t10,a)')'AVERAGING U*, THETASTAR, AND L; AVERAGE VALUES WITH Z/L < 0.7'
                iter=iter+1
                utemp=0.0_r8
                thtemp=0.0_r8
                ltemp=0.0_r8
                counter=0
                if (rpt > 0) then !looping pattern
    j1:             do j=1,rpt
                        if (z_over_l(iter-j) < 0.7_r8) then
                            utemp=utemp+ust(iter-j)
                            thtemp=thtemp+thst(iter-j)
                            ltemp=ltemp+lst(iter-j)
                            counter=counter+1
                            if (debug) then
                                if (counter==1) write(debug_unit,'(/t10,a)')&
                                    'COUNTER   ITER            U*        THETASTAR       L             z/L'
                                write(debug_unit,debugform(11))counter,iter-j,ust(iter-j),thst(iter-j),lst(iter-j),&
                                    z_over_l(iter-j)
                            endif
                        endif
                    enddo j1  
                    if (counter == 0) then
                        if (debug)write(debug_unit,'(/t10,a)')'NO VALUES TO AVERAGE, SET U*, THETASTAR, AND L TO MISSING'
                        ustar1=-9.0_r8
                        thetastar1=-9.0_r8
                        mol=-99999._r8
                    else
                        ustar1=utemp/real(counter,r8)
                        thetastar1=thtemp/real(counter,r8)
                        mol=ltemp/real(counter,r8)
                    endif
                    if (debug)then
                        str1='UPDATED U*, THETA-STAR, AND L'
                        str2='U*       THETA-STAR       L'
                        write(debug_unit,debugform(9))trim(adjustl(str1)),trim(adjustl(str2)),ustar1,thetastar1,mol
                    endif
                    
                    z_on_l=sfc_data(d)%wind_ht(h)/mol
                endif
                
            endif
            
        elseif (iter < 20) then !converged
            
            if (z_on_l > 0.7_r8) then
                if (debug)write(debug_unit,'(/t10,a,g13.6,a)')'ITERATIONS < 20 Z/L ',z_on_l,&
                    ' > 0.7; RECALCULATE U*, THETASTAR, AND L'
                mol=templ
                ustar1=tempu
                thetastar1=tempth
                
                z_on_l=sfc_data(d)%wind_ht(h)/mol
                z0_on_l=sfc_data(d)%zo(h)/mol
                term=alpha1*z_on_l**beta1*(1.0_r8+gamma1*z_on_l**(1.0_r8-beta1))
                term0=alpha1*z0_on_l**beta1*(1.0_r8+gamma1*z0_on_l**(1.0_r8-beta1))
                ustar1=vonk*sfc_data(d)%wspd(h)/(term-term0)
                
!               thetastar1
!               equation 18, Section 3.3.1, page 18 of AERMOD MFED
                thetastar1=vonk*dtheta/(dlog(osdt_hts(1,2)/osdt_hts(1,1))+betam*(osdt_hts(1,2)-osdt_hts(1,1))/mol)

                
!               apply upper limit on thetastar1 based on eq. 23 of Luhar and Raynor (2009);
!               first calcuate max value of u* based on eq. 25 of Luhar and Raynor
                ustarmax = 0.6_r8*(sfc_data(d)%wind_ht(h)/(dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))))**0.33333_r8
                
                if (debug)then
                    str1='ITERATIONS < 20, Z/L'
                    str2='U*       THETA-STAR       L            z/L          z0/L           term         term0         ustarmax'
                    write(debug_unit,debugform(8))trim(adjustl(str1)),z_on_l,'> 0.7',trim(adjustl(str2)),ustar1,thetastar1,mol,&
                        z_on_l,z0_on_l,term,term0,ustarmax
                endif
                
!               reset thetastar1
                thetastar1 = min(thetastar1,0.1_r8*(1.0_r8-(min((2._r8*ustar1)/ustarmax,1.0_r8)-1.0_r8)**2))
                if (ustarmax <= ustar1) ustar1=ustarmax
                
!               adjust thetastar1 for low solar angles
                if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2)  
                
!               calculate L
                mol=ustar1*ustar1*sfc_data(d)%airtemp(h)/(vonk*g*thetastar1)
                
                if (debug)then
                    str1='UPDATED U*, THETA-STAR, AND L'
                    str2='U*       THETA-STAR       L'
                    write(debug_unit,debugform(9))trim(adjustl(str1)),trim(adjustl(str2)),ustar1,thetastar1,mol
                endif
                
                
                z_on_l=sfc_data(d)%wind_ht(h)/mol                
                
!               thetastar1
!               equation 18, Section 3.3.1, page 18 of AERMOD MFED
                thetastar1=vonk*dtheta/(dlog(osdt_hts(1,2)/osdt_hts(1,1))+betam*(osdt_hts(1,2)-osdt_hts(1,1))/mol)
!               adjust thetastar1 for low solar angles
                if (angle > 0.0_r8 .and. angle < crit_angle) thetastar1=thetastar1*(1.0_r8-(angle/crit_angle)**2) 
!               calculate L
                mol=ustar1*ustar1*sfc_data(d)%airtemp(h)/(vonk*g*thetastar1)
                
                z_on_l=sfc_data(d)%wind_ht(h)/mol  
            else
                if (debug)write(debug_unit,'(/t10,a,g13.6,a)')'ITERATIONS < 20 Z/L ',z_on_l,&
                    ' < 0.7; DO NOT RECALCULATE U*, THETASTAR, AND L'
            endif
        endif
    endif
    
!   set final values to sfc_data
    sfc_data(d)%theta_star(h)=thetastar1
    sfc_data(d)%ustar(h)=ustar1
    sfc_data(d)%mol(h)=mol
    
    if (debug) then
        str1='FINAL VALUES      U*          THETA-STAR      L'
        write(debug_unit,debugform(7))trim(adjustl(str1)),ustar1,thetastar1,mol
        write(debug_unit,'(t10,a,1x,a)')trim(adjustl(modnam)),'END'
    endif
    
    return
    end subroutine bulk_rich
    
!*********************************************************************************************************

    subroutine check_flux(d,h,reset)
!=========================================================================================================
!   SUBROUTINE CHECK_FLUX
!   THIS SUBROUTINE CHECKS THAT U* AND THETA* WON'T RESULT IN UNREALISTIC VALUES OF HEAT FLUX FOR STABLE
!   CONDITIONS. IT RETURNS A LOGICAL VARIABLE RESET TO LET PBL_PROC KNOW THAT MONIN-OBUKHOV LENGTH SHOULD
!   RECALCULATED
!
!   MODIFIED DECEMBER 3, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS 
!
!   D:      DAY COUNTER
!   H:      HOUR COUNTER
!
!   OUTPUT ARGUMENTS
!
!   RESET:  LOGICAL VARIABLE DENOTING IF U* AND THETA-STAR RECALCULATED
!
!   Variable definitions
!      
!   Integer variables
!   d:          day counter
!   h:          hour counter
!
!   Real variables
!   betam:      constant (5 or 4.7)
!   cdn:        drag coefficient
!   hlim:       -64 W/m**2
!   xlimit:     -hlim/(density*cp*gravity)
!   aa:         -cdn*wind speed
!   bb:         0
!   cc:         see code
!   ustrout:    u* output from cubic solution code
!   
!   the following variables are related to the cubic solution
!   cm,sgn,a3,ap,bp,ap3,troot,bp2,app,bsv,alpha,tr,bpp,test
!
!   Logical variables
!   reset:  logical variable denoting if u* and theta-star recalculated
!
!   Character variables
!   debugform:  formats for debug messages
!   str1:       text string for debug messages
!   str2:       text string for debug messages
!   modnam:     Subroutine name
!=========================================================================================================
    use main1, only : g
    implicit none
    
    integer(kind=4),intent(in) :: d,h
    real(kind=r8) :: betam 
    real(kind=r8) :: hlim=-64._r8
    real(kind=r8) :: cdn,xlimit,aa,bb,cc,ustrout
    real(kind=r8) :: cm,sgn,a3,ap,bp,ap3,troot,bp2,app,bsv,alpha,tr,bpp,test
    logical,intent(out) :: reset
    character(len=100) :: debugform(7)
    character(len=110) :: str1,str2
    character(len=10) :: modnam='CHECK_FLUX'
    
!   debug message formats
!   1. initial constants and values
    write(debugform(1),'(a)')'(/t11,a/t10,f3.1,1x,f6.1,1x,f7.5,1x,f5.1,f5.1,3(2x,f6.2),2(2x,f5.1))'

!   2. CD, XLIIMIT, and CC or CM, ALPHA, and USTROUT values
    write(debugform(2),'(a)')'(/t15,a/t9,3(1x,g13.6))'

!   3.  values for cubic calcualation
    write(debugform(3),'(a)')'(/t11,a/t10,f3.1,8(1x,g13.6))'

!   4.  test values
    write(debugform(4),'(a)')'(/t15,a/t9,4(g13.6))'

!   5.  test values
    write(debugform(5),'(a)')'(/t15,a,/t9,3(1x,g13.6),t54,f4.1,t61,g13.6,1x,g13.6)'
    
!   6.  reset of u* and theta* based on u* out and xlimit
    write(debugform(6),'(a)')'(/t10,a/t15,a/t9,2(1x,g13.6))'

!   7. reset theta* based on u* and HLIM
    write(debugform(7),'(a)')'(/t10,a/t12,a/t10,g13.6)'

!   initialize reset
    reset=.false.
    
    if (adjustar) then
        betam=4.7_r8
    else
        betam=5.0_r8
    endif
    
    if (debug) then
        write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' START'
        
        str1='k    Cp    G       HLIM BETAM  TEMP      WS   WIND HT   Z0    DENSITY'
        write(debug_unit,debugform(1))trim(adjustl(str1)),vonk,cp,g,hlim,betam,sfc_data(d)%airtemp(h),sfc_data(d)%wspd(h),&
            sfc_data(d)%wind_ht(h),sfc_data(d)%zo(h),rho(h,d)
    endif
    
!   calculate drag coefficient
    if (adjustar) then
        cdn=vonk/dlog((sfc_data(d)%wind_ht(h)-5.0_r8*sfc_data(d)%zo(h))/sfc_data(d)%zo(h))
        xlimit=-hlim/(rho(h,d)*cp)
        cc=betam*(sfc_data(d)%wind_ht(h)-5.0_r8*sfc_data(d)%zo(h))*g*xlimit*cdn/sfc_data(d)%airtemp(h)
    else
!       defined in Section 3.3.1, page 17 of AERMOD MFED  
        cdn=vonk/dlog(sfc_data(d)%wind_ht(h)/sfc_data(d)%zo(h))
        xlimit=-hlim/(rho(h,d)*cp)
        cc=betam*sfc_data(d)%wind_ht(h)*g*xlimit*cdn /sfc_data(d)%airtemp(h)
    endif
    
!   debug information
    if (debug) then
        str1=' CDN         XLIMIT         CC'
        write(debug_unit,debugform(2))trim(adjustl(str1)),cdn,xlimit,cc
    endif
    
    if (sfc_data(d)%ustar(h)*sfc_data(d)%theta_star(h) > xlimit) then
        aa=-cdn*sfc_data(d)%wspd(h)
        bb=0.0_r8
!       the calculations below were formally in the subroutine CUBIC prior to this AERMET 
        a3=aa/3.0_r8
        ap=bb-aa*a3
        bp=2.0_r8*a3**3-a3*bb+cc
        ap3=ap/3.0_r8
        bp2=bp/2.0_r8
        troot=bp2*bp2+ap3*ap3*ap3
        
        if (debug) then
            str1='BB     AA             CC            A3            AP            BP           AP3           BP2          TROOT'
            write(debug_unit,debugform(3))trim(adjustl(str1)),bb,aa,cc,a3,ap,bp,ap3,bp2,troot
        endif
        if (troot > 0.0_r8) then
            tr=dsqrt(troot)
            test=-bp2+tr
            bsv=-bp2-tr
            if (test > 0.0_r8) then
                ustrout=-9.0_r8
                str1='TR           TEST          BSV          USTROUT'
                if (debug)write(debug_unit,debugform(4))trim(adjustl(str1)),tr,test,bsv,ustrout
            else
                app=test**0.3333330_r8
                if (bsv /= 0.0_r8) then
                    sgn=dsign(1.0_r8,bsv)
                    bpp=sgn*(dabs(bsv))**0.3333330_r8
                    ustrout=sngl(app+bpp-a3)
                    str1='TR           TEST          BSV          SGN        BPP         USTROUT'
                    if (debug)write(debug_unit,debugform(5))trim(adjustl(str1)),tr,test,bsv,sgn,bpp,ustrout
                else
                    ustrout=sngl(app-a3)
                    str1='TR           TEST          BSV          USTROUT'
                    if (debug)write(debug_unit,debugform(4))trim(adjustl(str1)),tr,test,bsv,ustrout
                endif
            endif
        else  
            cm=2.0_r8*dsqrt(-ap3)
            alpha=dacos(bp/(ap3*cm))/3.0_r8
            ustrout=sngl(cm*dcos(alpha)-a3)
            str1='CM           ALPHA        USTROUT'
            if (debug)write(debug_unit,debugform(2))trim(adjustl(str1)),cm,alpha,ustrout
        endif
        

        if (dabs(ustrout-(-9.0_r8)) > 0.01_r8) then
!           reset u* and theta *
            sfc_data(d)%ustar(h)=ustrout
            sfc_data(d)%theta_star(h)=xlimit/sfc_data(d)%ustar(h)
            reset=.true.
            str1='RESET U* AND THETASTAR BASED ON USTROUT AND XLIMIT'
            str2='U*         THETA-STAR'
            if (debug)write(debug_unit,debugform(6))trim(adjustl(str1)),trim(adjustl(str2)),sfc_data(d)%ustar(h),&
                sfc_data(d)%theta_star(h)
        else
!           keep u* as is but reset theta *
            sfc_data(d)%theta_star(h)=-hlim/(rho(h,d)*cp*sfc_data(d)%ustar(h))
            reset=.true.
            str1='RESET THETA-STAR BASED ON U* AND HLIM'
            if (debug)write(debug_unit,debugform(7))trim(adjustl(str1)),'THETA-STAR',sfc_data(d)%theta_star(h)
        endif
    endif
    
    if (debug) then
        if (.not. reset) write(debug_unit,'(/t10,a)')'U* AND THETA-STAR NOT RESET'
        write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' END'
    endif
    
    return
    end subroutine check_flux
!*********************************************************************************************************

    subroutine mech_ht(d,h)
!=========================================================================================================
!   SUBROUTINE MECH_HT
!   CALCULATE MECHANICAL MIXING HEIGHT FOR ANY HOUR (CONVECTIVE OR STABLE)
!     
!   MODIFIED MARCH 9, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS
!   D:              DAY COUNTER
!   H:              HOUR COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   h:              hour counter
!   prev_day:       day index for previous hour's mixing height.
!   prev_hr:        hour index for previous hour's mixing height
!
!   Real variables
!   betatau:        constant of 2 used to calculate tau based on equation on page 21 of AERMOD MFED
!   deltat:         time difference between current hour and previous hour in seconds (3,600 seconds)
!   xconst:         constant (2400) used to calculate unsmoothed height (equation 24 of MFED)
!   explim:         limit to avoid underflow
!   tau:            time scale to govern rate of change of height of layer (page 21 of AERMOD MFED)
!   xponen:         deltat/tau
!   mixht:          unsmoothed mixing height calculated by equation 24 of AERMOD MFED
!
!   Character variables
!   formstr:        formats for messages
!   date_hr_str:	date and hour text string
!   debugform:  formats for debug messages
!   str1:       text string for debug messages
!   modnam:         Subroutine name
!=========================================================================================================
    use file_units, only: msg_unit
    use main1, only: msg_form
    use onsite, only: mix_var
    implicit none
    integer(kind=4), intent(in) :: d,h
    integer(kind=4) :: prev_day,prev_hr
    real(kind=r8), parameter :: betatau=2.0_r8
    real(kind=r8), parameter :: deltat=3600._r8
    real(kind=r8), parameter :: xconst=2400._r8
    real(kind=r8), parameter :: explim=50._r8 
    real(kind=r8) :: tau,xponen,mixht
    character(len=80) :: formstr(2)
    character(len=20) :: date_hr_str
    character(len=100) :: debugform='(t10,a/t13,f3.1,t19,f6.1,t27,f11.4,3(1x,g13.6))'
    character(len=110) :: str1='BETATAU  DELTA_T  PREV. ZIM    TAU              U*        DELTA_T/TAU'
    character(len=10) :: modnam='MECH_HT' 

!   format for messages
!   1.  exceeds mech_max; reset
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,f6.1,1x,a,1x,a20)'
    
!   2.  mixing height is 0 m or calculate mixing height for missing hour
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a20,1x,a)'
    
    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sfc_data(d)%sfcdate,'HR',h
    
    if (debug)write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' START'
    
!   post 21DRF, correct bug that was initially put in AERMET 21112 and
!   carried over to 21DRF.  Prior to 21112, mechanical mixing height only used
!   the onsite mixing height when stable hour and calculated for convective hour. This
!   was thought to be a bug so in AERMET 21112 so mechanical height was always
!   based on onsite mixing height regardless of stability.  It was found that in Section A.11
!   of AERMOD MFED, onsite heights are only used for mechanical mixing heights when stable hour.
!   For convective hours, mechanical is calculated, regardless of whether onsite heights are used.
!   now, only assign the mixing height to the hour if a stable hour. if convective or no onsite height
!   calculate.
!   if have onsite mixing height, set mixht to that value
    if (onmix .and. .not. cbl(h,d)) then
        mixht=osdata1(osvars(mix_var)%readvar,1,h,ios(d))
    else
!       calculate initial mixing height based on Equation 24, Section 3.4.2 in AERMOD MFED    
        mixht=xconst*sfc_data(d)%ustar(h)**(1.5_r8)
!       post 21DRF, if mixing heights are read in but missing for hour, alert user
        if (osvars(mix_var)%lread .and. .not. onmix .and. .not. cbl(h,d)) then
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I87',modnam,date_hr_str,&
            'CALCULATED MECHANICAL MIXING HEIGHT'
            if (debug) write(debug_unit,'(/t10,a)')'MECHANICAL MIXING HEIGHT MISSING, WILL BE CALCULATED'
        endif
    endif
    
    if (debug)write(debug_unit,'(/t10,a,f10.4)')'INITIAL MIXING HEIGHT ',mixht
!   reset if > mech_max
    if (mixht > mech_max) then
        mixht=mech_max
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I79',modnam,'SBL HT SET TO',mech_max,'FOR',date_hr_str
        if (debug)write(debug_unit,'(/t10,a,f6.1)')'RESET MIXING HEIGHT TO MECH_MAX',mixht
    endif
    
!   smooth the height if the previous hour's mixing height is not missing
!   however, cannot smooth the first hour of the data period since there
!   is not a previous hour.  For first hour of data period, set zim in sfc_data to the 
!   unsmoothed height.  likewise, if the previous hour's mixing height is missing, set current
!   height to unsmoothed height
    if (h == 1 .and. d==1) then
        sfc_data(d)%zim(h)=mixht
        if (debug) write(debug_unit,'(t10,a,f11.4)')'MIXING HEIGHT OF FIRST HOUR OF DATA PERIOD SET TO ',sfc_data(d)%zim(h)
    else
!       first set the previous hour and day
!       previous day may be same day as current hour or previous day if current
!       hour is 1.
        if (h == 1) then
            prev_hr=24
            prev_day=d-1
        else
            prev_hr=h-1
            prev_day=d
        endif
        if (sfc_data(prev_day)%zim(prev_hr) > 0.0_r8) then
!           previous hour is okay, proceed
            tau=sfc_data(prev_day)%zim(prev_hr)/(betatau*sfc_data(d)%ustar(h))
            xponen=deltat/tau
            if (debug)write(debug_unit,debugform)trim(adjustl(str1)),betatau,deltat,sfc_data(prev_day)%zim(prev_hr),tau,&
                sfc_data(d)%ustar(h),xponen
            if (xponen > explim) then
!               avoid underflow
                sfc_data(d)%zim(h)=mixht
                if (debug) write(debug_unit,'(t10,a,f11.4)')'XPONEN > EXPLIM; MIXING HEIGHT SET TO UNSMOOTHED HEIGHT',&
                    sfc_data(d)%zim(h)
            else
!               smoothed mechanical mixing height, equation 26 section 3.4.2 AERMOD MFED
                sfc_data(d)%zim(h)=sfc_data(prev_day)%zim(prev_hr)*dexp(-xponen)+mixht*(1.0_r8-dexp(-xponen))
                if (debug)write(debug_unit,'(/t10,a,f11.4)')'SMOOTHED MIXING HEIGHT: ',sfc_data(d)%zim(h)
            endif
        else
!           use unsmoothed height
            sfc_data(d)%zim(h)=mixht
            if (debug) write(debug_unit,'(t10,a,f11.4)')'PREVIOUS HOUR MIXING HEIGHT MISSING; SET TO UNSMOOTHED HEIGHT: ',&
                sfc_data(d)%zim(h)
        endif
    endif
    
!   post 21DRF				
!   check the mechanical mixing heights for zero
!   mechanical mixing height
    if (sfc_data(d)%zim(h) == 0.0_r8) write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W75',modnam,date_hr_str,&
        'MECHANICAL MIXING HEIGHT = 0 M'

    if (debug)write(debug_unit,'(t10,2(a))')trim(adjustl(modnam)),' END'
    
    return
    end subroutine mech_ht
!*********************************************************************************************************

    subroutine read_sound(d)
!=========================================================================================================
!   SUBROUTINE READ_SOUND
!   PROCESS SOUNDING FOR CONVECTIVE MIXING HEIGHT AND POTENTIAL TEMPERATURE LAPSE RATE CALCULATIONS
!     
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS
!   D:              DAY COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   ilev:           level loop counter
!
!   Real variables
!   baseht:         1st level of sounding
!   zdz:            difference between original sounding top and 500 m
!   ptdz:           lapse rate of theta
!   dtdz:           temperature difference over 500 m
!   dt:             delta t btween original sounding top and 5 km
!
!   Character variables
!   formstr:        formats for messages
!   debugform:      formats for debug messages
!   str1:           text string for debug messages
!   str2:           text string for debug messages
!   modnam:         Subroutine name
!=========================================================================================================
    !use main1, only: eps 
    use file_units, only: msg_unit
    use main1, only: msg_form
    use upperair, only: updata,sound_info,uptop,upvars
  
    implicit none
    integer(kind=4), intent(in) :: d
    integer(kind=4) :: ilev
    real(kind=r8) :: baseht,zdz,ptzdz,dtdz,dt
    character(len=80) :: formstr(4)
    character(len=120) :: debugform(2)
    character(len=120) :: str1,str2
    character(len=10) :: modnam='READ_SOUND' 
    
!   formats for messages
!   1.  sounding number for date
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'2(a,1x,i8,1x),a,1x,i2.2,1x,a,1x,i2)'
    
!   2.  cannot extend sounding
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a)'
    
!   3.  top of sounding too low; cannot extend sounding
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,f4.1,1x,a)'
    
!   4.  sounding extends above max level; sounding not extended
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,2(1x,f4.1,1x,a))'
    
!   debug formats
!   1.  sounding date and number
    write(debugform(1),'(a)')'(/t15,a,1x,i8,1x,a,1x,i2.2,1x,a,1x,i2)'

!   2.  calculation of theta at 5 km
    write(debugform(2),'(2(a))')'(/t15,a,/t15,a,/t10,g13.6,t26,i3,t34,f6.1,t45,f6.1,t57,f8.3,t71,f6.1,t83,f8.3,t95,g13.6,1x,',&
        'g13.6,t125,f8.3)'
        
    lextend=.false.
    lextended=.false.
    baseht=0.0_r8
    zdz=0.0_r8
    if (debug) then
        write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' START'  
    endif
    
!   write message to message file indicating the sounding
    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I77',modnam,'SOUNDING FOR DATE:',sfc_data(d)%sfcdate,'IS',&
            sound_info(iup(d))%snddate,'HR',sound_info(iup(d))%snd_hr(isnd(d)),'SOUNDING #',isnd(d)
    
!   number of levels will depend on the sounding height
!   if below 5 km, then add 1 level
    baseht=updata(2,1,isnd(d),iup(d))
    
    if (updata(2,sound_info(iup(d))%nlevels(isnd(d)),isnd(d),iup(d)) < real(uptop,r8)) then
        nlevs=sound_info(iup(d))%nlevels(isnd(d))+1
        lextend=.true.
    else
        nlevs=sound_info(iup(d))%nlevels(isnd(d))
    endif
    
!   allocate tempk (temperature in K) and theta
    allocate(tempk(nlevs))
    allocate(theta(nlevs))
    allocate(pres(nlevs))
    allocate(ht(nlevs))
    
    pres=real(upvars(1)%missval,r8)!*upvars(1)%conv
    ht=real(upvars(2)%missval,r8)!*upvars(2)%conv
    tempk=real(upvars(3)%missval,r8)!*upvars(3)%conv
    theta=real(upvars(3)%missval,r8)!*upvars(3)%conv
    
!   convert temperature from C to K and calculate potential temperature
!   also subtract the height of the first level from each level above to
!   convert heights to above ground level.  this should be done in stage 1
!   but do again as a precaution.
    l1: do ilev=1,nlevs
!       account for fact that nlevs maybe more than one level above original sounding
        if (ilev <= sound_info(iup(d))%nlevels(isnd(d))) then
            pres(ilev)=updata(1,ilev,isnd(d),iup(d))
            ht(ilev)=updata(2,ilev,isnd(d),iup(d))
            tempk(ilev)=updata(3,ilev,isnd(d),iup(d)) !temperature in C for now
        endif
!       skip if height, temperature, or pressure is missing
        if (dabs(pres(ilev)-real(upvars(1)%missval,r8)) < eps .or. &
            dabs(ht(ilev)-real(upvars(2)%missval,r8)) < eps .or. &
            dabs(tempk(ilev)-real(upvars(3)%missval,r8)) < eps) cycle l1
            tempk(ilev)=tempk(ilev)+273.15_r8
            theta(ilev)=tempk(ilev)*(1000._r8/pres(ilev))**0.2857_r8
            !ht(ilev)=ht(ilev)-baseht
    enddo l1
    
!   extend the sounding if below 5000 m
    if (lextend) then
!       if sounding exceeds 600 m, then go ahead and compute potential temperature gradient and extend sounding

        zdz=ht(nlevs-1)-500._r8
        if (debug)write(debug_unit,'(a,f8.1,1x,a,1x,f8.1,1x,a)')'ZDZ:',zdz,'=',ht(nlevs-1),'- 500'
        if (zdz >= 100._r8) then
!           start at level just below top. remember nlevs is number of levels in sounding +1 because we
!           are extending, so that is why we start at nlevs -2. nlevs -2 = sound_info(iup(d))%nlevels(isnd(d))-1
            ilev=nlevs-2
            lextended=.false.
            do while (ilev >=1 .and. .not. lextended)
                if (ht(nlevs-1) - ht(ilev) >= 500._r8) then
                    ptzdz=theta(ilev)+(zdz - ht(ilev))/(ht(ilev+1)-ht(ilev))*(theta(ilev+1)-theta(ilev))
                    dtdz=(theta(nlevs-1)-ptzdz)/500.0_r8
                    dt=(real(uptop,r8)-ht(nlevs-1))*dtdz
                    theta(nlevs)=theta(nlevs-1)+dt
                    ht(nlevs)=real(uptop,r8)
                    if (debug) then
                        str1='CALCULATION OF THETA AT 5 KM'
                        write(str2,'(2(a))')'PTDZ       ILEV    ORIG TOP   HT(ILEV)   THETA(ILEV)  HT(ILEV+1)  ',&
                            'THETA(ILEV+1)     DTDZ          DT        THETA (5KM)'
                        write(debug_unit,debugform(2))trim(adjustl(str1)),trim(adjustl(str2)),ptzdz,ilev,ht(nlevs-1),ht(ilev),&
                            theta(ilev),ht(ilev+1),theta(ilev+1),dtdz,dt,theta(nlevs)
                    endif
                    lextended=.true.
                else
                    ilev=ilev-1
                endif
            enddo
            if (.not. lextended) then
                str1='CANNOT EXTEND SOUNDING; TWO LEVELS NOT FOUND FOR POTENTIAL TEMPERATURE'
                write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W76',modnam,'DATE:',sfc_data(d)%sfcdate,trim(adjustl(str1))
!               reset nlevs because it wasn't extended
                nlevs=nlevs-1
            endif
        else
!           too low
            str1='TOP OF SOUNDING'
            str2='KM IS TOO LOW; SOUNDING NOT EXTENDED'
            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W76',modnam,'DATE:',sfc_data(d)%sfcdate,trim(adjustl(str1)),&
                ht(nlevs-1)/1000.0_r8,trim(adjustl(str2))
!           reset nlevs because it wasn't extended
            nlevs=nlevs-1
        endif
    else
!       write message that sounding not extended
        write(msg_unit,formstr(4))adjustl(pathid(ipath)),'W76',modnam,'DATE',sfc_data(d)%sfcdate,'TOP OF SOUNDING',&
            ht(nlevs)/1000.0_r8,'KM EXTENDS BEYOND',real(uptop,r8)/1000._r8,'KM; SOUNDING NOT EXTENDED'
    endif
    
!   write sounding to debug file
    if (debug) then
        write(debug_unit,debugform(1))'SOUNDING DATE:',sound_info(iup(d))%snddate,'HR',sound_info(iup(d))%snd_hr(isnd(d)),&
            'SOUNDING #',sound_info(iup(d))%nsnding
        if (lextended) then
            write(debug_unit,'(t15,a,i1,a)')'SOUNDING EXTENDED TO ',int(real(uptop,r8)/1000.0_r8),' KM'
        else
            write(debug_unit,'(t15,a)')'SOUNDING NOT EXTENDED'
        endif
        write(debug_unit,'(/t15,a)')'HEIGHT    PRES      TEMP      THETA'
    l3: do ilev=1,nlevs
            write(debug_unit,'(t11,4(1x,f9.3))')ht(ilev),pres(ilev),tempk(ilev),theta(ilev)
        enddo l3
    endif
    
    if (debug)write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' END'
    
    return
    end subroutine read_sound
!*********************************************************************************************************

    subroutine conv_ht(d,cblhr1)
!=========================================================================================================
!   SUBROUTINE CONV_HT
!   CALCULATE CONVECTIVE MIXING HEIGHT FOR CONVECTIVE HOURS
!     
!   MODIFIED APRIL 22, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS
!   D:              DAY COUNTER
!   CBLHR1:         FIRST CONVECTIVE HOUR OF THE DAY
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   cblhr1:         first convective hour of the day
!   h:              hour counter
!   inc:            increment for loop k1 set to 15 minutes
!   ilev:           sounding level counter
!   hhmm:           sounding hour (LST) in hours:minutes, h*60
!   last_stable_hr: hour preceding first convective hour of the day
!   last_conv_hr:   last convective hour of the day, need two successive stable hours to assign
!   min_midnite:    2 element array of minutes past midnight for first convective hour (1) and last convective hour (2)
!                   e.g., first convective hour is 0800 or (8*60) and last convective hour is 1800 (18*60)
!   kk:             minute counter for loop k1
!   it1:            kk divided by 60; current hour of k1 loop
!   it2:            it1 + 1; next hour of k1 loop
!   kk60:           kk divided by 60; current hour of k1 loop; not necessarily the same as it1 as if it1 is 0 it is
!                   reset to 1.
!
!   Real variables
!   capa:           Capital A term in equation 22 of AERMOD MFED (page 20). set to 0.2
!   theta_max:      Maximum value of potential temperature in sounding profile
!   oldh:           saved value of integrated value heat flux, SHEAT
!   sheat:          cumulative heat flux
!   density:        density of air for the hour.  If density for hour is missing density is 1.2  
!   harea:          value of equation 22 of AERMOD MFED (page 20).
!   smh:            24-element array of integrated heat flux
!   pblht:          mixing height
!   homin:          minimum positive heat flux
!   theta_sum:      sum of theta_a
!   theta_a:        area of potential temperature profile
!
!   Logical variables
!   gotzi:          Logical variable denoting that a convective mixing height was calculated
!
!   Character variables
!   modnam:         Subroutine name
!   formstr:        formats for messages
!   debugform:      formats for debug messages
!   str1:           text string for debug messages
!   str2:           text string for debug messages
!   date_hr_str:	date and hour text string
!   fill:           24-element 1-character array denoting if heat flux for hour was persisted or filled;
!                   only used for debugging purposes
!   stable_str:     stability text string for heat flux reporting
!=========================================================================================================
    !use main1, only: eps    
    use file_units, only: msg_unit
    use main1, only: msg_form
    use upperair, only: sound_info
    implicit none
    integer(kind=4), intent(in) :: d,cblhr1
    integer(kind=4), parameter :: inc=15
    integer(kind=4) :: ilev,hhmm,h,last_stable_hr,last_conv_hr,min_midnite(2),kk,it1,it2,kk60
    real(kind=r8), parameter :: capa=0.2_r8
    real(kind=r8) :: theta_max,oldh,sheat,density,harea,smh(24),pblht

    real(kind=r8), parameter :: homin=0.0001_r8
    real(kind=r8), allocatable, dimension(:) :: theta_sum
    real(kind=r8), allocatable, dimension(:) :: theta_a
    logical :: gotzi
    character(len=1), allocatable, dimension(:) :: fill
    character(len=6) :: stable_str
    character(len=80) :: formstr(3)
    character(len=120) :: debugform(3)
    character(len=120) :: str1,str2
    character(len=20) :: date_hr_str
    character(len=10) :: modnam='CONV_HT' 
    
    allocate(theta_sum(nlevs))
    allocate(theta_a(nlevs))
    
!   formats for messages
!   1. convective mixing height exeeds original sounding top
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,f7.1,1x,a,1x,a20,1x,a,1x,f7.1)'
    
!   2.  CBL height
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,f6.1,1x,a,1x,a20)'
    
!   3.  convective height is 0 m
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a20,1x,a)'

!   debug formats
!   1. last stable hour, first/last convective hours; number of minutes since midnight
    write(debugform(1),'(a)')'(/t15,a/t20,i2.2,t42,i2.2,t64,i2.2/t15,a,2(1x,i4))'
    
!   2. heat flux and stability
    write(debugform(2),'(a)')'(t15,a1,1x,i2,t23,g13.6,t42,a)'
    
!   3. theta
    write(debugform(3),'(a)')'(t15,i5,t22,f9.3,t34,g13.6,3(1x,g13.6))' 

    gotzi=.false.
    min_midnite=0
!   initialize density to 1.2. it will be reset later if density is not missing
    density=1.2_r8
    if (debug) then
        write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' START'  
        allocate(fill(24))
        fill=''
    endif
    

    
!   convert time from hour to hour:iminute
    hhmm=sound_info(iup(d))%snd_hr(isnd(d))*100
!   initial conditions for potential temperature, area under the potential temperature
!   profile, and sum of the area under the profile
    theta_max=theta(1)
    theta_a=0.0_r8
    theta_sum(1)=0.0_r8

!   compute the potential temperature interals and sum the integrals to get sum of area under
!   potential temperature profile
        if (debug)write(debug_unit,'(/t15,a)')'LEVEL    HT        THETA(LEVEL)   THETA_A(LEVEL)   THETA_MAX   THETA_SUM'
    l1: do ilev=2,nlevs
        if (theta(ilev) < theta_max) then
!           potential temperature decreased with height
            theta_a(ilev)=0.0_r8
            theta(ilev)=theta_max
        else
!           compute the area under the potential temperature profile for the integral from ilev-1 to ilev
!           using the formula for the area under a trapezoid (0.5 x base x height)
            theta_a(ilev)=0.5_r8*(theta(ilev)-theta(ilev-1))*(ht(ilev)+ht(ilev-1))
!           save max potential temperature so far to check against that the values are increasing with 
!           height
            theta_max=dmax1(theta(ilev),theta_max)
        endif
        theta_sum(ilev)=theta_sum(ilev-1)+theta_a(ilev)
        if (debug)write(debug_unit,debugform(3))ilev,ht(ilev),theta(ilev),theta_a(ilev),theta_max,theta_sum(ilev)
    enddo l1
    
!   compute the integrated heat flux
!   first fill in the missing heat flux if a convective hour and the hour before or after are non-missing
!   start with the second convective hour of the day. note that the first hour is not missing because that
!   is a condition for this subroutine to be called.
    h1: do h=cblhr1+1,23
        if (cbl(h,d) .and. dabs(sfc_data(d)%hflux(h)-(-999.0_r8)) <= eps .and. dabs(sfc_data(d)%hflux(h-1)-(-999.0_r8)) > eps &
            .and. dabs(sfc_data(d)%hflux(h+1)-(-999.0_r8)) > eps .and. (sfc_data(d)%hflux(h-1) >  0.0_r8 .or. &
            sfc_data(d)%hflux(h+1) > 0.0_r8)) then
                sfc_data(d)%hflux(h)=(sfc_data(d)%hflux(h-1)+sfc_data(d)%hflux(h+1))/2.0_r8
                if (debug) fill(h)='*'
        endif
            
    enddo h1
    
!   set last stable hour, this will be the hour before cblhr1 (first convective hour)
    last_stable_hr=cblhr1-1

!   set last convective hour, usually last hour of positive heat flux.  note that original AERMET allowed for a flip-flop
!   but since AERMET sets negative heat flux to positive for convective hours, that doesn't happen
    last_conv_hr=0
    h=cblhr1+1 !2nd convective hour
    
    do while (last_conv_hr == 0 .and.  h <= 24)
        if (sfc_data(d)%hflux(h) < homin .and. dabs(sfc_data(d)%hflux(h)-(-999.0_r8)) > eps .and. &
            sfc_data(d)%hflux(h-1) <  homin .and. dabs(sfc_data(d)%hflux(h-1)-(-999.0_r8)) > eps) then
            last_conv_hr=h-2
        else
            h=h+1
        endif
    enddo
    
!   if didn't assign last_conv_hr in the loop because the last positive heat flux hour was followed
!   by missing values, then assign the last positive heat flux hour to last_conv_hr
!   start back at hour 23 and find first positive heat flux
    if (last_conv_hr == 0) then
        h=23
        do while (last_conv_hr==0 .and. h >= cblhr1)
            if (sfc_data(d)%hflux(h) <= 0.0_r8 .and. sfc_data(d)%hflux(h-1) > 0.0_r8) then
                last_conv_hr=h-1
            else
                h=h-1
            endif
        enddo
    endif
    
!   minutes since midnight when heat flux becomes positive
!   original AERMET has a complicated formula to account for fact that
!   time may not be a whole hour, i.e. 5:30.  However, that won't happen
!   because observation times are listed as whole hours. therefore the
!   time since midnight in minutes is the hour x 60 . also subtract 30 minutes
    min_midnite(1)=cblhr1*60-30
!   last convective hour
    min_midnite(2)=last_conv_hr*60
    
!   if debug write out the 24 hours of heat flux and indicate last stable hour, first convective hour and last convective hour
    if (debug) then
        str1='LAST STABLE HOUR  FIRST CONVECTIVE HOUR   LAST CONVECTIVE HOUR'
        str2='NUMBER OF MINUTES SINCE MIDNIGHT FOR START AND ENDING CONVECTIVE CONDITIONS'
        
        write(debug_unit,debugform(1))trim(adjustl(str1)),last_stable_hr,cblhr1,last_conv_hr,trim(adjustl(str2)),min_midnite(1),&
            min_midnite(2)
        str1='HOURLY SURFACE HEAT FLUX '
        str2='HOUR     HEAT FLUX        STABILITY'
        if (debug)write(debug_unit,'(//t15,a/t15,a)')trim(adjustl(str1)),trim(adjustl(str2))
        
    h2: do h=1,24
            if (cbl(h,d)) then
                stable_str='CONV'
            else
                stable_str='STABLE'
            endif
            
            write(debug_unit,debugform(2))fill(h),h,sfc_data(d)%hflux(h),&
                trim(adjustl(stable_str))
        enddo h2
        write(debug_unit,'(/t15,a)')'* = FILLED VALUE'
    endif
    
    smh=0.0_r8
    
!   compute integrated heat flux. convert from W/m^2 to J/m%2 by multiplying by 3600
    if (debug) write(debug_unit,'(2(/t15,a))')'SUMMED HEAT FLUX','HOUR    HEAT FLUX     INTEGRATED SUM'
    h3: do h=1,last_conv_hr !cblhr1,last_conv_hr
        if (h==cblhr1) then
            smh(h)=sfc_data(d)%hflux(h)*3600._r8
        elseif (h > cblhr1 .and. sfc_data(d)%hflux(h) >= 0.0_r8) then
            smh(h)=sfc_data(d)%hflux(h)*3600._r8+smh(h-1)
        elseif (h > cblhr1 .and. sfc_data(d)%hflux(h) <= 0.0_r8) then
!           flux is downward, persist previous hour.
            smh(h)=smh(h-1)
        endif
        if (debug .and. h >= cblhr1)write(debug_unit,'(t16,i2.2,t21,g13.6,t38,g13.6)')h,sfc_data(d)%hflux(h),smh(h)
    enddo h3
    
!   intialize cumulative heat flux oldh
    oldh=0.0_r8
    
!   integrate  
    if (debug)write(debug_unit,'(/t15,a,1x,i2,a)')' MIXING HEIGHT CALCULATIONS EVERY ',inc,' MINUTES'
    k1: do kk=min_midnite(1)+inc,min_midnite(2),inc
!       interpolate time-integrated heat flux, SHEAT
!       check on IT2 is in the the hourly heat flux is upward at the end of the day
!       (as could happen in an urban environment)
        it1=kk/60
        it2=it1+1
        if (it1 == 0) it1=1
        if (it2 == 25) it2=24
        sheat=smh(it1)+((smh(it2)-smh(it1))/60.0_r8)*(real(kk,r8)-real(it1,r8)*60.0_r8)
       
        if (sheat < oldh) sheat=oldh
        oldh=sheat
        
!       start actual convective mixing height calculations for this 15 minute period
!       get density, if missing use the initial value of 1.2 assigned earlier in the subroutine
        kk60=kk/60
        if (rho(kk60,d) > 0.0_r8)then 
            density=rho(kk60,d)
        else
            density=1.2_r8
        endif

!       equation 22 AERMOD MFED page 20
        harea=(sheat/(density*cp))*(1.0_r8+2.0_r8*capa)
        
        if (debug) then
            write(debug_unit,'(t15,a,i4,a,i2.2)')'MINUTES PAST MIDNIGHT ',kk,' HOUR ',kk60
            write(debug_unit,'(/t16,a,/t16,1x,f5.3,t27,g13.6,t48,g13.6)')'DENSITY   INTEGRATED HEAT FLUX       HAREA',density,&
                sheat,harea
            write(debug_unit,'(/t15,a)')'LEVEL    HT        THETA_SUM (LEVEL)  THETA_SUM (LEVEL-1)'
        endif
        
        gotzi=.false.
        ilev=2
        do while (.not. gotzi .and. ilev <= nlevs)
            if (debug)write(debug_unit,'(t15,i5,t22,f9.3,t36,g13.6,t55,g13.6)')ilev,ht(ilev),theta_sum(ilev),theta_sum(ilev-1)
            if (harea < theta_sum(ilev)) then
!               compute CBL height for this 15 minute period
!               if there is no change in area under the potential temperature curve between the
!               level determined above and the level below, then use the lower level as the CBL
!               height; otherwise interpolate between levels
                if (theta_sum(ilev) == theta_sum(ilev-1)) then
                    pblht=ht(ilev-1)
                    if (debug)write(debug_unit,'(/t15,a5,1x,f9.3,1x,a,1x,i4)')'PBLHT',pblht,'= HT(ILEV-1) ILEV=',ilev
                else
!                   interpolate between sounding levels that straddle the convective mixing height
!                   based on work from Venkatram and Rayner
                    pblht = dsqrt(ht(ilev-1)**2+(harea-theta_sum(ilev-1)) *(ht(ilev)**2-ht(ilev-1)**2) /&
                        (theta_sum(ilev)-theta_sum(ilev-1)))
                    if (debug)write(debug_unit,'(/t15,a5,1x,f9.3,1x,a,1x,i4)')'PBLHT',pblht,&
                    'INTERPOLATED B/W ILEV-1 AND ILEV; ILEV=',ilev
                endif
                
                gotzi=.true.
            else
                ilev=ilev+1
            endif   
        enddo
!       cumulative heat flux exceeds area under potential temperature profile
        if (.not. gotzi .and. harea >= theta_sum(nlevs)) pblht=ht(nlevs)
        
!       if pblht is 0 then set to missing
        if (pblht == 0) pblht=-999._r8
        
        if (debug)write(debug_unit,'(/t15,a5,1x,f9.3)')'PBLHT',pblht
        
!       determine if the time is on the hour. if it is and the hour is convective
!       then set the CBL height to pblht if that hour doesn't already have a mixing height
        if (mod(kk,60) == 0) then
            if (cbl(kk60,d) .and. sfc_data(d)%zic(kk60) < 0.0_r8) then
                sfc_data(d)%zic(kk60)=pblht
                write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sfc_data(d)%sfcdate,'HR',kk60
!               post 21DRF if convective mixing height is 0, issue warning
                if (sfc_data(d)%zic(kk60) == 0.0_r8) write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W75',modnam,date_hr_str,&
                        'CONVECTIVE MIXING HEIGHT = 0 M'
                if (lextended) then
                    if (pblht > ht(nlevs-1))write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W77',modnam,'CBL HT',pblht,&
                    'FOR',date_hr_str,'EXCEEDS ORIGINAL SOUNDING TOP',ht(nlevs-1)
                endif
                if (sfc_data(d)%zic(kk60) > conv_max) then
                    sfc_data(d)%zic(kk60)=conv_max
                    write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I79',modnam,'CBL HT SET TO',conv_max,'FOR',date_hr_str
                endif
            endif
            if (debug)write(debug_unit,'(/t15,a,1x,i2.2,1x,f9.3)')'PBLHT FOR HOUR:',kk60,pblht
            if (sfc_data(d)%mol(kk60) < 0.0_r8 .and. dabs(sfc_data(d)%mol(kk60)-(-99999.0_r8)) > eps .and. rho(kk60,d) > 0.0_r8 &
                .and. sfc_data(d)%airtemp(kk60) > 0.0_r8 .and. sfc_data(d)%airtemp(kk60) < 900._r8 .and. sfc_data(d)%hflux(kk60) &
                > -900.0_r8 .and. dabs(sfc_data(d)%hflux(kk60) -777.0_r8) > eps .and. pblht > 0.0_r8 .and. .not. onwstar) then
                
!               calculate w* if not read in as onsite variable and non-missing
                if (.not. onwstar) sfc_data(d)%wstar(kk60)=wstar(d,kk60)
!               calculate vptg if not read in as onsite variable and non-missing
                if (.not. onvptg) sfc_data(d)%vptg(kk60)=vptg(d,kk60)
            endif 
        endif
        
    enddo k1
    
    deallocate(theta_sum)
    deallocate(theta_a)
    if (allocated(fill))deallocate(fill)

    if (debug)write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' END'
    

    return
    end subroutine conv_ht
!*********************************************************************************************************

    real(kind=r8) function wstar(d,h)
!=========================================================================================================
!   FUNCTION WSTAR
!   CALCULATE CONVECTIVE VELOCITY SCALE W*
!     
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC, CONV_HT)
!
!   INPUT ARGUMENTS
!   D:              DAY COUNTER
!   H:              HOUR COUNTER
!
!   OUTPUT
!     
!   WSTAR:       CONVECTIVE VELOCITY SCALE W*
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   h:              hour counter
!
!   Character variables
!   formstr:        formats for messages
!   debugform:      formats for debug messages
!   str1:           text string for debug messages
!   date_hr_str:	date and hour text string
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: g,msg_form    
    use file_units, only: msg_unit
    implicit none
    integer(kind=4), intent(in) :: d,h   
    character(len=60) :: formstr
    character(len=100) :: debugform
    character(len=50) :: str1
    character(len=20) :: date_hr_str
    character(len=10) :: modnam='WSTAR'
    
!   wstar debug format
    write(debugform,'(a)')'(/t18,a/t15,g13.6,1x,f6.1,1x,f7.5,1x,g13.6,1x,f7.5,1x,f9.4,1x,f7.3)'
    
!   format for message file
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,a20)' 
    
    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sfc_data(d)%sfcdate,'HR',h
    
    wstar=(g*sfc_data(d)%hflux(h)*sfc_data(d)%zic(h)/(rho(h,d)*cp*sfc_data(d)%airtemp(h)))**(0.333_r8)
    
    str1='WSTAR        Cp      G      HEAT FLUX   DENSITY   ZIc     TEMP'
    if (debug)write(debug_unit,debugform)trim(adjustl(str1)),wstar,cp,g,sfc_data(d)%hflux(h),rho(h,d),sfc_data(d)%zic(h),&
        sfc_data(d)%airtemp(h)

    if (wstar > 0.0_r8 .and. wstar < 0.001_r8) then
        wstar=0.001_r8
        write(msg_unit,formstr)adjustl(pathid(ipath)),'W78',modnam,'W* < 0.001; RESET TO 0.001 FOR',date_hr_str
        if (debug)write(debug_unit,'(/t15,a)')'W* < 0.001; RESET TO 0.001'
    endif
    
    return
    end function wstar
!*********************************************************************************************************

    real(kind=r8) function vptg(d,h)
!=========================================================================================================
!   FUNCTION VPTG
!   CALCULATE POTENTIAL TEMPERATURE LAPSE RATE ABOVE MIXING HEIGHT
!     
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC, CONV_HT)
!
!   INPUT ARGUMENTS
!   D:              DAY COUNTER
!   H:              HOUR COUNTER
!
!   OUTPUT
!     
!   VPTG:       POTENTIAL TEMPERATURE LAPSE RATE ABOVE MIXING HEIGHT
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   h:              hour counter
!   ilev1:          first level below mixing height ZI
!   ilev2:          first level above mixing height ZI
!   ilev3:          first level below mixing height + 500 m (or sounding top < 500 m)
!   ilev4:          first level above mixing height + 500 m (or sounding top < 500 m)
!
!   Real variables
!   zi:             mixing height
!   sound_top:      top of unextended sounding
!   delz:           minimum of 500 m or (sounding top - 0.1 m)
!   z_delz:         mixing height + delz
!   theta_zi:       potential temperature at mixing height
!   theta_dz:       potential temperature at z_delz        
!   dtheta_dz:      potential temperature lapse rate between mixing height and z_delz
!
!   Logical variables
!   levs_found1:    ilev1 and ilev2 set to non-zero
!   levs_found2:    ilev3 and ilev4 set to non-zero
!
!   Character variables
!   debugform:      formats for debug messages
!   modnam:         Subroutine name
!=========================================================================================================
!    use main1, only: eps    
    implicit none
    integer(kind=4), intent(in) :: d,h
    integer(kind=4) :: ilev,ilev1,ilev2,ilev3,ilev4
    real(kind=r8) :: zi,sound_top,delz,theta_zi,theta_dz,z_delz,dtheta_dz
    logical :: levs_found1,levs_found2
    character(len=120) :: debugform(2)
    character(len=10) :: modnam='VPTG'
    
!   debug formats
!   1.   first level found
    write(debugform(1),'(a)')'(/t15,a/t21,a/t41,a/t16,a,t27,a,t36,a,t47,a/t15,f7.1,2x,f9.3,t35,f7.1,2x,f9.3//t15,a,t28,f9.3)'
    
!   2.  second level found
    write(debugform(2),'(2(a))')'(/t15,a,1x,f5.1/t21,a,t41,a/t16,a,t27,a,t36,a,t47,a/t15,f7.1,2x,f9.3,t35,f7.1,2x,',&
        'f9.3//t15,a,t28,f9.3)'
        
    if (debug)write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' START'            
!   use max of convective or mechanical mixing heights
    zi=dmax1(sfc_data(d)%zic(h),sfc_data(d)%zim(h)) 
    
    if (debug)write(debug_unit,'(/t15,2(a,f7.1),a)')'USE MAX OF ',sfc_data(d)%zic(h),' AND ',sfc_data(d)%zim(h),&
        ' FOR MIXING HEIGHT'
    
!   check location of mixing height relative to top of un-extended sounding
    
!   set value for sound_top
    if (lextended) then
        sound_top=ht(nlevs-1)
    else
        sound_top=ht(nlevs)
    endif
   
    if (debug)write(debug_unit,'(t15,a,f7.1)')'UN-EXTENDED SOUNDING TOP: ',sound_top
    
!   if difference is > 250 m then can proceed
    if ((sound_top-zi) > 250._r8) then
        delz=dmin1(500._r8,(sound_top-0.1_r8))
        if (debug)then
            write(debug_unit,'(/t15,a,f7.1)')'DIFFERENCE BETWEEN ZI AND SOUNDING TOP ',delz
            if (sound_top-0.1_r8 > 500._r8)write(debug_unit,'(t15,a)')'DIFFERENCE SET TO 500 M'
        endif
        
!       locate sounding levels immediately below and above zi; there is is always
!       a level the surface, so there is at least one level below zi
        levs_found1=.false.
        ilev=1
        ilev1=0
        ilev2=0
    
        do while (.not. levs_found1 .and. ilev <= nlevs)
            if (zi > ht(ilev)) then !found first level
                ilev1=ilev
            else !found second level
                ilev2=ilev
                if (ilev1 > 0) levs_found1=.true.
            endif
            ilev=ilev+1
        enddo
        
        theta_zi=0.0_r8
        theta_dz=0.0_r8
        if (levs_found1) then
            theta_zi=((zi-ht(ilev1))/(ht(ilev2)-ht(ilev1)))*(theta(ilev2)-theta(ilev1))+theta(ilev1) 
            if (debug)write(debug_unit,debugform(1))'HEIGHTS/THETA BELOW AND ABOVE ZI','LEVEL 1','LEVEL 2','HEIGHT','THETA',&
                'HEIGHT','THETA',ht(ilev1),theta(ilev1),ht(ilev2),theta(ilev2),'THETA AT ZI:',theta_zi
            
!           locate sounding levels immediately above and below zi+delz
            levs_found2=.false.
            ilev=1
            z_delz=zi+delz
            ilev3=0
            ilev4=0
            do while(.not. levs_found2 .and. ilev <= nlevs)
                if (z_delz > ht(ilev)) then
                    ilev3=ilev
                else
                    ilev4=ilev
                    levs_found2=.true.
                endif
                ilev=ilev+1
            enddo
            if (levs_found2) then
                theta_dz=((z_delz-ht(ilev3))/(ht(ilev4)-ht(ilev3)))*(theta(ilev4)-theta(ilev3))+theta(ilev3) 
                if (debug)write(debug_unit,debugform(2))'HEIGHTS/THETA BELOW AND ABOVE ZI +',delz,'LEVEL 1','LEVEL 2','HEIGHT',&
                    'THETA','HEIGHT','THETA',ht(ilev3),theta(ilev3),ht(ilev4),theta(ilev4),'THETA AT ZI:',theta_dz
                
            else
                theta_dz=0.0_r8
            endif
            dtheta_dz=(theta_dz-theta_zi)/(z_delz-zi)
!           take max of dtheta_dz and 0.005
            vptg=dmax1(dtheta_dz,0.005_r8)
            if (debug)write(debug_unit,'(/t15,a,g13.6,a)')'VPTG SET TO MAX OF ',dtheta_dz,' AND 0.005'
        else
            vptg=0.005_r8
        endif
    else
        vptg=0.005_r8
    endif
    if (debug)write(debug_unit,'(/t10,2(a))')trim(adjustl(modnam)),' END'
    
    return
    end function vptg
!*********************************************************************************************************
    
    subroutine profile(d,h)
!=========================================================================================================
!   SUBROUTINE PROFILE
!   ASSIGN VALUES TO PFL_DATA 
!     
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS
!   D:              DAY COUNTER
!   H:              HOUR COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   h:              hour counter
!   ilev:           level loop counter
!
!   Logical variables
!   gotos:          have at least 1 mon-missing variable at some level
!
!   Character variables
!   formstr:        format for messages
!   date_hr_str:	text string for date and hour
!   modnam:         Subroutine name
!=========================================================================================================
    !use main1, only: eps    
    use file_units, only: msg_unit
    use main1, only: lpath,msg_form
    use surface, only: nws_hgt
    use onsite, only: wind_vars,temp_var,sa_var,sw_var
    implicit none
    integer(kind=4), intent(in) :: d,h
    integer(kind=4) :: ilev
    logical gotos
    character(len=60) :: formstr
    character(len=20) :: date_hr_str
    character(len=10) :: modnam='PROFILE' 
    
    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sfc_data(d)%sfcdate,'HR',h
    
!   format for message
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a20)'
    
    gotos=.false.
    if (os_obs(h,d)) then
!       set temperatures for the profile
!       if not using NWS winds, then set the pfl data array to the osdata1 temperature values
!       if using NWS winds, then leave the temperatures above the temperature height as missing and
!       set the at the height to the temperature from osdata1.
!       note that tempsrc could be 0 but there could be temperatures above 100 m that are not missing
        !if ((tempsrc(h,d) == 1 .or. tempsrc(h,d) == 3)) then
        if ((tempsrc(h,d) == 1 .or. tempsrc(h,d) == 3) .or. (tempsrc(h,d) == 0 .and. osvars(temp_var)%lread)) then
    l1:     do ilev=1,pfl_levels
                if (windsrc(h,d) /=2) then
                    pfl_data(h,d)%airtemp(ilev)=osdata1(osvars(temp_var)%readvar,ilev,h,ios(d))
                else
                    if (dabs(osdata1(1,ilev,h,ios(d)) - sfc_data(d)%temp_ht(h)) < eps) pfl_data(h,d)%airtemp(ilev)=&
                    osdata1(osvars(temp_var)%readvar,ilev,h,ios(d))
                    pfl_data(h,d)%sigma_a(ilev)=99.0_r8
                    pfl_data(h,d)%sigma_w(ilev)=99.0_r8
                endif
            enddo l1
        endif
        ilev=1
!       if any one level has at least 1 non-missing variable then whole profile is used
        do while (ilev <= pfl_levels .and. .not. gotos)
            if (dabs(pfl_data(h,d)%speed(ilev)-(osvars(wind_vars(2))%missval*osvars(wind_vars(2))%conv)) > eps .or. &
                dabs(pfl_data(h,d)%dir(ilev)-(osvars(wind_vars(1))%missval*osvars(wind_vars(1))%conv)) > eps .or. &
                dabs(pfl_data(h,d)%airtemp(ilev)-(osvars(temp_var)%missval*osvars(temp_var)%conv)) > eps .or. &
                dabs(pfl_data(h,d)%sigma_a(ilev)-(osvars(sa_var)%missval*osvars(sa_var)%conv)) > eps .or. &
                dabs(pfl_data(h,d)%sigma_w(ilev)-(osvars(sw_var)%missval*osvars(sw_var)%conv)) > eps) gotos=.true.
                ilev=ilev+1
        enddo
    endif

!   only substitute if there is onsite data being read and NWS data as well.  If only NWS data
!   then the profile data has already been set.
    if (.not. gotos .and. lpath(4)) then
        if (lactions(3)) then ! .and. nws_obs(h,d)) then
!           reset the profile data to the NWS obs and one level
!           if all levels missing then the surface obs are from the NWS data and can
!           be used here.
            pfl_data(h,d)%speed(1)=sfc_data(d)%wspd(h)
            pfl_data(h,d)%dir(1)=sfc_data(d)%wdir(h)
            if (sfc_data(d)%airtemp(h) < 900._r8) pfl_data(h,d)%airtemp(1)=sfc_data(d)%airtemp(h)-273.15_r8
            pfl_data(h,d)%ht(1)=nws_hgt
            pfl_nlevels(h,d)=1
            if (lpath(4)) write(msg_unit,formstr)adjustl(pathid(4)),'I90',modnam,'NWS DATA SUBSTITUTED FOR',&
            trim(adjustl(pathid(4))),'DATA FOR',date_hr_str
        endif
    endif
    
    return
    end subroutine profile
!*********************************************************************************************************
    
    subroutine sfc_header
!=========================================================================================================
!   SUBROUTINE PROFILE
!   WRITE HEADER TO SURFACE FILE 
!     
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   h:              hour counter
!   ilev:           level loop counter
!
!   Character variables
!   alat:           character string of latitude
!   alon:           character string of longitude
!   hdrstr:         header text
!   substr:         temperature/cloud substitution string
!   threshstr:      threshold string
!   bulkrnstr:      text string denoting bulk richardson scheme used
!   progstr:        text string denoting mmif version
!   adjustustar:	text string denoting adjusted u* used
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: versn,lkey,actions,lpath
    use file_units, only: sfc_out_unit
    use upperair, only: upid
    use surface, only: sfid,asos_thresh
    use onsite, only: osid
    implicit none
    character(len=9) :: alat,alon
    character(len=50) :: hdrform='(2(2x,a8),8x,3(a,a8),t85,a,1x,a5)'
    character(len=98) :: hdrstr
    character(len=17) :: substr
    character(len=24) :: threshstr
    character(len=11) :: bulkrnstr
    character(len=25) :: progstr
    character(len=6) :: adjustarstr
    character(len=10) :: modnam='SFC_HEADER' 
    
!   convert lat/lon to character strings
    if (pbllat < 0.0_r8) then
        write(alat,'(f7.3,a1)')dabs(pbllat),'S'
    else
        write(alat,'(f7.3,a1)')pbllat,'N'
    endif
    
    if (pbllon < 0.0_r8) then
        write(alon,'(f8.3,a1)')dabs(pbllon),'W'
    else
        write(alon,'(f8.3,a1)')pbllon,'E'
    endif
    
    write(hdrstr,hdrform)alat,alon,'  UA_ID: ',upid,'  SF_ID: ',sfid,'  OS_ID: ',osid,'VERSION:',versn
    
!   cloud and temperature substitution string
    substr=''
    if (subcloud) then
        if (subtemp) then
            substr='CCVR_Sub TEMP_Sub'
        else
            substr='CCVR_Sub'
        endif
    else
        if (subtemp) substr='TEMP_Sub'
    endif    
         
!   ASOS threshold
    threshstr=''
    if (lkey(35)) write(threshstr,'(a,f5.2,a)')'THRESH_1MIN = ',asos_thresh,' m/s;'
        
!   adjust u*
    adjustarstr=''
    if (adjustar) adjustarstr=trim(adjustl(actions(7)))
    
!   PROGNOSTIC data
    progstr=''

    if (lpath(5))write(progstr,'(a,1x,a)')trim(adjustl(pathid(5))),trim(adjustl(mmif_versn))
    
!   Bulk-Richardson
    bulkrnstr=''
    if (lactions(4)) then
        if (lpath(5)) then
            write(bulkrnstr,'(a7,a4)')'BULKRN/',trim(adjustl(pathid(5)))
        else
            write(bulkrnstr,'(a6)')'BULKRN'
        endif
    endif
    
!   write header record to file
    write(sfc_out_unit,'(a98,5(1x,a))')hdrstr,trim(adjustl(threshstr)),trim(adjustl(adjustarstr)),trim(adjustl(bulkrnstr)),&
        trim(adjustl(substr)),trim(adjustl(progstr))
    
    return
    end subroutine sfc_header
!*********************************************************************************************************
    
    subroutine writefiles(d)
!=========================================================================================================
!   SUBROUTINE PROFILE
!   WRITE DATA TO SURFACE AND PROFILE FILES 
!     
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS
!   D:              DAY COUNTER
!
!   Variable definitions
!      
!   Integer variables
!   d:              day counter
!   h:              hour counter
!   ilev:           level loop counter
!   iyear:          2-digit year
!   imonth:         2-digit month
!   iday:           day of month
!   itop:           top of profile indicator
!
!   Character variables
!   sfcform:        format for surface file data
!   pflform:        format for profile file data
!   modnam:         Subroutine name
!=========================================================================================================
    use file_units, only: sfc_out_unit,pfl_out_unit
    use main1, only: eps
    implicit none
    integer(kind=4), intent(in) :: d
    integer(kind=4) :: h,ilev,iyear,imonth,iday,itop
    character(len=8) :: adate
    character(len=170) :: sfcform
    character(len=70) pflform
    character(len=10) :: modnam='WRITEFILES' 
    
    write(sfcform,'(2(a))')'(3(i2,1x),i3,1x,i2,1x,f6.1,1x,f6.3,1x,f6.3,1x,f6.3,1x,2(f5.0,1x),f8.1,1x,f7.4,1x,f6.2,1x,f6.2,1x,',&
        'f7.2,1x,f6.1,3(1x,f6.1),1x,i5,1x,f6.2,2(1x,f6.0),1x,i5,1x,a7,1x,a9)'

    write(pflform,'(a)')'(4(i2,1x),f7.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,1x,f8.2,1x,f8.2)'

    write(adate,'(i8)')sfc_data(d)%sfcdate
!   get year, month, day; note that year is 2 digit year
    read(adate,'(2x,3(i2.2))')iyear,imonth,iday
    
!   write surface and profile data
    h1: do h=1,24
!       write surface file
        write(sfc_out_unit,sfcform)iyear,imonth,iday,sfc_data(d)%jday,h,sfc_data(d)%hflux(h),sfc_data(d)%ustar(h),&
        sfc_data(d)%wstar(h),sfc_data(d)%vptg(h),sfc_data(d)%zic(h),sfc_data(d)%zim(h),sfc_data(d)%mol(h),sfc_data(d)%zo(h),&
        sfc_data(d)%bowen(h),sfc_data(d)%albedo(h),sfc_data(d)%wspd(h),sfc_data(d)%wdir(h),sfc_data(d)%wind_ht(h),&
        sfc_data(d)%airtemp(h),sfc_data(d)%temp_ht(h),sfc_data(d)%ipcode(h),sfc_data(d)%pamt(h),sfc_data(d)%rh(h),&
        sfc_data(d)%pres(h),sfc_data(d)%ccvr(h),adjustl(sfc_data(d)%windsrcflag(h)),adjustl(sfc_data(d)%subflag(h))
    l1: do ilev=1,pfl_nlevels(h,d)
            if (ilev == pfl_nlevels(h,d)) then
                itop=1
            else
                itop=0
            endif

            write(pfl_out_unit,pflform)iyear,imonth,iday,h,pfl_data(h,d)%ht(ilev),itop,pfl_data(h,d)%dir(ilev),&
                pfl_data(h,d)%speed(ilev),pfl_data(h,d)%airtemp(ilev),pfl_data(h,d)%sigma_a(ilev),pfl_data(h,d)%sigma_w(ilev)
        enddo l1
    enddo h1
    
    return

    end subroutine writefiles
!*********************************************************************************************************
    end module pbl

