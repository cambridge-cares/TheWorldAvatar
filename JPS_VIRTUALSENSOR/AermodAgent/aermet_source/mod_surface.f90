    module surface
!=========================================================================================================
!   MODULE SURFACE
!   THIS MODULE CONTAINS COMMON VARIABLES AND SUBROUTINES NEEDED THROUGOUT AERMET PROCESSING 
!   TO PROCESS SURFACE (NWS) DATA
!
!   MODIFIED MAY 5, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   USED BY:        AERMET, MODULES READ_INPUT, REPORTS
!
!   Variable definitions
!
!   Integer variables
!   sfstart:        surface extraction start date (YYYMMDD)
!                   start hour assumed to be 1
!   sfend:          surface extract end date (YYYYMMDD)
!                   end hour assumed to be 24
!   sfstart1min:    AERMINUTE data start date (YYYYMMDD)
!                   start hour assumed to be 1
!   sfend1min:      AERMINUTE data end date (YYYMMDD)
!                   start hour assumed to be 24
!   sonicdate:      AERMINUTE IFW commission date
!   sfgmt2lst:      conversion from Greenwich Mean Time (GMT) to
!                   local standard time (LST).
!                   example:  GMT to eastern time zone (EST) is 5
!                   do not have to account for daylight savings time
!   iasos:          number of station in the ASOS list (asoscommdates)
!   sfdates:        2x3 array of start month, day, year and end month,
!                   day and year.  1st dim is the start (1) or end (2)
!                   2nd dim is year (1), month (2), and day (3)
!   nsfdays:        number of days in the surface data period
!   nsf1mindays:    number of days in the AERMINUTE data period
!   isfform:        DATA file form index for sfformats reporting
!   nsfvars:        number of surface variables in master list
!   nsfvars_keep:   number of surface variables to write to EXTRACT file
!   nsfvars1:       number of surface variables to write to QAOUT file
!   nsfformats:     number of surface formats
!   nsfkeys:        number of SURFACE keywords    
!   nsf_audit_vars: number of variables to audit; always at least 3 (TMPD, WDIR, WSPD)
!   nsf_extra:      number of extra variables to output to QAOUT. these are
!                   the non-standard variables that will be audited
!                   in derived data type sound_info
!   nsfc:           10-element array denoting the total number of obs read from
!                   file:

!                   1:  total obs read in data window
!                   2:  valid observations (observations written to file)
!                   3:  special obs used
!                   4:  regular obs used
!                   5:  multiple observations
!                   6:  special obs not used
!                   7:  regular obs not used
!                   8: number of hours overwritten (multiple valid obs)
!                   9: non-supported obs skipped
!                   10: obs outside 30 minute window but in data window
!                   11: obs flagged as calm
!                   12: obs flagged as variable
!
!   nsfc1:          temporary array of number of obs per day;
!                   used to fill in nsfcobs in derived data type sfc_info  
!   isfid:          integer value of sfid if integer; otherwise = 99999
!   sf_windstats:   3-element array with counts of
!                   1: calm winds
!                   2: wind direction > 0 and wind speed = 0
!                   3: wind direction = 0 and wind speed > 0
!   sf_tempstat:    # of hours temperature < dewpoint
!   precip_wx:      2-element array with counts of:
!                   1: precip without weather code
!                   2: no precip with weather code
!   nsplit:         Number of splits for auditing; if an audited
!                   variable is a concatenated variable, then nsplit=2
!                   otherwise nsplit=1
!   iwban:          WBAN read from file
!   nwarnlim:       maximum number of times a warning message will be written
!
!   Real variables
!   sflat:          latitude of surface station
!   sflon:          longitude of surface station
!   sfelev:         surface station elevation
!   sf_user_elev:   user entered elevation
!   asos_thresh:    wind speed used for ASOS data (read in pbl_path in mod_pbl)
!   sfdata:         3-D surface air data array read from surface 
!                   file in stage 1.
!                   Dimensions:
!                   1st:  number of surface variables (22)
!                   2nd:  number of obs for the day 
!                       (max number of obs across all days)
!                   3rd:  number of days in period    
!   sfdata1:        temporary 3-D surface air data array read from surface 
!                   1st:  number of surface variables (22)
!                   2nd:  number of obs for the day 
!                       (max number of obs across all days)
!                   3rd:  number of days in period    
!   missvals:       array of missing values for auditing, 
!                   based on splitting of concatenated variables; 
!   lowbound:       lower BC values based on splitting of concatenated variables
!   upbound:        upper BC values based on splitting of concatenated variables
!   nws_hgt:        NWS instrument height (currently only wind)
!     
!   Logical variables
!   lsfkeywords:      array denoting if keywords found
!   note ikey below is the index of the keyword in the keywrd array in module main1
!   1:              denotes that DATA keyword (ikey=4) found for surface data. keyword
!                   is mandatory
!   2:              denotes NO_MISSING keyword found (ikey=5)
!   3:              denotes EXTRACT keyword (ikey=7) found. making optional for now, not used
!   4:              denotes that QAOUT keyword (ikey=8) found.  If processing stages 1
!                   and 2 in the same run, this keyword is optional.
!                   If processing stage 1 only, this is mandatory.
!   5:              denotes XDATES keyword found (ikey=9)
!   6:              denotes LOCATION keyword found (ikey=10)
!   7:              denotes RANGE keyword found (ikey=12)
!   8:              denotes AUDIT keyword found (ikey=13)
!   9:              denotes ASOS1MIN keyword found (ikey=32)
!   lbadsfc:        logical variable denoting issue with processing surface data   
!   lasos:          denotes station is ASOS station
!   lgmt2lst:       variable denoting if GMT to LST found (true if so)
!   user_elev:      denotes if user entered elevation on LOCATION keyword (true if entered)
!   lsfelev:        denotes elevation read from EXTRACT file
!   lsfcobs:        temporary array denoting if a day has any observations; used to
!                   fill sobs in derived data type sfc_info
!   writeext:       logical variable denoting if AERMET should write to (true) or
!                   read from (false) EXTRACT file
!   asoshr:         temporary array denoting which hours are ASOS hours if ASOS station
!   nomisshr:       tempoary array denoting if an hour is missing (true if not missing, false if missing)
!   write_asos:     logical variable denoting to write ASOS message
!   asos_switch:    2-element logical variable denoting that data period covered pre and post ASOS for station
!                   element 1 is potential pre-ASOS and 2=post ASOS
!   sfstage1:       denotes that stage 1 being processed for surface data
!
!   Character variables
!   sfid:           surface station on the LOCATION line (e.g. WBAN number)
!   sf1minid:       surface station WBAN number in AERMINUTE header
!   qaout_vars:     variables to write out to QAOUT file
!   ext_form:       extraction message formats
!
!   data type svars (sfvars)
!   varname:        variable name, i.e. PRES=pressure; see subroutine surf_init for definitions
!   laudit:         logical variable denoting if variable will be audited. Initial value is 
!                   false, no audit, except for dry bulb temperature (TMPD), wind speed (WDIR),
!                   and wind speed (WSPD) which are automatically tracked
!   lmod:           logical variable denoting if variable has already been listed with the
!                   RANGE keyword.  If listed more than once, user is warned and latest
!                   values used.
!   lnomiss:        logical variable denoting to skip (true) reporting hourly missing values
!                   of variable to MESSAGE file.  False means to report the value 
!                   (initial default value) when missing in MESSAGE file.
!   lincbound:      logical variable denoting if < or <= used for comparing to bounds.
!                   lincbound=.true. means to not include the upper and lower bounds in the range
!                   i.e. logic is lower < value < upper
!                   lincbound=.false. means to include the upper and lower bounds in the range
!                   i.e. logic is lower <= value <= upper
!   misscount:      integer value of # of missing hours
!   missval:        default missing value
!   lowbound:       lower acceptable bound
!   upbound:        upper acceptable bound
!
!   data type asosdata (asoscommdates)
!   city:           ASOS station city
!   location:       ASOS station (airport) name
!   state:          2-character state abbreviation
!   iwban:          WBAN number
!   intcall:        international call sign
!   usacall:        US call sign
!   ddlat:          station latitude
!   ddlon:          station longitude
!   commisdate:     commission date (YYYYMMDD)
!   pub:            
!   sitetype:       site type, FAA, etc.
!   anem_feet:      anemometer height in feet
!   anem_meters:    anemometer height in meters
!
!   data type sformats (sfformats)
!   sformat:        array of valid surface formats
!   lsfform:        array of logical variable denoting 
!                   which format used 
!   validstart:     valid start date of format
!   validend:       valid end date of format
!=========================================================================================================
!   ipath, lbad, r8, and pathid used in a majority of subroutines, keep here.
    use main1, only: ipath,lbad,r8,pathid,msg_form
    
!   msg_unit used in a majority of subroutines, keep here
    use file_units, only: msg_unit

    implicit none

    integer(kind=8) :: sfstart=0
    integer(kind=8) :: sfend=0
    integer(kind=8) :: sfstart1min=0
    integer(kind=8) :: sfend1min=0
    integer(kind=8) :: sonicdate=0
    integer(kind=4) :: sfgmt2lst=-9
    integer(kind=4) :: iasos=0
    integer(kind=4) :: sfdates(2,3)=0
    integer(kind=4) :: nsfdays=0
    integer(kind=4) :: nsf1mindays=0
    integer(kind=4) :: isfform=0
    integer(kind=4) :: nsfc(12)=0 
    integer(kind=4) :: nsfvars1=0
    integer(kind=4) :: nsf_audit_vars=0
    integer(kind=4) :: nsf_extra=0
    integer(kind=4) :: irec=0
    integer(kind=4) :: sf_windstats(3)=0
    integer(kind=4) :: sf_tempstat=0
    integer(kind=4) :: precip_wx(2)=0
    integer(kind=4), parameter :: nsfvars=22
    integer(kind=4), parameter :: nsfvars_keep=11
    integer(kind=4), parameter :: nsfformats=9
    integer(kind=4), parameter :: nsfkeys=9
    integer(kind=4), parameter :: nasos=886
    
    integer(kind=4), allocatable, dimension(:) :: sf_audit_index
    integer(kind=4), allocatable, dimension(:,:,:) :: sf_audit_counts
    
    integer(kind=4), allocatable, dimension(:) :: nsfc1
    integer(kind=4) :: nsplit=1
    
!   common to all read subroutines
    integer(kind=4) :: iwban=0
    integer(kind=4) :: isfid=0
    integer(kind=4), parameter :: nwarnlim=100
    
    real(kind=r8) :: sflat=0.0_r8
    real(kind=r8) :: sflon=0.0_r8
    real(kind=r8) :: sfelev=0.0_r8
    real(kind=r8) :: sf_user_elev=0.0_r8
    real(kind=r8) :: asos_thresh=0.0_r8
    real(kind=r8) :: nws_hgt=-9.0
    real(kind=r8), allocatable, dimension(:,:,:) :: sfdata
    real(kind=r8), allocatable, dimension(:,:,:) :: sfdata1
    real(kind=r8), allocatable, dimension(:,:) :: missvals
    real(kind=r8), allocatable, dimension(:,:) :: lowbound
    real(kind=r8), allocatable, dimension(:,:) :: upbound
    
    logical :: lsfkeywords(nsfkeys)=.false.
    logical :: lbadsfc=.false.
    logical :: lasos=.false.
    logical :: lgmt2lst=.false.
    logical :: user_elev=.false.
    logical :: lsfelev=.false.
    logical :: writeext=.false.
    logical :: write_asos=.false.
    logical :: switch_asos(2)=.false.
    logical :: miss_precip,miss_temp,miss_dir,miss_wind,writedat
    logical :: sfstage1=.false.
    logical, allocatable, dimension(:) :: lsfcobs
    logical, allocatable, dimension(:,:) :: asoshr
    logical, allocatable, dimension(:,:) :: nomisshr
    
    character(len=8) :: sfid=''
    character(len=8) :: sf1minid='-9'
    character(len=60) :: ext_form(10)
    character(len=4), allocatable, dimension(:) :: qaout_vars
    
    type svars
        character(len=4) varname
        logical laudit,lmod,lnomiss,lincbound
        integer(kind=4) missval,lowbound,upbound
        real(kind=r8) :: conv
    end type svars
      
    type(svars) :: sfvars(nsfvars)
      
    type asosdata
        character (len=40) :: city
        character (len=60) :: location
        character (len=2)  :: state
        integer(kind=4)            :: iwban
        character (len=4)  :: intcall
        character (len=3)  :: usacall
        real(kind=r8) :: ddlat
        real(kind=r8) :: ddlon
        integer (kind=8)  :: commisdate
        character (len=3)  :: pub
        character (len=3)  :: sitetype
        real(kind=r8)       :: anem_feet
        real(kind=r8)       :: anem_meters
    end type asosdata
      
    type (asosdata), dimension(nasos) :: asoscommdates
      
    type sformats
        character(len=10) :: sformat
        logical lsfform
        integer(kind=8) :: validstart,validend
    end type sformats
      
    type(sformats) :: sfformats(nsfformats)

    type sfinfo
        integer(kind=8) :: sfcdate
        integer(kind=4) :: nsfcobs
        logical :: asos_hr(24),have_obs(24)
        logical sobs
    end type sfinfo
    
    type(sfinfo), dimension(:),allocatable:: sfc_info
    
    type sf1mininfo
        integer(kind=8) :: sf1mindate
        logical :: ifwhr(24)
        real(kind=r8) :: wdir(24),wspd(24)
    end type sf1mininfo
  
    type(sf1mininfo),dimension(:),allocatable :: one_mindat

    
    contains 
!*********************************************************************************************************
      
    subroutine sfc_init
!=========================================================================================================
!   SUBROUTINE SFC_INIT
!	THIS SUBROUTINE INITIALIZES THE DATA TYPE SFVARS AND ASOS station information
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (SURF_PATH)
!
!   note missing, lower and upper bounds scaled
!   by multipliers, e.g. pressure is milibars*10, not milibars
!   1012 mb=10120
!   with exception of name, all values can be reset by user
!   using NO_MISSING for lnomiss, and RANGE for less than, missval, lowbound, and upbound
!   note, wind speed, wind direction, and temperature are automatically audited so the laudit
!   value is true for those variables.
!=========================================================================================================
!   initialize surface variables
!   note that conversions are not used for TSKC, ALC1-ALC6, PWVC, PWTH, and ASKY.  Conversion set to 1 for 
!   those variables
!   variables 1-11 are core variables to output to EXTRACT and array
!   variables 12-22 are only output to QUAOUT file if audited
    
!                   name    laudit lmod lnomis lincbound missval lowbound upbound conv
    sfvars(1)=svars('PRCP',.false.,.false.,.false.,.true.,-9,0,25400,0.01_r8)!precipitation amount
    sfvars(2)=svars('SLVP',.false.,.false.,.false.,.false.,99999,9000,10999,0.1_r8) !sea level pressure
    sfvars(3)=svars('PRES',.false.,.false.,.false.,.false.,99999,9000,10999,0.1_r8) !station pressure
    sfvars(4)=svars('TSKC',.false.,.false.,.false.,.true.,9999,0,1010,1.0_r8) !total/opaque sky cover
    sfvars(5)=svars('PWTH',.false.,.false.,.false.,.true.,9999,0,9800,1.0_r8) !precipitation type
    sfvars(6)=svars('ASKY',.false.,.false.,.false.,.true.,99,0,10,1.0_r8) !ASOS sky condition
    sfvars(7)=svars('TMPD',.true.,.false.,.false.,.false.,999,-300,400,0.1_r8) !dry bulb temperature
    sfvars(8)=svars('DPTP',.false.,.false.,.false.,.false.,999,-650,350,0.1_r8) !dew-point temperature
    sfvars(9)=svars('RHUM',.false.,.false.,.false.,.true.,999,0,100,1.0_r8) !relative humidity
    sfvars(10)=svars('WDIR',.true.,.false.,.false.,.true.,999,0,36,10.0_r8) !wind direction
    sfvars(11)=svars('WSPD',.true.,.false.,.false.,.true.,999,0,500,0.1_r8) !wind speed
    sfvars(12)=svars('CLHT',.false.,.false.,.false.,.true.,999,0,300,1.0_r8) !ceiling height
    sfvars(13)=svars('ALC1',.false.,.false.,.false.,.true.,09999,0,07300,1.0_r8)!sky condition/height, level 1
    sfvars(14)=svars('ALC2',.false.,.false.,.false.,.true.,09999,0,07300,1.0_r8)!sky condition/height, level 2
    sfvars(15)=svars('ALC3',.false.,.false.,.false.,.true.,09999,0,07300,1.0_r8)!sky condition/height, level 3
    sfvars(16)=svars('ALC4',.false.,.false.,.false.,.true.,09999,0,07850,1.0_r8)!sky condition/height, level 4
    sfvars(17)=svars('ALC5',.false.,.false.,.false.,.true.,09999,0,07850,1.0_r8)!sky condition/height, level 5
    sfvars(18)=svars('ALC6',.false.,.false.,.false.,.true.,09999,0,07850,1.0_r8)!sky condition/height, level 6
    sfvars(19)=svars('PWVC',.false.,.false.,.false.,.true.,9999,0,9800,1.0_r8) !present weather
    sfvars(20)=svars('ACHT',.false.,.false.,.false.,.true.,999,0,888,1.0_r8) !ASOS ceiling
    sfvars(21)=svars('HZVS',.false.,.false.,.false.,.true.,99999,0,1640,1.0_r8) !horizontal visibility
    sfvars(22)=svars('TMPW',.false.,.false.,.false.,.false.,999,-650,350,0.1_r8) !wet bulb temperature
    
    asoscommdates(1) = asosdata("ANCHORAGE","LAKE HOOD SEAPLANE BASE","AK",26491,"PALH","LHD",61.17_r8,-149.96_r8,19980122,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(2) = asosdata("ANCHORAGE","MERRILL FIELD AIRPORT","AK",26409,"PAMR","MRI",61.21_r8,-149.85_r8,19971015,"NO", &
        "FAA",16.00_r8,4.87_r8)
    asoscommdates(3) = asosdata("ANCHORAGE","TED STEVENS ANCHORAGE INTERNATIONAL AIRPORT","AK",26451,"PANC","ANC",61.17_r8, &
        -149.99_r8,19980601,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(4) = asosdata("ANNETTE ","ANNETTE ISLAND AIRPORT","AK",25308,"PANT","ANN",55.04_r8,-131.57_r8,19960901,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(5) = asosdata("BARROW","WILEY POST - WILL ROGERS MEMORIAL AIPRORT","AK",27502,"PABR","BRW",71.28_r8,-156.76_r8, &
        19980601,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(6) = asosdata("BETHEL","BETHEL AIRPORT","AK",26615,"PABE","BET",60.78_r8,-161.82_r8,19981101,"YES","NWS", &
        26.00_r8,7.92_r8)
    asoscommdates(7) = asosdata("BETTLES","BETTLES AIRPORT","AK",26533,"PABT","BTT",66.91_r8,-151.5_r8,19991119,"YES","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(8) = asosdata("COLD BAY","COLD BAY AIRPORT","AK",25624,"PACD","CDB",55.2_r8,-162.72_r8,19980701,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(9) = asosdata("CORDOVA","MERLE K.(MUDHOLE)SMITH AIRPORT","AK",26410,"PACV","CDV",60.48_r8,-145.45_r8,19991213, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(10) = asosdata("DEADHORSE","DEADHORSE AIRPORT","AK",27406,"PASC","SCC",70.19_r8,-148.47_r8,19990609,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(11) = asosdata("DEERING","DEERING AIRPORT","AK",26643,"PADE","0Z0",66.06_r8,-162.76_r8,19980727,"NO","FAA", &
        -9._r8,-9._r8)
    asoscommdates(12) = asosdata("DELTA JUNCTION/FT GREELY","ALLEN ARMY AIRFIELD ","AK",26415,"PABI","BIG",63.99_r8,-145.71_r8, &
        19971215,"YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(13) = asosdata("EAGLE","EAGLE AIRPORT","AK",26422,"PAEG","EAA",64.77_r8,-141.14_r8,19980215,"NO","FAA",33.00_r8,&
        10.05_r8)
    asoscommdates(14) = asosdata("FAIRBANKS","FAIRBANKS INTERNATIONAL AIRPORT","AK",26411,"PAFA","FAI",64.81_r8,-147.85_r8, &
        19971201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(15) = asosdata("GULKANA","GULKANA AIRPORT","AK",26425,"PAGK","GKN",62.16_r8,-145.45_r8,19991119,"YES","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(16) = asosdata("HAINES","HAINES AIRPORT","AK",25323,"PAHN","HNS",59.24_r8,-135.52_r8,19980605,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(17) = asosdata("HOMER","HOMER AIRPORT","AK",25507,"PAHO","HOM",59.64_r8,-151.47_r8,19971201,"YES","NWS", &
        26.00_r8,7.92_r8)
    asoscommdates(18) = asosdata("ILIAMNA","ILIAMNA AIRPORT","AK",25506,"PAIL","ILI",59.75_r8,-154.91_r8,19971201,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(19) = asosdata("JUNEAU","JUNEAU INTERNATIONAL AIRPORT","AK",25309,"PAJN","JNU",58.35_r8,-134.57_r8,19980301, &
        "YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(20) = asosdata("KALTAG","KALTAG AIRPORT","AK",26502,"PAKV","KAL",64.32_r8,-158.74_r8,19980707,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(21) = asosdata("KALWOCK","KALWOCK AIRPORT","AK",25367,"PAKW","AKW",55.58_r8,-133.07_r8,19970401,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(22) = asosdata("KENAI","KENAI MUNICIPAL AIRPORT","AK",26523,"PAEN","ENA",60.57_r8,-151.23_r8,19990510,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(23) = asosdata("KETCHIKAN","KETCHIKAN INTERNATIONAL AIRPORT","AK",25325,"PAKT","KTN",55.35_r8,-131.71_r8, &
        19961209,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(24) = asosdata("KING SALMON","KING SALMON AIRPORT","AK",25503,"PAKN","AKN",58.68_r8,-156.65_r8,19980601,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(25) = asosdata("KIVALINA","KIVALINA AIRPORT","AK",26642,"PAVL","KVL",67.73_r8,-164.54_r8,19980707,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(26) = asosdata("KODIAK","KODIAK AIRPORT","AK",25501,"PADQ","ADQ",57.75_r8,-152.49_r8,19970101,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(27) = asosdata("KOTZEBUE","RALPH WIEN MEMORIAL AIRPORT","AK",26616,"PAOT","OTZ",66.88_r8,-162.59_r8,19971201, &
        "YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(28) = asosdata("M! GRATH","M! GRATH AIRPORT","AK",26510,"PAMC","MCG",62.95_r8,-155.6_r8,19980701,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(29) = asosdata("NENANA","NENANA MUNICIPAL AIRPORT","AK",26435,"PANN","ENN",64.55_r8,-149.07_r8,19980101,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(30) = asosdata("NOME","NOME AIRPORT","AK",26617,"PAOM","OME",64.51_r8,-165.44_r8,19980701,"YES","NWS",26.00_r8, &
        7.92_r8)
    asoscommdates(31) = asosdata("NORTHWAY","NORTHWAY AIRPORT","AK",26412,"PAOR","ORT",62.96_r8,-141.94_r8,20000320,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(32) = asosdata("NUIQSUT","NUIQSUT AIRPORT","AK",27515,"PAQT","AQT",70.21_r8,-151._r8,19980723,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(33) = asosdata("PALMER","PALMER MUNICIPAL AIRPORT","AK",25331,"PAAQ","PAQ",61.59_r8,-149.09_r8,19970915,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(34) = asosdata("PORTAGE GLACIER","PORTAGE GLACIER","AK",26492,"PATO","POR",60.78_r8,-148.83_r8,19981015,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(35) = asosdata("SELDOVIA","SELDOVIA AIRPORT","AK",25516,"PASO","SOV",59.44_r8,-151.7_r8,19970502,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(36) = asosdata("SEWARD","SEWARD AIRPORT","AK",26438,"PAWD","SWD",60.12_r8,-149.41_r8,19970401,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(37) = asosdata("SITKA","SITKA ROCKY GUTIERREZ AIRPORT","AK",25333,"PASI","SIT",57.04_r8,-135.36_r8,19961201, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(38) = asosdata("SKAGWAY","SKAGWAY AIRPORT","AK",25335,"PAGY","SGY",59.46_r8,-135.31_r8,19960822,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(39) = asosdata("ST PAUL ISLAND","ST PAUL ISLAND AIRPORT","AK",25713,"PASN","SNP",57.16_r8,-170.22_r8,19970101, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(40) = asosdata("ST. GEORGE ISLAND","ST GEORGE AIRPORT","AK",25628,"PAPB","A8L",56.6_r8,-169.56_r8,19960918,"NO",&
        "FAA",-9._r8,-9._r8)
    asoscommdates(41) = asosdata("TALKEETNA","TALKEETNA AIRPORT","AK",26528,"PATK","TKA",62.32_r8,-150.09_r8,19980101,"YES","NWS",&
        26.00_r8,7.92_r8)
    asoscommdates(42) = asosdata("TANANA","RALPH M CALHOUN MEMORIAL AIRPORT","AK",26529,"PATA","TAL",65.17_r8,-152.1_r8,19991119, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(43) = asosdata("WAINWRIGHT","WAINWRIGHT AIRPORT (NOTE SITE ID IS 5WW)","AK",27503,"PAWI","AWI",70.63_r8, &
        -159.99_r8,19980915,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(44) = asosdata("YAKUTAT","YAKUTAT AIRPORT","AK",25339,"PAYA","YAK",59.51_r8,-139.62_r8,19971101,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(45) = asosdata("ALABASTER","SHELBY COUNTY AIRPORT","AL",53864,"KEET","EET",33.17_r8,-86.78_r8,19980617,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(46) = asosdata("ANNISTON","ANNISTON METROPOLITAN AIRPORT","AL",13871,"KANB","ANB",33.58_r8,-85.85_r8,19980617, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(47) = asosdata("BIRMINGHAM","BIRMINGHAM INTERNATIONAL AIRPORT","AL",13876,"KBHM","BHM",33.56_r8,-86.75_r8, &
        19980925,"YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(48) = asosdata("DECATUR","PRYOR FIELD REGIONAL AIRPORT","AL",53852,"KDCU","DCU",34.65_r8,-86.94_r8,19961023, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(49) = asosdata("DOTHAN","DOTHAN REGIONAL AIRPORT","AL",13839,"KDHN","DHN",31.32_r8,-85.44_r8,20010627,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(50) = asosdata("EVERGREEN","MIDDLETON FIELD AIRPORT","AL",53820,"KGZH","GZH",31.41_r8,-87.04_r8,19970331,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(51) = asosdata("HUNTSVILLE","HUNTSVILLE INTERNATIONAL/CARL T. JONES FIELD ARPT","AL",3856,"KHSV","HSV",34.64_r8,&
        -86.78_r8,19940801,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(52) = asosdata("MOBILE","MOBILE DOWNTOWN AIRPORT","AL",13838,"KBFM","BFM",30.62_r8,-88.06_r8,19960905,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(53) = asosdata("MOBILE","MOBILE REGIONAL AIRPORT","AL",13894,"KMOB","MOB",30.68_r8,-88.24_r8,19960201,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(54) = asosdata("MONTGOMERY","MONTGOMERY REGIONAL (DANNELLY FIELD) AIRPORT","AL",13895,"KMGM","MGM",32.3_r8, &
        -86.39_r8,19950701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(55) = asosdata("MUSCLE SHOALS","NORTH WEST ALABAMA REGIONAL AIRPORT","AL",13896,"KMSL","MSL",34.74_r8,-87.61_r8,&
        19970408,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(56) = asosdata("TROY","TROY MUNICIPAL AIRPORT","AL",3878,"KTOI","TOI",31.86_r8,-86.01_r8,20001005,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(57) = asosdata("TUSCALOOSA","TUSCALOOSA MUNICIPAL AIRPORT","AL",93806,"KTCL","TCL",33.21_r8,-87.61_r8,19990107,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(58) = asosdata("BLYTHEVILLE","BLYTHEVILLE MUNICIPAL AIRPORT","AR",53869,"KHKA","HKA",35.94_r8,-89.83_r8, &
        19981028,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(59) = asosdata("De QUEEN","J LYNN HELMS SEVIER COUNTY AIRPORT","AR",53925,"KDEQ","DEQ",34.04_r8,-94.39_r8, &
        20010712,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(60) = asosdata("EL DORADO","SOUTH ARKANSAS REGIONAL AT GOODWIN FIELD AIRPORT","AR",93992,"KELD","ELD",33.22_r8,&
        -92.81_r8,20001101,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(61) = asosdata("FAYETTEVILLE","DRAKE FIELD AIRPORT","AR",93993,"KFYV","FYV",36._r8,-94.17_r8,19970513,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(62) = asosdata("FAYETTEVILLE/SPRINGDALE","NORTHWEST ARKANSAS REGIONAL AIRPORT","AR",53922,"KXNA","XNA",36.28_r8,&
        -94.3_r8,19990428,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(63) = asosdata("FORT SMITH","FORT SMITH REGIONAL AIRPORT","AR",13964,"KFSM","FSM",35.33_r8,-94.36_r8,19940801, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(64) = asosdata("HARRISON","BOONE COUNTY AIRPORT","AR",13971,"KHRO","HRO",36.26_r8,-93.15_r8,20000913,"NO","FAA",&
        26.00_r8,7.92_r8)
    asoscommdates(65) = asosdata("HOT SPRINGS","MEMORIAL FIELD AIRPORT","AR",3962,"KHOT","HOT",34.47_r8,-93.09_r8,20001220,"NO",&
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(66) = asosdata("JONESBORO","JONESBORO MUNICIPAL AIRPORT","AR",3953,"KJBR","JBR",35.83_r8,-90.64_r8,19981028,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(67) = asosdata("LITTLE ROCK","ADAMS FIELD AIRPORT","AR",13963,"KLIT","LIT",34.74_r8,-92.23_r8,19981028,"YES", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(68) = asosdata("MONTICELLO","MONTICELLO MUNICIPAL/ELLIS FIELD AIRPORT","AR",53919,"KLLQ","LLQ",33.63_r8, &
        -91.75_r8,19981022,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(69) = asosdata("MOUNT IDA","MOUNT IDA","AR",53921,"KMWT","MWT",34.54_r8,-93.57_r8,19990301,"NO","NWS",33.00_r8,&
        10.05_r8)
    asoscommdates(70) = asosdata("MOUNTAIN HOME","OCARK REGIONAL AIRPORT","AR",53918,"KBPK","BPK",36.36_r8,-92.47_r8,19981022, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(71) = asosdata("PINE BLUFF","GRIDER FIELD AIRPORT","AR",93988,"KPBF","PBF",34.17_r8,-91.93_r8,20000926,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(72) = asosdata("RUSSELLVILLE","RUSSELLVILLE REGIONAL AIRPORT","AR",53920,"KRUE","RUE",35.25_r8,-93.09_r8, &
        19990112,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(73) = asosdata("TEXARKANA","TEXARKANA REGIONAL AIRPORT-WEBB FIELD","AR",13977,"KTXK","TXK",33.45_r8,-94._r8, &
        19960712,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(74) = asosdata("WEST MEMPHIS","WEST MEMPHIS MUNICIPAL AIRPORT","AR",53959,"KAWM","AWM",35.13_r8,-90.23_r8, &
        20030115,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(75) = asosdata("DOUGLAS BISBEE","BISBEE DOUGLAS INTERNATIONAL AIRPORT","AZ",93026,"KDUG","DUG",31.46_r8, &
        -109.6_r8,20000927,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(76) = asosdata("FLAGSTAFF","FLAGSTAGG PULLIAM AIRPORT","AZ",3103,"KFLG","FLG",35.14_r8,-111.67_r8,19940701, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(77) = asosdata("GRAND CANYON","GRAND CANYON NATIONAL PARK AIRPORT ","AZ",3195,"KGCN","GCN",35.94_r8,-112.15_r8,&
        19931201,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(78) = asosdata("KINGMAN","KINGMAN AIRPORT","AZ",93167,"KIGM","IGM",35.25_r8,-113.93_r8,19950901,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(79) = asosdata("NOGALES","NOGALES INTERNATIONAL AIRPORT","AZ",3196,"KOLS","OLS",31.42_r8,-110.84_r8,19990728, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(80) = asosdata("PAGE","PAGE MUNICIPAL AIRPORT","AZ",3162,"KPGA","PGA",36.92_r8,-111.44_r8,19970328,"NO","NWS", &
        26.00_r8,7.92_r8)
    asoscommdates(81) = asosdata("PHOENIX","PHOENIX DEER VALLEY AIRPORT","AZ",3184,"KDVT","DVT",33.68_r8,-112.08_r8,19980902,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(82) = asosdata("PHOENIX","PHOENIX SKY HARBOR INTERNATIONAL AIRPORT","AZ",23183,"KPHX","PHX",33.44_r8,-111.99_r8,&
        19940301,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(83) = asosdata("PRESCOTT","ERNEST A.LOVE FIELD AIRPORT","AZ",23184,"KPRC","PRC",34.65_r8,-112.42_r8,19990203, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(84) = asosdata("SAFFORD","SAFFORD REGIONAL AIRPORT","AZ",93084,"KSAD","SAD",32.85_r8,-109.63_r8,19970903,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(85) = asosdata("SCOTTSDALE","SCOTTSDALE AIRPORT","AZ",3192,"KSDL","SDL",33.62_r8,-111.91_r8,20011030,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(86) = asosdata("ST. JOHNS","ST.JOHNS INDUSTRIAL AIR PARK AIRPORT","AZ",93027,"KSJN","SJN",34.51_r8,-109.37_r8, &
        19990526,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(87) = asosdata("TUCSON","TUCSON INTERNATIONAL AIRPORT","AZ",23160,"KTUS","TUS",32.13_r8,-110.95_r8,19960101, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(88) = asosdata("WINDOW ROCK","WINDOW ROCK AIRPORT","AZ",3029,"KRQE","RQE",35.65_r8,-109.06_r8,19980818,"NO", &
        "NWS",26.00_r8,7.92_r8)
    asoscommdates(89) = asosdata("WINSLOW","WINSLOW-LINDBERGH REGIONAL AIRPORT","AZ",23194,"KINW","INW",35.02_r8,-110.72_r8, &
        19950701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(90) = asosdata("ALTURAS","ALTURAS MUNICIPAL AIRPORT","CA",94299,"KAAT","AAT",41.48_r8,-120.56_r8,19980601,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(91) = asosdata("ARCATA/EUREKA","ARCATA AIRPORT","CA",24283,"KACV","ACV",40.97_r8,-124.1_r8,20010208,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(92) = asosdata("AVALON","CATALINA AIRPORT","CA",23191,"KAVX","AVX",33.4_r8,-118.41_r8,20000607,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(93) = asosdata("BAKERSFIELD","MEADOWS FIELD AIRPORT","CA",23155,"KBFL","BFL",35.43_r8,-119.05_r8,19960601,"YES",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(94) = asosdata("BISHOP","BISHOP AIRPORT","CA",23157,"KBIH","BIH",37.37_r8,-118.36_r8,19940501,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(95) = asosdata("BLYTHE","BLYTHE AIRPORT","CA",23158,"KBLH","BLH",33.61_r8,-114.71_r8,20000830,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(96) = asosdata("BURBANK","BURBANK-GLENDALE-PASADENA AIRPORT","CA",23152,"KBUR","BUR",34.2_r8,-118.35_r8, &
        19980522,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(97) = asosdata("CAMARILLO","CAMARILLO AIRPORT","CA",23136,"KCMA","CMA",34.21_r8,-119.08_r8,19990929,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(98) = asosdata("CAMPO","CAMPO","CA",3164,"KCZZ","CZZ",32.61_r8,-116.46_r8,19970826,"NO","NWS",33.00_r8,10.05_r8)                                                                                                                                                                                                                                                                                  
    asoscommdates(99) = asosdata("CARLSBAD","McCLELLAN-PALOMAR AIRPORT","CA",3177,"KCRQ","CRQ",33.12_r8,-117.27_r8,19980218,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(100) = asosdata("CHINO","CHINO AIRPORT","CA",3179,"KCNO","CNO",33.97_r8,-117.63_r8,19980521,"NO","FAA",26.00_r8,&
        7.92_r8)
    asoscommdates(101) = asosdata("CONCORD","BUCHANAN FIELD AIRPORT","CA",23254,"KCCR","CCR",37.99_r8,-122.05_r8,19990608,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(102) = asosdata("CRESCENT CITY","JACK McNAMARA FIELD AIRPORT","CA",24286,"KCEC","CEC",41.78_r8,-124.23_r8, &
        20000913,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(103) = asosdata("DAGGETT","BARSTOW-DAGGETT AIRPORT","CA",23161,"KDAG","DAG",34.85_r8,-116.78_r8,20000823,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(104) = asosdata("EMIGRANT GAP","BLUE CANYON - NYACK AIRPORT","CA",23225,"KBLU","BLU",39.29_r8,-120.7_r8, &
        19930201,"NO","NWS",26.00_r8,7.92_r8)
    asoscommdates(105) = asosdata("FRESNO","FRESNO YOSEMITE INTERNATIONAL AIRPORT","CA",93193,"KFAT","FAT",36.78_r8,-119.71_r8, &
        19950901,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(106) = asosdata("FULLERTON","FULLERTON MUNICIPAL AIRPORT","CA",3166,"KFUL","FUL",33.87_r8,-117.97_r8,19980701, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(107) = asosdata("HANFORD","HANFORD MUNICIPAL AIRPORT","CA",53119,"KHJO","HJO",36.31_r8,-119.62_r8,19980219,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(108) = asosdata("HAWTHORNE","JACK NORTHROP FIELD/HAWTHORNE MUNICIPAL AIRPORT","CA",3167,"KHHR","HHR",33.92_r8, &
        -118.33_r8,19981110,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(109) = asosdata("HAYWARD","HAYWARD EXECUTIVE AIRPORT","CA",93228,"KHWD","HWD",37.65_r8,-122.12_r8,19980923,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(110) = asosdata("IMPERIAL","IMPERIAL COUNTY AIRPORT","CA",3144,"KIPL","IPL",32.83_r8,-115.57_r8,20000816,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(111) = asosdata("LANCASTER","GENERAL WILLIAM J. FOX AIRFIELD AIRPORT","CA",3159,"KWJF","WJF",34.74_r8, &
        -118.21_r8,20001213,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(112) = asosdata("LIVERMORE","LIVERMORE MUNICIPAL AIRPORT","CA",23285,"KLVK","LVK",37.69_r8,-121.81_r8,19980331, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(113) = asosdata("LONG BEACH","LONG BEACH/DAUGHERTY FIELD AIRPORT","CA",23129,"KLGB","LGB",33.82_r8,-118.16_r8, &
        19960901,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(114) = asosdata("LOS ANGELES","DOWNTOWN LOS ANGELES/US! CAMPUS","CA",93134,"KCQT","CQT",34.02_r8,-118.29_r8, &
        19990624,"YES","NWS",19.00_r8,5.79_r8)
    asoscommdates(115) = asosdata("LOS ANGELES","LOS ANGELES INTERNATIONAL AIRPORT","CA",23174,"KLAX","LAX",33.93_r8,-118.4_r8, &
        19970301,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(116) = asosdata("MADERA","MADERA MUNICIPAL AIRPORT","CA",93242,"KMAE","MAE",36.98_r8,-120.11_r8,19980902,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(117) = asosdata("MARYSVILLE","YUBA COUNTY AIRPORT","CA",93205,"KMYV","MYV",39.09_r8,-121.56_r8,20001004,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(118) = asosdata("MERCED","MERCED MUNICIPAL/MACREADY FIELD AIRPORT","CA",23257,"KMCE","MCE",37.28_r8,-120.51_r8, &
        19980806,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(119) = asosdata("MODESTO","MODESTO CITY COUNTY HARRY SHAM FIELD AIRPORT","CA",23258,"KMOD","MOD",37.62_r8, &
        -120.95_r8,19980513,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(120) = asosdata("MOUNTAIN VIEW","MOFFETT FEDERAL AIRFIELD","CA",23244,"KNUQ","NUQ",37.41_r8,-122.04_r8,19960526,&
        "NO","NWS",-9._r8,-9._r8)
    asoscommdates(121) = asosdata("MONTAGUE","SISKIYOU COUNTY AIRPORT","CA",24259,"KSIY","SIY",41.78_r8,-122.46_r8,20010110,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(122) = asosdata("MONTEREY","MONTEREY PENINSULA AIRPORT","CA",23259,"KMRY","MRY",36.58_r8,-121.84_r8,19980325, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(123) = asosdata("MOUNT SHASTA","MOUNT SHASTA","CA",24215,"KMHS","MHS",41.33_r8,-122.33_r8,19960801,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(124) = asosdata("NAPA","NAPA COUNTY AIRPORT","CA",93227,"KAPC","APC",38.21_r8,-122.27_r8,19980522,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(125) = asosdata("NEEDLES","NEEDLES AIRPORT","CA",23179,"KEED","EED",34.76_r8,-114.62_r8,20010124,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(126) = asosdata("OAKLAND","METROPOLITAN OAKLAND INTERNATIONAL AIRPORT","CA",23230,"KOAK","OAK",37.75_r8, &
        -122.22_r8,20000112,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(127) = asosdata("OCEANSIDE","OCEANSIDE MUNICIPAL AIRPORT","CA",53121,"KOKB","OKB",33.21_r8,-117.34_r8,19990512, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(128) = asosdata("ONTARIO","ONTARIO INTERNATIONAL AIRPORT","CA",3102,"KONT","ONT",34.05_r8,-117.6_r8,19980527, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(129) = asosdata("OROVILLE","OROVILLE MUNICIPAL AIRPORT","CA",93210,"KOVE","OVE",39.49_r8,-121.61_r8,19980617, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(130) = asosdata("OXNARD","OXNARD AIRPORT","CA",93110,"KOXR","OXR",34.2_r8,-119.2_r8,19980304,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(131) = asosdata("PALM SPRINGS","PALM SPRINGS INTERNATIONAL AIRPORT","CA",93138,"KPSP","PSP",33.82_r8,-116.5_r8, &
        19980218,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(132) = asosdata("PALM SPRINGS","DESERT RESORTS REGIONAL AIRPORT","CA",3104,"KTRM","TRM",33.62_r8,-116.16_r8, &
        20001011,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(133) = asosdata("PALMDALE","PALMDALE PRODUCTION FLIGHT/TEST INSTLN AF PLANT APT","CA",23182,"KPMD","PMD", &
        34.62_r8,-118.08_r8,19980408,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(134) = asosdata("PASO ROBLES","PASO ROBLES MUNICIPAL AIRPORT","CA",93209,"KPRB","PRB",35.67_r8,-120.62_r8, &
        20010118,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(135) = asosdata("RAMONA","RAMONA AIRPORT","CA",53120,"KRNM","RNM",33.03_r8,-116.91_r8,19980416,"NO","NWS", &
        26.00_r8,7.92_r8)
    asoscommdates(136) = asosdata("RED BLUFF","RED BLUFF MUNICIPAL AIRPORT","CA",24216,"KRBL","RBL",40.15_r8,-122.25_r8,19951001, &
        "NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(137) = asosdata("REDDING","REDDING MUNICIPAL AIRPORT","CA",24257,"KRDD","RDD",40.51_r8,-122.31_r8,19960701, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(138) = asosdata("RIVERSIDE","RIVERSIDE MUNICIPAL AIRPORT","CA",3171,"KRAL","RAL",33.95_r8,-117.43_r8,19980723, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(139) = asosdata("SACRAMENTO","SACRAMENTO EXECUTIVE AIRPORT","CA",23232,"KSAC","SAC",38.51_r8,-121.49_r8, &
        19980415,"YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(140) = asosdata("SACRAMENTO","SACRAMENTO INTERNATIONAL AIRPORT","CA",93225,"KSMF","SMF",38.69_r8,-121.58_r8, &
        19980521,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(141) = asosdata("SALINAS","SALINAS MUNICIPAL AIRPORT","CA",23233,"KSNS","SNS",36.66_r8,-121.6_r8,19980909,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(142) = asosdata("SAN DIEGO","MONTGOMERY FIELD AIRPORT","CA",3131,"KMYF","MYF",32.81_r8,-117.13_r8,19980218,"NO",&
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(143) = asosdata("SAN DIEGO","SAN DIEGO INTERNATIONAL-LINDBERGH FIELD AIRPORT","CA",23188,"KSAN","SAN",32.73_r8, &
        -117.16_r8,19960801,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(144) = asosdata("SAN DIEGO","BROWN FIELD MUNICIPAL AIRPORT","CA",3178,"KSDM","SDM",32.57_r8,-116.97_r8,19970201,&
        "NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(145) = asosdata("SAN FRANCISCO","SAN FRANCISCO INTERNATIONAL AIRPORT","CA",23234,"KSFO","SFO",37.61_r8, &
        -122.39_r8,19961001,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(146) = asosdata("SAN JOSE","NORMAN Y. MINETA SAN JOSE INTERNATIONAL AIRPORT","CA",23293,"KSJC","SJC",37.36_r8, &
        -121.92_r8,19980708,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(147) = asosdata("SAN LUIS OBISPO","SAN LUIS COUNTY REGIONAL AIRPORT","CA",93206,"KSBP","SBP",35.23_r8, &
        -120.64_r8,19980401,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(148) = asosdata("SANDBERG","SANDBERG","CA",23187,"KSDB","SDB",34.74_r8,-118.72_r8,19960401,"NO","NWS",33.00_r8, &
        10.05_r8)
    asoscommdates(149) = asosdata("SANTA ANA","J.WAYNE AIRPORT-ORANGE COUNTY AIRPORT","CA",93184,"KSNA","SNA",33.68_r8,-117.86_r8,&
        19990217,"NO","FAA",19.00_r8,5.79_r8)
    asoscommdates(150) = asosdata("SANTA BARBARA","SANTA BARBARA MUNICIPAL AIRPORT","CA",23190,"KSBA","SBA",34.42_r8,-119.84_r8, &
        19980304,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(151) = asosdata("SANTA MARIA","SANTA MARIA PUBLIC/CAPT G ALLEN HANCOCK FLD AIRPORT","CA",23273,"KSMX","SMX", &
        34.91_r8,-120.46_r8,19960801,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(152) = asosdata("SANTA MONICA","SANTA MONICA MUNICIPAL AIRPORT","CA",93197,"KSMO","SMO",34.01_r8,-118.45_r8, &
        20001005,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(153) = asosdata("SANTA ROSA","SONOMA COUNTY AIRPORT","CA",23213,"KSTS","STS",38.5_r8,-122.81_r8,19980603,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(154) = asosdata("SOUTH LAKE TAHOE","LAKE TAHOE AIRPORT","CA",93230,"KTVL","TVL",38.89_r8,-119.99_r8,20001018, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(155) = asosdata("STOCKTON","STOCKTON METROPOLITAN AIRPORT","CA",23237,"KSCK","SCK",37.89_r8,-121.23_r8,19961101,&
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(156) = asosdata("UKIAH","UKIAH MUNICIPAL AIRPORT","CA",23275,"KUKI","UKI",39.12_r8,-123.2_r8,20010125,"NO", &
        "FAA",19.00_r8,5.79_r8)
    asoscommdates(157) = asosdata("VACAVILLE","NUT TREE AIRPORT","CA",93241,"KVCB","VCB",38.37_r8,-121.96_r8,19980331,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(158) = asosdata("VAN NUYS","VAN NUYS AIRPORT","CA",23130,"KVNY","VNY",34.2_r8,-118.48_r8,19980526,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(159) = asosdata("WATSONVILLE","WATSONVILLE MUNICIPAL AIRPORT","CA",23277,"KWVI","WVI",36.93_r8,-121.78_r8, &
        19980723,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(160) = asosdata("AKRON","COLORADO PLAINS REGIONAL AIRPORT","CO",24015,"KAKO","AKO",40.17_r8,-103.23_r8,19960206,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(161) = asosdata("ALAMOSA","SAN LUIS VALLEY REGIONAL AIRPORT","CO",23061,"KALS","ALS",37.43_r8,-105.86_r8, &
        19920901,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(162) = asosdata("ASPEN","ASPEN-PITKIN COUNTY AIRPORT","CO",93073,"KASE","ASE",39.22_r8,-106.86_r8,19980521,"NO",&
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(163) = asosdata("BURLINGTON","KIT CARSON COUNTY AIRPORT","CO",3026,"KITR","ITR",39.24_r8,-102.28_r8,19970410, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(164) = asosdata("COLORADO SPRINGS","CITY OF COLORADO SPRINGS MUNICIPAL AIRPORT","CO",93037,"KCOS","COS", &
        38.81_r8,-104.71_r8,19921101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(165) = asosdata("CORTEZ","CORTEZ MUNICIPAL AIRPORT","CO",93069,"KCEZ","CEZ",37.3_r8,-108.62_r8,19960523,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(166) = asosdata("CRAIG","CRAIG-MOFFAT AIRPORT","CO",24046,"KCAG","CAG",40.49_r8,-107.52_r8,19960815,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(167) = asosdata("DENVER","CENTENNIAL AIRPORT","CO",93067,"KAPA","APA",39.57_r8,-104.84_r8,19980629,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(168) = asosdata("DENVER","DENVER INTERNATIONAL AIRPORT","CO",3017,"KDEN","DEN",39.83_r8,-104.65_r8,19950301, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(169) = asosdata("DURANGO","DURANGO LA PLATA COUNTY AIRPORT","CO",93005,"KDRO","DRO",37.14_r8,-107.75_r8, &
        19960523,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(170) = asosdata("GRAND JUNCTION","WALKER FIELD AIRPORT","CO",23066,"KGJT","GJT",39.13_r8,-108.53_r8,19960401, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(171) = asosdata("LA JUNTA","LA JUNTA MUNICIPAL AIRPORT","CO",23067,"KLHX","LHX",38.05_r8,-103.52_r8,19960206, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(172) = asosdata("LAMAR","LAMAR MUNICIPAL AIRPORT","CO",3013,"KLAA","LAA",38.07_r8,-102.68_r8,19950517,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(173) = asosdata("LEADVILLE","LEADVILLE/LAKE COUNTY AIRPORT","CO",93009,"KLXV","LXV",39.22_r8,-106.31_r8, &
        19980730,"NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(174) = asosdata("LIMON","LIMON MUNICIPAL AIRPORT","CO",93010,"KLIC","LIC",39.18_r8,-103.71_r8,19951214,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(175) = asosdata("MEEKER","MEEKER AIRPORT","CO",94050,"KEEO","EEO",40.04_r8,-107.88_r8,19970227,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(176) = asosdata("MONTROSE","MONTROSE REGIONAL AIRPORT","CO",93013,"KMTJ","MTJ",38.5_r8,-107.89_r8,19931201,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(177) = asosdata("PUEBLO","PUEBLO MEMORIAL AIRPORT","CO",93058,"KPUB","PUB",38.29_r8,-104.49_r8,19921001,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(178) = asosdata("RIFLE","GARFIELD COUNTY REGIONAL AIRPORT","CO",3016,"KRIL","RIL",39.52_r8,-107.72_r8,19970227,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(179) = asosdata("SPRINGFIELD","SPRINGFIELD AIRPORT","CO",3028,"KSPD","SPD",37.28_r8,-102.61_r8,19980625,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(180) = asosdata("TRINIDAD","PERRY STOKES AIRPORT","CO",23070,"KTAD","TAD",37.25_r8,-104.34_r8,20010927,"NO", &
        "FAA",-9_r8,-9_r8)
    asoscommdates(181) = asosdata("BRIDGEPORT","IGOR I SIKORSKY MEMORIAL AIRPORT","CT",94702,"KBDR","BDR",41.17_r8,-73.14_r8, &
        19960501,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(182) = asosdata("DANBURY","DANBURY MUNICIPAL AIRPORT","CT",54734,"KDXR","DXR",41.37_r8,-73.48_r8,19980513,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(183) = asosdata("GROTON NEW LONDON","GROTON-NEW LONDON AIRPORT","CT",14707,"KGON","GON",41.32_r8,-72.04_r8, &
        19991208,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(184) = asosdata("HARTFORD","HARTFORD-BRAINARD AIRPORT","CT",14752,"KHFD","HFD",41.73_r8,-72.65_r8,19970515,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(185) = asosdata("MERIDEN","MERIDEN MARKHAM MUNICIPAL AIRPORT","CT",54788,"KMMK","MMK",41.5_r8,-72.82_r8, &
        19990804,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(186) = asosdata("NEW HAVEN","TWEED-NEW HAVEN AIRPORT","CT",14758,"KHVN","HVN",41.26_r8,-72.88_r8,20011114,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(187) = asosdata("WILLIMANTIC","WINDHAM AIRPORT","CT",54767,"KIJD","IJD",41.74_r8,-72.18_r8,19950428,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(188) = asosdata("WINDSOR LOCKS","BRADLEY INTERNATIONAL AIRPORT","CT",14740,"KBDL","BDL",41.93_r8,-72.68_r8, &
        19960401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(189) = asosdata("WASHINGTON","RONALD REAGAN WASHINGTON NATIONAL AIRPORT","DC",13743,"KDCA","DCA",38.86_r8, &
        -77.03_r8,19980201,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(190) = asosdata("WASHINGTON ","WASHINGTON DULLES INTERNATIONAL AIRPORT","DC",93738,"KIAD","IAD",38.93_r8, &
        -77.44_r8,19960501,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(191) = asosdata("GEORGETOWN","SUSSEX COUNTY AIRPORT","DE",13764,"KGED","GED",38.68_r8,-75.35_r8,19971008,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(192) = asosdata("WILMINGTON","NEW CASTLE COUNTY AIRPORT","DE",13781,"KILG","ILG",39.67_r8,-75.6_r8,19941001, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(193) = asosdata("APALACHICOLA","APALACHICOLA MUNICIPAL AIRPORT","FL",12832,"KAAF","AAF",29.73_r8,-85.03_r8, &
        19980731,"NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(194) = asosdata("BROOKSVILLE","HERNANDO COUNTY AIRPORT","FL",12818,"KBKV","BKV",28.47_r8,-82.45_r8,19950524, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(195) = asosdata("CRESTVIEW","BOB SIKES AIRPORT","FL",13884,"KCEW","CEW",30.77_r8,-86.52_r8,19970701,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(196) = asosdata("CROSS CITY","CROSS CITY AIRPORT","FL",12833,"KCTY","CTY",29.55_r8,-83.1_r8,19980227,"NO","NWS",&
        33.00_r8,10.05_r8)
    asoscommdates(197) = asosdata("DAYTONA BEACH","DAYTONA BEACH INTERNATIONAL AIRPORT","FL",12834,"KDAB","DAB",29.17_r8, &
        -81.06_r8,19950601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(198) = asosdata("DESTIN","DESTIN-FORT WALTON BEACH AIRPORT","FL",53853,"KDTS","DTS",30.4_r8,-86.47_r8,19961106, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(199) = asosdata("FORT LAUDERDALE","FORT LAUDERDALE/HOLLYWOOD INTERNATIONAL AIRPORT","FL",12849,"KFLL","FLL", &
        26.07_r8,-80.15_r8,19980909,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(200) = asosdata("FORT LAUDERDALE","FORT LAUDERDALE EXECUTIVE AIRPORT","FL",12885,"KFXE","FXE",26.19_r8, &
        -80.17_r8,19980624,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(201) = asosdata("FORT MYERS","PAGE FIELD AIRPORT","FL",12835,"KFMY","FMY",26.58_r8,-81.86_r8,19980610,"YES", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(202) = asosdata("FORT MYERS","SOUTHWEST FLORIDA INTERNATIONAL AIRPORT","FL",12894,"KRSW","RSW",26.53_r8, &
        -81.75_r8,19980521,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(203) = asosdata("FORT PIERCE","ST LUCIE COUNTY INTERNATIONAL AIRPORT","FL",12895,"KFPR","FPR",27.49_r8, &
        -80.37_r8,19990728,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(204) = asosdata("GAINESVILLE","GAINESVILLE REGIONAL AIRPORT","FL",12816,"KGNV","GNV",29.68_r8,-82.27_r8, &
        19980812,"YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(205) = asosdata("HOLLYWOOD","NORTH PERRY AIRPORT","FL",92809,"KHWO","HWO",25.99_r8,-80.24_r8,19990421,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(206) = asosdata("JACKSONVILLE","CRAIG MUNICIPAL AIRPORT ","FL",53860,"KCRG","CRG",30.33_r8,-81.51_r8,19971125, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(207) = asosdata("JACKSONVILLE","JACKSONVILLE INTERNATIONAL AIRPORT","FL",13889,"KJAX","JAX",30.49_r8,-81.69_r8, &
        19960301,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(208) = asosdata("KEY WEST","KEY WEST INTERNATIONAL AIRPORT","FL",12836,"KEYW","EYW",24.55_r8,-81.75_r8,19960301,&
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(209) = asosdata("LEESBURG","LEESBURG REGIONAL AIRPORT","FL",12819,"KLEE","LEE",28.82_r8,-81.8_r8,19960807,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(210) = asosdata("MARATHON","THE FLORIDA KEYS MARATHON AIRPORT","FL",12896,"KMTH","MTH",24.72_r8,-81.05_r8, &
        19980514,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(211) = asosdata("MARIANNA","MARIANNA MUNICIPAL AIRPORT","FL",3818,"KMAI","MAI",30.83_r8,-85.18_r8,19970415,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(212) = asosdata("MELBOURNE","MELBOURNE INTERNATIONAL AIRPORT","FL",12838,"KMLB","MLB",28.1_r8,-80.64_r8, &
        20010207,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(213) = asosdata("MIAMI","MIAMI INTERNATIONAL AIRPORT","FL",12839,"KMIA","MIA",25.82_r8,-80.29_r8,19960701,"YES",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(214) = asosdata("MIAMI","OPA LOCKA AIRPORT","FL",12882,"KOPF","OPF",25.9_r8,-80.28_r8,19980521,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(215) = asosdata("MIAMI","KENDALL-TAMIAMI EXECUTIVE AIRPORT","FL",12888,"KTMB","TMB",25.64_r8,-80.43_r8,19980617,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(216) = asosdata("NAPLES","NAPLES MUNICIPAL AIRPORT","FL",12897,"KAPF","APF",26.15_r8,-81.77_r8,20020326,"NO", &
        "FAA",-9._r8,-9._r8)
    asoscommdates(217) = asosdata("ORLANDO","ORLANDO INTERNATIONAL AIRPORT","FL",12815,"KMCO","MCO",28.43_r8,-81.32_r8,19960701, &
        "YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(218) = asosdata("ORLANDO","EXECUTIVE AIRPORT","FL",12841,"KORL","ORL",28.54_r8,-81.33_r8,19980610,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(219) = asosdata("ORLANDO","ORLANDO SANFORD AIRPORT","FL",12854,"KSFB","SFB",28.77_r8,-81.24_r8,19990224,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(220) = asosdata("PANAMA CITY","PANAMA CITY-BAY COUNTY INTERNATIONAL AIRPORT","FL",3882,"KPFN","PFN",30.21_r8, &
        -85.68_r8,19980716,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(221) = asosdata("PENSACOLA","PENSACOLA REGIONAL AIRPORT","FL",13899,"KPNS","PNS",30.47_r8,-87.18_r8,19971201, &
        "YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(222) = asosdata("PERRY","PERRY-FOLEY AIRPORT","FL",53862,"K40J","40J",30.07_r8,-83.57_r8,19980227,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(223) = asosdata("POMPANO BEACH","POMPANO BEACH AIRPARK AIRPORT","FL",92805,"KPMP","PMP",26.25_r8,-80.1_r8, &
        19980312,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(224) = asosdata("PUNTA GORDA","CHARLOTTE COUNTY AIRPORT","FL",12812,"KPGD","PGD",26.91_r8,-81.99_r8,19960920, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(225) = asosdata("SARASOTA/BRADENTON","SARASOTA/BRADENTON INTERNATIONAL AIRPORT","FL",12871,"KSRQ","SRQ",27.4_r8,&
        -82.55_r8,19990318,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(226) = asosdata("ST PETERSBURG","ALBERT WHITTED AIRPORT","FL",92806,"KSPG","SPG",27.76_r8,-82.62_r8,19980617, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(227) = asosdata("ST PETERSBURG/CLEARWATER","ST PETERSBURG-CLEARWATER INTERNATIONAL AIRPORT","FL",12873,"KPIE", &
        "PIE",27.91_r8,-82.68_r8,19980924,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(228) = asosdata("TALLAHASSEE","TALLAHASSEE REGIONAL AIRPORT","FL",93805,"KTLH","TLH",30.39_r8,-84.35_r8, &
        19960401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(229) = asosdata("TAMPA","TAMPA INTERNATIONAL AIRPORT","FL",12842,"KTPA","TPA",27.96_r8,-82.54_r8,19951101,"YES",&
        "NWS",26.00_r8,7.92_r8)
    asoscommdates(230) = asosdata("VERO BEACH","VERO BEACH MUNICIPAL AIRPORT","FL",12843,"KVRB","VRB",27.65_r8,-80.41_r8,20010730,&
        "YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(231) = asosdata("WEST PALM BEACH","PALM BEACH INTERNATIONAL AIRPORT","FL",12844,"KPBI","PBI",26.68_r8,-80.09_r8,&
        19930401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(232) = asosdata("WINTER HAVEN","WINTER HAVEN'S GILBERT AIRPORT","FL",12876,"KGIF","GIF",28.06_r8,-81.75_r8, &
        19950701,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(233) = asosdata("ALBANY","SOUTHWEST GEORGIA REGIONAL AIRPORT","GA",13869,"KABY","ABY",31.53_r8,-84.19_r8, &
        20010117,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(234) = asosdata("ALMA","BACON COUNTY AIRPORT","GA",13870,"KAMG","AMG",31.53_r8,-82.5_r8,20001213,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(235) = asosdata("ATHENS","ATHENS/BEN EPPS AIRPORT","GA",13873,"KAHN","AHN",33.94_r8,-83.32_r8,19960201,"YES", &
        "NWS",26.00_r8,7.92_r8)
    asoscommdates(236) = asosdata("ATLANTA","HARTSFIELD-JACKSON ATLANTA INTERNATIONAL AIRPORT","GA",13874,"KATL","ATL",33.64_r8, &
        -84.42_r8,19950801,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(237) = asosdata("ATLANTA","PEACHTREE CITY-FALCON FIELD AIRPORT","GA",53819,"KFFC","FFC",33.35_r8,-84.56_r8, &
        19950927,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(238) = asosdata("ATLANTA","FULTON COUNTY AIRPORT-BROWN FIELD AIRPORT","GA",3888,"KFTY","FTY",33.77_r8,-84.52_r8,&
        19981028,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(239) = asosdata("ATLANTA","DEKALB-PEACHTREE AIRPORT","GA",53863,"KPDK","PDK",33.87_r8,-84.3_r8,19980318,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(240) = asosdata("AUGUSTA","AUGUSTA REGIONAL AT BUSH FIELD AIRPORT","GA",3820,"KAGS","AGS",33.36_r8,-81.96_r8, &
        19940501,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(241) = asosdata("AUGUSTA","DANIEL FIELD AIRPORT","GA",13837,"KDNL","DNL",33.46_r8,-82.03_r8,19951205,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(242) = asosdata("BRUNSWICK","MALCOLM M! KINNON AIRPORT","GA",13878,"KSSI","SSI",31.25_r8,-81.39_r8,20001025, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(243) = asosdata("CARTERSVILLE","CARTERSVILLE AIRPORT","GA",53873,"KVPC","VPC",34.12_r8,-84.84_r8,20000322,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(244) = asosdata("COLUMBUS","COLUMBUS METROPOLITAN AIRPORT","GA",93842,"KCSG","CSG",32.51_r8,-84.94_r8,19940501,&
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(245) = asosdata("GAINESVILLE","LEE GILMER MEMORIAL AIRPORT","GA",53838,"KGVL","GVL",34.27_r8,-83.83_r8,19951017,&
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(246) = asosdata("MACON","MIDDLE GEORGIA REGIONAL AIRPORT","GA",3813,"KMCN","MCN",32.68_r8,-83.65_r8,19940501, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(247) = asosdata("ROME","RICHARD B RUSSELL AIRPORT","GA",93801,"KRMG","RMG",34.34_r8,-85.16_r8,19970331,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(248) = asosdata("SAVANNAH","SAVANNAH/HILTON HEAD INTERNATIONAL AIRPORT","GA",3822,"KSAV","SAV",32.11_r8, &
        -81.2_r8,19960401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(249) = asosdata("VALDOSTA","VALDOSTA REGIONAL AIRPORT","GA",93845,"KVLD","VLD",30.78_r8,-83.27_r8,20010227,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(250) = asosdata("HILO","HILO INTERNATIONAL AIRPORT","HI",21504,"PHTO","ITO",19.72_r8,-155.05_r8,19980101,"YES",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(251) = asosdata("HONOLULU","HONOLULU INTERNATIONAL AIRPORT","HI",22521,"PHNL","HNL",21.32_r8,-157.94_r8, &
        19980201,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(252) = asosdata("KAHULUI","KAHULUI AIRPORT","HI",22516,"PHOG","OGG",20.9_r8,-156.43_r8,19980301,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(253) = asosdata("KAILUA/KONA","KONA INTERNATIONAL AT KEYHOLE AIRPORT","HI",21510,"PHKO","KOA",19.73_r8, &
        -156.04_r8,19971231,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(254) = asosdata("KAPOLEI","KALAELOA AIRPORT (JOHN RODGERS FIELD) ","HI",22551,"PHJR","JRF",21.31_r8,-158.06_r8,&
        19990831,"NO","NWS",-9._r8,-9._r8)
    asoscommdates(255) = asosdata("KAUNAKAKAI ","MOLOKAI AIRPORT","HI",22534,"PHMK","MKK",21.15_r8,-157.09_r8,19990601,"NO","NWS",&
        33.00_r8,10.05_r8)
    asoscommdates(256) = asosdata("LIHUE","LIHUE AIRPORT","HI",22536,"PHLI","LIH",21.97_r8,-159.34_r8,19971201,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(257) = asosdata("AMES","AMES MUNICIPAL AIRPORT","IA",94989,"KAMW","AMW",41.99_r8,-93.62_r8,19960919,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(258) = asosdata("BURLINGTON","SOUTHEAST IOWA REGIONAL AIRPORT","IA",14931,"KBRL","BRL",40.78_r8,-91.12_r8, &
        19961121,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(259) = asosdata("CEDAR RAPIDS","THE EASTERN IOWA AIRPORT","IA",14990,"KCID","CID",41.88_r8,-91.7_r8,19960424, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(260) = asosdata("DAVENPORT","DAVENPORT MUNICIPAL AIRPORT","IA",94982,"KDVN","DVN",41.61_r8,-90.59_r8,19960417, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(261) = asosdata("DES MOINES","DES MOINES INTERNATIONAL AIRPORT","IA",14933,"KDSM","DSM",41.53_r8,-93.66_r8, &
        19951201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(262) = asosdata("DUBUQUE","DUBUQUE REGIONAL AIRPORT","IA",94908,"KDBQ","DBQ",42.39_r8,-90.7_r8,19950901,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(263) = asosdata("ESTHERVILLE","ESTHERVILLE MUNICIPAL AIRPORT","IA",94971,"KEST","EST",43.4_r8,-94.74_r8, &
        19951214,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(264) = asosdata("IOWA CITY","IOWA CITY MUNICIPAL AIRPORT","IA",14937,"KIOW","IOW",41.63_r8,-91.54_r8,19950401, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(265) = asosdata("LAMONI","LAMONI MUNICIPAL AIRPORT","IA",94991,"KLWD","LWD",40.63_r8,-93.9_r8,19970901,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(266) = asosdata("MARSHALLTOWN","MARSHALLTOWN MUNICIPAL AIRPORT","IA",94988,"KMIW","MIW",42.11_r8,-92.91_r8, &
        19960904,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(267) = asosdata("MASON CITY","MASON CITY MUNICIPAL AIRPORT","IA",14940,"KMCW","MCW",43.15_r8,-93.33_r8,20000817,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(268) = asosdata("OTTUMWA","OTTUMWA INDUSTRIAL AIRPORT","IA",14950,"KOTM","OTM",41.1_r8,-92.44_r8,20000817,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(269) = asosdata("SIOUX CITY","SIOUX GATEWAY/COL. BUD DAY FIELD AIRPORT","IA",14943,"KSUX","SUX",42.39_r8, &
        -96.37_r8,19950601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(270) = asosdata("SPENCER","SPENCER MUNICIPAL AIRPORT","IA",14972,"KSPW","SPW",43.16_r8,-95.2_r8,19960919,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(271) = asosdata("WATERLOO","WATERLOO MUNICIPAL AIRPORT","IA",94910,"KALO","ALO",42.55_r8,-92.4_r8,19960401, &
        "YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(272) = asosdata("BOISE","BOISE AIR TERM/GOWEN FIELD AIRPORT","ID",24131,"KBOI","BOI",43.56_r8,-116.22_r8, &
        19951201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(273) = asosdata("BURLEY","BURLEY MUNICIPAL AIRPORT","ID",24133,"KBYI","BYI",42.54_r8,-113.77_r8,20001108,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(274) = asosdata("CHALLIS","CHALLIS AIRPORT","ID",4114,"KLLJ","LLJ",44.52_r8,-114.21_r8,19980922,"NO","NWS", &
        26.00_r8,7.92_r8)
    asoscommdates(275) = asosdata("ELK CITY","ELK CITY","ID",4109,"KP69","P69",46.14_r8,-115.59_r8,19970501,"NO","NWS",26.00_r8, &
        7.92_r8)
    asoscommdates(276) = asosdata("IDAHO FALLS","IDAHO FALLS REGIONAL AIRPORT","ID",24145,"KIDA","IDA",43.51_r8,-112.06_r8, &
        19980205,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(277) = asosdata("JEROME","JEROME COUNTY AIRPORT","ID",4110,"KJER","JER",42.72_r8,-114.45_r8,19970201,"NO","FAA",&
        26.00_r8,7.92_r8)
    asoscommdates(278) = asosdata("LEWISTON","LEWISTON-NEZ PERCE COUNTY AIRPORT","ID",24149,"KLWS","LWS",46.37_r8,-117.01_r8, &
        19950701,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(279) = asosdata("McCALL","McCALL MUNICIPAL AIRPORT","ID",94182,"KMYL","MYL",44.88_r8,-116.1_r8,19970916,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(280) = asosdata("MULLAN PASS","MULLAN PASS ","ID",24154,"KMLP","MLP",47.45_r8,-115.64_r8,19960619,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(281) = asosdata("POCATELLO","POCATELLO REGIONAL AIRPORT","ID",24156,"KPIH","PIH",42.92_r8,-112.57_r8,19960301, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(282) = asosdata("REXBURG","REXBURG-MADISON COUNTY AIRPORT","ID",94194,"KRXE","RXE",43.83_r8,-111.88_r8,19980212,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(283) = asosdata("STANLEY","STANLEY RANGER STATION","ID",4112,"KSNT","SNT",44.17_r8,-114.92_r8,19980313,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(284) = asosdata("TWIN FALLS","JOSLIN FIELD-MAGI! VALLEY REGIONAL AIRPORT","ID",94178,"KTWF","TWF",42.48_r8, &
        -114.48_r8,19970227,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(285) = asosdata("BLOOMINGTON/NORMAL","CENTRAL ILLINOIS REGIONAL AIRPORT","IL",54831,"KBMI","BMI",40.47_r8, &
        -88.91_r8,20050217,"NO","FAA",-9._r8,-9._r8)
    asoscommdates(286) = asosdata("CAHOKIA/ST.LOUIS","ST.LOUIS DOWNTOWN AIRPORT","IL",3960,"KCPS","CPS",38.57_r8,-90.15_r8, &
        19970529,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(287) = asosdata("CARBONDALE/MURPHYBORO","SOUTHERN ILLINOIS AIRPORT","IL",93810,"KMDH","MDH",37.77_r8,-89.24_r8, &
        19971120,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(288) = asosdata("CHAMPAIGN/URBANA","UNIVERSITY OF ILLINOIS-WILLARD AIRPORT","IL",94870,"KCMI","CMI",40.03_r8, &
        -88.27_r8,19970424,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(289) = asosdata("CHICAGO","CHICAGO MIDWAY INTERNATIONAL AIRPORT","IL",14819,"KMDW","MDW",41.78_r8,-87.75_r8, &
        19970410,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(290) = asosdata("CHICAGO","CHICAGO O'HARE INTERNATIONAL AIRPORT","IL",94846,"KORD","ORD",41.98_r8,-87.91_r8, &
        19960201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(291) = asosdata("CHICAGO/AURORA","AURORA MUNICIPAL AIRPORT","IL",4808,"KARR","ARR",41.77_r8,-88.48_r8,19981008, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(292) = asosdata("CHICAGO/PROSPECT HEIGHTS/WHEELING","PALWAUKEE MUNICIPAL AIRPORT","IL",4838,"KPWK","PWK", &
        42.12_r8,-87.9_r8,19960417,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(293) = asosdata("CHICAGO/WAUKEGAN","WAUKEGAN REGIONAL AIRPORT","IL",14880,"KUGN","UGN",42.42_r8,-87.86_r8, &
        19990901,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(294) = asosdata("CHICAGO/WEST CHICAGO","DUPAGE AIRPORT","IL",94892,"KDPA","DPA",41.91_r8,-88.24_r8,19970424, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(295) = asosdata("DECATUR","DECATUR AIRPORT","IL",3887,"KDEC","DEC",39.98_r8,-88.86_r8,20001129,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(296) = asosdata("LAWRENCEVILLE","LAWRENCEVILLE-VINCENNES INTERNATIONAL AIRPORT","IL",13809,"KLWV","LWV", &
        38.76_r8,-87.6_r8,19960918,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(297) = asosdata("MATTOON/CHARLESTON","COLES COUNTY MEMORIAL AIRPORT","IL",53802,"KMTO","MTO",39.47_r8,-88.28_r8,&
        19971204,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(298) = asosdata("MOLINE","QUAD CITY INTERNATIONAL AIRPORT","IL",14923,"KMLI","MLI",41.46_r8,-90.52_r8,19950701, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(299) = asosdata("PEORIA","GREATER PEORIA REGIONAL AIRPORT","IL",14842,"KPIA","PIA",40.66_r8,-89.68_r8,19951001, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(300) = asosdata("QUINCY","QUINCY REGIONAL-BALDWIN FIELD AIRPORT","IL",93989,"KUIN","UIN",39.94_r8,-91.19_r8, &
        20000906,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(301) = asosdata("ROCKFORD","GREATER ROCKFORD AIRPORT","IL",94822,"KRFD","RFD",42.19_r8,-89.09_r8,19950701,"YES",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(302) = asosdata("SPRINGFIELD","ABRAHAM LINCOLN CAPITAL AIRPORT","IL",93822,"KSPI","SPI",39.84_r8,-89.68_r8, &
        19951201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(303) = asosdata("BLOOMINGTON","MONROE COUNTY AIRPORT","IN",3893,"KBMG","BMG",39.14_r8,-86.61_r8,19980312,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(304) = asosdata("EVANSVILLE","EVANSVILLE REGIONAL AIRPORT","IN",93817,"KEVV","EVV",38.04_r8,-87.53_r8,19960201, &
        "YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(305) = asosdata("FORT WAYNE","FORT WAYNE INTERNATIONAL AIRPORT","IN",14827,"KFWA","FWA",41._r8,-85.2_r8, &
        19960701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(306) = asosdata("GOSHEN","GOSHEN MUNICIPAL AIRPORT","IN",14829,"KGSH","GSH",41.52_r8,-85.79_r8,19960926,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(307) = asosdata("INDIANAPOLIS","EAGLE CREEK AIRPARK AIRPORT","IN",53842,"KEYE","EYE",39.82_r8,-86.29_r8, &
        19960215,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(308) = asosdata("INDIANAPOLIS","INDIANAPOLIS INTERNATIONAL AIRPORT","IN",93819,"KIND","IND",39.71_r8,-86.27_r8, &
        19960101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(309) = asosdata("LAFAYETTE","PURDUE UNIVERSITY AIRPORT","IN",14835,"KLAF","LAF",40.41_r8,-86.93_r8,19980115, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(310) = asosdata("MUNCIE","DELAWARE COUNTY-JOHNSON FIELD AIRPORT","IN",94895,"KMIE","MIE",40.23_r8,-85.39_r8, &
        19990429,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(311) = asosdata("SHELBYVILLE","SHELBYVILLE MUNICIPAL AIRPORT","IN",53866,"KGEZ","GEZ",39.57_r8,-85.8_r8, &
        19980625,"NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(312) = asosdata("SOUTH BEND","SOUTH BEND REGIONAL AIRPORT","IN",14848,"KSBN","SBN",41.7_r8,-86.33_r8,19960701, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(313) = asosdata("TERRE HAUTE","TERRE HAUTE INTERNATIONAL-HULMAN FIELD AIRPORT","IN",93823,"KHUF","HUF",39.45_r8,&
        -87.3_r8,19980305,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(314) = asosdata("VALPARAISO","PORTER COUNTY MUNICIPAL AIRPORT","IN",4846,"KVPZ","VPZ",41.45_r8,-87._r8,19971113,&
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(315) = asosdata("CHANUTE","CHANUTE MARTIN JOHNSON AIRPORT","KS",13981,"KCNU","CNU",37.67_r8,-95.48_r8,19960516, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(316) = asosdata("COFFEYVILLE","COFFEYVILLE MUNICIPAL AIRPORT","KS",93967,"KCFV","CFV",37.09_r8,-95.56_r8, &
        19960417,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(317) = asosdata("CONCORDIA","BLOSSER MUNICIPAL AIRPORT","KS",13984,"KCNK","CNK",39.54_r8,-97.65_r8,19920901, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(318) = asosdata("DODGE CITY","DODGE CITY REGIONAL AIRPORT","KS",13985,"KDDC","DDC",37.77_r8,-99.96_r8,19920901, &
        "YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(319) = asosdata("EMPORIA","EMPORIA MUNICIPAL AIRPORT","KS",13989,"KEMP","EMP",38.33_r8,-96.18_r8,19960215,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(320) = asosdata("GARDEN CITY","GARDEN CITY REGIONAL AIRPORT","KS",23064,"KGCK","GCK",37.92_r8,-100.72_r8, &
        19961217,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(321) = asosdata("GOODLAND","RENNER FIELD/GOODLAND MUNICIPAL AIRPORT","KS",23065,"KGLD","GLD",39.36_r8, &
        -101.69_r8,19920901,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(322) = asosdata("HILL CITY","HILL CITY MUNICIPAL AIRPORT","KS",93990,"KHLC","HLC",39.37_r8,-99.82_r8,19960516, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(323) = asosdata("HUTCHINSON","HUTCHINSON MUNICIPAL AIRPORT","KS",13986,"KHUT","HUT",38.06_r8,-97.86_r8,19960620,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(324) = asosdata("LAWRENCE","LAWRENCE MUNICIPAL AIRPORT","KS",3997,"KLWC","LWC",39._r8,-95.21_r8,19960417,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(325) = asosdata("MANHATTAN","MANHATTAN REGIONAL AIRPORT","KS",3936,"KMHK","MHK",39.13_r8,-96.67_r8,19960215, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(326) = asosdata("MEDICINE LODGE","MEDICINE LODGE","KS",3957,"KP28","P28",37.28_r8,-98.55_r8,19980205,"NO","NWS",&
        33.00_r8,10.05_r8)
    asoscommdates(327) = asosdata("OLATHE","NEW CENTURY AIRCENTER AIRPORT","KS",93909,"KIXD","IXD",38.83_r8,-94.88_r8,19970410, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(328) = asosdata("OLATHE","JOHNSON COUNTY EXECUTIVE AIRPORT","KS",3967,"KOJC","OJC",38.85_r8,-94.73_r8,19960620, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(329) = asosdata("PARSONS","TRI-CITY AIRPORT","KS",3998,"KPPF","PPF",37.32_r8,-95.5_r8,19960516,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(330) = asosdata("RUSSELL","RUSSELL MUNICIPAL AIRPORT","KS",93997,"KRSL","RSL",38.87_r8,-98.82_r8,19951214,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(331) = asosdata("SALINA","SALINA MUNICIPAL AIRPORT","KS",3919,"KSLN","SLN",38.81_r8,-97.66_r8,19951214,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(332) = asosdata("TOPEKA","FORBES FIELD AIRPORT","KS",13920,"KFOE","FOE",38.95_r8,-95.66_r8,19971119,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(333) = asosdata("TOPEKA","PHILIP BILLARD MUNICIPAL AIRPORT","KS",13996,"KTOP","TOP",39.07_r8,-95.62_r8,19921201,&
        "YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(334) = asosdata("WICHITA","COLONEL JAMES JABARA AIRPORT","KS",3974,"KAAO","AAO",37.74_r8,-97.21_r8,19951114, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(335) = asosdata("WICHITA","WICHITA MID-CONTINENT AIRPORT","KS",3928,"KICT","ICT",37.64_r8,-97.42_r8,19921101, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(336) = asosdata("WINFIELD/ARKANSAS CITY","STROTNER FIELD AIRPORT","KS",13932,"KWLD","WLD",37.16_r8,-97.03_r8, &
        19951214,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(337) = asosdata("BOWLING GREEN","BOWLING GREEN-WARREN COUNTY REGIONAL AIRPORT","KY",93808,"KBWG","BWG",36.98_r8,&
        -86.43_r8,19960206,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(338) = asosdata("FRANKFORT","CAPITAL CITY AIRPORT","KY",53841,"KFFT","FFT",38.18_r8,-84.9_r8,19951214,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(339) = asosdata("JACKSON","JULIAN CARROLL AIRPORT","KY",3889,"KJKL","JKL",37.59_r8,-83.31_r8,19951201,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(340) = asosdata("LEXINGTON","BLUE GRASS AIRPORT","KY",93820,"KLEX","LEX",38.04_r8,-84.6_r8,19960301,"YES","NWS",&
        33.00_r8,10.05_r8)
    asoscommdates(341) = asosdata("LONDON","LONDON-CORBIN AIRPORT-MAGEE FIELD AIRPORT","KY",3849,"KLOZ","LOZ",37.08_r8,-84.07_r8, &
        19960918,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(342) = asosdata("LOUISVILLE","BOWMAN FIELD AIRPORT","KY",13810,"KLOU","LOU",38.22_r8,-85.66_r8,20000906,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(343) = asosdata("LOUISVILLE","LOUISVILLE INTERNATIONAL-STANDIFORD FIELD AIRPORT","KY",93821,"KSDF","SDF", &
        38.17_r8,-85.72_r8,19940801,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(344) = asosdata("PADUCAH","BARKLEY REGIONAL AIRPORT","KY",3816,"KPAH","PAH",37.05_r8,-88.77_r8,19950801,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(345) = asosdata("ALEXANDRIA","ALEXANDRIA INTERNATIONAL AIRPORT","LA",93915,"KAEX","AEX",31.33_r8,-92.55_r8, &
        19990720,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(346) = asosdata("ALEXANDRIA","ESLER REGIONAL AIRPORT","LA",13935,"KESF","ESF",31.39_r8,-92.29_r8,19960430,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(347) = asosdata("BATON ROUGE","BATON ROUGE METROPOLITAN; RYAN FIELD AIRPORT","LA",13970,"KBTR","BTR",30.53_r8, &
        -91.14_r8,19930501,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(348) = asosdata("BOOTHVILLE","BOOTHVILLE","LA",12884,"KBVE","BVE",29.34_r8,-89.4_r8,19990630,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(349) = asosdata("LAFAYETTE","LAFAYETTE REGIONAL AIRPORT","LA",13976,"KLFT","LFT",30.2_r8,-91.98_r8,19980825, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(350) = asosdata("LAKE CHARLES","LAKE CHARLES REGIONAL AIRPORT","LA",3937,"KLCH","LCH",30.12_r8,-93.22_r8, &
        19960101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(351) = asosdata("MONROE","MONROE REGIONAL AIRPORT","LA",13942,"KMLU","MLU",32.51_r8,-92.03_r8,19980624,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(352) = asosdata("NEW IBERIA","ACADIANA REGIONAL AIRPORT","LA",53915,"KARA","ARA",30.05_r8,-91.88_r8,19981105, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(353) = asosdata("NEW ORLEANS","LOUIS ARMSTRONG NEW ORLEANS INTERNATIONAL AIRPORT","LA",12916,"KMSY","MSY", &
        29.99_r8,-90.25_r8,19960501,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(354) = asosdata("NEW ORLEANS","LAKEFRONT AIRPORT","LA",53917,"KNEW","NEW",30.04_r8,-90.02_r8,19980812,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(355) = asosdata("SALT POINT","SALT POINT","LA",12968,"KP92","P92",29.56_r8,-91.52_r8,19980505,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(356) = asosdata("SHREVEPORT","SHREVEPORT DOWNTOWN AIRPORT","LA",53905,"KDTN","DTN",32.53_r8,-93.74_r8,19970501, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(357) = asosdata("SHREVEPORT","SHREVEPORT REGIONAL AIRPORT","LA",13957,"KSHV","SHV",32.44_r8,-93.82_r8,19951001, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(358) = asosdata("SLIDELL","SLIDELL AIRPORT","LA",53865,"KASD","ASD",30.34_r8,-89.82_r8,19980624,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(359) = asosdata("BEDFORD","LAURENCE G HANSCOM FIELD AIRPORT","MA",14702,"KBED","BED",42.47_r8,-71.28_r8, &
        19980819,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(360) = asosdata("BEVERLY","BEVERLY MUNICIPAL AIRPORT","MA",54733,"KBVY","BVY",42.58_r8,-70.91_r8,19981209,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(361) = asosdata("BOSTON","GENERAL E.L. LOGAN INTERNATIONAL AIRPORT","MA",14739,"KBOS","BOS",42.36_r8,-71.01_r8, &
        19960401,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(362) = asosdata("CHATHAM","CHATHAM MUNICIPAL AIRPORT","MA",94624,"KCQX","CQX",41.68_r8,-69.99_r8,19950614,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(363) = asosdata("FITCHBURG","FITCHBURG MUNICIPAL AIRPORT","MA",4780,"KFIT","FIT",42.55_r8,-71.75_r8,19970917, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(364) = asosdata("HYANNIS","BARNSTABLE MUNICIPAL-BOARDMAN/POLANDO FIELD AIRPORT","MA",94720,"KHYA","HYA", &
        41.66_r8,-70.28_r8,19970219,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(365) = asosdata("LAWRENCE","LAWRENCE MUNICIPAL AIRPORT","MA",94723,"KLWM","LWM",42.71_r8,-71.12_r8,19970515, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(366) = asosdata("MILTON","BLUE HILL OBSERVATORY","MA",14753,"KMQE","MQE",42.21_r8,-71.11_r8,19981015,"YES", &
        "NWS",14.00_r8,4.26_r8)
    asoscommdates(367) = asosdata("NANTUCKET","NANTUCKET MEMORIAL AIRPORT","MA",14756,"KACK","ACK",41.25_r8,-70.06_r8,19970616, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(368) = asosdata("NEW BEDFORD","NEW BEDFORD REGIONAL AIRPORT","MA",94726,"KEWB","EWB",41.67_r8,-70.95_r8, &
        19960320,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(369) = asosdata("NORTH ADAMS","HARRIMAN-AND-WEST AIRPORT","MA",54768,"KAQW","AQW",42.70_r8,-73.17_r8,19980505, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(370) = asosdata("NORWOOD","NORWOOD MEMORIAL AIRPORT","MA",54704,"KOWD","OWD",42.19_r8,-71.17_r8,19980603,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(371) = asosdata("ORANGE","ORANGE MUNICIPAL AIRPORT","MA",54756,"KORE","ORE",42.57_r8,-72.29_r8,19950524,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(372) = asosdata("PITTSFIELD","PITTSFIELD MUNICIPAL AIRPORT","MA",14763,"KPSF","PSF",42.42_r8,-73.28_r8,19990120,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(373) = asosdata("PLYMOUTH","PLYMOUTH MUNICIPAL AIRPORT","MA",54769,"KPYM","PYM",41.9_r8,-70.72_r8,19950512,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(374) = asosdata("TAUNTON","TAUNTON MUNICIPAL AIRPORT","MA",54777,"KTAN","TAN",41.87_r8,-71.02_r8,19971105,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(375) = asosdata("VINEYARD HAVEN","MARTHAS VINEYARD AIRPORT","MA",94724,"KMVY","MVY",41.39_r8,-70.61_r8,19970617,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(376) = asosdata("WESTFIELD/SPRINGFIELD","BARNES MUNICIPAL AIRPORT","MA",14775,"KBAF","BAF",42.15_r8,-72.71_r8, &
        19980805,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(377) = asosdata("WORCESTER","WORCHESTER REGIONAL AIRPORT","MA",94746,"KORH","ORH",42.26_r8,-71.87_r8,19950701, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(378) = asosdata("BALTIMORE","BALTIMORE-WASHINGTON INTERNATIONAL AIRPORT","MD",93721,"KBWI","BWI",39.17_r8, &
        -76.68_r8,19960401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(379) = asosdata("BALTIMORE","BALTIMORE DOWNTOWN","MD",93784,"KDMH","DMH",39.28_r8,-76.61_r8,19980429,"NO","NWS",&
        0.00_r8,0.00_r8)
    asoscommdates(380) = asosdata("HAGERSTOWN","HAGERSTOWN REGIONAL - RICHARD A HENSON FIELD ARPT","MD",93706,"KHGR","HGR", &
        39.7_r8,-77.72_r8,19981001,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(381) = asosdata("OCEAN CITY","OCEAN CITY MUNICIPAL AIRPORT","MD",93786,"KOXB","OXB",38.3_r8,-75.12_r8,19990901, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(382) = asosdata("SALISBURY","SALISBURY-OCEAN CITY WICOMICO REGIONAL AIRPORT","MD",93720,"KSBY","SBY",38.34_r8, &
        -75.51_r8,20010509,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(383) = asosdata("AUGUSTA","AUGUSTA STATE AIRPORT","ME",14605,"KAUG","AUG",44.32_r8,-69.79_r8,20010110,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(384) = asosdata("BANGOR","BANGOR INTERNATIONAL AIRPORT","ME",14606,"KBGR","BGR",44.8_r8,-68.82_r8,19980401,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(385) = asosdata("CARIBOU","CARIBOU MUNICIPAL AIRPORT","ME",14607,"KCAR","CAR",46.86_r8,-68.03_r8,19960801,"YES",&
        "NWS",26.00_r8,7.92_r8)
    asoscommdates(386) = asosdata("FRENCHVILLE","NORTHERN AROOSTOOK REGIONAL AIRPORT","ME",4836,"KFVE","FVE",47.28_r8,-68.3_r8, &
        19950531,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(387) = asosdata("FRYEBURG","EASTERN SLOPES REGIONAL AIRPORT","ME",54772,"KIZG","IZG",43.99_r8,-70.94_r8, &
        19951206,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(388) = asosdata("GREENVILLE","MAINE FOREST SERVICE","ME",94626,"KGNR","GNR",45.46_r8,-69.59_r8,19980304,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(389) = asosdata("HOULTON","HOULTON INTERNATIONAL AIRPORT","ME",14609,"KHUL","HUL",46.12_r8,-67.79_r8,20000913, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(390) = asosdata("MILLINOCKET","MILLINOCKET MUNICIPAL AIRPORT","ME",14610,"KMLT","MLT",45.64_r8,-68.68_r8, &
        19950920,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(391) = asosdata("PORTLAND","PORTLAND INTERNATIONAL JETPORT AIRPORT","ME",14764,"KPWM","PWM",43.64_r8,-70.3_r8, &
        19940801,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(392) = asosdata("WISCASSET","WISCASSET AIRPORT","ME",94623,"KIWI","IWI",43.96_r8,-69.71_r8,19950428,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(393) = asosdata("ADRIAN","ADRIAN LENAWEE COUNTY AIRPORT","MI",4847,"KADG","ADG",41.86_r8,-84.07_r8,19971217, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(394) = asosdata("ALPENA","ALPENA COUNTY REGIONAL AIRPORT","MI",94849,"KAPN","APN",45.08_r8,-83.17_r8,19950614, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(395) = asosdata("ANN ARBOR","ANN ARBOR MUNICIPAL AIRPORT","MI",94889,"KARB","ARB",42.22_r8,-83.74_r8,19960401, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(396) = asosdata("BATTLE CREEK","W.K.KELLOGG AIRPORT","MI",14815,"KBTL","BTL",42.3_r8,-85.25_r8,19980212,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(397) = asosdata("BENTON HARBOR","SOUTHWEST MICHIGAN REGIONAL AIRPORT","MI",94871,"KBEH","BEH",42.12_r8, &
        -86.42_r8,19960619,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(398) = asosdata("COPPER HARBOR","COPPER HARBOR","MI",94899,"KP59","P59",47.46_r8,-87.87_r8,19980730,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(399) = asosdata("DETROIT","DETROIT CITY AIRPORT","MI",14822,"KDET","DET",42.4_r8,-83.01_r8,20001019,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(400) = asosdata("DETROIT","DETROIT METROPOLITAN WAYNE COUNTY AIRPORT","MI",94847,"KDTW","DTW",42.21_r8, &
        -83.34_r8,19950701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(401) = asosdata("DETROIT","WILLOW RUN AIRPORT","MI",14853,"KYIP","YIP",42.23_r8,-83.53_r8,19990311,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(402) = asosdata("FLINT","BISHOP INTERNATIONAL AIRPORT","MI",14826,"KFNT","FNT",42.96_r8,-83.74_r8,19950601, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(403) = asosdata("GAYLORD","OTSEGO COUNTY AIRPORT","MI",4854,"KGLR","GLR",45.01_r8,-84.7_r8,19980326,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(404) = asosdata("GRAND RAPTIDS","GERALD R. FORD INTERNATIONAL AIRPORT","MI",94860,"KGRR","GRR",42.88_r8, &
        -85.52_r8,19950801,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(405) = asosdata("HANCOCK","HOUGHTON COUNTY MEMORIAL AIRPORT","MI",14858,"KCMX","CMX",47.16_r8,-88.5_r8,20000914, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(406) = asosdata("HARBOR BEACH","HARBOR BEACH","MI",94898,"KP58","P58",44.02_r8,-82.79_r8,19990624,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(407) = asosdata("HOLLAND","TULIP CITY AIRPORT","MI",4839,"KBIV","BIV",42.74_r8,-86.09_r8,19960621,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(408) = asosdata("HOUGHTON LAKE","ROSCOMMON COUNTY AIRPORT","MI",94814,"KHTL","HTL",44.36_r8,-84.69_r8,19960401, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(409) = asosdata("IRON MOUNTAIN/KINGSFORD","FORD AIRPORT","MI",94893,"KIMT","IMT",45.81_r8,-88.11_r8,19960910, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(410) = asosdata("JACKSON","JACKSON COUNTY-REYNOLDS FIELD AIRPORT","MI",14833,"KJXN","JXN",42.25_r8,-84.45_r8, &
        20001013,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(411) = asosdata("KALAMAZOO","KALAMAZOO/BATTLE CREEK INTERNATIONAL AIRPORT","MI",94815,"KAZO","AZO",42.23_r8, &
        -85.55_r8,19980115,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(412) = asosdata("LANSING","CAPITAL CITY AIRPORT","MI",14836,"KLAN","LAN",42.78_r8,-84.57_r8,19960601,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(413) = asosdata("MUNISING","MUNISING LAKESHORE","MI",54813,"KP53","P53",46.42_r8,-86.65_r8,20041201,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(414) = asosdata("MUSKEGON","MUSKEGON COUNTY AIRPORT","MI",14840,"KMKG","MKG",43.17_r8,-86.23_r8,19960501,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(415) = asosdata("PELLSTON","PELLSTON REGIONAL AIRPORT OF EMMET COUNTY AIRPORT","MI",14841,"KPLN","PLN",45.57_r8,&
        -84.79_r8,20000824,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(416) = asosdata("PONTIAC","OAKLAND COUNTY INTERNATIONAL AIRPORT","MI",94817,"KPTK","PTK",42.66_r8,-83.41_r8, &
        19980820,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(417) = asosdata("SAGINAW","MBS INTERNATIONAL AIRPORT","MI",14845,"KMBS","MBS",43.53_r8,-84.07_r8,19980909,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(418) = asosdata("SAULT STE MARIE","SAULT STE MARIE MUNI/SANDERSON FIELD AIRPORT","MI",14847,"KANJ","ANJ", &
        46.46_r8,-84.36_r8,19970101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(419) = asosdata("TRAVERSE CITY","CHERRY CAPITAL AIRPORT","MI",14850,"KTVC","TVC",44.74_r8,-85.58_r8,19980611, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(420) = asosdata("ALEXANDRIA","CHANDLER FIELD AIRPORT","MN",14910,"KAXN","AXN",45.88_r8,-95.39_r8,19951214,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(421) = asosdata("BAUDETTE","BAUDETTE INTERNATIONAL AIRPORT","MN",94961,"KBDE","BDE",48.72_r8,-94.61_r8,19950701,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(422) = asosdata("BRAINERD","BRAINERD LAKES REGIONAL AIRPORT","MN",94938,"KBRD","BRD",46.4_r8,-94.13_r8,19951024,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(423) = asosdata("DULUTH","DULUTH INTERNATIONAL AIRPORT","MN",14913,"KDLH","DLH",46.84_r8,-92.19_r8,19960401, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(424) = asosdata("GRAND MARAIS","GRAND MARAIS","MN",94992,"KGNA","GNA",47.74_r8,-90.34_r8,19980730,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(425) = asosdata("HIBBING","CHISHOLM-HIBBING AIRPORT","MN",94931,"KHIB","HIB",47.38_r8,-92.83_r8,20000824,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(426) = asosdata("INTERNATIONAL FALLS","FALLS INTERNATIONAL AIRPORT","MN",14918,"KINL","INL",48.56_r8,-93.4_r8, &
        19961101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(427) = asosdata("MINNEAPOLIS","FLYING CLOUD AIRPORT","MN",94963,"KFCM","FCM",44.83_r8,-93.47_r8,19971002,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(428) = asosdata("MINNEAPOLIS","CRYSTAL AIRPORT","MN",94960,"KMIC","MIC",45.06_r8,-93.35_r8,19970911,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(429) = asosdata("MINNEAPOLIS","MINNEAPOLIS-ST PAUL INTL/WORLD-CHAMBERLAIN AIRPORT","MN",14922,"KMSP","MSP", &
        44.88_r8,-93.22_r8,19960601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(430) = asosdata("PARK RAPIDS","PARK RAPIDS MUNICIPAL-KONSHOK FIELD AIRPORT","MN",94967,"KPKD","PKD",46.9_r8, &
        -95.06_r8,19950701,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(431) = asosdata("REDWOOD FALLS","REDWOOD FALLS MUNICIPAL AIRPORT","MN",14992,"KRWF","RWF",44.54_r8,-95.08_r8, &
        20000824,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(432) = asosdata("ROCHESTER","ROCHESTER INTERNATIONAL AIRPORT","MN",14925,"KRST","RST",43.9_r8,-92.49_r8, &
        19960601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(433) = asosdata("ST CLOUD","ST CLOUD REGIONAL AIRPORT","MN",14926,"KSTC","STC",45.54_r8,-94.05_r8,19950601, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(434) = asosdata("ST PAUL","ST PAUL DOWNTOWN HOLMAN FIELD AIRPORT","MN",14927,"KSTP","STP",44.93_r8,-93.04_r8, &
        19960628,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(435) = asosdata("CAPE GIRARDEAU","CAPE GIRARDEAU REGIONAL AIRPORT","MO",3935,"KCGI","CGI",37.22_r8,-89.57_r8, &
        19970305,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(436) = asosdata("CHILLICOTHE","AGRICULTURAL-SCIENCE CENTER ","MO",53916,"KCDJ","CDJ",39.82_r8,-93.57_r8, &
        19980521,"NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(437) = asosdata("COLUMBIA","COLUMBIA REGIONAL AIRPORT","MO",3945,"KCOU","COU",38.81_r8,-92.21_r8,19950901,"YES",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(438) = asosdata("JEFFERSON CITY","JEFFERSON CITY MEMORIAL AIRPORT","MO",3963,"KJEF","JEF",38.59_r8,-92.15_r8, &
        19970410,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(439) = asosdata("JOPLIN","JOPLIN REGIONAL AIRPORT","MO",13987,"KJLN","JLN",37.14_r8,-94.49_r8,19980226,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(440) = asosdata("KANSAS CITY","KANSAS CITY INTERNATIONAL AIRPORT","MO",3947,"KMCI","MCI",39.29_r8,-94.71_r8, &
        19950701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(441) = asosdata("KANSAS CITY","CHARLES B WHEELER DOWNTOWN AIRPORT","MO",13988,"KMKC","MKC",39.12_r8,-94.59_r8, &
        19971113,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(442) = asosdata("KIRKSVILLE","KIRKSVILLE REGIONAL AIRPORT","MO",14938,"KIRK","IRK",40.09_r8,-92.54_r8,19980611, &
        "NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(443) = asosdata("LEE'S SUMMIT","LEE'S SUMMIT MUNICIPAL AIRPORT","MO",53879,"KLXT","LXT",38.95_r8,-94.37_r8, &
        20020418,"NO","FAA",-9._r8,-9._r8)
    asoscommdates(444) = asosdata("POPLAR BLUFF","POPLAR BLUFF MUNICIPAL AIRPORT","MO",3975,"KPOF","POF",36.77_r8,-90.32_r8, &
        19971120,"NO","NWS",26.00_r8,7.92_r8)
    asoscommdates(445) = asosdata("ROLLA/VICHY","ROLLA NATIONAL AIRPORT","MO",13997,"KVIH","VIH",38.12_r8,-91.76_r8,19961112,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(446) = asosdata("SEDALIA","SEDALIA MEMORIAL AIRPORT","MO",3994,"KDMO","DMO",38.7_r8,-93.18_r8,19951024,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(447) = asosdata("SPRINGFIELD","SPRINGFIELD-BRANSON REGIONAL AIRPORT","MO",13995,"KSGF","SGF",37.23_r8,-93.38_r8,&
        19951101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(448) = asosdata("ST CHARLES","ST CHARLES COUNTY SMARTT AIRPORT","MO",53904,"KSET","SET",38.92_r8,-90.42_r8, &
        19970326,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(449) = asosdata("ST JOSEPH","ROSECRANS MEMORIAL AIRPORT","MO",13993,"KSTJ","STJ",39.77_r8,-94.9_r8,19960919, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(450) = asosdata("ST LOUIS","LAMBERT-ST LOUIS INTERNATIONAL AIRPORT","MO",13994,"KSTL","STL",38.75_r8,-90.37_r8, &
        19960601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(451) = asosdata("ST LOUIS","SPIRIT OF ST LOUIS AIRPORT","MO",3966,"KSUS","SUS",38.65_r8,-90.65_r8,19980115,"NO",&
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(452) = asosdata("WEST PLAINS","WEST PLAINS MUNICIPAL AIRPORT","MO",53901,"KUNO","UNO",36.87_r8,-91.9_r8, &
        19960815,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(453) = asosdata("GREENVILLE","MID DELTA REGIONAL AIRPORT","MS",13939,"KGLH","GLH",33.48_r8,-90.98_r8,20010815, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(454) = asosdata("GREENWOOD","GREENWOOD-LEFLORE AIRPORT","MS",13978,"KGWO","GWO",33.49_r8,-90.08_r8,19970603, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(455) = asosdata("GULFPORT","GULFPORT-BILOXI INTERNATIONAL AIRPORT","MS",93874,"KGPT","GPT",30.4_r8,-89.07_r8, &
        19980812,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(456) = asosdata("HATTIESBURG","BOBBY L CHAIN MUNICIPAL AIRPORT","MS",13833,"KHBG","HBG",31.26_r8,-89.25_r8, &
        20000510,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(457) = asosdata("JACKSON","HAWKINS FIELD AIRPORT","MS",13927,"KHKS","HKS",32.33_r8,-90.22_r8,20000720,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(458) = asosdata("JACKSON","JACKSON INTERNATIONAL AIRPORT","MS",3940,"KJAN","JAN",32.31_r8,-90.07_r8,19930701, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(459) = asosdata("MCCOMB","M! COMB/PIKE COUNTY/JOHN E LEWIS FIELD AIRPORT","MS",93919,"KMCB","MCB",31.17_r8, &
        -90.47_r8,20000927,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(460) = asosdata("MERIDIAN","KEY FIELD AIRPORT","MS",13865,"KMEI","MEI",32.33_r8,-88.75_r8,19950701,"YES","NWS",&
        33.00_r8,10.05_r8)
    asoscommdates(461) = asosdata("PASCAGOULA","TRENT LOTT INTERNATIONAL AIRPORT","MS",53858,"KPQL","PQL",30.46_r8,-88.53_r8, &
        19970814,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(462) = asosdata("TALLULAH/VICKSBURG","VICKSBURG/TALLULAH REGIONAL AIRPORT","MS",3996,"KTVR","TVR",32.34_r8, &
        -91.03_r8,19960229,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(463) = asosdata("TUPELO","TUPELO REGIONAL AIRPORT","MS",93862,"KTUP","TUP",34.26_r8,-88.77_r8,19930601,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(464) = asosdata("BAKER","BAKER  MUNICIPAL AIRPORT","MT",94055,"KBHK","BHK",46.35_r8,-104.25_r8,19980219,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(465) = asosdata("BILLINGS","BILLINGS LOGAN INTERNATIONAL AIRPORT","MT",24033,"KBIL","BIL",45.8_r8,-108.54_r8, &
        19950501,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(466) = asosdata("BOZEMAN","GALLATIN FIELD AIRPORT","MT",24132,"KBZN","BZN",45.79_r8,-111.15_r8,19950615,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(467) = asosdata("BUTTE","BERT MOONEY AIRPORT","MT",24135,"KBTM","BTM",45.95_r8,-112.51_r8,20001109,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(468) = asosdata("CUT BANK","CUT BANK MUNICIPAL AIRPORT","MT",24137,"KCTB","CTB",48.6_r8,-112.37_r8,20010117, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(469) = asosdata("DILLON","DILLON AIRPORT","MT",24138,"KDLN","DLN",45.25_r8,-112.55_r8,19970514,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(470) = asosdata("GLASGOW","WOKAL FIELD/GLASGOW INTERNATIONAL AIRPORT","MT",94008,"KGGW","GGW",48.21_r8, &
        -106.62_r8,19940401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(471) = asosdata("GREAT FALLS","GREAT FALLS INTERNATIONAL AIRPORT","MT",24143,"KGTF","GTF",47.47_r8,-111.38_r8, &
        19940801,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(472) = asosdata("HAVRE","HAVRE CITY-COUNTY AIRPORT","MT",94012,"KHVR","HVR",48.55_r8,-109.78_r8,19940401,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(473) = asosdata("HELENA","HELENA REGIONAL AIRPORT","MT",24144,"KHLN","HLN",46.6_r8,-111.96_r8,19941101,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(474) = asosdata("JORDAN","JORDAN AIRPORT","MT",94051,"KJDN","JDN",47.32_r8,-106.94_r8,19970814,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(475) = asosdata("KALISPELL","GLACIER PARK INTERNATIONAL AIRPORT","MT",24146,"KGPI","GPI",48.3_r8,-114.26_r8, &
        19940201,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(476) = asosdata("LEWISTOWN","LEWISTOWN MUNICIPAL AIRPORT","MT",24036,"KLWT","LWT",47.04_r8,-109.46_r8,20001214, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(477) = asosdata("LIVINGSTON","MISSION FIELD AIRPORT","MT",24150,"KLVM","LVM",45.69_r8,-110.44_r8,20001025,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(478) = asosdata("MILES","FRANK WILEY FIELD AIRPORT","MT",24037,"KMLS","MLS",46.42_r8,-105.88_r8,20001102,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(479) = asosdata("MISSOULA","MISSOULA INTERNATIONAL AIRPORT","MT",24153,"KMSO","MSO",46.92_r8,-114.09_r8, &
        19960901,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(480) = asosdata("WOLF POINT","L M CLAYTON AIRPORT","MT",94017,"KOLF","OLF",48.09_r8,-105.57_r8,19980917,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(481) = asosdata("ASHEVILLE","ASHEVILLE REGIONAL AIRPORT","NC",3812,"KAVL","AVL",35.43_r8,-82.53_r8,19960601, &
        "YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(482) = asosdata("BEAUFORT","MICHAEL J. SMITH FIELD AIRPORT","NC",93765,"KMRH","MRH",34.73_r8,-76.66_r8,20000426,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(483) = asosdata("BURLINGTON","BURLINGTON ALAMANCE REGIONAL AIRPORT","NC",93783,"KBUY","BUY",36.04_r8,-79.47_r8, &
        19980701,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(484) = asosdata("CAPE HATTERAS","BILLY MITCHEL AIRPORT","NC",93729,"KHSE","HSE",35.23_r8,-75.62_r8,19950701, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(485) = asosdata("CHAPEL HILL","HORACE WILLIAMS AIRPORT","NC",93785,"KIGX","IGX",35.93_r8,-79.06_r8,19990714, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(486) = asosdata("CHARLOTTE","CHARLOTTE/DOUGLAS INTERNATIONAL AIRPORT","NC",13881,"KCLT","CLT",35.21_r8, &
        -80.94_r8,19980701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(487) = asosdata("ELIZABETH CITY","ELIZABETH CITY CG AIR STATION/REGIONAL AIRPORT","NC",13786,"KECG","ECG", &
        36.26_r8,-76.17_r8,19980325,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(488) = asosdata("FAYETTEVILLE","FAYETTEVILLE REGIONAL/GRANNIS FIELD AIRPORT","NC",93740,"KFAY","FAY",34.99_r8, &
        -78.88_r8,19980415,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(489) = asosdata("GASTONIA","GASTONIA MUNICIPAL AIRPORT","NC",53870,"KAKH","AKH",35.19_r8,-81.15_r8,19990120, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(490) = asosdata("GREENSBORO","PIEDMONT TRIAD INTERNATIONAL AIRPORT","NC",13723,"KGSO","GSO",36.09_r8,-79.94_r8, &
        19951001,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(491) = asosdata("HICKORY","HICKORY REGIONAL AIRPORT","NC",3810,"KHKY","HKY",35.74_r8,-81.38_r8,19970905,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(492) = asosdata("LUMBERTON","LUMBERTON MUNICIPAL AIRPORT","NC",13776,"KLBT","LBT",34.61_r8,-79.05_r8,19980916, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(493) = asosdata("MAXTON","LAURINBURG-MAXTON AIRPORT","NC",93782,"KMEB","MEB",34.79_r8,-79.36_r8,19980624,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(494) = asosdata("MONROE","MONROE AIRPORT","NC",53872,"KEQY","EQY",35.01_r8,-80.62_r8,19990127,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(495) = asosdata("NEW BERN","CRAVEN COUNTY REGIONAL AIRPORT","NC",93719,"KEWN","EWN",35.06_r8,-77.04_r8,19970912,&
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(496) = asosdata("RALEIGH/DURHAM","RALEIGH-DURHAM INTERNATIONAL AIRPORT","NC",13722,"KRDU","RDU",35.87_r8, &
        -78.78_r8,19960201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(497) = asosdata("ROANOKE RAPIDS","HALIFAX COUNTY AIRPORT","NC",93781,"KRZZ","RZZ",36.43_r8,-77.7_r8,19980513, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(498) = asosdata("ROCKY MOUNT","ROCKY MOUNT-WILSON REGIONAL AIRPORT","NC",93759,"KRWI","RWI",35.85_r8,-77.89_r8, &
        20001011,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(499) = asosdata("WILMINGTON","WILMINGTON INTERNATIONAL AIRPORT","NC",13748,"KILM","ILM",34.26_r8,-77.9_r8, &
        19951101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(500) = asosdata("WINSTON SALEM","SMITH REYNOLDS AIRPORT","NC",93807,"KINT","INT",36.13_r8,-80.22_r8,19981202, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(501) = asosdata("BISMARCK","BISMARCK MUNICIPAL AIRPORT","ND",24011,"KBIS","BIS",46.77_r8,-100.74_r8,19960501, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(502) = asosdata("DICKINSON","DICKINSON MUNICIPAL AIRPORT","ND",24012,"KDIK","DIK",46.79_r8,-102.8_r8,20010621, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(503) = asosdata("FARGO","HECTOR INTERNATIONAL AIRPORT","ND",14914,"KFAR","FAR",46.92_r8,-96.81_r8,19951101, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(504) = asosdata("GARRISON","GARRISON","ND",94041,"KN60","N60",47.66_r8,-101.43_r8,19980521,"NO","NWS",33.00_r8, &
        10.05_r8)
    asoscommdates(505) = asosdata("GRAND FORKS","GRAND FORKS INTERNATIONAL AIRPORT","ND",14916,"KGFK","GFK",47.94_r8,-97.17_r8, &
        19971218,"YES","FAA",26.00_r8,7.92_r8)
    asoscommdates(506) = asosdata("HETTINGER","HETTINGER MUNICIPAL AIRPORT","ND",94038,"KHEI","HEI",46.01_r8,-102.65_r8,19960313, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(507) = asosdata("JAMESTOWN","JAMESTOWN REGIONAL AIRPORT","ND",14919,"KJMS","JMS",46.92_r8,-98.67_r8,20001005, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(508) = asosdata("MINOT","MINOT INTERNATIONAL AIRPORT","ND",24013,"KMOT","MOT",48.25_r8,-101.28_r8,20010726,"NO",&
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(509) = asosdata("WILLISTON","SLOULIN FIELD INTERNATIONAL AIRPORT","ND",94014,"KISN","ISN",48.19_r8,-103.64_r8, &
        19960401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(510) = asosdata("ALLIANCE","ALLIANCE MUNICIPAL AIRPORT","NE",24044,"KAIA","AIA",42.05_r8,-102.8_r8,19960516, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(511) = asosdata("BROKEN BOW","BROKEN BOW MUNICIPAL AIRPORT","NE",94946,"KBBW","BBW",41.43_r8,-99.63_r8,19990729,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(512) = asosdata("CHADRON","CHADRON MUNICIPAL AIRPORT","NE",24017,"KCDR","CDR",42.83_r8,-103.09_r8,20000830,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(513) = asosdata("FALLS CITY","BRENNER FIELD AIRPORT","NE",94957,"KFNB","FNB",40.07_r8,-95.59_r8,20000830,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(514) = asosdata("GRAND ISLAND","CENTRAL NEBRASKA REGIONAL AIRPORT","NE",14935,"KGRI","GRI",40.95_r8,-98.31_r8, &
        19921001,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(515) = asosdata("HASTINGS","HASTINGS MUNICIPAL AIRPORT","NE",94949,"KHSI","HSI",40.6_r8,-98.42_r8,19950530,"NO",&
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(516) = asosdata("IMPERIAL","IMPERIAL MUNICIPAL AIRPORT","NE",24091,"KIML","IML",40.51_r8,-101.62_r8,20000629, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(517) = asosdata("LINCOLN","LINCOLN MUNICIPAL AIRPORT","NE",14939,"KLNK","LNK",40.83_r8,-96.76_r8,19921101,"YES",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(518) = asosdata("M! COOK","M! COOK MUNICIPAL AIRPORT","NE",94040,"KMCK","MCK",40.2_r8,-100.59_r8,19961204,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(519) = asosdata("NORFOLK","KARL STEFAN MEMORIAL AIRPORT","NE",14941,"KOFK","OFK",41.98_r8,-97.43_r8,19960401, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(520) = asosdata("NORTH PLATTE","NORTH PLATT REGIONAL AIRPORT LEE BIRD FIELD AIRPORT","NE",24023,"KLBF","LBF", &
        41.12_r8,-100.66_r8,19960201,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(521) = asosdata("OMAHA","EPPLEY AIRFIELD AIRPORT","NE",14942,"KOMA","OMA",41.31_r8,-95.89_r8,19960222,"YES", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(522) = asosdata("ORD","EVELYN SHARP FIELD AIRPORT","NE",94958,"KODX","ODX",41.62_r8,-98.95_r8,20000830,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(523) = asosdata("SCOTTSBLUFF","WESTERN NEB. RGNL/WILLIAM B HEILIG FIELD AIRPORT","NE",24028,"KBFF","BFF", &
        41.87_r8,-103.59_r8,19950601,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(524) = asosdata("SIDNEY","SIDNEY MUNICIPAL AIRPORT","NE",24030,"KSNY","SNY",41.1_r8,-102.98_r8,19951214,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(525) = asosdata("TEKAMAH","TEKAMAH MUNICIPAL AIRPORT","NE",94978,"KTQE","TQE",41.76_r8,-96.17_r8,19950530,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(526) = asosdata("VALENTINE","MILLER FIELD AIRPORT","NE",24032,"KVTN","VTN",42.85_r8,-100.55_r8,19951001,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(527) = asosdata("BERLIN","BERLIN MUNICIPAL AIRPORT","NH",94700,"KBML","BML",44.57_r8,-71.17_r8,19950428,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(528) = asosdata("CONCORD","CONCORD MUNICIPAL AIRPORT","NH",14745,"KCON","CON",43.19_r8,-71.5_r8,19960301,"YES", &
        "NWS",26.00_r8,7.92_r8)
    asoscommdates(529) = asosdata("JAFFREY","JAFFREY MUNICIPAL - SILVER RANCH AIRPORT","NH",54770,"KAFN","AFN",42.8_r8,-72._r8, &
        19950524,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(530) = asosdata("LEBANON","LEBANON MUNICIPAL AIRPORT","NH",94765,"KLEB","LEB",43.62_r8,-72.3_r8,19980513,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(531) = asosdata("MANCHESTER","MANCHESTER AIRPORT","NH",14710,"KMHT","MHT",42.93_r8,-71.43_r8,19971114,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(532) = asosdata("ROCHESTER","SKYHAVEN AIRPORT","NH",54791,"KDAW","DAW",43.27_r8,-70.92_r8,20000119,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(533) = asosdata("WHITEFIELD","MOUNT WASHINGTON REGIONAL AIRPORT","NH",54728,"KHIE","HIE",44.36_r8,-71.54_r8, &
        19950518,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(534) = asosdata("ANDOVER","AEROFLEX-ANDOVER AIRPORT","NJ",54779,"K12N","12N",41._r8,-74.73_r8,19980817,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(535) = asosdata("ATLANTI! CITY","ATLANTI! CITY INTERNATIONAL AIRPORT","NJ",93730,"KACY","ACY",39.45_r8, &
        -74.45_r8,19950901,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(536) = asosdata("CALDWELL","ESSEX COUNTY AIRPORT","NJ",54743,"KCDW","CDW",40.87_r8,-74.28_r8,19990602,"NO", &
        "NWS",26.00_r8,7.92_r8)
    asoscommdates(537) = asosdata("MILLVILLE","MILLVILLE MUNICIPAL AIRPORT","NJ",13735,"KMIV","MIV",39.36_r8,-75.07_r8,19990217, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(538) = asosdata("MOUNT HOLLY","SOUTH JERSEY REGIONAL AIRPORT","NJ",93780,"KVAY","VAY",39.94_r8,-74.84_r8, &
        19971105,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(539) = asosdata("NEWARK","NEWARK LIBERTY INTERNATIONAL AIRPORT","NJ",14734,"KEWR","EWR",40.71_r8,-74.16_r8, &
        19960701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(540) = asosdata("SOMERVILLE","SOMERSET AIRPORT","NJ",54785,"KSMQ","SMQ",40.62_r8,-74.66_r8,19990602,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(541) = asosdata("SUSSEX","SUSSEX AIRPORT","NJ",54793,"KFWN","FWN",41.2_r8,-74.62_r8,20001025,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(542) = asosdata("TETERBORO","TETERBORO AIRPORT","NJ",94741,"KTEB","TEB",40.85_r8,-74.06_r8,19970101,"NO","NWS", &
        26.00_r8,7.92_r8)
    asoscommdates(543) = asosdata("TRENTON","TRENTON MERCER AIRPORT","NJ",14792,"KTTN","TTN",40.27_r8,-74.81_r8,19980311,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(544) = asosdata("ALBUQUERQUE","ALBUQUERQUE INTERNATIONAL SUNPORT AIRPORT","NM",23050,"KABQ","ABQ",35.04_r8, &
        -106.61_r8,19960301,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(545) = asosdata("CARLSBAD","CAVERN CITY AIR TERMINAL AIRPORT","NM",93033,"KCNM","CNM",32.33_r8,-104.26_r8, &
        20001129,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(546) = asosdata("CLAYTON","CLAYTON MUNICIPAL AIRPARK AIRPORT","NM",23051,"KCAO","CAO",36.44_r8,-103.15_r8, &
        19960601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(547) = asosdata("CLINES CORNERS","CLINES CORNERS","NM",3027,"KCQC","CQC",35._r8,-105.66_r8,19980527,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(548) = asosdata("DEMING","DEMING MUNICIPAL AIRPORT","NM",23078,"KDMN","DMN",32.26_r8,-107.72_r8,20000830,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(549) = asosdata("FARMINGTON","FOUR CORNERS REGIONAL AIRPORT","NM",23090,"KFMN","FMN",36.74_r8,-108.22_r8, &
        19971210,"NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(550) = asosdata("GALLUP","GALLUP MUNICIPAL AIRPORT","NM",23081,"KGUP","GUP",35.51_r8,-108.78_r8,20001011,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(551) = asosdata("GRANTS","GRANTS-MILAN MUNICIPAL AIRPORT","NM",93057,"KGNT","GNT",35.16_r8,-107.9_r8,19971001, &
        "NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(552) = asosdata("LAS VEGAS","LAS VEGAS MUNICIPAL AIRPORT","NM",23054,"KLVS","LVS",35.65_r8,-105.14_r8,20001011, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(553) = asosdata("RATON","RATON MUNICIPAL/CREWS FIELD AIRPORT","NM",23052,"KRTN","RTN",36.74_r8,-104.5_r8, &
        19980827,"NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(554) = asosdata("ROSWELL","ROSWELL INDUSTRIAL AIR CENTER AIRPORT","NM",23009,"KROW","ROW",33.3_r8,-104.54_r8, &
        19961001,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(555) = asosdata("SANTA FE","SANTA FE MUNICIPAL AIRPORT","NM",23049,"KSAF","SAF",35.61_r8,-106.08_r8,19971002, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(556) = asosdata("TRUTH OR CONSEQUENCES","TRUTH OR CONSEQUENCES MUNICIPAL AIRPORT","NM",93045,"KTCS","TCS", &
        33.23_r8,-107.26_r8,19960916,"NO","NWS",26.00_r8,7.92_r8)
    asoscommdates(557) = asosdata("TUCUMCARI","TUCUMCARI MUNICIPAL AIRPORT","NM",23048,"KTCC","TCC",35.18_r8,-103.6_r8,20000906, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(558) = asosdata("ELKO","ELKO REGIONAL AIRPORT","NV",24121,"KEKO","EKO",40.82_r8,-115.79_r8,20010214,"YES","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(559) = asosdata("ELY","ELY AIRPORT/YELLAND FIELD/AIRPORT","NV",23154,"KELY","ELY",39.29_r8,-114.84_r8,19940601, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(560) = asosdata("EUREKA","EUREKA","NV",3170,"KP68","P68",39.6_r8,-116._r8,19970813,"NO","NWS",33.00_r8,10.05_r8)                                                                                                                                                                                                                                                                                  
    asoscommdates(561) = asosdata("LAS VAGAS","NORTH LAS VEGAS AIRPORT","NV",53123,"KVGT","VGT",36.21_r8,-115.19_r8,20000928,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(562) = asosdata("LAS VEGAS","MCCARRAN INTERNATIONAL AIRPORT","NV",23169,"KLAS","LAS",36.07_r8,-115.15_r8, &
        19950901,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(563) = asosdata("LOVELOCK","DERBY FIELD AIRPORT","NV",24172,"KLOL","LOL",40.06_r8,-118.56_r8,20001206,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(564) = asosdata("MERCURY","DESERT ROCK AIRPORT","NV",3160,"KDRA","DRA",36.62_r8,-116.02_r8,19960701,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(565) = asosdata("RENO","RENO/TAHOE INTERNATIONAL AIRPORT","NV",23185,"KRNO","RNO",39.48_r8,-119.77_r8,19950901, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(566) = asosdata("TONOPAH","TONOPAH AIRPORT","NV",23153,"KTPH","TPH",38.06_r8,-117.08_r8,20001129,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(567) = asosdata("WINNEMUCCA","WINNEMUCCA MUNICIPAL AIRPORT","NV",24128,"KWMC","WMC",40.9_r8,-117.8_r8,19941001, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(568) = asosdata("ALBANY","ALBANY INTERNATIONAL AIRPORT","NY",14735,"KALB","ALB",42.74_r8,-73.8_r8,19950801, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(569) = asosdata("BINGHAMTON","GREATER BINGHAMTON/EDWIN A LINK FIELD AIRPORT","NY",4725,"KBGM","BGM",42.2_r8, &
        -75.98_r8,19951101,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(570) = asosdata("BUFFALO","BUFFALO NIAGARA INTERNATIONAL AIRPORT","NY",14733,"KBUF","BUF",42.94_r8,-78.73_r8, &
        19951201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(571) = asosdata("DANSVILLE","DANSVILLE MUNICIPAL AIRPORT","NY",94704,"KDSV","DSV",42.57_r8,-77.71_r8,20000301, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(572) = asosdata("DUNKIRK","CHAUTAUQUA COUNTY/DUNKIRK AIRPORT","NY",14747,"KDKK","DKK",42.49_r8,-79.27_r8, &
        19961211,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(573) = asosdata("ELMIRA/CORNING","ELMIRA/CORNING REGIONAL AIRPORT","NY",14748,"KELM","ELM",42.15_r8,-76.89_r8, &
        19971217,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(574) = asosdata("FARMINGDALE","REPUBLI! AIRPORT","NY",54787,"KFRG","FRG",40.73_r8,-73.41_r8,19990804,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(575) = asosdata("FULTON","OSWEGO COUNTY AIRPORT","NY",54773,"KFZY","FZY",43.34_r8,-76.38_r8,19970601,"NO","FAA",&
        26.00_r8,7.92_r8)
    asoscommdates(576) = asosdata("GLEN FALLS","FLOYD BENNETT MEMORIAL AIRPORT","NY",14750,"KGFL","GFL",43.34_r8,-73.61_r8, &
        20010718,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(577) = asosdata("ISLIP","LONG ISLAND MA! ARTHUR AIRPORT","NY",4781,"KISP","ISP",40.79_r8,-73.1_r8,19990801, &
        "YES","FAA",26.00_r8,7.92_r8)
    asoscommdates(578) = asosdata("MASSENA","MASSENA INTERNATIONAL-RICHARDS FIELD AIRPORT","NY",94725,"KMSS","MSS",44.93_r8, &
        -74.84_r8,20000913,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(579) = asosdata("MONTAUK","MONTAUK AIRPORT","NY",54780,"KMTP","MTP",41.07_r8,-71.92_r8,19980901,"NO","NWS", &
        19.00_r8,5.79_r8)
    asoscommdates(580) = asosdata("MONTGOMERY","ORANGE COUNTY AIRPORT","NY",4789,"KMGJ","MGJ",41.5_r8,-74.26_r8,19971217,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(581) = asosdata("NEW YORK","JOHN F KENNEDY INTERNATIONAL AIRPORT","NY",94789,"KJFK","JFK",40.65_r8,-73.79_r8, &
        19960501,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(582) = asosdata("NEW YORK","LA GUARDIA AIRPORT","NY",14732,"KLGA","LGA",40.77_r8,-73.88_r8,19960501,"YES","NWS",&
        33.00_r8,10.05_r8)
    asoscommdates(583) = asosdata("NEW YORK ","CENTRAL PARK","NY",94728,"KNYC","NYC",40.78_r8,-73.96_r8,19951101,"YES","NWS", &
        -9.0_r8,-9.0_r8)
    asoscommdates(584) = asosdata("NIAGARA FALLS","NIAGARA FALLS INTERNATIONAL AIRPORT","NY",4724,"KIAG","IAG",43.1_r8,-78.94_r8, &
        20010926,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(585) = asosdata("PLATTSBURGH","PLATTSBURG INTERNATIONAL AIRPORT","NY",04742,"KPBG","PBG",44.65_r8,-73.46_r8, &
        20070618,"NO","FAA",-9._r8,-9._r8)
    asoscommdates(586) = asosdata("PENN YAN","PENN YAN AIRPORT","NY",54778,"KPEO","PEO",42.64_r8,-77.05_r8,19971210,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(587) = asosdata("PLATTSBURGH","CLINTON COUNTY AIRPORT (CLOSED 5/9/2007)","NY",94733,"KPLB","PLB",44.68_r8, &
        -73.52_r8,19980701,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(588) = asosdata("POUGHKEEPSIE","DUTCHESS COUNTY AIRPORT","NY",14757,"KPOU","POU",41.62_r8,-73.88_r8,20000927, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(589) = asosdata("ROCHESTER","GREATER ROCHESTER INTERNATION AIRPORT","NY",14768,"KROC","ROC",43.11_r8,-77.67_r8, &
        19960701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(590) = asosdata("SARANA! LAKE","ADIRONDACK REGIONAL AIRPORT","NY",94740,"KSLK","SLK",44.38_r8,-74.2_r8,19980610,&
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(591) = asosdata("SHIRLEY","BROOKHAVEN AIRPORT","NY",54790,"KHWV","HWV",40.82_r8,-72.86_r8,19990929,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(592) = asosdata("SYRACUSE","SYRACUSE HANCOCK INTERNATIONAL AIRPORT","NY",14771,"KSYR","SYR",43.1_r8,-76.1_r8, &
        19931101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(593) = asosdata("UTICA","ONEIDA COUNTY AIRPORT (closed 011807)","NY",94794,"KUCA","UCA",43.14_r8,-75.38_r8, &
        20010131,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(594) = asosdata("WATERTOWN","WATERTOWN INTERNATIONAL AIRPORT","NY",94790,"KART","ART",43.99_r8,-76.02_r8, &
        20010124,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(595) = asosdata("WELLSVILLE","WELLSVILLE MUNI ARPT;TARANTINE FIELD AIRPORT","NY",54757,"KELZ","ELZ",42.1_r8, &
        -77.99_r8,20000202,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(596) = asosdata("WESTHAMPTON BEACH","FRANCIS S GABRESKI AIRPORT","NY",14719,"KFOK","FOK",40.84_r8,-72.63_r8, &
        19980722,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(597) = asosdata("WHITE PLAINS","WESTCHESTER COUNTY AIRPORT","NY",94745,"KHPN","HPN",41.06_r8,-73.7_r8,20010425, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(598) = asosdata("AKRON","AKRON FULTON INTERNATIONAL AIRPORT","OH",14813,"KAKR","AKR",41.03_r8,-81.46_r8, &
        19990519,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(599) = asosdata("AKRON","AKRON-CANTON REGIONAL AIRPORT","OH",14895,"KCAK","CAK",40.91_r8,-81.44_r8,19950901, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(600) = asosdata("ASHTABULA","ASHTABULA COUNTY AIRPORT","OH",4857,"KHZY","HZY",41.77_r8,-80.69_r8,19981202,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(601) = asosdata("CINCINNATI","CINCINNATI MUNICIPAL AIRPORT/LUNKEN FIELD AIRPORT","OH",93812,"KLUK","LUK", &
        39.1_r8,-84.41_r8,19970813,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(602) = asosdata("CLEVELAND","CLEVELAND BURKE LAKEFRONT AIRPORT","OH",4853,"KBKL","BKL",41.51_r8,-81.68_r8, &
        19980211,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(603) = asosdata("CLEVELAND","CLEVELAND-HOPKINS INTERNATIONAL AIRPORT","OH",14820,"KCLE","CLE",41.4_r8,-81.85_r8,&
        19951201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(604) = asosdata("COLUMBUS","PORT COLUMBUS INTERNATIONAL AIRPORT","OH",14821,"KCMH","CMH",39.99_r8,-82.88_r8, &
        19960201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(605) = asosdata("COLUMBUS","OHIO STATE UNIVERSITY AIRPORT","OH",4804,"KOSU","OSU",40.07_r8,-83.07_r8,19971008, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(606) = asosdata("COVINGTON/CINCINNATI","CINCINNATI/NORTHERN KY INTERNATIONAL AIRPORT","OH",93814,"KCVG","CVG", &
        39.04_r8,-84.67_r8,19951001,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(607) = asosdata("DAYTON","JAMES M COX DAYTON INTERNATIONAL AIRPORT","OH",93815,"KDAY","DAY",39.9_r8,-84.21_r8, &
        19951101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(608) = asosdata("DAYTON","DAYTON WRIGHT BROTHERS AIRPORT","OH",53859,"KMGY","MGY",39.59_r8,-84.22_r8,19971015, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(609) = asosdata("DEFIANCE","DEFIANCE MEMORIAL AIRPORT","OH",4851,"KDFI","DFI",41.33_r8,-84.42_r8,19980107,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(610) = asosdata("FINDLAY","FINDLAY AIRPORT","OH",14825,"KFDY","FDY",41.01_r8,-83.66_r8,20000816,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(611) = asosdata("HAMILTON","BUTLER COUNTY REGIONAL AIRPORT","OH",53855,"KHAO","HAO",39.36_r8,-84.52_r8,19970515,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(612) = asosdata("LANCASTER","FAIRFIELD COUNTY AIRPORT","OH",53844,"KLHQ","LHQ",39.75_r8,-82.65_r8,19960320,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(613) = asosdata("LIMA","LIMA ALLEN COUNTY AIRPORT","OH",4850,"KAOH","AOH",40.7_r8,-84.02_r8,19980128,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(614) = asosdata("LORAIN/ELYRIA","LORAIN COUNTY REGIONAL AIRPORT","OH",4849,"KLPR","LPR",41.17_r8,-82.17_r8, &
        19970425,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(615) = asosdata("MANSFIELD","MANSFIELD LAHM MUNICIPAL AIRPORT","OH",14891,"KMFD","MFD",40.82_r8,-82.51_r8, &
        19960201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(616) = asosdata("MARION","MARION MUNICIPAL AIRPORT","OH",4855,"KMNN","MNN",40.61_r8,-83.06_r8,19980506,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(617) = asosdata("NEW PHILADELPHIA","HARRY CLEVER FIELD AIRPORT","OH",4852,"KPHD","PHD",40.47_r8,-81.42_r8, &
        19980128,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(618) = asosdata("NEWARK","NEWARK-HEATH AIRPORT","OH",4858,"KVTA","VTA",40.02_r8,-82.46_r8,19990224,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(619) = asosdata("TOLEDO","METCALF FIELD AIRPORT","OH",4848,"KTDZ","TDZ",41.56_r8,-83.47_r8,19971217,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(620) = asosdata("TOLEDO","TOLEDO EXPRESS AIRPORT","OH",94830,"KTOL","TOL",41.58_r8,-83.8_r8,19951201,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(621) = asosdata("WILMINGTON","AIRBORNE AIRPARK AIRPORT","OH",13841,"KILN","ILN",39.42_r8,-83.82_r8,19980401, &
        "NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(622) = asosdata("WOOSTER","WAYNE COUNTY AIRPORT","OH",4842,"KBJJ","BJJ",40.87_r8,-81.88_r8,19961211,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(623) = asosdata("YOUNGSTOWN/WARREN","YOUNGSTOWN-WARREN REGIONAL AIRPORT","OH",14852,"KYNG","YNG",41.25_r8, &
        -80.67_r8,19950901,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(624) = asosdata("ZANESVILLE","ZANESVILLE MUNICIPAL AIRPORT","OH",93824,"KZZV","ZZV",39.94_r8,-81.89_r8,20000913,&
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(625) = asosdata("CLINTON","CLINTON-SHERMAN AIRPORT","OK",3932,"KCSM","CSM",35.33_r8,-99.2_r8,19961015,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(626) = asosdata("FREDERICK","FREDERICK MUNICIPAL AIRPORT","OK",3981,"KFDR","FDR",34.35_r8,-98.98_r8,19980128, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(627) = asosdata("GAGE","GAGE AIRPORT","OK",13975,"KGAG","GAG",36.29_r8,-99.77_r8,19961031,"NO","FAA",33.00_r8, &
        10.05_r8)
    asoscommdates(628) = asosdata("GUTHRIE","GUTHRIE MUNICIPAL AIRPORT","OK",53913,"KGOK","GOK",35.85_r8,-97.41_r8,19980409,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(629) = asosdata("GUYMON","GUYMON MUNICIPAL AIRPORT","OK",3030,"KGUY","GUY",36.68_r8,-101.5_r8,19981201,"NO", &
        "NWS",26.00_r8,7.92_r8)
    asoscommdates(630) = asosdata("HOBART","HOBART MUNICIPAL AIRPORT","OK",93986,"KHBR","HBR",35._r8,-99.05_r8,19960731,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(631) = asosdata("LAWTON","LAWTON-FORT SILL REGIONAL AIRPORT","OK",3950,"KLAW","LAW",34.56_r8,-98.41_r8,19960913,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(632) = asosdata("M! ALESTER","M! ALESTER REGIONAL AIRPORT","OK",93950,"KMLC","MLC",34.89_r8,-95.78_r8,19960715, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(633) = asosdata("MUSKOGEE","DAVIS FIELD AIRPORT","OK",93953,"KMKO","MKO",35.65_r8,-95.36_r8,19960712,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(634) = asosdata("OKLAHOMA CITY","WILL ROGERS WORLD AIRPORT","OK",13967,"KOKC","OKC",35.38_r8,-97.6_r8,19921001, &
        "YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(635) = asosdata("OKLAHOMA CITY","WILEY POST AIRPORT","OK",3954,"KPWA","PWA",35.53_r8,-97.64_r8,19960731,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(636) = asosdata("PONCA CITY","PONCA CITY REGIONAL AIRPORT","OK",13969,"KPNC","PNC",36.73_r8,-97.09_r8,20001108, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(637) = asosdata("STILLWATER","STILLWATER REGIONAL AIRPORT","OK",3965,"KSWO","SWO",36.16_r8,-97.08_r8,19961015, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(638) = asosdata("TULSA","RICHARD LLOYD JONES JR AIRPORT","OK",53908,"KRVS","RVS",36.03_r8,-95.98_r8,19970904, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(639) = asosdata("TULSA","TULSA INTERNATIONAL AIRPORT","OK",13968,"KTUL","TUL",36.19_r8,-95.88_r8,19921001,"YES",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(640) = asosdata("ASTORIA","ASTORIA REGIONAL AIRPORT","OR",94224,"KAST","AST",46.15_r8,-123.87_r8,19930301,"YES",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(641) = asosdata("AURORA","AURORA STATE AIRPORT","OR",94281,"KUAO","UAO",45.24_r8,-122.76_r8,19970129,"NO","FAA",&
        26.00_r8,7.92_r8)
    asoscommdates(642) = asosdata("BAKER CITY","BAKER CITY MUNICIPAL AIRPORT","OR",24130,"KBKE","BKE",44.83_r8,-117.8_r8,20011025,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(643) = asosdata("BURNS","BURNS MUNICIPAL AIRPORT","OR",94185,"KBNO","BNO",43.59_r8,-118.95_r8,19950701,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(644) = asosdata("EUGENE","MAHLON SWEET FIELD AIRPORT","OR",24221,"KEUG","EUG",44.13_r8,-123.21_r8,19950901, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(645) = asosdata("HERMISTON","HERMISTON MUNICIPAL AIRPORT","OR",4113,"KHRI","HRI",45.82_r8,-119.26_r8,19980313, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(646) = asosdata("KLAMATH FALLS","KLAMATH FALLS AIRPORT","OR",94236,"KLMT","LMT",42.14_r8,-121.72_r8,19971015, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(647) = asosdata("M! MINNVILLE","M! MINNVILLE MUNICIPAL AIRPORT","OR",94273,"KMMV","MMV",45.19_r8,-123.13_r8, &
        19970129,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(648) = asosdata("MEACHAM","MEACHAM","OR",24152,"KMEH","MEH",45.51_r8,-118.42_r8,19980507,"NO","NWS",33.00_r8, &
        10.05_r8)
    asoscommdates(649) = asosdata("MEDFORD","ROGUE VALLEY INTERNATIONAL-MEDFORD AIRPORT","OR",24225,"KMFR","MFR",42.38_r8, &
        -122.87_r8,19980101,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(650) = asosdata("ONTARIO","ONTARIO MUNICIPAL AIRPORT","OR",24162,"KONO","ONO",44.02_r8,-117.01_r8,19970409,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(651) = asosdata("PENDLETON","EASTERN OREGON REGIONAL AT PENDLETON AIRPORT","OR",24155,"KPDT","PDT",45.69_r8, &
        -118.83_r8,19950601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(652) = asosdata("PORTLAND","PORTLAND-HILLSBORO AIRPORT","OR",94261,"KHIO","HIO",45.54_r8,-122.94_r8,19980618, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(653) = asosdata("PORTLAND","PORTLAND INTERNATIONAL AIRPORT","OR",24229,"KPDX","PDX",45.59_r8,-122.6_r8,19951101,&
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(654) = asosdata("PORTLAND","PORTLAND-TROUTDALE AIRPORT","OR",24242,"KTTD","TTD",45.54_r8,-122.4_r8,19980625, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(655) = asosdata("REDMOND","ROBERTS FIELD AIRPORT","OR",24230,"KRDM","RDM",44.25_r8,-121.15_r8,20000420,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(656) = asosdata("ROME","ROME STATE AIRPORT","OR",94107,"KREO","REO",42.59_r8,-117.86_r8,19980318,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(657) = asosdata("ROSEBURG","ROSEBURG REGIONAL AIRPORT","OR",24231,"KRBG","RBG",43.23_r8,-123.35_r8,19970625, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(658) = asosdata("SALEM","MCNARY FIELD AIRPORT","OR",24232,"KSLE","SLE",44.9_r8,-122.99_r8,19950701,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(659) = asosdata("SCAPPOOSE","SCAPPOOSE INDUSTRIAL AIRPARK AIRPORT","OR",4201,"KSPB","SPB",45.77_r8,-122.86_r8, &
        19980827,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(660) = asosdata("SEXTON SUMMIT","SEXTON SUMMIT","OR",24235,"KSXT","SXT",42.61_r8,-123.38_r8,19930101,"NO","NWS",&
        33.00_r8,10.05_r8)
    asoscommdates(661) = asosdata("THE DALLES","COLUMBIA GORGE RGNL/THE DALLES MUNICIPAL AIRPORT","OR",24219,"KDLS","DLS", &
        45.61_r8,-121.16_r8,20001026,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(662) = asosdata("ALLENTOWN","LEHIGH VALLEY INTERNATIONAL AIRPORT","PA",14737,"KABE","ABE",40.65_r8,-75.44_r8, &
        19951101,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(663) = asosdata("ALTOONA","ALTOONA-BLAIR COUNTY AIRPORT","PA",14736,"KAOO","AOO",40.3_r8,-78.31_r8,19990714, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(664) = asosdata("BRADFORD","BRADFORD REGIONAL AIRPORT","PA",4751,"KBFD","BFD",41.8_r8,-78.64_r8,19961202,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(665) = asosdata("CLEARFIELD","CLEARFIELD - LAWRENCE AIRPORT","PA",54792,"KFIG","FIG",41.04_r8,-78.41_r8, &
        20000126,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(666) = asosdata("DOYLESTOWN","DOYLESTOWN AIRPORT","PA",54786,"KDYL","DYL",40.33_r8,-75.12_r8,19990728,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(667) = asosdata("DU BOIS","DU BOIS-JEFFERSON COUNTY AIRPORT","PA",4787,"KDUJ","DUJ",41.17_r8,-78.89_r8,20000614,&
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(668) = asosdata("ERIE","ERIE INTERNATIONAL/TOM RIDGE FIELD AIRPORT","PA",14860,"KERI","ERI",42.08_r8,-80.18_r8, &
        19951001,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(669) = asosdata("HARRISBURG","CAPITAL CITY AIRPORT","PA",14751,"KCXY","CXY",40.21_r8,-76.85_r8,20001011,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(670) = asosdata("HARRISBURG","HARRISBURG INTERNATIONAL AIRPORT","PA",14711,"KMDT","MDT",40.19_r8,-76.76_r8, &
        20001201,"YES","FAA",26.00_r8,7.92_r8)
    asoscommdates(671) = asosdata("JOHNSTOWN","JOHN MURTHA JOHNSTOWN-CAMBRIA COUNTY AIRPORT","PA",4726,"KJST","JST",40.3_r8, &
        -78.83_r8,20000830,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(672) = asosdata("LANCASTER","LANCASTER AIRPORT","PA",54737,"KLNS","LNS",40.12_r8,-76.29_r8,19990317,"NO","FAA", &
        26.00_r8,7.92_r8)
    asoscommdates(673) = asosdata("MEADVILLE","PORT MEADVILLE AIRPORT","PA",4843,"KGKJ","GKJ",41.62_r8,-80.21_r8,19970122,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(674) = asosdata("MOUNT POCONO","POCONO MOUNTAINS MUNICIPAL AIRPORT","PA",54789,"KMPO","MPO",41.13_r8,-75.37_r8, &
        19990929,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(675) = asosdata("PHILADELPHIA","PHILADELPHIA INTERNATIONAL AIRPORT","PA",13739,"KPHL","PHL",39.86_r8,-75.23_r8, &
        19951201,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(676) = asosdata("PHILADELPHIA","NORTHEAST PHILADELPHIA AIRPORT","PA",94732,"KPNE","PNE",40.08_r8,-75.01_r8, &
        19960501,"NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(677) = asosdata("PITTSBURGH","ALLEGHENY COUNTY AIRPORT","PA",14762,"KAGC","AGC",40.35_r8,-79.92_r8,19990203, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(678) = asosdata("PITTSBURGH","PITTSBURGH INTERNATIONAL AIRPORT","PA",94823,"KPIT","PIT",40.5_r8,-80.23_r8, &
        19960701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(679) = asosdata("POTTSTOWN","POTTSTOWN LIMERICK AIRPORT","PA",54782,"KPTW","PTW",40.23_r8,-75.55_r8,19990303, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(680) = asosdata("READING","READING REGIONAL/CARL A SPAATZ FIELD AIRPORT","PA",14712,"KRDG","RDG",40.37_r8, &
        -75.95_r8,19990217,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(681) = asosdata("SELINSGROVE","PENN VALLEY AIRPORT","PA",14770,"KSEG","SEG",40.82_r8,-76.86_r8,19970813,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(682) = asosdata("WILKES-BARRE/SCRANTON","WILKES-BARRE/SCRANTON INTERNATIONAL AIRPORT","PA",14777,"KAVP","AVP", &
        41.33_r8,-75.72_r8,19960401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(683) = asosdata("WILLIAMSPORT","WILLIAMSPORT REGIONAL AIRPORT","PA",14778,"KIPT","IPT",41.24_r8,-76.92_r8, &
        19950901,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(684) = asosdata("YORK","YORK AIRPORT","PA",93778,"KTHV","THV",39.91_r8,-76.87_r8,19970813,"NO","FAA",33.00_r8, &
        10.05_r8)
    asoscommdates(685) = asosdata("NEWPORT","NEWPORT STATE AIRPORT","RI",14787,"KUUU","UUU",41.53_r8,-71.28_r8,19960229,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(686) = asosdata("PROVIDENCE","THEODORE FRANCIS GREEN STATE AIRPORT","RI",14765,"KPVD","PVD",41.72_r8,-71.43_r8, &
        19950901,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(687) = asosdata("WESTERLY","WESTERLY STATE AIRPORT","RI",14794,"KWST","WST",41.34_r8,-71.79_r8,19990728,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(688) = asosdata("ANDERSON","ANDERSON COUNTY AIRPORT","SC",93846,"KAND","AND",34.49_r8,-82.7_r8,19981104,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(689) = asosdata("CHARLESTON","CHARLESTON AFB/INTERNATIONAL AIRPORT","SC",13880,"KCHS","CHS",32.89_r8,-80.04_r8, &
        19951001,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(690) = asosdata("CLEMSON","OCONEE COUNTY REGIONAL AIRPORT","SC",53850,"KCEU","CEU",34.67_r8,-82.88_r8,20000316, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(691) = asosdata("COLUMBIA","COLUMBIA METROPOLITAN AIRPORT","SC",13883,"KCAE","CAE",33.94_r8,-81.11_r8,19951201, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(692) = asosdata("COLUMBIA","COLUMBIA OWENS DOWNTOWN AIRPORT","SC",53867,"KCUB","CUB",33.97_r8,-80.99_r8, &
        19981015,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(693) = asosdata("FLORENCE","FLORENCE REGIONAL AIRPORT","SC",13744,"KFLO","FLO",34.18_r8,-79.73_r8,19990423,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(694) = asosdata("GREENVILLE","GREENVILLE DOWNTOWN AIRPORT","SC",13886,"KGMU","GMU",34.84_r8,-82.34_r8,19990428, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(695) = asosdata("GREENWOOD","GREENWOOD COUNTY AIRPORT","SC",53874,"KGRD","GRD",34.24_r8,-82.15_r8,20000524,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(696) = asosdata("GREER","GREENVILLE-SPARTANBURG INTERNATIONAL AIRPORT","SC",3870,"KGSP","GSP",34.89_r8, &
        -82.21_r8,19960401,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(697) = asosdata("NORTH MYRTLE BEACH","GRAND STRAND AIRPORT","SC",93718,"KCRE","CRE",33.81_r8,-78.72_r8,19990616,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(698) = asosdata("ORANGEBURG","ORANGEBURG MUNICIPAL AIRPORT","SC",53854,"KOGB","OGB",33.46_r8,-80.85_r8,19970320,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(699) = asosdata("ROCK HILL","ROCK HILL/YORK COUNTY/BRYANT FIELD AIRPORT","SC",53871,"KUZA","UZA",34.98_r8, &
        -81.05_r8,19990120,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(700) = asosdata("ABERDEEN","ABERDEEN REGIONAL AIRPORT","SD",14929,"KABR","ABR",45.44_r8,-98.42_r8,19941101, &
        "YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(701) = asosdata("BUFFALO","BUFFALO AMOS","SD",94037,"K2WX","2WX",45.6_r8,-103.54_r8,19980416,"NO","NWS", &
        26.00_r8,7.92_r8)
    asoscommdates(702) = asosdata("CHAMBERLAIN","CHAMBERLAIN MUNICIPAL AIRPORT","SD",94943,"K9V9","9V9",43.76_r8,-99.31_r8, &
        19980205,"NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(703) = asosdata("CUSTER","CUSTER COUNTY AIRPORT","SD",94032,"KCUT","CUT",43.73_r8,-103.62_r8,19990409,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(704) = asosdata("FAITH","FAITH MUNICIPAL AIRPORT","SD",94056,"KD07","D07",45.03_r8,-102.01_r8,19980521,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(705) = asosdata("SISSETON","SISSETON MUNICIPAL AIRPORT","SD",94993,"K8D3","8D3",45.66_r8,-96.99_r8,19980827, &
        "NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(706) = asosdata("HURON","HURON REGIONAL AIRPORT","SD",14936,"KHON","HON",44.38_r8,-98.22_r8,19961101,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(707) = asosdata("MITCHELL","MITCHELL MUNICIPAL AIRPORT","SD",94950,"KMHE","MHE",43.77_r8,-98.03_r8,19990901, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(708) = asosdata("MOBRIDGE","MOBRIDGE MUNICIPAL AIRPORT","SD",94052,"KMBG","MBG",45.54_r8,-100.4_r8,19970901, &
        "NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(709) = asosdata("PHILIP","PHILIP AIRPORT","SD",24024,"KPHP","PHP",44.05_r8,-101.6_r8,19980626,"NO","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(710) = asosdata("PIERRE","PIERRE REGIONAL AIRPORT","SD",24025,"KPIR","PIR",44.38_r8,-100.28_r8,20000907,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(711) = asosdata("PINE RIDGE","PINE RIDGE AIRPORT","SD",94039,"KIEN","IEN",43.02_r8,-102.51_r8,19970611,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(712) = asosdata("RAPID CITY","RAPID CITY REGIONAL AIRPORT","SD",24090,"KRAP","RAP",44.04_r8,-103.05_r8,19950901,&
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(713) = asosdata("SIOUX FALLS","JOE FOSS FIELD AIRPORT","SD",14944,"KFSD","FSD",43.57_r8,-96.75_r8,19960401, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(714) = asosdata("WATERTOWN","WATERTOWN MUNICIPAL AIRPORT","SD",14946,"KATY","ATY",44.93_r8,-97.15_r8,19960424, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(715) = asosdata("WINNER","WINNER REGIONAL AIRPORT (SITE ID/LOCATION CHANGE) 7/5/2007","SD",94990,"KICR","ICR", &
        43.39_r8,-99.84_r8,19970319,"NO","FAA",-9.0_r8,-9.0_r8)
    asoscommdates(716) = asosdata("BRISTOL/JOHNSON CITY/KINGSPORT","TRI-CITY REGIONAL TN/VA AIRPORT","TN",13877,"KTRI","TRI", &
        36.47_r8,-82.39_r8,19951001,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(717) = asosdata("CHATTANOOGA","LOVELL FIELD AIRPORT","TN",13882,"KCHA","CHA",35.03_r8,-85.2_r8,19950901,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(718) = asosdata("CLARKSVILLE","OUTLAW FIELD AIRPORT","TN",3894,"KCKV","CKV",36.62_r8,-87.41_r8,20010410,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(719) = asosdata("CROSSVILLE","CROSSVILLE MEMORIAL-WHITSON FIELD AIRPORT","TN",3847,"KCSV","CSV",35.95_r8, &
        -85.08_r8,20001004,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(720) = asosdata("JACKSON","MCKELLAR-SIPES REGIONAL AIRPORT","TN",3811,"KMKL","MKL",35.59_r8,-88.91_r8,19970603, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(721) = asosdata("KNOXVILLE","M! GHEE TYSON AIRPORT","TN",13891,"KTYS","TYS",35.81_r8,-83.98_r8,19951001,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(722) = asosdata("MEMPHIS","MEMPHIS INTERNATIONAL AIRPORT","TN",13893,"KMEM","MEM",35.06_r8,-89.98_r8,19990430, &
        "YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(723) = asosdata("NASHVILLE","NASHVILLE INTERNATIONAL AIRPORT","TN",13897,"KBNA","BNA",36.11_r8,-86.68_r8, &
        19960601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(724) = asosdata("OAK RIDGE","OAK RIDGE","TN",53868,"KOQT","OQT",36.02_r8,-84.23_r8,19980901,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(725) = asosdata("ABILENE","ABILENE REGIONAL AIRPORT","TX",13962,"KABI","ABI",32.41_r8,-99.68_r8,19960501,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(726) = asosdata("ALICE","ALICE INTERNATIONAL AIRPORT","TX",12932,"KALI","ALI",27.74_r8,-98.02_r8,20010110,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(727) = asosdata("AMARILLO","AMARILLO INTERNATIONAL AIRPORT","TX",23047,"KAMA","AMA",35.21_r8,-101.7_r8,19921101,&
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(728) = asosdata("ANGLETON/LAKE JACKSON","BRAZORIA COUNTY AIRPORT","TX",12976,"KLBX","LBX",29.1_r8,-95.46_r8, &
        19980513,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(729) = asosdata("ARLINGTON","ARLINGTON MUNICIPAL AIRPORT","TX",53907,"KGKY","GKY",32.66_r8,-97.09_r8,19970722, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(730) = asosdata("AUSTIN/BERGSTROM","AUSTIN-BERGSTROM INTERNATIONAL AIRPORT","TX",13904,"KAUS","AUS",30.17_r8, &
        -97.68_r8,19971002,"YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(731) = asosdata("AUSTIN/CITY","CAMP MABRY ARMY NATIONAL GUARD","TX",13958,"KATT","ATT",30.31_r8,-97.76_r8, &
        19950701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(732) = asosdata("BEAUMONT/PORT ARTHUR","SOUTHEAST TEXAS REGIONAL AIRPORT","TX",12917,"KBPT","BPT",29.95_r8, &
        -94.02_r8,19950701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(733) = asosdata("BORGER","HUTCHINSON COUNTY AIRPORT","TX",3024,"KBGD","BGD",35.7_r8,-101.39_r8,19951218,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(734) = asosdata("BROWNSVILLE","BROWNSVILLE/SOUTH PADRE IS. INTERNATIONAL AIRPORT","TX",12919,"KBRO","BRO", &
        25.9_r8,-97.42_r8,19940501,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(735) = asosdata("BURNET","BURNET MUNICIPAL KATE CRADDOCK FIELD AIRPORT","TX",3999,"KBMQ","BMQ",30.74_r8, &
        -98.23_r8,19960712,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(736) = asosdata("CHILDRESS","CHILDRESS MUNICIPAL AIRPORT","TX",23007,"KCDS","CDS",34.43_r8,-100.28_r8,19960731, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(737) = asosdata("COLLEGE STATION","EASTERWOOD FIELD AIRPORT","TX",3904,"KCLL","CLL",30.58_r8,-96.36_r8,19961210,&
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(738) = asosdata("CONROE","MONTGOMERY COUNTY AIRPORT","TX",53902,"KCXO","CXO",30.35_r8,-95.41_r8,19961210,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(739) = asosdata("CORPUS CHRISTI","CORPUS CHRISTI INTERNATIONAL AIRPORT","TX",12924,"KCRP","CRP",27.77_r8, &
        -97.51_r8,19951201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(740) = asosdata("CORSICANA","! DAVID CAMPBELL FIELD-CORSICANA MUNICIPAL AIRPORT","TX",53912,"KCRS","CRS", &
        32.02_r8,-96.39_r8,19970520,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(741) = asosdata("COTULLA","COTULLA-LA SALLE COUNTY AIRPORT","TX",12947,"KCOT","COT",28.45_r8,-99.22_r8,20010110,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(742) = asosdata("DALHART","DALHART MUNICIPAL AIRPORT","TX",93042,"KDHT","DHT",36.02_r8,-102.54_r8,20000920,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(743) = asosdata("DALLAS","DALLAS LOVE FIELD AIRPORT","TX",13960,"KDAL","DAL",32.84_r8,-96.85_r8,19971119,"YES", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(744) = asosdata("DALLAS","DALLAS EXECUTIVE AIRPORT","TX",3971,"KRBD","RBD",32.68_r8,-96.86_r8,19970826,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(745) = asosdata("DALLAS-FT WORTH","DALLAS/FT WORTH INTERNATIONAL AIRPORT","TX",3927,"KDFW","DFW",32.89_r8, &
        -97.04_r8,19951201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(746) = asosdata("DEL RIO","DEL RIO INTERNATIONAL AIRPORT","TX",22010,"KDRT","DRT",29.36_r8,-100.92_r8,19960401, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(747) = asosdata("DENTON","DENTON MUNICIPAL AIRPORT","TX",3991,"KDTO","DTO",33.2_r8,-97.19_r8,19950726,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(748) = asosdata("DRYDEN","TERRELL COUNTY AIRPORT","TX",3032,"K6R6","6R6",30.04_r8,-102.21_r8,19990615,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(749) = asosdata("EL PASO","EL PASO INTERNATIONAL AIRPORT","TX",23044,"KELP","ELP",31.81_r8,-106.37_r8,19950601, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(750) = asosdata("FORT STOCKTON","FORT STOCKTON-PECOS COUNTY AIRPORT","TX",23091,"KFST","FST",30.91_r8, &
        -102.91_r8,19951206,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(751) = asosdata("FORT WORTH","FORT WORTH ALLIANCE AIRPORT","TX",53909,"KAFW","AFW",32.97_r8,-97.31_r8,19970725, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(752) = asosdata("FORT WORTH","FORT WORTH MEACHAM INTERNATIONAL AIRPORT","TX",13961,"KFTW","FTW",32.81_r8, &
        -97.36_r8,19970923,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(753) = asosdata("GALVESTON","SCHOLES INTERNATIONAL AT GALVESTON AIRPORT","TX",12923,"KGLS","GLS",29.26_r8, &
        -94.86_r8,19961130,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(754) = asosdata("GUADALUPE PASS","GUADALUPE PASS AUTO METEORLOLGICAL OBSERVING SYSTEM ","TX",23055,"KGDP","GDP",&
        31.83_r8,-104.8_r8,19990615,"NO","NWS",19.00_r8,5.79_r8)
    asoscommdates(755) = asosdata("HARLINGEN","VALLEY INTERNATIONAL AIRPORT","TX",12904,"KHRL","HRL",26.22_r8,-97.65_r8,19961125, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(756) = asosdata("HONDO","HONDO MUNICIPAL AIRPORT","TX",12962,"KHDO","HDO",29.35_r8,-99.17_r8,19960315,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(757) = asosdata("HOUSTON","DAVID WAYNE HOOKS MEMORIAL AIRPORT","TX",53910,"KDWH","DWH",30.06_r8,-95.55_r8, &
        19971209,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(758) = asosdata("HOUSTON","WILLIAM P.HOBBY AIRPORT","TX",12918,"KHOU","HOU",29.64_r8,-95.27_r8,19980812,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(759) = asosdata("HOUSTON","G. BUSH INTERCONTINENTAL AIRPORT/HOUSTON AIRPORT","TX",12960,"KIAH","IAH",29.99_r8, &
        -95.36_r8,19960601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(760) = asosdata("HOUSTON","CLOVER FIELD AIRPORT","TX",12975,"KLVJ","LVJ",29.52_r8,-95.24_r8,19970410,"NO","FAA",&
        26.00_r8,7.92_r8)
    asoscommdates(761) = asosdata("HOUSTON","SUGAR LAND REGIONAL AIRPORT","TX",12977,"KSGR","SGR",29.62_r8,-95.65_r8,20001228, &
        "NO","FAA",25.00_r8,7.62_r8)
    asoscommdates(762) = asosdata("HUNTSVILLE","HUNTSVILLE MUNICIPAL AIRPORT","TX",53903,"KUTS","UTS",30.74_r8,-95.58_r8,19970203,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(763) = asosdata("JUNCTION","KIMBLE COUNTY AIRPORT","TX",13973,"KJCT","JCT",30.51_r8,-99.76_r8,19961202,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(764) = asosdata("LONGVIEW","EAST TEXAS REGIONAL AIRPORT","TX",3901,"KGGG","GGG",32.38_r8,-94.71_r8,19980520, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(765) = asosdata("LUBBOCK","LUBBOCK INTERNATIONAL AIRPORT","TX",23042,"KLBB","LBB",33.66_r8,-101.82_r8,19950901, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(766) = asosdata("LUFKIN","ANGELINA COUNTY AIRPORT","TX",93987,"KLFK","LFK",31.23_r8,-94.75_r8,20000823,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(767) = asosdata("M! ALLEN","M! ALLEN MILLER INTERNATIONAL AIRPORT","TX",12959,"KMFE","MFE",26.17_r8,-98.23_r8, &
        19960930,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(768) = asosdata("M! KINNEY","M! KINNEY MUNICIPAL AIRPORT","TX",53914,"KTKI","TKI",33.18_r8,-96.59_r8,19980422, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(769) = asosdata("MIDLAND","MIDLAND INTERNATIONAL AIRPORT","TX",23023,"KMAF","MAF",31.93_r8,-102.2_r8,19960301, &
        "YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(770) = asosdata("MINERAL WELLS","MINERAL WELLS AIRPORT","TX",93985,"KMWL","MWL",32.78_r8,-98.06_r8,20001206, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(771) = asosdata("NEW BRAUNFELS","NEW BRAUNFELS MUNICIPAL AIRPORT","TX",12971,"KBAZ","BAZ",29.7_r8,-98.04_r8, &
        19960229,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(772) = asosdata("ODESSA","ODESSA-SCHLEMEYER FIELD AIRPORT","TX",3031,"KODO","ODO",31.92_r8,-102.38_r8,19981201, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(773) = asosdata("PALACIOS","PALACIOS MUNICIPAL AIRPORT","TX",12935,"KPSX","PSX",28.72_r8,-96.25_r8,20001018, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(774) = asosdata("PORT ISABEL","PORT ISABEL-CAMERON COUNTY AIRPORT","TX",12957,"KPIL","PIL",26.16_r8,-97.34_r8, &
        19980917,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(775) = asosdata("ROCKPORT","ARANSAS COUNTY AIRPORT","TX",12972,"KRKP","RKP",28.08_r8,-97.04_r8,19960229,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(776) = asosdata("SAN ANGELO","SAN ANGELO REGIONAL/MATHIS FIELD AIRPORT","TX",23034,"KSJT","SJT",31.35_r8, &
        -100.49_r8,19960201,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(777) = asosdata("SAN ANTONIO","SAN ANTONIO INTERNATIONAL AIRPORT","TX",12921,"KSAT","SAT",29.53_r8,-98.46_r8, &
        19950601,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(778) = asosdata("SAN ANTONIO","STINSON MUNICIPAL AIRPORT","TX",12970,"KSSF","SSF",29.33_r8,-98.47_r8,19980513, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(779) = asosdata("TERRELL","TERRELL MUNICIPAL AIRPORT","TX",53911,"KTRL","TRL",32.71_r8,-96.26_r8,19970520,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(780) = asosdata("TYLER","TYLER POUNDS REGIONAL AIRPORT","TX",13972,"KTYR","TYR",32.35_r8,-95.4_r8,19980520,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(781) = asosdata("VICTORIA","VICTORIA REGIONAL AIRPORT","TX",12912,"KVCT","VCT",28.86_r8,-96.92_r8,19951201, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(782) = asosdata("WACO","WACO REGIONAL AIRPORT","TX",13959,"KACT","ACT",31.61_r8,-97.22_r8,19930701,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(783) = asosdata("WICHITA FALLS","SHEPPARD AFB/WICHITA FALLS MUNICIPAL AIRPORT","TX",13966,"KSPS","SPS",33.97_r8,&
        -98.49_r8,19930501,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(784) = asosdata("WINK","WINKLER COUNTY AIRPORT","TX",23040,"KINK","INK",31.77_r8,-103.2_r8,20001129,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(785) = asosdata("BRYCE CANYON","BRYCE CANYON AIRPORT","UT",23159,"KBCE","BCE",37.7_r8,-112.14_r8,20001116,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(786) = asosdata("CEDAR CITY","CEDAR CITY REGIONAL AIRPORT","UT",93129,"KCDC","CDC",37.7_r8,-113.09_r8,19980528, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(787) = asosdata("LOGAN","LOGAN-CACHE AIRPORT","UT",94128,"KLGU","LGU",41.78_r8,-111.85_r8,19981001,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(788) = asosdata("MILFORD","MILFORD MUNICIPAL AIRPORT","UT",23176,"KMLF","MLF",38.44_r8,-113.02_r8,19960801,"NO",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(789) = asosdata("MOAB","CANYONLANDS FIELD AIRPORT","UT",93075,"KCNY","CNY",38.75_r8,-109.75_r8,19980716,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(790) = asosdata("OGDEN","OGDEN-HINCKLEY AIRPORT","UT",24126,"KOGD","OGD",41.19_r8,-112.01_r8,19980506,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(791) = asosdata("PRICE","CARBON COUNTY AIRPORT","UT",93141,"KPUC","PUC",39.54_r8,-110.74_r8,19980911,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(792) = asosdata("SALT LAKE CITY","SALT LAKE CITY INTERNATIONAL AIRPORT","UT",24127,"KSLC","SLC",40.78_r8, &
        -111.96_r8,19980301,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(793) = asosdata("VERNAL","VERNAL AIRPORT","UT",94030,"KVEL","VEL",40.44_r8,-109.5_r8,19980129,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(794) = asosdata("CHARLOTTESVILLE","CHARLOTTESVILLE-ALBEMARLE AIRPORT","VA",93736,"KCHO","CHO",38.13_r8, &
        -78.45_r8,19981118,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(795) = asosdata("DANVILLE","DANVILLE REGIONAL AIRPORT","VA",13728,"KDAN","DAN",36.57_r8,-79.33_r8,20000816,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(796) = asosdata("LYNCHBURG","LYNCHBURG REGIONAL/PRESTON GLENN FIELD AIRPORT","VA",13733,"KLYH","LYH",37.33_r8, &
        -79.2_r8,19960801,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(797) = asosdata("NEWPORT NEWS","NEWPORT NEWS/WILLIAMSBURG INTERNATIONAL AIRPORT","VA",93741,"KPHF","PHF", &
        37.13_r8,-76.49_r8,20001213,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(798) = asosdata("NORFOLK","NORFOLK INTERNATIONAL AIRPORT","VA",13737,"KORF","ORF",36.9_r8,-76.19_r8,19960301, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(799) = asosdata("RICHMOND","RICHMOND INTERNATIONAL AIRPORT","VA",13740,"KRIC","RIC",37.51_r8,-77.32_r8,19951001,&
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(800) = asosdata("RICHMOND/ASHLAND","HANOVER COUNTY MUNICIPAL AIRPORT","VA",93775,"KOFP","OFP",37.7_r8,-77.43_r8,&
        19950301,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(801) = asosdata("ROANOKE","ROANOKE REGIONAL/WOODRUM FIELD AIRPORT","VA",13741,"KROA","ROA",37.31_r8,-79.97_r8, &
        19960501,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(802) = asosdata("WAKEFIELD","WAKEFIELD MUNICIPAL AIRPORT","VA",93773,"KAKQ","AKQ",36.98_r8,-77._r8,19971016, &
        "NO","NWS",26.00_r8,7.92_r8)
    asoscommdates(803) = asosdata("WALLOPS ISLAND","WALLOPS FLIGHT FACILITY AIRPORT","VA",93739,"KWAL","WAL",37.94_r8,-75.49_r8, &
        19960901,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(804) = asosdata("BARRE/MONTPELIER","EDWARD F KNAPP STATE AIRPORT","VT",94705,"KMPV","MPV",44.2_r8,-72.57_r8, &
        19960618,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(805) = asosdata("BENNINGTON","WILLIAM H. MORSE STATE AIRPORT","VT",54781,"KDDH","DDH",42.89_r8,-73.24_r8, &
        19981209,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(806) = asosdata("BURLINGTON","BURLINGTON INTERNATIONAL AIRPORT","VT",14742,"KBTV","BTV",44.46_r8,-73.15_r8, &
        19960201,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(807) = asosdata("MORRISVILLE","MORRISVILLE-STOWE STATE AIRPORT","VT",54771,"KMVL","MVL",44.53_r8,-72.61_r8, &
        19951115,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(808) = asosdata("SPRINGFIELD","HARTNESS STATE (SPRINGFIELD) AIRPORT","VT",54740,"KVSF","VSF",43.34_r8,-72.51_r8,&
        19950823,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(809) = asosdata("ST. JOHNSBURY","FAIRBANKS MUSEUM AMOS","VT",54742,"K1V4","1V4",44.41_r8,-72.01_r8,19980715, &
        "NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(810) = asosdata("BELLINGHAM","BELLINGHAM INTERNATIONAL AIRPORT","WA",24217,"KBLI","BLI",48.79_r8,-122.53_r8, &
        19980917,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(811) = asosdata("DEER PARK","DEER PARK AIRPORT","WA",94119,"KDEW","DEW",47.96_r8,-117.42_r8,19981105,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(812) = asosdata("ELLENSBURG","BOWERS FIELD AIRPORT","WA",24220,"KELN","ELN",47.03_r8,-120.53_r8,19980730,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(813) = asosdata("EPHRATA","EPHRATA MUNICIPAL AIRPORT","WA",24141,"KEPH","EPH",47.3_r8,-119.51_r8,20010920,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(814) = asosdata("EVERETT","SNOHOMISH COUNTY (PAINE FIELD) AIRPORT","WA",24222,"KPAE","PAE",47.9_r8,-122.28_r8, &
        19980924,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(815) = asosdata("FRIDAY HARBOR","FRIDAY HARBOR AIRPORT","WA",94276,"KFHR","FHR",48.52_r8,-123.02_r8,19971218, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(816) = asosdata("HOQUIAM","BOWERMAN AIRPORT","WA",94225,"KHQM","HQM",46.97_r8,-123.93_r8,20010322,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(817) = asosdata("MOSES LAKE","GRANT COUNTY INTERNATIONAL AIRPORT","WA",24110,"KMWH","MWH",47.2_r8,-119.31_r8, &
        19970630,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(818) = asosdata("OLYMPIA","OLYMPIA AIRPORT AIRPORT","WA",24227,"KOLM","OLM",46.97_r8,-122.9_r8,19951101,"YES", &
        "NWS",26.00_r8,7.92_r8)
    asoscommdates(819) = asosdata("OMAK","OMAK AIRPORT","WA",94197,"KOMK","OMK",48.46_r8,-119.51_r8,19980217,"NO","FAA",33.00_r8, &
        10.05_r8)
    asoscommdates(820) = asosdata("PASCO","TRI-CITIES AIRPORT","WA",24163,"KPSC","PSC",46.26_r8,-119.11_r8,19980129,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(821) = asosdata("PORT ANGELES","WILLIAM R. FAIRCHILD INTERNATIONAL AIRPORT","WA",94266,"KCLM","CLM",48.12_r8, &
        -123.49_r8,19981016,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(822) = asosdata("PULLMAN/MOSCOW","PULLMAN/MOSCOW REGIONAL AIRPORT","WA",94129,"KPUW","PUW",46.74_r8,-117.1_r8, &
        19980611,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(823) = asosdata("QUILLAYUTE","QUILLAYUTE AIRPORT","WA",94240,"KUIL","UIL",47.93_r8,-124.56_r8,19961201,"YES", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(824) = asosdata("RENTON","RENTON MUNICIPAL AIRPORT","WA",94248,"KRNT","RNT",47.49_r8,-122.21_r8,19981008,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(825) = asosdata("SEATTLE","BOEING FIELD/KING COUNTY INTERNATIONAL AIRPORT","WA",24234,"KBFI","BFI",47.53_r8, &
        -122.3_r8,19981209,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(826) = asosdata("SEATTLE","SEATTLE-TACOMA INTERNATIONAL AIRPORT","WA",24233,"KSEA","SEA",47.46_r8,-122.31_r8, &
        19961001,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(827) = asosdata("SHELTON","SANDERSON FIELD AIRPORT","WA",94227,"KSHN","SHN",47.23_r8,-123.14_r8,19980522,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(828) = asosdata("SPOKANE","SPOKANE INTERNATIONAL AIRPORT","WA",24157,"KGEG","GEG",47.62_r8,-117.52_r8,19950901, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(829) = asosdata("SPOKANE","FELTS FIELD AIRPORT","WA",94176,"KSFF","SFF",47.68_r8,-117.32_r8,19981015,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(830) = asosdata("STAMPEDE PASS","STAMPEDE PASS FLTWO","WA",24237,"KSMP","SMP",47.29_r8,-121.33_r8,19940201,"NO",&
        "NWS",26.00_r8,7.92_r8)
    asoscommdates(831) = asosdata("TACOMA","TACOMA NARROWS AIRPORT","WA",94274,"KTIW","TIW",47.26_r8,-122.57_r8,19990114,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(832) = asosdata("VANCOUVER","PEARSON FIELD AIRPORT","WA",94298,"KVUO","VUO",45.62_r8,-122.65_r8,19960619,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(833) = asosdata("WALLA WALLA","WALLA WALLA REGIONAL AIRPORT","WA",24160,"KALW","ALW",46.09_r8,-118.28_r8, &
        19981023,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(834) = asosdata("WENATCHEE","PANGBORN MEMORIAL AIRPORT","WA",94239,"KEAT","EAT",47.39_r8,-120.2_r8,20001130, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(835) = asosdata("YAKIMA","YAKIMA AIR TERMINAL/MCALLISTER FIELD AIRPORT","WA",24243,"KYKM","YKM",46.56_r8, &
        -120.53_r8,19960401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(836) = asosdata("ASHLAND","JOHN F KENNEDY INTERNATIONAL AIRPORT","WI",94929,"KASX","ASX",46.54_r8,-90.91_r8, &
        19981016,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(837) = asosdata("B0SCOBEL","BOSCOBEL AIRPORT","WI",94994,"KOVS","OVS",43.15_r8,-90.67_r8,19990121,"NO","FAA", &
        33.00_r8,10.05_r8)
    asoscommdates(838) = asosdata("EAU CLAIRE","CHIPPEWA VALLEY REGIONAL AIRPORT","WI",14991,"KEAU","EAU",44.86_r8,-91.48_r8, &
        20000824,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(839) = asosdata("FOND DU LAC","FOND DU LA! COUNTY AIRPORT","WI",4840,"KFLD","FLD",43.77_r8,-88.48_r8,19960729, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(840) = asosdata("GREEN BAY","AUSTIN STRAUBEL INTERNATIONAL AIRPORT","WI",14898,"KGRB","GRB",44.51_r8,-88.12_r8, &
        19960701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(841) = asosdata("HAYWARD","SAWYER COUNTY AIRPORT","WI",94973,"KHYR","HYR",46.02_r8,-91.44_r8,19951114,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(842) = asosdata("KENOSHA","KENOSHA REGIONAL AIRPORT","WI",4845,"KENW","ENW",42.59_r8,-87.93_r8,19971009,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(843) = asosdata("LA CROSSE","LA CROSSE MUNICIPAL AIRPORT","WI",14920,"KLSE","LSE",43.75_r8,-91.25_r8,20001005, &
        "YES","FAA",33.00_r8,10.05_r8)
    asoscommdates(844) = asosdata("LONE ROCK","TRI-COUNTY REGIONAL AIRPORT","WI",14921,"KLNR","LNR",43.21_r8,-90.18_r8,20001019, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(845) = asosdata("MADISON","DANE COUNTY REGIONAL-TRUAX FIELD AIRPORT","WI",14837,"KMSN","MSN",43.14_r8,-89.34_r8,&
        19960401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(846) = asosdata("MARSHFIELD","MARSHFIELD MUNICIPAL AIRPORT","WI",94985,"KMFI","MFI",44.63_r8,-90.18_r8,19951024,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(847) = asosdata("MILWAUKEE","GENERAL MITCHELL INTERNATIONAL AIRPORT","WI",14839,"KMKE","MKE",42.94_r8,-87.89_r8,&
        19950701,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(848) = asosdata("OSHKOSH","WHITTMAN REGIONAL AIRPORT","WI",94855,"KOSH","OSH",43.98_r8,-88.55_r8,19960417,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(849) = asosdata("RACINE","JOHN H BATTEN AIRPORT","WI",94818,"KRAC","RAC",42.76_r8,-87.81_r8,19980326,"NO","FAA",&
        33.00_r8,10.05_r8)
    asoscommdates(850) = asosdata("RHINELANDER","RHINELANDER-ONEIDA COUNTY AIRPORT","WI",4803,"KRHI","RHI",45.63_r8,-89.46_r8, &
        19980528,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(851) = asosdata("SHEBOYGAN","SHEBOYGAN COUNTY MEMORIAL AIRPORT","WI",4841,"KSBM","SBM",43.76_r8,-87.85_r8, &
        19960815,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(852) = asosdata("WAUSAU","WAUSAU DOWNTOWN AIRPORT","WI",14897,"KAUW","AUW",44.92_r8,-89.62_r8,20000914,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(853) = asosdata("WISCONSIN RAPTIDS","ALEXANDER FIELD SOUTH WOOD COUNTY AIRPORT","WI",4826,"KISW","ISW",44.35_r8,&
        -89.83_r8,19951114,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(854) = asosdata("BECKLEY","RALEIGH COUNTY MEMORIAL AIRPORT","WV",3872,"KBKW","BKW",37.79_r8,-81.12_r8,19960201, &
        "YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(855) = asosdata("BLUEFIELD","MERCER COUNTY AIRPORT","WV",3859,"KBLF","BLF",37.29_r8,-81.2_r8,20001108,"NO", &
        "FAA",26.00_r8,7.92_r8)
    asoscommdates(856) = asosdata("CHARLESTON","YEAGER AIRPORT","WV",13866,"KCRW","CRW",38.37_r8,-81.59_r8,19941001,"YES","NWS", &
        26.00_r8,7.92_r8)
    asoscommdates(857) = asosdata("CLARKSBURG","HARRISON/MARION REGIONAL AIRPORT","WV",3802,"KCKB","CKB",39.29_r8,-80.22_r8, &
        19980812,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(858) = asosdata("ELKINS","ELKINS-RANDOLPH COUNTY-JENNINGS RANDOLPH FLD ARPT","WV",13729,"KEKN","EKN",38.88_r8, &
        -79.85_r8,19960501,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(859) = asosdata("HUNTINGTON","TRI-STATE/MILTON J.FERGUSON FIELD AIRPORT","WV",3860,"KHTS","HTS",38.38_r8, &
        -82.55_r8,19960901,"YES","NWS",26.00_r8,7.92_r8)
    asoscommdates(860) = asosdata("MARTINSBURG","EASTERN WEST VIRGINIA REGIONAL/SHEPHERD FLD AIRPORT","WV",13734,"KMRB","MRB", &
        39.4_r8,-77.98_r8,20001129,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(861) = asosdata("MORGANTOWN","MORGANTOWN REGIONAL-WALTER L. BILL HART FLD AIRPORT","WV",13736,"KMGW","MGW", &
        39.64_r8,-79.91_r8,19990106,"NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(862) = asosdata("PARKERSBURG","WOOD COUNTY AIRPORT GILL ROBB WILSON FIELD AIRPORT","WV",3804,"KPKB","PKB", &
        39.34_r8,-81.43_r8,20001108,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(863) = asosdata("WHEELING","WHEELING OHIO COUNTY AIRPORT","WV",14894,"KHLG","HLG",40.17_r8,-80.64_r8,19980325, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(864) = asosdata("BIG PINEY","BIG PINEY-MARBLETON AIRPORT","WY",24164,"KBPI","BPI",42.58_r8,-110.1_r8,19980226, &
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(865) = asosdata("BUFFALO","JOHNSON COUNTY AIRPORT","WY",94054,"KBYG","BYG",44.38_r8,-106.72_r8,19980730,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(866) = asosdata("CASPER","NATRONA COUNTY INTERNATIONAL AIRPORT","WY",24089,"KCPR","CPR",42.89_r8,-106.47_r8, &
        19960401,"YES","NWS",33.00_r8,10.05_r8)
    asoscommdates(867) = asosdata("CHEYENNE","CHEYENNE AIRPORT","WY",24018,"KCYS","CYS",41.15_r8,-104.8_r8,19951101,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(868) = asosdata("DOUGLAS","CONVERSE COUNTY AIRPORT","WY",94057,"KDGW","DGW",42.79_r8,-105.38_r8,19980528,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(869) = asosdata("EVANSTON","EVANSTON-UINTA COUNTY BURNS FIELD AIRPORT","WY",4111,"KEVW","EVW",41.27_r8, &
        -111.03_r8,19990729,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(870) = asosdata("GILLETTE","GILLETTE-CAMPBELL COUNTY AIRPORT","WY",94023,"KGCC","GCC",44.33_r8,-105.54_r8, &
        19980716,"NO","NWS",26.00_r8,7.92_r8)
    asoscommdates(871) = asosdata("GREYBULL","SOUTH BIG HORN COUNTY AIRPORT","WY",24048,"KGEY","GEY",44.51_r8,-108.08_r8,19981015,&
        "NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(872) = asosdata("LANDER","HUNT FIELD AIRPORT","WY",24021,"KLND","LND",42.81_r8,-108.73_r8,19961201,"YES","NWS", &
        33.00_r8,10.05_r8)
    asoscommdates(873) = asosdata("LARAMIE","LARAMIE REGIONAL AIRPORT","WY",24022,"KLAR","LAR",41.31_r8,-105.67_r8,20000928,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(874) = asosdata("RAWLINS","RAWLINS MUNICIPAL AIRPORT","WY",24057,"KRWL","RWL",41.8_r8,-107.2_r8,20000928,"NO", &
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(875) = asosdata("RIVERTON","RIVERTON REGIONAL AIRPORT","WY",24061,"KRIW","RIW",43.06_r8,-108.45_r8,19951201, &
        "NO","NWS",33.00_r8,10.05_r8)
    asoscommdates(876) = asosdata("ROCK SPRINGS","ROCK SPRINGS-SWEETWATER COUNTY AIRPORT","WY",24027,"KRKS","RKS",41.59_r8, &
        -109.06_r8,20010517,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(877) = asosdata("SHERIDAN","SHERIDAN COUNTY AIRPORT","WY",24029,"KSHR","SHR",44.77_r8,-106.97_r8,19961201,"YES",&
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(878) = asosdata("TORRINGTON","TORRINGTON MUNICIPAL AIRPORT","WY",94053,"KTOR","TOR",42.06_r8,-104.15_r8, &
        19990826,"NO","FAA",33.00_r8,10.05_r8)
    asoscommdates(879) = asosdata("WORLAND","WORLAND MUNICIPAL AIRPORT","WY",24062,"KWRL","WRL",43.96_r8,-107.95_r8,20001220,"NO",&
        "FAA",33.00_r8,10.05_r8)
    asoscommdates(880) = asosdata("YELLOWSTONE","YELLOWSTONE LAKE","WY",94173,"KP60","P60",44.54_r8,-110.42_r8,19980813,"NO", &
        "NWS",33.00_r8,10.05_r8)
    asoscommdates(881) = asosdata("AGANA","GUAM INTERNATIONAL AIRPORT"," ",41415,"PGUM","GUM",13.48_r8,144.79_r8,20000111,"YES", &
        "NWS",-9._r8,-9._r8)
    asoscommdates(882) = asosdata("CHARLOTTE AMALIE","CYRIL E KING AIRPORT"," ",11640,"TIST","STT",18.34_r8,-64.97_r8,19980810, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(883) = asosdata("CHRISTIANSTED","HENRY E ROHISEN AIRPORT"," ",11624,"TISX","STX",17.7_r8,-64.79_r8,20000808, &
        "NO","FAA",26.00_r8,7.92_r8)
    asoscommdates(884) = asosdata("NAVSTN ROOSEVELT ROADS; CEIBA","ROOSEVELT ROADS"," ",11630,"TJNR","NRR",18.25_r8,-65.63_r8, &
        20051019,"YES","NWS",-9._r8,-9._r8)
    asoscommdates(885) = asosdata("OBYAN","FRANCISCO C. ADA/SAIPAN INTERNATIONAL AIRPORT"," ",41418,"PGSN","GSN",15.11_r8, &
        145.72_r8,20000111,"NO","NWS",-9._r8,-9._r8)
    asoscommdates(886) = asosdata("SAN JUAN","LUIS MUNOZ MARIN INTERNATIONAL AIRPORT"," ",11641,"TJSJ","SJU",18.44_r8,-66._r8, &
        19960501,"YES","NWS",33.00_r8,10.05_r8)


    sfformats(1)=sformats('CD144',.false.,0,19951231)
    sfformats(2)=sformats('SCRAM',.false.,19840101,19921231)
    sfformats(3)=sformats('SAMSON',.false.,19610101,19901231)
    sfformats(4)=sformats('3280VB',.false.,0,99999999)
    sfformats(5)=sformats('3280FB',.false.,0,99999999)
    sfformats(6)=sformats('HUSWO',.false.,19900101,19951231)
    sfformats(7)=sformats('ISHD',.false.,0,99999999)
    sfformats(8)=sformats('EXTRACT',.false.,0,99999999)
    sfformats(9)=sformats('CD144FB',.false.,0,19951231)
    
!   extraction message formats
!   1.  invalid date field or station ID
    write(ext_form(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'

!   2.  station ID does not match user entered station ID
    write(ext_form(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5,2(1x,a))'
    
!   3.  reached limit of warning messages
    write(ext_form(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,1x,a3,1x,a)'
    
!   4.  station found in ASOS list
    write(ext_form(4),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),a)'
    
!   5.  observation replaced (note that read_ishd has it's own format because it reports what kind of report is replacing)
!   or data source indicates ASOS but no commission date
    write(ext_form(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20)'

!   6.  number of observations flagged as calm or variable (for ISHD data)
    write(ext_form(6),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5)'

!   7.  station is ASOS but before commission date 
    write(ext_form(7),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,i8)'

!   8.  data source indicates ASOS but before commission date, set as ASOS
    write(ext_form(8),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,a20)'
    
!   9.  user entered elevation differs from elevation in file
    write(ext_form(9),'(2(a))')trim(adjustl(msg_form)),'a,1x,f9.2,1x,a,1x,f9.2,a)'

!	10.	invalid entry (for CD144 and SCRAM)
        write(ext_form(10),'(2(a))')trim(adjustl(msg_form)),'a,1x,a4,2(1x,a),1x,a20)'
    return
    end subroutine sfc_init
!*********************************************************************************************************
      
    subroutine surf_path
!=========================================================================================================
!   SUBROUTINE SURF_PATH
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE SURFACE PATHWAY.  THIS INCLUDES: DATA, EXTRACT, QAOUT, RANGE, AUDIT, NO_MISSING,
!   KEYWORDS XDATES, AND LOCATION KEYWORDS
!
!   MODIFIED DECEMBER 3, 2021
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
!   j:              counter for checking DATA file format against valid surface data file types
!   jj:             counter for lsfkeywords based on ikey value
!
!   Logical variables
!   lvalid:         input DATA file type is valid (true) or invalid (false)
!   lgood:          variable denoting if a filename is okay
!
!   Character variables
!   form1:          format to read filename
!   fileform:       character string of DATA file format to compare against adformats array
!   asosflag:       character string ASOS denoting data file is from ASOS station
!   formstr:        format for messages
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: inpline,inpline1,ikey,nfield,ilen,keywrd,checkfile,dataline,getdates,getloc,fileind,formind,writeunit,&
        getunit  
    use file_units, only: flength,sf_data_unit,sf_inpfile,sf_qaout_unit,sf_extract_unit,sf_qafile,sf_extfile,one_min_unit,&
        one_minfile
    implicit none
      
    integer(kind=4) :: i,i1,j,jj      
    logical :: lvalid,lgood
      
    character(len=6) :: form1
    character(len=10) ::fileform
    character(len=4) :: asosflag
    character(len=60) :: formstr(4)
    character(len=10) :: modnam='SURF_PATH'
      
!   initialize
    i=0
    i1=0
    j=0
    jj=0
    lvalid=.false.
    lgood=.true.
    lbad=.false.
      
!   1.  invalid or duplicate keyword or invalid /not allowed asos flag
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2.  invalid number of fields
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'i3,2(1x,a))'
    
!   3.  too many fields
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,3(1x,a))'

!   4.  invalid file format
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
    
!   form1 is the format to use to read the message or report file
!   format string is (a300)
    write(form1,'(a2,i3,a1)')'(a',flength,')'

!   get file unit for messages
    call getunit
      
!   ikey = 0 means text string SURFACE found on input line
!   set the logical variables for keywords below to false
!   meaning they have not been detected yet
!   initialize surface variables
    if (ikey == 0) then
!       set lsfkeywords(8) to true because at least variables are audited
        lsfkeywords(8)=.true.
        call sfc_init !initialize variables
    else
        i=index(inpline1,trim(adjustl(keywrd(ikey))))
        i1=len_trim(keywrd(ikey))
          
!       set the logical variable for the keyword to true
        if ((ikey == 4 .or. ikey== 5 .or. ikey == 12 .or. ikey == 13 .or. ikey==32) .or. (ikey >= 7 .and. ikey <=10)) then
            if (ikey <=5) then
                jj=ikey-3
            elseif (ikey == 12 .or. ikey == 13) then
                jj=ikey-5
            elseif (ikey == 32) then
                jj=9
            else
                jj=ikey-4
            endif
            if (.not. lsfkeywords(jj)) then
                lsfkeywords(jj)=.true.
            else
!               issue error message that keyword has already been found
                if (ikey == 4 .or. (ikey >=7 .and. ikey < 11 )) then
                    write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,'DUPLICATE ENTRY OF KEYWORD:',&
                        trim(adjustl(keywrd(ikey)))
                    lbad=.true.
                    return
                endif
            endif
        else !invalid keyword for this path
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,'INVALID KEYWORD:',&
                        trim(adjustl(keywrd(ikey)))
            lbad=.true.
            return
        endif
          
        if (ikey == 4) then !data file
            sfstage1=.true.
!           if not 3 to 6 fields, then there is an error
            if (nfield < 3 .or. nfield > 6) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
!               issue warning that blocking factor and ASCII string not needed
!               this is not an error. allow for the fact of the optional ASOS flag
                if (nfield > 4) write(writeunit,formstr(3))adjustl(pathid(ipath)),'W01',modnam,'TOO MANY FIELDS',nfield,'FOR',&
                    trim(adjustl(keywrd(ikey))),'KEYWORD; BLOCKING FACTOR AND/OR TYPE NOT NEEDED'
                      
!               read DATA line to get input file and format
!               and positions in input line to read
!               see subroutine dataline for details
                call dataline(i,i1)
                      
!               read the original line to get the filename
                read(inpline(fileind(1):fileind(2)),'(a)')sf_inpfile
                      
!               read the uppercase version of the line to get the file format
                read(inpline1(formind(1):formind(2)),'(a)')fileform
                      
!               get the DATA filename
                call checkfile(sf_data_unit,sf_inpfile,1,lgood)
                      
!               check the format
                j=1
!               lvalid=.false.
                do while (j .le. nsfformats .and. .not. lvalid)
                    if (trim(adjustl(fileform)) == trim(adjustl(sfformats(j)%sformat))) then
                        sfformats(j)%lsfform=.true.
                        lvalid=.true.
                        isfform=j
                    endif
                    j=j+1
                enddo
                if (.not. lvalid) then
!                   invalid file format
                    write(writeunit,formstr(4))adjustl(pathid(ipath)),'E04',modnam,'INVALID FILE FORMAT',trim(adjustl(fileform)),&
                        'FOR SURFACE FILE',trim(adjustl(sf_inpfile))
                    lbad=.true.
                elseif (lvalid .and. (sfformats(4)%lsfform .or. sfformats(5)%lsfform)) then
                    write(writeunit,formstr(4))adjustl(pathid(ipath)),'E04',modnam,'OBSOLETE FILE FORMAT',trim(adjustl(fileform)),&
                        'FOR SURFACE FILE',trim(adjustl(sf_inpfile))
                    lbad=.true.
                endif

!               check to see if the optional ASOS flag is present or it could be blocking factor or type
                if (nfield == 4) then
                    read(inpline1(formind(2):ilen),'(a)')asosflag
                    if (trim(adjustl(asosflag)) == 'ASOS') then
                        if (sfformats(7)%lsfform) then
                            lasos=.true.
                        else
!                           can't have ASOS flag for non-ISHD data, issue fatal error
                            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'ASOS FLAG NOT ALLOWED FOR DATA TYPE:',&
                                trim(adjustl(sfformats(j-1)%sformat))
                            lbad=.true.
                        endif
                    else
                        if (sfformats(7)%lsfform) then
                            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR ASOS FLAG',&
                                trim(adjustl(asosflag))
                            lbad=.true. 
                        else
!                           issue warning
                            write(writeunit,formstr(3))adjustl(pathid(ipath)),'W01',modnam,'TOO MANY FIELDS',nfield,'FOR',&
                                trim(adjustl(keywrd(ikey))),'KEYWORD; BLOCKING FACTOR AND/OR TYPE NOT NEEDED'
                        endif
                    endif
                endif
            endif
              
        else if (ikey == 5) then !no_missing
  
            call surf_list(i+i1)
              
        else if (ikey == 7) then !extract
            sfstage1=.true.
!           if number of fields is not 2, then line is bad
            if (nfield /= 2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                read(inpline(i+i1+1:ilen),form1)sf_extfile
!               get the EXTRACT filename
                call checkfile(sf_extract_unit,sf_extfile,4,lgood)
            endif 

        else if (ikey == 8) then !qaout
!           if number of fields is not 2, then line is bad
            if (nfield /= 2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                read(inpline(i+i1+1:ilen),form1)sf_qafile
!               get the QAOUT filename
                call checkfile(sf_qaout_unit,sf_qafile,4,lgood)
                
            endif

        else if (ikey == 9) then !xdates
!           incorrect number of fields, line is bad                  
            if (nfield /= 3 .and. nfield /= 4 .and. nfield /= 7 .and. nfield /= 8) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else  
                call getdates(i+i1,sfstart,sfend,sfdates)
            endif
                 
        else if (ikey == 10) then !location
!           incorrect number of fields                  
            if (nfield < 4 .or. nfield > 6) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call getloc(i+i1,sfid,sflat,sflon,sfgmt2lst,sfelev,lgmt2lst,user_elev)
                sf_user_elev=sfelev
            endif

        else if (ikey == 12) then !range
            if (nfield /= 6) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call surf_range(i+i1)
            endif
              
        else if (ikey == 13) then !audit
!           lsfaudit is already true since three variables are
!           automatically audited for surface data
!           call surf_list to get any other variables to audit
            call surf_list(i+i1)
          
        else !ikey = 32; ASOS one-minute output
!           if number of fields is not 2, then line is bad
            if (nfield /= 2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,&
                    'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                read(inpline(i+i1+1:ilen),form1)one_minfile
!               get the AERMINUTE filename
                call checkfile(one_min_unit,one_minfile,1,lgood)
            endif   
        endif
    endif

    return
      
    end subroutine surf_path
!*********************************************************************************************************

    subroutine surf_list(i1)
!=========================================================================================================
!   SUBROUTINE SURF_LIST
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE NO_MISSING OR AUDIT KEYWORD FOR SURFACE DATA.
!   THIS SUBROUTINE IS ANALOGOUS TO UP_LIST
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (SURF_PATH)
!
!   INPUT ARGUMENT(S)
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!
!   NOTE: nsfvars is the number of surface variables
!
!   Logical variables
!   sflogical:      Array of values for logical check for missing values (true=skip in message file,
!                   false=report, this is default) or audit (true=audit,false=don't audit)
!
!   Character variables
!   sfvar:               array of surface variable names
!   formstr:            formats for messages
!   modnam:             Subroutine name
!=========================================================================================================   
      
    use main1, only : inpline1,var_list,ikey,nfield,writeunit
   
    implicit none
    integer(kind=4),intent(in) :: i1
    integer(kind=4):: nfield1,i
    logical :: sflogical(nsfvars)
    character(len=4) :: sfvar(nsfvars)
    character(len=60) :: formstr
    character(len=10) :: modnam='SURF_LIST'
      
!   initialize
    i=0
    nfield1=nfield-1

!   format for warning messages about duplicates
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
    
!   initialize sfvar and sflogical to pass to var_list
    v1: do i=1,nsfvars
        sfvar(i)=sfvars(i)%varname
!       set sflogical to appropriate values based on keyword
        if (ikey == 5) then
            sflogical(i)=sfvars(i)%lnomiss
        else
            sflogical(i)=sfvars(i)%laudit
        endif
    enddo v1

!   check the string of variables for keyword
    call var_list(i1,nfield1,sfvar,sflogical,nsfvars)

!   reset the values of the logical variables of whether to track missing values or audit variables
!   issue warning if variable has already been found for the keyword
    v2: do i=1,nsfvars
        if (ikey == 5) then
            if (sfvars(i)%lnomiss)write(writeunit,formstr)adjustl(pathid(ipath)),'W02',modnam,&
                'DUPLICATING LISTING OF VARIABLE FOR',trim(adjustl(sfvars(i)%varname)),'KEYWORD:','NO_MISSING'
            sfvars(i)%lnomiss=sflogical(i)
        else
!           issue warning if variable is already being audited
!           check for keyword on the AUDIT line; this is so if TMPD, WSPD, and WDIR
!           are not listed, then a message won't be written since they are always being audited.
            
            if (sfvars(i)%laudit .and. index(inpline1,sfvars(i)%varname) >0) then
                write(writeunit,formstr)adjustl(pathid(ipath)),'W02',modnam,&
                'DUPLICATING LISTING OF VARIABLE FOR',trim(adjustl(sfvars(i)%varname)),'KEYWORD:','AUDIT'
            else
                sfvars(i)%laudit=sflogical(i)
            endif
        endif
    enddo v2
    
    return 
    end subroutine surf_list
!*********************************************************************************************************

    subroutine surf_range(i1)
!=========================================================================================================
!   SUBROUTINE SURF_RANGE
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE RANGE KEYWORD FOR SURFACE DATA TO MODIFY THE RANGES OF VALID DATA.
!   THIS SUBROUTINE IS ANALAGOUS TO UPPER_RANGE
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (SURF_PATH, SF_STAGE2)
!
!   INPUT ARGUMENT(S)
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!   lowval:         numerical lower bound read from v1
!   hival:          numerical upper bound read from v3
!   missval:        numerical missing value read from v4
!
!   NOTE: nsfvars is the number of surface variables
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
!   formstr:            formats for messages
!   modnam:         Subroutine name
!=========================================================================================================   
    use main1, only : range_mod,nfield,getfields,writeunit
    implicit none
    integer(kind=4),intent(in) :: i1
    integer(kind=4):: nfield1,i,lowval,hival,missval
    character(len=10) :: modnam='SURF_RANGE'
    character(len=60) :: formstr
    logical :: lfound,linc,lgood
    character(len=100) :: varnam1  !for getfields
      
      
    allocatable :: varnam1(:)

!   format for warning messages about invalid variable name or duplicates
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'

!   initialize
    i=1
    lowval=-9
    hival=-9
    missval=-9
    lfound=.false.
    linc=.false.
    lgood=.true.
    nfield1=nfield-1
      
    allocate(varnam1(nfield1))
    varnam1='0000000000000000000000000'
!   read input line to get variable name, the inclusion indicator, lower bound, and upper bound
    call getfields(i1,nfield1,varnam1)
    
!   read varnam1 to see if in the surface variable list

    do while(i <= nsfvars .and. .not. lfound)
        if (trim(adjustl(varnam1(1))) == trim(adjustl(sfvars(i)%varname))) then
            lfound=.true.
        else
            i=i+1
        endif
    enddo
      
!   if the variable from the input line is in the surface array
!   then get the new ranges and/or missing data value.
    if (lfound) then
        call range_mod(varnam1(2),varnam1(3),varnam1(4),varnam1(5),linc,lowval,hival,missval,lgood)
        if (lgood) then
!           if variable has already been modified then warn user but still reset values
            if (sfvars(i)%lmod) write(writeunit,formstr)adjustl(pathid(ipath)), 'W02',modnam,&
                'DUPLICATING LISTING OF VARIABLE FOR RANGE KEYWORD:',trim(adjustl(sfvars(i)%varname))
!           even if variable modified previously, use the latest values
            sfvars(i)%lincbound=linc
            sfvars(i)%lowbound=lowval
            sfvars(i)%upbound=hival
            sfvars(i)%missval=missval
            if (.not. sfvars(i)%lmod) sfvars(i)%lmod=.true.
        endif
    else
        write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID VARIABLE NAME FOR RANGE KEYWORD:',&
            trim(adjustl(varnam1(1)))
        lbad=.true.
    endif
      
    deallocate(varnam1)
      
    return 
    end subroutine surf_range
!*********************************************************************************************************

    subroutine sf_test
!=========================================================================================================
!   SUBROUTINE SF_TEST
!   THIS SUBROUTINE CHECKS THAT MANDATORY KEYWORDS HAVE BEEN INCLUDED
!   THIS IS NOT A CHECK ON THE SYNTAX OR IF INCLUDED FILENAMES EXIST
!   THAT HAS BEEN DONE EARLIER IN SF_PATH
!
!   MODIFIED DECEMBER 3, 2021
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
!   iflag:          I/O flag for reading isfid from sfid
!   sdate:          date of observation (yyyymmdd) for use when checkhr is true
!
!   Logical variables
!   lfound:         logical variable used in DO WHLE LOOP for ASOS flag check
!   no_extract:     true if EXTRACT keyword not found or EXTRACT file is empty
!   asoshr1:        dummy logical variable used in subroutine check_assos
!
!   Character variables
!   junk:           data read from first line of EXTRACT file (if any data)
!   formstr:            formats for messages
!   modnam:         Subroutine name
!=========================================================================================================   
    use file_units, only: sf_extract_unit
    use main1, only: keywrd,lstage
    implicit none
    integer(kind=4) :: i,iflag
    integer(kind=8) :: sdate=0
    logical :: lfound,no_extract,asoshr1
    character(len=8) :: junk
    character(len=60) :: formstr(5)
    character(len=10) :: modnam='SF_TEST'
      
!   initialize
!   1.  missing keyword
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2.  xdates missing; read from METPREP
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a)'
    
!   3.  station found in ASOS list or ASOS flag set
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),a)'

!   4.  station found in ASOS list for non-ISHD data
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'

!   5.  commission date
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),i8)'
    
    i=0
    lfound=.false.
    no_extract=.false.
    asoshr1=.false.
    junk=''
!   note that when SF_TEST is called, msg_unit has been set
!   no longer need to write messages to the value from getunit
!   write to msg_unit
      
!   DATA keyword if stage 1 only
!   if stage 1 only and both EXTRACT and QAOUT are listed
!   then AERMET assumes data is not read from raw input 
!   but extracted data is QA'd
!   if the EXTRACT datafile is empty and no QAOUT, and DATA keyword is
!   not listed, then issue error.
!   DATA not needed if running stage 2 without stage 1
    if (sfstage1 .and. .not. lsfkeywords(1)) then
!       if EXTRACT keyword has been found, check to see if
!       the file is empty. 1st line should have word LOCATION
        if (lsfkeywords(3)) then
            read(sf_extract_unit,*,iostat=iflag)junk
            if ((iflag == 0 .and. index(junk,'AERMET') == 0) .or. iflag /=0) no_extract=.true. !file is empty or bad
            rewind(sf_extract_unit)
        else
            no_extract=.true.
        endif
!       if no EXTRACT keyword or empty EXTRACT file and no QAOUT keyword and only
!       running stage 1, then error
        if ((no_extract .or. (.not. no_extract .and. .not. lsfkeywords(4))) .and. .not. lstage(2)) then
            write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(4)))
            lbad=.true.
        endif
    elseif (lsfkeywords(1)) then
        writeext=.true.
    endif
          
!   check EXTRACT if processing stage 1 only
!   if stage 1 and 2, then it is optional
    if (sfstage1 .and. .not. lstage(2) .and. .not. lsfkeywords(3)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(7)))
        lbad=.true.
    endif
      
!   check QAOUT if stage 2 only
!   if stage 1 or stage 1 and 2, then it's optional
    if (.not. sfstage1 .and. lstage(2) .and. .not. lsfkeywords(4)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(8)))
        lbad=.true.
    endif
      
!   check XDATES
!   must be there if stage 1 and not stage 2
!   if stage 1 and 2 and no XDATES, then issue warning that
!   xdates will come from METPREP XDATES
    if (sfstage1) then
        if (.not. lsfkeywords(5)) then
            if (lstage(2)) then
                write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W03',modnam,&
                    'XDATES KEYWORD MISSING; READ XDATES FROM METPREP XDATES'
            else
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(9)))
                lbad=.true.
            endif
        endif
    endif
      
!   check LOCATION if stage 1 and not stage 2
!   not needed for stage 2
    if (sfstage1 .and. .not. lstage(2) .and. .not. lsfkeywords(6)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(10)))
        lbad=.true.
    endif
   
!   check to see if station is in ASOS list regardless
!   of whether ISHD data or non-ISHD data
!   read integer value of sfid and if not a number set isfid = 99999
    if (sfstage1 .and. lsfkeywords(6)) then
        read(sfid,*,iostat=iflag)isfid
        if (iflag /= 0) isfid=9999
        asoshr1=.false.
        call check_asos(isfid,sdate,0,.true.,.false.,.false.,lfound,asoshr1)
        if (lfound) then
            if (sfformats(7)%lsfform) then
!               issue warning if ASOS flag set and station was in list
!               otherwise set lasos to true
                if (lasos) then
                    write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W53',modnam,'STATION',trim(adjustl(sfid)),&
                        'FOUND IN ASOS LIST FOR ISHD DATA FORMAT'
                else
                    lasos=.true.
                endif
            else
!               if not ISHD data and not EXTRACT data, issue warning that station with non-ISHD data
!               found in ASOS list and issue warning if ASOS flag was set
                if (lasos) then
    f1:             do i=1,nsfformats
                        if (sfformats(i)%lsfform .and. i /= 8)then
                            write(msg_unit,formstr(4))adjustl(pathid(ipath)),'W55',modnam,'STATION',trim(adjustl(sfid)),&
                                ' FOUND IN ASOS LIST FOR NON-ISHD DATA FORMAT:',trim(adjustl(sfformats(i)%sformat))
                            if (lasos) then
                                write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W52',modnam,&
                                    'ASOS FLAG SET FOR NON-ISHD DATA FORMAT:',trim(adjustl(sfformats(i)%sformat)),&
                                    'RESET ASOS FLAG TO NON-ASOS'
                                lasos=.false.
                            endif
                        endif
                    enddo f1
                endif
            endif
!           notify user of ASOS commission date
            write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I59',modnam,'STATION',trim(adjustl(sfid)),' ASOS COMMISSION DATE:',&
                asoscommdates(iasos)%commisdate
        else
!       issue warning if ASOS flag set and not in list or a different
!       warning if ASOS flag not set
            if (lasos) then
                write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W54',modnam,'ASOS FLAG SET FOR STATION',trim(adjustl(sfid)),&
                    'NOT LISTED IN ASOS LIST'
            else
                write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W54',modnam,'STATION',trim(adjustl(sfid)),&
                    'NOT LISTED IN ASOS LIST; CHECK STATION ID'
            endif
        endif
    endif
   
    return
    end subroutine sf_test
!*********************************************************************************************************

    subroutine sf_proc
!=========================================================================================================
!   SUBROUTINE SF_PROC
!   THIS SUBROUTINE CONTROLS THE READING AND QA OF SURFACE DATA FOR STAGE 1
!
!   MODIFIED DECEMBER 6, 2021
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
!   ivar1:          calculated audit variable counter (UASS, UADS, UALR, UADD)
!   ivar:           variable counter (osvars)
!   i:              looping variable
!   sfc_date:       temporary array of surface dates (MMDDYYY) in LST
!                   this array is used to fill in the dates for
!                   sfcdate in the derived data type sfc_info
!   iday1:          counter of number of days in data period for arrays
!   extract_msg_start:  last message code before extraction totals messages; used for looping
!
!   Logical variables
!   leap:           variable denoting if year is a leap year
!
!   Character variables
!   cdate:          date output from data_and_time function
!   formstr:		formats for messages
!   extract_code:   code used for reporting sounding extraction info (# of observations, duplicates, etc.)
!   extract_msgs:   extraction messages used in conjunction with extract_codes
!   modnam:         Subroutine name
!=========================================================================================================   
    use main1, only: days,leapyr,numdays,istage
    use file_units, only: sf_qaout_unit,sf_extract_unit
    implicit none
    integer(kind=4) :: d,h,idattim(8),y,m,imon(2),idy(2),ivar1,i,ivar,iday1
    integer(kind=8), allocatable, dimension(:) :: sfc_date
    integer(kind=4) :: extract_msg_start=45
    logical :: leap
    character(len=8) :: cdate
    character(len=1) :: asosflag
    character(len=10) :: modnam='SF_PROC'
    character(len=60) :: formstr(5)
    character(len=100) :: data_form
    character(len=3) :: extract_code
    character(len=50) :: extract_msgs(12)
      
    extract_msgs(1)='NUMBER OF EXTRACTED SURFACE OBSERVATIONS:'
    extract_msgs(2)='NUMBER OF RETAINED OBSERVATIONS:'
    extract_msgs(3)='NUMBER OF SPECIAL OBSERVATIONS USED:'
    extract_msgs(4)='NUMBER OF REGULAR OBSERVATIONS USED:'
    extract_msgs(5)='NUMER OF DUPLICATE OBSERVATIONS NOT USED:'
    extract_msgs(6)='NUMBER OF SPECIAL OBSERVATIONS NOT USED:'
    extract_msgs(7)='NUMBER OF REGULAR OBSERVATIONS NOT USED:'
    extract_msgs(8)='NUMBER OF OVERWRITTEN OBSERVATIONS:'
    extract_msgs(9)='NUMBER OF NON-SUPPORTED OBSERVATIONS:'
    extract_msgs(10)='NUMBER OF OBSERVATIONS OUTSIDE 30-MINUTE WINDOW:'
    extract_msgs(11)='NUMBER OF OBSERVATIONS FLAGGED AS CALM'
    extract_msgs(12)='NUMBER OF OBSERVATIONS FLAGGED AS VARIABLE'
    
!   1.  surface extraction start/stop
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a8,1x,2(i2.2,a),i2.2)'

!   2.  format for extraction messages
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5)'

!   3.  no data extracted
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a1,1x,i8,a)'
    
!    initialize
    d=0
    h=0
    y=0
    m=0
    imon=0
    idy=0
    ivar1=0
    lbad=.false.
    iday1=0

!   get the number of days in the data period
    nsfdays=numdays(sfstart,sfend)

!   set sf_audit_index and nsf_extra
    vv:  do ivar=1,nsfvars
        if (sfvars(ivar)%laudit) then
            nsf_audit_vars=nsf_audit_vars+1
            if (ivar > nsfvars_keep) nsf_extra=nsf_extra+1
        endif
    enddo vv
    
!   number of variables to keep for QAOUT
    nsfvars1=nsfvars_keep+nsf_extra
    
!   allocate the temporary arrays 
    allocate(sfdata1(nsfvars1,24,nsfdays))
    if (istage == 1 .and. sfstage1) allocate(qaout_vars(nsfvars1))
    allocate(sfc_date(nsfdays))
    allocate(asoshr(24,nsfdays))
    allocate(nomisshr(24,nsfdays))
    allocate(nsfc1(nsfdays))
    allocate(lsfcobs(nsfdays))
    

!   initialize temporary arrays; sfdata1 will be based
!   on the missing values from sfvar
    ivar1=0 !counter for calculated audited variables
    v1: do ivar=1,nsfvars
        if (ivar <= nsfvars_keep) then !one of core variables
            sfdata1(ivar,:,:)=real(sfvars(ivar)%missval,r8)
            if(allocated(qaout_vars)) qaout_vars(ivar)=sfvars(ivar)%varname
        else !extra variable
            if (sfvars(ivar)%laudit) then !if audited then part of sfdata1
                ivar1=ivar1+1
                sfdata1(ivar1+nsfvars_keep,:,:)=real(sfvars(ivar)%missval,r8)
                if(allocated(qaout_vars)) qaout_vars(ivar1+nsfvars_keep)=sfvars(ivar)%varname
            endif
        endif
    enddo v1
    
!   initialize sfc_date to have all dates within the data period
!   even if a day may not have observations

    y1: do y=sfdates(1,1),sfdates(2,1)
!       check to see if year is leap year
        call leapyr(y,leap)
!       set initial values of imonth(1) and imonth(2) to
!       1 and 12
        imon(1)=1
        imon(2)=12
!       reset imonth(1) and imonth(2) to first and last
!       month if first or last year of data period
        if (y == sfdates(1,1)) imon(1)=sfdates(1,2)
        if (y == sfdates(2,1)) imon(2)=sfdates(2,2)
          
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
            if (m == imon(1) .and. y == sfdates(1,1)) idy(1)=sfdates(1,3)
            if (m == imon(2) .and. y == sfdates(2,1))idy(2)=sfdates(2,3)
    d1:     do d=idy(1),idy(2)
                iday1=iday1+1
                sfc_date(iday1)=y*10000+m*100+d
            enddo d1
        enddo m1         
    enddo y1
      
    nsfc1=0
    lsfcobs=.false.
    asoshr=.false.
    nomisshr=.false.  !if hour is not missing nomisshr will be true, if missing, then false
    
!   read the input data
!   get time time before extraction
    call date_and_time(date=cdate,values=idattim)

    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I40',modnam,'SURFACE EXTRACTION BEGIN:',cdate,idattim(5),':',idattim(6),':',&
        idattim(7)
    
    if (sfstage1) then
        if (sfformats(1)%lsfform .or. sfformats(9)%lsfform) then !CD144
             call read_cd144
        elseif (sfformats(2)%lsfform) then !SCRAM
            call read_scram
        elseif (sfformats(3)%lsfform) then !SAMSON
            call read_sam
        elseif (sfformats(6)%lsfform) then !HUSWO
            call read_huswo
        elseif (sfformats(7)%lsfform) then !ISHD
            call read_ishd
        else
            isfform=8 !needed for new_obs
            call read_ext  
        endif
    else
        isfform=8
        call read_ext  
    endif
    
!   reset sfelev to the user-entered elevation
    if (user_elev .and. istage== 1 .and. sfstage1)sfelev=sf_user_elev
    
!   for stage 1 write number of:
!   observations in data window, all below are a subset of this number
!   retained observations (obs kept)
!   duplicate obs
!   special obs used
!   special obs not used
!   regular obs not used
!   non-supported obs
!   obs outside 30 minute window
    if (istage == 1 .and. sfstage1) then
    l1: do i=1,12 !10
            write(extract_code,'(a1,i2.2)')'I',i+extract_msg_start
!			if not reading ISHD data, don't write certain messages
            if (.not. sfformats(7)%lsfform .and. (i == 3 .or. i == 4 .or. i == 6 .or. i == 7 .or. i == 9 .or. i == 10 .or. &
                i == 12)) cycle l1
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),extract_code,modnam,trim(adjustl(extract_msgs(i))),nsfc(i)
        enddo l1
    else
    l2: do i=1,2
            write(extract_code,'(a1,i2.2)')'I',i+45   
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),extract_code,modnam,trim(adjustl(extract_msgs(i))),nsfc(i)
        enddo l2
    endif
    
!   get time time after extraction
    call date_and_time(date=cdate,values=idattim)
    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I40',modnam,'SURFACE EXTRACTION END:',cdate,idattim(5),':',idattim(6),':',&
        idattim(7)

!   if no errors, proceed with auditing (if applicable) and filling in sfdata array
    if (.not. lbadsfc) then
!       if no valid data, alert user, may be outside XDATES
        if (nsfc(2) == 0) then
        !if (nsfc(4) == 0) then
            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W46',modnam,'NO EXTRACTED DATA BETWEEN',sfstart,'-',sfend,&
                '; CHECK DATES'
        else
            allocate(sfc_info(nsfdays))
!           fill in arrays
    d2:     do d=1,nsfdays
                sfc_info(d)%sfcdate=sfc_date(d)
                sfc_info(d)%nsfcobs=24
                sfc_info(d)%sobs=lsfcobs(d)
    h1:         do h=1,24
                    sfc_info(d)%asos_hr(h)=asoshr(h,d) !asoshr(d,h)
                    sfc_info(d)%have_obs(h)=nomisshr(h,d) !nomisshr(d,h)
                enddo h1
            enddo d2
 
            if (istage == 1 .and. sfstage1) then
                
!               write non-QA'd data to the EXTRACT file if requested
                if ((lsfkeywords(3) .and. writeext)) then
                    call header(sf_extract_unit)
!                   data format
                    write(data_form,'(a)')'(i8.8,3x,i2.2,5x,a1,1x,f8.2,10(1x,f8.1))'
    d3:             do d=1,nsfdays
                        if (.not. sfc_info(d)%sobs) cycle d3
    h2:                 do h=1,24
                            if (sfc_info(d)%asos_hr(h)) then 
                                asosflag='Y'
                            else
                                asosflag='N'
                            endif
    !                       only write if the hour is not missing
                            if (nomisshr(h,d))write(sf_extract_unit,data_form)sfc_info(d)%sfcdate,h,asosflag,(sfdata1(ivar,h,d),&
                                ivar=1,nsfvars_keep)
                        enddo h2
                    enddo d3
            
                endif
       
!               QA
                call sf_audit
            endif

!           allocate sfdata array
            allocate(sfdata(nsfvars_keep,24,nsfdays))
     

!           fill in data array
    d4:     do d=1,nsfdays
    h3:         do h=1,24
    v2:             do ivar=1,nsfvars_keep
                        sfdata(ivar,h,d)=sfdata1(ivar,h,d)
                    enddo v2
                enddo h3
            enddo d4
    
!           write QA'd output to the QAOUT files if requested
            if (istage == 1 .and. sfstage1) then
                if (lsfkeywords(4) .and. sfstage1) then
                    call header(sf_qaout_unit)  
!                   data format
                    write(data_form,'(a,i2,a)')'(i8.8,3x,i2.2,5x,a1,1x,f8.2,',nsfvars1-1,'(1x,f8.1))'
    d5:             do d=1,nsfdays
                        if (.not. sfc_info(d)%sobs) cycle d5
    h4:                 do h=1,24
                            if (sfc_info(d)%asos_hr(h)) then 
                                asosflag='Y'
                            else
                                asosflag='N'
                            endif
    !                       only write if the hour is not missing
                            if (nomisshr(h,d))write(sf_qaout_unit,data_form)sfc_info(d)%sfcdate,h,asosflag,&
                                (sfdata1(ivar,h,d),ivar=1,nsfvars1)
                        enddo h4
                    enddo d5
                endif
            endif
        endif
    endif
    
!   deallocate temporary arrays
    if (allocated(sfdata1)) deallocate(sfdata1)
    if (allocated(qaout_vars)) deallocate(qaout_vars)
    if (allocated(sfc_date)) deallocate(sfc_date)
    if (allocated(nsfc1)) deallocate(nsfc1)
    if (allocated(asoshr)) deallocate(asoshr)
    if (allocated(lsfcobs)) deallocate(lsfcobs)
    if (allocated(nomisshr)) deallocate(nomisshr)
    if (allocated(missvals)) deallocate(missvals)
    
    return
    end subroutine sf_proc
!*********************************************************************************************************

    subroutine header(iunit)
!=========================================================================================================
!   SUBROUTINE SF_PROC
!   THIS SUBROUTINE WRITES THE HEADER FOR THE EXTRACT AND QAOUT FILES
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (SF_PROC)
!
!   INPUT ARGUMENTS
!
!   IUNIT:          INPUT FILE UNIT
!
!   Variable definitions
!      
!   Integer variables
!   iunit:          input file unit
!   ivar:           variable counter
!
!   Character variables
!   bndstr:         bounds text string (< or <=)
!   celev:          character string of user entered elevation (sfelev)
!   alat:           character string of latitude with N or S descriptor
!   alon:           character string of longitude with E or W descriptor
!   cgmt2lst:       character string of time adjustment (sfgmt2lst)
!   outtype:        character string denonting output file type for
!                   EXTRACT or QAOUT file header
!   formstr:        format for writing messages
!   header_form:	header format
!   modnam:         Subroutine name
!=========================================================================================================   
    use main1, only: versn
    use file_units, only: sf_extract_unit
    implicit none
    integer(kind=4), intent(in) :: iunit
    integer(kind=4) :: ivar
    character(len=2) :: bndstr
    character(len=7) :: outtype(2)
    character(len=10) :: alat,alon
    character(len=10) :: celev=''
    character(len=3) :: cgmt2lst=''
    character(len=60) :: formstr
    character(len=100) :: header_form
    character(len=10) :: modnam='SF_PROC'
        
    data outtype /'EXTRACT','QAOUT'/
    
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,a4,i6,1x,a2,1x,2(1x,i6))'
    
!   set values for cgmt2lst and celev
    if (lgmt2lst) write(cgmt2lst,'(i3)')sfgmt2lst
    if (user_elev) write(celev,'(f10.3)')sfelev
            
!   set values for alat/alon
    if (sflat < 0.0_r8) then
        write(alat,'(f7.3,a1)')dabs(sflat),'S'
    else
        write(alat,'(f7.3,a1)')sflat,'N'
    endif
    if (sflon < 0.0_r8) then
        write(alon,'(f8.3,a1)')dabs(sflon),'W'
    else
        write(alon,'(f8.3,a1)')sflon,'E'
    endif
    
!   write AERMET version, LOCATION, filetype, and dates
    write(header_form,'(a)')'(a,a5/a,a8,2(1x,a10),2(1x,a)/2(a)/a,i4,2(1x,i2.2),1x,i4,2(1x,i2.2))'
    write(iunit,header_form)'AERMET ',versn,'LOCATION ',sfid,alat,alon,trim(adjustl(cgmt2lst)),&
    trim(adjustl(celev)),'FILE TYPE: ',trim(adjustl(outtype(iunit-sf_extract_unit+1))),'DATES ',sfdates(1,1),sfdates(1,2),&
    sfdates(1,3),sfdates(2,1),sfdates(2,2),sfdates(2,3)

    
!   now write any variables who have modified ranges or missing value
    v1: do ivar=1,nsfvars
        if (sfvars(ivar)%lmod) then
             if (sfvars(ivar)%lincbound) then
                bndstr='<='
            else
                bndstr='<'
            endif
            !write(iunit,formstr)'RANGE',sfvars(ivar)%varname,sfvars(ivar)%lowbound,bndstr,sfvars(ivar)%upbound,sfvars(ivar)%missval
            !post 21DRF
            write(iunit,'(a5,1x,a4,i6,1x,a2,1x,2(1x,i6))')'RANGE',sfvars(ivar)%varname,sfvars(ivar)%lowbound,bndstr,&
                sfvars(ivar)%upbound,sfvars(ivar)%missval
        endif
    enddo v1
    
!   write the remainder of the header, the data variable line which is dependent on file type
    if (iunit==sf_extract_unit) then
!       core variables
        write(header_form,'(a,i2,a)')'(a,4x,a4,',nsfvars_keep-1,'(5x,a4))'
        write(iunit,header_form)'DATE       HR  ASOS',(trim(adjustl(sfvars(ivar)%varname)),ivar=1,nsfvars_keep)
    else
!       core variables plus others that are audited
        write(header_form,'(a,i2,a)')'(a,4x,a4,',nsfvars1-1,'(5x,a4))'
        write(iunit,header_form)'DATE       HR  ASOS',(trim(adjustl(qaout_vars(ivar))),ivar=1,nsfvars1)
    endif
    
    return
    end subroutine header
!*********************************************************************************************************

    subroutine read_ishd
!=========================================================================================================
!   SUBROUTINE READ_ISHD
!   THIS SUBROUTINE READS ISHD FORMAT NWS DATA
!
!   MODIFIED DECEMBER 27, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!       
!   CALLED BY:      MODULE SURFACE (SF_PROC)
!
!   Integer variables
!   eof:            integer end of file indicator to read data file
!   lendat:         length of variable data
!   icontrol:       integer value to specify which value of control_len to use
!                   based on if data is from CDROM or not. icontrol =1 if not
!                   from CDROM, and icontrol=2 if from CDROM.
!   control_len:    length of CONTROL section, depending on if data is from CDROM
!                   icontrol determines which value to use.
!   rpt_indx:       position in CONTROL section to check for report type; value
!                   based lcdrom.  icontrol determines which value to use.
!   call_indx:      location in control data variable to look for the 3-character
!                   and 4-character call signs.
!   ir:             report type counter
!   iflag1:         I/O flag for reading USAF and WBAN numbers
!   endadd:         value for end of reading additional data for different fields
!   length:         18-element array of length of fields array elements
!   ifield:         field loop counter
!   fldindx:        array for start and stop location to read a field
!   asky:           sky condition
!   w1:             first weather indicator
!   mw:             2-D array of weather indicators
!   ifield1:        field counter
!   ifile:          file unit number of discard files
!   ishobs:         variables read from ISHD file
!   missobs:        missing value indicators for ISHD variables
!   inc:            number of minutes to subtract from 60 minutes to determine
!                   valid observations (30 minutes before hour)
!   savewban:       WBAN from first occurrence in file
!   iwarn:          number of warnings that WBAN doesn't match ID from input file
!   iday1:          counter of number of days in data period for arrays
!   iflag:          I/O indicator when reading linetype
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!   ihr:            hour of day
!   imin:           minute of the hour
!   savehr:         previous observation's hour
!   sdate:          integer observation date (YYYYMMDD)
!   savedate:       previous observation's date (YYYYMMDD)
!
!   Real variables
!   lat:            latitude read from file
!   lon:            longitude from file
!   elev:           elevation read from file
!   p:              calculated pressure
!
!   Logical variables
!   lfound:         logical variable denoting certain value found
!   gotwban:        have valid WBAN
!   lcdrom:         data is from CDROM (no lat/lon); affects control section length
!   lgo:            logical variable denoting if date of record within data window
!   lgo2:           observation is within 30 minute of the hour or on the hour
!   lgo3:           record type is supported record type
!   calcrh:         logical variable denoting to calculate RH (true) or set to missing (false)
!   spec_obs:       current record is special obs (true) or not (false)
!   reg_obs:        at least one observation for hour is regular obs (true) or not (false)
!   have_elev:      elevation available in data file
!   asoshr1:        dummy logical variable used in subroutine check_asos
!   miss_wind:      denotes if speed is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_dir:      denotes if direction is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_temp:      denotes if temperature is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_precip:    denotes if precip is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   writedat:       denotes to write data to sfdata1
!
!   Character variables
!   date_hr_str:    date and hour text string for error messages
!   formstr:        format for messages unique to read_ishd
!   datline:        data line read from ISHD file
!   control:        CONTROL portion of data line
!   mandatory:      MANDATORY portion of data line
!   additional:     ADDITIONAL portion of data line determined by mandatory and variable
!   variable:       variable portion of data line
!   rpt_type:       report type of observation
!   last_rpt:       last report type read
!   rpt_use:        report type used for sfdata1
!   rpt_list:       list of valid report types 
!   form1:          format for reading CONTROL, MANDATORY, and VARIABLE data from dataline
!   iusaf:          USAF identifier read from current record
!   wind_code:      wind code used to determine if wind is variable or calm
!   call_sign3:     3-character call sign
!   call_sign4:     4-character call sign
!   fld:            field indicator for additional fields
!   field:          initial values of additional fields
!   modnam:         Subroutine name
!  
!   mapping of ishobs to sfdata1
!   ishobs(1)   = sfdata1(1,:,:)    precipitation
!   ishobs(2)   = sfdata1(2,:,:)    sea level pressure; ishobs is pressure*10
!   ishobs(3)   = sfdata1(3,:,:)    station pressure; ishobs is pressure*10
!   ishobs(4)   = sfdata1(12,:,:)   ceiling height
!   ishobs(5)   = sfdata1(5,:,:)    precipitation type
!   ishobs(6)   = sfdata1(6,:,:)    ASOS sky condition
!   ishobs(7)   = sfdata1(7,:,:)    dry bulb temperature; ishobs is temperature*10
!   ishobs(8)   = sfdata1(8,:,:)    dew point temperature; ishobs is dewpoint*10
!   ishobs(9)   = sfdata1(9,:,:)    relative humidity
!   ishobs(10)  = sfdata1(10,:,:)   wind direction 
!   ishobs(11)  = sfdata1(11,:,:)   wind speed; ishobs is speed*10
!   ishobs(14)  = sfdata1(21,:,:)   horizontal visibility
!   ishobs(15)  = sfdata1(20,:,:)   ASOS ceiling
!
!   ishobs(12)+ishobs(13)  = sfdata1(4,:,:) total/opaque sky cover
!   ishobs(16) - ishobs(27) used to calculate ishobs(6)
!   ishobs(28)  = altimeter; used to calculate sea level pressure and/or station pressure
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit
    use main1, only: data_dates,humidity,pressure,debug
    use file_units, only: ishd_units,ishd_files,sf_data_unit
    implicit none
    integer(kind=4) :: eof,lendat,icontrol,control_len(2),rpt_indx(2),call_indx(2,2),ir,iflag1,iday1,iyear,imonth,iday,&
        endadd,length(18),ifield,fldindx(2),asky,w1,mw(2),ifield1,ifile,savewban,iwarn,iflag,ihr,imin,savehr
    integer(kind=4) :: ishobs(28),missobs(28)
    integer(kind=4) :: inc=30
    integer(kind=8) :: savedate=0
    integer(kind=8) :: sdate
    real(kind=r8) :: lat,lon,elev,p

    logical lfound,lcdrom,lgo,lgo2,lgo3,calcrh,spec_obs,reg_obs,gotwban,have_elev,asoshr1,miss_wind,miss_dir,miss_temp,&
        miss_precip,writedat
    character(len=1) :: data_source_flag
    character(len=5) :: rpt_type,last_rpt,rpt_use,rpt_list(17)
    character(len=20) :: form1
    character(len=60) :: control,control1
    character(len=8) :: iusaf 
    character(len=2) :: wind_code
    character(len=3) :: call_sign3,fld(18)
    character(len=15) :: field(18)
    character(len=4) :: call_sign4
    character(len=45) :: mandatory
    character(len=1000) :: additional,variable
    character(len=10200) :: datline,lastline
    character(len=20) :: date_hr_str
    character(len=60) :: formstr(6)
    character(len=10) :: modnam='READ_ISHD'
    
    data rpt_list /'FM-12','FM-13','FM-14','FM-15','FM-16','FM-18','SAO','SAOSP','AERO','AUTO','SY-AE','SY-SA','SY-MT','SY-AU',&
        'SA-AU','S-S-A','SMARS'/
    data control_len /60,33/
    data rpt_indx/42,29/
    
!   the order of the arrays for ishobs and missobs correspond to order of the sfvars data type for
!   variables 1-3 and 5-11.  the exceptions are sfvars(4), TSKC is composed of ishobs(12) and ishobs(13).  
!   sfvars(21), HZVS is composed of ishobs(14).
    
    data missobs /9999,99999,99999,99999,9999,99,9999,9999,999,999,9999,99,99,999999,99,9,9,9,9,9,9,99,99,99,99,99,99,99999/
    
    data fld /'AA1','AG1','GD1','GD2','GD3','GD4','GD5','GD6','GF1','MA1','MW1','MW2','GA1','GA2','GA3','GA4','GA5','GA6'/ 
    data length /11,7,6*4,8,15,6,6,6*5/
    
!   formats for messages
!	1.	error reading variable data length
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a)'
    
!	2.	observation minute not with x-minutes of the hour
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'2(a,1x,i2,1x),a,1x,a20)'
    
!	3.	report type not processed
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a20)'
    
!	4.	elevation not in file, use user elevation
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,f9.2,1x,a1)'
    
!	5.	replace observation; ISHD is unique in that the report type is listed
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20,4(1x,a))'
    
!	6.	special obs
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20,2(1x,a))'
    
! call index;  
!               second dim 1=3-character call sign start, 2=4-character call sign start
    call_indx(1,1)=53
    call_indx(1,2)=52
    call_indx(2,1)=39
    call_indx(2,2)=34
    
    iwarn=0
    spec_obs=.false.
    reg_obs=.false.
    eof=0
    lgo2=.false.
    lgo3=.false.
    data_source_flag='9' !initialize to missing
    ishobs=0
    endadd=0
    asoshr1=.false.
    miss_wind=.false.
    miss_dir=.false.
    miss_temp=.false.
    miss_precip=.false.
    sdate=0
    gotwban=.false.
    savewban=0
    
!   mapping of ishobs to sfdata1
    
!   open the discard and replace files
    f1: do ifile=1,2
        open(unit=ishd_units(ifile),file=ishd_files(ifile),status='replace')
    enddo f1
!   initialize ishobs to missing
    f2: do ifield=1,28
        ishobs(ifield)=missobs(ifield)
    enddo f2
    
    do while (eof == 0 .and. .not. lbad .and. sdate <= sfend)
        read(sf_data_unit,'(a10200)',iostat=eof)datline
        lgo2=.false.
        lgo3=.false.
        have_elev=.false.
        lcdrom=.false. !initially set to false
        icontrol=1 !set to no CDROM for icontrol
        if (eof == 0) then
            read(datline,'(a60)')control
            
            spec_obs=.false. !initialize spec_obs for each record or observation
            ishobs=0 !reset ishobs
!           check to see if lat/lon are present, if so this means data not extracted from CDROM;
!           this affects the length of the CONTROL data section
            if (datline(29:29) /= '+' .and. datline(29:29) /= '-') then !no coordinates, from CDROM
                lcdrom=.true.
                icontrol=2
            endif
            
!           get length of variable data
            read(datline(1:4),*,iostat=iflag)lendat
            if (iflag /= 0) then
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E40',modnam,'ERROR READING VARIABLE DATA LENGTH'
                lbad=.true.
            else
                write(form1,'(a,i3,a,i5,a)')'(a',control_len(icontrol),',a45,a',lendat,')'
!               control1 is a dummy variable
                read(datline,form1)control1,mandatory,variable
            endif
            !nsfc(1)=nsfc(1)+1 !total number of obs read
            
!************** date ****************************************
            read(control(16:27),'(i4,4(i2))',iostat=iflag)iyear,imonth,iday,ihr,imin
            if (iflag == 0) then
!               first reset the hour if minute is > than 0, with no gmt2lst 
!               this will help to make sure to get the last hour of the day before sfstart
!               not concerned at this point if the minute fits the increment tested below.
!               use a value of -1 for gmt2lst conversion.  see subroutine data_dates for reason why using -1
                if (imin > 0) then
                    call data_dates(ihr,iday,imonth,iyear,-1,sfstart,sfend,42,sdate,lgo)
                else
                    call data_dates(ihr,iday,imonth,iyear,0,sfstart,sfend,42,sdate,lgo)
                endif
!               now check the date to see if within data window
                call new_obs(iyear,imonth,iday,ihr,savedate,savehr,lgo,iday1,sdate)
                write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
                
                if (lgo) then
                    
    !               check the minute to see if within acceptable bounds
                    if (imin == 0 .or. imin >= (60-inc)) then !obs on the hour accepted
                        lgo2=.true.
                    else
                        lgo2=.false.
                        write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I41',modnam,'OBSERVATION MINUTE',imin,'NOT WITHIN',inc,&
                            'MINUTES OF HOUR FOR',date_hr_str
                
                        nsfc(10)=nsfc(10)+1
                        write(ishd_units(1),'(a)')trim(adjustl(datline))  !write to discard file
                    endif
!                   if within the window and within 30 minutes of hour, proceed
                    if (lgo2) then
    !                   check the observation type to see if a supported format and if special or
    !                   regular observation type
                        read(control(rpt_indx(icontrol):rpt_indx(icontrol)+4),'(a5)')rpt_type
                        ir=1
                        lfound=.false.
                        do while (ir <= 17 .and. .not. lfound)
                            if (trim(adjustl(rpt_type)) == trim(adjustl(rpt_list(ir)))) then
                                
                                lfound=.true.
                            else
                                ir=ir+1
                            endif
                        enddo
                        if (lfound) then
                           lgo3=.true. 
                        else !not a type of obs to use and record to discard file
                            if (debug) write(msg_unit,formstr(3))adjustl(pathid(ipath)),'I42',modnam,'REPORT TYPE',&
                                trim(adjustl(rpt_type)),'NOT PROCESSED FOR',date_hr_str
                            !nsfc(10)=nsfc(10)+1
                            nsfc(9)=nsfc(9)+1
                            lgo3=.false.
                            write(ishd_units(1),'(a)')trim(adjustl(datline))
                        endif            
                    endif  
                endif
!               if within data window (lgo=true), and within 30 minutes of hour (lgo2=true) and
!               one of the accepted report types has been found (lfound=true), lgo3 has been set to
!               true and now proceed
!************** date ****************************************

!               start data processing if within date window and valid observation type
                if (lgo3) then
                    if (sdate == savedate) then
                        if (ihr /= savehr) then
    !                       initialize reg_obs to false since a new hour
                            reg_obs=.false.
    !                       reset rpt_use to blank
                            rpt_use=''
                        endif
                    else
                        reg_obs=.false.
                    endif
!                   check to see if a special obs
                    if (trim(adjustl(rpt_type)) == 'SAOSP' .or. trim(adjustl(rpt_type)) == 'FM-16') then
                        spec_obs=.true.
                    else
                        reg_obs=.true.
                    endif
               
!**************   station ID ****************************************
                    read(control(5:15),'(a6,i5)',iostat=iflag1)iusaf,iwban
                    if (iflag1==0) then
!                       check WBAN or USAF # against user entered value if file's ID not missing (99999)
!                       also keep the first occurrence for subsequent checks in the data

                        if (.not. gotwban .and. iwban /= 99999) then
                            savewban=iwban
                            gotwban=.true.
                            if (iwban /= isfid .and. iwban /= 99999) then !no match
                                if (trim(adjustl(iusaf)) == trim(adjustl(sfid))) then
                                    write(msg_unit,ext_form(4))adjustl(pathid(ipath)),'W41',modnam,'STATION ID',iusaf,&
                                        'IS WMO NUMBER; CONSIDER WBAN FOR STAGE 2'
                    
                                else
                                    write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'W40',modnam,'STATION ID',iwban,&
                                    'DOES NOT MATCH USER ENTERED ID',trim(adjustl(sfid))
                                    iwarn=iwarn+1
                                endif
                            endif
                        else !compare against saved WBAN or USAF from first record if not missing (99999)
                            if (iwban /= savewban .and. iwban /= 99999) then !no match
                                iwarn=iwarn+1
                                if (iwarn <= nwarnlim)write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'W40',modnam,&
                                    'STATION ID',iwban,'DOES NOT MATCH PREVIOUS WBAN',trim(adjustl(sfid))
                                if (iwarn == nwarnlim) write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'W42',modnam,&
                                    'REACH LIMIT OF',nwarnlim,'W40','WARNING MESSAGES'
                            endif
                        endif
                        call check_asos(iwban,sdate,ihr,.true.,.true.,.false.,lasos,asoshr(ihr,iday1))
                    else
                        write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E47',modnam,'INVALID STATION ID FIELD:',control(5:15)
                        lbad=.true.
                    endif
!**************   station ID ****************************************
                    
!**************   call sign **************************************** 
                   read(control(call_indx(icontrol,1):call_indx(icontrol,1)+2),*)call_sign3
                   read(control(call_indx(icontrol,2):call_indx(icontrol,2)+3),*)call_sign4
                   
!**************   call sign **************************************** 
                   
!**************  station coordinates *******************************
                    if (lcdrom) then  !no coordinates
                        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W44',modnam,&
                            'STATION COORDINATES NOT PROVIDED IN FILE; DATA FROM CDROM'
                    else
                        read(control(29:41),'(f6.3,f7.3)',iostat=iflag1)lat,lon
                        if (iflag1 /= 0) then
                            write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E44',modnam,'INVALID COORDINATES FIELD:',&
                                control(29:41)
                            lbad=.true.
                        endif
                    endif
!**************  station coordinates *******************************
                    
!**************  station elevation *******************************
                    if (lcdrom) then  !no elevation
                        if (user_elev) then
                            write(msg_unit,formstr(4))adjustl(pathid(ipath)),'W43',modnam,&
                                'STATION ELEVATION NOT PROVIDED IN FILE; DATA FROM CDROM; USE USER ELEVATION',sfelev,'M' !user supplied elevation
                            
                        else
                            write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'W43',modnam,&
                                'STATION ELEVATION NOT PROVIDED IN FILE; DATA FROM CDROM;',&
                                'NO USER ELEVATION,CONSIDER INPUTTING ELEVATION' !no user elevation, suggest adding elevation
                        endif
                    else
                        read(control(47:51),'(f5.0)',iostat=iflag1)elev
                        if (iflag1 == 0) then
                            if (elev == 9999.0_r8) then
!                               if missing, use user elevation if provided, otherwise suggest adding elevation
                                if (user_elev) then 
                                    write(msg_unit,formstr(4))adjustl(pathid(ipath)),'W43',modnam,&
                                    'STATION ELEVATION PROVIDED IN FILE BUT MISSING; USE USER ELEVATION',sfelev,'M' !user supplied elevation
                                else
                                    write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'W43',modnam,&
                                    'STATION ELEVATION PROVIDED IN FILE BUT MISSING; NO USER ELEVATION,',&
                                        'CONSIDER INPUTTING ELEVATION'!no user elevation, suggest adding elevation
                                endif
                            else !non-missing elevation
                                have_elev=.true.
                                if (user_elev) then  !check for consistency
                                    if (dabs(sfelev - elev) > 0.02_r8*elev) write(msg_unit,ext_form(9))adjustl(pathid(ipath)),&
                                        'W43',modnam,'USER ENTERED ELEVATION',sfelev,'M DIFFERS FROM ELEVATION',elev,&
                                        'M IN ISHD FILE'
                                endif
                                sfelev=elev
                                lsfelev=.true.
                            endif    
                        else
                            write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E43',modnam,'INVALID ELEVATION FIELD:',&
                                control(47:51)
                            lbad=.true.
                        endif
                    endif
!**************  station elevation *******************************
                    
!**************  check for ASOS status ******************************
!                   if have a WBAN or call sign and lasos is not true
!                   check the station again in the ASOS list using 
!                   the station 4-character and 3-character call sign or saved WBAN,
!                   only check when first record
!                   don't use check_asos since the call signs are used as well
     
                    if (.not. lasos) then
                        iasos=1
                        lfound=.false.
                        
                        do while (iasos <= nasos .and. .not. lasos)
                            if (savewban == asoscommdates(iasos)%iwban .or. trim(adjustl(call_sign4)) == &
                                trim(adjustl(asoscommdates(iasos)%intcall)) .or. trim(adjustl(call_sign3)) == &
                                trim(adjustl(asoscommdates(iasos)%usacall))) then
                                lasos=.true.
                            else
                                iasos=iasos+1
                            endif
                        enddo
                        if (lasos) then
                            write(msg_unit,ext_form(4))adjustl(pathid(ipath)),'W53',modnam,'STATION',&
                                trim(adjustl(sfid)),'FOUND IN ASOS LIST BASED ON STATION IDENTIFIERS IN SURFACE FILE'
                            asoshr(ihr,iday1)=.true.
                        endif
                    endif
                    read(control(28:28),'(a1)')data_source_flag
                    if (data_source_flag == '6' .or. data_source_flag == '7') then !possible ASOS obs
!                       if station is indicated to be ASOS but there is no commission date or observation
!                       is prior to commission date, then issue a warning and asoshr will be set to true
!                       for the hour.  If station is not ASOS, do not issue warning but set asos for the 
!                       hour to true; do not set lasos to true however, to avoid triggering one of the messages below.
                        if (lasos) then
                            if (iasos == 0) then !no commission date was found, warn user
                                write(msg_unit,ext_form(5))adjustl(pathid(ipath)),'W45',modnam,&
                                    'DATA SOURCE FLAG = ASOS, BUT NO COMMISSION DATE, SET OBS=ASOS FOR',date_hr_str
                            else
                                if (sdate < asoscommdates(iasos)%commisdate) write(msg_unit,ext_form(8))adjustl(pathid(ipath)),&
                                    'W45',modnam,'DATA SOURCE FLAG = ASOS, BUT BEFORE COMMISSION DATE',&
                                    asoscommdates(iasos)%commisdate,'SET OBS=ASOS FOR',date_hr_str
                            endif
                        endif
                        asoshr(ihr,iday1)=.true.
                    endif
!**************  check for ASOS status ******************************  

!**************  read actual data ******************************  
!                   have checked all the preliminaries (station info,date), now read the
!                   meteorological data
                    
!                   ***** MANDATORY DATA *******
                    
!                   read in order found in mandatory data section
                    read(mandatory(1:3),'(i3)')ishobs(10)   !wind direction
                    read(mandatory(4:5),'(a2)')wind_code    !wind direction quality code and wind obs type code 
                    read(mandatory(6:9),'(i4)')ishobs(11)   !wind speed
                    read(mandatory(11:15),'(i5)')ishobs(4)  !ceiling; will be mapped to sfdata1(12,:,:,:)
                    read(mandatory(19:24),'(i6)')ishobs(14) !visibility
                    read(mandatory(28:32),'(i5)')ishobs(7)  !dry-bulb
                    read(mandatory(34:38),'(i5)')ishobs(8)  !dew-point
                    read(mandatory(40:44),'(i5)')ishobs(2)  !sea level pressure
                    
!                   ***** MANDATORY DATA *******
                    
!                   ***** ADDITIONAL DATA *******
                    
!                   read from additional fields from variable data section
!                   look for REM, EQD, or QNN to see where to stop 
!                   reading.  default is lendat
                    if (index(variable,'REM') > 0) then
                        endadd=index(variable,'REM')-1
                    elseif (index(variable,'EQD') > 0) then
                        endadd=index(variable,'EQD')-1
                    elseif (index(variable,'QNN') > 0) then
                        endadd=index(variable,'QNN')-1
                    else
                        endadd=lendat
                    endif
                    
!                   only want useful part of the variable data
                    additional=variable(1:endadd)
                    
!                   initialize the fields to missing values
!                   Initialize the additional fields to missing

                    field(1)  = 'AA10100009'        ! Initialize 1-hour precipitation to none
                    field(2)  = 'AG19999'           ! Bogus precipitation/Wx indicator

                    field(3)  = 'GD19999+99999'     ! Sky cover summation level 1
                    field(4)  = 'GD29'              ! Sky cover summation level 2
                    field(5)  = 'GD39'              ! Sky cover summation level 3
                    field(6)  = 'GD49'              ! Sky cover summation level 4
                    field(7)  = 'GD59'              ! Sky cover summation level 5
                    field(8)  = 'GD69'              ! Sky cover summation level 6

                    field(9)  = 'GF199999'          ! Sky condition (total & opaque)
                    field(10) = 'MA1999999999999'   ! Atmospheric pressure

!                   Initialize present weather to none
                    field(11) = 'MW1001'
                    field(12) = 'MW1002'

!                   Add GA1-GA6 cloud layer codes as third tier, after GF1 and
!                   GD1-GD6.  GA1-GA6 codes may not be important for primary stations,
!                   but could increase data recovery for secondary stations      
                    field(13) = 'GA199'                                
                    field(14) = 'GA299'                               
                    field(15) = 'GA399'                               
                    field(16) = 'GA499'                             
                    field(17) = 'GA599'                              
                    field(18) = 'GA699'                               
                    
!                   get actual values of fields
!                   for precip, only want hourly precip, not 3-hour, 6-hour, etc.
                    if (index(additional,'AA101') > 0) then
                        fldindx(1)=index(additional,fld(1))
                        fldindx(2)=index(additional,fld(1))+length(1)-1
                        field(1)=additional(fldindx(1):fldindx(2))
                    endif
                    
!                   skip field(2)
    f3:             do ifield=3,18
                        if (index(additional,fld(ifield)) > 0) then
                            fldindx(1)=index(additional,fld(ifield))
                            fldindx(2)=index(additional,fld(ifield))+length(ifield)-1
                            field(ifield)=additional(fldindx(1):fldindx(2))
                        endif
                    enddo f3
                    
!                   precipitation
                    read(field(1)(6:9),'(i4)')ishobs(1) 
                    
!                   GD codes
    f4:             do ifield=3,8
                        read(field(ifield)(4:4),'(i1)')ishobs(ifield+13)
                    enddo f4
!                   GA codes; may not be needed but fill in array
    f5:             do ifield=13,18
                        read(field(ifield)(4:5),'(i2)')ishobs(ifield+9)
                    enddo f5
                   
!                   total cloud and opaque cloud
                    read(field(9)(4:5),'(i2)')ishobs(12)
                    read(field(9)(6:7),'(i2)')ishobs(13)
                  
!                   altimeter and station pressure
                    read(field(10)(4:8),'(i5)')ishobs(28) !altimeter
                    read(field(10)(10:14),'(i5)')ishobs(3) !station pressure
!                   
!                   ***** ADDITIONAL DATA *******
                    
                    

!**************  read actual data ******************************  
                    
!**************  data processing ******************************

!                   total sky cover ishobs(12) and opaque sky cover ishobs(13)
    f6:             do ifield=12,13
                        if (ishobs(ifield) == missobs(ifield)) then
                            ishobs(ifield)=99
                            
                        else
                            if (ishobs(ifield) > 1) then
                                if (ishobs(ifield) <= 5) then ! 2 to 5 oktas, add 1; if 1 then no change
                                    ishobs(ifield)=ishobs(ifield)+1
                                   
                                elseif (ishobs(ifield) <= 8) then ! 6 to 8 oktas, add 2
                                    ishobs(ifield)=ishobs(ifield)+2
                                    
                                elseif (ishobs(ifield) <=10) then ! Obscured, assign 10/10
                                    ishobs(ifield)=10
                                   
                                else
                                    ishobs(ifield)=99
                                    
                                endif
                            endif
                        endif
                    enddo f6
                  
!                   total sky cover ishobs(12) and opaque sky cover ishobs(13)      
                    
!                   ASOS cloud layers ishobs(6)
                   
                    if (ishobs(12) == missobs(12) .and. ishobs(13) == missobs(13)) then
!                       both total and opaque sky cover are missing. process ASOS cloud layers
!                       start with GD1-GD6 (ishobs(16)-ishobs(21)) then GA1-GA6 (ishobs(22)-ishobs(27))
!                       start with level 6 (highest level) and work downward
                        asky=missobs(6)
                        ifield=21                 
                        do while (asky == missobs(6) .and. ifield >= 16)
                            if (ishobs(ifield) /= missobs(ifield))asky=ishobs(ifield)
                            ifield=ifield-1
                        enddo
                    
                        if (asky /= missobs(6)) then
                           
                            if (asky >= 4) then !overcast or obscured
                                ishobs(6)=10
                            elseif (asky == 3) then !broken (5/8 - 7/8)
                                ishobs(6)=7
                            elseif (asky == 2) then !scattered (3/8 or 4/8)
                                ishobs(6)=4
                            elseif (asky == 1) then !few (1/8 or 2/8)
                                ishobs(6)=2
                            else                    !clear
                                ishobs(6)=0
                            endif
                        else
!                           use GF1 and GD1-6 are missing; use GA1-GA6 (ishobs(22)-ishobs(27))
!                           start with level 6 and work downward, using max value of asky and ishobs
                            if (ishobs(27) /= missobs(27)) then
                                asky=ishobs(27)
                            endif
                            
!                           now start downward
    f7:                     do ifield=26,22,-1
                                if (ishobs(ifield) /= missobs(ifield)) then
                                    if (asky /= missobs(6)) then
                                        asky=max(asky,ishobs(ifield))
                                    else
                                        asky=ishobs(ifield)
                                    endif
                                endif
                            enddo f7
                            
!                           go from oktas to tenths
                            if (asky <= 1) then ! 0 to 1 oktas, round to tenths
                                ishobs(6)=asky
                            elseif (asky <= 5) then ! 2 to 5 oktas, add 1
                                ishobs(6)=asky+1
                            elseif (asky <= 8) then ! 6 to 8 oktas, add 2
                                ishobs(6)=asky+2
                            elseif (asky <= 10) then ! Obscured, assign 10/10
                                ishobs(6)=10
                            else
                                ishobs(6)=99
                            endif
                        endif
                    else
                        ishobs(6)=sfvars(6)%missval  !set to missing value for sfvars
                    endif
!                   ASOS cloud layers ishobs(6)                   
 
!                   dry bulb  and dewpoint temperature
!                   set calcrh to true initially but if temp or dewpoint is missing
!                   set to false.
                    calcrh=.true.
                    
    f8:             do ifield=7,8
                        if (ishobs(ifield) == missobs(ifield)) then
                            ishobs(ifield)=sfvars(ifield)%missval
                            calcrh=.false.
                        endif
                    enddo f8
                    
!                   dry bulb  and dewpoint temperature
                    
!                   relative humidity
                    if (calcrh) then
                        ishobs(9)=humidity(real(ishobs(7),r8)/10.0_r8,real(ishobs(8),r8)/10.0_r8)
                    else
                        ishobs(9)=sfvars(9)%missval
                    endif
!                   relative humidity

!                   wind direction
                    if (ishobs(10) == missobs(10)) then !missing wind direction
                        if ((wind_code=='59' .and. ishobs(11) == 0) .or. wind_code(2:2) == 'C') then !valid calm
                            ishobs(10)=0
                            nsfc(11)=nsfc(11)+1
                        elseif (wind_code(2:2) == 'V' .or. (ishobs(10) == missobs(10) .and. ishobs(11) /= missobs(11))) then
                            ishobs(10)=sfvars(10)%missval
                            nsfc(12)=nsfc(12)+1
                        endif
                    endif
!                   wind direction                    
                    
!                   wind speed
                    if (ishobs(11) == missobs(11))ishobs(11)=sfvars(11)%missval
!                   wind speed

!                   ceiling height
                  
                    if (ishobs(4) == missobs(4)) then 
                        ishobs(4)=sfvars(12)%missval !missing
                      
                    elseif (ishobs(4) == 22000) then
                        ishobs(4)=300 !unlimited
                       
                    else
                        ishobs(4)=int(real(ishobs(4),r8)/100._r8) !convert to km and multiply by 10
                        
                    endif
                    
!                   ceiling height 
                    
!                   visibility
                    if (ishobs(14)==missobs(14)) then
                        ishobs(14)=sfvars(21)%missval
                    else
                        ishobs(14)= (ishobs(14)/1000)*10
                    endif
!                   visibility         
                    
                  
 
!                   precip ishobs (1) and precip code ishobs(5)
!                   get weather codes and evaluate
    f9:             do ifield=11,12
                        read(field(ifield)(4:5),'(i2)')w1
                        if (w1 <= 19) then
                            mw(ifield-10)=0
                        elseif (w1 == 22 .or. (w1 >= 36 .and. w1 <= 39) .or. (w1 >= 70 .and. w1 <= 79) .or. w1 == 85 .or. &
                        w1 == 86) then
                            mw(ifield-10)=2
                        elseif (w1 == 21 .or. 21 == 25) then
                            mw(ifield-10)=1
                        elseif (w1 <= 28) then
                            mw(ifield-10)=3
                        elseif (w1 <= 49) then
                            mw(ifield-10)=0
                        elseif (w1 == 56 .or. w1 == 57 .or. (w1 >= 66 .and. w1 <= 69) .or. (w1 >= 83 .and. w1 <= 90) .or. &
                        w1 >= 93) then
                            mw(ifield-10)=3
                        else
                            mw(ifield-10)=1
                        endif    
                    enddo f9
!                   get weather
                    if ((mw(1) == 0 .and. mw(2) == 0) .or. (mw(1) == 1 .and. mw(2) == 1) .or. (mw(1) == 2 .and. mw(2) == 2)) then
                        ishobs(5)=mw(1)
                    elseif ((mw(1) == 1 .and. mw(2) == 2) .or. (mw(1) == 2 .and. mw(2) == 1)) then
                        ishobs(5)=3
                    else
                        ishobs(5)=max(mw(1),mw(2))
                    endif
                    
                    if (ishobs(1) == missobs(1)) then
                        ishobs(1)=sfvars(1)%missval
                        ishobs(5)=0
                    endif
                    if (ishobs(1)==0)ishobs(5)=0
!                   precip ishobs (1) and precip code ishobs(5)

!                   altimeter (used to calculate station pressure)
                    read(field(10)(4:8),'(i5)')ishobs(28)
                    
!                   station pressure
                    read(field(10)(10:14),'(i5)')ishobs(3)
 
!                   if station pressure is missing perform the following:
!                   1) if altimeter is not missing but sea level pressure is missing, use altimeter  
!                      and elevation from ISHD file to calculate station pressure
!                      and then use the calculated station pressure to calculate sea level pressure
!                      with elevation.
!                   2) if the altimeter is missing, and sea level pressure is missing, use
!                      elevation and standard atmosphere to calculate station pressure
!                   3) if sea level pressure available then use that and elevation to 
!                      calculate station pressure
!                   if station pressure not missing but sea level pressure is missing
!                   use station pressure and elevation to calculate sea level pressure.
                    if (ishobs(3) == missobs(3)) then
                        if (ishobs(28) /= missobs(28) .and. ishobs(2) == missobs(2)) then
!                           calculate station pressure from altimeter and virtual temperature
!                           of 290K
                            p=pressure(2,sfelev,290._r8,real(ishobs(28),r8)/10._r8)
                            ishobs(3)=nint(p)*10
!                           now calculate sea level pressure from calculated station pressure
                        endif
                        if (ishobs(28) == missobs(28) .and. ishobs(2) == missobs(2)) then
                            if (have_elev .and. .not. user_elev) then
!                               calculate station pressure from elevation in data file and standard atmosphere
                                ishobs(3)=nint(1013.25_r8*(1.0_r8-((6.5e-3_r8/288.15_r8)*sfelev))**5.255_r8)*10 
                            else
                                ishobs(3)=sfvars(3)%missval
                            endif
                            ishobs(2)=sfvars(2)%missval
                        endif
                        if (ishobs(2) /=missobs(2)) then
                            if (have_elev .and. .not. user_elev) then
!                               calculate station pressure from elevation and sea level pressure
                                p=pressure(2,sfelev,290._r8,real(ishobs(2),r8)/10._r8)
                                ishobs(3)=nint(p)*10
                            else
                                ishobs(3)=sfvars(3)%missval
                            endif
                        endif
                    else
                        if (ishobs(2) == missobs(2)) then
                            if (have_elev .and. .not. user_elev) then
!                               calculate sea level pressure from elevation and station pressure
                                p=pressure(1,sfelev,290._r8,real(ishobs(3),r8)/10._r8)
                                ishobs(2)=nint(p)*10
                            else
                                ishobs(2)=sfvars(2)%missval
                            endif
                        endif
                    endif
                    
!                   reset missing values to sfvars missing value for pressures
    f10:            do ifield=2,3
                        if (ishobs(ifield) == missobs(ifield))ishobs(ifield)=sfvars(ifield)%missval
                    enddo f10

!                   station pressure
!**************  data processing ******************************

!**************  write to the surface array *******************
! check to see if there are multiple obs for the hour. decided whether to 
! overwrite an existing obs with the latest one.  want to keep the lastest
! non-special obs up to and including the hour (minute=0). only use special
! obs if there are no regular observations available for the hour.

                    if (nomisshr(ihr,iday1)) then
                        call check_dup(iday1,ihr,real(ishobs(1),r8),real(ishobs(7),r8),real(ishobs(10),r8),real(ishobs(11),r8),&
                            miss_precip,miss_temp,miss_dir,miss_wind,writedat)
                        if (spec_obs .and. reg_obs)  then
                           
                            nsfc(6)=nsfc(6)+1  !don't use obs but increment counter for special obs
                            
                            writedat=.false.
                            write(ishd_units(1),'(a)')trim(adjustl(datline))
                        elseif (reg_obs .and. .not. writedat) then !do not overwrite
!                          
                            nsfc(7)=nsfc(7)+1  !increment counter for non-used regular obs
!                        
                            write(ishd_units(1),'(a)')trim(adjustl(datline))
                        else !overwrite; increment nsfc(12)

!                           if replacing a special obs with a special obs, increment the not used special counter, nsfc(6)
!                           but not the using special counter, nsfc(3)
!                           also increment duplicate counter
                            !increment non-used special; nsfc(3) remains unchanged since replacing special with special
                            if (spec_obs .and. (trim(adjustl(rpt_use)) == 'SAOSP' .or. trim(adjustl(rpt_use)) == 'FM-16')) &
                            nsfc(6)=nsfc(6)+1
                            if (reg_obs) then
                               

!                               if this is a regular obs and the last valid obs was a special obs,
!                               increment nsfc(6), take one away from nsfc(3), and increment nsfc(4)
                                if (trim(adjustl(rpt_use)) == 'SAOSP' .or. trim(adjustl(rpt_use)) == 'FM-16') then
                                    nsfc(4)=nsfc(4)+1 !increment regular obs used only if last report was special
                                    nsfc(6)=nsfc(6)+1 !increment non-used special obs
                                    nsfc(3)=nsfc(3)-1 !take one way from used special obs
                                else
                                    nsfc(7)=nsfc(7)+1 !increment non-used regular obs but not regular obs used, nsfc(4)
                                endif
                            endif
                            
!                           write message that data is being overwritten and write last data record to replace records file
                            if (debug) write(msg_unit,formstr(5))adjustl(pathid(ipath)),'I45',modnam,'OBSERVATION REPLACED FOR',&
                                date_hr_str,'WITH REPORT TYPE:',trim(adjustl(rpt_type)),'REPLACING REPORT TYPE:',&
                                trim(adjustl(last_rpt))
                            
                            write(ishd_units(2),'(a)')trim(adjustl(lastline))
                            
!                           write core variables and precip if new precip is not missing or 0
                    
!                           only overwrite precip if the current value is not missing and the previous value is missing or zero
                            if (.not. miss_precip .and. (ishobs(1) > 0 .or. (sfdata1(1,ihr,iday1) == sfvars(1)%missval .and. &
                                ishobs(1) == 0))) sfdata1(1,ihr,iday1)=real(ishobs(1),r8)/10._r8                            
                           
                            
   v1:                      do ifield=2,nsfvars_keep
!                               all variables except ASOS sky condition and missing wind direction use conversion
!                               don't convert missing values
                                if ((ifield < 10 .and. ifield /= 4) .or. (ifield == 11 .and. ishobs(ifield) /= &
                                    sfvars(ifield)%missval)) then
                                    if (ishobs(ifield) == sfvars(ifield)%missval) then
                                        sfdata1(ifield,ihr,iday1)=real(ishobs(ifield),r8)
                                    else
                                        sfdata1(ifield,ihr,iday1)=real(ishobs(ifield),r8)*sfvars(ifield)%conv
                                    endif
                                elseif (ifield == 4) then !ASOS sky condition
                                    sfdata1(ifield,ihr,iday1)=real(ishobs(12)*100+ishobs(13),r8)
                                   
                                else !wind speed and direction do not adjust missing value; non-missing wind direction unchanged
                                    sfdata1(ifield,ihr,iday1)=real(ishobs(ifield),r8)
                                endif
                            enddo v1
                            rpt_use=rpt_type !reset rpt_type
!                           if ceiliing height, ASOS ceiling or hazardous weather are audited, write variables
!                           these are the only extra variables from sfdata that are in ISHD data
                            if (sfvars(12)%laudit .or. sfvars(20)%laudit .or. sfvars(21)%laudit) then
                                ifield1=0 !used to determine where in sfdata1 this variable is
    v2:                         do ifield=nsfvars_keep+1,nsfvars
                                    if (sfvars(ifield)%laudit)ifield1=ifield1+1
                                    if ((ifield == 12 .or. ifield == 20 .or. ifield == 21) .and. sfvars(ifield)%laudit) then
                                        if (ifield == 12) sfdata1(ifield1+nsfvars_keep,ihr,iday1)=ishobs(4) !ceiling
                                        if (ifield == 20) sfdata1(ifield1+nsfvars_keep,ihr,iday1)=ishobs(15) !asos ceiling
                                        if (ifield == 21) sfdata1(ifield1+nsfvars_keep,ihr,iday1)=ishobs(14) !visibility
                                    endif
                                enddo v2
                            endif
                        endif
                    else !write to array; first occurrence of date/hour
!                       have at least one observation for the day
                        writedat=.true.
                        if (.not. lsfcobs(iday1))lsfcobs(iday1)=.true.
                      
                        nomisshr(ihr,iday1)=.true.
!                       increment # of obs for the day; don't worry about replacing with future obs because this means the hour has an obs regardless of replacement.
                        nsfc1(iday1)=nsfc1(iday1)+1
                        rpt_use=rpt_type
!                       only increment nsfc(2) when not overwriting, since it has already been incremented when first observation for hour written                        
                       
                        nsfc(2)=nsfc(2)+1
                        if (reg_obs) then
                           
                            nsfc(4)=nsfc(4)+1 !increment regular obs used
                        endif
                        
!                       write message if special obs
                        if (spec_obs) then
                            if (debug)write(msg_unit,formstr(6))adjustl(pathid(ipath)),'I44',modnam,'OBSERVATION FOR DATE:',&
                                date_hr_str,'IS SPECIAL OBSERVATION REPORT TYPE:',trim(adjustl(rpt_type))
                            
                            nsfc(3)=nsfc(3)+1 !increment special obs used
                        endif
!                       do not apply conversion to missing wind direction or speed 
!                       divide non-missing precip by 10; no conversion for missing precip
    v3:                 do ifield=1,nsfvars_keep
!                           apply conversion for missing precip, non-missing winds, and when ifield not equal to 4 (TSKC)
                            if ((ifield > 1 .and. ifield < 10 .and. ifield /= 4) .or. (ifield == 11 .and. ishobs(ifield) /= &
                                sfvars(ifield)%missval)) then
                                if (ishobs(ifield) == sfvars(ifield)%missval) then
                                    sfdata1(ifield,ihr,iday1)=real(ishobs(ifield),r8)
                                else
                                    sfdata1(ifield,ihr,iday1)=real(ishobs(ifield),r8)*sfvars(ifield)%conv
                                endif
                            elseif (ifield == 4) then
                                sfdata1(ifield,ihr,iday1)=real(ishobs(12)*100+ishobs(13),r8)
                                
                            elseif (ifield == 1 .and. ishobs(ifield) /= sfvars(ifield)%missval) then
                                sfdata1(ifield,ihr,iday1)=real(ishobs(1),r8)/10.0_r8
                            else 
                                sfdata1(ifield,ihr,iday1)=real(ishobs(ifield),r8)
                            endif
                        enddo v3
                         
!                       if ceiliing height, ASOS ceiling or visibility are audited, write variables
!                       these are the only extra variables from sfdata that are in ISHD data
                        if (sfvars(12)%laudit .or. sfvars(20)%laudit .or. sfvars(21)%laudit) then
                            ifield1=0 !used to determine where in sfdata1 this variable is
    v4:                     do ifield=nsfvars_keep+1,nsfvars
                                if (sfvars(ifield)%laudit)ifield1=ifield1+1
                                if ((ifield == 12 .or. ifield == 20 .or. ifield == 21) .and. sfvars(ifield)%laudit) then
                                    if (ifield == 12) sfdata1(ifield1+nsfvars_keep,ihr,iday1)=ishobs(4) !ceiling
                                    if (ifield == 20) sfdata1(ifield1+nsfvars_keep,ihr,iday1)=ishobs(15) !asos ceiling
                                    if (ifield == 21) sfdata1(ifield1+nsfvars_keep,ihr,iday1)=ishobs(14) !visibility
                                endif
                            enddo v4
                        endif
                    endif
                    
!**************  write to the surface array *******************

!                   after processing, set savedate and savehr and last_rpt type
                    savedate=sdate
                    savehr=ihr      
                    
                    last_rpt=rpt_type
                    lastline=datline
!                   reset ishobs to missing for next record
    f11:            do ifield=1,28
                        ishobs(ifield)=missobs(ifield)
                    enddo f11
                endif !end record processing
                
            else !bad date string
                write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E42',modnam,'INVALID DATE FIELD:',control(16:27)
                lbad=.true.
                write(ishd_units(1),'(a)')trim(adjustl(datline)) !write to discard file, use for error check on this record
            endif
        endif
    enddo
    if (.not. lbadsfc .and. lbad)lbadsfc=.true. 
    
!   close the discard and replace files
    f12: do ifile=1,2
        close(ishd_units(ifile))
    enddo f12


    return
    end subroutine read_ishd
!*********************************************************************************************************
 
    subroutine read_cd144
!=========================================================================================================
!   SUBROUTINE READ_CD144
!   THIS SUBROUTINE READS CD144 FORMAT NWS DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!       
!   CALLED BY:      MODULE SURFACE (SF_PROC)
!
!   Integer variables
!   eof:            integer end of file indicator to read data file
!   i:              loop counter
!   centuries:      2-element array of the century of the start year (1) and
!                   end year(2)
!   iwarn:          number of warnings that WBAN doesn't match ID from input file
!   iflag1:         I/O flag used for reading variables from file
!   iyear2:         2-digit year before year is set to 4-digits
!   ceil:           3-element array of ceiling variables
!   iceil:          value of CLHT 
!   icent:          number of centuries covered by data
!   sky:            2-element arry of sky cover used for TSKC
!   iliq1,iliq2:    liquid precip weather codes
!   ifrz1,ifrz2:    frozen precip weather codes
!   iv:             loop counter
!   ivsby:          integer visibility 
!   ivis:           20-element integer array of visibility codes
!   itemp:          3-element temperature array for reading temps (1=TMPD, 2=DPTP, 3=TMPW)
!   svar:           integer indicating array index in sfdata1
!   it:             integer indicating array index in itemp
!   ispd:           integer wind speed
!   is:             sky cover loop
!   iweth:          integer weather code
!   last_year:      2-digit year of the previous record read from data
!   icent:          number of centuries since century of start year
!   irec:           record counter
!   ivar:           variable counter
!   savewban:       WBAN from first occurrence in file
!   iday1:          counter of number of days in data period for arrays
!   iflag:          I/O indicator when reading linetype
!   ihr:            hour of day
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!   savehr:         previous observation's hour
!   sdate:          integer observation date (YYYYMMDD)
!   savedate:       previous observation's date (YYYYMMDD)
!
!   Real variables
!   cd144_obs:      12-element array of variables read from CD-144 format data file
!   vsby:           real visibility code
!   visdis:         20-element array of visibility distance codes
!
!   Logical variables
!   crossover:      logical variable denoting that data has cross centuries during
!                   the reading of the data file, used for determining century of
!                   current year being read.
!   gotvis:         logical variable denoting visibility in array
!   lfound:         logical variable used in subroutine check_asos
!   asoshr1:        dummy logical variable used in subroutine check_asos
!   lgo:            logical variable denoting if date of record within data window
!   lsavewban:      denotes that savewban set
!   miss_wind:      denotes if speed is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_dir:      denotes if direction is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_temp:      denotes if temperature is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_precip:    denotes if precip is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   writedat:       denotes to write data to sfdata1
!	
!   Character variables
!   modnam:         Subroutine name
!   adate:          character value of date
!   date_hr_str:    date and hour text string for error messages
!   datline:        data line read from CD-144 file
!   wx:             iweth as character string
!
!   cd144_obs
!   1:  SLVP
!   2:  PRES
!   3:  TSKC
!   4:  PWTH
!   5:  TMPD
!   6:  DPTP
!   7:  RHUM
!   8:  WDIR
!   9:  WSPD
!   10: CLHT
!   11: HZVS
!   12: TMPW
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit
    use file_units, only: sf_data_unit
    implicit none
!   itemp(1)=temperature, itemp(2)=dewpoint, itemp(3)=wet bulb
!   datind = indices to read data from datline
    integer(kind=4) :: eof,i,centuries(2),last_year,iwarn,iflag1,iyear2,icent,iflag,ihr,iyear,imonth,iday,iday1,&
        ceil(3),iceil,sky(2),iliq1,iliq2,ifrz1,ifrz2,ivsby,iv,ivis(20),itemp(3),svar,it,ispd,is,iweth,irec,ivar,savewban,savehr
    integer(kind=8) :: savedate=0
    integer(kind=8) :: sdate
    real(kind=r8) :: cd144_obs(12),vsby,visdis(20)
    logical :: crossover,gotvis,lfound,asoshr1,lgo,lsavewban,miss_wind,miss_dir,miss_precip,miss_temp,writedat
    character(len=8) :: wx,adate
    character(len=20) :: date_hr_str
    character(len=90) :: datline !actual length is 79 but have a buffer
    character(len=10) :: modnam='READ_CD144'
    
    data ivis /0,1,2,3,4,5,6,7,8,9,10,12,14,16,17,18,19,20,24,27/
    data visdis /0.0_r8,0.067_r8,0.125_r8,0.188_r8,0.25_r8,0.312_r8,0.375_r8,0.50_r8,0.625_r8,0.75_r8,1.0_r8,1.125_r8,1.25_r8,&
        1.375_r8,1.5_r8,1.625_r8,1.75_r8,2.0_r8,2.25_r8,2.5_r8/
     
    sdate=0
    lgo=.false.
    crossover=.false.
    last_year=0
    write_asos=.false.
    asoshr1=.false.
    iwarn=0
    cd144_obs=0.0_r8
    ceil=-99999
    sky=-99999
    ispd=-99999
    vsby=real(sfvars(21)%missval,r8)
    ivis=0
    gotvis=.false.
    icent=0
    irec=0
    savewban=0
    lsavewban=.false.
    
!   get the centuries of the start and end dates
!   get 2-digit years
    l1: do i=1,2
        if (i==1) then
            write(adate,'(i8)')sfstart
        else
            write(adate,'(i8)')sfend
        endif
        read(adate(1:2),'(i2)')centuries(i) !2-digit century, i.e. 19, 20
        centuries(i)=centuries(i)*100 !make century 1900 or 2000
    enddo l1
    
!   read the data file
    eof=0
    do while (eof == 0 .and. .not. lbad .and. sdate <=sfend)
        read(sf_data_unit,'(a90)',iostat=eof)datline
        lgo=.false.
        if (eof == 0) then
            cd144_obs=0.0_r8
            ceil=-99999
            sky=-99999
            itemp=-99999
            ispd=-99999
            writedat=.false.
!************** date ****************************************
            read(datline(6:13),'(4(i2))',iostat=iflag)iyear,imonth,iday,ihr
            
            if (iflag == 0) then
                iyear2=iyear !keep 2-digit year
            
!               determine if data file has possibly crossed a century
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
                
                call new_obs(iyear,imonth,iday,ihr,savedate,savehr,lgo,iday1,sdate)
                irec=irec+1
                
                if (lgo) then
                  write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr  
!**************   station ID ****************************************  
                    read(datline(1:5),'(i5)',iostat=iflag1)iwban
                    if (iflag1 == 0) then
!                       check WBAN # against user entered value if file's ID not missing (99999)
!                       also keep the first occurrence for subsequent checks in the data
                        if (.not. lsavewban) then
                            savewban=iwban
                            lsavewban=.true.
                            if (iwban /= isfid .and. iwban /= 99999) then !no match
                                write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'W40',modnam,'STATION ID',iwban,&
                                    'DOES NOT MATCH USER ENTERED ID',trim(adjustl(sfid))
                                iwarn=iwarn+1
!                               check the ASOS status based on iwban
                                call check_asos(iwban,sdate,ihr,.true.,.false.,.false.,lasos,asoshr1)
                                if (lasos) write(msg_unit,ext_form(4))adjustl(pathid(ipath)),'W53',modnam,'STATION',&
                                    trim(adjustl(sfid)),'FOUND IN ASOS LIST BASED ON STATION IDENTIFIERS IN SURFACE FILE'
                            endif
                        else !compare against saved WBAN from first record if not missing (99999)
                            if (iwban /= savewban .and. iwban /= 99999) then !no match
!                               check the ASOS status based on iwban
                                call check_asos(iwban,sdate,ihr,.true.,.false.,.false.,lasos,asoshr1)
!                               alert user that station found in ASOS list			
                                if (lasos) write(msg_unit,ext_form(4))adjustl(pathid(ipath)),'W53',modnam,'STATION',&
                                    trim(adjustl(sfid)),'FOUND IN ASOS LIST BASED ON STATION IDENTIFIERS IN SURFACE FILE'
                                iwarn=iwarn+1
!                               warn user that user entered station ID does not match ID from data file
                                if (iwarn <= nwarnlim)write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'W40',modnam,&
                                    'STATION ID',iwban,'DOES NOT MATCH PREVIOUS WBAN',trim(adjustl(sfid))
                                
!                               notify user that limit reached								
                                if (iwarn == nwarnlim) write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'W42',modnam,&
                                    'REACH LIMIT OF',nwarnlim,'W40','WARNING MESSAGES'
                            endif
                        endif
                    else
                        write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E47',modnam,'INVALID STATION ID FIELD:',datline(1:5)
                        lbad=.true.
                    endif
                    
!**************   station ID ****************************************
!**************  check for ASOS status for the hour ******************************
  
                    if (lasos) call check_asos(iwban,sdate,ihr,.false.,.true.,.false.,lfound,asoshr(ihr,iday1))
!**************  check for ASOS status ******************************
!**************  read actual data ******************************

!                   sea level pressure (SLVP)
                    svar=2
                    ivar=1
                    
!                   sea level pressure is read in as mb
                    if (trim(adjustl(datline(32:35))) == '') then
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8)!*sfvars(svar)%conv
                    else
                        read(datline(32:35),'(f4.0)',iostat=iflag1)cd144_obs(ivar)
                        if (iflag1 /= 0) then
!                           invalid entry
                            write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',&
                                sfvars(svar)%varname,datline(32:35),'FOR',date_hr_str
                            cd144_obs(ivar)=real(sfvars(svar)%missval,r8)*sfvars(svar)%conv
                        else
!                           if less than 800, then is above 1000 mb; otherwise just divide by 10
                            if (cd144_obs(ivar) < 800.0_r8) then
                                cd144_obs(ivar)=cd144_obs(ivar)/10._r8+1000._r8
                            else
                                cd144_obs(ivar)=cd144_obs(ivar)/10._r8
                            endif
                            
                        endif
                    endif
!                   sea level pressure (SLVP)   
                    
!                   station pressure (PRES)
                    svar=3
                    ivar=2
                    if (trim(adjustl(datline(43:46))) =='') then
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8)!*sfvars(svar)%conv
                    else
                        read(datline(43:46),'(f4.0)',iostat=iflag1)cd144_obs(ivar)
                        if (iflag1 /= 0) then
!                           invalid entry
                            write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',&
                                sfvars(svar)%varname,datline(43:46),'FOR',date_hr_str
                            
                            cd144_obs(ivar)=real(sfvars(svar)%missval,r8)*sfvars(svar)%conv
                        else
!                           convert to mb
                            cd144_obs(ivar)=(cd144_obs(ivar)/100.0_r8)*33.8639_r8
                        endif
                    endif
                    
!                   station pressure (PRES)

!                   sky cover (TSKC)
                    svar=4
                    ivar=3
                    call decode144(sdate,ihr,datline(56:56),svar,sky(1)) !1st part of sky cover
                    call decode144(sdate,ihr,datline(79:79),svar,sky(2)) !2nd part of sky cover

    s1:				do is=1,2
                        if (sky(is) == -9999 .or. sky(is) == -99999 .or. sky(is) >= 10) then
                            if (is == 1) then
                                sky(is)=sfvars(svar)%missval/100
                            else
                                sky(is)=sfvars(svar)%missval-(sfvars(svar)%missval/100)*100
                            endif
                        else
                            if (sky(is) == -8888) sky(is)=10
                        endif
                    enddo s1
                    cd144_obs(ivar)=real((sky(1)*100+sky(2)),r8)
!                   sky cover (TSKC)
                    
!                   present weather (PWTH)
                    svar=5
                    ivar=4
                    read(datline(24:31),*,iostat=iflag1)iweth
                    iliq1=0
                    ifrz1=0
                    iliq2=0
                    ifrz2=0
                    if (iflag1 == 0) then
                        write(wx,'(i8.8)')iweth
!                        Classify liquid precipitation
!                       drizzle
                        if(wx(3:3) .EQ. '4') iliq1=33
                        if(wx(3:3) .EQ. '5') iliq1=34
                        if(wx(3:3) .EQ. '6') iliq1=35
                        if(wx(3:3) .EQ. '7') iliq1=36
                        if(wx(3:3) .EQ. '8') iliq1=37
                        if(wx(3:3) .EQ. '9') iliq1=38

!                       Rain
                        if(wx(2:2) .EQ. '1') iliq1=20
                        if(wx(2:2) .EQ. '2') iliq1=21
                        if(wx(2:2) .EQ. '3') iliq1=22
                        if(wx(2:2) .EQ. '4') iliq1=23
                        if(wx(2:2) .EQ. '5') iliq1=24
                        if(wx(2:2) .EQ. '6') iliq1=25
                        if(wx(2:2) .EQ. '7') iliq1=26
                        if(wx(2:2) .EQ. '8') iliq1=27
                        if(wx(2:2) .EQ. '9') iliq1=28

!                        Classify frozen precipitation

!                       Snow showers
                        if(wx(5:5) .EQ. '1') ifrz1=50
                        if(wx(5:5) .EQ. '2') ifrz1=51
                        if(wx(5:5) .EQ. '3') ifrz1=52
                        if(wx(5:5) .EQ. '7') ifrz1=56
                        if(wx(5:5) .EQ. '8') ifrz1=57
                        if(wx(5:5) .EQ. '9') ifrz1=58
    
                        if(wx(4:4) .EQ. '1') ifrz1=40
                        if(wx(4:4) .EQ. '2') ifrz1=41
                        if(wx(4:4) .EQ. '3') ifrz1=42
                        if(wx(4:4) .EQ. '4') ifrz1=43
                        if(wx(4:4) .EQ. '5') ifrz1=44
                        if(wx(4:4) .EQ. '6') ifrz1=45
                        if(wx(4:4) .EQ. '8') ifrz1=47

!                       Ice
                        if(wx(6:6) .EQ. '1') ifrz1=90
                        if(wx(6:6) .EQ. '2') ifrz1=91
                        if(wx(6:6) .EQ. '3') ifrz1=92
                        if(wx(6:6) .EQ. '5') ifrz1=64
                        if(wx(6:6) .EQ. '8') ifrz1=66
                            
                        iliq2=0
                        ifrz2=0
                        if (iliq1 > 0) iliq2=1
                        if (ifrz1 > 0) ifrz2=2
                        cd144_obs(ivar)=real((iliq2+ifrz2),r8)
                    endif
                    
!                   present weather (PWTH)          
                   
!                   temperature (TMPD)
                    svar=7
                    ivar=5
                    it=1
!                   convert from F to C
                    call decode144(sdate,ihr,datline(47:47),svar,itemp(it))
                    if (itemp(it) == -9999) itemp(it)=0 !reset itemp since blanks are field delimiters
                    read(datline(48:49),'(f2.0)',iostat=iflag1)cd144_obs(ivar)
                    
                    if (trim(adjustl(datline(48:49))) == '' .or. itemp(it) == -99999) then
!                       set to missing
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8) 
                    elseif (iflag1 /= 0) then !invalid number, set to missing
                        write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',sfvars(svar)%varname,&
                                datline(48:49),'FOR',date_hr_str
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8) 
                    elseif(itemp(it) == -8888) then !negative temp
                        cd144_obs(ivar)=-cd144_obs(ivar)
                    else 
                        if(itemp(it) >= 10) then !temp less than -99.9
                            cd144_obs(ivar)=-100._r8-cd144_obs(ivar)
                        else
                            cd144_obs(ivar)=real(itemp(it)*100,r8)+cd144_obs(ivar)
                        endif
!                       convert to Celcius
                        cd144_obs(ivar)=(cd144_obs(ivar)-32.0_r8)*(5._r8/9._r8)
                    endif
                 
!                   temperature (TMPD)

!                   dewpoint (DPTP)
                    svar=8
                    ivar=6
                    it=2
!                   convert from F to C
                    call decode144(sdate,ihr,datline(36:36),svar,itemp(it))
                    if (itemp(it) == -9999) itemp(it)=0 !reset itemp since blanks are field delimiters
                    read(datline(37:38),'(f2.0)',iostat=iflag1)cd144_obs(ivar)
                    
                    if (trim(adjustl(datline(37:38))) == '' .or. itemp(it) == -99999) then
!                       set to missing
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8) 
                    elseif (iflag1 /= 0) then !invalid number, set to missing
                        write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',sfvars(svar)%varname,&
                                datline(37:38),'FOR',date_hr_str
                        
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8) 
                    else 
                        if (itemp(it) == -8888) then !negative temp
                            cd144_obs(ivar)=-cd144_obs(ivar)
                        else
                            cd144_obs(ivar)=real(itemp(it)*100,r8)+cd144_obs(ivar)
                        endif
!                       convert to Celcius
                        cd144_obs(ivar)=(cd144_obs(ivar)-32.0_r8)*(5._r8/9._r8)
                    endif
                
!                   dewpoint (DPTP) 
                    
!                   relative humidity (RHUM)
                    svar=9
                    ivar=7
                    
                    if (trim(adjustl(datline(53:55))) == '') then !missing
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                    else
                        read(datline(53:55),'(f3.0)',iostat=iflag1)cd144_obs(ivar)
                        if (iflag1 /=0) then
!                           invalid entry
                            write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',&
                                sfvars(svar)%varname,datline(53:55),'FOR',date_hr_str
                            
                            cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                        endif
                    endif
                            
!                   relative humidity (RHUM)
                    
!                   wind direction (WDIR)
                    svar=10
                    ivar=8
                    
                    if (trim(adjustl(datline(39:40))) == '') then !missing
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                    else
                        read(datline(39:40),'(f2.0)',iostat=iflag1)cd144_obs(ivar)
                        if (iflag1 /=0) then
!                           invalid entry
                            write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',&
                                sfvars(svar)%varname,datline(39:40),'FOR',date_hr_str
                            
                            cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                        else
                            cd144_obs(ivar)=cd144_obs(ivar)*10._r8 !make 3-digit number, i.e. 29 becomes 290
                        endif
                    endif
                            
!                   wind direction (WDIR)
                    
!                   wind speed (WSPD)
                    svar=11
                    ivar=9
                    call decode144(sdate,ihr,datline(41:41),svar,ispd)
                    if (ispd == -9999) ispd=0 
                    if (trim(adjustl(datline(42:42))) == '' .or. ispd == -8888 .or. ispd == -99999) then
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                    else
                        read(datline(42:42),'(f1.0)',iostat=iflag1)cd144_obs(ivar)
                        if (iflag1 /=0) then
!                           invalid entry
                            write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',&
                                sfvars(svar)%varname,datline(42:42),'FOR',date_hr_str
                        
                            cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                        else
                            cd144_obs(ivar)=(cd144_obs(ivar)+real(ispd,r8)*10.0_r8)*0.514791_r8 !convert from knots to m/s
                        endif
                    endif
                    
!                   check for possible calms
                    if (cd144_obs(8) == 0.0_r8 .and. cd144_obs(9) == 0.0_r8)nsfc(11)=nsfc(11)+1
                    
!                   wind speed (WSPD) 
                    
!                   ceiling height (CLHT)
!                   check each individual character and set values for ceil(1), ceil(2), and ceil(3)
                    svar=12
                    ivar=10
    l2:             do i=14,16
                        call decode144(sdate,ihr,datline(i:i),svar,ceil(i-13))
                    enddo l2

!                   calculate ceiling height
                    if (ceil(1) == -8888) then
                        cd144_obs(ivar)=300.0_r8
                    elseif (minval(ceil) >= 0 .and. maxval(ceil) <= 9) then
                        iceil=ceil(1)*100+ceil(2)*10+ceil(3)
                        if (iceil==888) then
                            cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                        else
                            iceil=int(real(iceil,r8)*100._r8*0.3048_r8)!hundreds of feet to feet converted to m
                            cd144_obs(ivar)=real(nint(real(iceil,r8)/100._r8),r8)!convert to km and multiply by 10; basically divide by 100
                        endif
                    elseif (minval(ceil) == -99999) then
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                    endif   
                    
!                   ceiling height (CLHT) 

!                   visibility (HZVS)
                    svar=21
                    ivar=11
                    gotvis=.false.
                    
                    cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                    if (trim(adjustl(datline(21:23))) == '') then
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                    else
                        read(datline(21:23),*,iostat=iflag1)ivsby
                      
                        if (iflag1 == 0) then
                            if (ivsby < 30) then
                                iv=1
                                gotvis=.false.
!                               logic for do while loop is opposite of D144LV in original AERMET
!                               original logic would never execute the loop.
                                do while (.not. gotvis .and. iv <= 20)
                                    if (ivsby == ivis(iv)) then
                                        vsby=visdis(iv)
                                        gotvis=.true.
                                        if (vsby < 0.0_r8) then
                                            cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                                        else
                                            vsby=(vsby*1609.3_r8/1000.0_r8)*10.0_r8
                                            cd144_obs(ivar)=real(nint(vsby),r8)
                                        endif
                                    else
                                        iv=iv+1
                                    endif
                                enddo
                            elseif (ivsby < 990) then
                                cd144_obs(ivar)=real(nint(real(ivsby,r8)/10._r8),r8)
                            elseif (ivsby >= 990) then
                                cd144_obs(ivar)=100.0_r8
                            else
                                cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                            endif
                        else !invalid #
                            write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',&
                                sfvars(svar)%varname,datline(21:23),'FOR',date_hr_str
                            cd144_obs(ivar)=real(sfvars(svar)%missval,r8)
                        endif
                        
                    endif
        
!                   visibility (HZVS)
                    
!                   wet bulb (TMPW)
                    svar=22
                    ivar=12
                    it=3
!                   convert from F to C
                    call decode144(sdate,ihr,datline(50:50),svar,itemp(it))
                    if (itemp(it) == -9999) itemp(it)=0 !reset itemp since blanks are field delimiters
                    read(datline(51:52),'(f2.0)',iostat=iflag1)cd144_obs(ivar)
                    
                    if (trim(adjustl(datline(51:52))) == '' .or. itemp(it) == -99999) then
!                       set to missing
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8) 
                    elseif (iflag1 /= 0) then !invalid number, set to missing
                        write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',sfvars(svar)%varname,&
                                datline(51:52),'FOR',date_hr_str
                        
                        cd144_obs(ivar)=real(sfvars(svar)%missval,r8) 
                    elseif(itemp(it) == -8888) then !negative temp
                        cd144_obs(ivar)=-cd144_obs(ivar)
                    else 
                        cd144_obs(ivar)=real(itemp(it)*100,r8)+cd144_obs(ivar)
!                       convert to Celcius
                        cd144_obs(ivar)=(cd144_obs(ivar)-32.0_r8)*(5._r8/9._r8)
                    endif
                
!                   wet bulb (TMPW)                   
!**************  read actual data ******************************
!**************  write to surface array ************************
! check to see if there are multiple obs for the hour and decide
! whether to overwrite existing obs with latest one.

                    if (nomisshr(ihr,iday1)) then
!                       enter a missing value for precip when checking for duplicates
                        call check_dup(iday1,ihr,real(sfvars(1)%missval,r8),cd144_obs(5),cd144_obs(10),cd144_obs(11),miss_precip,&
                            miss_temp,miss_dir,miss_wind,writedat)
!                       write message that data is being overwritten
                        if (writedat) write(msg_unit,ext_form(5))adjustl(pathid(ipath)),'I45',modnam,&
                            'OBSERVATION REPLACED FOR',date_hr_str
                        
                    else !write to array; first occurrence of date/hour
                        if (.not. lsfcobs(iday1))lsfcobs(iday1)=.true.
                        
                        nomisshr(ihr,iday1)=.true.
                        writedat=.true.
!                       increment # of obs for the day; don't worry about replacing with future obs because this means the hour has an obs regardless of replacement.
                        nsfc1(iday1)=nsfc1(iday1)+1
!                       only increment nsfc(2) when not overwriting, since it has already been incremented when first observation for hour written                        
                        nsfc(2)=nsfc(2)+1  
                        
                    endif
                    if (writedat) then
!                       core variables
!                       note precip not in CD144 data and ASKY not in data
    v1:                 do svar=2,5
                            sfdata1(svar,ihr,iday1)=cd144_obs(svar-1)
                        enddo v1
                        
    v2:                 do svar=7,11
                            sfdata1(svar,ihr,iday1)=cd144_obs(svar-2)
                        enddo v2
!                       if ceiling height, visbility, or wet bulb temp audited, write
                        if (sfvars(12)%laudit .or. sfvars(21)%laudit .or. sfvars(22)%laudit) then
                            ivar=0
    v3:                     do svar=nsfvars_keep+1,nsfvars
                                if (sfvars(svar)%laudit) ivar=ivar+1
                                if ((svar == 12 .or. svar >= 21) .and. sfvars(svar)%laudit) then
                                    if (svar == 12) sfdata1(ivar+nsfvars_keep,ihr,iday1)=cd144_obs(10) !ceiling
                                    if (svar >= 21) sfdata1(ivar+nsfvars_keep,ihr,iday1)=cd144_obs(svar-10) !visibility or wet bulb temperature
                                endif
                            enddo v3
                          endif  
                    endif
                    
!**************  write to surface array ************************
!                   after processing, set savedate and savehr and last_rpt type
                    savedate=sdate
                    savehr=ihr

                endif !end record processing
            else !bad date string
                write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E42',modnam,'INVALID DATE FIELD:',datline(6:13)
                lbad=.true.
            endif
        endif

        
    enddo
    if (.not. lbadsfc .and. lbad)lbadsfc=.true.

    end subroutine read_cd144
!*********************************************************************************************************
 
    subroutine read_scram
!=========================================================================================================
!   SUBROUTINE READ_SCRAM
!   THIS SUBROUTINE READS SCRAM FORMAT NWS DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!       
!   CALLED BY:      MODULE SURFACE (SF_PROC)
!
!   Integer variables
!   eof:            integer end of file indicator to read data file
!   i:              loop counter
!   centuries:      2-element array of the century of the start year (1) and
!                   end year(2)
!   iwarn:          number of warnings that WBAN doesn't match ID from input file
!   iflag1:         I/O flag used for reading variables from file
!   iyear2:         2-digit year before year is set to 4-digits
!   ceil:           3-element array of ceiling variables
!   iceil:          value of CLHT 
!   sky:            2-element arry of sky cover used for TSKC
!   itemp:          integer temperature
!   svar:           integer indicating array index in sfdata1
!   is:             sky cover loop
!   last_year:      2-digit year of the previous record read from data
!   icent:          number of centuries since century of start year
!   irec:           record counter
!   ivar:           variable counter
!   savewban:       WBAN from first occurrence in file
!   iday1:          counter of number of days in data period for arrays
!   iflag:          I/O indicator when reading linetype
!   ihr:            hour of day
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!   savehr:         previous observation's hour
!   sdate:          integer observation date (YYYYMMDD)
!   savedate:       previous observation's date (YYYYMMDD)
!
!   Real variables
!   scram_obs:      12-element array of variables read from CD-144 format data file
!
!   Logical variables
!   crossover:      logical variable denoting that data has cross centuries during
!                   the reading of the data file, used for determining century of
!                   current year being read.
!   lfound:         logical variable denoting station found in ASOS list
!   asoshr1:        dummy logical variable used in subroutine check_asos
!   lgo:            logical variable denoting if date of record within data window
!   lsavewban:      saved WBAN set
!   miss_wind:      denotes if speed is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_dir:      denotes if direction is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_temp:      denotes if temperature is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_precip:    denotes if precip is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   writedat:       denotes to write data to sfdata1
!
!   Character variables
!   adate:          character string of date
!   date_hr_str:    date and hour text string for error messages
!   modnam:         Subroutine name
!   datline:        data line read from SCRAM file
!   asky:           2-element array of total and opaque cloud cover
!   aceil:          character variable for ceiling height
!
!   scram_obs
!   1:  TSKC
!   2:  TMPD
!   3:  WDIR
!   4:  WSPD
!   5:  CLHT
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit
    use file_units, only: sf_data_unit
    implicit none
    
    integer(kind=4) :: eof,i,centuries(2),last_year,iwarn,iflag1,iyear2,icent,iyear,imonth,iday,&
        ceil(3),iceil,sky(2),svar,is,irec,ivar,savewban,iday1,iflag,ihr,savehr 
    integer(kind=8) :: savedate=0
    integer(kind=8) :: sdate
    real(kind=r8) :: scram_obs(5)
    logical :: crossover,lfound,asoshr1,lgo,lsavewban,miss_wind,miss_dir,miss_temp,miss_precip,writedat
    character(len=8) :: adate
    character(len=30) :: datline
    character(len=2) :: asky2(2)  !for TSKC
    character(len=3) :: aceil
    character(len=20) :: date_hr_str
    character(len=10) :: modnam='READ_SCRAM'
     
    lgo=.false.
    crossover=.false.
    asoshr1=.false.
    last_year=0
    sdate=0
    !write_asos=.false.
    iwarn=0
    scram_obs=0.0_r8
    ceil=-99999
    sky=-99999
    writedat=.false.
    miss_wind=.false.
    miss_dir=.false.
    miss_temp=.false.
    miss_precip=.false.
    asky2=''
    icent=0
    irec=0
    savewban=0
    lsavewban=.false.
!   get the centuries of the start and end dates
!   get 2-digit years
    l1: do i=1,2
        if (i==1) then
            write(adate,'(i8)')sfstart
        else
            write(adate,'(i8)')sfend
        endif
        read(adate(1:2),'(i2)')centuries(i) !2-digit century, i.e. 19, 20
        centuries(i)=centuries(i)*100 !make century 1900 or 2000
    enddo l1
    
!   read the data file
    eof=0
    do while (eof == 0 .and. .not. lbad .and. sdate <=sfend)
        read(sf_data_unit,'(a30)',iostat=eof)datline
        lgo=.false.
        if (eof == 0) then
            scram_obs=0.0_r8
            ceil=-99999
            sky=-99999
            writedat=.false.
            irec=irec+1
!************** date ****************************************
            read(datline(6:13),'(4(i2))',iostat=iflag)iyear,imonth,iday,ihr
            if (iflag == 0) then
                iyear2=iyear !keep 2-digit year
            
!               determine if data file has possibly crossed a century
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
                
!               check date/hr
                call new_obs(iyear,imonth,iday,ihr,savedate,savehr,lgo,iday1,sdate)
                if (lgo) then     
                    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
!**************   station ID ****************************************  
                    read(datline(1:5),'(i5)',iostat=iflag1)iwban
                    if (iflag1 == 0) then
!                       check WBAN # against user entered value if file's ID not missing (99999)
!                       also keep the first occurrence for subsequent checks in the data
                        if (.not. lsavewban) then
                            savewban=iwban
                            lsavewban=.true.
                            if (iwban /= isfid .and. iwban /= 99999) then !no match
                                write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'W40',modnam,'STATION ID',iwban,&
                                    'DOES NOT MATCH USER ENTERED ID',trim(adjustl(sfid))
                                iwarn=iwarn+1
                                call check_asos(iwban,sdate,ihr,.true.,.false.,.false.,lasos,asoshr1)
                            endif
                        else !compare against saved WBAN or USAF from first record if not missing (99999)
                            if (iwban /= savewban .and. iwban /= 99999) then !no match
                                iwarn=iwarn+1
                                if (iwarn <= nwarnlim)write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'W40',modnam,'STATION ID',&
                                    iwban,'DOES NOT MATCH PREVIOUS WBAN',trim(adjustl(sfid))
                                if (iwarn == nwarnlim) write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'W42',modnam,&
                                    'REACH LIMIT OF',nwarnlim,'W40','WARNING MESSAGES'
                                
                                call check_asos(iwban,sdate,ihr,.true.,.false.,.false.,lasos,asoshr1)
                            endif
                        endif
                    else
                        write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E47',modnam,'INVALID STATION ID FIELD:',datline(1:5)
                        lbad=.true.
                    endif
                    
!**************   station ID ****************************************
!**************  check for ASOS status ******************************
                    call check_asos(iwban,sdate,ihr,.false.,.true.,.false.,lfound,asoshr(ihr,iday1))
!**************  check for ASOS status ******************************
!**************  read actual data ******************************
                    aceil=datline(14:16)
                    asky2(1)=datline(25:26)!total
                    asky2(2)=datline(27:28)!total opaque
                    
!                   sky cover (TSKC)
                    svar=4
                    ivar=1
                    
                    if (trim(adjustl(asky2(2))) == '') then
                        if (asky2(1) /= '') then
                            if (asky2(1) == '10') then
                                asky2(2)='--'
                            else
                                asky2(2)=asky2(1)
                            endif
                        elseif (trim(adjustl(aceil)) == '') then
                            asky2(2)=''
                        elseif (aceil == '---') then
                            asky2(2)='00'
                        elseif (aceil < '070') then
                            asky2(2)='07'
                        else
                            asky2(2)='00'
                        endif
                    elseif (asky2(2) == '10') then
                        asky2(2)='--'
                    endif
                    
                    if (asky2(1) == '10') asky2(1)='--'
                    
                    call decode144(sdate,ihr,asky2(1)(2:2),svar,sky(1)) !1st part of sky cover
                    call decode144(sdate,ihr,asky2(2)(2:2),svar,sky(2)) !2nd part of sky cover
               
!                   set cloud cover to missing if ASOS 
                    if (asoshr(ihr,iday1)) then
                        scram_obs(ivar)=sfvars(svar)%missval
                    else
                        if (minval(sky) == -9999 .or. minval(sky) == -99999 .or. maxval(sky) >= 10) then
                            scram_obs(ivar)=sfvars(svar)%missval
                        else
                            if (minval(sky) == -8888) then
    s1:                         do is=1,2
                                    if (sky(is)==-8888) sky(is)=10
                                enddo s1
                            endif
                            scram_obs(ivar)=real((sky(1)*100+sky(2)),r8)
                        endif
                    endif
!                   sky cover (TSKC) 
                   
!                   temperature (TMPD)
                    svar=7
                    ivar=2
!                   convert from F to C
                    read(datline(22:24),'(f3.0)',iostat=iflag1)scram_obs(ivar)
                    
                    if (trim(adjustl(datline(22:24))) == '') then
!                       set to missing
                        scram_obs(ivar)=real(sfvars(svar)%missval,r8) !*sfvars(ivar)%conv
                    elseif (iflag1 /= 0) then !invalid number, set to missing
                        write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',&
                                sfvars(svar)%varname,datline(22:24),'FOR',date_hr_str
                        scram_obs(ivar)=real(sfvars(svar)%missval,r8) !*sfvars(ivar)%conv
                    else 
!                       convert to Celcius
                        scram_obs(ivar)=(scram_obs(ivar)-32.0_r8)*(5._r8/9._r8)
                    endif
                
!                   temperature (TMPD)
                    
!                   wind direction (WDIR)
                    svar=10
                    ivar=3
                    
                    if (trim(adjustl(datline(17:18))) == '') then !missing
                        scram_obs(ivar)=real(sfvars(svar)%missval,r8)
                    else
                        read(datline(17:18),'(f2.0)',iostat=iflag1)scram_obs(ivar)
                        if (iflag1 /=0) then
                            write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',&
                                sfvars(svar)%varname,datline(17:18),'FOR',date_hr_str
                            scram_obs(ivar)=real(sfvars(svar)%missval,r8)
                        else
                            scram_obs(ivar)=scram_obs(ivar)*10._r8 !make 3-digit number, i.e. 29 becomes 290
                        endif
                    endif
                            
!                   wind direction (WDIR)
                    
!                   wind speed (WSPD)
                    svar=11
                    ivar=4
                    
                    if (trim(adjustl(datline(19:21))) == '') then
                        scram_obs(ivar)=real(sfvars(svar)%missval,r8)
                    else
                        read(datline(19:21),'(f3.0)',iostat=iflag1)scram_obs(ivar)
                        if (iflag1 /=0) then
                            write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W47',modnam,'INVALID ENTRY FOR',&
                                sfvars(svar)%varname,datline(19:21),'FOR',date_hr_str
                            scram_obs(ivar)=real(sfvars(svar)%missval,r8)
                        else
                            scram_obs(ivar)=scram_obs(ivar)*0.514791_r8 !convert from knots to m/s
                        endif
                    endif
                    
!                   check for possible calms
                    if (scram_obs(3) == 0.0_r8 .and. scram_obs(4) == 0.0_r8)nsfc(11)=nsfc(11)+1
                    
!                   wind speed (WSPD) 
                    
!                   ceiling height (CLHT)
!                   check each individual character and set values for ceil(1), ceil(2), and ceil(3)
                    svar=12
                    ivar=5
    v1:             do i=1,3
                        call decode144(sdate,ihr,aceil(i:i),svar,ceil(i))
                    enddo v1

!                   calculate ceiling height
                    if (ceil(1) == -8888) then
                        scram_obs(ivar)=300.0_r8
                    elseif (minval(ceil) >= 0 .and. maxval(ceil) <= 9) then
                        iceil=ceil(1)*100+ceil(2)*10+ceil(3)
                        if (iceil==888) then
                            scram_obs(ivar)=real(sfvars(svar)%missval,r8)
                        else
                            iceil=int(real(iceil,r8)*100._r8*0.3048_r8)!hundreds of feet to feet converted to m
                            scram_obs(ivar)=real(nint(real(iceil,r8)/100._r8),r8)!convert to km and multiply by 10; basically divide by 100
                        endif
                    elseif (minval(ceil) == -99999) then
                        scram_obs(ivar)=real(sfvars(svar)%missval,r8)
                    endif   
                    
!                   ceiling height (CLHT) 

         
!**************  read actual data ******************************
!**************  write to surface array ************************
! check to see fi there are multiple obs for the hour and decide
! whether to overwrite exsint obs with latest one.

                    if (nomisshr(ihr,iday1)) then
!                       enter a missing value for precip when checking for duplicates
                        call check_dup(iday1,ihr,real(sfvars(1)%missval,r8),scram_obs(2),scram_obs(3),scram_obs(4),miss_precip,&
                            miss_temp,miss_dir,miss_wind,writedat)
!                       write message that data is being overwritten
                        if (writedat) write(msg_unit,ext_form(5))adjustl(pathid(ipath)),'I45',modnam,&
                            'OBSERVATION REPLACED FOR',date_hr_str
                        
                    else !write to array; first occurrence of date/hour
                        if (.not. lsfcobs(iday1))lsfcobs(iday1)=.true.
                        nomisshr(ihr,iday1)=.true.
                        writedat=.true.
!                       increment # of obs for the day; don't worry about replacing with future obs because this means the hour has an obs regardless of replacement.
                        nsfc1(iday1)=nsfc1(iday1)+1
!                       only increment nsfc(2) when not overwriting, since it has already been incremented when first observation for hour written                        
                        nsfc(2)=nsfc(2)+1
                    endif
                    if (writedat) then
!                       core variables
!                       note precip not in CD144 data and ASKY not in data
                        
!                       TSKC
                        sfdata1(4,ihr,iday1)=scram_obs(1)
!                       PWTH set to 0
                        sfdata1(5,ihr,iday1)=0.0_r8
!                       TMPD
                        sfdata1(7,ihr,iday1)=scram_obs(2)
!                       WDIR and WSPD
    v2:                 do svar=10,11
                            sfdata1(svar,ihr,iday1)=scram_obs(svar-7)
                        enddo v2
                        
!                       if ceiling height audited, write
                        if (sfvars(12)%laudit) then
                            ivar=0
    v3:                     do svar=nsfvars_keep+1,nsfvars
                                if (sfvars(svar)%laudit) ivar=ivar+1
                                if (svar == 12 .and. sfvars(svar)%laudit) sfdata1(ivar+nsfvars_keep,ihr,iday1)=scram_obs(5) !ceiling
                            enddo v3
                          endif  
                    endif
                    
!**************  write to surface array ************************
!                   after processing, set savedate and savehr and last_rpt type
                    savedate=sdate
                    savehr=ihr

                endif !end record processing
            else !bad date string
                write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E42',modnam,'INVALID DATE FIELD:',datline(6:13)
                lbad=.true.
            endif
        endif

        
    enddo

    if (.not. lbadsfc .and. lbad)lbadsfc=.true.

    end subroutine read_scram
!*********************************************************************************************************

    subroutine decode144(sdate,ihr,cstr,ivar,outvar)
!=========================================================================================================
!   SUBROUTINE DECODE
!   THIS SUBROUTINE DECODES CD-144 OVERPUNCHES
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (READ_CD144, READ_SCRAM)
!
!   INPUT ARGUMENT(S)
!   SDATE:			DATE OF OBSERVATION (YYYYMMDD)
!   CSTR:           1-CHARACTER STRING BEING EVALUATED
!   IVAR:           INDEX IN SFVARS OF VARIABLE BEING EVALUATED
!
!   OUTPUT ARGUMENT(S)
!   OUTVAR:         INTEGER VARIABLE DECODED FROM CSTR
!   
!   Variable definitions
!   
!   Integer variables
!   sdate:			date of observation (yyyymmdd)
!   j:              loop counter
!   ivar:           index in sfvars of variable being evaluated
!   outvar:         output decoded from cstr
!   ihr:            hour of day
!   sdate:          date being processed (YYYYMMDD)
!
!   Logical variables
!   lfound1:          used for looping
!
!   Character variables
!   cstr:           input character string being evalauted
!   ovr11:          set of characters to evaluate against
!   ovr12:          set of characters to evaluate against
!   ovrnor:         set of characters to evaluate against
!   formstr:        format for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    implicit none
    integer(kind=4), intent(in) :: ivar,ihr
    integer(kind=8), intent(in) :: sdate
    integer(kind=4), intent(out) :: outvar
    integer(kind=4) :: j
    character(len=1), intent(in):: cstr
    character(len=1) :: ovr11(10),ovr12(10),ovrnor(10)
    character(len=60) :: formstr
    character(len=10) :: modnam='DECODE144'
    logical lfound1
    
     data ovr11/'[','A','B','C','D','E','F','G','H','I'/, ovr12/']','J','K','L','M','N','O','P','Q','R'/, &
         ovrnor/'0','1','2','3','4','5','6','7','8','9'/
     
    
!   format for improper code
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'2(a,1x),i8,1x,a,1x,i2.2)'
    
    if (cstr == 'X' .or. cstr == '-') then
        outvar=-8888
    elseif (cstr == ' ') then
        outvar=-9999
    else
        j=0
        lfound1=.false.
        do while (j <= 10 .and. .not. lfound1)
            if (cstr == ovrnor(j+1)) then
                outvar=j
                lfound1=.true.
            elseif (cstr == ovr11(j+1)) then
                outvar=j+10
                lfound1=.true.
            elseif (cstr==ovr12(j+1)) then
                outvar=j+10
                lfound1=.true.
            endif
                j=j+1
        enddo
    endif
    
    if (outvar == -99999) write(msg_unit,formstr)adjustl(pathid(ipath)),'W47',modnam,'CODES NOT PROPERLY DECODED FOR DATE',&
        trim(adjustl(sfvars(ivar)%varname)),sdate,'HR',ihr
    
    return
    end subroutine decode144
!*********************************************************************************************************
 
    subroutine read_sam
!=========================================================================================================
!   SUBROUTINE READ_SAM
!   THIS SUBROUTINE READS SAMSON FORMAT NWS DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!       
!   CALLED BY:      MODULE SURFACE (SF_PROC)
!
!   Integer variables
!   eof:            integer end of file indicator to read data file
!   i:              loop counter
!   centuries:      2-element array of the century of the start year (1) and
!                   end year(2)
!   iwban:          WBAN read from file
!   iflag1:         I/O flag used for reading variables from file
!   iyear2:         2-digit year before year is set to 4-digits
!   sky:            2-element arry of sky cover used for TSKC
!   svar:           integer indicating array index in sfdata1
!   ielev:           integer elevation read from first line of data file
!   j:              looping variable
!   ifield:         field counter for 2nd header row
!   lastfield:      number of fields read from 2nd header row
!   ivar1:          variable counter
!   datevars:       4-element array of year, month, day, and hour
!   np:             variable read from weather code
!   iliq:           liquid precip indicator
!   ifrzn:          frozen precip indicator
!   ipcode:         precip indicator
!   maxvar:         maximum number of variables read from data file
!   linelen1:       length of data line
!   ind:            index locations of column header on 2nd row
!   idvar:          header designations for each column
!   last_year:      2-digit year of the previous record read from data
!   icent:          number of centuries since century of start year
!   irec:           record counter
!   ivar:           variable counter
!   iday1:          counter of number of days in data period for arrays
!   ihr:            hour of day
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!   savehr:         previous observation's hour
!   sdate:          integer observation date (YYYYMMDD)
!   savedate:       previous observation's date (YYYYMMDD)
!
!   Real variables
!   sam_obs:        11-element array of variables read from SAMSON format data file
!   elev:           real elevation based on ielev
!   rtemp:          real variable read from data file for real variables
!
!   Logical variables
!   crossover:      logical variable denoting that data has cross centuries during
!                   the reading of the data file, used for determining century of
!                   current year being read.
!   lfound:         logical variable denoting station found in ASOS list
!   gotpress:       logical variable denoting that station pressure is in data file
!   good_date:      logical variable denoting that the datevars elements are valid
!   have_elev:      logical variable denoting elevation read from file
!   asoshr1:        dummy logical variable used in subroutine check_asos
!   lgo:            logical variable denoting if date of record within data window
!   miss_wind:      denotes if speed is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_dir:      denotes if direction is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_temp:      denotes if temperature is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_precip:    denotes if precip is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   writedat:       denotes to write data to sfdata1
!
!   Character variables
!   modnam:         Subroutine name
!   date_hr_str:    date and hour text string for error messages
!   formstr:        format for messages unique to read_sam
!   datline:        data line read from SAMSON file
!   pflag:          precip flag
!   missflag:       missing precip flag
!   accflag:        accumulated precip flag
!   datestr:        4-element array of date variables used for QA of datevars
!   sam_vars:       11-element character array of data variables
!   atemp:          character value of sam_vars for particular variables
!   datform:        format statement for reading datline from file
!   wmft:           formats to read variables from sam_vars
!   vfmt:           formats to read column headers
!   aelev:			character string of elevation
!   sam_form:       format for reading data
!   hdr_vars:       header variables
!   adate:          character date string (YYYYMMDD)
!
!   sam_obs
!   1:  PRCP
!   2:  PRES
!   3:  TSKC
!   4:  PWTH
!   5:  TMPD
!   6:  DPTP
!   7:  RHUM
!   8:  WDIR
!   9:  WSPD
!   10: CLHT
!   11: HZVS
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit
    use main1, only: getfields
    use file_units, only: sf_data_unit
    implicit none
    integer(kind=4) :: eof,i,centuries(2),last_year,iwban,iflag1,iyear2,ivar,iday1,iyear,imonth,iday,&
        sky(2),svar,ielev,j,ifield,lastfield,ivar1,datevars(4),np,iliq,ifrzn,ipcode,icent,irec,ihr,savehr
    integer(kind=4), parameter :: maxvar=26
    integer(kind=4), parameter :: linelen1=200
    integer(kind=4) :: ind(maxvar),idvar(maxvar)
    integer(kind=8) :: savedate=0
    integer(kind=8) :: sdate
    real(kind=r8) :: sam_obs(11),elev,rtemp
    logical :: crossover,lfound,gotpress,good_date,have_elev,asoshr1,lgo,miss_wind,miss_dir,miss_temp,miss_precip,writedat
    character(len=1) :: pflag,missflag,accflag
    character(len=5) :: datestr(4)
    character(len=9) :: sam_vars(maxvar),atemp,datform
    character(len=6) :: wfmt(maxvar)
    character(len=180) :: sam_form
    character(len=8) :: adate
    character(len=4) :: aelev !character elevation
    character(len=linelen1) :: datline !actual length is 59 but have a buffer
    character(len=10) :: modnam='READ_SAM'
    character(len=2) :: vfmt(maxvar-5)
    character(len=40) :: hdr_vars(maxvar)
    character(len=20) :: date_hr_str
    character(len=60) :: formstr(4)
    
    
     
    data vfmt /'a4','a4','a7','a7','a7','a2','a2','a5','a5','a3','a4','a3','a5','a6','a6','a9','a4','a6','a4','a3','a7'/
    data wfmt /'(i3)','(i3)','(i3)','(i3)','(i1)','(a4)','(a4)','(a7)','(a7)','(a7)','(f2.0)','(f2.0)','(f5.0)','(f5.0)',&
    '(f3.0)','(f4.0)','(f3.0)','(f5.0)','(f6.0)','(f6.0)','(a9)','(f4.0)','(f6.0)','(f4.0)','(f3.0)','(a7)'/
    
    data datestr /'YEAR','MONTH','DAY','HOUR'/
    
!   formats for error messages
!   1. station elevation missing in file but provided by users
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,f9.2,1x,a1)'

!   2.  elevation in file missing and no user elevation, consider adding elevation
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a)'
    
!   3.  invalid date field
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
    
!   4.  data are modeled message
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20)'
    
    lgo=.false.
    crossover=.false.
    sdate=0
    last_year=0
    write_asos=.false.
    sky=-99
    writedat=.false.
    miss_wind=.false.
    miss_dir=.false.
    miss_temp=.false.
    miss_precip=.false.
    asoshr1=.false.
    good_date=.false.
    sam_vars=''
    ind=0
    lastfield=0
    idvar=0
    gotpress=.false.
    datevars=0
    have_elev=.false.
    pflag=''
    accflag=''
    missflag=''
    iliq=0
    ifrzn=0
    icent=0
    irec=0
!   initialize sam_obs to missing
    sam_obs(1)=real(sfvars(1)%missval,r8) !PRCP
    sam_obs(2)=real(sfvars(3)%missval,r8) !PRES
    sam_obs(3)=real(sfvars(4)%missval,r8) !TSKC
    sam_obs(4)=real(sfvars(5)%missval,r8) !PWTH
    sam_obs(5)=real(sfvars(7)%missval,r8) !TMPD
    sam_obs(6)=real(sfvars(8)%missval,r8) !DPTP
    sam_obs(7)=real(sfvars(9)%missval,r8) !RHUM
    sam_obs(8)=real(sfvars(10)%missval,r8) !WDIR
    sam_obs(9)=real(sfvars(11)%missval,r8) !WSPD
    sam_obs(10)=real(sfvars(12)%missval,r8) !CLHT
    sam_obs(11)=real(sfvars(21)%missval,r8) !HZVS
    
!   initial value of sam_form (format statement to read data)
    sam_form='(4(a3),1x,a1'
    
!   get the centuries of the start and end dates
!   get 2-digit years
    l1: do i=1,2
        if (i==1) then
            write(adate,'(i8)')sfstart
        else
            write(adate,'(i8)')sfend
        endif
        read(adate(1:2),'(i2)')centuries(i) !2-digit century, i.e. 19, 20
        centuries(i)=centuries(i)*100 !make century 1900 or 2000
    enddo l1
    
!   read the data file
    eof=0
    write(datform,'(a1,i3,a2)')'(',linelen1,'a)'
    
    do while (eof == 0 .and. .not. lbad .and. sdate <=sfend)
        read(sf_data_unit,datform,iostat=eof)datline
        if (eof == 0) then
            irec=irec+1 !record counter
!**************   station ID & elevation ****************************************
            if (irec == 1) then !header record
!               get WBAN
                read(datline,'(t2,i5)',iostat=iflag1)iwban
                if (iflag1 == 0) then
!                   check WBAN # against user entered value if file's ID not missing (99999)
!                   also keep the first occurrence for subsequent checks in the data
                    if (iwban /= isfid .and. iwban /= 99999) then !no match
                        write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'W40',modnam,'STATION ID',iwban,&
                            'DOES NOT MATCH USER ENTERED ID',trim(adjustl(sfid))
                        call check_asos(iwban,sdate,0,.true.,.false.,.false.,lasos,asoshr1)
                    endif
                else
                    write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E47',modnam,'INVALID STATION ID FIELD:',datline(1:5)
                    lbad=.true.
                endif       
!               get elevation
                read(datline,'(t56,a4)')aelev
                if (trim(adjustl(aelev)) == '') then
                    if (user_elev) then
                        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W43',modnam,&
                            'STATION ELEVATION PROVIDED IN FILE BUT MISSING; USE USER ELEVATION',sfelev,'M'
                    else
                        write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W43',modnam,&
                            'STATION ELEVATION PROVIDED IN FILE BUT MISSING; NO USER ELEVATION, CONSIDER INPUTTING ELEVATION'
                    endif
                else !non-missing elevation
                    read(aelev,'(i4)',iostat=iflag1)ielev
                    if (iflag1 == 0) then
                        elev=real(ielev,r8)
                        if (user_elev) then  !check for consistency
                            if (dabs(sfelev - elev) > 0.02_r8*elev) write(msg_unit,ext_form(9))adjustl(pathid(ipath)),'W43',&
                            modnam,'USER ENTERED ELEVATION',sfelev,'M DIFFERS FROM ELEVATION',elev,'M IN SAMSON FILE'
                            
                        endif
                        sfelev=elev       
                        have_elev=.true.
                        lsfelev=.true.
                    else
                        lbad=.true.
                        write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E43',modnam,'INVALID ELEVATION FIELD',aelev
                    endif
                endif
!**************   station ID & elevation ****************************************            
            elseif (irec == 2) then !variable numbers; based on getfields subroutine
!               initialize
                ind=0
                j=0
                ifield=1
                ind(ifield)=1
!               get number of fields and location of first non-blank character of the individual fields
    l2:         do j=2,len_trim(datline)
                    if (ichar(datline(j-1:j-1)) == 32 .and. ichar(datline(j:j)) /= 32) then                     
                        ifield=ifield+1
                        ind(ifield)=j
                        lastfield=ifield
                    endif
                enddo l2 
      
!               fill in fields
    f1:         do ifield=1,lastfield
                    if (ifield /= lastfield) then
                        read(datline(ind(ifield):ind(ifield+1)-1),'(a)')hdr_vars(ifield)
                    else
                        read(datline(ind(ifield):len_trim(datline)),'(a)')hdr_vars(ifield)      
                    endif
                enddo f1
!               create format statement    
!               skip first five variables (year, month, day, hour, indicator)
    v1:         do ivar=6,lastfield
                    read(hdr_vars(ivar),*,iostat=iflag1)ivar1
                    if (iflag1 == 0) then
                        sam_form=trim(adjustl(sam_form))//',1x,'//trim(adjustl(vfmt(ivar1)))
                        idvar(ivar)=ivar1
                        if (ivar1 == 11) gotpress=.true. !have station pressure in file
                    else
                        write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E45',modnam,'INVALID SAMSON HEADER',&
                            trim(adjustl(hdr_vars(ivar)))
                    endif
                enddo v1

!               put final parenthesis
                sam_form=trim(adjustl(sam_form))//')'
            else
!               initialize sam_obs to missing
                sam_obs(1)=real(sfvars(1)%missval,r8) !PRCP
                sam_obs(2)=real(sfvars(3)%missval,r8) !PRES
                sam_obs(3)=real(sfvars(4)%missval,r8) !TSKC
                sam_obs(4)=real(sfvars(5)%missval,r8) !PWTH
                sam_obs(5)=real(sfvars(7)%missval,r8) !TMPD
                sam_obs(6)=real(sfvars(8)%missval,r8) !DPTP
                sam_obs(7)=real(sfvars(9)%missval,r8) !RHUM
                sam_obs(8)=real(sfvars(10)%missval,r8) !WDIR
                sam_obs(9)=real(sfvars(11)%missval,r8) !WSPD
                sam_obs(10)=real(sfvars(12)%missval,r8) !CLHT
                sam_obs(11)=real(sfvars(21)%missval,r8) !HZVS
                sky=99
!               read data
                
                read(datline,sam_form)(sam_vars(ivar),ivar=1,lastfield)
!               if first character of line is ~, then a new header, i.e. more than 1 year
!               alert user and stop processing.
                if (datline(1:1) == '~') then
                    write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E46',modnam,'MORE THAN ONE SET OF HEADER RECORDS IN FILE'
                    lbad=.true.
                else
!                   get date and hour
                    datevars=0
                    good_date=.false.
    v2:             do ivar=1,4
                        read(sam_vars(ivar),*,iostat=iflag1)datevars(ivar)
                        if (iflag1 ==0) then
                            good_date=.true.
                        else
                            lbad=.true.
                            good_date=.false.
                            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'E42',modnam,'INVALID',trim(adjustl(datestr(ivar))),&
                                ' FIELD:',trim(adjustl(sam_vars(ivar)))
                            lbad=.true.
                        endif
                    enddo v2
                    if (good_date) then
                        iyear2=datevars(1) !keep 2-digit year
!                       determine if data file has possibly crossed a century
                        if (datevars(1) < last_year)then !implies a century crossover, e.g. 1999 to 2000
                            crossover=.true. 
                            icent=icent+1
                        endif
                        if (crossover) then
                            datevars(1)=datevars(1)+centuries(1)+icent*100
                        else
                            datevars(1)=datevars(1)+centuries(1)
                        endif
                        last_year=iyear2
                        ihr=datevars(4)
                        iyear=datevars(1)
                        imonth=datevars(2)
                        iday=datevars(3)
!                       check date/hr
                        call new_obs(iyear,imonth,iday,ihr,savedate,savehr,lgo,iday1,sdate)
                    
                        if (lgo) then !proceed
!                           if data is modeled, alert user
                            if (trim(adjustl(sam_vars(5))) == '9') write(msg_unit,formstr(4))adjustl(pathid(ipath)),'I58',modnam,&
                            'DATA ARE MODELED FOR',date_hr_str               
!**************             check for ASOS status ******************************
                            call check_asos(iwban,sdate,ihr,.false.,.true.,.false.,lfound,asoshr(ihr,iday1))
!**************         check for ASOS status ******************************
!**************         process weather variables ******************************
!                           note variables are already in metric system except
!                           precip so those variables do not need units conversion

    v3:                     do ivar=6,lastfield
                                if (idvar(ivar) <= 5 .or. idvar(ivar) == 16 .or. idvar(ivar) == 21) then !read in as character string
                                    read(sam_vars(ivar),wfmt(idvar(ivar)+5))atemp
                                    if (idvar(ivar) == 16) then  !weather (PWTH)
                                        iliq=0
                                        ifrzn=0
    l3:                                 do j=6,2,-1
                                            if (atemp(j:j) == '9') then
                                                cycle l3
                                            else
                                                read(atemp(j:j),*)np
                                                ipcode=j*10+np
                                            endif
                                            if (ipcode <= 39) then
                                                iliq=1
                                            else
                                                ifrzn=2
                                            endif
                                        enddo l3
                                        sam_obs(4)=real(iliq+ifrzn,r8)
                                    endif !weather (PWTH)
                                    if (idvar(ivar) == 21) then !precipitation (PRCP)
!                                       precip is hundredths of inches, i.e. 1 is 0.01 inches
!                                       convert to mm
                                        read(atemp,'(f6.0,a1)')rtemp,pflag
                                        if (trim(adjustl(pflag)) == '') then
                                            if (trim(adjustl(missflag)) == '') sam_obs(1)=rtemp*0.254_r8
                                        elseif (trim(adjustl(pflag)) == 'M') then
                                            if (trim(adjustl(missflag)) == '') then
                                                missflag='M'
                                            else
                                                missflag=''
                                            endif
                                        elseif (trim(adjustl(pflag)) == 'A') then
                                            if (rtemp < 99990.0_r8) sam_obs(1)=rtemp*0.254_r8
                                            if (trim(adjustl(accflag)) == '') then
                                                accflag='A'
                                            elseif (trim(adjustl(accflag)) == 'A') then
                                                accflag=''
                                            endif
                                        endif
                                    endif !precipitation (PRCP)
                                else !read in as number
                                    read(sam_vars(ivar),wfmt(idvar(ivar)+5),iostat=iflag1)rtemp
!                                   6 is total sky cover, 7 is opaque
                                    if (idvar(ivar) == 6 .or. idvar(ivar) == 7) then !total sky components (TSKC)
                                       
                                        if (asoshr(ihr,iday1)) then !set to missing
                                            sky(idvar(ivar)-5)=99
                                        else
                                            sky(idvar(ivar)-5)=nint(rtemp)
                                        endif
                                    endif !total sky components (TSKC)
!                                   8 is dry bulb temp (TMPD), 10 is relative humidity (RHUM)
                                    if (idvar(ivar) == 8 .or. idvar(ivar) == 10) then 
                                        if (rtemp < 900.0_r8) sam_obs(idvar(ivar)-3) = rtemp
                                    endif
                                    if (idvar(ivar) == 9) then !dewpoint (DPTP)
                                        if (rtemp < 9000.0_r8) sam_obs(6) = rtemp
                                    endif !dewpoint (DPTP)
                                    if (idvar(ivar) == 11) then !station pressure (PRES)
                                        if (rtemp < 9000._r8) then
                                            sam_obs(2)=rtemp
                                        else
!                                           if missing and have elevation, calculate from elevation
                                            if (have_elev .or. user_elev) sam_obs(2)=1013.25_r8*(1.0_r8-((6.5e-3_r8/288.15_r8)&
                                            *sfelev))**5.255_r8
                                        endif
                                    endif !station pressure (PRES)
                                    if (idvar(ivar) == 12) then !wind direction (WDIR)
!                                       nearest 10 degrees
                                        if (rtemp < 10._r8) rtemp=real(nint(rtemp/10._r8),r8)
                                        if (rtemp <= 360._r8) sam_obs(8)=rtemp
                                    endif !wind direction (WDIR)
                                    if (idvar(ivar) == 13) then !wind speed (WSPD)
!                                       convert from m/s to knots then back to m/s for consistency with current AERMET
                                        if (rtemp < 100.0_r8) then
                                            rtemp=real(nint(rtemp/0.51444_r8),r8)
                                            sam_obs(9)=rtemp*0.51444_r8
                                        endif
                                    endif !wind speed (WSPD)
                                    if (idvar(ivar) == 14) then !horizontal visibility (HZVS)
                                        if (rtemp < 99990._r8) sam_obs(11)=rtemp*10.0_r8
                                    endif !wind speed (WSPD)
                                    if (idvar(ivar) == 15) then !ceiling height (CLHT)
                                        if (rtemp > 77776._r8 .and. rtemp < 77778.0_r8) then
                                            sam_obs(10)=300
                                        elseif (rtemp < 99990._r8) then
                                            sam_obs(10)=real(nint(rtemp/100.0_r8),r8)
                                        endif
                                    endif !ceiling height (CLHT)
                                endif
                            enddo v3
!                           calculate TSKC
                            sam_obs(3)=real((sky(1)*100+sky(2)),r8)

!                           calculate station pressure if not available but have elevation
                            if ((have_elev .or. user_elev) .and. .not. gotpress) &
                                sam_obs(2)=1013.25_r8*(1.0_r8-((6.5e-3_r8/288.15_r8)*sfelev))**5.255_r8
                        
!                           calms
                            if (sam_obs(8) == 0 .and. sam_obs(9) == 0) nsfc(11)=nsfc(11)+1
!**************         process weather variables ******************************
!                           write the data
                            if (nomisshr(ihr,iday1)) then
                                call check_dup(iday1,ihr,sam_obs(1),sam_obs(5),sam_obs(8),sam_obs(9),miss_precip,miss_temp,&
                                    miss_dir,miss_wind,writedat)
!                               write message that data is being overwritten
                                if (writedat) write(msg_unit,ext_form(5))adjustl(pathid(ipath)),'I45',modnam,&
                                    'OBSERVATION REPLACED FOR',date_hr_str
                            else !write to array; first occurrence of date/hour
                                if (.not. lsfcobs(iday1))lsfcobs(iday1)=.true.
                                
                                nomisshr(ihr,iday1)=.true.
                                writedat=.true.
!                               increment # of obs for the day; don't worry about replacing with future obs because this means the hour has an obs regardless of replacement.
                                nsfc1(iday1)=nsfc1(iday1)+1
!                               only increment nsfc(2) when not overwriting, since it has already been incremented when first observation for hour written                        
                              
                                nsfc(2)=nsfc(2)+1
                            endif
                            if (writedat) then
!                               core variables
!                               note ASKY not in data
                                sfdata1(1,ihr,iday1)=sam_obs(1)
    v4:                         do ivar=2,9
                                    if (ivar > 1 .and. ivar <=4) then
                                        sfdata1(ivar+1,ihr,iday1)=sam_obs(ivar)
                                    else
                                        sfdata1(ivar+2,ihr,iday1)=sam_obs(ivar)
                                    endif
                                enddo v4
!                               if ceiling height or visbility audited, write
                                if (sfvars(12)%laudit .or. sfvars(21)%laudit) then
                                    ivar=0
    v5:                             do svar=nsfvars_keep+1,nsfvars
                                        if (sfvars(svar)%laudit) ivar=ivar+1
                                        if ((svar == 12 .or. svar == 21) .and. sfvars(svar)%laudit) then
                                            if (svar == 12) sfdata1(ivar+nsfvars_keep,ihr,iday1)=sam_obs(10) !ceiling
                                            if (svar == 21) sfdata1(ivar+nsfvars_keep,ihr,iday1)=sam_obs(11) !visibility
                                        endif
                                    enddo v5
                                endif  
                            endif                        
!                           write the data
                            savedate=sdate
                            savehr=ihr
                        endif !end data processing within window
                    endif
                endif
            endif
        endif
    enddo
    
    if (.not. lbadsfc .and. lbad)lbadsfc=.true.
    
    end subroutine read_sam
!*********************************************************************************************************
 
    subroutine read_huswo
!=========================================================================================================
!   SUBROUTINE READ_HUSWO
!   THIS SUBROUTINE READS HUSWO FORMAT NWS DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!       
!   CALLED BY:      MODULE SURFACE (SF_PROC)
!
!   Integer variables
!   eof:            integer end of file indicator to read data file
!   j:              looping variable
!   k:              looping variable
!   irec:           record counter
!   iflag1:         I/O flag used for reading variables from file
!   sky:            2-element arry of sky cover used for TSKC
!   svar:           integer indicating array index in sfdata1
!   ifield:         field counter for 2nd header row
!   lastfield:      number of fields read from 2nd header row
!   ivar1:          variable counter
!   iliq:           liquid precip indicator
!   ifrzn:          frozen precip indicator
!   itemp:          integer variable read from data
!   nip:            2-element weather indicator as number
!   acc:            3-element cloud cover indicator
!   maxcld:         max cloud cover indicator
!   iccvr:          cloud cover
!   iceil:          ceiling height
!   acht:           3-element ceiling height variable
!   iwx:            weather indicator
!   maxvar:         maximum number of variables read from data file
!   linelen1:       length of data line
!   ind:            index locations of column header on 2nd row
!   idvar:          header designations for each column
!   ivar:           variable counter
!   iday1:          counter of number of days in data period for arrays
!   ihr:            hour of day
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!
!   Real variables
!   huswo_obs:        11-element array of variables read from SAMSON format data file
!   rtemp:          real variable read from data file for real variables
!
!   Logical variables
!   lfound:         logical variable denoting station found in ASOS list
!   asoshr1:        dummy logical variable used in subroutine check_asos
!   lgo:            logical variable denoting if date of record within data window
!   miss_wind:      denotes if speed is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_dir:      denotes if direction is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_temp:      denotes if temperature is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_precip:    denotes if precip is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   writedat:       denotes to write data to sfdata1
!
!   Character variables
!   date_hr_str:    date and hour text string for error messages
!   hdr_vars:       header variables
!   formstr:        format for messages unique to read_huswo
!   modnam:         Subroutine name
!   datline:        data line read from SAMSON file
!   pflag:          precip flag
!   aflag:          ASOS flag
!   huswo_vars:       11-element character array of data variables
!   datform:        format statement for reading datline from file
!   vfmt:           formats to read column headers
!   pwth:           present weather 
!   ip:             2-element weather indicator as character string
!
!   huswo_obs follow sfvars order through CLHT.  ACHT and HZVS differ. HUSWO order is:
!   13:  ACHT
!   14:  HZVS
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit
    use main1, only: getfields
    use file_units, only: sf_data_unit
    implicit none
    integer(kind=4), parameter :: maxvar=22
    integer(kind=4), parameter :: linelen1=200
    integer(kind=4) :: eof,j,k,iflag1,sky(2),svar,ifield,lastfield,ivar1,iliq,ifrzn,itemp,iday1,&
        nip(2),acc(3),maxcld,iccvr,iceil,acht(3),iwx,irec,ivar,ihr,iyear,imonth,iday,savehr
    integer(kind=8) :: savedate=0
    integer(kind=8) :: sdate
    integer(kind=4) :: ind(maxvar),idvar(maxvar)
    real(kind=r8) :: huswo_obs(14),rtemp
    logical :: lfound,asoshr1,lgo,miss_wind,miss_dir,miss_temp,miss_precip,writedat
    character(len=1) :: pflag,aflag,ip(2)
    character(len=8) :: pwth
    character(len=12) :: huswo_vars(maxvar),datform
    character(len=180) :: huswo_form
    character(len=linelen1) :: datline 
    character(len=10) :: vfmt(20)
    character(len=40) :: hdr_vars(maxvar)
    character(len=20) :: date_hr_str
    character(len=60) :: formstr
    character(len=10) :: modnam='READ_HUSWO'
    
    data vfmt /'a5',',a11',',1x,a4',',1x,a4',',1x,a2',',a2',',1x,a5,1x',',1x,a5',',1x,a3',',1x,a4,1x',',1x,a3',',1x,a4',&
    ',1x,a6',',1x,a5',',1x,a8',',1x,a5',',1x,a5',',1x,a5,1x',',1x,a4',',1x,a3'/
    
    sky=-99
    sdate=0
    writedat=.false.
    miss_wind=.false.
    miss_dir=.false.
    miss_temp=.false.
    miss_precip=.false.
    asoshr1=.false.
    huswo_vars=''
    ind=0
    lastfield=0
    idvar=0
    pflag=''
    iliq=0
    ifrzn=0
    aflag=''
    irec=0
!   format for invalid ASOS cloud cover
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,i5,1x,a,1x,a20)'
    
!   initialize huswo_obs to missing
!   order of 1st 12 variables same as sfvars
    v1: do ivar=1,12
        huswo_obs(ivar)=real(sfvars(ivar)%missval,r8)
    enddo v1
    
!   ACHT and HZVS
    v2: do ivar=20,21
        huswo_obs(ivar-7)=real(sfvars(ivar)%missval,r8)
    enddo v2

    
!   initial value of sam_form (format statement to read data)
    huswo_form='(a5,a11'
    
!   read the data file
    eof=0
    write(datform,'(a1,i3,a2)')'(',linelen1,'a)'
    do while (eof == 0 .and. .not. lbad .and. sdate <=sfend)
        read(sf_data_unit,datform,iostat=eof)datline
        if (eof == 0) then
            irec=irec+1 !record counter
!**************   headers ****************************************            
            if (irec == 1) then !variable numbers; based on getfields subroutine
!               initialize
                ind=0
                j=0
                ifield=0
!               get number of fields and location of first non-blank character of the individual fields
    f1:         do j=2,len_trim(datline)
                    if (ichar(datline(j-1:j-1)) == 32 .and. ichar(datline(j:j)) /= 32) then                     
                        ifield=ifield+1
                        ind(ifield)=j
                        lastfield=ifield
                    endif
                enddo  f1 
      
!               fill in fields
    f2:         do ifield=1,lastfield
                    if (ifield /= lastfield) then
                        read(datline(ind(ifield):ind(ifield+1)-1),'(a)')hdr_vars(ifield)
                    else
                        read(datline(ind(ifield):len_trim(datline)),'(a)')hdr_vars(ifield)      
                    endif
                enddo f2

!               create format statement    
!               skip first 2 variables (year, month, day, hour, indicator)
    v3:         do ivar=3,lastfield
                    read(hdr_vars(ivar),*,iostat=iflag1)ivar1
                    if (iflag1 == 0) then
                        huswo_form=trim(adjustl(huswo_form))//trim(adjustl(vfmt(ivar1)))
                        idvar(ivar)=ivar1
                    else
                        write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E45',modnam,'INVALID HUSWO HEADER',&
                            trim(adjustl(hdr_vars(ivar)))
                        lbad=.true.
                    endif
                enddo v3

!               put final parenthesis
                huswo_form=trim(adjustl(huswo_form))//')'

!**************   headers ****************************************
            else
!               initialize huswo_obs to missing
    v4:         do ivar=1,12
                    huswo_obs(ivar)=real(sfvars(ivar)%missval,r8)
                enddo v4
    v5:         do ivar=20,21
                    huswo_obs(ivar-7)=real(sfvars(ivar)%missval,r8)
                enddo v5
                sky=99
!               read data
            
                read(datline,huswo_form)(huswo_vars(ivar),ivar=1,lastfield)

!**************   station ID  ****************************************
                read(datline(1:5),'(i5)',iostat=iflag1)iwban
                if (iflag1 == 0) then
                    if (iwban /= isfid .and. iwban /= 99999) then !no match
                        write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'W40',modnam,'STATION ID',iwban,&
                            'DOES NOT MATCH USER ENTERED ID',trim(adjustl(sfid))
!                       check ASOS status based on iwban
                        call check_asos(isfid,sdate,0,.true.,.false.,.false.,lasos,asoshr1)
                        if (lasos) write(msg_unit,ext_form(4))adjustl(pathid(ipath)),'W53',modnam,'STATION',datline(1:5),&
                            'FOUND IN ASOS LIST BASED ON STATION IDENTIFIERS IN SURFACE FILE'
                    endif
                else
                    write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E47',modnam,'INVALID STATION ID FIELD:',&
                        trim(adjustl(huswo_vars(1)))
                    lbad=.true.
                endif  
!**************   station ID  ****************************************
!               date and hour 
!               years are 4 digit so don't have to worry about century crossover
                read(datline(7:16),'(i4,3(i2))',iostat=iflag1)iyear,imonth,iday,ihr
                if (iflag1 == 0) then
!                   new obs
                    call new_obs(iyear,imonth,iday,ihr,savedate,savehr,lgo,iday1,sdate)
                    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
                    if (lgo) then !proceed
                                           
!**************         check for ASOS status for date ******************************
                        read(datline(6:6),'(a1)')aflag
                        
                        if (trim(adjustl(aflag)) == 'A' ) then !possible ASOS obs
                            call check_asos(iwban,sdate,ihr,.false.,.true.,.true.,lfound,asoshr(ihr,iday1))
                        else
                            call check_asos(iwban,sdate,ihr,.false.,.true.,.false.,lfound,asoshr(ihr,iday1))
                        endif
!**************         check for ASOS status ******************************                    
!**************         process variables ***********************************
!                       not all variables are used but they will be read and
!                       checked to make sure nothing wrong with file
    v6:                 do ivar=3,lastfield
                            if (idvar(ivar) > 4) then
                                if (idvar(ivar) <= 6) then !cloud cover 5=total 6=opaque
                                    read(huswo_vars(ivar),'(i2)')sky(ivar-4)
                                
                                elseif (idvar(ivar) <= 8) then !7=TMPD,8=DPTP
                                    read(huswo_vars(ivar),'(f5.0)')rtemp
                                    if (rtemp < 900.0_r8) huswo_obs(ivar)=rtemp
                                
                                elseif (idvar(ivar) == 9) then !RHUM
                                    read(huswo_vars(ivar),'(i3)')itemp
                                
                                    if (itemp < 999) huswo_obs(ivar)=real(itemp,r8)
                                
                                elseif (idvar(ivar) == 10) then !station pressure
                                    read(huswo_vars(ivar),'(i4)')itemp
                                    if (itemp /= 9999) huswo_obs(3)=real(itemp,r8)
                            
                                elseif (idvar(ivar) == 11) then !WDIR
                                    read(huswo_vars(ivar),'(i3)')itemp
                                
                                    if (itemp < 999) huswo_obs(10)=real(itemp,r8)
                                
                                elseif (idvar(ivar) == 12) then !wind speed
                                    read(huswo_vars(ivar),'(f4.0)')rtemp
                                    if (rtemp < 900._r8) huswo_obs(11)=rtemp
                                
                                elseif (idvar(ivar) == 13) then !visibility
                                    read(huswo_vars(ivar),'(f6.0)')rtemp
                                    if (rtemp < 9999._r8) huswo_obs(14)=rtemp*10._r8
                                
                                elseif (idvar(ivar) == 14) then !ceiling
                                    iliq=0
                                    ifrzn=0
                                    read(huswo_vars(ivar),'(i5)')itemp  
                                    if (itemp /= 999) then
                                        if (itemp == 77777) then
                                            huswo_obs(12)=300._r8
                                        else
                                            huswo_obs(12)=real(nint(real(itemp,r8)/100._r8))
                                        endif
                                    endif
                            
                                 elseif (idvar(ivar) == 15) then !PWTH
                                     read(huswo_vars(ivar),'(a8)')pwth
                                     if (trim(adjustl(pwth)) == '') then
                                         iwx=9
                                     elseif (trim(adjustl(pwth)) =='99999999') then
                                         iwx=8
                                     elseif (trim(adjustl(pwth)) =='00009999' .or. trim(adjustl(pwth)) == '00000000') then
                                        iwx=0
                                     else
                                        nip=0
                                    
    l2:                                 do j=1,7,2
                                            ip(1)=pwth(j:j)
                                            ip(2)=pwth(j+1:j+1)
    l3:                                     do k=1,2
                                                read(ip(k),'(i1)')nip(k)
                                            enddo l3
                                            if (nip(1) /= 0) then
                                                if (nip(1) == 2 .or. nip(1) == 3) then
                                                    if (nip(2) /= 9) then !liquid
                                                        iliq=1
                                                    endif
                                                elseif ((nip(1) >= 4) .and. (nip(1) <= 6) .or. (nip(1) == 9)) then
                                                    if (nip(2) /= 9) then !frozen
                                                        ifrzn=2
                                                    endif
                                                endif
                                            endif
                                        enddo l2
                                        iwx=iliq+ifrzn
                                     endif
                                     huswo_obs(5)=real(iwx,r8)
                                     
       
    
                                 elseif(idvar(ivar) == 19) then !precip
                                     read(huswo_vars(ivar)(4:4),'(a1)')pflag
                                     if (trim(adjustl(pflag))=='') then
                                        read(huswo_vars(ivar)(1:3),*,iostat=iflag1)rtemp
                                        huswo_obs(1)=rtemp
                                     endif
                                endif
                            endif
                        enddo v6
!                       calculate TSKC
                        huswo_obs(4)=real(sky(1)*100+sky(2),r8)
                    
!                       calculate ASKY and ACHT
                       
                        if (asoshr(ihr,iday1)) then
                            acc=0
                            maxcld=0
                            iccvr=0
                            iceil=77777
                            acht=0
!                           process cloud cover/ceiling height codes
!                           first two digits are cloud cover and last
!                           three are ceiling height; 99999 is missing data
    l4:                     do j=1,3
                                if (trim(adjustl(huswo_vars(j+15))) /= '99999') read(huswo_vars(j+15),'(i2,i3)')acc(j),acht(j)
                                if (j == 1) then
                                    maxcld=acc(j)
                                    if (maxcld > 2) iceil=acht(j)
                                else
                                    if (acc(j) /= 99 .and. acc(j) > maxcld) then
                                        maxcld=acc(j)
                                        if (maxcld > 2 .and. iceil /= 77777) iceil=acht(j)
                                    endif
                                endif
                            enddo l4
                        
!                           cloud cover
                            if (maxcld == 0) then !clear
                                iccvr=0
                            elseif (maxcld == 2) then !scattered
                                iccvr=3
                            elseif (maxcld == 4) then !broken
                                iccvr=7
                            elseif (maxcld == 6 .or. maxcld == 7) then !overcast (6), obstruction (7)
                                iccvr=10
                            elseif (maxcld == 9 .or. maxcld == 99) then !unknown (9) or missing (99)
                                iccvr=99
                            else
                                iccvr=99
                                write(msg_unit,formstr)adjustl(pathid(ipath)),'W47',modnam,'INVALID ASOS CLOUD COVER',maxcld,&
                                    'FOR',date_hr_str
                            endif
                            huswo_obs(6)=real(iccvr,r8)
                            huswo_obs(13)=real(iceil,r8)
                        endif
!                       calculate ASKY and ACHT  

!                       check for calm condition
                        if (huswo_obs(10) == 0.0_r8 .and. huswo_obs(11) == 0.0_r8)nsfc(11)=nsfc(11)+1
!**************         process variables ***********************************                    
!                       write the data

                        if (nomisshr(ihr,iday1)) then
                            call check_dup(iday1,ihr,huswo_obs(1),huswo_obs(7),huswo_obs(10),huswo_obs(11),miss_precip,miss_temp,&
                                miss_dir,miss_wind,writedat)
                            
!                           write message that data is being overwritten
                            if (writedat) write(msg_unit,ext_form(5))adjustl(pathid(ipath)),'I45',modnam,&
                                'OBSERVATION REPLACED FOR',date_hr_str
                        else !write to array; first occurrence of date/hour
                            if (.not. lsfcobs(iday1))lsfcobs(iday1)=.true.
                            
                            nomisshr(ihr,iday1)=.true.
                            writedat=.true.
!                           increment # of obs for the day; don't worry about replacing with future obs because this means the hour has an obs regardless of replacement.
                            nsfc1(iday1)=nsfc1(iday1)+1
!                           only increment nsfc(2) when not overwriting, since it has already been incremented when first observation for hour written                        
                            
                            nsfc(2)=nsfc(2)+1 
                        endif
                        if (writedat) then
!                           core variables

    v7:                     do ivar=1,11
                                sfdata1(ivar,ihr,iday1)=huswo_obs(ivar)
                            enddo  v7
!                           if ceiling height or visbility audited, write
                            if (sfvars(12)%laudit .or. sfvars(20)%laudit .or. sfvars(21)%laudit) then
                                ivar=0
    v8:                         do svar=nsfvars_keep+1,nsfvars
                                    if (sfvars(svar)%laudit) ivar=ivar+1
                                    if ((svar == 12 .or. svar == 21) .and. sfvars(svar)%laudit) then
                                        if (svar == 12) sfdata1(ivar+nsfvars_keep,ihr,iday1)=huswo_obs(12) !CLHT
                                        if (svar == 20 .or. svar == 21) sfdata1(ivar+nsfvars_keep,ihr,iday1)=huswo_obs(svar-7) !ACHT or HZVS
                                    endif
                                enddo v8
                            endif  
                        endif                        
!                       write the data
                        savedate=sdate
                        savehr=ihr  
                    
                    endif
                else
                    lbad=.true.
                    write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E42',modnam,'INVALID DATE FIELD:',&
                        trim(adjustl(huswo_vars(2)))
                endif
            endif !end irec check
        endif
    enddo
        
    if (.not. lbadsfc .and. lbad)lbadsfc=.true.
    
    return
    end subroutine read_huswo
!*********************************************************************************************************
      
    subroutine read_ext
!=========================================================================================================
!   SUBROUTINE READ_EXT
!   THIS SUBROUTINE READS THE EXTRACT OR QAOUT FILE
!
!   MODIFIED MAY 5, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (SF_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   eof:            end of file indicator
!   i:              loop counter
!   ifile:          file unit to read data from
!   dates1:         array of start and end date read from file header
!   gmt2lst:        GMT to LST conversion read from file header
!   ivar:           variable counter
!   iwban:          WBAN number
!   iwarn:          number of warnings that station is ASOS but not in ASOS list
!   irec:           record counter
!   i1:             loop counter for reading inpline1
!   iday1:          counter of number of days in data period for arrays
!   iflag:          I/O indicator when reading linetype
!   ihr:            hour of day
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!   savehr:         previous observation's hour
!   sdate:          integer observation date (YYYYMMDD)
!   savedate:       previous observation's date (YYYYMMDD)
!     
!   Real variables
!   rsfvars:		values read from data lines
!
!   Logical variables
!   lfound:         logical variable denoting station found in ASOS list
!   asoshr1:        dummy logical variable used in subroutine check_asos
!   lgo:            logical variable denoting if date of record within data window
!   miss_wind:      denotes if speed is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_dir:      denotes if direction is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_temp:      denotes if temperature is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_precip:    denotes if precip is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   writedat:       denotes to write data to sfdata1
!     
!   Character variables
!   adate:          character value of sdate
!   date_hr_str:    date and hour text string for error messages
!   formstr:        format for messages unique to read_ext
!   j1-j3:          dummy variables to read in from file (not used)
!   asosflag:       ASOS flag in data line
!   form1:          format for reading inpline1
!   lower:          string of lowercase letters
!   upper:          string of uppercase letters
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit
    use main1, only: inpline1,nfield,ilen,getdates,linelen,getloc
    use file_units, only: sf_data_unit,sf_qaout_unit,sf_extract_unit
    implicit none
    integer(kind=4) :: i,eof,ifile,dates1(2,3),gmt2lst,iwban,i1,irec,ivar,iday1,iflag,ihr,iyear,imonth,iday,savehr,iwarn
    integer(kind=8) :: savedate=0
    integer(kind=8) :: sdate
    real(kind=r8), allocatable, dimension(:) :: rsfvars
    logical :: lfound,asoshr1,lgo,miss_wind,miss_dir,miss_temp,miss_precip,writedat
    !character(len=220) :: datline
    character(len=8) :: j1,j2,j3,adate
    character(len=26) :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=26) :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=1) :: asosflag
    character(len=6) :: form1
    character(len=20) :: date_hr_str
    character(len=80) :: formstr(3)
    character(len=10) :: modnam='READ_EXT'
    
    allocate(rsfvars(nsfvars_keep))
    
!   format for messages
    
!   1.  invalid ASOS flag
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a20)'
    
!   2.  station not in ASOS list
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),a)'
    
!   3. station is ASOS for hour but not in list
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20,1x,a)'
    
    write(form1,'(a2,i3,a1)')'(a',linelen,')'   
!   initialize
    i=0
    iwarn=0
    eof=0
    ifile=0
    sdate=0
!    iupid=0
    dates1=0
    gmt2lst=0
    lfound=.false.
    asoshr1=.false.
    irec=0
      
    rsfvars=0.0_r8
      
!   determine which file to read
!   if stage 1 and EXTRACT is DATA keyword format use sf_data_unit
!   if stage 1 and EXTRACT and no DATA keyword use  sf_extract_unit     
!   if stage 2 only, use QAOUT regardless
!   if EXTRACT is specified
      
    if (sfstage1) then
        if (lsfkeywords(1)) then
            ifile=sf_data_unit
        else
            ifile=sf_extract_unit
        endif
    else
        ifile=sf_qaout_unit
    endif

          
!   read the file to make sure correct format, get array bounds and fill in temporary arrays
    do while (eof == 0 .and. .not. lbad)
        read(ifile,form1,iostat=eof)inpline1
        ilen=len_trim(inpline1)
        if (eof == 0) then
    l1:     do i1=1,len_trim(inpline1)
                i=index(lower,inpline1(i1:i1))
                if (i > 0) inpline1(i1:i1)=upper(i:i)
            enddo l1
            irec=irec+1
!           if first line, should be location, check station ID against 
!           what is entered in the runstream file
!           if different, issue warning
            if (irec == 2) then
                nfield=0
!               get the number of fields because it can vary depending on if GMT to LST or elevation included
    l2:         do i=1,len_trim(inpline1)
                    if (ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32) nfield=nfield+1
                enddo l2
                i=index(inpline1,'LOCATION')
                call getloc(i+8,sfid,sflat,sflon,sfgmt2lst,sfelev,lgmt2lst,lsfelev)
                read(sfid,*)iwban
!               save the original GMT TO LST because the data is already in local
!               reset sfgm2lst to 0
                gmt2lst=sfgmt2lst 
                sfgmt2lst=0
!               station doesn't match user entered ID
                if (iwban /= isfid)write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'W40',modnam,'STATION ID ',iwban,&
                        'DOES NOT MATCH USER ENTERED ID',trim(adjustl(sfid))
                
!               check to see if station is an ASOS station
                call check_asos(iwban,sdate,0,.true.,.false.,.false.,lasos,asoshr1)
                if (.not. lasos) write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W54',modnam,'STATION',trim(adjustl(sfid)),&
                    'NOT LISTED IN ASOS LIST; CHECK STATION ID'
            elseif (irec == 3) then 
!               check file type, should be EXTRACT or QAOUT
!               if line is incorrect, issue error
                read(inpline1,*)j1,j2,j3
                if (trim(adjustl(j1)) /= 'FILE' .or. trim(adjustl(j2))/= 'TYPE:' .or. (trim(adjustl(j3)) /= 'EXTRACT' .and. &
                    trim(adjustl(j3)) /= 'QAOUT')) then
                    write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E40',modnam,'INCORRECT FILE TYPE LINE; LINE',&
                        trim(adjustl(inpline1))
                    lbad=.true.
                endif
            elseif (irec == 4) then
!               check the dates, make sure are numeric
                read(inpline1,*,iostat=iflag)j1,dates1(1,2),dates1(1,3),dates1(1,1),dates1(2,2),dates1(2,3),dates1(2,1)
                if (iflag /= 0) then
                    write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E40',modnam,&
                        'ERROR READING EXTRACTION DATES INFORMATION; LINE',trim(adjustl(inpline1))
                    lbad=.true.
                endif
            elseif (irec > 5 .and. index(inpline1,'RANGE') == 0 .and. index(inpline1,'DATE ') == 0) then !data record  
!               actual data
                read(inpline1,*,iostat=iflag)sdate,ihr,asosflag,(rsfvars(ivar),ivar=1,nsfvars_keep)
                write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
                
                if (iflag == 0) then
!					check to see if obs is within data window
!					need to get year, month, and day from sdate
                    write(adate,'(i8)')sdate
                    read(adate,'(i4,2(i2))')iyear,imonth,iday
                    call new_obs(iyear,imonth,iday,ihr,savedate,savehr,lgo,iday1,sdate)
                    irec=irec-1  !subtract 1 from irec because it is incremented at the read and in new_obs
                    if (lgo) then 
!						check ASOS status for the hour
                        if (asosflag=='Y') then
                            asoshr(ihr,iday1)=.true.
                            if (.not. lasos) then
                                iwarn=iwarn+1
                                if (iwarn <= nwarnlim)write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W54',modnam,&
                                    'STATION IS ASOS FOR',date_hr_str,'BUT NOT IN ASOS LIST OR NOT ASOS FOR DATE'
                                if (iwarn == nwarnlim) write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'W42',modnam,&
                                    'REACH LIMIT OF',nwarnlim,'W54','WARNING MESSAGES'
                            endif
                            !call check_asos(iwban,sdate,ihr,.false.,.true.,.true.,lfound,asoshr(ihr,iday1))
                        elseif (asosflag == 'N') then 
                            !call check_asos(iwban,sdate,ihr,.false.,.true.,.true.,lfound,asoshr(ihr,iday1))
                            call check_asos(iwban,sdate,ihr,.false.,.true.,.false.,lfound,asoshr(ihr,iday1))
                        else
                            write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E40',modnam,'INVALID ASOS FLAG',&
                                trim(adjustl(asosflag)),' FOR',date_hr_str
                            lbad=.true.
                        endif
                        if (nomisshr(ihr,iday1)) then
                            call check_dup(iday1,ihr,rsfvars(1),rsfvars(7),rsfvars(10),rsfvars(11),miss_precip,miss_temp,&
                                miss_dir,miss_wind,writedat)
                            if (writedat) write(msg_unit,ext_form(5))adjustl(pathid(ipath)),'I45',modnam,&
                                'OBSERVATION REPLACED FOR',date_hr_str
                        else
                            if (.not. lsfcobs(iday1))lsfcobs(iday1)=.true.
                            
                            nomisshr(ihr,iday1)=.true.
                            writedat=.true.
!							increment # of obs for the day; don't worry about replacing with future obs because this means the hour has an obs regardless of replacement.
                            nsfc1(iday1)=nsfc1(iday1)+1
!							only increment nsfc(2) when not overwriting, since it has already been incremented when first observation for hour written                        
                            
                            nsfc(2)=nsfc(2)+1
                        endif
                        
!						write data to temporary array
                        if (writedat) then
!                           if wind direction and speed both 0, flag as calm
                            if (rsfvars(10) == 0.0_r8 .and. rsfvars(11) == 0.0_r8) nsfc(11)=nsfc(11)+1
    l3:						do ivar=1,nsfvars_keep
                                sfdata1(ivar,ihr,iday1)=rsfvars(ivar)
                            enddo l3
                        endif
                        savedate=sdate
                        savehr=ihr
                    endif
                else
                    write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E40',modnam,'ERROR READING DATA; LINE',&
                        trim(adjustl(inpline1))
                    lbad=.true.
                endif
            endif
        endif
    enddo
!   reset sfgmt2lst back to its original value
    sfgmt2lst=gmt2lst
    if (.not. lbadsfc .and. lbad)lbadsfc=.true.
      
    if (allocated(rsfvars)) deallocate(rsfvars)
      
    return
    end subroutine read_ext
!*********************************************************************************************************
      
    subroutine check_asos(i_id,sdate,ihr,checkstat,checkhr,aflag,lfound,asoshr1)
!=========================================================================================================
!   SUBROUTINE CHECK_ASOS
!   THIS SUBROUTINE CHECKS THE ASOS STATUS OF A STATION AND SETS THE ASOS STATUS OR ASOSHR STATUS
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (SF_TEST, READ_ISHD, READ_CD144, READ_SCRAM, READ_SAM, READ_HUSWO,
!                                   READ_EXT, SF_STAGE2)
!
!   INPUT ARGUMENTS:
!   
!   I_ID:           STATION ID
!   SDATE:          DATE OF OBSERVATION (YYYYMMDD) FOR USE WHEN CHECKHR IS TRUE
!   IHR:            HOUR OF DAY FOR USE WHEN CHECKHR IS TRUE
!   CHECKSTAT:      LOGICAL CHECK TO SEE IF THE STATION IS IN THE ASOS LIST
!   CHECKHR:        LOGICAL CHECK TO SEE IF THE STATION IS ASOS FOR THE GIVEN HOUR
!   AFLAG:          LOGICAL FLAG INDICATING AN ASOS DATA FLAG WAS PRESENT IN DATA
!
!   INPUT/OUTPUT ARGUMENTS:
!   
!   LFOUND:         LOGICAL VARIABLE DENOTING STATION FOUND IN ASOS LIST
!   ASOSHR1:		OBSERVATION IS AN ASOS OBSERVATION FOR THE HOUR
!                   THIS IS ONLY USED WHEN CHECKHR is .TRUE.
!                   OTHERWISE WILL JUST BE A DUMMY ARGUMENT
!
!   Variable definitions
!   
!   Integer variables
!   i_id:           station id
!   ihr:            hour of day for use when checkhr is true
!   sdate:          date of observation (yyyymmdd) for use when checkhr is true
!
!   Logical variables
!   checkstat:      logical check to see if the station is in the asos list
!   checkhr:        logical check to see if the station is asos for the given hour
!   lfound:         logical variable denoting station found in asos list
!   aflag:          logical flag indicating an asos data flag was present in data
!   asoshr1:		observation is an asos observation for the hour
!
!   Character variables
!   formstr:        format for writing messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    implicit none
    integer(kind=4),intent(in) :: i_id,ihr
    integer(kind=8), intent(in) :: sdate
    logical, intent(in):: aflag,checkstat,checkhr
    logical, intent(inout):: lfound,asoshr1
    character(len=60) :: formstr(2)
    character(len=10) :: modnam='CHECK_ASOS'
 
    asoshr1=.false.
    
!   1.  ASOS status
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,i8)'
    
!   2.  data source says ASOS but before commission date
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'2(a,1x,i8,1x),a,1x,i2.2)'

!   check to see if the station ID is in the ASOS list
!   this is either the station ID from the control file or
!   from the met data file
    if (checkstat .and. i_id /= 99999) then
        iasos=1
        lfound=.false.
        do while (iasos <= nasos .and. .not. lfound)
            if (i_id == asoscommdates(iasos)%iwban) then
                lfound=.true.
            else
                iasos=iasos+1
            endif
        enddo
    endif
    
    if (checkstat .and. .not. lfound) iasos=0
!   check the current observation
    if (checkhr) then
        if (lasos) then
            if (iasos /=0) then
                if (sdate < asoscommdates(iasos)%commisdate .and. .not. write_asos) then
                    if (.not. aflag) then
                        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W45',modnam,&
                            'STATION IS ASOS, BUT BEFORE COMMISSION DATE',asoscommdates(iasos)%commisdate,'FOR DATE:',sdate
                        write_asos=.true.
                        switch_asos(1)=.true.
                    else !data flag indicates ASOS observation
                        write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W45',modnam,&
                            'DATA SOURCE FLAG = ASOS, BUT BEFORE COMMISSION DATE',asoscommdates(iasos)%commisdate,&
                            'SET OBS=ASOS FOR DATE:',sdate,'HR',ihr
                       
                        asoshr1=.true.
                    endif
                else
!                   if after the commission date, set ASOS status to true for the hour
                    if (sdate >= asoscommdates(iasos)%commisdate) asoshr1=.true. !asoshr(iday1,ihr)=.true.
!                   if switch before and after ASOS commission date, notify user
                    if (switch_asos(1) .and. .not. switch_asos(2) .and. asoshr1) then
                        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'W45',modnam,&
                            'STATION IS ASOS, AND AFTER COMMISSION DATE',asoscommdates(iasos)%commisdate,'FOR DATE:',sdate
                        switch_asos(2)=.true.
                    endif
                    
                endif
            else
            
                asoshr1=.true.
            endif
        endif
    endif
    
    return
    end subroutine check_asos
!*********************************************************************************************************
      
    subroutine new_obs(iyear,imonth,iday,ihr,savedate,savehr,lgo,iday1,sdate)
!=========================================================================================================
!   SUBROUTINE NEW_OBS
!   THIS SUBROUTINE INCREMENTS/INITIALIZES VARIABLES AND CHECKS FOR DUPLICATES FOR NEW OBSERVATION
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (READ_ISHD, READ_CD144, READ_SCRAM, READ_SAM, READ_HUSWO,
!                                   READ_EXT)
!
!   INPUT ARGUMENTS:
!
!   SAVEDATE:       PREVIOUS OBSERVATIONS DATE (YYYYMMDD)
!   
!   INPUTS/OUTPUT ARGUMENTS:
!
!   IYEAR:          4-DIGIT YEAR OBSERVATION IN LST
!   IMONTH:         MONTH OF OBSERVATION IN LST
!   IDAY:           DAY OF MONTH OF OBSERVATION IN LST
!   IHR:            HOUR OF DAY OF OBSERVATION IN LST
!   SAVEHR:         PREVIOUS RECORD'S HOUR; SAVEHR RESET TO IHR AT END OF ROUTINE
!
!   OUTPUTS ARGUMENTS:
!   
!   SDATE:          DATE OF OBSERVATION (YYYYMMDD)
!   IDAY1:          NUMBER OF DAYS SINCE SFSTART BASED ON SDATE
!   Variable definitions
!
!   Integer variables
!   iday1:          number of days since sfstart based on sdate
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!   savedate:       previous observations date (yyyymmdd)
!   sdate:          date of observation (yyyymmdd)
!   savehr:         previous record's hour; savehr reset to ihr at end of routine
!
!   Logical variables
!   lgo:            logical variable denoting if date of record within data window
!   linvalid:       denotes observation is outside valid dates for particular format
!                   valid dates set in sfc_init
!
!   Character variables
!   adate:          character string of date
!   formstr:        format for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: data_dates,numdays,noprint,istage
    implicit none
    integer(kind=4), intent(inout) :: iyear,imonth,iday,ihr,savehr
    integer(kind=8), intent(in) :: savedate
    integer(kind=8), intent(out) :: sdate
    integer(kind=4), intent(out) :: iday1
    logical,intent(out) :: lgo
    logical, save :: linvalid(2)=.false.
    
    
    character(len=8) :: adate
    character(len=60) :: formstr(4)
    character(len=10) :: modnam='NEW_OBS'
    
!   1.  message to screen to write out processing day
    write(formstr(1),'(a)')'(1x,a,1x,i1,a,1x,2(a2,a1),a4,1x,a)'

!   2.  message about duplicate
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,i2.2)'

!   3.  improper ordering of dates
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,i8)'

!   4.  data exceeds valid start/end dates
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,3(a,1x),i8)'
    
    call data_dates(ihr,iday,imonth,iyear,sfgmt2lst,sfstart,sfend,42,sdate,lgo)
    
!   check the date against the valid start/end dates even if the date from the file
!   is outside the XDATES dates
!   message is only written for first occurence, not every date that triggers logic
    if (sdate < sfformats(isfform)%validstart) then
        if (.not. linvalid(1)) write(msg_unit,formstr(4))adjustl(pathid(ipath)),'W50',modnam,'OBSERVATION',sdate,&
            'IS BEFORE VALID',trim(adjustl(sfformats(isfform)%sformat)),'START DATE',sfformats(isfform)%validstart
        linvalid(1)=.true.
    endif
    if (sdate > sfformats(isfform)%validend) then
        if (.not. linvalid(2)) write(msg_unit,formstr(4))adjustl(pathid(ipath)),'W50',modnam,'OBSERVATION',sdate,&
            'IS AFTER VALID',trim(adjustl(sfformats(isfform)%sformat)),'END DATE',sfformats(isfform)%validend
        linvalid(2)=.true.
    endif
    
    if (lgo) then
       
        nsfc(1)=nsfc(1)+1
        write(adate,'(i8.8)')sdate
        
        if (sdate == savedate) then
            if (ihr == savehr) write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I43',modnam,&
                    'POTENTIAL DUPLICATE OBSERVATION FOR DATE:',sdate,'HR',ihr
        else
            if (sdate < savedate) write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W49',modnam,'CURRENT DATE',sdate,&
                'IS PRIOR TO PREVIOUS DATE IN SURFACE FILE',savedate
            
!           write to screen the date and hour being processed if NOPRINT not specfied on JOB pathway
            if (.not. noprint) write(output_unit,formstr(1))'Stage',istage,': Extracting surface data for month/day/year',&
                adate(5:6),'/',adate(7:8),'/',adate(1:4),'LST'
            
!           new day, check nsurf against nsfc(3) before resetting nsfc(3)
            iday1=numdays(sfstart,sdate)!increment day counter for updata1
            write_asos=.false.
        endif
    endif
    
    return
    end subroutine new_obs
!*********************************************************************************************************
      
    subroutine check_dup(iday1,ihr,prcp,tmpd,wdir,wspd,miss_precip,miss_temp,miss_dir,miss_wind,writedat)
!=========================================================================================================
!   SUBROUTINE CHECK_DUP
!   THIS SUBROUTINE CHECKS TO SEE IF CURRENT OBSERVATION CAN BE OVERWRITTEN WITH DUPLICATE OBSERVATION
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (READ_ISHD, READ_CD144, READ_SCRAM, READ_SAM, READ_HUSWO,
!                                   READ_EXT)
!
!   INPUT ARGUMENTS
!   
!   IDAY1:          COUNTER OF NUMBER OF DAYS IN DATA PERIOD FOR ARRAYS
!   IHR:            HOUR OF DAY
!   PRCP:           PRECIP READ FROM DATA FILE
!   TMPD:           TEMPERATURE READ FROM DATA FILE
!   WDIR:           WIND DIRECTION READ FROM DATA FILE
!   WSPD:           WIND SPEED READ FROM DATA FILE
!
!   OUTPUT ARGUMENTS:
!   MISS_WIND:      DENOTES IF SPEED IS MISSING (TRUE IF CURRENT VALUE IN SFDATA1 IS NOT
!                   MISSING AND CURRENT ISHOBS IS MISSING
!   MISS_WIND:      DENOTES IF DIRECTION IS MISSING (TRUE IF CURRENT VALUE IN SFDATA1 IS NOT
!                   MISSING AND CURRENT ISHOBS IS MISSING
!   MISS_TEMP:      DENOTES IF TEMPERATURE IS MISSING (TRUE IF CURRENT VALUE IN SFDATA1 IS NOT
!                   MISSING AND CURRENT ISHOBS IS MISSING
!   MISS_PRECIP:    DENOTES IF PRECIP IS MISSING (TRUE IF CURRENT VALUE IN SFDATA1 IS NOT
!                   MISSING AND CURRENT ISHOBS IS MISSING
!   WRITEDAT:       DENOTES TO WRITE DATA TO SFDATA1
!
!   Variable definitions
!
!   Integer variables
!   iday1:          counter of number of days in data period for arrays
!   ihr:            hour of day
!
!   Real variables
!   prcp:           precip read from data file
!   tmpd:           temperature read from data file
!   wdir:           wind direction read from data file
!   wspd:           wind speed read from data file
!
!   Logical variables
!   miss_wind:      denotes if speed is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_wind:      denotes if direction is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_temp:      denotes if temperature is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   miss_precip:    denotes if precip is missing (true if current value in sfdata1 is not
!                   missing and current ishobs is missing
!   writedat:       denotes to write data to sfdata1
!
!   Character variables
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    implicit none
    integer(kind=4), intent(in) :: iday1,ihr
    real(kind=r8), intent(in) :: prcp,tmpd,wdir,wspd
    logical,intent(out):: miss_wind,miss_dir,miss_temp,miss_precip,writedat
    character(len=10) :: modnam='CHECK_DUP'
    
!   initialize the logical variables
    miss_wind=.false.
    miss_dir=.false.
    miss_temp=.false.
    miss_precip=.false.
    writedat=.true.
    
    if (sfdata1(1,ihr,iday1) /= real(sfvars(1)%missval,r8) .and. prcp == real(sfvars(1)%missval,r8))miss_precip=.true.   
    if (sfdata1(7,ihr,iday1) /= real(sfvars(7)%missval,r8) .and. tmpd == real(sfvars(7)%missval,r8))miss_temp=.true.
    if (sfdata1(10,ihr,iday1) /= real(sfvars(10)%missval,r8) .and. wdir == real(sfvars(10)%missval,r8))miss_dir=.true.               
    if (sfdata1(11,ihr,iday1) /= real(sfvars(11)%missval,r8) .and. wspd == real(sfvars(11)%missval,r8))miss_wind=.true.

    if (miss_temp .or. miss_dir .or. miss_wind)writedat=.false.
    
    nsfc(5)=nsfc(5)+1
    if (.not. writedat) then
        nsfc(3)=nsfc(3)-1
    else !overwrite; increment nsfc(8)
        nsfc(8)=nsfc(8)+1
    endif
    
    return
    end subroutine check_dup
!*********************************************************************************************************
      
    subroutine sf_audit
!=========================================================================================================
!   SUBROUTINE SF_AUDIT
!   THIS SUBROUTINE AUDITS SURFACE DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (SF_PROC)
!
!   Variable definitions
!   
!   Integer variables
!   d:              day counter
!   h:              hour counter
!   i:              variable counter
!   i1:             variable counter
!   iv:            variable index in sfvar of variable in sf_audit_index
!   iv1:            index of variable in sfdata1
!   iv2:            counter for split variables
!   imult:          multiplier for concatenated variables
!   isplit:         split loop counter
!   parts:          2-element array of the parts of the concatenated variables
!   wxind:          location of PWTH in sf_audit_vars; used with precip QA
!   ivar:           variable counter
!
!   Logical variables
!   lsplit:         indicates concatenated variables are in audit array
!   lowviolate:     indicates variable violates lower BC
!   upviolate:      indicates variable violates upper BC
!
!   Character variables
!   adate:          character string of date
!   lstr:           2-element character array for lower BC violations
!                   1='LB' 2='<' or '<='
!   ustr:           2-element character array for upper BC violations
!                   1='UB' 2='>' or '>='
!   formstr:        format for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: noprint,eps,istage
    implicit none
    integer(kind=4) :: d,h,i,iv,i1,imult,isplit,parts(2),iv1,iv2,ivar !,wxind

    logical lsplit,lowviolate,upviolate
    character(len=80) :: formstr(8)
    character(len=8) :: adate
    character(len=10) :: modnam='SF_AUDIT'
    character(len=2) :: lstr(2),ustr(2)
    
!   allocate the audit variable array
    allocate(sf_audit_index(nsf_audit_vars))

!   1.  write extraction date to screen
    write(formstr(1),'(a)')'(1x,a,1x,i1,a,1x,2(a2,a1),a4,1x,a)'

!   2. messages about missing part of data
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,2(1x,a),1x,i1,1x,a)'

!   3. part of a variable split violates bounds
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,2(1x,a),1x,i1,1x,a2,i3,1x,a,i3)'

!   4. missing variable
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,2(1x,a))'

!   5. integer variable violates bounds
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,1x,a,1x,a2,i5,1x,a,i5)'

!   6.  real variable violates bounds
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,1x,a,1x,a2,f9.1,1x,a,f9.1)'
    
!   7. calm wind or temperature < dewpoint
    write(formstr(7),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,1x,a)'
    
!   8.  wind direction and wind speed don't match (one indicates calm, the other doesn't)
    write(formstr(8),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a,1x,i2.2,1x,a)'	

    sf_audit_index=0
    iv1=0
    iv2=0
    i=0
    i1=0
    imult=0
    lsplit=.false.
    parts=0
    lstr(1)='LB'
    lstr(2)=''
    ustr(1)='UB'
    ustr(2)=''
!   check to see if any variables that are "split" are being audited
!   fill i sf_audit_index array
    v1: do ivar=1,nsfvars
        if (sfvars(ivar)%laudit) then
            i=i+1
            sf_audit_index(i)=ivar
            if (.not. lsplit) then
                if (ivar == 4 .or. ivar == 5 .or. ivar == 19 .or. (ivar >= 13 .and. ivar <= 18)) then
                    lsplit=.true.
                    nsplit=2
                endif
            endif
        endif
    enddo v1
    
!   now assign the missing values
    allocate(missvals(nsf_audit_vars,nsplit))
    allocate(lowbound(nsf_audit_vars,nsplit))
    allocate(upbound(nsf_audit_vars,nsplit))
    
    allocate(sf_audit_counts(nsf_audit_vars,nsplit,4))
    sf_audit_counts=0

    
!   set missing values and bounds
!   some variables are split, such as TSKC which is combined total and opaque cloud cover
    v2: do ivar=1,nsf_audit_vars
        iv=sf_audit_index(ivar)
        if (sfvars(iv)%laudit) then
            if (iv == 4 .or. iv == 5 .or. iv == 19 .or. (iv >= 13 .and. iv <= 18)) then
                if (iv == 4) then
                    imult=100
                else
                    imult=1000
                endif
                missvals(ivar,1)=real((sfvars(iv)%missval/imult),r8)
                missvals(ivar,2)=real((sfvars(iv)%missval-(sfvars(iv)%missval/imult)*imult),r8)
                lowbound(ivar,1)=real((sfvars(iv)%lowbound/imult),r8)
                upbound(ivar,1)=real((sfvars(iv)%upbound/imult),r8)
                lowbound(ivar,2)=real((sfvars(iv)%lowbound-(sfvars(iv)%lowbound/imult)*imult),r8)
                upbound(ivar,2)=real((sfvars(iv)%upbound-(sfvars(iv)%upbound/imult)*imult),r8)
            else
    split1:     do isplit=1,nsplit
                    missvals(ivar,isplit)=real(sfvars(iv)%missval,r8)
                    lowbound(ivar,isplit)=real(sfvars(iv)%lowbound,r8)*sfvars(iv)%conv
                    upbound(ivar,isplit)=real(sfvars(iv)%upbound,r8)*sfvars(iv)%conv
                enddo split1
            endif
        endif
    enddo v2
    
    dayloop: do d=1,nsfdays
        if (sfc_info(d)%sobs) then !if the day has at least one observation
!           write to screen the date and hour being processed if NOPRINT not requested
            write(adate,'(i8.8)')sfc_info(d)%sfcdate
            if (.not. noprint) write(output_unit,formstr(1))'Stage',istage,': QA''ing surface data for month/day/year',&
                adate(5:6),'/',adate(7:8),'/',adate(1:4),'LST'
                
    hrloop: do h=1,24
                iv2=0 !reset iv2 to 0 for a new hour
                if (sfc_info(d)%have_obs(h)) then
!                   now check for 
    v3:             do ivar=1,nsf_audit_vars
                       iv=sf_audit_index(ivar)
                        if (iv > nsfvars_keep) then
                            iv2=iv2+1
                            iv1=iv2
                        else
                            iv1=iv
                        endif
                        
!                       check for missing values and boundary checks
                        if (iv == 4 .or. iv == 5 .or. iv == 19 .or. (iv >= 13 .and. iv <= 18)) then !concatenated variables
!                           check each part
                            if (iv == 4) then
                                imult=100
                            else
                                imult=1000
                            endif
!                           split and check each part for missing and boundary check
                            parts(1)=int(sfdata1(iv1,h,d))/imult
                            parts(2)=int(sfdata1(iv1,h,d))-parts(1)*imult
    split2:                 do isplit=1,nsplit
                                sf_audit_counts(ivar,isplit,1)=sf_audit_counts(ivar,isplit,1)+1  !increment total count
                                lowviolate=.false.
                                upviolate=.false.
!                               check for missing
                                if (dabs(real(parts(isplit),r8)-missvals(ivar,isplit)) < eps) then
                                    sf_audit_counts(ivar,isplit,2)=sf_audit_counts(ivar,isplit,2)+1
!                                   write message that variable is missing if set to write for variable
                                    if (.not. sfvars(iv)%lnomiss)write(msg_unit,formstr(2))adjustl(pathid(ipath)),'Q40',modnam,&
                                        sfc_info(d)%sfcdate,'HR',h,trim(adjustl(sfvars(iv)%varname)),'PART',isplit,'MISSING'
                                else !bound checks
                                    if (sfvars(iv)%lincbound) then !bounds are acceptable, included in the range
                                        if (real(parts(isplit),r8) < lowbound(ivar,isplit) .and. &
                                            dabs(real(parts(isplit),r8)-lowbound(ivar,isplit)) > eps)then
                                            lowviolate=.true.
                                            lstr(2) = '<'
                                        endif
                                        if (real(parts(isplit),r8) > upbound(ivar,isplit) .and. &
                                            dabs(real(parts(isplit),r8)-upbound(ivar,isplit)) > eps) then
                                            upviolate=.true.
                                            ustr(2)='>'
                                        endif
                                    else
                                        if (real(parts(isplit),r8) <= lowbound(ivar,isplit)) then
                                            lowviolate=.true.
                                            lstr(2) = '<='
                                        endif
                                        if (real(parts(isplit),r8) >= upbound(ivar,isplit)) then
                                            upviolate=.true.
                                            ustr(2)='>='
                                        endif
                                    endif
                                    if (lowviolate) then
!                                       convert numbers to integers, these variables are actually integers
                                        sf_audit_counts(ivar,isplit,3)=sf_audit_counts(ivar,isplit,3)+1
                                        
                                        write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q41',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                        trim(adjustl(sfvars(iv)%varname)),'PART',isplit,lstr(1),parts(isplit),&
                                        trim(adjustl(lstr(2))),int(lowbound(ivar,isplit))
                                        
                                    endif
                                    if (upviolate) then
!                                       convert numbers to integers, these variables are actually integers
                                        sf_audit_counts(ivar,isplit,4)=sf_audit_counts(ivar,isplit,4)+1
                                        
                                        write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q42',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                        trim(adjustl(sfvars(iv)%varname)),'PART',isplit,ustr(1),parts(isplit),&
                                        trim(adjustl(ustr(2))),int(upbound(ivar,isplit))

                                    endif
                                endif
                            enddo split2
                        else !non-concatenated variables
                            lowviolate=.false.
                            upviolate=.false.
                            sf_audit_counts(ivar,1,1)=sf_audit_counts(ivar,1,1)+1  !increment total count
                    
                            if (dabs(sfdata1(iv1,h,d)-missvals(ivar,1)) < eps) then
                                sf_audit_counts(ivar,1,2)=sf_audit_counts(ivar,1,2)+1
!                               write message that variable is missing if set to write for variable
                                
                                if (.not. sfvars(iv)%lnomiss)write(msg_unit,formstr(4))adjustl(pathid(ipath)),'Q40',modnam,&
                                    sfc_info(d)%sfcdate,'HR',h,trim(adjustl(sfvars(iv)%varname)),'MISSING'
                                
                            else
                                if (sfvars(iv)%lincbound) then !bounds are acceptable, included in the range
                                   
                                    if (sfdata1(iv1,h,d) < lowbound(ivar,1) .and. &
                                        dabs(sfdata1(iv1,h,d)-lowbound(ivar,1)) > eps) then
                                        lowviolate=.true.
                                        lstr(2) = '<'
                                    endif
                                    
                                    if (sfdata1(iv1,h,d) > upbound(ivar,1) .and. &
                                        dabs(sfdata1(iv1,h,d)-upbound(ivar,1)) > eps) then
                                        upviolate=.true.
                                        ustr(2)='>'
                                    endif
                                else
                                    if (sfdata1(iv1,h,d) <= lowbound(ivar,1)) then
                                        lowviolate=.true.
                                        lstr(2) = '<='
                                    endif
                                    if (sfdata1(iv1,h,d) >= upbound(ivar,1)) then
                                        upviolate=.true.
                                        ustr(2)='>='
                                    endif
                                endif
                                if (lowviolate) then
                                    sf_audit_counts(ivar,1,3)=sf_audit_counts(ivar,1,3)+1
                                    if (iv == 6) then !write as integers
                                        write(msg_unit,formstr(5))adjustl(pathid(ipath)),'Q41',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                        trim(adjustl(sfvars(iv)%varname)),lstr(1),int(sfdata1(iv1,h,d)),&
                                        trim(adjustl(lstr(2))),int(lowbound(ivar,1))
                                    else    
                                        write(msg_unit,formstr(6))adjustl(pathid(ipath)),'Q41',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                        trim(adjustl(sfvars(iv)%varname)),lstr(1),sfdata1(iv1,h,d),&
                                        trim(adjustl(lstr(2))),lowbound(ivar,1)
                                    endif
                                endif
                                if (upviolate) then
                                    sf_audit_counts(ivar,1,4)=sf_audit_counts(ivar,1,4)+1
                                    if (iv == 6) then !write as integers
                                        write(msg_unit,formstr(5))adjustl(pathid(ipath)),'Q42',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                        trim(adjustl(sfvars(iv)%varname)),lstr(1),int(sfdata1(iv1,h,d)),&
                                        trim(adjustl(ustr(2))),int(upbound(ivar,1))
                                    else    
                                        write(msg_unit,formstr(6))adjustl(pathid(ipath)),'Q42',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                        trim(adjustl(sfvars(iv)%varname)),lstr(1),sfdata1(iv1,h,d),&
                                        trim(adjustl(ustr(2))),upbound(ivar,1)
                                    endif
                                endif
                            endif
                        endif

                        
                    enddo v3
!                   calculate # of calms, mismatch of wind speed and direction
                    if (dabs(sfdata1(10,h,d)-real(sfvars(10)%missval,r8)) > eps .and. &
                        dabs(sfdata1(11,h,d)-real(sfvars(11)%missval,r8)) > eps) then
                        if (dabs(sfdata1(10,h,d)-0.0_r8) < eps) then
                            if (dabs(sfdata1(11,h,d)-0.0_r8) < eps) then !calm
                                sf_windstats(1)=sf_windstats(1)+1
                                write(msg_unit,formstr(7))adjustl(pathid(ipath)),'Q43',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                    'CALM WIND'
                            else !direction = 0, wind speed > 0
                                sf_windstats(2)=sf_windstats(2)+1
                                write(msg_unit,formstr(8))adjustl(pathid(ipath)),'Q45',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                    'WDIR = 0, WSPD > 0'
                                
                            endif
                        else
                            if (dabs(sfdata1(11,h,d)-0.0_r8) < eps) then !direction > 0, wind speed = 0
                                sf_windstats(3)=sf_windstats(3)+1
                                write(msg_unit,formstr(8))adjustl(pathid(ipath)),'Q44',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                    'WDIR > 0, WSPD = 0'
            
                            endif
                        endif
                    endif
!                   check for temperature < dewpoint
                    if (dabs(sfdata1(7,h,d)-real(sfvars(7)%missval,r8)) > eps .and. &
                        dabs(sfdata1(8,h,d)-real(sfvars(8)%missval,r8)) > eps) then
                        if (sfdata1(7,h,d) < sfdata1(8,h,d) .and. dabs(sfdata1(7,h,d)-sfdata1(8,h,d)) > eps) then
                            sf_tempstat=sf_tempstat+1
                            write(msg_unit,formstr(7))adjustl(pathid(ipath)),'Q43',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                    'TMPD < TPDP'
                        endif
                    endif
!                   also check precip and present weather to make sure
!                   they correlate, but only if both are being audited
                    if (sfvars(1)%laudit .and. sfvars(5)%laudit) then
                        parts(1)=int(sfdata1(5,h,d))/1000
                        parts(2)=int(sfdata1(5,h,d))-parts(1)*1000
                        if (sfdata1(1,h,d) > 0.0_r8 .and. dabs(sfdata1(1,h,d)-0.0_r8) > eps) then
                            if (dabs(sfdata1(5,h,d)-real(sfvars(5)%missval,r8)) < eps) then !precip without weather
                                precip_wx(1)=precip_wx(1)+1
                                write(msg_unit,formstr(7))adjustl(pathid(ipath)),'Q46',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                    'PRECIP WITHOUT WEATHER'
                            endif
                        else
                            if ((parts(1) > 0 .and. parts(1) < 70) .or. parts(1) > 90) then !weather without precip
                                precip_wx(2)=precip_wx(2)+1
                                write(msg_unit,formstr(7))adjustl(pathid(ipath)),'Q47',modnam,sfc_info(d)%sfcdate,'HR',h,&
                                    'WEATHER WITHOUT PRECIP'
                            endif
                        endif
                    endif
                endif !day has at least 1 obs
            enddo hrloop
        endif
    enddo dayloop

    return
    end subroutine sf_audit
!*********************************************************************************************************
      
    subroutine sf_stage2
!=========================================================================================================
!   SUBROUTINE SF_STAGE2
!   THIS SUBROUTINE PERFORMS STAGE 2 PROCESSING FOR SURFACE DATA WHEN READING QAOUT DATA
!   WITHOUT STAGE 1
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE READ_INPUT (READINP)
!
!   Variable definitions
!   
!   Integer variables
!   i:              variable counter
!   i1:             variable counter
!   eof:            end of file indicator
!   iline:          line number
!   iday1:          counter of number of days in data period for arrays (only used for check_asos)
!   iflag:          I/O indicator when reading linetype
!
!   Logical variables
!   asoshr1:        dummy logical variable used in subroutine check_asos
!
!   Character variables
!   lower:          character string of lowercase letters
!   upper:          character string of uppercase letters
!   form1:          format for reading inpline1
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use file_units, only: sf_qaout_unit
    use main1, only: inpline1,nfield,ilen,getdates,getloc,linelen
    implicit none
    integer(kind=4) :: i,i1,eof,iline,iday1,iflag
    logical :: asoshr1
    character(len=26) :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=26) :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=6) :: form1
    character(len=10) :: modnam='SF_STAGE2'
    
    write(form1,'(a2,i3,a1)')'(a',linelen,')'
!   read in the header from the QAOUT file to get station information and dates
    eof=0
    lbad=.false.
    asoshr1=.false.
    iline=0
    iday1=0
    do while (eof == 0 .and. .not. lbad .and. iline <=3)
        read(sf_qaout_unit,form1)inpline1
        ilen=len_trim(inpline1)
        iline=iline+1
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
                call getloc(i+8,sfid,sflat,sflon,sfgmt2lst,sf_user_elev,lgmt2lst,user_elev)
!               check to see if station is in ASOS list regardless
!               of whether ISHD data or non-ISHD data
!               read integer value of sfid and if not a number set isfid = 99999
                read(sfid,*,iostat=iflag)isfid
                if (iflag /= 0) isfid=9999
!               just use sfstart as the date argument
                call check_asos(isfid,sfstart,0,.true.,.false.,.false.,lasos,asoshr1)
            endif
            if (index(inpline1,'RANGE') > 0) then
                i=index(inpline1,'RANGE')
                nfield=6
                call surf_range(i+5)
            endif    
            if (index(inpline1,'DATES') > 0) then
                i=index(inpline1,'DATES')
                nfield=7
                call getdates(i+5,sfstart,sfend,sfdates)
            endif
        endif
    enddo
    rewind(sf_qaout_unit)
    return
    end subroutine sf_stage2
!*********************************************************************************************************
      
    subroutine read_1min
!=========================================================================================================
!   SUBROUTINE READ_1MIN
!   THIS SUBROUTINE READS AERMINUTE OUTPUT DURING STAGE 2
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      AERMET
!
!   Variable definitions
!   
!   Integer variables
!   i:              index value
!   j:              index value
!   iflag1:         I/O flag
!   eof:            end of file indicator
!   idate:          Integer date (YYYYMMDD)
!   iday1:          counter of number of days in data period for arrays
!   iyear:          4-digit year
!   imonth:         integer month
!   iday:           integer day of the month
!   ihr:            hour of day
!
!   Real variables
!   wspd1:          wind speed
!   wdir1:          wind direction
!
!   Logical variables
!   loverlap:       1-minute data and SURFACE data dates overlap
!   lifw:           logical variable denoting that station is part of the IFW group
!                   for AERMINUTE data period
!
!   Character variables
!   adate:          character string of date
!   header:         header record of AERMINUTE output file
!   formstr:        formats for messages
!   ifwtag:         IFW indicator
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use file_units, only: one_min_unit
    use main1, only: numdays,noprint,istage
    implicit none
    integer(kind=4) :: i,eof,iflag1,j,irec,iday1,iyear,imonth,iday,ihr
    integer(kind=8) :: idate,savedate
    real(kind=r8) :: wdir1,wspd1
    logical :: loverlap=.true.
    logical :: lifw=.false.
    character(len=60) :: formstr(5)
    character(len=8) :: adate
    character(len=90) :: header
    character(len=1) :: ifwtag
    character(len=10) :: modnam='READ_1MIN'
    
!   formats
!   1.  processing date
    write(formstr(1),'(a)')'(1x,a,1x,i1,a,1x,2(a2,a1),a4,1x,a)'
    
!   2.  AERMINUTE WBAN doesn't match SURFACE WBAN
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
    
!   3.  invalid data field
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   4.  dates out of order
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,i8)'

!   5.  no overlap in SURFACE and AERMINUTE dates
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,a1,i8,1x,a,1x,i8,a1,i8)'

    eof=0
    irec=0
    savedate=0
    
!   read the header and check the station WBAN, IFW status, and IFW date    
    read(one_min_unit,'(a90)')header
    
!   get the WBAN
    i=index(header,'WBAN:')
    j=index(header,'Call')
    if (i > 0 .and. j > 0) read(header(i+5:j-1),'(a)')sf1minid
    if (trim(adjustl(sf1minid)) /= trim(adjustl(sfid))) write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W40',modnam,&
        'AERMINUTE WBAN',trim(adjustl(sf1minid)),'DOES NOT MATCH SURFACE WBAN',trim(adjustl(sfid))

    
!   get IFW status
    i=index(header,'IFW:')
    if (i > 0) then
        read(header(i+5:i+9),'(a)')ifwtag
        if (ifwtag == 'Y') then
            lifw=.true.
        else
            lifw=.false.
        endif
    endif
    
!   check for IFW date
    i=index(header,'IFW date:')
    if (i > 0) then
      read(header(i+10:i+19),'(2(i2,1x),i4)',iostat=iflag1)imonth,iday,iyear
      if (iflag1 == 0) then
          sonicdate=iyear*10000+imonth*100+iday
      else
          lbad=.true.
          write(msg_unit,formstr(3))adjustl(pathid(ipath)),'E41',modnam,'INVALID DATE FIELD',trim(adjustl(header(i+11:i+19)))
      endif
    endif
    
!   now read the data to determine the start and end dates
!   years are 2-digit years but 1-minute output from NCEI starts
!   at 2000, so add 2000 to year.

    do while (eof == 0 .and. .not. lbad)
        read(one_min_unit,*,iostat=eof)iyear,imonth,iday,ihr,wspd1,wdir1
        if (eof == 0) then
            irec=irec+1
            idate=(iyear+2000)*10000+imonth*100+iday
            if (idate < savedate) then
                write(msg_unit,formstr(4))adjustl(pathid(ipath)),'E41',modnam,'CURRENT DATE',idate,'IS BEFORE PREVIOUS DATE',&
                    savedate
                lbad=.true.
            endif
            if (irec==1) then
                sfstart1min=idate
            else
!               as proceed in file, dates increase so by end of file, this will be the last date
                sfend1min=idate
            endif
            savedate=idate
        endif
    enddo
    rewind(one_min_unit)
!   if no problem with the data, allocate arrays and fill in arrays
    if (.not. lbad) then
!       check to see how the 1-minute data overlaps with the surface data
!       if they do not overlap issue a warning message
        if (sfend1min < sfstart .or. sfstart1min > sfend) then
            loverlap=.false.
            write(msg_unit,formstr(5))adjustl(pathid(ipath)),'W48',modnam,'SURFACE DATES,',sfstart,'-',sfend,&
                'DO NOT OVERLAP AERMINUTE DATES',sfstart1min,'-',sfend1min
        endif
        if (loverlap) then
!           get the number of days in the data period based on sfstart and sfend
            !nsf1mindays=numdays(sfstart1min,sfend1min)
            nsf1mindays=numdays(sfstart,sfend)
            allocate(one_mindat(nsf1mindays))
!           initialize
    d1:     do iday=1,nsf1mindays
                one_mindat(iday)%sf1mindate=0
    h1:         do ihr=1,24
                one_mindat(iday)%ifwhr(ihr)=.false.
                one_mindat(iday)%wdir(ihr)=real(sfvars(10)%missval,r8)
                one_mindat(iday)%wspd(ihr)=real(sfvars(11)%missval,r8)
                enddo h1
            enddo d1
 
!           now re-read file and fill in arrays
            read(one_min_unit,'(a90)')header
            eof=0
            savedate=0
            iday1=0
            do while (eof == 0)
                read(one_min_unit,*,iostat=eof)iyear,imonth,iday,ihr,wspd1,wdir1
                idate=(iyear+2000)*10000+imonth*100+iday
                if (idate >= sfstart .and. idate <= sfend) then
                    if (idate /= savedate) then
                        iday1=iday1+1
                        write(adate,'(i8.8)')idate
!                       write to screen the date and hour being processed if NOPRINT not specfied on JOB pathway
                        if (.not. noprint) write(output_unit,formstr(1))'Stage',istage,&
                        ': Extracting 1-minute data for month/day/year',adate(5:6),'/',adate(7:8),'/',adate(1:4),'LST'
                    endif
                    one_mindat(iday1)%sf1mindate=idate
                    if (lifw .and. idate >= sonicdate) one_mindat(iday1)%ifwhr(ihr)=.true.
                    one_mindat(iday1)%wspd(ihr)=wspd1
                    one_mindat(iday1)%wdir(ihr)=wdir1
                endif
                savedate=idate
            enddo
        endif
    else
        lbadsfc=.true.
    endif    

    return
    end subroutine read_1min
!*********************************************************************************************************

    subroutine sf_thresh(i1)
!=========================================================================================================
!   SUBROUTINE SF_THRESH
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE THRESH_1MIN KEYWORD FOR ASOS DATA TO GET 1-MINUTE WIND SPEED THRESHOLD.
!
!   MODIFIED DECEMBER 3, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE PBL (PBL_PATH)
!     
!   INPUT ARGUMENTS 
!
!   I1:             LENGTH OF KEYWORD WITH TRAILING BLANKS REMOVED+INDEX VALUE OF KEYWORD
!
!   Variable definitions
!      
!   Integer variables
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!   iflag:          IO flag
!
!   Real variables
!   minthresh:      minimum valid threshold
!   maxthresh:      maximum valid threshold
!   warnthresh:     threshold to issue warning
!
!   Character variables
!   datafield:      data for OS_THRESH keyword read from input line
!   formstr:        format for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: ikey,nfield,keywrd,getfields,writeunit
    implicit none
    integer(kind=4), intent(in) :: i1
    integer(kind=4) :: nfield1,iflag
    real(kind=r8) :: minthresh=0.0_r8
    real(kind=r8) :: maxthresh=1.0_r8
    real(kind=r8) :: warnthresh=0.5_r8
    character(len=60) :: formstr(2)
    character(len=100),allocatable, dimension(:) :: datafield
    character(len=10) :: modnam='SF_THRESH'
    
!   initialize
    nfield1=0
    iflag=0
    nfield1=nfield-1
      
    allocate(datafield(nfield1))
      
    datafield='0000000000000000000000000'
      
!   formats
!   1.  warning about speed exceeds threshold
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),f5.3,1x,a,1x,f3.1,1x,a)'
    
!   2.  invalid value for keyword
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'2(a,1x,a))'

!   read input line to get threshold
    call getfields(i1,nfield1,datafield)  

    read(datafield(1),*,iostat=iflag)asos_thresh

    if (iflag == 0) then
        if (asos_thresh < minthresh) then !wind speed < minimum speed
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(keywrd(ikey))),'SPEED',asos_thresh,&
                'M/S IS BELOW',minthresh,'M/S'
            lbad=.true.
        elseif (asos_thresh > maxthresh) then !wind speed > maxspeed
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,trim(adjustl(keywrd(ikey))),'SPEED',asos_thresh,&
                'M/S EXCEEDS',maxthresh,'M/S'
            lbad=.true.
        else
            if (asos_thresh > warnthresh) write(writeunit,formstr(1))adjustl(pathid(ipath)),'W51',modnam,&
                trim(adjustl(keywrd(ikey))),'SPEED',asos_thresh,'M/S EXCEEDS',warnthresh,'M/S' !wind speed > warning speed
        endif
    else !invalid entry
        write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR KEYWORD',trim(adjustl(keywrd(ikey))),&
            ':',trim(adjustl(datafield(1)))
        lbad=.true.
    endif
      
    deallocate(datafield)
      
    return
    end subroutine sf_thresh
!*********************************************************************************************************
      
    subroutine nws_hgts(i1)
!=========================================================================================================
!   SUBROUTINE NWS_HGTS
!   THIS SUBROUTINE PROCESSES THE LINE NWS_HGT FROM THE RUNSTREAM INPUT FILE TO GET NWS ANEMOMETER
!   HEIGHT
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
!   i1:             starting index to read from input line
!   nfield1:        number fields on line minus 1 to account for keyword
!   iflag:          IO flag
!
!   Real variables
!   hgt:            NWS instrument height
!
!   Character variables
!   formstr:        format for messages
!   datafield:      data for DELTA_TEMP keyword read from input line
!   modnam:         Subroutine name
!=========================================================================================================
!   note that instr and ninstr are not currently used but are kept here in case more instruments are added
!   later
    use main1, only: nfield,getfields,writeunit,instr,ninstr
    implicit none
    integer(kind=4), intent(in) :: i1
    integer(kind=4) :: nfield1,iflag
    real(kind=r8) :: hgt=-9
    character(len=60) :: formstr(2)
    character(len=100),allocatable,dimension(:) :: datafield
    character(len=10) :: modnam='NWS_HGTS'
      
    nfield1=nfield-1
   
    allocate(datafield(nfield1))
    datafield='0000000000000000000000000'
 
!   formats
!   1.  invalid variable name or height for NWS_HGT
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2.  height exeeds minimum or maximum threshold
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,f7.2,1x,a)'
    
    call getfields(i1,nfield1,datafield)   
    
!   get instrument, should only be wind
    if (trim(adjustl(datafield(1))) /= trim(adjustl(instr(1)))) then
        write(writeunit,formstr(1))adjustl(pathid(ipath)),'E79',modnam,'INVALID VARIABLE FOR NWS_HGT:',trim(adjustl(datafield(1)))
    else
        read(datafield(2),*,iostat=iflag)hgt
        if (iflag == 0) then
            if (hgt <= 0.0) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E21',modnam,'NWS_HGT',hgt,'< 0 M'
                lbad=.true.
            else
                if (hgt > 30.0) write(writeunit,formstr(2))adjustl(pathid(ipath)),'W72',modnam,'NWS_HGT',hgt,'> 30 M'
                nws_hgt=hgt
            endif
        else
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E80',modnam,'INVALID HEIGHT FOR NWS_HGT:',&
                trim(adjustl(datafield(2)))
            lbad=.true.
        endif
    endif

    deallocate(datafield)

    return
    end subroutine nws_hgts
!*********************************************************************************************************   
    end module surface