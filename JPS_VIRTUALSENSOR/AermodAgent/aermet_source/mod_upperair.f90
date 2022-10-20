    module upperair
!=========================================================================================================
!   MODULE MAIN1
!   THIS MODULE CONTAINS COMMON VARIABLES AND SUBROUTINES NEEDED THROUGOUT AERMET PROCESSING 
!   TO PROCESS UPPER AIR DATA
!
!   MODIFIED APRIL 22, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   USED BY:        AERMET, MODULES READ_INPUT, REPORTS
!
!   Variable definitions
!
!   Integer variables
!   upstart:        upper air extraction start date (YYYMMDD)
!                   start hour assumed to be 1
!   upend:          upper air extract end date (YYYYMMDD)
!                   end hour assumed to be 24
!   upgmt2lst:      conversion from Greenwich Mean Time (GMT) to
!                   local standard time (LST).
!                   example:  GMT to eastern time zone (EST) is 5
!                   do not have to account for daylight savings time
!   iupform:        DATA file form index for upformats reporting
!   updates:        2x3 array of start month, day, year and end month,
!                   day and year.  1st dim is the start (1) or end (2)
!                   2nd dim is year (1), month (2), and day (3)
!   nupdays:        number of days in the upper air data period
!   nsound:         maximum number of soundings per day
!   nuplevels:      maximum number of levels in the soundings
!   uptop:          Maximum level extracted (set to 5,000 m)
!   maxlevels:      maximum number of levels allowed per sounding (300)
!   nupvars:        number of upper air variables
!   nupvars_keep:   number of upper air variables to keep for output for stage 2
!   nupvars1:       number of upper air variables to write to QAOUT file
!                   this is the core six variables plus any audited calculated variables
!                   such as UASS, UADS, UALR, and UADD
!   nupformats:     number of upper air formats
!   nupkeys:        number of UPPERAIR keywords
!   nsnd:           6-element array denoting the total number of soundings read from
!                   file (element 1),number of soundings in data window (element 2),
!                   number of soundings per day (element 3), total number of valid
!                   soundings, i.e. not skipped (element 4), number of duplicate soundings
!                   (element 5), and number of skipped soundings(element 6).  
!                   Note that in the final reported tally for total soundings
!                   valid, the number of duplicates will be subtracted from those
!                   numbers.  Skipped and duplicate soundings are included in the 
!                   total extraction number.
!                   if an error or warning is issued that indicates the sounding number,
!                   nsnd(1) will be the referenced number.
!   nup_audit_vars: number of variables to audit
!   writelevs:      number of levels written to output array
!   savepress:      previous level's pressure
!   nwarnlim:       maximum number of times to write warning message that
!                   station ID in file mismatches user entered station ID
!                   once this limit is reached, messages will no longer
!                   be written that station ID doesn't match user station ID
!                   for individual soundings. A message will be written
!                   that limit has been reached. 
!   up_audit_index: array of integers indicating which upper air variables to audit
!                   example: if 3 variables to audit are UAPR, UAHT, and UATD
!                   then n_audit_vars=3 and audit_index is (1,2,4)
!   up_audit_counts:3-D array audit counts
!                   1st dim is variable, 2nd dim is # of layers
!                   3rd dim is count where 1=#total count,2=# of missing, 
!                   3= is number of lower bound,violations, 
!                   4 =is number of upper bound violations
!   snd_date:       temporary array of sounding dates (MMDDYYY) in LST
!                   this array is used to fill in the dates for
!                   snddate in the derived data type sound_info
!   snd_hrs:        temporary array of the hour in LST for each
!                   sounding of a day; used to fill in snd_hr
!                   in derived data type sound_info
!   s_levels:       temporary array of number of levels per sounding; 
!                   used to fill in nlevels in derived data type 
!                   sound_info
!   nsnding1:       temporary array of number of soundings per day;
!                   used to fill in nsnding in derived data type sound_info  
!   nmods:          number of modifications based on MODIFY keyword (3)
!   up_inc:         increments of souding for QA
!   wind_stats:     counts of 1) calm winds, 2) wind speed = 0 and non-missing/non-zero wind direction
!   no_sound_days:  number of days with no soundings
!   no_am_days:     number of days with no morning soundings
!
!   Real variables
!   uplat:          latitude of upper air station
!   uplon:          longitude of upper air station
!   upelev:         upper air station elevation
!                   note upelev is not used but checked
!                   if found, issue warning
!   updata:         4-D upper air data array read from sounding 
!                   file in stage 1.
!                   Dimensions:
!                   1st:  number of main upper air variables (6)
!                   2nd:  number of levels (nuplevels)
!                   3rd:  number of soundings for the day (nsound)
!                   4th:  number of days in period (nupdays)
!   updata1:        temporary 4-D upper air data array read from sounding 
!                   file in stage 1.
!                   Dimensions:
!                   1st:  number of upper air variables (12)
!                   2nd:  number of levels (maxlevels)
!                   3rd:  number of soundings for the day (24)
!                   4th:  number of days in period (nupdays)
!
!   Logical variables
!   lupkeywords:      array denoting if keywords found
!   note ikey below is the index of the keyword in the keywrd array in module main1
!   1:              denotes that DATA keyword (ikey=4) found for upper air data. keyword
!                   is mandatory
!   2:              denotes NO_MISSING keyword found (ikey=5)
!   3:              denotes EXTRACT (ikey=7) keyword found. 
!   4:              denotes that QAOUT (ikey=8) keyword found.  If processing stages 1
!                   and 2 in the same run, this keyword is optional.
!                   If processing stage 1 only, this is mandatory.
!   5:              denotes XDATES keyword found (ikey=9)
!   6:              denotes LOCATION keyword found (ikey=10)
!   7:              denotes MODIFY keyword found (ikey=11)
!   8:              denotes RANGE keyword found (ikey=12)
!   9:              denotes AUDIT keyword found (ikey=13)
!   have_soundings: have at least 1 sounding for the data period
!   lbadup:         logical variable denoting issue with processing upper air data   
!   lsound:         temporary array denoting if a day has any soundings; used to
!                   fill sounding in derived data type sound_info
!   writeext:       logical variable denoting if AERMET should write to (true) or
!                   read from (false) EXTRACT file
!   ltop:           logical variable denoting that the sounding has exceeded uptop
!   lskip:          logical variable denoting to skip processing because
!   lmodify:        3-element array denoting what upper air modifications to 
!                   make. 1= delete mandatory levels, 2=set wind direction to 0
!                   for wind speed=0 and 3=interpolate missing temperature and dewpoint
!                   from non-missing levels above and below missing level.
!   lgmt2lst:       variable denoting if GMT to LST found (true if so)
!   lelev:          denotes if user entered elevation on LOCATION keyword (true if entered)
!   upstage1:       denotes if stage 1 being processed for upperair data
!
!   Character variables
!   upid:           upper air station ID
!   iupid:          upper air station ID read from file to compare against upid
!   ext_form:        formats for messages
!   ext_errstr:         error messages when reading data
!   ext_endstr:         end of extraction messages
!
!   data type uavars (upvars)
!   varname:        variable name, i.e. UAPR=pressure; see subroutine upper_init for definitions
!   laudit:         logical variable denoting if variable will be audited. Initial value is 
!                   false, no audit.
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
!   missval:        default missing value
!   lowbound:       lower accetable bound
!   upbound:        upper accetable bound
!   conv:           real value to convert values to correct units
!
!   Character variables
!   qaout_vars:         variable names for the QAOUT variables
!                       includes the six core variables and any calculated variables
!                       (UASS, UADS, UALR, and UADD) that are audited. These will be written to the 
!                       QAOUT file but not EXTRACT file
!
!   data type uformats (upformats)
!   uformat:        array of valid upper air formats
!   lupform:        array of logical variable denoting 
!                     which format used  
!   maxblock:       maximum block size for reading 6201 format data
!   rdsz:           size of record within the block for 6201 format data
!   rdpos:          starting position of record within the block for 6201 format data
!   offset:         offset from beginning of block for 6201 format data
!
!   data type sdinfo (sound_info): sounding info for each day
!   dimension by number of days in data period
!   snddate:          date for sounding (YYYYMMDD) 
!   nsndg:          number of soundings for the day
!   nlevels:        number of levels for the sounding
!   snd_hr:         time of sounding in LST
!   sounding:       sounding available for the day
!=========================================================================================================
!   ipath,lbad, r8, and pathid used in most subroutines keep here. others are named in subroutines in which
!   they are used
    use main1, only: ipath,lbad,r8,pathid
    
    implicit none

    integer(kind=8) :: upstart=0
    integer(kind=8) :: upend=0
    integer(kind=4) :: upgmt2lst=-9
    integer(kind=4) :: iupform=1
    integer(kind=4) :: updates(2,3)=0
    integer(kind=4):: nupdays=0
    integer(kind=4) :: nsound=0
    integer(kind=4) :: nuplevels=0
    integer(kind=4) :: nsnd(6)=0
    integer(kind=4) :: nup_audit_vars=0
    integer(kind=4) :: nup_calc_audit=0
    integer(kind=4) :: nupvars1=0
    integer(kind=4) :: writelevs=0
    integer(kind=4) :: savepress=0
    integer(kind=4) :: wind_stats(2)=0
    integer(kind=4) :: tempstat=0
    integer(kind=4) :: nsnd_5k=0
    integer(kind=4) :: no_sound_days=0
    integer(kind=4) :: no_am_days=0
    integer(kind=4), parameter :: nwarnlim=100
   
    logical :: ltop=.false.
    logical :: lskip=.false.
    logical :: have_soundings=.false.

      
    integer(kind=4), parameter :: uptop=5000
    integer(kind=4), parameter :: maxlevels=300
    integer(kind=4), parameter :: nupvars=10 
    integer(kind=4), parameter :: nupvars_keep=6
    integer(kind=4), parameter :: nupformats=5 
    integer(kind=4), parameter :: nupkeys=9
    integer(kind=4), parameter :: nmods=3
    integer(kind=4), parameter :: up_inc=500
     
    integer(kind=4), allocatable, dimension(:) :: up_audit_index
    integer(kind=4), allocatable, dimension(:,:,:) :: up_audit_counts
    integer(kind=8), allocatable, dimension(:) :: snd_date
    integer(kind=4), allocatable, dimension(:,:) :: snd_hrs
    integer(kind=4), allocatable, dimension(:,:) :: s_levels
    integer(kind=4), allocatable, dimension(:) :: nsnding1
      
    real(kind=r8) :: uplat=0.0_r8
    real(kind=r8) :: uplon=0.0_r8
    real(kind=r8) :: upelev=0.0_r8

    real(kind=r8), allocatable, dimension(:,:,:,:) :: updata
    real(kind=r8), allocatable, dimension(:,:,:,:) :: updata1
      
    logical :: lupkeywords(nupkeys)=.false.
    logical :: lbadup=.false.
    logical :: writeext=.false.
    logical :: lmodify(nmods)=.false.
    logical :: lgmt2lst=.false.
    logical :: lelev=.false.
    logical :: upstage1=.false.
    logical, allocatable, dimension(:) :: lsound
      
    character(len=8) :: upid=''
    character(len=8) :: iupid='0'
    character(len=60) :: ext_form(14)
    character(len=55) :: ext_errstr(5),ext_endstr(4)
    character(len=4), allocatable, dimension(:) :: qaout_vars
    
    type uavars
        character(len=4) varname
        logical laudit,lmod,lnomiss,lincbound
        integer(kind=4) :: missval,lowbound,upbound
        real(kind=r8) :: conv
    end type uavars
      
    type(uavars) :: upvars(nupvars)
      
    type uformats
        character(len=10) :: uformat
        logical lupform
        integer(kind=4) :: maxblock,rdsz,rdpos,offset
    end type uformats
      
    type(uformats) :: upformats(nupformats)
      
    type sdinfo
        integer(kind=8) :: snddate
        integer(kind=4) :: nsnding
        integer(kind=4), allocatable, dimension(:) :: snd_hr
        integer(kind=4), allocatable, dimension(:) :: nlevels
        logical sounding
    end type sdinfo
      
    type(sdinfo), dimension(:),allocatable:: sound_info
     
    data ext_errstr /'ERROR READING DATE/HOUR FOR SOUNDING #','ERROR READING MONTH FOR SOUNDING #',&
        'ERROR READING STATION INFORMATION FOR SOUNDING #','ERROR DETERMINING LINE TYPE FOR SOUNDING #',&
        'ERROR DETERMINING NUMBER OF LEVELS FOR SOUNDING #'/
    
    data ext_endstr /'END OF DATA WINDOW ENCOUNTERED','NO SOUNDINGS RETRIEVED; NO OTHER ERRORS',&
        'NO SOUNDINGS RETRIEVED; EXITING WITH AN ERROR','EXTRACT NOT COMPLETED; STOPPED AFTER SDG #'/
    contains 
!*********************************************************************************************************
      
    subroutine upper_init
!=========================================================================================================
!   SUBROUTINE UPPER_INIT
!   THIS SUBROUTINE INITIALIZES THE DATA TYPE UPVARS
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!   CALLED BY:      MODULE UPPERAIR (UPPER_PATH)
!
!   note missing, lower and upper bounds scaled
!   by multipliers, e.g. pressure is milibars*10, not milibars
!   1012 mb=10120
!   with exception of name, all values can be reset by user
!   using NO_MISSING for lnomiss, and RANGE for lessthan, missval, lowbound, and upbound
!=========================================================================================================
      use main1, only : msg_form
      implicit none
!                   name    laudit  lmod   lnomis  lincbound missval lowbound upbound
    upvars(1)=uavars('UAPR',.false.,.false.,.false.,.false.,99999,5000,10999,0.1_r8) !pressure
    upvars(2)=uavars('UAHT',.false.,.false.,.false.,.true.,-99999,0,5000,1.0_r8) !height
    upvars(3)=uavars('UATT',.false.,.false.,.false.,.false.,-9990,-350,+350,0.1_r8) !dry bulb temperature
    upvars(4)=uavars('UATD',.false.,.false.,.false.,.false.,-9990,-350,+350,0.1_r8) !dew point temperature
    upvars(5)=uavars('UAWD',.false.,.false.,.false.,.true.,999,0,360,1.0_r8) !wind direction
    upvars(6)=uavars('UAWS',.false.,.false.,.false.,.false.,9990,0,500,0.1_r8) !wind speed
    upvars(7)=uavars('UASS',.false.,.false.,.false.,.true.,-9999,0,5,1.0_r8) !wind speed shear
    upvars(8)=uavars('UADS',.false.,.false.,.false.,.true.,-9999,0,90,1.0_r8) !wind direction shear
    upvars(9)=uavars('UALR',.false.,.false.,.false.,.true.,-9999,-2,5,1.0_r8) !temperature lapse rate
    upvars(10)=uavars('UADD',.false.,.false.,.false.,.true.,-9999,0,2,1.0_r8) !dew point deviation
!    upvars(11)=uavars('UAM1',.false.,.false.,.false.,.true.,-9999,50,500,1.0_r8) !user defined, drop?
!    upvars(12)=uavars('UAM2',.false.,.false.,.false.,.true.,-9999,500,3500,1.0_r8) !user defined, drop? 
      
!                           uformat   lupform maxblock rdsz rdpos offset
    upformats(1)=uformats('6201FB',.false.,2876,2876,5752,0)
    upformats(2)=uformats('6201VB',.false.,12000,0,0,4)
    upformats(3)=uformats('FSL',.false.,0,0,0,0)
    upformats(4)=uformats('IGRA',.false.,0,0,0,0) !new
    upformats(5)=uformats('EXTRACT',.false.,0,0,0,0) !new
      
!   extraction message formats
!   1.  sounding is not correct format
    write(ext_form(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2.  invalid order of lines
    write(ext_form(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,1x,a,1x,i3,1x,a,1x,i8)'
    
!   3.  error reading date/hour, month, station info, line type, number of levels
    write(ext_form(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5,2(1x,a))'
    
!   4.  station ID from file doesn't match user entered ID
    write(ext_form(4),'(2(a))')trim(adjustl(msg_form)),'5(a,1x),i5,1x,a20)'
    
!   5.  limit of warning messages
    write(ext_form(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,1x,a3,1x,a)'
   
!   6.  invalid wind units (FSL only)
    write(ext_form(6),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),i5)'

!   7.  invalid line type
    write(ext_form(7),'(2(a))')trim(adjustl(msg_form)),'a,1x,i4,1x,a,1x,i5,2(1x,a))'
    
!   8.  multiple FSL versions detected (FSL only)
    write(ext_form(8),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5,1x,a20)'

   
!   9.  skip sounding, 1st level not surface or number of levels not equal to expected number
    write(ext_form(9),'(2(a))')trim(adjustl(msg_form)),'2(a,1x,i5,1x),a20)'
    
!   10. skip sounding, surface level height missing
    write(ext_form(10),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5,1x,a20)'
                                
!   11. error reading line
    write(ext_form(11),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5,2(1x,a),1x,a20)'
   
!   12. multiple surface data lines
    write(ext_form(12),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5,1x,a20)'

!   13. end of data window, no soundings retrieved no errors, or no soundings retrieved with error
    write(ext_form(13),'(2(a))')trim(adjustl(msg_form)),'a)'

!   14. extraction not complete
    write(ext_form(14),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5)' 
    
    return
    end subroutine upper_init

!*********************************************************************************************************

    subroutine upper_path
!=========================================================================================================
!   SUBROUTINE UPPER_PATH
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE UPPER PATHWAY.  THIS INCLUDES: DATA, EXTRACT, QAOUT, RANGE, AUDIT, NO_MISSING,
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
!   j:              counter for checking DATA file format against valid upper air data file types
!   jj:             counter for lupkeywords based on ikey value
!
!   Logical variables
!   lvalid:         input DATA file type is valid (true) or invalid (false)
!   lgood:          variable denoting if a filename is okay
!
!   Character variables
!   form1:          format to read filename
!   fileform:       character string of DATA file format to compare against upformats array
!   formstr:        format strings for error messages
!   modnam:             Subroutine name
!=========================================================================================================
    use main1, only: inpline,inpline1,ikey,nfield,ilen,keywrd,checkfile,dataline,getdates,getloc,fileind,formind,writeunit,getunit,&
        msg_form
    use file_units,only : flength,up_data_unit,up_inpfile,up_qaout_unit,up_qafile,up_extract_unit,up_extfile
    implicit none
      
    integer(kind=4):: i,i1,j,jj  
    logical :: lvalid,lgood
    character(len=6) :: form1
    character(len=10) :: fileform
    character(len=50) :: formstr(4)
    character(len=10) :: modnam='UPPER_PATH'
      
!   initialize
    i=0
    i1=0
    j=0
    jj=0
    lvalid=.false.
    lgood=.true.
    lbad=.false.
  
!   formats for message file
!   1.  invalid keyword or duplicate keyword
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
!   2.  invalid number of fields for keyword
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'i3,2(1x,a))'
!   3. too many fields, blocking factor/type not needed
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,3(1x,a))'
!   4.  invalid file format
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,3(1x,a))'
    
!   form1 is the format to use to read the message or report file
!   format string is (a300)
    write(form1,'(a2,i3,a1)')'(a',flength,')'

!   get file unit for messages
    call getunit

!   ikey = 0 means text string UPPER found on input line
!   set the logical variables for keywords below to false
!   meaning they have not been detected yet
!   initialize upper air variables
    if (ikey == 0) then
        call upper_init !initialize variables
    else
        i=index(inpline1,trim(adjustl(keywrd(ikey))))
        i1=len_trim(keywrd(ikey))
          
!       set the logical variable for the keyword to true
        if ((ikey == 4 .or. ikey== 5) .or. (ikey >= 7 .and. ikey <=13)) then
            if (ikey <=5) then
                jj=ikey-3
            else
                jj=ikey-4
            endif
            if (.not. lupkeywords(jj)) then
                lupkeywords(jj)=.true.
            else
!               issue error message that keyword has already been found
                if (ikey == 4 .or. (ikey >=7 .and. ikey < 11)) then
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
     
        if (ikey == 4) then !data file
            upstage1=.true.
!           if not 3 to 5 fields, then there is an error
            if (nfield < 3 .or. nfield > 5) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
!               issue warning that blocking factor and ASCII string not needed
!               this is not an error
                if (nfield /=3) write(writeunit,formstr(3))adjustl(pathid(ipath)),'W01',modnam,'TOO MANY FIELDS',nfield,'FOR',&
                    trim(adjustl(keywrd(ikey))),'KEYWORD; BLOCKING FACTOR AND/OR TYPE NOT NEEDED'
                      
!               read DATA line to get input file and format
!               and positions in input line to read
!               see subroutine dataline for details
                call dataline(i,i1)
!               read the original line to get the filename
                read(inpline(fileind(1):fileind(2)),'(a)')up_inpfile
!               get the DATA filename
                call checkfile(up_data_unit,up_inpfile,1,lgood)                  
!               read the uppercase version of the line to get the file format
                read(inpline1(formind(1):formind(2)),'(a)')fileform
                    
!               check the format
                j=1
                do while (j .le. nupformats .and. .not. lvalid)
                    if (trim(adjustl(fileform)) == trim(adjustl(upformats(j)%uformat))) then
                        upformats(j)%lupform=.true.
                        lvalid=.true.
                        iupform=j
                    endif
                    j=j+1
                enddo
                if (.not. lvalid) then
!                   invalid file format
                    write(writeunit,formstr(4))adjustl(pathid(ipath)),'E04',modnam,'INVALID FILE FORMAT',trim(adjustl(fileform)),&
                        'FOR UPPER AIR FILE',trim(adjustl(up_inpfile))
                    lbad=.true.
                endif
            endif

        else if (ikey == 5) then !no_missing
!           this keyword is repeatable so duplicate entries allowed
            call upper_list(i+i1)
              
        else if (ikey == 7) then !extract
            upstage1=.true.
!           if number of fields is not 2, then line is bad
            if (nfield /= 2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else 
                read(inpline(i+i1+1:ilen),form1)up_extfile
!               get the EXTRACT filename
                call checkfile(up_extract_unit,up_extfile,4,lgood) 
            endif

        else if (ikey == 8) then !qaout
!           if number of fields is not 2, then line is bad
            if (nfield /= 2) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                read(inpline(i+i1+1:ilen),form1)up_qafile
!               get the QAOUT filename
                call checkfile(up_qaout_unit,up_qafile,4,lgood)
            endif

        else if (ikey == 9) then !xdates
!           incorrect number of fields, line is bad                  
            if (nfield /= 3 .and. nfield /= 4 .and. nfield /= 7 .and. nfield /= 8) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else  
                call getdates(i+i1,upstart,upend,updates)
            endif
                  
        else if (ikey == 10) then !location
!           incorrect number of fields                  
            if (nfield < 4 .or. nfield > 6) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call getloc(i+i1,upid,uplat,uplon,upgmt2lst,upelev,lgmt2lst,lelev)
            endif
              
        else if (ikey == 12) then !range
            if (nfield /= 6) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call up_range(i+i1)
            endif
              
        else if (ikey == 13) then !audit   
            call upper_list(i+i1)
              
        else if (ikey == 11) then !modify keyword
            if (nfield > nmods+1) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                call up_modkey(i+i1)
            endif
              
        else !invalid keyword
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E01',modnam,'INVALID KEYWORD:',trim(adjustl(keywrd(ikey)))
        endif
    endif
    
    return
      
    end subroutine upper_path
!*********************************************************************************************************

    subroutine upper_list(i1)
!=========================================================================================================
!   SUBROUTINE UPPER_LIST
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE NO_MISSING OR AUDIT KEYWORD FOR UPPER AIR DATA.
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UPPER_PATH)
!
!   INPUT ARGUMENT(S)
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
!   NOTE: nupvars is the number of upper air variables
!
!   Logical variables
!   uplogical:      Array of values for logical check for missing values (true=skip in message file,
!                   false=report, this is default) or audit (true=audit,false=don't audit)
!
!   Character variables
!   uvar:               array of upper air variable names
!   formstr:            format string for error message
!   modnam:             Subroutine name
!=========================================================================================================   
      
    use main1, only : var_list,nfield,writeunit,ikey,inpline1,msg_form
    implicit none
    integer(kind=4), intent(in) :: i1
    integer(kind=4) nfield1,i
    logical uplogical(nupvars)
    character(len=4)uvar(nupvars)
    character(len=50) :: formstr
    character(len=10) :: modnam='UPPER_LIST'
    
!   initialize
    i=0
    nfield1=nfield-1
      
!   format for message file
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,2(1x,a))'
    
!   initialize uvar and uplogical to pass to var_list
    v1: do i=1,nupvars
        uvar(i)=upvars(i)%varname
!       set uplogical to appropriate values based on keyword
        if (ikey == 5) then
            uplogical(i)=upvars(i)%lnomiss
        else
            uplogical(i)=upvars(i)%laudit
        endif
    enddo v1

!   check the string of variables for keyword
    call var_list(i1,nfield1,uvar,uplogical,nupvars)

!   reset the values of the logical variables of whether to track missing values or audit variables
!   issue warning if variable has already been found for the keyword
    v2: do i=1,nupvars
        if (ikey == 5) then
            if (upvars(i)%lnomiss)write(writeunit,formstr)adjustl(pathid(ipath)), 'W02',modnam,&
                'DUPLICATING LISTING OF VARIABLE FOR',trim(adjustl(upvars(i)%varname)),'KEYWORD NO_MISSING'
            upvars(i)%lnomiss=uplogical(i)
        else
            if (upvars(i)%laudit .and. index(inpline1,upvars(i)%varname) > 0) then
                write(writeunit,formstr)adjustl(pathid(ipath)), 'W02',modnam,'DUPLICATING LISTING OF VARIABLE FOR',&
                    trim(adjustl(upvars(i)%varname)),'KEYWORD AUDIT'
            else
                upvars(i)%laudit=uplogical(i)
            endif
        endif
    enddo v2
 
    return 
    end subroutine upper_list
!*********************************************************************************************************

    subroutine up_range(i1)
!=========================================================================================================
!   SUBROUTINE UP_RANGE
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE RANGE KEYWORD FOR UPPER AIR DATA TO MODIFY THE RANGES OF VALID DATA.
!
!   MODIFIED DECEMBER 3, 2021
!   
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UPPER_PATH, UP_STAGE2)
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
!   NOTE: nupvars is the number of upper air variables
!
!   Logical variables
!   lfound:         logical variable denoting variable name found in upper air list
!   linc:           logical value to include lower and upper bounds (true)]
!                   or exclude (false)
!                   value is based on the value of v2
!   lgood:          logical variable denoting if all values are valid (true)
!                   or not (false).  this variable is used to decide whether
!                   to write the new values to the upper air arrays.
!
!   Character variables
!   varnam1:        data for RANGE keyword read from input line
!   formstr:        format for message file
!   modnam:         Subroutine name
!=========================================================================================================   
      
    use main1, only : range_mod,nfield,getfields,writeunit,msg_form
    implicit none
    integer(kind=4), intent(in) :: i1
    integer(kind=4) nfield1,i,lowval,hival,missval
    logical :: lfound,linc,lgood
    character(len=10) :: modnam='UP_RANGE'
    character(len=100) :: varnam1  !for getfields
    character(len=50) :: formstr
    allocatable :: varnam1(:)
      
!   initialize
    i=0
    lowval=-9
    hival=-9
    missval=-9
    lfound=.false.
    linc=.false.
    lgood=.true.
    nfield1=nfield-1
      
!   format for messsage file
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
    allocate(varnam1(nfield1))
    varnam1='0000000000000000000000000'
!   read input line to get variable name, the inclusion indicator, lower bound, and upper bound
    call getfields(i1,nfield1,varnam1)
    
!   read varnam1 to see if in the upper air variable list
    i=1
    l1: do while(i <= nupvars .and. .not. lfound)
        if (trim(adjustl(varnam1(1))) == trim(adjustl(upvars(i)%varname))) then
            lfound=.true.
        else
            i=i+1
        endif
    enddo l1
      
!   if the variable from the input line is in the upper air array
!   then get the new ranges and/or missing data value.
    if (lfound) then
        call range_mod(varnam1(2),varnam1(3),varnam1(4),varnam1(5),linc,lowval,hival,missval,lgood)
        if (lgood) then
!           if variable has already been modified then warn user but still reset values
            if (upvars(i)%lmod) write(writeunit,formstr)adjustl(pathid(ipath)), 'W02',modnam,&
                'DUPLICATING LISTING OF VARIABLE FOR RANGE KEYWORD:',trim(adjustl(upvars(i)%varname))
!           even if variable modified previously, use the latest values
            upvars(i)%lincbound=linc
            upvars(i)%lowbound=lowval
            upvars(i)%upbound=hival
            upvars(i)%missval=missval
            if (.not. upvars(i)%lmod) upvars(i)%lmod=.true.
        endif
    else
        write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID VARIABLE NAME FOR RANGE KEYWORD:',&
            trim(adjustl(varnam1(1)))
        lbad=.true.
    endif
      
    deallocate(varnam1)
      
    return 
    end subroutine up_range
!*********************************************************************************************************

    subroutine up_test
!=========================================================================================================
!   SUBROUTINE UP_TEST
!   THIS SUBROUTINE CHECKS THAT MANDATORY KEYWORDS HAVE BEEN INCLUDED
!   THIS IS NOT A CHECK ON THE SYNTAX OR IF INCLUDED FILENAMES EXIST
!   THAT HAS BEEN DONE EARLIER IN UP_PATH
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
!   iflag:          IO flag for reading header record of EXTRACT file
!                   when checking DATA keyword status
!
!   Logical variables
!   no_extract:     true if EXTRACT keyword not found or EXTRACT file is empty 
!
!   Character variables
!   junk:           data read from first line of EXTRACT file (if any data)
!   formstr:        format for messages
!   modnam:         Subroutine name
!=========================================================================================================   
    use main1, only: ipath,keywrd,lstage,msg_form
    use file_units, only: msg_unit,up_extract_unit
    implicit none
    integer(kind=4) :: iflag
    logical :: no_extract
    character(len=8) :: junk
    character(len=50) :: formstr(2)
    character(len=10) :: modnam='UP_TEST'

!   initialize
    iflag=0
    no_extract=.false.
    junk=''

!   initialize formats
!   1.  MISSING KEYWORD
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2.  XDATES MISSING AND STAGE 2 BEING PROCESSED; DATES TAKEN FROM METPREP
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a)'
    
!   note that when UP_TEST is called, msg_unit has been set
!   no longer need to write messages to the value from getunit
!   write to msg_unit
      
!   DATA keyword if stage 1 only
!   if stage 1 only and both EXTRACT and QAOUT are listed
!   then AERMET assumes data is not read from raw input 
!   but extracted data is QA'd
!   if the EXTRACT datafile is empty and no QAOUT, and DATA keyword is
!   not listed, then issue error.
!   DATA not needed if running stage 2 without stage 1

    if (upstage1 .and. .not. lupkeywords(1)) then
!       if EXTRACT keyword has been found, check to see if
!       the file is empty. 1st line should have word LOCATION
        if (lupkeywords(3)) then
            read(up_extract_unit,*,iostat=iflag)junk
            if ((iflag == 0 .and. index(junk,'LOCATION') == 0) .or. iflag /=0) no_extract=.true. !file is empty or bad
            rewind(up_extract_unit)
        else
            no_extract=.true.
        endif
         
!       if no EXTRACT keyword or empty EXTRACT file and no QAOUT keyword and only
!       running stage 1, then error
        if ((no_extract .or. (.not. no_extract .and. .not. lupkeywords(4))) .and. .not. lstage(2)) then
            write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING KEYWORD',trim(adjustl(keywrd(4)))
            lbad=.true.
        endif
    elseif (lupkeywords(1)) then
        writeext=.true.
    endif
          
!   check EXTRACT if processing stage 1 only
!   if stage 1 and 2, then it is optional
    if (upstage1 .and. .not. lstage(2) .and. .not. lupkeywords(3)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING KEYWORD',trim(adjustl(keywrd(7)))
        lbad=.true.
    endif
      
!   check QAOUT if stage 2 only
!   if stage 1 or stage 1 and 2, then it's optional
    if (.not. upstage1 .and. lstage(2) .and. .not. lupkeywords(4)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING KEYWORD',trim(adjustl(keywrd(8)))
        lbad=.true.
    endif
      
!   check XDATES
!   must be there if stage 1 and not stage 2
!   if stage 1 and 2 and no XDATES, then issue warning that
!   xdates will come from METPREP XDATES
    if (upstage1) then
        if (.not. lupkeywords(5)) then
            if (lstage(2)) then
                write(msg_unit,formstr(2))adjustl(pathid(ipath)),'W03',modnam,&
                    'XDATES KEYWORD MISSING; READ XDATES FROM METPREP XDATES'
            else
                write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING KEYWORD',trim(adjustl(keywrd(9)))
                lbad=.true.
            endif
        endif
    endif
      
!   check LOCATION if stage 1 and not stage 2
!   not needed for stage 2
    if (upstage1 .and. .not. lstage(2) .and. .not. lupkeywords(6)) then
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E08',modnam,'MISSING KEYWORD',trim(adjustl(keywrd(10)))
        lbad=.true.
    endif
   
    return
    end subroutine up_test
!*********************************************************************************************************

    subroutine up_proc
!=========================================================================================================
!   SUBROUTINE UP_PROC
!   THIS SUBROUTINE CONTROLS THE READING AND QA OF UPPERAIR DATA FOR STAGE 1
!
!   MODIFIED MARCH 2, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:       AERMET
!
!   Variable definitions
!      
!   Integer variables
!   d:              day loop counter
!   h:              hour loop counter
!   l:              level loop counter
!   idattim:        8-element array to get current date/time from date_and_time subroutine
!   y:              year loop counter
!   m:              month loop counter
!   imon:           start and end month for a year in the data period
!   idy:            start day and end day for a month in the data period
!   ivar1:          calculated audit variable counter (UASS, UADS, UALR, UADD)
!   jj:             extraction reporting loop counter
!   icount:         count of number of soundings (extracted, duplicate, skipped)
!   iday1:          number of days since upstart
!   nup_calc_audit: number of calculated variables to audit (UASS, UADS, UALR, or UADD)
!   ivar:           upper air variable loop counter
!
!   Logical variables
!   leap:           variable denoting if year is a leap year
!
!   Character variables
!   cdate:          date output from data_and_time function
!   formstr:        format string for messages
!   extract_code:   code used for reporting sounding extraction info (# of soundings, duplicates, etc.)
!   extract_msgs:   extraction messages used in conjunction with extract_codes
!   modnam:         Subroutine name
!=========================================================================================================  
    use main1, only: days,leapyr,numdays,eps,istage,msg_form
    use file_units, only: msg_unit,up_qaout_unit,up_extract_unit
    implicit none
    integer(kind=4) :: d,h,l,idattim(8),y,m,imon(2),idy(2),ivar1,jj,icount,iday1,ivar
    integer(kind=4) :: nup_calc_audit=0
    logical :: leap
    character(len=8) :: cdate
    character(len=60) :: formstr(4)
    character(len=3) :: extract_code
    character(len=50) :: extract_msgs(4)
    character(len=100) :: data_form
    character(len=10) :: modnam='UP_PROC'
      
    data extract_msgs /'NUMBER OF EXTRACTED SOUNDINGS:','NUMBER OF UNIQUE VALID SOUNDINGS:','NUMBER OF DUPLICATE SOUNDINGS:',&
        'NUMBER OF SKIPPED SOUNDINGS:'/


!    initialize
    d=0
    h=0
    l=0
    y=0
    m=0
    imon=0
    idy=0
    ivar1=0
    lbad=.false.
    
!   write formats
!   1.  format for extraction start/stop
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a8,1x,i2.2,2(a,i2.2))'
!   2.  format for number of soundings
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5)'
!   3.  no data extracted
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,i8,a)'
!   4. no sounding for the day or no AM sounding
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'i8.8,1x,a)'
    
!   get the number of days in the sounding period
    nupdays=numdays(upstart,upend)
      
!   set nup_audit_vars and nup_calc_audit
    vv: do ivar=1,nupvars
        if (upvars(ivar)%laudit)then
            nup_audit_vars=nup_audit_vars+1
            if (ivar > nupvars_keep)nup_calc_audit=nup_calc_audit+1
        endif
    enddo vv
!   get the number of variables that will be output to
!   updata1, the core variables plus any calculated audited variables
    nupvars1=nupvars_keep+nup_calc_audit
!   allocate the temporary arrays 
!   allocate(updata1(nupvars,maxlevels,24,nupdays))
    allocate(updata1(nupvars1,maxlevels,24,nupdays))
!   allocate qaoutvars if auditing requested or QAOUT file requsted
    if ((lupkeywords(4) .or. lupkeywords(9)) .and. istage == 1 .and. upstage1) allocate(qaout_vars(nupvars1))
    allocate(snd_date(nupdays))
    allocate(snd_hrs(nupdays,24))
    allocate(s_levels(nupdays,24))
    allocate(nsnding1(nupdays))
    allocate(lsound(nupdays))
    

!   initialize temporary arrays; updata1 will be based
!   on the missing values from upvar
    ivar1=0 !counter for calculated audited variables
    v1: do ivar=1,nupvars !nupvars
        if (ivar <= nupvars_keep) then !one of core six variables
            updata1(ivar,:,:,:)=real(upvars(ivar)%missval,r8)*upvars(ivar)%conv
            if(allocated(qaout_vars)) qaout_vars(ivar)=upvars(ivar)%varname
        else !calculated variable
            if (upvars(ivar)%laudit) then !if audited then part of updata1
                ivar1=ivar1+1
                updata1(ivar1+nupvars_keep,:,:,:)=real(upvars(ivar)%missval,r8)*upvars(ivar)%conv
                if(allocated(qaout_vars)) qaout_vars(ivar1+nupvars_keep)=upvars(ivar)%varname
            endif
        endif
    enddo v1
     
!   initialize snd_date to have all dates within the data period
!   even if a day may not have a sounding
    iday1=0
    y1: do y=updates(1,1),updates(2,1)
!       check to see if year is leap year
        call leapyr(y,leap)
!       set initial values of imonth(1) and imonth(2) to
!       1 and 12
        imon(1)=1
        imon(2)=12
!       reset imonth(1) and imonth(2) to first and last
!       month if first or last year of data period
        if (y == updates(1,1)) imon(1)=updates(1,2)
        if (y == updates(2,1)) imon(2)=updates(2,2)
          
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
            if (m == imon(1) .and. y == updates(1,1)) idy(1)=updates(1,3)
            if (m == imon(2) .and. y == updates(2,1))idy(2)=updates(2,3)
    d1:     do d=idy(1),idy(2)
                iday1=iday1+1
                snd_date(iday1)=y*10000+m*100+d
            enddo d1
        enddo m1        
    enddo y1
      
    snd_hrs=-9
    s_levels=1
    nsnding1=0
    lsound=.false.
      
!   read the input data
!   get time time before extraction
    call date_and_time(date=cdate,values=idattim)
     
    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I20',modnam,'UPPER AIR EXTRACTION BEGIN',cdate,idattim(5),':',idattim(6),':',&
        idattim(7)
        
    if (upstage1) then
        if (upformats(1)%lupform .or. upformats(2)%lupform) then
            call read_6201
        elseif (upformats(3)%lupform) then
            call read_fsl
        elseif (upformats(4)%lupform) then
            call read_igra
        else
            call read_ext
        endif
    else
        call read_ext !read data for stage 2
    endif
      
!   write total number of unique soundings extracted, number of
!   valid soundings, number of duplicate soundings,
!   number of skipped soundings
!   if stage 2, unique valid soundings is same as extracted because data has already been extracted in stage 1
    ex1:do jj=23,26
        write(extract_code,'(a1,i2)')'I',jj
        if (jj==23) then
            icount=nsnd(2)
        elseif (jj==24) then
            if (istage == 1 .and. upstage1) then
                icount=nsnd(4)-nsnd(5)
            else
                icount=nsnd(2)
            endif
        elseif (jj==25) then
            icount=nsnd(5)
        else
            icount=nsnd(6)
        endif
        write(msg_unit,formstr(2))adjustl(pathid(ipath)),extract_code,modnam,trim(adjustl(extract_msgs(jj-22))),icount
    enddo ex1
    
!   get time time after extraction
    call date_and_time(date=cdate,values=idattim)
    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I20',modnam,'UPPER AIR EXTRACTION END',cdate,idattim(5),':',idattim(6),':',&    
        idattim(7)
    
    
    if (.not. lbadup) then
!       alert user if no valid obs pulled, may be outside of XDATES
        if ((istage == 1 .and. upstage1 .and. (nsnd(4)-nsnd(5)) == 0) .or. (istage==2 .and. nsnd(2) == 0)) then
            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W34',modnam,'NO EXTRACTED DATA BETWEEN',upstart,'-',upend,&
                '; CHECK XDATES'
            have_soundings=.false.
        else
!           write data to the sound_info data type
            allocate(sound_info(nupdays))
            have_soundings=.true.
            
    d2:     do d=1,nupdays
                sound_info(d)%snddate=snd_date(d) !assign date
                if (nsnding1(d) > 0) then
                    sound_info(d)%nsnding=nsnding1(d) !number of soundings for the day
                else
                    sound_info(d)%nsnding=1 !at least one sounding for array processing purposes
!                   post 21DRF, increment counter for days with no soundings
                    no_sound_days=no_sound_days+1
                    write(msg_unit,formstr(4))adjustl(pathid(ipath)),'I28',modnam,snd_date(d),&
                        'NO SOUNDINGS FOR THE DAY'
                endif
                sound_info(d)%sounding=lsound(d) !if the day has any soundings
          
!               allocate hours array and assign values
!               allocate levels array and assign values
                allocate(sound_info(d)%snd_hr(sound_info(d)%nsnding))
                allocate(sound_info(d)%nlevels(sound_info(d)%nsnding))
                if (nsnding1(d) > 0) then
                    do h=1,nsnding1(d)
!                       post 21DRF, get earliest sounding for the day and if after 1100 LST
!						increment no_am_days
                        if (minval(snd_hrs(d,:)) > 11) then
                            no_am_days=no_am_days+1
                            write(msg_unit,formstr(4))adjustl(pathid(ipath)),'I29',modnam,snd_date(d),&
                                'NO AM SOUNDINGS FOR THE DAY'
                        endif
                        sound_info(d)%snd_hr(h)=snd_hrs(d,h)
                        sound_info(d)%nlevels(h)=s_levels(d,h)
                    enddo
                else
                    sound_info(d)%snd_hr(1)=snd_hrs(d,1)
                    sound_info(d)%nlevels(1)=s_levels(d,1)
                endif
            enddo d2
        
!           set values for celev and cmgm2lst, character values of elevation
!           and GMT to LST time factor.  If not included on LOCATION keyword, they
!           are left blank
!           start here, only need to do this when processing stage 1, regardless of stage 2
            if (istage == 1 .and. upstage1) then
                
!               write non-QA'd data to the EXTRACT file if requested
                if ((lupkeywords(3) .and. writeext)) then
!                   write header
                    call header(up_extract_unit)

!                   write format for data format
                    write(data_form,'(a,i2,a)')'(i8.8,3x,i3,3x,i2.2,1x,i3,',nupvars_keep,'(1x,f8.1))'
                    
!                   write core data to file                   
    d3:             do d=1,nupdays
                        if (.not. sound_info(d)%sounding) cycle d3
    h1:                 do h=1,sound_info(d)%nsnding
    l1:                     do l=1,sound_info(d)%nlevels(h)
                                write(up_extract_unit,data_form)sound_info(d)%snddate,h,sound_info(d)%snd_hr(h),l,&
                                    (updata1(ivar,l,h,d),ivar=1,nupvars_keep) 
                            enddo l1
                        enddo h1
                    enddo d3
                endif

!               modify data 
!               note that in previous versions of AERMET
!               only 6201 formatted data was modified.
!               this version of AERMET will modify regardless
!               of input data type
                if (lupkeywords(7))call up_modify    

!               QA data
!               if AUDIT requested, data will be QA'd and checks for calms, non-zero
!               wind direction and zero wind speed, temperature < dewpoint, and sounding < 5 km are done
!               if AUDIT not requested but QAOUT file is requested
!               only checks for calms, non-zero wind direction and zero wind speed, 
!               temperature < dewpoint, and sounding < 5 km are done.
                if (lupkeywords(9) .or. (lupkeywords(4) .and. upstage1)) call up_audit
        
!               allocate updata array
!               allocate(updata(nupvars,nuplevels,nsound,nupdays))
            endif
!           this is done if stage 1 or stage 2 without stage 1
            allocate(updata(nupvars_keep,nuplevels,nsound,nupdays))
  
!           fill in updata array
    d4:     do d=1,nupdays
    h2:          do h=1,nsound
    l2:             do l=1,nuplevels
    v2:                 do ivar=1,nupvars_keep !nupvars
                            updata(ivar,l,h,d)=updata1(ivar,l,h,d)
                        enddo v2
                    enddo l2
                enddo h2
    enddo d4
    

    
!           write QA'd output to the QAOUT files if requested, only stage 1
            if (istage == 1 .and. upstage1) then
                if (lupkeywords(4) .and. upstage1) then 
!                   write header
                    call header(up_qaout_unit)
!                   write format for data format
                    write(data_form,'(a,i2,a)')'(i8.8,3x,i3,3x,i2.2,1x,i3,',nupvars1,'(1x,f8.1))'
!                   formats will differ depending on if calculated variables audited
!                   write core variables and any calculated variables
    d5:             do d=1,nupdays
                        if (.not. sound_info(d)%sounding) cycle d5
    h3:                 do h=1,sound_info(d)%nsnding
    l3:                     do l=1,sound_info(d)%nlevels(h) 
                                write(up_qaout_unit,data_form)sound_info(d)%snddate,h,sound_info(d)%snd_hr(h),l,&
                                    (updata1(ivar,l,h,d),ivar=1,nupvars1)
                            enddo l3
                        enddo h3
                    enddo d5
                endif
            endif
        endif
    endif
    
!   deallocate temporary arrays, regardless of stage
    if (allocated(updata1)) deallocate(updata1)
    if (allocated(qaout_vars)) deallocate(qaout_vars)
    if (allocated(snd_date)) deallocate(snd_date)
    if (allocated(snd_hrs)) deallocate(snd_hrs)
    if (allocated(s_levels)) deallocate(s_levels)
    if (allocated(nsnding1)) deallocate(nsnding1)
    if (allocated(lsound)) deallocate(lsound)
      
    return
    end subroutine up_proc
!*********************************************************************************************************

    subroutine header(iunit)
!=========================================================================================================
!   SUBROUTINE HEADER
!   THIS SUBROUTINE WRITES THE HEADER FOR THE EXTRACT AND QAOUT FILES
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:       MODULE UPPERAIR (UP_PROC)
!
!   INPUT ARGUMENTS
!
!   IUNIT:          INPUT FILE UNIT
!
!   Variable definitions
!      
!   Integer variables
!   iunit:          input file unit
!   ivar:           upper air variable loop counter
!
!   Character variables
!   celev:          character string of user entered elevation (upelev)
!   cgmt2lst:       character string of time adjustment (upgmt2lst)
!   alat:           character string of latitude with N or S descriptor
!   alon:           character string of longitude with E or W descriptor
!   outtype:        character string denonting output file type for
!                   EXTRACT or QAOUT file header
!   header_form:    header format for EXTRACT and QAOUT files
!   modnam:         Subroutine name
!=========================================================================================================  
    use main1, only: versn
    use file_units, only: up_extract_unit
    implicit none
    integer(kind=4), intent(in) :: iunit
    integer(kind=4) :: ivar
    character(len=2) :: bndstr
    character(len=7) :: outtype(2)
    character(len=7) :: celev=''
    character(len=10) :: alat,alon
    character(len=3) :: cgmt2lst=''
    character(len=100) :: header_form 
    character(len=10) :: modnam='HEADER'
    
    data outtype /'EXTRACT','QAOUT'/
    
!   set values for celev and cmgm2lst, character values of elevation
!   and GMT to LST time factor.  If not included on LOCATION keyword, they
!   are left blank
    if (lgmt2lst) write(cgmt2lst,'(i3)')upgmt2lst
    if (lelev) write(celev,'(f7.3)')upelev
!   set values for alat/alon
    if (uplat < 0.0_r8) then
        write(alat,'(f7.3,a1)')dabs(uplat),'S'
    else
        write(alat,'(f7.3,a1)')uplat,'N'
    endif
    if (uplon < 0.0_r8) then
        write(alon,'(f8.3,a1)')dabs(uplon),'W'
    else
        write(alon,'(f8.3,a1)')uplon,'E'
    endif

!   write AERMET version, LOCATION, filetype, and dates
    write(header_form,'(a)')'(a,a5/a,a8,2(1x,a10),2(1x,a)/2(a)/a,i4,2(1x,i2.2),1x,i4,2(1x,i2.2))'
    write(iunit,header_form)'AERMET ',versn,'LOCATION ',upid,alat,alon,trim(adjustl(cgmt2lst)),&
    trim(adjustl(celev)),'FILE TYPE: ',trim(adjustl(outtype(iunit-up_extract_unit+1))),'DATES ',updates(1,1),updates(1,2),&
    updates(1,3),updates(2,1),updates(2,2),updates(2,3)
    
!   now write any variables who have modified ranges or missing value
    v1: do ivar=1,nupvars
        if (upvars(ivar)%lmod) then
             if (upvars(ivar)%lincbound) then
                bndstr='<='
            else
                bndstr='<'
            endif
            write(iunit,'(a5,1x,a4,i6,1x,a2,1x,2(1x,i6))')'RANGE',upvars(ivar)%varname,upvars(ivar)%lowbound,bndstr,&
                upvars(ivar)%upbound,upvars(ivar)%missval
        endif
    enddo v1
!   write the remainder of the header, the data variable line which is dependent on file type
    
    if (iunit==up_extract_unit) then
!       only write the core upperair variables
        write(header_form,'(a,i2,a)')'(a,4x,a4,',nupvars_keep-1,'(5x,a4))'
        write(iunit,header_form)'DATE       SND   HR  LEV',&
        (trim(adjustl(upvars(ivar)%varname)),ivar=1,nupvars_keep)
    else
!       write the core upper air variables plus the others that are calculated
        write(header_form,'(a,i2,a)')'(a,4x,a4,',nupvars1-1,'(5x,a4))'
        write(iunit,header_form)'DATE       SND   HR  LEV',&
        (trim(adjustl(qaout_vars(ivar))),ivar=1,nupvars1)
    endif
    
    return
    end subroutine header
!*********************************************************************************************************

    subroutine read_fsl
!=========================================================================================================
!   SUBROUTINE READ_FSL
!   THIS SUBROUTINE CONTROLS THE READING OF FSL FORMAT UPPER AIR DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   eof:            integer end of file indicator to read runstream input file
!   iflag1:         I/O indicator when reading data specifi! to line type
!   v:              FSL version number
!   iday1:          number of days since upstart
!   ivar:           upper air variable loop counter
!   nlevs:          maximum number of levels across all soundings in data window
!                   will be used to set nuplevels   
!   readlevs:       2-element number of levels read for the processed sounding
!                   1st element is number of levels read for a sounding
!                   2nd element is number of levels read that are processed for the sounding
!                   these are levels that are not missing and below the top
!   iwarn:          number of times station ID in file mismatches user entered ID
!   iupvars:        6-element array of integer upper air variables read from FSL file
!                   1:  integer pressure (whole mb for original FSL format, mb*10 for new 
!                       format.
!                   2:  height in m
!                   3:  air temperature in Celcius*10
!                   4:  dewpoint temperature in Celcius*10
!                   5:  wind direction in degrees
!                   6:  wind speed in knots or m/s *10 (depending on wind_units)
!   linetype:       data line type:
!                   254: indicates new sounding in file and contains sounding date/time in GMT
!                     1: station information (WBAN, WMO, latitude, longitude, elevation (m), release time
!                     2: sounding checks line (hydro, mxwd, tropl, readlevs, tindex, source) only readlevs used
!                     3: station call sign and wind speed units (kts=knots, ms=m/s*10)
!                     4: mandatory level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     5: signficant level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     6: wind level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     7: tropopause level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     8: maximum wind level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     9: surface level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!   linetype1:      previous value of line type before current line. this is to check to make sure
!                   lines are in correct order
!   iline:          line counter used for error messaging
!   iflag:          I/O indicator
!   iyear:          4-digit year read from data file
!   imonth:         integer month 
!   iday:           integer day of the month
!   ihr:            integer sounding hour 
!                   will be reset to LST hours 1-24 if needed
!   ibase:          surface or lowest level height
!   missval:        2-element array value for missing data from data file
!   sdate:          integer sounding date (YYYYMMDD)
!     
!   Real variables
!   rupvars:        6-element array of real upper air variables in un-scaled units
!                   1:  real pressure in mb after appropriate conversions
!                   2:  real height in m (integer to real)
!                   3:  real air temperature in Celcius
!                   4:  real dewpoint temperature in Celcius
!                   5:  real wind direction in degrees
!                   6:  real wind speed in m/s  
!                   for 6201 format, 1st dimension is # of levels and second
!                   dimension is 6.  Other data formats, the array is 6x1.
!                   values depend on sounding type
!   unit_conv:      1-D array of unit conversions for each rupvar
!   rbase:          real value of surface level height
!   pres_conv:      conversion factor to convert pressure to proper units based on 
!                   FSL version.  if version 1, conversion is 1.0, version 2 is 0.1
!
!   Logical variables
!   lmissvals:      6-element array denoting if variable is missing for level
!   lmonth:         logical variable denoting imonth assigned based on value of inmonth and
!                   index in months array
!   lgo:            logical variable denoting if sounding is in data window
!   lgo1:           logical variable denoting if FSL version determined
!   set_conv:       logical variable denoting pressure conversion has been set
!   lversion:       denotes which FSL version (1) or (2) being used
!                   reset for each sounding. both lversion(1) and lversion(2)
!                   cannot be true in same sounding; both can be true in the same
!                   FSL file.  If both are true in same sounding then issue error
!   lsurface:       logical variable denoting that surface level found
!                   for a sounding
!     
!   Character variables
!   wind_units:     wind units (kt for knots, ms for m/s*10) read from linetype 3
!   datline:        data line read from FSL data file
!   months:         array of 3-character month abbreviations
!   inmonth:        3-character month read from FSL file
!   j1-4:           variables to read in that are not used
!   date_hr_str:    date and hour text string for error messages
!   modnam:         Subroutine name
!=========================================================================================================  
    use, intrinsic :: iso_fortran_env,only : output_unit
    use main1, only: data_dates
    use file_units, only: msg_unit,up_data_unit
    implicit none
    integer(kind=4) :: eof,iflag1,v,iday1,ivar,nlevs,iwarn,linetype,linetype1,iline,iflag,iyear,imonth,iday,ihr,ibase,missval(2)
    integer(kind=4) :: readlevs(2)=0
    integer(kind=4) :: iupvars(6)=0
    integer(kind=8) :: sdate=0
    real(kind=r8) :: pres_conv(2),rbase 
    real(kind=r8) :: unit_conv(6)
    real(kind=r8), allocatable, dimension(:,:) :: rupvars
    logical :: lmonth,lgo,lgo1,set_conv,lversion(2),lsurface,lmissvals(6)
    character(len=2) :: wind_units
    character(len=60) :: datline
    character(len=3):: months(12),inmonth
    character(len=20) :: date_hr_str
    character(len=6) :: j1,j2,j3,j4
    character(len=10) :: modnam='READ_FSL'
      
    data months /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
    
    data pres_conv /1.0_r8,0.1_r8/
      
!   initialize    
    lmissvals=.false.
    linetype1=0
    eof=0
    iflag1=0
    iflag=0
    v=0
    lmonth=.false.
    lgo=.false.
    lgo1=.false.
    lversion=.false.
    lsurface=.false.
    missval(1)=32767
    missval(2)=99999
    allocate(rupvars(6,1))
    rupvars=0.0_r8
    iday1=0
    nlevs=0
    iwarn=0
    iline=0
      
!   set unit conversions
!   pressure and wind speed will be reset based
!   on FSL version
    unit_conv(1)=0.1_r8
    unit_conv(2)=1.0_r8
    unit_conv(3)=0.1_r8
    unit_conv(4)=0.1_r8
    unit_conv(5)=1.0_r8
    unit_conv(6)=0.1_r8
          
!   read first line to make sure possible FSL format
    read(up_data_unit,'(a60)',iostat=eof)datline
    read(datline,*,iostat=iflag)linetype
    if (linetype /= 254) then
        write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E30',modnam,'SOUNDING IS NOT FSL FORMAT LINE:',trim(adjustl(datline))
        lbad=.true.
    endif
    if (.not. lbad) then
        rewind(up_data_unit)
    endif
      
!   read the data lines in as a character string and then
!   assign to variables based on the data line type
    do while (eof == 0 .and. sdate <= upend .and. iflag ==0 .and. iflag1 == 0 .and. .not. lbad)
        read(up_data_unit,'(a60)',iostat=eof)datline
        iline=iline+1
!       check the data line type
        read(datline,*,iostat=iflag)linetype
        
        if (iflag == 0 .and. eof == 0) then
      
!**************  linetype = 254 DATE/TIME data line****************************************
      
            if (linetype == 254) then !date
!               new sounding; check the previous readlev(1) against the previous nlev
!               to make sure didn't under read levels
                if (readlevs(1) < nlevs-4) then
                    write(msg_unit,ext_form(9))adjustl(pathid(ipath)),'E36',modnam,'NUMBER OF LEVELS',readlevs(1),&
                        'IS LESS THAN NUMBER OF EXPECTED LEVELS',nlevs-4,date_hr_str
                    lbad=.true.
                endif
                lsurface=.false. !reset lsurface to false
                readlevs=0
!               check to make sure line is in correct order
!               linetype1 should be between 4 and 9 for linetype 254 (excluding firstline)
!               issue error if not betwen 4 and 9
                if (iline > 1 .and. linetype1 < 4 .or. linetype1 > 9) then
                    write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'E31',modnam,'INVALID ORDER SOUNDING LINE TYPE',linetype,&
                        'FOLLOWS LINE TYPE',linetype1,'LINE NUMBER',iline
                    lbad=.true.
                else
                    if (eof ==0 .and. .not. lbad) nsnd(1)=nsnd(1)+1  !add to total soundings being read
                    read(datline,*,iostat=iflag1)j1,ihr,iday,inmonth,iyear
                    if (iflag1 ==0) then
!                       convert the text month to a numerical month
                        imonth=1
                        lmonth=.false.
                        do while (imonth <= 12 .and. .not. lmonth)
                            if (trim(adjustl(inmonth)) == trim(adjustl(months(imonth)))) then
                                lmonth=.true.
                            else
                                imonth=imonth+1
                            endif
                        enddo
                          
!                       check the date if the month was found
!                       otherwise issue error that month couldn't be found
                        if (lmonth) then  !calculate date and see if inside the data window
                            call data_dates(ihr,iday,imonth,iyear,upgmt2lst,upstart,upend,32,sdate,lgo)
                            write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
                            if (lgo) call new_sound(sdate,ihr,iday1) !increment/initialize key variables for new sounding
                        else !couldn't find the month, bad data
                            write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(2))),&
                                nsnd(1),'; LINE',trim(adjustl(datline))
                            lbad=.true.
                        endif
                          
                    else  !invalid date line
                        write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(1))),&
                                nsnd(1),'; LINE',trim(adjustl(datline))
                        lbad=.true.
                    endif
                endif
                linetype1=linetype !reset lintype1 to 254
!**************  linetype = 254 DATE/TIME data line****************************************
                  
!**************  linetype = 1 STATION data line****************************************                  
            elseif (linetype == 1) then !station info
!               check to make sure lintype1 is 254, if not, issue error                      
                if (linetype1 /= 254) then
                    write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'E31',modnam,'INVALID ORDER SOUNDING LINE TYPE',linetype,&
                        'FOLLOWS LINE TYPE',linetype1,'LINE NUMBER',iline
                    lbad=.true.
                else
                    read(datline,*,iostat=iflag1)j1,iupid
                    if (iflag1 == 0) then
!                       only check the station ID if within the date window
!                       this is a departure from previous AERMET versions where
!                       the station check was done outside of the date check
                        if (trim(adjustl(iupid)) /= trim(adjustl(upid)) .and. lgo) then
                            iwarn=iwarn+1
!                           write warning message if still below the limit of message writes
                            if (iwarn <= nwarnlim) write(msg_unit,ext_form(4))adjustl(pathid(ipath)),'W30',modnam,&
                                'STATION ID FROM FILE',trim(adjustl(iupid)),'DOES NOT MATCH USER ENTERED STATION ID',&
                                trim(adjustl(upid)),'FOR SOUNDING NUMBER',nsnd(1),date_hr_str
!                           only this message once
                            if (iwarn==nwarnlim)write(msg_unit,ext_form(5))adjustl(pathid(ipath)),'W33',modnam,'REACH LIMIT OF',&
                                nwarnlim,'W30','WARNING MESSAGES'
                        endif
                        
                    else
                        write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(3))),&
                                nsnd(1),'; LINE',trim(adjustl(datline))
                        lbad=.true.
                    endif
                endif
                linetype1=linetype !reset linetype1 to 1
!**************  linetype = 1 STATION data line**************************************** 
                  
!**************  linetype = 2 NUMBER OF LEVELS data line**************************************** 
            elseif (linetype == 2) then !sounding check line; get # of levels
!               check to make sure lintype1 is 1, if not, issue error 
                if (linetype1 /= linetype-1) then
                    write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'E31',modnam,'INVALID ORDER SOUNDING LINE TYPE',linetype,&
                        'FOLLOWS LINE TYPE',linetype1,'LINE NUMBER',iline
                    lbad=.true.
                else
                    read(datline,*,iostat=iflag1)j1,j2,j3,j4,nlevs
                    if (iflag1 == 0) then
                        readlevs=0  !initialize
                        if (lgo) then !initialize variables
!                           check the FSL version; use missing value for pressure
!                           since not needed for this linetype
                            call fsl_versn(datline,nsnd(1),linetype,upvars(1)%missval,v,set_conv,lgo1)
                            if (v > 0) lversion(v)=.true.
!                           check to see if both lversion(1) and lversion(2) are true
!                           if so, issue error
                            if (lversion(1) .and. lversion(2)) then
                                write(msg_unit,ext_form(8))adjustl(pathid(ipath)),'E35',modnam,&
                                    'FSL VERSION 1 AND 2 BOTH DETECTED FOR SOUNDING #',nsnd(1),date_hr_str
                                lbad=.true.
                            endif
                        endif
                    else
                        write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(5))),&
                                nsnd(1),'; LINE',trim(adjustl(datline))
                        lbad=.true.
                    endif
                endif
                linetype1=linetype !reset linetype1 to 2
!**************  linetype = 2 NUMBER OF LEVELS data line****************************************
                  
!**************  linetype = 3 WIND SPEED UNITS data line****************************************
            elseif (linetype == 3) then !wind speed units
!               check to make sure lintype1 is 2, if not, issue error 
                if (linetype1 /= linetype-1) then
                    write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'E31',modnam,'INVALID ORDER SOUNDING LINE TYPE',linetype,&
                        'FOLLOWS LINE TYPE',linetype1,'LINE NUMBER',iline
                    lbad=.true.
                else
                    read(datline,'(47x,a2)')wind_units
                    if (lgo) then
!                       check the FSL version; use missing value for pressure
!                       since not needed for this linetype
                        call fsl_versn(datline,nsnd(1),linetype,upvars(1)%missval,v,set_conv,lgo1)
                        if (v > 0) lversion(v)=.true.
!                       check to see if both lversion(1) and lversion(2) are true
!                       if so, issue error
                        if (lversion(1) .and. lversion(2)) then
                            write(msg_unit,ext_form(8))adjustl(pathid(ipath)),'E35',modnam,&
                                'FSL VERSION 1 AND 2 BOTH DETECTED FOR SOUNDING #',nsnd(1),date_hr_str
                            lbad=.true.
                        endif
                          
                        if (trim(adjustl(wind_units)) == 'kt') then
                            unit_conv(6)=0.51_r8
                        elseif (trim(adjustl(wind_units)) == 'ms') then
                            unit_conv(6)=0.1_r8
                        else
                            write(msg_unit,ext_form(6))adjustl(pathid(ipath)),'E33',modnam,'INVALID WIND SPEED UNITS:',&
                                trim(adjustl(wind_units)),'FOR SOUNDING #',nsnd(1)
                            lbad=.true.
                        endif
                    endif
                endif
                linetype1=linetype !reset linetype1 to 3
!**************  linetype = 3 WIND SPEED UNITS data line****************************************
                  
!**************  linetype = 4-9 SOUNDING data data line****************************************
            elseif (linetype >= 4) then
!               invalid line types
!               if linetype is 9, only valid valid value for linetype1 is 3
!               if linetype is 4 through 8, only valid values for linetype1 
!               is 4 through 9 (this would assume a missing surface level) 
                if ((linetype == 9 .and. linetype1 /= 3) .or. (linetype /= 9 .and. lsurface .and. (linetype1 <= 3 .or. &
                    linetype1 == 254))) then
                    write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'E31',modnam,'INVALID ORDER SOUNDING LINE TYPE',linetype,&
                        'FOLLOWS LINE TYPE',linetype1,'LINE NUMBER',iline
                    lbad=.true.
                endif
!               if linetype = 9, check to see if linetype = 9 already 
!               read in; if already seen, issue error
                if (linetype == 9) then
                    if (lsurface) then
                        write(msg_unit,ext_form(12))adjustl(pathid(ipath)),'E34',modnam,&
                            'MULTIPLE SURFACE DATA LINES FOR SOUNDING #',nsnd(1),date_hr_str
                        lbad=.true.
                    else
                        lsurface=.true.
                    endif
                endif
                if (.not. lbad) then
!                   read in integer variables: pressure, height, temperature, dewpoint, 
!                   wind direction, wind speed
!                   iupvars order corresponds to upvars order
                    read(datline,*,iostat=iflag1)j1,(iupvars(ivar),ivar=1,6)
                    readlevs(1)=readlevs(1)+1
!                   if number of levels exceeds expected number of levels 
!                   indicated by linetype 2 issue error.
                    if (readlevs(1) > nlevs-4) then 
                        write(msg_unit,ext_form(9))adjustl(pathid(ipath)),'E36',modnam,'NUMBER OF LEVELS',readlevs(1),&
                        'EXCEEDS NUMBER OF EXPECTED LEVELS',nlevs-4,date_hr_str
                        lbad=.true.
                    endif
!                   process if within date window
                      

                    if (lgo) then
                        if (iflag1 == 0 .and. readlevs(1) <= maxlevels.and. .not. ltop) then
                            call fsl_versn(datline,nsnd(1),linetype,iupvars(1),v,set_conv,lgo1)
                            if (v > 0) lversion(v)=.true.
!                           check to see if both lversion(1) and lversion(2) are true
!                           if so, issue error
                            if (lversion(1) .and. lversion(2)) then
                                write(msg_unit,ext_form(8))adjustl(pathid(ipath)),'E35',modnam,&
                                'FSL VERSION 1 AND 2 BOTH DETECTED FOR SOUNDING #',nsnd(1),date_hr_str
                                lgo1=.false.
                            endif
                              
!                           FSL version determined, proceed
                            if (lgo1) then
                                unit_conv(1)=pres_conv(v) !reset pressure units conversion
                                readlevs(2)=readlevs(2)+1
!                               if first level not the surface, skip processing
                                if (readlevs(1) == 1 .and. linetype/= 9) then
                                    lskip=.true.
                                    write(msg_unit,ext_form(9))adjustl(pathid(ipath)),'W31',modnam,'SKIP SOUNDING; 1ST LEVEL TYPE',&
                                        linetype,'NOT TYPE 9, FOR SOUNDING #',nsnd(1),date_hr_str
                                    nsnd(6)=nsnd(6)+1
!                                   reset snd_hrs to -9 and s_levels to 0 so that AERMET will know 
!                                   not to process this sounding later in stage 2
!                                   also take 1 away from nsnding1 and nsnd(3) for the day
                                    snd_hrs(iday1,nsnd(3))=-9
                                    s_levels(iday1,nsnd(3))=0
                                    nsnding1(iday1)=nsnding1(iday1)-1
                                    nsnd(3)=nsnd(3)-1
                                elseif (readlevs(1) == 1) then
!                                   if the surface level set ibase based on height
                                    if (iupvars(2) /= missval(v)) then
                                        ibase=iupvars(2)
                                        rbase=real(iupvars(2),r8)
                                    else !skip the sounding if no height
                                        ibase=0
                                        rbase=0.0_r8
                                        lskip=.true.
                                        nsnd(6)=nsnd(6)+1
                                        write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W32',modnam,&
                                            'SKIP SOUNDING; SURFACE LEVEL HEIGHT MISSING FOR SOUNDING #',nsnd(1),date_hr_str
!                                       reset snd_hrs to -9 and s_levels to 0 so that AERMET will know 
!                                       not to process this sounding later in stage 2
!                                       also take 1 away from nsnding1 for the day
                                        snd_hrs(iday1,nsnd(3))=-9
                                        s_levels(iday1,nsnd(3))=0
                                        nsnding1(iday1)=nsnding1(iday1)-1
                                    endif
                                endif

!                               calculate real values of variables if not missing
!                               note rupvars(2,1) (height) will be recalculated below
                                lmissvals=.false.
    v1:                         do ivar=1,6
                                    if (iupvars(ivar) == missval(v)) then
                                        lmissvals(ivar)=.true.
                                        rupvars(ivar,1)=real(upvars(ivar)%missval,r8)!*upvars(ivar)%conv
                                    else
                                        rupvars(ivar,1)=real(iupvars(ivar),r8)*unit_conv(ivar)
                                        if (ivar==2) rupvars(ivar,1)=rupvars(ivar,1)-rbase
                                    endif
                                enddo v1
!                               only process if temperature, pressure, and height not missing
!                               and pressure not equal to savepress (previous level's pressure)
!                               and not skipping sounding
!                               also skip if linetype = 6 (the checks above should take of this but
!                               adding to make sure. linetype 6 is wind data only

                                if (.not. lmissvals(3) .and. .not. lmissvals(1) .and. iupvars(1) /= savepress .and. .not. &
                                    lmissvals(2) .and. .not. lskip .and. linetype /=6) then
                                    writelevs=writelevs+1
                                    s_levels(iday1,nsnd(3))=writelevs !increment s_levels
                                      
!                                   check writelevs against nuplevels only if within data window
!                                   reset nuplevels to writelevs if writelevs larger than nuplevels
                                    if (writelevs > nuplevels) nuplevels=writelevs
                                  
                                    savepress=iupvars(1) !set savepress to pressure
                                      
                                    if (iupvars(2) > uptop+ibase) ltop=.true.!exceed top
                              
!                                   write data to temporary array
    v2:                             do ivar=1,6
                                        updata1(ivar,writelevs,nsnd(3),iday1)=rupvars(ivar,1)
                                    enddo v2
                                endif                     
                            else !lgo1 bad
                                lbad=.true.
                            endif
                        elseif (iflag1 /=0) then !error reading data
                            write(msg_unit,ext_form(11))adjustl(pathid(ipath)),'E31',modnam,&
                                'ERROR READING UPPER AIR DATA SOUNDING #',nsnd(1),'; LINE',trim(adjustl(datline)),date_hr_str
                            lbad=.true.
                        endif
                    endif !end lgo if
                endif
                  
                linetype1=linetype !reset to linetype value
!**************  linetype = 4-9 SOUNDING data data line****************************************
                                    
            else !line type invalid
                write(msg_unit,ext_form(7))adjustl(pathid(ipath)),'E31',modnam,'INVALID LINE TYPE',linetype,'FOR SOUNDING #',&
                    nsnd(1),'LINE',trim(adjustl(datline))
                linetype1=linetype
                lbad=.true.
            endif !end linetype processing logic
              
        else !line type not determined 
            if (eof == 0) then
                write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(4))),nsnd(1),'; LINE',&
                    trim(adjustl(datline))
                lbad=.true.
            endif
        endif !end linetype check
        
!       at end of loop, check to see if lbad is true
!       and if so reset snd_hrs to -9 and s_levels to 0 so that AERMET will know 
!       not to process this sounding later in stage 2
!       also take 1 away from nsnding1 for the day
        if (lbad) then
            if (iday1 > 0) then
                snd_hrs(iday1,nsnd(3))=-9
                s_levels(iday1,nsnd(3))=0
                nsnding1(iday1)=nsnding1(iday1)-1
            endif
        endif
!       increment number of valid soundings
        if (.not. lskip .and. linetype==9 .and. lgo) nsnd(4)=nsnd(4)+1
    enddo
                  
    if (.not. lbadup .and. lbad)lbadup=.true. 
      
!   final messages
    if (.not. lbadup) then
!       reached data that exceeded xdates,issue message
        write(msg_unit,ext_form(13))adjustl(pathid(ipath)),'I22',modnam,trim(adjustl(ext_endstr(1)))
    elseif (nsnd(2) == 0 .and. .not. lbadup) then
!       no soundings extracted but no errors, issue error
        write(msg_unit,ext_form(13))adjustl(pathid(ipath)),'E37',modnam,trim(adjustl(ext_endstr(2)))
    elseif (lbadup .and. nsnd(1) > 0) then
!       soundings extracted but error
        write(msg_unit,ext_form(14))adjustl(pathid(ipath)),'E38',modnam,trim(adjustl(ext_endstr(4))),nsnd(1)
    elseif (lbadup .and. nsnd(1) == 0) then
!       error and no soundings extracted
        write(msg_unit,ext_form(13))adjustl(pathid(ipath)),'E39',modnam,trim(adjustl(ext_endstr(3)))
    endif
      
!   assign value to totsounds
!    totsounds=nsnd(2)
      
!   check to see if any sounding days need to
!   be reset to not have any soundings due to skips or
!   bad data
    d1: do iday1=1,nupdays
        if (lsound(iday1) .and. nsnding1(iday1) == 0) then
            lsound(iday1)=.false.
            nsnding1(iday1)=1 !for array processing purposes
        endif
    enddo d1

    if (allocated(rupvars)) deallocate(rupvars)
    
    return
    end subroutine read_fsl
!*********************************************************************************************************

    subroutine fsl_versn(datline,nsnding,linetype,pressure,fsl_version,set_conv,lgood)
!=========================================================================================================
!   SUBROUTINE FSL_VERSN
!   THIS SUBROUTINE DETERMINES THE FSL FILE FORMAT VERSION: 1 OR 2
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:  MODULE UPPERAIR (READ_FSL)
!
!   INPUT ARGUMENTS
!
!   DATLINE:    DATA LINE READ FROM FSL FILE
!   NSNDING:    SOUNDING NUMBER
!   LINETYPE:   LINETYPE (4 THROUGH 9)
!   PRESSURE:   PRESSURE FOR LEVEL BEING READ
!
!   OUTPUT ARGUMENTS:
!   FSL_VERSION:    FSL VERSION, 1 OR 2
!   SET_CONV:       LOGICAL VARIABLE DENOTING THAT 
!                   CONVERSION FACTOR HAS BEEN SET
!   LGOOD:          LOGICAL VARIABLE DENOTING THAT VALID 
!                   VERSION SET
!
!   Variable definitions
!      
!   Integer variables
!   fsl_version:    FSL version, 1 or 2
!   nv1:            index value for searching for 32767 in data line
!   nv2:            index value for searching for 99999 in data line
!   pressure:       integer pressure (whole mb for original FSL format, mb*10 for new 
!                   format.
!   linetype:       data line type:
!                   254: indicates new sounding in file and contains sounding date/time in GMT
!                     1: station information (WBAN, WMO, latitude, longitude, elevation (m), release time
!                     2: sounding checks line (hydro, mxwd, tropl, readlevs, tindex, source) only readlevs used
!                     3: station call sign and wind speed units (kts=knots, ms=m/s*10)
!                     4: mandatory level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     5: signficant level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     6: wind level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     7: tropopause level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     8: maximum wind level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!                     9: surface level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!   nsnding:        sounding number
!     
!   Logical variables
!   lgood:          logical variable denoting error with finding version number
!   set_conv:       logical variable denoting pressure conversion has been set
!     
!   Character variables
!   modnam:         Subroutine name
!   formstr:        formats for messages
!   datline:        Data line read from file
!========================================================================================================= 
    use file_units, only: msg_unit
    use main1, only: msg_form
    implicit none
!   inputs, do not initialize
    integer(kind=4), intent(in) :: nsnding,linetype,pressure
    integer(kind=4), intent(out) :: fsl_version
    integer(kind=4) :: nv1,nv2
    logical, intent(out) :: set_conv,lgood
    character(len=10) :: modnam='FSL_VERSN'
    character(len=60) :: formstr(2)
    character(len=60), intent(in) :: datline
      
!   initialize
    nv1=0
    nv2=0
    lgood=.true.
      
!   formats for messages
!   1.  multiple missing value indicators
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,i5,2(1x,a))'
    
!   2.  cannot determine pressure conversion factor
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a)'
    
!   first check to see what the potential missing indicator
!   is to determine the FSL version
!   if missing indicator is 32767 then it's the old version
!   and pressures are in mb.
!   if missing indicator is 99999 then it's the new version
!   and pressures are in mb*10
!   if no missing indicators found, look at the pressure
!   and determine based on its magnitude
      
    nv1=index(datline, ' 32767 ')
    nv2=index(datline, ' 99999 ')

    if (nv1 > 0 .and. nv2 > 0) then  !can't have both on same line, error
        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E35',modnam,'MULTIPLE MISSING VALUE INDICATORS FOR SOUNDING #',nsnding,&
            '; LINE',trim(adjustl(datline))
        lgood=.false.
    elseif (nv1 > 0) then !version 1
        fsl_version=1
        set_conv=.true.
    elseif (nv2 > 0) then !version 2
        fsl_version=2
        set_conv=.true.
    elseif (linetype == 9 .and. pressure >= 6500 .and. pressure <= 11000) then !pressure is 6500 mb or greater, version 2
        fsl_version=2
        set_conv=.true.
    elseif (linetype == 9 .and. pressure >= 650 .and. pressure <= 1100) then  !pressure is 650 mb or greater, version 1
        fsl_version=1
        set_conv=.true.
    elseif (.not. set_conv .and. linetype > 3) then
        write(msg_unit,formstr(2))adjustl(pathid(ipath)),'E35',modnam,'CANNOT DETERMINE PRESSURE CONVERSION FACTOR'
        lgood=.false.
        fsl_version=0
    endif
      
    return
    end subroutine fsl_versn
!*********************************************************************************************************

    subroutine read_igra
!=========================================================================================================
!   SUBROUTINE READ_IGRA
!   THIS SUBROUTINE CONTROLS THE READING OF IGRA FORMAT UPPER AIR DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   eof:            integer end of file indicator to read runstream input file
!   k:              looping variable
!   k1:             index to start reading station ID from j1
!   rtime:          actual launch time
!   iflag1:         I/O indicator when reading data specific to line type
!   iflag2:         I/O indicator
!   itime:          time
!   iday1:          number of days since upstart
!   irh:            relative humidity read from file
!   ivar:           upper air variable loop counter
!   nlevs:          maximum number of levels across all soundings in data window
!                   will be used to set nuplevels     
!   readlevs:       2-element number of levels read for the processed sounding
!                   1st element is number of levels read for a sounding
!                   2nd element is number of levels read that are processed for the sounding
!                   these are levels that are not missing and below the top
!   iwarn:          number of times station ID in file mismatches user entered ID
!   iupvars:        6-element array of integer upper air variables read from FSL file
!                   1:  integer pressure (whole mb for original FSL format, mb*10 for new 
!                       format.
!                   2:  height in m
!                   3:  air temperature in Celcius*10
!                   4:  dewpoint temperature in Celcius*10
!                   5:  wind direction in degrees
!                   6:  wind speed in knots or m/s *10 (depending on wind_units)
!   linetype:       data line type:
!				      1: date/time
!				  10-40: mandatory level (pressure, height, airtemp, dewpoint, wind_dir wind_speed)
!   linetype1:      previous value of line type before current line. this is to check to make sure
!                   lines are in correct order
!   iline:          line counter used for error messaging
!   iflag:          I/O indicator
!   iyear:          4-digit year read from data file
!   imonth:         integer month 
!   iday:           integer day of the month
!   ihr:            integer sounding hour 
!                   will be reset to LST hours 1-24 if needed
!   ibase:          surface or lowest level height
!   missval:        2-element array value for missing data from data file
!   sdate:          integer sounding date (YYYYMMDD)
!
!   Real variables
!   rupvars:        6-element array of real upper air variables in un-scaled units
!                   1:  real pressure in mb after appropriate conversions
!                   2:  real height in m (integer to real)
!                   3:  real air temperature in Celcius
!                   4:  real dewpoint temperature in Celcius
!                   5:  real wind direction in degrees
!                   6:  real wind speed in m/s  
!                   for 6201 format, 1st dimension is # of levels and second
!                   dimension is 6.  Other data formats, the array is 6x1.
!                   values depend on sounding type
!   rbase:          real value of surface level height
!   unit_conv:      1-D array of unit conversions for each rupvar
!
!   Logical variables
!   lmissvals:      6-element array denoting if variable is missing for level
!   lgo:            logical variable denoting if sounding is in data window
!   lsurface:       logical variable denoting that surface level found
!                     for a sounding
!     
!   Character variables
!   datline:        data line read from IGRA data file
!   j1:             variable to read in that are not used
!   date_hr_str:    date and hour text string for error messages
!   modnam:         Subroutine name
!=========================================================================================================  
    use, intrinsic :: iso_fortran_env,only : 
    use main1, only: data_dates
    use file_units, only: msg_unit,up_data_unit
    implicit none
    integer(kind=4) :: eof,k,k1,rtime,irh,itime,iflag1,iflag2,iday1,ivar,nlevs,iwarn,linetype,linetype1,iline,iflag,iyear,imonth,&
        iday,ihr,ibase,missval(2)
    integer(kind=4) :: readlevs(2)=0
    integer(kind=4) :: iupvars(6)=0
    integer(kind=8) :: sdate=0
    real(kind=r8), allocatable, dimension(:,:) :: rupvars
    real(kind=r8) :: unit_conv(6)
    real(kind=r8) :: rbase
    logical :: lgo,lsurface,lmissvals(6)

    character(len=20) :: date_hr_str
    character(len=75) :: datline
    character(len=12) :: j1 
    character(len=10) :: modnam='READ_IGRA'

    
!   initialize
    linetype1=0
    lmissvals=.false.
    eof=0
    iday1=0
    k=0
    k1=0
    rtime=0
    irh=0
    itime=0
    iflag1=0
    iflag2=0
    iflag=0
    iwarn=0
    rbase=0.0_r8
    lgo=.false.
    lsurface=.false.
    missval(1)=-9999
    missval(2)=-8888
    allocate(rupvars(6,1))
    rupvars=0.0_r8
    nlevs=0
    iline=0

!   set unit conversions
    unit_conv(1)=0.01_r8
    unit_conv(2)=1.0_r8
    unit_conv(3)=0.1_r8
    unit_conv(4)=0.1_r8
    unit_conv(5)=1.0_r8
    unit_conv(6)=0.1_r8
    
!   read first line to make sure possible IGRA format
    read(up_data_unit,'(a75)',iostat=eof)datline
    if (datline(1:1) /= '#') then
        write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E30',modnam,'SOUNDING IS NOT IGRA FORMAT LINE:',trim(adjustl(datline))
        lbad=.true.
    endif
    if (.not. lbad) then
        rewind(up_data_unit)
    endif
      
!   read the data lines in as a character string and then
!   assign to variables based on the data line type
    do while (eof == 0 .and. sdate <= upend .and. iflag ==0 .and. iflag1 == 0 .and. .not. lbad)
        read(up_data_unit,'(a75)',iostat=eof)datline
        iline=iline+1
!       get the line type
        if (datline(1:1) == '#') then 
            linetype=1
            iflag=0
        else
            read(datline(1:2),*,iostat=iflag)linetype
!           reset some line types for easier coding
            if (linetype == 1) linetype=30 !reset to avoid problems with header line    
        endif
        if (iflag == 0 .and. eof == 0) then
      
!**************  linetype = 1 DATE/TIME data line****************************************
      
            if (linetype == 1) then !date
!               new sounding; check the previous readlev(1) against the previous nlev
!               to make sure didn't under read levels
                if (readlevs(1) < nlevs .and. iline > 1) then
                    write(msg_unit,ext_form(9))adjustl(pathid(ipath)),'E36',modnam,'NUMBER OF LEVELS',readlevs(1),&
                        'IS LESS THAN NUMBER OF EXPECTED LEVELS',nlevs,date_hr_str
                    lbad=.true.
                endif
                readlevs=0
                lsurface=.false. !reset lsurface to false
                savepress=0  !reset save pressure
!               check to make sure line is in correct order
!               linetype1 should be > 10 for linetype 1 (excluding firstline)
!               issue error if < 10
                if (iline > 1 .and. linetype1 < 10) then
                    write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'E31',modnam,'INVALID ORDER SOUNDING LINE TYPE',linetype,&
                        'FOLLOWS LINE TYPE',linetype1,'LINE NUMBER',iline
                    lbad=.true.
                else
                    if (eof ==0 .and. .not. lbad) nsnd(1)=nsnd(1)+1  !add to total soundings being read
                    read(datline,*,iostat=iflag1)j1,iyear,imonth,iday,ihr,rtime,nlevs
                    if (iflag1 ==0) then

!                       check the date
!                       first check the hour to see if between 0 and 24
!                       older data in the IGRA format has 99 for hour
!                       if hour is 99, check the release time and if
!                       the release time is not 9999 then use the 
!                       release time to get the hour
!                       if can't determine hour from the release time,
!                       skip sounding.
                        if (ihr == 99 .and. rtime /= 9999)ihr=(rtime/100)   
                        if (ihr == 99) then
                            lgo=.false.
                        else
                            call data_dates(ihr,iday,imonth,iyear,upgmt2lst,upstart,upend,32,sdate,lgo)
                            write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
                        endif
                        if (lgo) then
                            call new_sound(sdate,ihr,iday1) !increment/initialize key variables for new sounding
!                           get the station identifier
!                           find first occurrence of non-letter character
                            k=1
                            k1=0
                            do while (k <= len_trim(j1) .and. k1 == 0)
                                if (ichar(j1(k:k)) >= 48 .and. ichar(j1(k:k)) <= 57) then
                                    k1=k
                                else
                                    k=k+1
                                endif
                            enddo
                            read(j1(k1:len_trim(j1)),*,iostat=iflag2)iupid
                            if (iflag2 == 0) then
!                               only check the station ID if within the date window
!                               this is a departure from previous AERMET versions where
!                               the station check was done outside of the date check    
                                if (trim(adjustl(iupid)) /= trim(adjustl(upid))) then
                                    iwarn=iwarn+1
!                                   write warning message if still below the limit of message writes
                                    if (iwarn <= nwarnlim) write(msg_unit,ext_form(4))adjustl(pathid(ipath)),'W30',modnam,&
                                        'STATION ID FROM FILE',trim(adjustl(iupid)),'DOES NOT MATCH USER ENTERED STATION ID',&
                                        trim(adjustl(upid)),'FOR SOUNDING NUMBER',nsnd(1),date_hr_str
                                    if (iwarn==nwarnlim)write(msg_unit,ext_form(5))adjustl(pathid(ipath)),'W33',modnam,&
                                        'REACH LIMIT OF',nwarnlim,'W30','WARNING MESSAGES'
                                endif
                            else
                                write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(3))),&
                                    nsnd(1),'; LINE',trim(adjustl(datline))
                                lbad=.true.
                            endif 
                        endif
                    else  !invalid date line
                        write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(1))),&
                                nsnd(1),'; LINE',trim(adjustl(datline))
                        lbad=.true.
                    endif
                endif
                linetype1=linetype !reset lintype1 to 1
!**************  linetype = 1 DATE/TIME data line****************************************
                  
                  
!**************  linetype 10 -40 SOUNDING data data line****************************************
            elseif (linetype >= 10 .and. linetype < 40) then
!               invalid line types
!               if linetype is 21 or 11 only valid valid value for linetype1 is 1
!               otherwise, can be 1 or > 10 

                if (((linetype == 21 .or. linetype == 11) .and. linetype1 /= 1) .or. ((linetype /= 21 .and. linetype /= 11) .and. &
                    lsurface .and. linetype1 == 1))then
                    write(msg_unit,ext_form(2))adjustl(pathid(ipath)),'E31',modnam,'INVALID ORDER SOUNDING LINE TYPE',linetype,&
                        'FOLLOWS LINE TYPE',linetype1,'LINE NUMBER',iline
                    lbad=.true.
                endif
!               if linetype = 21 or 11, check to see if linetype = 21 or 11 already 
!               read in; if already seen, issue error
                if (linetype == 21 .or. linetype == 11) then
                    if (lsurface) then
                        write(msg_unit,ext_form(12))adjustl(pathid(ipath)),'E34',modnam,&
                            'MULTIPLE SURFACE DATA LINES FOR SOUNDING #',nsnd(1),date_hr_str
                        lbad=.true.
                    else
                        lsurface=.true.
                    endif
                endif
                if (.not. lbad) then
!                   datline may have the letter B at the end of several variables
!                   read through datline and replace B with a blank
    kk1:            do k=1,len_trim(datline)
                        if (datline(k:k) == 'B' .or. datline(k:k) == 'A') datline(k:k)=' '
                    enddo kk1
!                   read in integer variables: pressure, height, temperature, rh, dewpoint, 
!                   wind direction, wind speed
!                   iupvars order corresponds to upvars order but add RH in even though not kept
                    read(datline,*,iostat=iflag1)j1,itime,(iupvars(ivar),ivar=1,3),irh,(iupvars(ivar),ivar=4,6)
                    readlevs(1)=readlevs(1)+1
!                   if number of levels exceeds expected number of levels 
!                   indicated by linetype 1 issue error.
                    if (readlevs(1) > nlevs) then 
                        write(msg_unit,ext_form(9))adjustl(pathid(ipath)),'E36',modnam,'NUMBER OF LEVELS',readlevs(1),&
                        'EXCEEDS NUMBER OF EXPECTED LEVELS',nlevs,date_hr_str
                        lbad=.true.
                    endif
!                   process if within date window
                    if (lgo) then
                          if (iflag1 == 0 .and. readlevs(1) <= maxlevels .and. .not. ltop) then
                            readlevs(2)=readlevs(2)+1
!                           if first sounding not the surface, skip processing
                            if (readlevs(1) == 1 .and. (linetype /= 21 .and. linetype /= 11)) then
                                lskip=.true.
                                nsnd(6)=nsnd(6)+1
                                write(msg_unit,ext_form(9))adjustl(pathid(ipath)),'W31',modnam,'SKIP SOUNDING; 1 LEVEL TYPE',&
                                        linetype,'NOT TYPE 11 or 21, FOR SOUNDING #',nsnd(1),date_hr_str
!                               reset snd_hrs to -9 and s_levels to 0 so that AERMET will know 
!                               not to process this sounding later in stage 2
!                               also take 1 away from nsnding1 and nsnd(3) for the day
                                snd_hrs(iday1,nsnd(3))=-9
                                s_levels(iday1,nsnd(3))=0
                                nsnding1(iday1)=nsnding1(iday1)-1
                                nsnd(3)=nsnd(3)-1
                            elseif (readlevs(1) == 1) then
!                               if the surface level set ibase based on height
                                if (iupvars(2) /= missval(1) .and. iupvars(2) /= missval(2)) then
                                    ibase=iupvars(2)
                                else !skip the sounding if no height
                                    ibase=0
                                    lskip=.true.
                                    nsnd(6)=nsnd(6)+1
                                    write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W32',modnam,&
                                            'SKIP SOUNDING; SURFACE LEVEL HEIGHT MISSING FOR SOUNDING #',nsnd(1),date_hr_str
!                                   reset snd_hrs to -9 and s_levels to 0 so that AERMET will know 
!                                   not to process this sounding later in stage 2
!                                   also take 1 away from nsnding1 for the day
                                    snd_hrs(iday1,nsnd(3))=-9
                                    s_levels(iday1,nsnd(3))=0
                                    nsnding1(iday1)=nsnding1(iday1)-1
                                endif
                            endif

!                           calculate real values of variables if not missing
!                           note rupvars(2,1) (height) will be recalculated below
!                           calculate dewpoint from dewpoint depression and temperature
!                           if both non-missing, otherwise if one is missing, set to missing
                            lmissvals=.false.
    v1:                     do ivar=1,6
                                if (iupvars(ivar) == missval(1) .or. iupvars(ivar) == missval(2)) then
                                    lmissvals(ivar)=.true.
                                    rupvars(ivar,1)=real(upvars(ivar)%missval,r8)!*upvars(ivar)%conv
                                else
                                    if (ivar == 4) then !calculate dewpoint from temperature dewpoint depress
                                        if (lmissvals(3)) then
                                            lmissvals(ivar)=.true.
                                            rupvars(ivar,1)=real(upvars(ivar)%missval,r8)!*upvars(ivar)%conv
                                        else
                                            rupvars(ivar,1)=rupvars(3,1)-real(iupvars(ivar),r8)*unit_conv(ivar)
                                        endif
                                    else
                                        rupvars(ivar,1)=real(iupvars(ivar),r8)*unit_conv(ivar)
                                    endif
                                    if (ivar==2) rupvars(ivar,1)=rupvars(ivar,1)-rbase
                                endif
                            enddo v1
!                           only process if temperature, pressure, and height not missing
!                           and pressure not equal to savepress (previous level's pressure)
!                           and not skipping sounding
!                           do not process if linetype is > 30 (wind data only)
                            if (.not. lmissvals(3) .and. .not. lmissvals(1) .and.  iupvars(1) /=savepress.and. .not. lmissvals(1) &
                                .and. .not. lmissvals(2) .and. .not. lskip .and. linetype < 30) then
                                writelevs=writelevs+1
                                s_levels(iday1,nsnd(3))=writelevs !increment s_levels
                                      
!                               check writelevs against nuplevels only if within data window
!                               reset nuplevels to writelevs if writelevs larger than nuplevels
                                if (writelevs > nuplevels)nuplevels=writelevs 
                                savepress=iupvars(1) !set savepress to pressure                   
                                if (iupvars(2) > uptop+ibase) ltop=.true.!exceed top
                                rupvars(2,1)=real(iupvars(2),r8)-real(ibase,r8)
                              
!                               write data to temporary array
    v2:                         do ivar=1,6
                                      updata1(ivar,writelevs,nsnd(3),iday1)=rupvars(ivar,1)
                                enddo v2
                            endif
                                 

                        elseif (iflag1 /=0) then !error reading data
                            write(msg_unit,ext_form(11))adjustl(pathid(ipath)),'E31',modnam,&
                                'ERROR READING UPPER AIR DATA SOUNDING #',nsnd(1),'; LINE',trim(adjustl(datline)),date_hr_str
                            lbad=.true.
                        endif
                    endif !end lgo if
                endif
                  
                linetype1=linetype !reset to linetype value
!**************  linetype = 10-39 SOUNDING data data line****************************************
                                    
            else !line type invalid
                 write(msg_unit,ext_form(7))adjustl(pathid(ipath)),'E31',modnam,'INVALID LINE TYPE',linetype,'FOR SOUNDING #',&
                    nsnd(1),'LINE',trim(adjustl(datline))
                linetype1=linetype
                lbad=.true.
            endif !end linetype processing logic
              
        else !line type not determined 
            if (eof == 0) then
                write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(4))),nsnd(1),'; LINE',&
                    trim(adjustl(datline))
                lbad=.true.
            endif
        endif !end linetype check
!       at end of loop, check to see if lbad is true
!       and if so reset snd_hrs to -9 and s_levels to 0 so that AERMET will know 
!       not to process this sounding later in stage 2
!       also take 1 away from nsnding1 for the day
        if (lbad) then
            snd_hrs(iday1,nsnd(3))=-9
            s_levels(iday1,nsnd(3))=0
            nsnding1(iday1)=nsnding1(iday1)-1
        endif
!       increment number of valid soundings
        if (.not. lskip .and. (linetype==21 .or. linetype ==11) .and. lgo)nsnd(4)=nsnd(4)+1
    enddo
                  
    if (.not. lbadup .and. lbad)lbadup=.true. 
      
!   final messages
    if (.not. lbadup) then
!       reached data that exceeded xdates,issue message
        write(msg_unit,ext_form(13))adjustl(pathid(ipath)),'I22',modnam,trim(adjustl(ext_endstr(1)))
    elseif (nsnd(2) == 0 .and. .not. lbadup) then
!       no soundings extracted but no errors, issue error
        write(msg_unit,ext_form(13))adjustl(pathid(ipath)),'E37',modnam,trim(adjustl(ext_endstr(2)))
    elseif (lbadup .and. nsnd(1) > 0) then
!       soundings extracted but error
        write(msg_unit,ext_form(14))adjustl(pathid(ipath)),'E38',modnam,trim(adjustl(ext_endstr(4))),nsnd(1)
    elseif (lbadup .and. nsnd(1) == 0) then
!       error and no soundings extracted
        write(msg_unit,ext_form(13))adjustl(pathid(ipath)),'E39',modnam,trim(adjustl(ext_endstr(3)))
    endif

!   check to see if any sounding days need to
!   be reset to not have any soundings due to skips or
!   bad data
    d1: do iday1=1,nupdays
        if (lsound(iday1) .and. nsnding1(iday1) == 0) then
            lsound(iday1)=.false.
            nsnding1(iday1)=1 !for array processing purposes
        endif
    enddo d1

    if (allocated(rupvars)) deallocate(rupvars)
      
    return
    end subroutine read_igra
!*********************************************************************************************************

    subroutine read_6201
!=========================================================================================================
!   SUBROUTINE READ_6201
!   THIS SUBROUTINE CONTROLS THE READING OF 6201 FORMAT UPPER AIR DATA
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   eof:            integer end of file indicator to read runstream input file
!   k:              looping variable
!   k1:             index to start reading station ID from j1
!   iflag1:         I/O indicator when reading data specifi! to line type
!   iflag2-4:       I/0 indicators for various data
!   ilev:           level counter
!   lastht:         integer value of height before current height to check
!                   for duplicates
!   lendat:         length of data record
!   offset:         offset used to read data based on if 6201FB or 6201VB format
!   ivar:           upper air variable loop counter
!   nlevs:          maximum number of levels across all soundings in data window
!                   will be used to set nuplevels      
!   iwarn:          number of times station ID in file mismatches user entered ID
!   iflag:          I/O indicator
!   iyear:          4-digit year read from data file
!   imonth:         integer month 
!   iday:           integer day of the month
!   ihr:            integer sounding hour 
!                   will be reset to LST hours 1-24 if needed
!   sdate:          integer sounding date (YYYYMMDD)
!   iday1:		    number of days since upstart
!     
!   Real variables
!   rupvars:        6-element array of real upper air variables in un-scaled units
!                   1:  real pressure in mb after appropriate conversions
!                   2:  real height in m (integer to real)
!                   3:  real air temperature in Celcius
!                   4:  real dewpoint temperature in Celcius
!                   5:  real wind direction in degrees
!                   6:  real wind speed in m/s  
!                   for 6201 format, 1st dimension is # of levels and second
!                   dimension is 6.  Other data formats, the array is 6x1.
!                   values depend on sounding type
!   unit_conv:      1-D array of unit conversions for each rupvar
!   rmissval:       6-element array value for missing data from data file
!                   matches indices for rupvars
!   rbase:          surface height
!   a:              variable needed for RH to dewpoint calculations
!   b:              variable needed for RH to dewpoint calculations
!   es:             saturation vapor pressure
!   e:              vapor presure
!   teten:          variable needed for RH to dewpoint calculations
!   rh:             relative humidity (original value of rupvars(4)
!   realht:         each level's actual height, not height relative to base
!   ruptop:         real value of uptop
!
!   Logical variables
!   lmissvals:      6-element array denoting if variable is missing for level
!   lgo:            logical variable denoting if sounding is in data window
!     
!   Character variables
!   datline:        data line read from 6201 data file
!   date_hr_str:    date and hour text string for error messages
!   j1:             variables to read in that are not used
!   form1:          format for reading data
!   form2:          format for reading data from line
!   modnam:         Subroutine name
!=========================================================================================================  
    use main1, only: data_dates
    use file_units, only: msg_unit,up_data_unit
    use, intrinsic :: iso_fortran_env,only : output_unit
    implicit none
    integer(kind=4) :: eof,k,k1,iflag1,iflag2,iflag3,iflag4,ilev,lendat,lastht,offset,iday1,ivar,nlevs,iwarn,iflag,iyear,imonth,&
        iday,ihr
    integer(kind=8) :: sdate=0
    real(kind=r8) :: unit_conv(6)
    real(kind=r8) :: rbase,a,b,es,e,teten,rh,realht,ruptop,rmissval(6)
    real(kind=r8), allocatable, dimension(:,:) :: rupvars
    logical :: lgo,lmissvals(6)
    character(len=upformats(iupform)%maxblock) :: datline
    character(len=20) :: date_hr_str
    character(len=12) :: j1
    character(len=10) :: form1
    character(len=50) :: form2
    character(len=10) :: modnam='READ_6201'
      
    data rmissval /999.99_r8,-99999._r8,-99.9_r8,999._r8,999._r8,999._r8/
   
!   initialize
    lmissvals=.false.
    nlevs=0
    iday1=0
    eof=0
    k=0
    k1=0
    iwarn=0
    iflag1=0
    iflag2=0
    iflag3=0
    iflag4=0
    ilev=0
    lendat=0
    lastht=0
    offset=0
    rbase=0.0_r8
    a=7.5_r8
    b=237.3_r8
    es=0.0_r8
    e=0.0_r8
    teten=0.0_r8
    rh=0.0_r8
    realht=0.0_r8
    allocate(rupvars(maxlevels,6))
    rupvars=0.0_r8
    ruptop=real(uptop,r8)
    lgo=.false.
      
!   set unit conversions
!   dewpoint set to 1 for non-missing since
!   it is calculated from the input RH
    unit_conv(1)=10.0_r8
    unit_conv(2)=1.0_r8
    unit_conv(3)=1.0_r8
    unit_conv(4)=1.0_r8
    unit_conv(5)=1.0_r8
    unit_conv(6)=1.0_r8
      
      
!   format for reading the data file 
    write(form1,'(a2,i5,a1)')'(a',upformats(iupform)%maxblock,')'

!   format for reading variables
    offset=upformats(iupform)%offset

      
!   read first line to make sure possible 6201 format
    read(up_data_unit,form1,iostat=eof)datline
    read(datline(1+offset+19:1+offset+28),'(i4,3(i2))',iostat=iflag)iyear,imonth,iday,ihr
     
    if (iflag /= 0) then
        write(msg_unit,ext_form(1))adjustl(pathid(ipath)),'E30',modnam,'SOUNDING IS NOT 6201 FORMAT LINE'
        lbad=.true.
    endif
    if (.not. lbad) then
        rewind(up_data_unit)
    endif
      

!   read the data lines in as a character string and then
!   assign to variables based on the data line type
    do while (eof == 0 .and. sdate <= upend .and. iflag ==0 .and. iflag1 == 0 .and. .not. lbad)
        read(up_data_unit,form1,iostat=eof)datline
        !iline=iline+1
        !readlevs=0
        if (iflag == 0 .and. eof == 0) then
            lendat=len_trim(datline)
            if (eof ==0 .and. .not. lbad) nsnd(1)=nsnd(1)+1  !add to total soundings being read
            read(datline(1+offset:lendat),'(a8,11x,i4,3(i2))',iostat=iflag1)j1,iyear,imonth,iday,ihr
            if (iflag1 ==0) then
                call data_dates(ihr,iday,imonth,iyear,upgmt2lst,upstart,upend,32,sdate,lgo)
                write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
                if (lgo) then
                    call new_sound(sdate,ihr,iday1) !increment/initialize key variables for new sounding
!                   get the station identifier
!                   find first occurrence of non-letter character
                    k=1
                    k1=0
                    do while (k <= len_trim(j1) .and. k1 == 0)
                        if (ichar(j1(k:k)) >= 48 .and. ichar(j1(k:k)) <= 57) then
                            k1=k
                        else
                            k=k+1
                        endif
                    enddo
                    read(j1(k1:len_trim(j1)),*,iostat=iflag2)iupid
                    if (iflag2 == 0) then !station ID check
!                       only check the station ID if within the date window
!                       this is a departure from previous AERMET versions where
!                       the station check was done outside of the date check    
                        if (trim(adjustl(iupid)) /= trim(adjustl(upid))) then
                            iwarn=iwarn+1
!                           write warning message if still below the limit of message writes
                            if (iwarn <= nwarnlim) write(msg_unit,ext_form(4))adjustl(pathid(ipath)),'W30',modnam,&
                                'STATION ID FROM FILE',trim(adjustl(iupid)),'DOES NOT MATCH USER ENTERED STATION ID',&
                                trim(adjustl(upid)),'FOR SOUNDING NUMBER',nsnd(1),date_hr_str
!                           only this message once
                            if (iwarn==nwarnlim)write(msg_unit,ext_form(5))adjustl(pathid(ipath)),'W33',modnam,&
                                'REACH LIMIT OF',nwarnlim,'W30','WARNING MESSAGES'
                        endif
                    else !bad station ID
                        write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(3))),&
                                nsnd(1),'; LINE',trim(adjustl(j1))
                        lbad=.true.
                    endif    
                    if (.not. lbad) then  !read data
                        rupvars=0
!                       note that rupvars(4) is actually dewpoint but
!                       what is being read from file is relative humidity.
!                       dewpoint will be calculated from RH and temperature
!                       so rupvars(4) will become dewpoint after all
!                       calcuations.
!                       first get nlevs
                        read(datline(1+offset:lendat),'(29x,i3)',iostat=iflag3)nlevs
                        if (iflag3 == 0) then
                            write(form2,'(a,i3,a)')'(29X,I3,',nlevs,'(5X,F5.2,F6.0,F4.1,3(F3.0),7X))'
                    
                            read(datline(1+offset:lendat),form2,iostat=iflag4)nlevs,(rupvars(ilev,1),rupvars(ilev,2),&
                                rupvars(ilev,3),rupvars(ilev,4),rupvars(ilev,5),rupvars(ilev,6),ilev=1,nlevs)
                        else
                            write(msg_unit,ext_form(11))adjustl(pathid(ipath)),'E31',modnam,&
                                'ERROR READING UPPER AIR DATA SOUNDING #',nsnd(1),'; LINE',trim(adjustl(datline)),date_hr_str
                            lbad=.true.
                        endif
      
                        ilev=1
                        if (iflag4 /=0)then
                             write(msg_unit,ext_form(11))adjustl(pathid(ipath)),'E31',modnam,&
                                'ERROR READING UPPER AIR DATA SOUNDING #',nsnd(1),'; LINE',trim(adjustl(datline)),date_hr_str
                            lbad=.true.
                        endif
!                       process the levels
                        if (.not. lbad) lastht=rupvars(1,2) !set last height
                        writelevs=0 !initialize
                        do while (.not. lbad .and. .not. ltop .and. ilev <= nlevs .and. .not. lskip)
!                           calculate real values of variables if not missing
!                           note rupvars(4,ilev) (dewpoint) will be recalculated below
                            lmissvals=.false.
    v1:                     do ivar=1,6
                                if (rupvars(ilev,ivar) == rmissval(ivar)) then
                                    lmissvals(ivar)=.true.
                                    rupvars(ilev,ivar)=real(upvars(ivar)%missval,r8)!*upvars(ivar)%conv
                                else
                                    rupvars(ilev,ivar)=rupvars(ilev,ivar)*unit_conv(ivar)
                                    if (ivar==2) then
!                                       set surface level base height
                                        if (ilev==1)rbase=rupvars(ilev,ivar)
                                        rupvars(ilev,ivar)=rupvars(ilev,ivar)-rbase
                                    endif
                                endif
                            enddo v1
                              
!                           check surface height and if missing, skip sounding
                            if (ilev == 1 .and. lmissvals(2)) then 
                                lskip=.true.
                                nsnd(6)=nsnd(6)+1
                                write(msg_unit,ext_form(10))adjustl(pathid(ipath)),'W32',modnam,&
                                    'SKIP SOUNDING; SURFACE LEVEL HEIGHT MISSING FOR SOUNDING #',nsnd(1),date_hr_str
!                               reset snd_hrs to -9 and s_levels to 0 so that AERMET will know 
!                               not to process this sounding later in stage 2
!                               also take 1 away from nsnding1 for the day
                                snd_hrs(iday1,nsnd(3))=-9
                                s_levels(iday1,nsnd(3))=0
                                nsnding1(iday1)=nsnding1(iday1)-1
                            endif
                              
!                           calculate dewpoint from RH and temperature
                            if (.not. lmissvals(4) .and. .not. lmissvals(3)) then
                                rh=rupvars(ilev,4)
                                if (rh > 100.0_r8)rh=99.9_r8 !set RH to 99.9 if > 100 %
                                es=6.1078_r8*(10.0_r8**(a*rupvars(ilev,3)/(rupvars(ilev,3)+b)))
                                e=es*(rh/100.0_r8)
                                teten=dlog10(e/6.1078_r8)
                                rupvars(ilev,4)=b*teten/(a-teten)
                            endif
                                 
!                           if temperature, pressure, and height are not missing then
!                           process
                            if (.not. lmissvals(1) .and. .not. lmissvals(2) .and. .not. lmissvals(3) .and. .not. lmissvals(4))then
                                if (rupvars(ilev,2) > ruptop) ltop=.true. !check to see if above top only if writing the data                       
!                               6201 format allows for duplicate heights, if 
!                               a duplicate height, do not increment writelevs

!                               JAT, this has been commented out because existing AERMET code
!                               seems to indicate that the number of levels is not increased via
!                               comments, but code doesn't actually do that.  
!                               if (nint(realht) /= lastht .or. ilev == 1)  writelevs=writelevs+1  !increment if not a duplicate height

                                writelevs=writelevs+1
                                s_levels(iday1,nsnd(3))=writelevs
                                if (writelevs > nuplevels) nuplevels=writelevs
                                  
    v2:                         do ivar=1,6
                                    updata1(ivar,writelevs,nsnd(3),iday1)=rupvars(ilev,ivar)
                                enddo v2
                            endif

                            lastht=nint(realht) !set last height
                            ilev=ilev+1
                        enddo
!                       end data processing     
                    endif
                endif                  
            else  !invalid dates
                write(msg_unit,ext_form(3))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(ext_errstr(1))),&
                    nsnd(1),'; LINE',trim(adjustl(datline))
                lbad=.true.
            endif
        endif
!       increment number of valid soundings
        if (.not. lskip .and. lgo .and. eof==0) nsnd(4)=nsnd(4)+1
    enddo
     
                      

    if (.not. lbadup .and. lbad)lbadup=.true. 
      
!   final messages
    if (.not. lbadup) then
!       reached data that exceeded xdates,issue message
        write(msg_unit,ext_form(13))adjustl(pathid(ipath)),'I22',modnam,trim(adjustl(ext_endstr(1)))
    elseif (nsnd(2) == 0 .and. .not. lbadup) then
!       no soundings extracted but no errors, issue error
        write(msg_unit,ext_form(13))adjustl(pathid(ipath)),'E37',modnam,trim(adjustl(ext_endstr(2)))
    elseif (lbadup .and. nsnd(1) > 0) then
!       soundings extracted but error
        write(msg_unit,ext_form(14))adjustl(pathid(ipath)),'E38',modnam,trim(adjustl(ext_endstr(4))),nsnd(1)
    elseif (lbadup .and. nsnd(1) == 0) then
!       error and no soundings extracted
        write(msg_unit,ext_form(13))adjustl(pathid(ipath)),'E39',modnam,trim(adjustl(ext_endstr(3)))
    endif
      
!   check to see if any sounding days need to
!   be reset to not have any soundings due to skips or
!   bad data
    d1: do iday1=1,nupdays
        if (lsound(iday1) .and. nsnding1(iday1) == 0) then
            lsound(iday1)=.false.
            nsnding1(iday1)=1 !for array processing purposes
        endif
    enddo d1
      
    if (allocated(rupvars)) deallocate(rupvars)
      
    return
    end subroutine read_6201
!*********************************************************************************************************
      
    subroutine read_ext
!=========================================================================================================
!   SUBROUTINE READ_EXT
!   THIS SUBROUTINE READS THE EXTRACT OR QAOUT FILE
!
!   MODIFIED DECEMBER 6, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   eof:            end of file indicator
!   i:              loop counter
!   j:              loop counter
!   ifile:          file unit to read data from
!   isnd:           sounding number from file
!   ilev:           level from file
!   dates1:         array of start and end date read from file header
!   gmt2lst:        GMT to LST conversion read from file header
!   ifield:         field counter
!   n:              index value for checking for N latitude
!   s:              index value for checking for S latitude
!   e:              index value for checking for E longitude
!   w:              index value for checking for W longitude
!   iflag1:         I/O flag for reading latitude
!   iflag2:         I/O flag for reading longitude
!   ivar:           upper air variable loop counter
!   iline:          line counter 
!   iflag:          I/O indicator 
!   sdate:          integer sounding date (YYYYMMDD)
!   ihr:            hour of day (1-24)
!     
!   Real variables
!   rupvars:        6-element array of real upper air variables in un-scaled units
!                   1:  real pressure in mb after appropriate conversions
!                   2:  real height in m (integer to real)
!                   3:  real air temperature in Celcius
!                   4:  real dewpoint temperature in Celcius
!                   5:  real wind direction in degrees
!                   6:  real wind speed in m/s  
!                   for 6201 format, 1st dimension is # of levels and second
!                   dimension is 6.  Other data formats, the array is 6x1.
!                   values depend on sounding type
!   rlat:           station latitude
!   rlon:           station longitude
!
!   Logical variables
!   lgooddates:     logical variable denoting dates in header are within sounding window
!     
!   Character variables
!   j1-j3:          dummy variables to read in from file (not used)
!   alat:           character string of latitude with N or S descriptor
!   alon:           character string of longitude with E or W descriptor
!   formstr:        formats for messages
!   errstr:         error text strings for messages
!   date_hr_str:    date and hour text string for error messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit
    use main1, only: data_dates,eps,istage,msg_form
    use file_units, only: msg_unit,up_data_unit,up_qaout_unit,up_extract_unit
    implicit none
    integer(kind=4) :: i,j,eof,ifile,isnd,ilev,dates1(2,3),gmt2lst,ifield,n,s,e,w,iday1,iflag1,iflag2,ivar,iline,iflag,ihr
    integer(kind=8) :: sdate=0
    real(kind=r8) :: rlat,rlon
    real(kind=r8), allocatable, dimension(:,:) :: rupvars
    logical :: lgooddates(2)
    character(len=10) :: alat,alon
    character(len=110) :: datline
    character(len=8) :: j1,j2,j3
    character(len=60) :: formstr(5)
    character(len=60) :: errstr(4)
    character(len=20) :: date_hr_str
    character(len=10) :: modnam='READ_EXT'
      
    data errstr /'ERROR READING STATION INFORMATION; LINE','INCORRECT FILE TYPE LINE; LINE',&
        'ERROR READING EXTRACTION DATES INFORMATION; LINE','ERROR READING DATA; LINE'/
    allocate(rupvars(nupvars_keep,1))
      
!   1.  incorrect number of fields
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,i3,1x,a)'
    
!   2.  mismatch in station ID between data file and control file
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
    
!   3. station coordinates do not match coordinates in control file
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a)'
    
!   4. GMT conversion doesn't match control file
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a,1x,i2,1x,a,1x,i2)'

!   5.  error reading station information line, file type, extraction dates, data line, location line
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    

!   initialize
    i=0
    eof=0
    ifile=0
    isnd=0
    iline=0
    ilev=0
    writelevs=0
    dates1=0
    gmt2lst=0
    lgooddates=.false.
    iday1=0
    rupvars=0.0_r8
    iflag=0
    
!   determine which file to read
!   if stage 1 and EXTRACT is DATA keyword format use up_data_unit
!   if stage 1 and EXTRACT and no DATA keyword use up_extract_unit   
!   if stage 2 only, use QAOUT regardless
!   if EXTRACT is specified
      
!   
    if (upstage1) then
        if (lupkeywords(1)) then
            ifile=up_data_unit
        else
            ifile=up_extract_unit
        endif
    else
        ifile=up_qaout_unit
    endif
          
!   read the file to make sure correct format, get array bounds and fill in temporary arrays 
    do while (eof == 0 .and. .not. lbad)
        read(ifile,'(a100)',iostat=eof)datline
        if (eof == 0) then
            iline=iline+1
!           if second line, should be location, check station ID against 
!           what is entered in the runstream file
!           if different, issue warning
            if (iline == 2) then
                read(datline,*,iostat=iflag)j1
                if (trim(adjustl(j1)) == 'LOCATION') then
!                   check for number of fields to see if GMT2LST and elevation are included
                    ifield=0
    l1:             do j=9,len_trim(datline)
                        if (ichar(datline(j-1:j-1)) == 32 .and. ichar(datline(j:j)) /= 32) ifield=ifield+1                    
                    enddo l1
                    if (ifield == 3) then !station ID and coordinates
                        read(datline,*,iostat=iflag)j1,iupid,alat,alon
                    elseif (ifield == 4 .or. ifield == 5) then !GMT TO LST conversion listed; don't care about elevation
                        read(datline,*,iostat=iflag)j1,iupid,alat,alon,gmt2lst
                    else !incorrect # of fields
                        lbad=.true.
                        write(msg_unit,formstr(1))adjustl(pathid(ipath)),'E31',modnam,'INCORRECT NUMBER OF FIELDS',ifield,&
                            'ON LOCATION LINE'
                    endif
                    if (.not. lbad) then
!                       calculate rlat and rlon from alat and alon
                        s=index(alat,'S')
                        n=index(alat,'N')
                        e=index(alon,'E')
                        w=index(alon,'W')
                        iflag1=-1
                        if (s > 0) then
                            read(alat(1:s-1),*,iostat=iflag1)rlat
                            rlat=-rlat
                        elseif (n > 0) then
                            read(alat(1:n-1),*,iostat=iflag1)rlat
                        endif
                        if (iflag1 == -1) then
                            write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E31',modnam,&
                                'LOCATION: ERROR READING LATITUDE',trim(adjustl(alat))
                            lbad=.true.
                        endif
                        iflag2 = -1
                        if (w > 0) then
                            read(alon(1:w-1),*,iostat=iflag2)rlon
                            rlon=-rlon
                        elseif (e > 0) then
                            read(alon(1:e-1),*,iostat=iflag2)rlon
                        endif
                        if (iflag2 == -1) then
                            write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E31',modnam,&
                                'LOCATION: ERROR READING LONGITUDE',trim(adjustl(alon))
                            lbad=.true.
                        endif
                        if (iflag == 0 .and. iflag1 == 0 .and. iflag2 == 0) then
                            if (trim(adjustl(iupid)) /= trim(adjustl(upid))) write(msg_unit,formstr(2))adjustl(pathid(ipath)),&
                                'W30',modnam,'STATION ID FROM FILE',trim(adjustl(iupid)),'DOES NOT MATCH USER ENTERED STATION ID',&
                                trim(adjustl(upid))
                            if (dabs(rlat-uplat) > eps .or. dabs(rlon-uplon) > eps) write(msg_unit,formstr(3)) &
                                adjustl(pathid(ipath)),'W30',modnam,&
                                'STATION COORDINATES DO NOT MATCH INPUT COORDINATES IN RUNSTREAM FILE'
                            if (ifield >=4 .and. gmt2lst /= upgmt2lst .and. istage == 1)write(msg_unit,formstr(4))&
                            adjustl(pathid(ipath)),'W30',modnam,'GMT TO LST CONVERSION',gmt2lst,&
                            'DOES NOT MATCH INPUT GMT TO LST CONVERSION',upgmt2lst
                        else
                            write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E31',modnam,&
                                trim(adjustl(errstr(1))),trim(adjustl(datline))
                            lbad=.true. 
                        endif
                    endif
                else
                    write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(errstr(1))),trim(adjustl(datline))
                    lbad=.true.
                endif  
            elseif (iline == 3) then 
!               check file type, should be EXTRACT or QAOUT
!               if line is incorrect, issue error
                read(datline,*)j1,j2,j3
                if (trim(adjustl(j1)) /= 'FILE' .or. trim(adjustl(j2))/= 'TYPE:' .or. (trim(adjustl(j3)) /= 'EXTRACT' .and. &
                    trim(adjustl(j3)) /= 'QAOUT')) then
                    write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(errstr(2))),trim(adjustl(datline))
                    lbad=.true.
                endif
            elseif (iline == 4) then
!               get the dates
                read(datline,*,iostat=iflag)j1,dates1(1,1),dates1(1,2),dates1(1,3),dates1(2,1),dates1(2,2),dates1(2,3)
                if(iflag /=0) then
                    write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(errstr(3))),trim(adjustl(datline)) 
                    lbad=.true.
                endif
            elseif (iline > 4 .and. index(datline,'RANGE') == 0 .and. index(datline,'DATE ') == 0) then !this is a data record   
!               actual data accounting that previous lines could be RANGE keywords or data header
                read(datline,*,iostat=iflag)sdate,isnd,ihr,ilev,(rupvars(ivar,1),ivar=1,nupvars_keep)
                if (iflag == 0) then
                    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
                    if (ilev==1) then
                        nsnd(1)=nsnd(1)+1 !total number of soundings
                    endif
!					check to see if sounding is within data window
                    if (upstart <= sdate .and. sdate <= upend) then     
                        if (ilev==1) then 
                            call new_sound(sdate,ihr,iday1) !increment/initialize key variables for new sounding
!							reset some variables if no sounding
                            if (ihr == -9) then
                                lsound(iday1)=.false.
                            else
                                nsnd(4)=nsnd(4)+1
                            endif
                        endif
                        writelevs=writelevs+1
                        s_levels(iday1,isnd)=writelevs !increment s_levels                                      
!						check writelevs against nuplevels only if within data window
!						reset nuplevels to writelevs if writelevs larger than nuplevels
                        if (writelevs > nuplevels)nuplevels=writelevs
!						write data to temporary array
    v1:					do ivar=1,nupvars_keep
                            updata1(ivar,writelevs,isnd,iday1)=rupvars(ivar,1)
                        enddo v1
                    endif
                else
                    write(msg_unit,formstr(5))adjustl(pathid(ipath)),'E31',modnam,trim(adjustl(errstr(4))),trim(adjustl(datline))
                    lbad=.true.
                endif
            endif
        endif
    enddo
    if (.not. lbadup .and. lbad)lbadup=.true.
      
    if (allocated(rupvars)) deallocate(rupvars)
      
    return
    end subroutine read_ext
!*********************************************************************************************************
      
    subroutine new_sound(sdate,ihr,iday1)
!=========================================================================================================
!   SUBROUTINE NEW_SOUND
!   THIS SUBROUTINE INCREMENTS/INITIALIZES VARIABLES FOR NEW SOUNDING
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (READ_EXT, READ_FSL, READ_IGRA, READ_6201)
!
!   INPUT ARGUMENTS
!   SDATE:          DATE (YYYYMMDD)
!   IHR:            HOUR OF DAY
!
!   OUTPUT ARGUMENTS
!   IDAY1:          NUMBER OF DAYS SINCE START DATE
!
!   Variable definitions
!
!   Integer variables
!   i:              hour loop counter
!   ihr:            hour of day
!   iday1:          number of days since start date
!   sdate:          integer sounding date (YYYYMMDD)
!   savedate:       previous sounding's date (YYYYMMDD)
!
!   Logical variables
!   lfound          variable denoting that hour already has been read in
!
!   Character variables
!   adate:          character string of sdate used for writing to screen
!   formstr:        formats for messages
!   date_hr_str:    date and hour text string for error messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit
    use main1, only : numdays,noprint,eps,istage,msg_form
    use file_units, only: msg_unit
    implicit none
    integer(kind=4), intent(in) :: ihr
    integer(kind=8), intent(in) :: sdate
    integer(kind=4) :: i
    integer(kind=4), intent(out) :: iday1
    integer(kind=8),save :: savedate=0
    logical :: lfound
    character(len=60) :: formstr(6)
    character(len=20) :: date_hr_str
    character(len=8) :: adate
    character(len=10) :: modnam='NEW_SOUND'
    
!   1.  write extraction date to screen
    write(formstr(1),'(a)')'(1x,a,1x,i1,a,1x,2(a2,a1),a4,1x,a)'
    
!   2.  duplicate sounding
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a,1x,a20)'
    
!   3. current date is prior to previous date in sounding
    !code modified by GMM
	!original code:-
	!write(formstr(3),'(2(a))'),'a,1x,i8,1x,a,i8)'
	write(formstr(3),'(2(a))') 'a,1x,i8,1x,a,i8)'
    
!   write to screen the date and hour being processed if NOPRINT not specfied on JOB pathway
    write(adate,'(i8.8)')sdate
    write(date_hr_str,'(a5,1x,i8,1x,a2,1x,i2.2)')'DATE:',sdate,'HR',ihr
    
!   issue warning if the current date is before save date, indicating that the data is out of 
!   temporal order
    if (sdate < savedate) write(msg_unit,formstr(3))adjustl(pathid(ipath)),'W35',modnam,'CURRENT DATE',&
        sdate,'IS PRIOR TO PREVIOUS DATE IN SOUNDING',savedate
    
    nsnd(2)=nsnd(2)+1 !number of soundings being processed
    
!   see if the date and hour already have data; soundings may be out of order
    iday1=numdays(upstart,sdate)
    
    i=1
    lfound=.false.
    do while(i <= 24 .and. .not. lfound)
        if (snd_hrs(iday1,i) == ihr) then
!           duplicate hour for the day
            lfound=.true.
            write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I21',modnam,'DUPLICATE SOUNDING FOR',date_hr_str
            nsnd(5)=nsnd(5)+1
        else
            i=i+1
        endif
    enddo
    
!   if not a duplicate process
    if (.not. lfound) then
        if (.not. lsound(iday1)) then
            if (.not. noprint) write(output_unit,formstr(1))'Stage',istage,': Extracting upper air data for month/day/year',&
                adate(5:6),'/',adate(7:8),'/',adate(1:4),'LST'
            lsound(iday1)=.true.
        endif
        nsnding1(iday1)=nsnding1(iday1)+1
        nsnd(3)=nsnding1(iday1)
        if(nupdays == 1)nsound=nsnd(3) !only 1 day of soundings
        if (nsound < nsnding1(iday1)) nsound=nsnding1(iday1)
        snd_hrs(iday1,nsnding1(iday1))=ihr
        s_levels(iday1,nsnding1(iday1))=0
        writelevs=0
        ltop=.false.
        lskip=.false.
        savepress=0
    endif
    savedate=sdate
    
    return
    end subroutine new_sound
!*********************************************************************************************************
      
    subroutine up_modkey(i1)
!=========================================================================================================
!   SUBROUTINE UP_MODKEY
!   THIS SUBROUTINE CHECKS THE MODIFY KEYWORD TO DETERMINE WHAT MODIFICATIONS TO MAKE
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UPPER_PATH)
!
!   INPUT ARGUMENTS 
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE

!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   j:              loop counter
!   i1:             length of keyword with trailing blanks removed+index value of keyword
!   nfield1:        number fields on line minus 1 to account for keyword
!
!   Logical variables
!   lfound:         logical variable denoting variable name found in upper air list
!
!   Character variables
!   u_mods:             MODIFY keyword options
!   mods1:          data for MODIFY keyword read from input line
!   formstr:        format for messages to message file
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: ikey,nfield,keywrd,getfields,writeunit,msg_form
    implicit none
    integer(kind=4), intent(in) :: i1
    integer(kind=4) :: nfield1,i,j
    logical :: lfound
    character(len=8) :: u_mods(nmods)
    character(len=100),allocatable, dimension(:) :: mods1  !for getfields
    character(len=60) :: formstr
    character(len=10) :: modnam='UP_MODKEY'
    
    data u_mods/'DELMAND','CALMDIR','SUB_TTTD'/  
    
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,3(1x,a))'
    
    i=0
    j=0
    lfound=.false.
    nfield1=nfield-1
      
    if (nfield1 == 0) then
!       only MODIFY keyword present, set all lmodify to true
        lmodify=.true.
    else
!       get list of modifications
        allocate(mods1(nfield1))
        mods1='0000000000000000000000000'
!       read input line to get variable name, the inclusion indicator, lower bound, and upper bound
        call getfields(i1,nfield1,mods1)
    l1: do i=1,nfield1
            lfound=.false.
            j=1
            do while (j .le. nmods .and. .not. lfound)
                if (trim(adjustl(mods1(i))) == trim(adjustl(u_mods(j)))) then
                    lfound=.true.
                    if (lmodify(j))write(writeunit,formstr)adjustl(pathid(ipath)),'W02',modnam,&
                        'DUPLICATING LISTING OF MODIFICATION',trim(adjustl(mods1(i))),'FOR KEYWORD:',trim(adjustl(keywrd(ikey)))
                    lmodify(j)=.true.
                else
                    j=j+1
                endif
            enddo
            if (.not. lfound)then
                if (trim(adjustl(mods1(i))) == 'ALL') then
                    lmodify=.true.
                else
                    write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID MODIFICATION',trim(adjustl(mods1(i))),&
                    'FOR KEYWORD:',trim(adjustl(keywrd(ikey)))
                    lbad=.true.
                endif
            endif
        enddo l1
        deallocate(mods1)
    endif
      
    return
    end subroutine up_modkey
!*********************************************************************************************************
      
    subroutine up_modify
!=========================================================================================================
!   SUBROUTINE UP_MODIFY
!   THIS SUBROUTINE MODIFIES UPPERAIR DATA
!   MODIFICATIONS INCLUDE
!   1.  DELETION OF MANDATORY LEVELS
!   2.  IF WIND SPEED = 0 SET WIND DIRECTION = 0 IF WIND DIRECTION NON-ZERO
!   3.  IF TEMPERATURE AND DEWPOINT MISSING, INTERPOLATE FROM NON-MISSING
!       LEVELS JUST ABOVE AND BELOW.
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_PROC)
!
!   Variable definitions
!      
!   Integer variables
!   nmandlev:       number of mandatory levels (19)
!   orig_levs:      original level
!   newlevs:        new level
!   ivar:           variable counter
!   d:              day counter
!   l:              level counter
!   h:              hour counter
!   l1:             level counter
!   ipress:         pressure
!
!   Real variables
!   pcheck:         difference between pressure at level l and level l+1
!   misspress:      missing pressure value
!   misstemp:       missing temperature and dewpoint value
!   mand_levs:      mandatory levels
!
!   Character variables
!   formstr:        formats for messages
!   hrstr:          character string HR: XX where XX is hour
!   modnam:         Subroutine name
!=========================================================================================================  
    use file_units, only: msg_unit
    use main1, only: msg_form
    implicit none
    integer(kind=4), parameter :: nmandlev=19
    integer(kind=4) :: orig_levs,newlevs,ivar,d,l,h,ilev,l1,ipress(2)
    real(kind=r8) :: pcheck(2),misspress,misstemp(2),mand_levs(nmandlev)
    character(len=60) :: formstr(3)  
    character(len=6) :: hrstr
    character(len=10) :: modnam='UP_MODIFY'
    
    data mand_levs/1000._r8,950._r8,900._r8,850._r8,800._r8,750._r8,700._r8,650._r8,600._r8,550._r8,500._r8,450._r8,400._r8, &
        350._r8,300._r8,250._r8,200._r8,150._r8,100._r8/
    
!   1.  mandatory level deleted
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a6,1x,a,1x,f6.0,1x,a)'

!   2.  no levels after mandatory level deletion
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a6,1x,a)'

!   3.  wind set to calm
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'i8,1x,a6,1x,a,1x,i3,1x,a)'

    pcheck=0.0_r8
    ipress=0
    misspress=real(upvars(1)%missval,r8) !*0.1_r8
    v1: do ivar=3,4
        misstemp(ivar-2)=real(upvars(ivar)%missval,r8)*0.1_r8
    enddo v1
!   delete mandatory levels before resetting wind speeds and temperature/dewpoints since
!   data levels will change.
    if (lmodify(1)) then
    d1: do d=1,nupdays
!           only process if a sounding for the day
            if (lsound(d)) then
    h1:         do h=1,nsnding1(d) !# of soundings for the day
                    orig_levs=s_levels(d,h) !# of levels for the sounding
                    write(hrstr,'(a3,1x,i2.2)')'HR:',snd_hrs(d,h)
                    newlevs=orig_levs
    levs_loop:      do l=1,orig_levs-1
                        pcheck=0.0_r8
                        if (newlevs <= l) cycle levs_loop
                        pcheck(2)=100.0_r8
                          
!                       get difference between current level and next level up (vertically, i.e. lower pressure)
                        if (updata1(1,l,h,d) == misspress .or. updata1(1,l+1,h,d) == misspress) then !one of the pressures is missing, cycle loop            
                            cycle levs_loop
                        else
                            pcheck(1)=updata1(1,l,h,d)-updata1(1,l+1,h,d)
                        endif
                          
!                       get difference between current level and next level below (vertically, higher pressure)
                        if (l < 2) then
                            cycle levs_loop
                        elseif (l >= 2 .and. updata1(1,l-1,h,d) ==misspress) then
                            cycle levs_loop
                        else
                            pcheck(2)=updata1(1,l-1,h,d)-updata1(1,l,h,d)
                        endif
                          
!                       if the differences are less than 1% of the pressure, then check for mandatory levels
                        if ((pcheck(1) <= 0.01_r8*updata1(1,l,h,d)) .or. (pcheck(2)  <= 0.01_r8*updata1(1,l,h,d))) then
                            l1=1
                            ipress(1)=nint(updata1(1,l,h,d))
                            ipress(2)=0
                            do while (l1 <= nmandlev .and. ipress(1) /= ipress(2))
                                ipress(2)=nint(mand_levs(l1)) 
                                if (ipress(1)==ipress(2)) then
                                    newlevs=newlevs-1
                                    s_levels(d,h)=newlevs 
    ll1:                            do ilev=l,newlevs
    v2:                                 do ivar=1,6
                                            updata1(ivar,ilev,h,d)=updata1(ivar,ilev+1,h,d)
                                        enddo v2
                                    enddo ll1
                                    write(msg_unit,formstr(1))adjustl(pathid(ipath)),'I23',modnam,snd_date(d),hrstr,&
                                        'MANDATORY LEVEL: DELETED',mand_levs(l1),'MB'
                                endif    
                                l1=l1+1
                            enddo
                        endif
                    enddo levs_loop
                enddo h1
!               subtract 1 from nsnding1 and reset the hour to -9 if no levels
                if (s_levels(d,h)==0) then
                    write(msg_unit,formstr(2))adjustl(pathid(ipath)),'I23',modnam,snd_date(d),hrstr,&
                        'SOUNDING HAS NO LEVELS AFTER MANDATORY LEVEL DELETIONS'
                    nsnding1(d)=nsnding1(d)-1
                    snd_hrs(d,h)=-9
                endif
            endif
!           reset lsound to false if no longer any soundings for the day
            if (lsound(d) .and. nsnding1(d) == 0) lsound=.false.
        enddo d1
    endif

!   set wind direction to 0 if wind speed calm and substitute temperatures/dewpoints
    if (lmodify(2) .or. lmodify(3)) then
    d2: do d=1,nupdays
            if (lsound(d)) then
    h2:         do h=1,nsnding1(d)
    l2:             do l=1,s_levels(d,h)
!                       wind direction
                        if (lmodify(2) .and. nint(updata1(5,l,h,d)) /= 0 .and. nint(updata1(6,l,h,d)) == 0) then
                            updata1(5,l,h,d)=0.0_r8
                            write(msg_unit,formstr(3))adjustl(pathid(ipath)),'I24',modnam,snd_date(d),hrstr,'LEVEL',l,&
                                'WIND DIRECTION SET TO 0 - CALM WIND'
                        endif
!                       temperature and dewpoint, start with level 2 and do not process upper limit
                        if (lmodify(3) .and. l >=2 .and. l < s_levels(d,h)) then
    v3:                     do ivar=3,4
                                if (updata1(ivar,l,h,d) == misstemp(ivar-2)) then
                                    updata1(ivar,l,h,d)=newtemp(l,h,d,ivar)
                                endif
                            enddo v3
                        endif
                    enddo l2
                enddo h2
            endif
        enddo d2
    endif
      
    return
    end subroutine up_modify     
!*********************************************************************************************************

    real(kind=r8) function newtemp(l,h,d,v)
!=========================================================================================================
!   FUNCTION NEWTEMP
!   CALCULATE NEW TEMPERATURE OR DEWPOINT 
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_MODIFY)
!     
!   INPUT ARGUMENTS
!
!   L:              LEVEL INDEX
!   H:              HOUR INDEX
!   D:              DAY INDEX
!   V:              VARIABLE INDEX
!
!   OUTPUT
!     
!   NEWTEMP:        NEW TEMPERATURE OR DEWPOINT
!
!   Variable definitions
!      
!   Integer variables
!   l:              level index
!   h:              hour index
!   d:              day index
!   v:              variable index
!   l1:             level index
!
!   Real variables
!   misspress:      missing pressure value from upvars
!   misstemp:       missing temperature or dewpoint value from upvars
!   plog:           pressure difference variable
!
!   Logical variables
!   lsub:           logical variable denoting substitution occurs
!
!   Character variables
!   istr:               information message code
!   tname:              temperature or dewpoint text string for information message
!   formstr:            format for message
!   hrstr:              character string HR: XX where XX is hour
!   modnam:             Subroutine name
!=========================================================================================================
    use file_units, only: msg_unit
    use main1, only: msg_form
    implicit none
    integer(kind=4), intent(in):: l,h,d,v
    integer(kind=4) :: l1
    real(kind=r8):: misspress,misstemp,plog
    logical lsub
    character(len=3) :: istr
    character(len=11) :: tname(2)
    character(len=60) :: formstr
    character(len=6) :: hrstr
    character(len=10) :: modnam='NEWTEMP'
      
    data tname /'TEMPERATURE','DEW POINT'/
    misspress=real(upvars(1)%missval,r8)
    misstemp=real(upvars(v)%missval,r8)*0.1_r8
    write(istr,'(a,i2)')'I',v+22
    l1=0
    newtemp=updata1(v,l,h,d)
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'i8,1x,a6,1x,a,1x,i3,2(1x,a))'
    write(hrstr,'(a3,1x,i2.2)')'HR',snd_hrs(d,h)
!   if no missing values, calculate temperature or dewpoint
    if (updata1(1,l-1,h,d) /= misspress .and. updata1(1,l,h,d) /= misspress .and. updata1(v,l-1,h,d) /= misstemp) then
        l1=l+1
        lsub=.false.
        do while (l1 <= s_levels(d,h) .and. .not. lsub)
            if (updata1(1,l1,h,d) /= misspress .and. updata1(v,l1,h,d)/= misstemp) then
                plog=dlog(updata1(1,l,h,d)/updata1(1,l-1,h,d))/dlog(updata1(1,l1,h,d)/updata1(1,l-1,h,d))
                newtemp=updata1(v,l-1,h,d)+(updata1(v,l1,h,d)-updata1(v,l-1,h,d))*plog
                write(msg_unit,formstr)adjustl(pathid(ipath)),istr,modnam,snd_date(d),hrstr,'LEVEL',l,&
                    trim(adjustl(tname(v-2))),'ESTIMATED'
                lsub=.true.
            endif
            l1=l1+1
        enddo
    endif
    
    return
    end function newtemp
!*********************************************************************************************************
      
    subroutine up_audit
!=========================================================================================================
!   SUBROUTINE UP_AUDIT
!   THIS SUBROUTINE AUDITS UPPERAIR DATA
!
!   MODIFIED APRIL 22, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_PROC)
!
!   Variable definitions
!   
!   Integer variables
!   d:              day counter
!   h:              hour counter
!   ilev:           level counter
!   i:              variable counter
!   ilayer:         layer index
!   i1:             variable counter
!   i2:             variable counter
!   wd1:            wind direction at level ilev
!   wd2:            wind direction at level ilev-1 (level below)
!   iv:             variable index in upvars
!   iv1:            variable index in upvar of variable in up_audit_index
!   ivar:           upper air variable loop counter
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
!   lcalc:          calculate heights if true (set to false if missing values for key variables)
!   lowviolate:     value exceeds lower bound
!   upviolate:      value exceeds upper bound
!   debug1:         write debugging statements
!
!   Character variables
!   adate:          character string of sdate used for writing to screen
!   formstr:        formats for messages
!   date_hr_str:    date and hour text string for error messages
!   debug_form:     formats for debug file
!   asterisk:       string of asterisks
!   lstr:           lower bound string (1=LB, 2= < or <=)
!   ustr:           lower bound string (1=UB, 2= > or >=)   
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only: noprint,r,g,debug,eps,msg_form,istage
    use file_units, only: msg_unit,debug_unit
    implicit none
    integer(kind=4) :: d,h,ilev,i,ilayer,wd1,wd2,iv,iv1,i1,i2,ivar
    real(kind=r8) :: missvals(nupvars),ht,tv1,tv2,dz,delta,diff_dir,dew_dev
    logical lcalc,lowviolate,upviolate,debug1 
    character(len=60) :: formstr(9)
    character(len=75) :: debug_form(2)
    character(len=77) :: asterisk 
    character(len=15) :: date_hr_str
    character(len=8) :: adate
    character(len=2) :: lstr(2),ustr(2) 
    character(len=10) :: modnam='UP_AUDIT'
    
    a1: do i=1,77
        asterisk(i:i)='*'
    enddo a1
!   debug formats
!   1.  debug_form(1), header
    write(debug_form(1),'(a)')'(a77/1x,a,1x,i8//)'
    
!   2. differences for shears
    write(debug_form(2),'(a)')'(/t15,a4,t26,a,t42,a4,t52,a3,t61,a4,t70,a3/t10,g13.6,3x,i3,5(3x,f6.1))'
    
!   error messages
!   1.  write extraction date to screen
    write(formstr(1),'(a)')'(1x,a,1x,i1,a,1x,2(a2,a1),a4,1x,a)'
    
!   2.  can't recompute heights
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'a15,1x,a)'
    
!   3.  difference between obs and calculated height > 50 m
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a15,1x,a,1x,i3,1x,a,2(1x,f7.1))'
    
!   4.  can not compute layer index, or zero wind speed for non-zero wind speed, or calm wind
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a15,1x,a,1x,i3,1x,a)'
    
!   5.  height missing for level
    write(formstr(5),'(2(a))')trim(adjustl(msg_form)),'a15,2(1x,a,1x,i3),a,1x,a)'

!   6.  variable missing
    write(formstr(6),'(2(a))')trim(adjustl(msg_form)),'a15,2(1x,a),1x,i3)'

!   7.  violate bounds
    write(formstr(7),'(2(a))')trim(adjustl(msg_form)),'a15,2(1x,a),2(1x,f9.1,1x,a),1x,i3)'

!   8.  temperature < dewpoint
    write(formstr(8),'(2(a))')trim(adjustl(msg_form)),'a15,1x,a,1x,i3,2(1x,a,1x,f5.1))'

!   9.  top of sounding < 5 km
    write(formstr(9),'(2(a))')trim(adjustl(msg_form)),'a15,1x,a,1x,f6.1,1x,a)'
    
    
    ht=0.0_r8
    
!   allocate the audit variable array
    allocate(up_audit_index(nup_audit_vars))
    allocate(up_audit_counts(nup_audit_vars,10,4))
    
    up_audit_index=0
    up_audit_counts=0
    lstr(1)='LB'
    lstr(2)=''
    ustr(1)='UB'
    ustr(2)=''
    
    i=0
    i1=0
!   set missing values as real
    v1: do ivar=1,nupvars
        if (upvars(ivar)%laudit) then
            i=i+1
            up_audit_index(i)=ivar
        endif
        missvals(ivar)=real(upvars(ivar)%missval,r8) !*upvars(ivar)%conv
    enddo v1
      
!   write debug header if one of the calculated variables is being calculated and debug is requested
    if (debug .and. nupvars1 /= nupvars_keep)then
        write(debug_unit,'(a)')'STAGE 1 UPPERAIR DEBUG PROCESSING START'
        debug1=.true.
    else
        debug1=.false.
    endif
    
    d1: do d=1,nupdays
        if (lsound(d)) then
            write(adate,'(i8.8)')snd_date(d)
            if (debug1)write(debug_unit,debug_form(1))asterisk,'DATE:',snd_date(d)
            
!           write to screen the date and hour being processed if NOPRINT not requested
            if (.not. noprint) write(output_unit,formstr(1))'Stage',istage,': QA''ing upper air data for month/day/year',&
                adate(5:6),'/',adate(7:8),'/',adate(1:4),'LST'
    l1:     do h=1,nsnding1(d)
                if (s_levels(d,h) == 0) cycle l1
                write(date_hr_str,'(i8,1x,a2,1x,i2.2)')snd_date(d),'HR',snd_hrs(d,h)
                if ((real(uptop,r8)-updata1(2,s_levels(d,h),h,d)) > eps) then
                    write(msg_unit,formstr(9))adjustl(pathid(ipath)),'Q28',modnam,date_hr_str,'TOP OF SOUNDING',&
                        updata1(2,s_levels(d,h),h,d),'M < 5000 M'
                    nsnd_5k=nsnd_5k+1
                endif
!               write debug hour
                if (debug1)write(debug_unit,'(/t5,a,1x,i2.2)')'HOUR',snd_hrs(d,h)
                
                lcalc=.true.  !calculate heights  
!               check the heights, if a discrepancy then issue message
!               check to see if the surface height temp or dewpoint is missing
                if (dabs(updata1(2,1,h,d)-missvals(2)) < eps .or. dabs(updata1(3,1,h,d)-missvals(3)) < eps .or. &
                    dabs(updata1(4,1,h,d)-missvals(4)) < eps) then
                    write(msg_unit,formstr(2))adjustl(pathid(ipath)),'Q20',modnam,date_hr_str,'CANNOT RECOMPUTE HEIGHTS'
                    lcalc=.false.  !do not calculate heights for the rest of the sounding
!                   cycle l1    
!               endif
                else
!                   calculate virtual temperature for the surface
                    ilev=1
                    tv1=virt_temp(d,h,ilev)
                    ht=0.0_r8
!                   calculate heights until a missing level reached
                    ilev=2
                    do while (ilev <= s_levels(d,h) .and. lcalc)
!                       check for missing temperature and dewpoint and pressure at
!                       current level and level below
                        if (dabs(updata1(3,ilev,h,d)-missvals(3)) < eps .or. dabs(updata1(4,ilev,h,d)-missvals(4)) < eps .or. &
                            dabs(updata1(1,ilev,h,d)-missvals(1)) < eps .or. dabs(updata1(1,ilev-1,h,d)-missvals(1)) < eps) then
                            write(msg_unit,formstr(2))adjustl(pathid(ipath)),'Q20',modnam,date_hr_str,'CANNOT RECOMPUTE HEIGHTS'
                            lcalc=.false.  !do not calculate heights for the rest of the sounding
                        else
                            tv2=virt_temp(d,h,ilev)
                            ht=ht+(r/g)*0.5_r8*(tv2+tv1)*dlog(updata1(1,ilev-1,h,d)/updata1(1,ilev,h,d))
                            tv1=tv2 !reset tv1 to current level for next level
!                           check for differences and issue message
                            if (dabs(updata1(2,ilev,h,d) - ht) > 50.0_r8)write(msg_unit,formstr(3))adjustl(pathid(ipath)),'Q21',&
                                modnam,date_hr_str,'LEVEL:',ilev,'|OBS - CALC| HT > 50 M',updata1(2,ilev,h,d),ht
                            ilev=ilev+1
                        endif
                    enddo
                endif 
!               end height checks
                
!               now do boundary checks, missing values, and winds
    l2:         do ilev=1,s_levels(d,h)
!                   calculate layer index for counts
                    if (ilev==1) then
                        ilayer=1
                    elseif (int(updata1(2,ilev,h,d) - updata1(2,1,h,d)) >(8*up_inc)) then
                        ilayer=10
                    elseif (int(updata1(2,ilev,h,d) - updata1(2,1,h,d)) >0) then
                        ilayer = ((updata1(2,ilev,h,d)-updata1(2,1,h,d))/up_inc) + 2   
                    else
                        write(msg_unit,formstr(4))adjustl(pathid(ipath)),'I27',modnam,date_hr_str,'LEVEL',ilev,&
                            'COULD NOT COMPUTE LAYER INDEX'
                        ilayer=1 !just to have a value
                    endif
!                   check for calm wind, non-zero wind direction and wind speed = 0
!                   do not report if missing; will be done in loop v2 if auditing requested
                    if (dabs(updata1(5,ilev,h,d)-missvals(5)) > eps .and. dabs(updata1(6,ilev,h,d)-missvals(6)) > eps) then
                        if (dabs(updata1(5,ilev,h,d)-0.0_r8) > eps) then
   
                            if (dabs(updata1(6,ilev,h,d)-0.0_r8) < eps) then
!                               wind speed = 0 for non-zero wind direction 
                                write(msg_unit,formstr(4))adjustl(pathid(ipath)),'Q26',modnam,date_hr_str,'LEVEL',ilev,&
                                    'WIND SPEED = 0 FOR NON-ZERO WIND DIRECTION'
                                wind_stats(2)=wind_stats(2)+1
                            endif
                        else
                            if (dabs(updata1(6,ilev,h,d)-0.0_r8) < eps) then
!                               calm wind
                                write(msg_unit,formstr(4))adjustl(pathid(ipath)),'Q28',modnam,date_hr_str,'LEVEL',ilev,'CALM WIND'
                                wind_stats(1)=wind_stats(1)+1
                            endif
                        endif
                    endif
!                   temperature and dewpoint check      
                    if (dabs(updata1(3,ilev,h,d)-missvals(3)) > eps .and. dabs(updata1(4,ilev,h,d)-missvals(4)) > eps .and.&
                        updata1(4,ilev,h,d) > updata1(3,ilev,h,d) .and. (updata1(4,ilev,h,d)-updata1(3,ilev,h,d)) > eps ) then
                        tempstat=tempstat+1
                        write(msg_unit,formstr(8))adjustl(pathid(ipath)),'Q27',modnam,date_hr_str,'LEVEL',ilev,'UATT',&
                            updata1(3,ilev,h,d),'< UATD',updata1(4,ilev,h,d)
                    endif
                    if (lupkeywords(9)) then            
                        i1=0  !variable counter for calculated variables
    v2:                 do ivar=1,nup_audit_vars
                            iv=up_audit_index(ivar)
                        
!                           calculate variable if necessary (upvar index > 6)
!                           can only calculate for levels above surface and if
!                           levels not missing
                            if (iv > 6) i1=i1+1
                            if (iv > 6 .and. ilev > 1) then
                                !i1=i1+1
                                if (dabs(updata1(2,ilev,h,d)-missvals(2)) < eps .and. &
                                    dabs(updata1(2,ilev-1,h,d)-missvals(2)) < eps) then
                                    write(msg_unit,formstr(5))adjustl(pathid(ipath)),'Q22',modnam,date_hr_str,&
                                        'HEIGHT MISSING FOR LEVEL',ilev-1,'OR',ilev,'; CANNOT COMPUTE',&
                                        trim(adjustl(upvars(iv)%varname))
                                    dz=missvals(2)
                                    up_audit_counts(ivar,ilayer,1)=up_audit_counts(ivar,ilayer,1)+1
                                    cycle l2
                                endif
                              
                                dz=updata1(2,ilev,h,d)-updata1(2,ilev-1,h,d)
                                if (iv == 7 .or. iv == 9) then !speed shear or temperature lapse rate
                                    delta=0.0_r8
                                
                                    if (iv == 7) then
                                        iv1=6
                                    else
                                        iv1=3
                                    endif   
                                    if (dabs(updata1(iv1,ilev,h,d)-missvals(iv1)) > eps .and. &
                                        dabs(updata1(iv1,ilev-1,h,d)-missvals(iv1)) > eps .and. dz > 0.0_r8) then
!                                       calculate variable difference across levels
                                        delta=updata1(iv1,ilev,h,d)-updata1(iv1,ilev-1,h,d)
                                        if (iv == 7) then !speed shear is absolute value
                                            updata1(i1+nupvars_keep,ilev,h,d)=dabs(delta/dz)*100.0_r8
                                        else
                                            updata1(i1+nupvars_keep,ilev,h,d)=(delta/dz)*100.0_r8
                                        endif
                                        if (debug1) write(debug_unit,debug_form(2))upvars(iv)%varname,'LEV     HT',&
                                            upvars(iv1)%varname,'HT1',upvars(iv1)%varname,'HT2',&
                                            updata1(i1+nupvars_keep,ilev,h,d),ilev,updata1(2,ilev,h,d),updata1(iv1,ilev-1,h,d),&
                                            updata1(2,ilev-1,h,d),updata1(iv1,ilev,h,d),updata1(2,ilev,h,d)
                                    endif
                                elseif (iv == 8) then !direction shear
                                    wd1=0
                                    wd2=0
                                    diff_dir=0.0_r8
                                    if (dabs(updata1(5,ilev,h,d)-missvals(5)) > eps .and. dabs(updata1(5,ilev-1,h,d)-missvals(5)) &
                                        > eps .and. dz > 0.0_r8) then
                                        wd1=mod(int(updata1(5,ilev,h,d)),360)
                                        if(wd1 .lt. 0) wd1=wd1+360
                                        wd2=mod(int(updata1(5,ilev-1,h,d)),360)
                                        if(wd2 .lt. 0) wd2=wd2+360
                                        diff_dir=real(abs(wd1-wd2),r8)
!                                       if difference > 180, take complementary difference
                                        if (diff_dir > 180.0_r8) diff_dir=360.0_r8-diff_dir
                                        updata1(i1+nupvars_keep,ilev,h,d)=(diff_dir/dz)*100.0_r8
                                        if (debug1) write(debug_unit,debug_form(2))upvars(iv)%varname,'LEV     HT',&
                                            upvars(5)%varname,'HT1',upvars(5)%varname,'HT2',updata1(i1+nupvars_keep,ilev,h,d),&
                                            ilev,updata1(2,ilev,h,d),updata1(5,ilev-1,h,d),updata1(2,ilev-1,h,d),&
                                            updata1(5,ilev,h,d),updata1(2,ilev,h,d)
                                    endif
                                else !dewpoint deviation, don't do at top level
                                    if (ilev < s_levels(d,h)) then
                                        if (dabs(updata1(4,ilev,h,d)-missvals(4)) > eps .and. dabs(updata1(4,ilev-1,h,d)-&
                                            missvals(4))  > eps .and. dabs(updata1(4,ilev+1,h,d)-missvals(4)) > eps .and. &
                                            dabs(updata1(2,ilev+1,h,d)-missvals(2)) > eps .and. dz > 0.0_r8) then
                                            dew_dev=updata1(4,ilev-1,h,d)+dz/(updata1(2,ilev+1,h,d)-updata1(2,ilev-1,h,d))*&
                                                (updata1(4,ilev+1,h,d)-updata1(4,ilev-1,h,d))
                                            updata1(i1+nupvars_keep,ilev,h,d)=(dabs(dew_dev-updata1(4,ilev,h,d))/dz)*100.0_r8
                                            
                                            if (debug1) write(debug_unit,debug_form(2))upvars(iv)%varname,'LEV     HT',&
                                            upvars(4)%varname,'HT1',upvars(4)%varname,'HT2',updata1(i1+nupvars_keep,ilev,h,d),&
                                            ilev,updata1(2,ilev,h,d),updata1(4,ilev-1,h,d),updata1(2,ilev-1,h,d),&
                                            updata1(4,ilev,h,d),updata1(2,ilev,h,d)
                                        endif
                                    endif
                                endif  
                            endif
!                           increment total count counter
                            up_audit_counts(ivar,ilayer,1)=up_audit_counts(ivar,ilayer,1)+1
!                           now check for missing values  
!                           calculate i2 which will be index to use for updata1
                           
                            if (iv <= 6) then
                                i2=iv
                            else
                                i2=nupvars_keep+i1
                            endif
                        
                            if (dabs(updata1(i2,ilev,h,d)-missvals(iv)) < eps .and. (iv <= 6 .or. (iv >= 7 .and. iv <=9 .and. &
                                ilev > 1) .or. (iv == 10 .and. ilev > 1 .and. ilev <s_levels(d,h))))then
                                up_audit_counts(ivar,ilayer,2)=up_audit_counts(ivar,ilayer,2)+1
                                if (.not. upvars(iv)%lnomiss .and. iv <=6) write(msg_unit,formstr(6))adjustl(pathid(ipath)),'Q23',&
                                    modnam,date_hr_str,trim(adjustl(upvars(iv)%varname)),'MISSING FOR LEVEL',ilev
                            endif
                        
                            lowviolate=.false.
                            upviolate=.false. 
                        
!                           check bounds for observed and calculated variables
                            if (dabs(updata1(i2,ilev,h,d)-missvals(iv)) > eps) then
                                if (upvars(iv)%lincbound) then  !the bounds are acceptable, included in range
                                
                                    if (updata1(i2,ilev,h,d) < real(upvars(iv)%lowbound,r8)*upvars(iv)%conv .and. &
                                        dabs(updata1(i2,ilev,h,d)-real(upvars(iv)%lowbound,r8)*upvars(iv)%conv) > eps)then
                                        lowviolate=.true.
                                        lstr(2)='<'
                                    endif
                                  
                                    if (updata1(i2,ilev,h,d) > real(upvars(iv)%upbound,r8)*upvars(iv)%conv .and. &
                                        dabs(updata1(i2,ilev,h,d)-real(upvars(iv)%upbound,r8)*upvars(iv)%conv) > eps) then
                                        upviolate=.true.
                                        ustr(2)='>'
                                    endif
                                  
                                else !bounds are not acceptable, outside of range
                                    if (updata1(i2,ilev,h,d) <= real(upvars(iv)%lowbound,r8)*upvars(iv)%conv) then
                                        lowviolate=.true.
                                        lstr(2)='<='
                                    endif
                                
                                    if (updata1(i2,ilev,h,d) >= real(upvars(iv)%upbound,r8)*upvars(iv)%conv) then
                                        upviolate=.true.
                                        ustr(2)='>='
                                    endif
                                endif
                            endif
                            if (lowviolate) then
                                up_audit_counts(ivar,ilayer,3)=up_audit_counts(ivar,ilayer,3)+1
                                write(msg_unit,formstr(7))adjustl(pathid(ipath)),'Q24',modnam,date_hr_str,&
                                trim(adjustl(upvars(iv)%varname)),lstr(1),updata1(i2,ilev,h,d),lstr(2),&
                                real(upvars(iv)%lowbound,r8)*upvars(iv)%conv,'LEVEL',ilev    
                            endif
                            if (upviolate) then
                                up_audit_counts(ivar,ilayer,4)=up_audit_counts(ivar,ilayer,4)+1
                                write(msg_unit,formstr(7))adjustl(pathid(ipath)),'Q25',modnam,date_hr_str,&
                                trim(adjustl(upvars(iv)%varname)),ustr(1),updata1(i2,ilev,h,d),ustr(2),&
                                real(upvars(iv)%upbound,r8)*upvars(iv)%conv,'LEVEL',ilev
                            endif
                        enddo v2
                    endif
                enddo l2
                  
            enddo l1
        endif
    enddo d1

    return 
    end subroutine up_audit
!*********************************************************************************************************

    real(kind=r8) function virt_temp(d,h,l)
!=========================================================================================================
!   FUNCTION VIRT_TEMP
!   CALCULATE VIRTUAL TEMPERATURE 
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_AUDIT)
!
!   INPUT ARGUMENTS
!
!   D:              DAY INDEX
!   H:              HOUR INDEX
!   L:              LEVEL INDEX
!
!   OUTPUT
!     
!   VIRT_TEMP:        VIRTUAL TEMPERATURE
!
!   Variable definitions
!      
!   Integer variables
!   d:              day index
!   h:              hour index
!   l:              level index
!
!   Real variables
!   e:              vapor pressure
!   q:              specifi! humidity
!   plog:           pressure difference variable
!   clr:            constant
!   t0:             constant
!
!   Character variables
!   modnam:             Subroutine name
!=========================================================================================================
    implicit none
    integer(kind=4),intent(in):: l,h,d
    real(kind=r8):: e,q
    real(kind=r8), parameter:: clr=2500.0_r8/0.461_r8
    real(kind=r8), parameter :: t0=1.0_r8/302.16_r8
      
    character(len=10) :: modnam='VIRT_TEMP'
      
    e = dexp(clr*(t0-1.0_r8/(updata1(4,l,h,d) + 273.16_r8))*40.0_r8)
    q = (0.62197_r8*e)/(updata1(1,l,h,d) - 0.37803_r8*e)
    virt_temp = (updata1(3,l,h,d) + 273.16_r8)*(1.0_r8+0.6078_r8*q)

    return
    end function virt_temp
!*********************************************************************************************************
      
    subroutine up_stage2
!=========================================================================================================
!   SUBROUTINE UP_STAGE2
!   THIS SUBROUTINE PERFORMS STAGE 2 PROCESSING FOR UPPERAIR DATA WHEN READING QAOUT DATA
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
!   iline:          line counter 
!
!   Character variables
!   lower:          lowercase letters string
!   upper:          uppercase letters string
!   form1:          format for reading QAOUT file
!   modnam:         Subroutine name
!========================================================================================================= 
    use, intrinsic :: iso_fortran_env,only : output_unit 
    use main1, only : linelen,inpline1,nfield,ilen,getdates,getloc
    use file_units, only: up_qaout_unit
    implicit none
    integer(kind=4) :: i,i1,eof,iline
    character(len=26) :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=26) :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=6) :: form1
    character(len=10) :: modnam='UP_STAGE2'
    
    write(form1,'(a2,i3,a1)')'(a',linelen,')'
!   read in the header from the QAOUT file to get station information and dates
    eof=0
    lbad=.false.
    iline=0
    do while (eof == 0 .and. .not. lbad .and. iline <=3)
        read(up_qaout_unit,form1)inpline1
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
                call getloc(i+8,upid,uplat,uplon,upgmt2lst,upelev,lgmt2lst,lelev)
            endif
                
            if (index(inpline1,'DATES') > 0) then
                i=index(inpline1,'DATES')
                nfield=7
                call getdates(i+5,upstart,upend,updates)
            endif
            if (index(inpline1,'RANGE') > 0) then
                i=index(inpline1,'RANGE')
                nfield=6
                call up_range(i+5)
            endif
        endif
    enddo
    rewind(up_qaout_unit)
    return
    end subroutine up_stage2
!*********************************************************************************************************
      
    end module upperair