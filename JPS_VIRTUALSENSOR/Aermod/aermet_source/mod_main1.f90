    module main1

!=========================================================================================================
!   MODULE MAIN1
!   THIS MODULE CONTAINS COMMON VARIABLES AND SUBROUTINES NEEDED THROUGOUT AERMET PROCESSING 
!   BY MULTIPLE MODULES
!
!   MODIFIED APRIL 28, 2022
!
!   USED BY:  AERMET, MODULES ONSITE, READ_INPUT, REPORTS, SURFACE, UPPERAIR, PBL
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   Variable definitions
!
!   Integer variables
!   npath:              Number of paths (JOB, UPPERAIR, SURFACE, ONSITE, PROG, METPREP, MERGE)
!   nkeys:              Number of keywords
!   nitems:             Number of items that go with some keywords
!   nactions:           Number of actions
!   ninstr:             Number of instrument parameters
!   linelen:            length of line read from AERMET runstream file
!   ikey:               index of the keyword array of input keyword
!   ipath:              index of the pathid array of input path
!   nfield:             number of fields on line
!   ilen:               length of line after removing trailing blanks
!   fileind:            2-element array of starting (1) and ending (2) positions of filenames read from line in AERMET file
!   formind:            2-element array of starting (1) and ending (2) positions of file format read from line in AERMET file
!   r8:                 double precision value for real numbers
!   writeunit:          file unit to write informational or report messages
!   runtimes:           2x8 array of date/times to signify when AERMET started and stopped processing
!   days:               array of number of days per month
!                       note, February has 28 days listed
!   istage:             stage being processed
!
!   Real variables
!   R:                  Gas constant (287)
!   g:                  gravity  (9.80655)
!   d2r:                degrees to radians conversion (pi/180)
!                       multiply variable by d2r to convert from degrees to radians
!                       divide variable by d2r to convert from radians to degrees
!   eps:                buffer for checks against bounds and missing values for real variables
!                       set to 0.01
!
!   Logical variables
!   lpath:              Logical array denoting if path IDs detected
!                       If a path ID is found, lpath for that ID is 
!                       set to TRUE, otherwise it is FALSE until detected
!	lkey:               Logical array denoting if keyword detected
!                       if a keyword is found, lkey for that keyword is
!                       set to TRUE, otherwise it is FALSE until detected
!   lstage:             logical variable denoting which stage, 1 or 2, or both
!                       is run.  if stage 1 is run lstage(1)=true, if stage 2
!                       lstage(2) is true
!   noprint:            logical variable denoting whether to print hourly processing
!                       to screen (false) or not to print (true). value is set
!                       from ljbkeywords(4).
!   debug:              write debug output.
!   lbad:               logical variable denoting that a line is bad from the runstream file
!
!   Character variables
!   pathid:             AERMET pathway variable array
!   keywrd:             AERMET keywords variable array
!   items:              AERMET items variable array
!   actions:            AERMET actions variable array
!   instr:              AERMET instrument parameters
!   inpline:            line read from input file, original case, needed for filenames 
!   inpline1:           line read from input file, upper case, needed for QA of path, keywords, etc.
!   versn:              AERMET version number
!   msg_form:           initial format string for writing messages; used throughout AERMET
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit
    use file_units, only: flength,msg_unit,msgfile
    implicit none

    integer(kind=4), parameter :: npath = 7 
    integer(kind=4), parameter :: nkeys = 36
    integer(kind=4), parameter :: nitems=7
    integer(kind=4), parameter :: nactions=12
    integer(kind=4), parameter :: ninstr=1 

    integer(kind=4),parameter :: linelen=500
    integer(kind=4) :: ikey=0
    integer(kind=4) :: ipath=0
    integer(kind=4) :: nfield=0
    integer(kind=4) :: ilen=0
    integer(kind=4) :: fileind(2)=0
    integer(kind=4) :: formind(2)=0 
    integer(kind=4) :: writeunit=0
    integer(kind=4) :: runtimes(2,8)=-1
    integer(kind=4) :: days(12)
    integer(kind=4) :: istage=0
    integer(kind=4), parameter :: r8=selected_real_kind(15,307)  !double precision for reals

    real(kind=r8), parameter :: r=287.0406_r8  
    real(kind=r8), parameter :: g=9.80655_r8
    real(kind=r8), parameter :: d2r=3.141592654/180._r8  !radians divided by 180
    real(kind=r8), parameter :: eps=0.01_r8
    
    logical :: lpath(npath)=.false.
    logical :: lkey(nkeys)=.false.
    logical :: lstage(2)=.false.
    logical :: noprint=.false.
    logical :: debug=.false.
    logical :: lbad=.false.
    
    character(len=10) :: pathid(npath)
    character(len=12) :: keywrd(nkeys)
    character(len=8) :: items(nitems)
    character(len=7) :: actions(nactions)
    character(len=8) :: instr(ninstr)
      
    character(len=linelen) :: inpline
    character(len=linelen) :: inpline1
    character(len=5) :: versn='22112'
    character(len=24) :: msg_form='(1x,a10,1x,a3,5x,a10,1x,'
  

      
!   pathid
!   note that the MERGE pathway is a holdover from previous versions of AERMET
!   and is only checked to allow for older control files; no processing is done
!   for the MERGE pathway
    data pathid /'JOB','UPPERAIR','SURFACE','ONSITE','PROG','METPREP','MERGE'/
    
!   keywords
    data keywrd /'REPORT','MESSAGES','CHK_SYNTAX','DATA','NO_MISSING','FORMAT','EXTRACT','QAOUT','XDATES','LOCATION','MODIFY',&
        'RANGE','AUDIT','DELTA_TEMP','OSHEIGHTS','THRESHOLD','READ','FREQ_SECT','SITE_CHAR','SECTOR','OUTPUT','MODEL','METHOD',&
        'UAWINDOW','DEBUG','PROFILE','OBS/HOUR','NWS_HGT','FREQ_SECT2','SITE_CHAR2','SECTOR2','ASOS1MIN','AERSURF','AERSURF2',&
        'THRESH_1MIN','NOPRINT'/            

!   items
    data items /'WIND_DIR','REFLEVEL','STABLEBL','ASOS_ADJ','UASELECT','CCVR','TEMP'/

!   actions
    data actions /'RANDOM','NORAND','SUBNWS','BULKRN','NO_ADJ','SUNRISE','ADJ_U*','SUB_CC','NO_SUB','SUB_TT','NOTSUB','NOPERS'/
      
!   instruments
    data instr /'WIND'/
      
!   number of days per month
    data days /31,28,31,30,31,30,31,31,30,31,30,31/
    contains
!*********************************************************************************************************     
    
    subroutine datetime(i)
!=========================================================================================================
!   SUBROUTINE DATETIME
!   THIS SUBROUTINE WRITES DATE AND TIME TO THE RUNTIMES ARRAY TO DENOTE WHEN AERMET STARTS/STOP
!   SUBROUTINE CALLS THE INTRINISIC FORTRAN SUBROUTINE DATE_AND_TIME
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:  AERMET
!
!   INPUT ARGUMENT(S)
!   I:                  INDEX OF RUNTIMES ARRAY (1=START, 2=END)
!      
!   Integer variables
!   i:                  index of runtimes array to process (1=start, 2=end)
!   j:                  loop counter
!   idattim:            array of date/time returned by date_and_time with 8 elements
!                       elements are:
!                       (1) 4-digit year
!                       (2) month of year (1 to 12)
!                       (3) day of month
!                       (4) time difference with respect to GMT in minutes
!                       (5) hour of day (0 to 23) in local time
!                       (6) minute of time
!                       (7) second of time
!                       (8) milliseconds of time
!
!   Character variables
!   modnam:             Subroutine name
!=========================================================================================================
    
    implicit none
    integer(kind=4), intent(in) :: i
    integer(kind=4) :: idattim(8),j 
    character(len=10) :: modnam='DATETIME'
      
    call date_and_time(values=idattim)
!   assign the date and time to the appropriate index of the array runtimes
    l1: do j=1,8
        runtimes(i,j)=idattim(j)
    enddo l1
      
    return
    end subroutine datetime     
!*********************************************************************************************************

    subroutine start_stop(i,ifile)
!=========================================================================================================
!   SUBROUTINE START_STOP
!   THIS SUBROUTINE WRITES DATE AND TIME TO SPECIFIED OUTPUT FILE OR DEVICE OF WHEN AERMET STARTED AND
!   STOPPED
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:  AERMET, MODULE REPORTS (SUBROUTINE INPUT_SUMM)
!
!   INPUT ARGUMENT(S)
!   I:                  INDEX OF RUNTIMES ARRAY (1=START, 2=END)
!
!   IFILE:              FILE UNIT TO WHICH TO WRITE
!      
!   Integer variables
!   i:                  index of runtimes array to process (1=start, 2=end)
!   ifile:              file unit to which to write date/time
!   hr:                 integer hour of day
!
!   Character variables
!   ampm:               character variable with value of 'AM' or 'PM' 
!   amonths:            character array of month names
!   str:                character array of text string to write
!                       to denote starting or stopping AERMET
!   formstr:            format string for writing message
!   modnam:             Subroutine name
!=========================================================================================================
    implicit none

    integer(kind=4), intent(in) :: i,ifile
    integer(kind=4) :: hr
    character(len=2) :: ampm
    character(len=10) :: amonths(12)
    character(len=5) :: str(2)
    character(len=60) :: formstr
    character(len=10) :: modnam='START_STOP'
      
    data amonths /'JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE','JULY','AUGUST','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER'/
    
    data str /'START','END'/

    formstr='(3(1x,a),1x,i2.2,a1,1x,i4,2x,i2,a1,i2.2,a1,i2.2,1x,a2)'
!   initialize
     hr=0
      
!   set AM/PM flag
    if (runtimes(i,5) .le. 11) then
        hr=runtimes(i,5)
        ampm='AM'
    else
        hr=12+(runtimes(i,5)-12)
        ampm='PM'
    endif
      
    write(ifile,trim(adjustl(formstr)))trim(adjustl(str(i))),'PROCESSING DATE/TIME:',trim(adjustl(amonths(runtimes(i,2)))),&
        runtimes(i,3),',',runtimes(i,1),hr,':',runtimes(i,6),':',runtimes(i,7),ampm
 
    return
    end subroutine start_stop           
      
!*********************************************************************************************************

    subroutine leapyr(iyear,leap)
!=========================================================================================================
!   SUBROUTINE LEAPYR
!   THIS SUBRUTINE DETERIMINES IF THE INPUT YEAR, IYEAR IS A LEAP YEAR
!     
!   MODIFIED DECEMBER 2, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:  MODULE MAIN1 (CHECKDATES, NUMDAYS, DATA_DATES), MODULE SURFACE (SF_PROC),
!               MODULE UPPERAIR (UP_PROC), MODULE ONSITE (OS_PROC), MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENT(S)
!   IYEAR:          4-DIGIT YEAR
!
!   OUTPUT ARGUMENT(S)
!   LEAP:           OUTPUT LOGICAL VARIABLE IF LEAP YEAR OR NON-LEAP YEAR
!

!   Integer variables
!   iyear:          input 4-digit year
!   ileap4:         integer result of MOD(iyear,4)
!   ileap100:       integer result of MOD(iyear,100)
!   ileap400:       integer result of MOD(iyear,400)
!
!   logical variables
!   leap:           true if a leap year, false if non-leap year
!
!   Character variables
!   modnam:             Subroutine name
!=========================================================================================================
    implicit none
    
    integer(kind=4), intent(in) :: iyear
    integer(kind=4) :: ileap4,ileap100,ileap400 
    logical, intent(out) :: leap
    character(len=10) :: modnam='LEAPYR'
      
!   initialize 
    ileap4=0
    ileap100=0
    ileap400=0
    leap=.false.
      
    ileap4=mod(iyear,4)   
    ileap100=mod(iyear,100)
    ileap400=mod(iyear,400)

!   if year is not divisible by 4 or year is divisible by 100 but not divisible by 400 then year 
!   is not a leap year

!   examples:
!   2001 is not a leap year because it is not divisible by 4
!   2000 is is a leap year because it is divisible by 4, 100, and 400 
!   1900 is not a leap year because it is divisible by 4, 100, but not 400

    if (ileap4 .ne. 0 .or. (ileap100 .eq. 0 .and. ileap400 .ne. 0)) then 
        leap=.false.  
    else
        leap=.true.  
    endif
    
    return
    end subroutine leapyr
!*********************************************************************************************************

    subroutine checkfile(iunit,fname,itype,lgood)
!=========================================================================================================
!   SUBROUTINE CHECKFILE
!   THIS SUBROUTINE PROCESSES THE MESSAGE, REPORT, DEBUG, DATA, EXTRACT, QAOUT, SURFACE, PROFILE, 
!   AND AERSURF(2) LINES FROM THE RUNSTREAM FILE
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE READ_INPUT(JOB_PATH), MODULE UPPERAIR (UPPER_PATH), MODULE SURFACE (SURF_PATH),
!                   MODULE ONSITE(OS_PATH), MODULE PBL (PBL_PATH)
!
!   INPUT ARGUMENTS:
!   IUNIT:          FILE UNIT OF FILE BEING CHECKED
!   LGOOD:          LOGICAL CHECK THAT FILENAME IS OKAY	
!   ITYPE:          INTEGER TO DETERMINE WHICH VALUE OF FTYPES ARRAY TO USE
!
!   INPUT/OUTPUT ARGUMENTS:
!   FNAME:          FILENAME BEING CHECKED
!
!   Variable definitions
!      
!   Integer variables
!   iunit:          file unit of file being checked
!   itype:          integer to determine which value of ftypes array to use
!   ioflag:         flag determining if new file has valid filename
!   flen:           length of filename
!   i:              loop counter
!
!   Logical variables
!   lexist:         logical check on whether a file exists or if a new file, pathname is correct
!   lopened:        logical check to see if file is already open, this means it may be a duplicate
!                   filename for another file in the runstream file.
!   lgood:          logical check that filename is okay
!   lblank:         check for blanks in filename
!
!   Character variables
!   fname:          filename being checked
!   ftypes:         array of file types
!   form_str:       format string for writing invalid or duplicate filename message
!   modnam:			Subroutine name
!=========================================================================================================
    implicit none
      
!   do not initialize, input arguments
    integer(kind=4), intent(in) :: iunit,itype
    integer(kind=4) :: ioflag,flen,i
    logical, intent(out) :: lgood
    logical :: lexist,lopened,lblank
    character(len=flength), intent(inout) :: fname
    character(len=7) :: ftypes(4)
    character(len=50) :: form_str
    character(len=10) :: modnam='CHECKFILE'
      
    data ftypes /'old','replace','new','unknown'/
      
!   initialize
    lgood=.true.
    ioflag=0
    flen=0
    lexist=.false.
    lopened=.false.
    i=1
    lblank=.false.
    
    write(form_str,'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
      
!   check for quotes, if there are quotes, remove
!   quotes should be at the beginning and end
!   the quotes should be the same at beginning and end
!   if a single quote at the beginning, should be single quote
!   at the end.  The same is true for double quotes
!   if quotes are not at both ends or don't match, don't 
!   change the filename so that AERMET will say invalid file
    fname=trim(adjustl(fname))
    flen=len_trim(fname)
    
    if ((ichar(fname(1:1)) == 34 .and. ichar(fname(flen:flen)) == 34) .or. (ichar(fname(1:1)) == 39 .and. ichar(fname(flen:flen)) &
        == 39)) then 
!       check to see if any blanks
        do while (i .le. flen .and. .not. lblank)
            if (ichar(fname(i:i))==32)lblank=.true.
            i=i+1
        enddo
!       set first and last character to blanks
        fname(1:1)=''
        fname(flen:flen)=''
    endif

!   first check to see if file is opened
!   if so, may be a duplicate filename
    inquire(file=fname,opened=lopened)
    if (.not. lopened) then
!       if a file's type is anything other than new, then
!       first check to see if the file exists. This is because
!       a file such as an EXTRACT or QAOUT file can be an input or
!       output file depending on the stage, but don't know the stage
!       at this point when calling this subroutine
        if (itype == 1) then !old file, should exist, if not, then will be error
            inquire(file=fname,exist=lexist)
            if (lexist) open(unit=iunit,file=fname,status=ftypes(itype))
        elseif (itype /= 3 ) then !unknown or replace, could exist or not
!           first check to see if it exists
            inquire(file=fname,exist=lexist)
            if (.not. lexist) then
!               not a pre-existing file, open it to check the directory path
                open(unit=iunit,file=fname,iostat=ioflag,status=ftypes(itype))
                if (ioflag /= 0) then
                    lexist=.false.
                else
                    lexist=.true.
        !           close(iunit)
                endif
            else
                open(unit=iunit,file=fname,status=ftypes(itype))
            endif
        else !new file
!           check to see if can open such a file
!           this is more of a check for the directory path
            open(unit=iunit,file=fname,iostat=ioflag,status=ftypes(itype))
            if (ioflag /= 0) then
                lexist=.false.
            else
                lexist=.true.
!               close(iunit)
            endif
        endif
    else
        lexist=.true.
    endif   

!   write error message if problem with file if
!   it doesn't exist or is already opened
    if (.not. lexist) then
        write(writeunit,form_str)adjustl(pathid(ipath)),'E02',adjustl(modnam),'INVALID',trim(adjustl(keywrd(ikey))),'FILENAME:',&
            trim(adjustl(fname))
        lbad=.true.
        lgood=.false.
    endif
    if (lopened) then
        write(writeunit,form_str)adjustl(pathid(ipath)),'E02',adjustl(modnam),'DUPLICATE',trim(adjustl(keywrd(ikey))),&
            'FILENAME:',trim(adjustl(fname))
        lbad=.true.
        lgood=.false.
    else
        if (lexist) then
            lgood=.true.
        else
            lgood=.false.
            lbad=.true.
        endif
    endif
    
    return
    end subroutine checkfile
!*********************************************************************************************************

    subroutine dataline(i,i1)
!=========================================================================================================
!   SUBROUTINE dataline
!   READ DATA LINE TO GET INPUT DATA OR AERSURF(2) FILE AND FILE FORMAT INDICES
!     
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UPPER_PATH), MODULE SURFACE (SURF_PATH), MODULE ONSITE(OS_PATH), 
!                   MODULE PBL (PBL_PATH)
!  
!   INPUT ARGUMENTS
!
!   I:              INDEX OF KEYWORD DATA ON INPUT RUNSTREAM LINE
!   I1:             LENGTH OF KEYWORD WITHOUT TRAILING BLANKS
!
!   Variable definitions
!      
!   Integer variables
!   i:              index value of keyword
!   i1:             length of keyword with trailing blanks removed
!   j:              loop counter
!
!   Logical variables
!   qindx:          2-element array denoting existence quotations in string
!                   used for filenames with blanks in the name.
!
!   Character variables
!   modnam:             Subroutine name
!=========================================================================================================     
    implicit none
      
!   do not initialize, input arguments
    integer(kind=4), intent(in) :: i,i1
    integer(kind=4) :: j
    logical :: qindx(2)
    character(len=10) :: modnam='DATALINE'

!   initialize
    j=0
    fileind=0
    formind=0
    qindx=.false.
!   start at first character after keyword to get positions on line
!   that contain filename
    
!   look for quotation marks, single (') or double (") quotes
    j=i+i1
    do while (j <= ilen .and. fileind(2) == 0)
!       if current character is a quote and previous character or next character is a blank, set qindx
        if ((ichar(inpline1(j:j)) == 34 .or. ichar(inpline1(j:j)) == 39) .and. (ichar(inpline1(j-1:j-1)) == 32 .or. &
            ichar(inpline1(j+1:j+1))==32)) then
			! code modified by GMM
			! original line:-
			! if(qindx(1)==0) then
            if (.not. qindx(1)) then
                qindx(1)=.true.
            else
                qindx(2)=.true.
            endif
        endif
            
!       if preceeding or proceeding character is blank and current
!       character is non-blank set values for fileind(1) or fileind(2)
        if ((ichar(inpline1(j-1:j-1)) == 32 .or. ichar(inpline1(j+1:j+1))== 32).and. ichar(inpline1(j:j)) /= 32) then   
            if (fileind(1) == 0) then
                fileind(1)=j
            else !fileind(1) has been set
                if ((qindx(1) .and. qindx(2)) .or. (.not. qindx(1) .and. .not. qindx(2))) fileind(2)=j !set if both qindx values the same
            endif
        endif
        j=j+1
    enddo
      
!   if no file format, i.e. onsite or PROG data then set fileind(2) to ilen+1
!   step below will reset it to ilen, the desired value.
    if (fileind(2) == 0) fileind(2)=ilen+1
!   set formind
!   set j after fileind(2)
    j=fileind(2)+1
 
    do while (j <= ilen .and. formind(2) == 0)
!       if preceeding or proceeding character is blank and current
!       character is non-blank set values for formind(1) or formind(2)
        if ((ichar(inpline1(j-1:j-1)) == 32 .or. ichar(inpline1(j+1:j+1))== 32).and. ichar(inpline1(j:j))/= 32) then 
            if (formind(1) == 0) then
                formind(1)=j
            else
                formind(2)=j
            endif
        endif
        j=j+1
    enddo
    
    if (formind(2)==0)formind(2)=ilen
    
    return
    end subroutine dataline
!*********************************************************************************************************

    subroutine getdates(i1,startdate,enddate,dates1)
!=========================================================================================================
!   SUBROUTINE GETDATES
!   READ XDATES LINE TO GET START AND END DATES OF DATA
!     
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UPPER_PATH, UP_STAGE2), MODULE SURFACE (SURF_PATH, SF_STAGE2), 
!                   MODULE ONSITE(OS_PATH OS_STAGE2), PBL (PBL_PATH)
!
!   INPUT ARGUMENTS
!
!   I1:             LENGTH OF KEYWORD WITHOUT TRAILING BLANKS+INDEX of KEYWORD in RUNSTREAM LINE
!
!   OUTPUT ARGUMENTS
!     
!   STARTDATE:      START DATE (YYYYMMDD)
!   ENDDATE:        END DATE (YYYYMMDD)
!   DATES1:         ARRAY OF START/END YEAR, MONTH, DAY
!
!   Variable definitions
!      
!   Integer variables
!   i1:             length of keyword with trailing blanks removed+index of keword in runstream line
!   j:              loop counter
!   ifield:         field loop counter
!   nfield1:		nfield-1 (accounts for keyword)
!   startdate:      start date (yyymmdd)
!   enddate:        end date (yyyymmdd)
!   dates1:         2x3 array of start month, day, year and end month,
!                   day and year.  1st dim is the start (1) or end (2)
!                   2nd dim is year (1), month (2), and day (3)
!   istep:          integer to skip TO or - when dates separated by spaces
!   iflag:          flag to determine if an error when reading the character
!                   date strings and converting to numbers
!                   variable has 3 elements (1=year, 2=month, 3=day)
!   idates:         3 element array with element 1=year, 2=month, 3=day
!   ii:             loop counter.  ii=1 is start date, ii=2 end date
!    
!   Logical variables
!   lgood:          Dates are okay
!   lsep:           / or blank found in datafield as date separator (between year,month,day)
!
!   Character variables
!   datefields:     Date fields read from runstream line
!   datefields1:    Original value of datefields used for writing error message
!   adates:         3-element array of date variables (1=year,2=month, 3=day)
!   timestr1:       time string array for write to error message (start, end).
!   timestr2:       time string array for write to error message (year, month, day)
!   formstr:        format strings for error messages
!   modnam:			Subroutine name
!=========================================================================================================

    implicit none
    integer(kind=4), intent(in) :: i1
    integer(kind=4) :: nfield1,j,ifield,istep,iflag(3),idates(3),ii
    integer(kind=4), intent(out) :: dates1(2,3)
    integer(kind=8), intent(out) :: startdate,enddate
      
    character(len=100), allocatable, dimension(:) :: datefields,datefields1
    character(len=10) :: adates(2,3)
    character(len=5) :: timestr1(2),timestr2(3)
    character(len=10) :: modnam='GETDATES'
    character(len=50) :: formstr(3)
    logical lgood,lsep
      
    data timestr1 /'START','END'/
    data timestr2 /'YEAR','MONTH','DAY'/
      
!   initialize
    j=0
    ifield=0
    istep=0
    iflag=0
    idates=0
    ii=0
    dates1=0
    startdate=0
    enddate=0	
    adates='-999'
    lsep=.false.
    
!   get # of fields by subtracting 1 from nfield to account for presence of
!   XDATES keyword, allocate arrays, and initialize
    nfield1=nfield-1
    
    allocate(datefields(nfield1))
    allocate(datefields1(nfield1))
    
    datefields='0000000000000000000000000'

!   create format string for messages
!   1:  invalid date separator or invalid character in field
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
!   2:  invalid month or day
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'(3(a,1x),a)'
!   3:  start date after end date
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'a,1x,i8,1x,a,1x,i8)'
    
!   fill in the arrays
    call getfields(i1,nfield1,datefields)
      
!   if a word is used to separate the start and endates, it
!   must be TO or -
!   set the location of the separator
    if (nfield1 == 3 .or. nfield1 == 7) then
        if (nfield1 == 3) then
!           years, months, and days separated by / so three array elements, so TO or - is the 2nd array
!           element of datefields
            ifield=2
        else
!           years, months, and days separated by spaces, so TO or - is the 4th array
!           element of datefields
            ifield=4
        endif
!       if the separator is not TO or - then error
        if (trim(adjustl(datefields(ifield))) /= 'TO' .and. trim(adjustl(datefields(ifield))) /= '-') then 
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,'INVALID DATE SEPARATOR FOR:',&
                trim(adjustl(inpline1))
            lbad=.true.
        endif
    endif
  
    if (.not. lbad) then   
!       initialize counter for ayear,amonth,aday
        ii=0
!       date string has / year,month,day,separator
        if (nfield1 == 2 .or. nfield1==3) then
            if (nfield1 == 2) then
!               no TO or -, check every array element
                istep=1
            else
!               TO or - is present, then skip that element
                istep=2
            endif
    l1:     do ifield=1,nfield1,istep
                ii=ii+1
                lsep=.false.
!               convert / to spaces for easier reading
!               also check for non-numeric characters
!               only non-numeric characters allowed are / and -
                datefields1(ifield)=datefields(ifield)
                do j=1,len_trim(datefields(ifield))
                    if (datefields(ifield)(j:j) == '/' .or. datefields(ifield)(j:j) == '-') then                
                        datefields(ifield)(j:j)=' '
                        lsep=.true.
                    else
                        if ((ichar(datefields(ifield)(j:j)) /= 32 .and. ichar(datefields(ifield)(j:j)) < 48) .or. &
                            ichar(datefields(ifield)(j:j)) > 57) then
                            lbad=.true.   
                            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E05',modnam,&
                            'XDATES: INVALID CHARACTER IN FIELD',datefields1(ifield)	
                        endif
                    endif
                enddo
                if (.not. lbad) then
                    if (lsep) then
!                       dates were separated by blank or / (/ converted to blank)
                        read(datefields(ifield),*)adates(ii,1),adates(ii,2),adates(ii,3)
                    else
!                       no delimiter, just numbers
                        read(datefields(ifield),'(a4,2(a2))')adates(ii,1),adates(ii,2),adates(ii,3)
                    endif
                endif
            enddo l1
    
        else !dates separated by spaces
            read(datefields(1),*)adates(1,1)
            read(datefields(2),*)adates(1,2)
            read(datefields(3),*)adates(1,3)
            if (nfield1 .eq. 6) then
                istep=0
            else
                istep=1
            endif
            read(datefields(istep+4),*)adates(2,1)
            read(datefields(istep+5),*)adates(2,2)
            read(datefields(istep+6),*)adates(2,3)
        endif    
          
!       now read the character strings of the years, months, and days
        if (.not. lbad) then
    l2:     do ii=1,2
                iflag=0
                idates=-9
    l3:         do j=1,3
                    read(adates(ii,j),*,iostat=iflag(j))idates(j)
                    if (iflag(j) /= 0) then
                        write(writeunit,formstr(2))adjustl(pathid(ipath)),'E05',modnam,'XDATES: INVALID',&
                            trim(adjustl(timestr1(ii))), trim(adjustl(timestr2(j))),trim(adjustl(adates(ii,j)))              
                        lbad=.true.
                    endif
                enddo l3
                if (iflag(1) == 0 .and. iflag(2) == 0 .and. iflag(3) == 0)then  
                    if (ii == 1) then
                        call checkdates(idates(1),idates(2),idates(3),ii,startdate,lgood)                 
                        if (lgood) then
                            do j=1,3
                                dates1(ii,j)=idates(j)
                            enddo
                        endif
                    else
                        call checkdates(idates(1),idates(2),idates(3),ii,enddate,lgood)        
                        if (lgood) then
    l4:                     do j=1,3
                                dates1(ii,j)=idates(j)
                            enddo l4
                        endif
                    endif
                    if (.not. lgood) lbad=.true.
                endif
            enddo l2
        endif
!       now make sure the start date is before the end date
        if (.not. lbad) then
            if (startdate > enddate) then
                write(writeunit,formstr(3))adjustl(pathid(ipath)),'E05',modnam,'XDATES: START DATE',startdate,'IS AFTER END DATE',&
                    enddate
                 
                lbad=.true.
            endif
        endif
    endif
      
    deallocate(datefields)
    deallocate(datefields1)
    
    return
    end subroutine getdates
!*********************************************************************************************************
          
    subroutine getfields(i1,nfield1,fields)
!=========================================================================================================
!   SUBROUTINE GETFIELDS
!   GET FIELDS OF DATA FROM INPUT RUNSTREAM LINE
!     
!   MODIFIED DECEMBER 2, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE MAIN1 (GETDATES, GETLOC, VAR_LIST) MODULE UPPERAIR (UP_RANGE, UP_MODKEY),
!                   MODULE SURFACE (SURF_RANGE, SF_THRESH, NWS_HGTS) 
!                   MODULE ONSITE (DELTA_T_HT, OS_OBSNUM, OS_RANGE, OS_THRESH, OS_VARS, OSHEIGHTS)
!                   MODULE PBL (FREQ_SEC, METHODS, SECTORS, SITE_CHAR, UP_WINDOW, YEAR_STR)
!
!   INPUT ARGUMENTS
!
!   I1:             INITIAL POSITION TO READ INPUT LINE
!   NFIELD1:        NUMBER OF FIELDS ON INPUT LINE, MINUS KEYWORD
!
!   OUTPUT ARGUMENTS
!     
!   FIELDS:         ARRAY OF CHARACTER STRING FIELDS
!
!   Variable definitions
!      
!   Integer variables
!   i1:             position to start reading input runstream line
!   j:              loop counter
!   ifield:         field counter
!   nfield1:        number of fields on line, minus keyword
!   ind:            array of indices
!     
!   Character variables
!   fields:         Fields read from line
!   modnam:			Subroutine name
!=========================================================================================================
    implicit none
!   do not initialize, input arguments
    integer(kind=4), intent(in) :: nfield1,i1
    integer(kind=4) ::j,ifield,ind(nfield1)
    character(len=100), intent(out) :: fields(nfield1)
    character(len=10) :: modnam='GETFIELDS'
      
!   initialize
    ind=0
    j=0
    ifield=0
!   get number of fields and location of first non-blank (or comma) character of the individual fields
    l1: do j=i1+1,ilen
        !if (ichar(inpline1(j-1:j-1)) == 32 .and. ichar(inpline1(j:j)) /= 32) then
        if ((ichar(inpline1(j-1:j-1)) == 32 .and. (ichar(inpline1(j:j)) /= 32 .and. ichar(inpline1(j:j)) /= 44)) .or. &
            (ichar(inpline1(j-1:j-1)) == 44 .and. (ichar(inpline1(j:j)) /= 32 .and. ichar(inpline1(j:j)) /= 44))) then
            ifield=ifield+1
            ind(ifield)=j
        endif
    enddo l1  

!   fill in fields

    l2: do ifield=1,nfield1
        if (ifield /= nfield1) then
            read(inpline1(ind(ifield):ind(ifield+1)-1),5)fields(ifield)   
        else
            read(inpline1(ind(ifield):ilen),5)fields(ifield)      
        endif
!       if the last character of fields is a comma, set to blank, this is to account for a
!       comma field delimiter
        if (fields(ifield)(len_trim(fields(ifield)):len_trim(fields(ifield)))==',') &
            fields(ifield)(len_trim(fields(ifield)):len_trim(fields(ifield)))=''
    enddo l2
 5  format(a)
    return
    end subroutine getfields
!*********************************************************************************************************

    subroutine checkdates(iyear,imonth,iday,istartend,idate,lgood)
!=========================================================================================================
!   SUBROUTINE CHECKDATES
!   CHECK YEAR, MONTH, AND DAY TO MAKE SURE A VALID NUMBER OR DATE
!     
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE MAIN1 (GETDATES)
!
!   INPUT ARGUMENTS
!
!   IYEAR:          4-DIGIT YEAR
!   IMONTH:         MONTH
!   IDAY:           DAY
!   ISTARTEND:      INDICATOR IF DATE IS THE START OR END DATE
!                   USED FOR ERROR MESSAGE
!                     
!   OUTPUT ARGUMENTS
!     
!   IDATE:          INTEGER DATE OF YYYYMMDD
!   LGOOD:          LOGICAL VARIABLE DENOTING THAT DATES ARE VALID
!      
!   Variable definitions
!      
!   Integer variables
!   iyear:          4-digit year
!   imonth:         month
!   iday:           day of the month
!   day1:           value of days array for the month
!                     being checked.  If February and a leap
!                     year, day1 = 29
!   istartend:      indicator if date is the start or end date
!   idate:          integer date YYYYMMDD
!
!   Logical variables
!   lgood:          Dates are okay
!   leap:           Logical variable denoting year is a 
!                   leap year (true) , or non-leap year (false)
!
!   Character variables
!   timestr1:       time string array for write to error message (start, end).
!   timestr2:       time string array for write to error message (year, month, day)
!   formstr:        format string for writing error message
!   modnam:         Subroutine name
!=========================================================================================================
    implicit none
!   do not initialize, input arguments
    integer(kind=4), intent(in) :: iyear,imonth,iday,istartend
    integer(kind=4) :: day1
    integer(kind=8),intent(out) :: idate !output argument, do not initialize
    logical,intent(out) :: lgood
    logical :: leap
    character(len=5) :: timestr1(2),timestr2(3) 
    character(len=60) :: formstr
    character(len=10) :: modnam='CHECKDATES'
    
    data timestr1 /'START','END'/
    data timestr2 /'YEAR','MONTH','DAY'/
      
!   initialize 
    day1=0
    lgood=.true.
    
!   format for messages
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'2(a,1x),2(a),1x,i8)'
    
!   year must be 4-digit, otherwise a bad year
    if (iyear < 1000 .or. iyear > 9999) then
        write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'XDATES: INVALID',trim(adjustl(timestr1(istartend))),&
            trim(adjustl(timestr2(1))),':',iyear    
        lgood=.false.
    else
!       check to see if a leap year
        call leapyr(iyear,leap)
    endif
          
!   check the month, should be between 1 and 12
    if (imonth < 1 .or. imonth > 12) then
        write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'XDATES: INVALID',trim(adjustl(timestr1(istartend))),&
            trim(adjustl(timestr2(2))),':',imonth 
        lgood=.false.
!       since don't know month, just make sure day is not < 1 or > 31
        if (iday < 1 .or. iday > 31) then
            write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'XDATES: INVALID',trim(adjustl(timestr1(istartend))),&
            trim(adjustl(timestr2(3))),':',iday   
            lgood=.false.
        endif
    else
!       check to make sure day is valid for month
        if (imonth == 2 .and. leap) then
            day1=days(imonth)+1
        else
            day1=days(imonth)
        endif
        if (iday < 1 .or. iday > day1) then
            write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'XDATES: INVALID',trim(adjustl(timestr1(istartend))),&
            trim(adjustl(timestr2(3))),':',iday
            lgood=.false.
        endif
    endif

    if (lgood) idate=iyear*10000+imonth*100+iday

    return
    end subroutine checkdates
!*********************************************************************************************************

    subroutine  getloc(i1,stid,lat,lon,gmt2lst,elev,lgmt2lst,lelev)
!=========================================================================================================
!   SUBROUTINE GETLOC
!   READ LOCATION LINE TO GET STATION ID COORDINATES, GMT-LST CONVERSION, AND ELEVATION
!    
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UPPER_PATH, UP_STAGE2), MODULE SURFACE (SURF_PATH, SF_STAGE2, READ_EXT), 
!                   MODULE ONSITE(OS_PATH, OS_STAGE2), MODULE PBL (PBL_PATH)
!
!   INPUT ARGUMENTS
!
!   I1:             LENGTH OF KEYWORD WITHOUT TRAILING BLANKS+INDEX of KEYWORD in RUNSTREAM LINE
!
!   OUTPUT ARGUMENTS
!     
!   STID:           STATION ID
!   LAT:            LATITUDE
!   LON:            LONGITUDE
!   GMT2LST:        GMT TO LST CONVERSION
!   ELEV:           STATION ELEVATION
!   LGMT2LST:       GMT2LST CONVERSION LISTED
!   LELEV:          ELEVATION SUPPLIED
!
!   Variable definitions
!      
!   Integer variables
!   i1:             length of keyword with trailing blanks removed+index of keword in runstream line
!   ifield:         loop counter
!   nfield1:		nfield-1 (account for keyword)
!   gmt2lst:        GMT to LST conversion
!   iflag:          flag to determine if an error when reading the character
!                   date strings and converting to numbers
!   Real variables:
!   lat:            latitude
!   lon:            longitude
!   elev:           elevation
!
!   Logical variables
!   lgmt2lst:       logical variable indicating if GMT to LST conversion listed (true if listed)
!   lelev:          logical variable indicating if elevation entered (true if so)
!
!   Character variables
!   stid:           integer station ID as character string
!   locfields:      Location information array
!   formstr:        format strings for error messages
!   modnam:			Subroutine name
!=========================================================================================================
    implicit none
    integer(kind=4), intent(in) :: i1
    integer(kind=4) :: nfield1,ifield,iflag
    integer(kind=4), intent(out) :: gmt2lst
    real(kind=r8), intent(out) :: lat,lon,elev
    logical,intent(out):: lgmt2lst,lelev
    character(len=8), intent(out) :: stid
    character(len=100), allocatable, dimension(:) :: locfields
    character(len=50) :: formstr(4)
    character(len=10) :: modnam='GETLOC'
      
!   initialize
    ifield=0
    iflag=0
    stid='0'
    gmt2lst=0
    lat=-999._r8
    lon=-999._r8
    elev=0.0_r8

!   formats for messages
!   1. invalid format for station coordinate, time zone, or station elevation
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2. time zone adjustment exceeds 12 hours
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'i6,1x,a)'
    
!   3.  Incorrect sign for longitude
    write(formstr(3),'(2(a))')trim(adjustl(msg_form)),'i6,1x,a,1x,f10.4)'
    
!   4.  message that station elevation not needed
    write(formstr(4),'(2(a))')trim(adjustl(msg_form)),'a)'
    
!   get # of fields by subtracting 1 from nfield to account for presence of
!   LOCATION keyword      
    nfield1=nfield-1
    allocate(locfields(nfield1))
    locfields='00000000000000000000000000000000'

!   fill in the locfields array
    call getfields(i1,nfield1,locfields)

!   Number of fields:
!   if 3 fields, this means ID, and coordinates listed
!   if 4 fields, this means ID, coordinates, and GMT to LST conversion listed
!   if 5 fields, this means ID, coordinates, GMT to LST conversion, and elevation listed
 
!   read station ID

    read(locfields(1),*,iostat=iflag)stid
    read(locfields(1),*)stid
    
!   read in coordinates, order does not matter
!   will use N or S to determine if latitude and
!   E/W for longitude

    l1: do ifield=2,3
!       get coordinates
!       longitude
        if (index(locfields(ifield),'E') > 0 .or. index(locfields(ifield),'W') > 0) then
            lon=coord(locfields(ifield))
!       latitude
        else if (index(locfields(ifield),'N') > 0 .or. index(locfields(ifield),'S') > 0) then   
            lat=coord(locfields(ifield))
        else
            write(writeunit,formstr(1))adjustl(pathid(ipath)), 'E05',modnam,'INVALID FORMAT FOR STATION COORDINATE',&
                trim(adjustl(locfields(ifield)))   
            lbad=.true.
        endif    
    enddo l1

!   now get GMT to LST conversion
    if (nfield1 >= 4) then
        lgmt2lst=.true.
        read(locfields(4),*,iostat=iflag)gmt2lst
        if (iflag /= 0) then
            lbad=.true.
            write(writeunit,formstr(1))adjustl(pathid(ipath)), 'E05',modnam,'INVALID FORMAT FOR TIME ZONE ADJUSTMENT:',&
                trim(adjustl(locfields(ifield)))     
        else
!           check to see if GMT-LST exceeds +- 12 hours
            if (abs(gmt2lst) > 12) then
                write(writeunit,formstr(2))adjustl(pathid(ipath)), 'E05',modnam,gmt2lst,&
                    'EXCEEDS 12 HOUR ADJUSTMENT FOR TIME ZONE ADJUSTMENT'       
                lbad=.true.
            endif
!           check to see if sign is correct for longitude
!           for western hemisphere, longitude is negative and GMT to LST is +
!           for eastern hemisphere, longitude is positive and GMT to LST is -
!           if 0, assume local time
            if ((gmt2lst > 0 .and. lon > 0.0_r8) .or. (gmt2lst < 0 .and. lon < 0.0_r8)) then
                write(writeunit,formstr(3))adjustl(pathid(ipath)), 'E05',modnam,gmt2lst,&
                    'GMT TO LST IS INCORRECT SIGN FOR LONGITUDE OF',lon
                lbad=.true.
            endif
        endif
    endif

!   get elevation if entered
    if (nfield1 == 5) then
        lelev=.true.
        if (ipath == 2) write(writeunit,formstr(4))adjustl(pathid(ipath)),'W01',modnam,'STATION ELEVATION NOT NEEDED' !elevation not needed for upper air or NWS data
        read(locfields(5),*,iostat=iflag)elev
        if (iflag /=0) then
            lbad=.true.
            write(writeunit,formstr(1))adjustl(pathid(ipath)), 'E05',modnam,'INVALID FORMAT FOR STATION ELEVATION:',&
                trim(adjustl(locfields(5)))
        endif
    endif
      
    deallocate(locfields)

    return
    end subroutine getloc
!*********************************************************************************************************

    real(kind=r8) function coord(lfield)
!=========================================================================================================
!   FUNCTION COORD
!   READ LATITUDE OR LONGITUDE STRING AND CONVERT TO NUMBER
!     
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!    US EPA/AQMG
!
!   CALLED BY:      MODULE MAIN1 (GETLOC)
!
!   INPUT ARGUMENTS
!
!   LFIELD:         COORDINATE TEXT STRING
!
!   OUTPUT
!     
!   COORD:          LATITUDE OR LONGITUDE
!
!   Variable definitions
!      
!   Integer variables
!   j:              loop counter
!   e:              index in coordinate string for 'E' (east longitude) or number of occurences of E
!   w:              index in coordinate string for 'W' (west longitude) or number of occurences of W
!   n:              index in coordinate string for 'N' (north latitude) or number of occurences of N
!   s:              index in coordinate string for 'S' (south latitude) or number of occurences of S
!   iflag:          flag to determine if an error when reading the character
!   ival:           value in ASCII table of character (e.g. blank=32)
!   inum:           Number of numeric characters or decimals in lfield string
!   badloc:         Number of invalid characters in lfield string
!
!   Real variables
!   mult:           multiplier to convert latitude or longitude to correct sign
!                   west longitude, south latitude mult=-1.0, otherwise mult=1.0
!
!   Logical variables
!   nogood:         Coordinate is invalid (true) or valid (false)
!
!   Character variables
!   lfield:         Coordinate text string
!   formstr:        format strings for error messages
!   modnam:         Subroutine name
!=========================================================================================================
    implicit none
    
    integer(kind=4) :: j,e,w,n,s,badloc,iflag,inum,ival     
    real(kind=r8) :: mult
    logical :: nogood
    character(len=40),intent(in):: lfield
    character(len=50) :: formstr(2)
    character(len=10) :: modnam='COORD'

!   initialize
    mult=0.0_r8
    j=0
    e=0
    w=0
    n=0
    s=0
    badloc=0
    iflag=0
    inum=0
    ival=0
    nogood=.false.
   
!   formats for messages
    
!   1.  Invalid station coordinate
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2.  Invalid value for station coordinate
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'2(a,1x),f11.4)'
    
!   read through lfield string
    l1: do j=1,len_trim(lfield)
        ival=ichar(lfield(j:j))
        if (ival == 69) then !E found
            e=e+1
        elseif(ival == 87) then !W found
            w=w+1
        elseif (ival == 78) then !N found
            n=n+1
        elseif (ival == 83) then !S found
            s=s+1
        elseif (ival == 46 .or. (ival >= 48 .and. ival <=57)) then  !decimal or number
            inum=inum+1
        else  !invalid character
        badloc=badloc+1  
        endif
!       if at the end of the string, the last character should be E, W, N, or S
        if (j == len_trim(lfield) .and. ival /= 69 .and. ival /= 87 .and. ival /= 78 .and. ival /= 83) badloc=badloc+1
    enddo l1
    
!   add up e, w, n, and s.  since it should only be one of the 4 and only one
!   occurrence of that particular letter, the value should be 1.  0 or anything
!   other than 1 is bad
    
    ival=e+w+n+s
    
    if (ival /= 1)badloc=badloc+1
    
    if (badloc /= 0) then  !something bad about location field
        write(writeunit,formstr(1))adjustl(pathid(ipath)), 'E05',modnam,'INVALID FORMAT FOR STATION COORDINATE:',&
            trim(adjustl(lfield)) 
        nogood=.true.
    else  !should be valid coordinate
!       determine what coordinate type it is 
        e=index(lfield,'E')
        w=index(lfield,'W')
        n=index(lfield,'N')
        s=index(lfield,'S')
        if (e > 0) then
            mult=1.0
            read(lfield(1:e-1),*,iostat=iflag)coord
        elseif (w > 0) then
            mult=-1.0
            read(lfield(1:w-1),*,iostat=iflag)coord
        elseif (n > 0) then
            mult=1.0
            read(lfield(1:n-1),*,iostat=iflag)coord
        else
            mult=-1.0
            read(lfield(1:s-1),*,iostat=iflag)coord
        endif
        if (iflag /= 0) then !bad string
            write(writeunit,formstr(1))adjustl(pathid(ipath)), 'E05',modnam,'INVALID FORMAT FOR STATION COORDINATE:',&
            trim(adjustl(lfield))   
            lbad=.true.
        else
            if ((e > 0 .or. w > 0)) then
!               outside the bounds of longitude
                if (coord < 0.0_r8 .or. coord > 180.0_r8) then
                    write(writeunit,formstr(2))adjustl(pathid(ipath)), 'E05',modnam,'INVALID VALUE FOR STATION','LONGITUDE',coord
                    nogood=.true.
                endif
            else
!               outside the bounds of latitude
                if (coord < 0.0_r8 .or. coord > 90.0_r8) then
                    write(writeunit,formstr(2))adjustl(pathid(ipath)), 'E05',modnam,'INVALID VALUE FOR STATION','LATITUDE',coord         
                    nogood=.true.
                endif
            endif
        endif
    endif
      
!   calculate coordinate with correct sign
    if (nogood) then
        lbad=.true.
        coord=-999.0
    else
        coord=coord*mult
    endif
      
    return
    end function coord
!*********************************************************************************************************

    subroutine var_list(i1,nfield1,varnam,varlogic,nvar)
!=========================================================================================================
!   SUBROUTINE VAR_LIST
!   READ LIST OF VARIABLES TO NOT REPORT MISSING VALUES IN MESSAGE FILE WHEN AUDITING OR
!   VARIABLES TO AUDIT
!   NOTE, IF NO_MISSING KEYWORD IS SPECIFIED BUT AUDIT IS NOT SPECIFIED, VALUES WILL BE RESET
!   SO MISSING VALUES ARE NOT AUTOMATICALLY REPORTED.  
!     
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UPPER_LIST), MODULE SURFACE (SURF_LIST), MODULE ONSITE(OS_LIST)
!
!   INPUT ARGUMENTS
!
!   I1:             STARTING INDEX TO READ FROM INPUT LINE
!   NFIELD1:        NUMBER OF FIELDS TO READ ON LINE, I.E. # OF VARIABLES
!   VARNAM:         ARRAY OF VARIABLE NAMES TO COMPARE AGAINST
!   VARLOGIC:       INITIAL VALUES OF LOGICAL VARIABLE TO REPORT MISSING VALUES OR AUDIT
!   NVAR:           NUMBER OF VARIABLES (UPPERAIR, SURFACE, ONSITE)
!
!   OUTPUT
!     
!   VARLOGIC:       RESET VALUES OF LOGICAL VARIABLE TO REPORT MISSING VALUES OR AUDIT
!
!   Variable definitions
!      
!   Integer variables
!   i1:             starting index to read from input line
!   nfield1:        number of fields to read on line
!   nvar:           number of variables (upperair, surface, onsite)
!   ifield:         field loop counter
!   ivar:           variable loop counter
!   ilev:           integer level number from varnam1; used to determine if a possible multi-level variable
!                   for ONSITE or PROG data
!   nlev:           integer max level number from varnam; used to determine if a possible multi-level variable
!                   for ONSITE or PROG data
!   iflag1:         I/O flag for reading the height for varnam1 for ONSITE or PROG multi-level variable
!   iflag2:         I/O flag for reading the height for varnam for ONSITE or PROG multi-level variable
!
!   Logical variables
!   varlogic:       array of logical variables to determine if to not output missing values in 
!                   message file or audit. 
!   lfound:         logical variable denoting that a variable in varnam1 found in varnam
!
!   Character variables
!   varnam:         array of variable names to compare against
!                   these are the upperair, surface, or onsite variable lists
!   varnam1:        array of variable names read from input line
!   formstr:        format strings for error messages
!   modnam:			Subroutine name
!=========================================================================================================
    implicit none
!   do not initialize, input argument
    integer(kind=4), intent(in) :: i1,nfield1,nvar
    integer(kind=4) :: ifield,ivar,ilev,iflag1,iflag2,nlev
    logical,intent(inout) :: varlogic(nvar)
    logical :: lfound
    character(len=4), intent(in) :: varnam(nvar)
    character(len=100) :: varnam1(nfield-1)  !for getfields
    character(len=50) :: formstr
    character(len=40) :: modnam='VAR_LIST'
      
!   initialize
    ifield=0
    ivar=0
    ilev=0
    iflag1=0
    iflag2=0
    nlev=0
    
!   format for messages
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'3(a,1x),a)'
    
!   read the input line to get the list of variables for keyword
    call getfields(i1,nfield1,varnam1)

!   check to see if the list of variables is in the master list 
!   of the input varnam
    l1: do ifield=1,nfield1
        ivar=1
        lfound=.false.
        do while (ivar <= nvar .and. .not. lfound .and. (len_trim(varnam1(ifield)) == 4 .or. len_trim(varnam1(ifield))==2))
            if (trim(adjustl(varnam1(ifield))) == trim(adjustl(varnam(ivar)))) then
                lfound=.true.
                if (.not. varlogic(ivar))  then
                    varlogic(ivar)=.true.
                else
                    write(writeunit,formstr)adjustl(pathid(ipath)),'W02',modnam,'DUPLICATING LISTING OF VARIABLE',&
                                trim(adjustl(varnam1(ifield))),'ON INPUT LINE FOR KEYWORD:',trim(adjustl(keywrd(ikey)))      
                endif
            else
!               check to see if a multi-level variable for ONSITE or PROG
!               compare the first two characters and also read the
!               last two characters of the variables to get the heights
!               varnam should be equal to maxlevel set in module ONSITE
!               also allow only the first 2 characters
                if ((ipath == 4 .or. ipath == 5) .and. ivar >= 26 .and. varnam1(ifield)(1:2) == varnam(ivar)(1:2)) then
                    read(varnam1(ifield)(3:4),*,iostat=iflag1)ilev !level number, i.e. 01, 02, etc.
                    read(varnam(ivar)(3:4),*,iostat=iflag2)nlev !max level number = maxlevel in mod_onsite
                    if ((iflag1 == 0 .and. iflag2 == 0 .and. ilev <= nlev) .or. len_trim(varnam1(ifield)) == 2) then !potential valid number or only 1st 2 characters input
                        lfound=.true.
                        if (.not. varlogic(ivar))  then
                            varlogic(ivar)=.true.
                        else
                            write(writeunit,formstr)adjustl(pathid(ipath)),'W02',modnam,'DUPLICATING LISTING OF VARIABLE',&
                                trim(adjustl(varnam1(ifield)(1:2))),'ON INPUT LINE FOR KEYWORD:',trim(adjustl(keywrd(ikey)))      
                        endif
                    else
                        ivar=ivar+1
                    endif
                else
                    ivar=ivar+1
                endif
            endif
        enddo
!       input variable string not found, issue error
        if (.not. lfound) then
!           set varlogic to true for all variables if nfield1 is 1 and the variable string is ALL
!           if the nfield1 is not 1 or nfield1 is 1 and the string is not ALL, then issue error
            if (nfield1 == 1 .and. trim(adjustl(varnam1(nfield1))) == 'ALL') then
                varlogic=.true.
            else
                write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID VARIABLE NAME FOR',&
                    trim(adjustl(keywrd(ikey))),'KEYWORD',trim(adjustl(varnam1(ifield)))     
                lbad=.true.
            endif
        endif
    enddo l1
    
    return
    end subroutine var_list
!*********************************************************************************************************

    subroutine range_mod(v1,v2,v3,v4,linc,lowval,hival,missval,lgood)
!=========================================================================================================
!   SUBROUTINE RANGE_MOD
!   READ VARIABLE AND UPDATED RANGE CHECKS, INCLUDING WHETHER TO INCLUDE/EXCLUDE BOUNDS IN CHECK  
!     
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_RANGE), MODULE SURFACE (SURF_RANGE), MODULE ONSITE(OS_RANGE)
!
!   INPUT ARGUMENTS
!
!   V1:             CHARACTER STRING OF LOWER BOUND
!   V2:             CHARACTER STRING OF < OR <=
!   V3:             CHARACTER STRING OF UPPER BOUND
!   V4:             CHARACTER STRING OF MISSING VALUE
!
!   OUTPUT ARGUMENTS
!   
!   LINC:           LOGICAL VALUE TO INCLUDE LOWER AND UPPER BOUNDS (TRUE)]
!                   OR EXCLUDE (FALSE)
!                   VALUE IS BASED ON THE VALUE OF V2
!   LOWVAL:         NUMERIC LOWER BOUND READ FROM V1
!   HIVAL:          NUMERIC UPPER BOUND READ FROM V3
!   MISSVAL:        NUMERIC MISSING VALUE READ FROM V4
!   LGOOD:          LOGICAL VARIABLE DENOTING IF ALL VALUES ARE VALID (TRUE)
!                   OR NOT (FALSE).  THIS VARIABLE IS USED TO DECIDE WHETHER
!                   TO WRITE THE NEW VALUES TO THE UPPER AIR ARRAYS.
!
!   Variable definitions
!      
!   Integer variables
!   lowval:         numeric lower bound read from v1
!   hival:          numeric upper bound read from v3
!   missval:        numeric missing value read from v4
!   iflag:          i/o flag denoting if number read from character string
!
!   Logical variables
!   linc:           logical value to include lower and upper bounds (true)]
!                   or exclude (false)
!                   value is based on the value of v2
!   lgood:          logical variable denoting if all values are valid (true)
!                   or not (false).  this variable is used to decide whether
!                   to write the new values to the upper air arrays.
!
!   Character variables
!   v1:             character string of lower bound
!   v2:             character string of < or <=
!   v3:             character string of upper bound
!   v4:             character string of missing value
!   formstr:        format strings for error messages
!   modnam:         Subroutine name
!=========================================================================================================
    implicit none
!   do not initialize output arguments
    integer(kind=4), intent(out) :: lowval,hival,missval
    integer(kind=4):: iflag
    logical,intent(out):: lgood,linc
    character(len=40), intent(in) :: v1,v2,v3,v4
    character(len=50) :: formstr
    character(len=10) :: modnam='RANGE_MOD'
      
!   initialize
    iflag=0
    lowval=-9
    hival=-9
    missval=0
    lgood=.true.
    linc=.false.
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   read the lower bound
    read(v1,*,iostat=iflag)lowval
    if (iflag /= 0) then
        write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR LOWER BOUND',trim(adjustl(v1))
        lbad=.true.
    endif
      
!   read the field to determine whether bounds are inclusive or not
!   valid strings are: < or <=
      
    if (trim(adjustl(v2)) == '<') then
        linc=.false.
    else if (trim(adjustl(v2)) == '<=') then
        linc=.true.
    else
        lbad=.true.
        write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR RANGE INCLUSION KEY:',trim(adjustl(v2))  
    endif
     
!    read the upper bound
    read(v3,*,iostat=iflag)hival
    if (iflag /= 0) then
        write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR UPPER BOUND',trim(adjustl(v3))
        lbad=.true.
    endif
      
!   read the missing value
    read(v4,*,iostat=iflag)missval
    if (iflag /= 0) then
        write(writeunit,formstr)adjustl(pathid(ipath)),'E05',modnam,'INVALID VALUE FOR MISSING VALUE',trim(adjustl(v4))
        lbad=.true.
    endif  
      
    if (lbad) lgood=.false.

    return
    end subroutine range_mod
!*********************************************************************************************************

    subroutine getunit
!=========================================================================================================
!   SUBROUTINE GETUNIT
!   SUBROUTINE TO DETERMINE WHAT FILE OR STANDARD OUTPUT TO WRITE MESSAGE FILE TYPE MESSAGES 
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UPPER_PATH), MODULE SURFACE (SURF_PATH), MODULE ONSITE (OS_PATH),
!                   MODULE READ_INPUT (CHECK_LINE, JOB_PATH), MODULE PBL (PBL_PATH)
!
!   Variable definitions
!      
!   Logical variables
!   lopened:        logical variable to determine if message file already open for writing.
!
!   Character variables
!   modnam:             Subroutine name
!=========================================================================================================
    implicit none
    logical :: lopened
    character(len=10) :: modnam='GETUNIT'
      
!   check to see if the message file has been opened
    if (lkey(2)) then
!       should be open but check to make sure
        inquire(file=msgfile,opened=lopened)
        if (lopened) then
!           write to message file
            writeunit=msg_unit
        else
!           write to screen
            writeunit=output_unit
        endif
    else
!       MESSAGE keyword not found yet        
        writeunit=output_unit
    endif
    return
    end subroutine getunit
!*********************************************************************************************************

    integer(kind=4) function numdays(start1,end1)
!=========================================================================================================
!   FUNCTION NUMDAYS
!   CALCULATE NUMBER OF DAYS IN A PERIOD BASED ON THE START AND END DATES (YYYYMMDD)
!     
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (UP_PROC, NEW_SOUND), MODULE SURFACE (SF_PROC, NEW_OBS, READ_1MIN),
!                   MODULE ONSITE (OS_PROC, READ_EXT, READ_OS), MODULE PBL (PBL_PROC)
!
!   INPUT ARGUMENTS
!
!   START1:         START DATE OF PERIOD (YYYYMMDD)
!   END1:           END DATE OF PERIOD (YYYYMMDD)
!
!   OUTPUT
!     
!   NDAYS:          NUMBER OF DAYS BETWEEN START1 AND END1
!
!   Variable definitions
!      
!   Integer variables
!   start1:         start date of period (YYYYMMDD)
!   end1:           end date of period (YYYYMMDD)
!   julian:         2-element array of the Julian day of the start date(1)
!                   and end date (2)
!   jday1:          variable used to calculate Julian day
!   jday2:          variable used to calculate Julian day
!   ndays:          number of days for the year (non-leap year is 365,
!                   leap year is 366)
!   iyear:          2-element array: start year (1) and end year (2). 
!                   used to calculate number of years that are leap and non-leap
!                   as well.
!   imonth:         month of start or end date
!   iday:           day of month of start or end date
!   nyears(2)       2-element array of number of non-leap years (1) and
!                   leap years (2).
!     
!   Logical variables
!   lleap:          indicates year is leap year (true) or non-leap year (false)
!
!   Character variables
!   modnam:             Subroutine name
!   adate:              8-character string of start or end date
!=========================================================================================================
    implicit none
!   do not initialize, input arguments
    integer(kind=8), intent(in) :: start1,end1
    integer(kind=4) :: i,julian(2),jday1,jday2,iyear(2),imonth,iday,nyears(2),ndays
    logical :: lleap !set by subroutine leapyr
    character(len=10):: modnam='NUMDAYS'
    character(len=8)adate
      
!   initialize
    i=0
    julian=0
    jday1=0
    jday2=0
    ndays=0
    iyear=0
    imonth=0
    iday=0
    nyears=0
      
    l1: do i=1,2
        if (i==1) then
            write(adate,'(i8.8)')start1
        else
            write(adate,'(i8.8)')end1
        endif
        read(adate,'(i4,3(i2))')iyear(i),imonth,iday
        jday1=mod((imonth+9),12)
        jday2=(jday1*153+2)/5+iday+58
!       check to see if the year is a leap year
        call leapyr(iyear(i),lleap)
        if (lleap) then
            ndays=366
            jday2=jday2+1
        else
            ndays=365
        endif
        julian(i)=1+mod(jday2,ndays)
    enddo l1
      
!   now get the number of leap years and regular years in the data period
    nyears=0
    l2: do i=iyear(1),iyear(2)-1
        call leapyr(i,lleap)
        if (lleap) then  !leap year
            nyears(2)=nyears(2)+1
        else
            nyears(1)=nyears(1)+1
        endif
    enddo l2
      
!   now calculate number of days in the period
    numdays=julian(2)+nyears(1)*365+nyears(2)*366-julian(1)+1
      
    return
    end function numdays
!*********************************************************************************************************

    subroutine data_dates(ihr,iday,imonth,iyear,gmt2lst,istart,iend,ierr,idate,lgo)
!=========================================================================================================
!   SUBROUTINE DATA_DATES
!   CHECK YEAR, MONTH, DAY, AND HOUR TO MAKE SURE A VALID NUMBER OR DATE FROM UPPERAIR, SURFACE, OR
!   ONSITE (PROG) DATA.  SUBROUTINE ALSO CONVERTS GMT TO LST TIMES IF APPLICABLE
!
!   MODIFIED DECEMBER 2, 2021
!
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE UPPERAIR (READ_6201, READ_FSL, READ_IGRA), 
!                   MODULE SURFACE (NEW_OBS, READ_ISHD),
!                   MODULE ONSITE (AUDIT, READ_EXT, READ_OS),
!                   MODULE PBL (CHECK_OBS)
!
!   INPUT ARGUMENTS
!
!   IHR:            HOUR OF DAY (1-24 OR 0-23) BEFORE GMT TO LST
!   IDAY:           DAY BEFORE GMT TO LST
!   IMONTH:         MONTH BEFORE GMT TO LST
!   IYEAR:          4-DIGIT YEAR BEFORE GMT TO LST
!   GMT2LST:        GMT TO LST CONVERSION
!   ISTART:         START DATE (YYYYMMDD) TO COMPARE IDATE AGAINST
!   IEND:           END DATE (YYYYMMDD) TO COMPARE IDATE AGAINST
!   IERR:           ERROR CODE TO WRITE IF ERROR
!                     
!   OUTPUT ARGUMENTS
!    
!   IHR:            HOUR OF DAY (1-24 OR 0-23) AFTER GMT TO LST
!   IDAY:           DAY AFTER GMT TO LSTAFTER GMT TO LST
!   IMONTH:         MONTH AFTER GMT TO LST
!   IYEAR:          4-DIGIT YEAR AFTER GMT TO LST
!   IDATE:          INTEGER DATE OF YYYYMMDD
!   LGO:            LOGICAL VARIABLE DENOTING IF DATE IS BETWEEN
!                   ISTART AND IEND AND VALID
!   
!   Variable definitions
!      
!   Integer variables
!   ihr:            hour of day (1-24 or 0-23)
!   iday:           day
!   imonth:         month
!   iyear:          4-digit year
!   gmt2lst:        gmt to lst conversion
!   lastday:        last day of the month accounting for leap year if February
!   ierr:           error code to write if error
!   day1:           value of days array for the month
!                   being checked.  If February and a leap
!                   year, day1 = 29
!   istart:         start date (YYYYMMDD) to compare idate against
!   iend:           end date (YYYYMMDD) to compare idate against
!   idate:          integer date YYYYMMDD
!
!   Logical variables
!   lgo:            Date is okay and between istart and iend
!   leap:           Logical variable denoting year is a 
!                   leap year (true) , or non-leap year (false)
!
!   Character variables
!   timestr2:       time string array for write to error message (year, month, day)
!   ecode:          error code string for writing to message file
!   formstr:        format strings for error messages
!   modnam:         Subroutine name
!=========================================================================================================      
    implicit none
!   do not initialize, input argument
    integer(kind=4), intent(in) ::gmt2lst,ierr 
    integer(kind=4), intent(inout) :: iyear,imonth,iday,ihr
    integer(kind=4):: day1,lastday
    integer(kind=8),intent(in) :: istart,iend
    integer(kind=8),intent(out) :: idate
    logical,intent(out) :: lgo
    logical :: leap
    character(len=3) :: ecode
    character(len=5) :: timestr2(4)
    character(len=50) :: formstr
    character(len=10) :: modnam='DATA_DATES'
      
    !data timestr1 /'START','END'/
    data timestr2 /'YEAR','MONTH','DAY','HOUR'/
      
!   initialize
    day1=0

    lastday=0
    idate=0
    lgo=.false.
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,2(a),1x,i8)'
    
!   create ecode 
    write(ecode,'(a,i2.2)')'E',ierr
!   year must be 4-digit, otherwise a bad year
    if (iyear < 1000 .or. iyear > 9999) then
        write(msg_unit,formstr)adjustl(pathid(ipath)),ecode,modnam,'INVALID',trim(adjustl(timestr2(1))),':',iyear 
        lbad=.true.
    else
!       check to see if a leap year
        call leapyr(iyear,leap)
    endif
          
!   check the month, should be between 1 and 12
    if (imonth < 1 .or. imonth > 12) then
        write(msg_unit,formstr)adjustl(pathid(ipath)),ecode,modnam,'INVALID',trim(adjustl(timestr2(2))),':',imonth
        lbad=.true.
!       since don't know month, just make sure day is not < 1 or > 31
        if (iday < 1 .or. iday > 31) then
            write(msg_unit,formstr)adjustl(pathid(ipath)),ecode,modnam,'INVALID',trim(adjustl(timestr2(3))),':',iday  
            lbad=.true.
        endif
    else
!       check to make sure day is valid for month
        if (imonth == 2 .and. leap) then
            day1=days(imonth)+1
        else
            day1=days(imonth)
        endif
        if (iday < 1 .or. iday > day1) then
            write(msg_unit,formstr)adjustl(pathid(ipath)),ecode,modnam,'INVALID',trim(adjustl(timestr2(3))),':',iday
            lbad=.true.
        endif
    endif

    if (.not. lbad) then
!       check the hour before any GMT to LST
        if (ihr < 0 .or. ihr > 24) then
            write(msg_unit,formstr)adjustl(pathid(ipath)),ecode,modnam,'INVALID',trim(adjustl(timestr2(4))),':',ihr    
            lbad=.true.
        else !convert time
!           note, subtracting the gmt2lst offset. for western hemisphere
!           this number is positive, eastern hemisphere is negative
!           so subtracting a negative will be adding, which is correct for
!           eastern hemisphere
            
            ihr=ihr-gmt2lst
!           if ihr is negative or 0 then set to previous day's hour
!           i.e. if ihr is -5, then ihr is actually hour 19
!           hour 0 will become hour 24 of the day before
            if (ihr <= 0) then
                ihr=24+ihr
!               reset the day
                iday=iday-1
                if (iday == 0) then !reset to the previous month's last day
                    imonth=imonth-1
                    if (imonth == 0) then !reset to December of previous year and reset year
                        imonth=12
                        iyear=iyear-1
                    endif
                    if (imonth == 2 .and. leap) then
                        iday=days(imonth)+1
                    else
                        iday=days(imonth)
                    endif
                endif
            elseif (ihr > 24) then
                ihr=ihr-24
!               reset the day
                iday=iday+1
                if (imonth == 2 .and. leap) then
                    lastday=days(imonth)+1
                else
                    lastday=days(imonth)
                endif
!               reset the day to the next month's first day if
!               the reset day is > the month's last day
                if (iday > lastday) then
                    iday=1
                    imonth=imonth+1
                    if (imonth == 13) then !reset to January of next year and reset year
                        imonth=1
                        iyear=iyear+1
                    endif
                endif
            endif
        endif
    endif
!   calculate idate, it may be reset 
!   after gmt2lst conversion
    if (.not. lbad) then
        idate=iyear*10000+imonth*100+iday
    endif
          
    if (.not. lbad .and. istart <= idate .and. idate <= iend) lgo=.true.

    return
    end subroutine data_dates
!*********************************************************************************************************

    real(kind=r8) function humidity(db,dp)
!=========================================================================================================
!   FUNCTION HUMIDITY
!   CALCULATE HUMIDITY FROM TEMPERATURE AND DEWPOINT
!     
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (READ_ISHD), MODULE PBL (RH)
!
!   INPUT ARGUMENTS
!
!   DB:             DRY BULB TEMPERATURE IN CELCIUS
!   DP:             DEW POINT IN CELCIUS
!
!   OUTPUT
!     
!   HUMIDITY:       RH IN PERCENT
!
!   Variable definitions
!      
!   Integer variables
!   humidity:       RH in percent
!
!   Real variables
!   db:             dry bulb temperature in Celcius
!   dp:             dew point in Celcius
!   dbk:            dry bulb temperature in Kelvin
!   dpk:            dew point in Kelvin
!   ttdb:           difference between inverse of 273 and temperature
!   ttdp:           difference between inverse of 273 and dewpoint
!   vpsat:          saturation vapor pressure
!   vpact:          actual vapor pressure

!
!   Character variables
!   modnam:         Subroutine name
!=========================================================================================================
    implicit none
             
    real(kind=r8),intent(in) :: db,dp
    real(kind=r8) :: dbk,dpk,ttdb,ttdp,vpsat,vpact
    character(len=10) :: modnam='HUMIDITY' 
    
    humidity=0 !initialize
    
!   convert temperatures to K
    dbk=db+273.15_r8
    dpk=dp+273.15_r8
    
    ttdb=(1.0_r8/273.15_r8)-(1.0_r8/dbk)
    ttdp=(1.0_r8/273.15_r8)-(1.0_r8/dpk)
   
!   5418 = latent heat of vaporation/gas constant
!       4.61E6/461
    
    vpsat = 6.11_r8 * dexp(5418.0_r8 * ttdb)
    vpact = 6.11_r8 * dexp(5418.0_r8 * ttdp)

!   integer value of humidity
    humidity=int(100._r8*(vpact/vpsat))
    return
    end function humidity
!*********************************************************************************************************

    real(kind=r8) function pressure(iopt,elev,tv,inp_pres)
!=========================================================================================================
!   FUNCTION PRESSURE
!   CALCULATE PRESSURE FROM ELEVATION, MEAN VIRTUAL TEMPERATURE, AND INPUT PRESSURE
!   IF INPUT PRESSURE IS STATION PRESSURE, CALCULATE THE SEA LEVEL PRESSURE
!   IF INPUT PRESSURE IS SEA LEVEL PRESSURE, CALCULATE STATION PRESSURE
!     
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE SURFACE (READ_ISHD), MODULE PBL (PRESS)
!
!   INPUT ARGUMENTS
!
!   IOPT:           INTEGER OPTION (1=sea level pressure given station pressure)
!                                  (2=station pressure given sea level pressure)
!   ELEV:           STATION ELEVATION (M)
!   TV:             MEAN VIRTUAL TEMPERATURE (K)
!   INP_PRES:       INPUT PRESSURE (IF IOPT = 1 THEN INP_PRES=STATION PRESSURE; OTHERWISE
!                   INP_PRES=SEA LEVEL PRESSURE)
!
!   OUTPUT
!     
!   PRESSURE:       OUTPUT PRESSURE
!
!   Variable definitions
!      
!   Integer variables
!   iopt:           integer option 
!
!   Real variables
!   inp_pres:       input pressure (mb)
!   elev:           station elevation (m)
!   tv:             mean virtual temperature(K)
!   pressure:       output pressure
!   denom:          R*tv/g
!
!   Character variables
!   modnam:         Subroutine name
!=========================================================================================================
    implicit none
             
    integer(kind=4),intent(in) :: iopt
    real(kind=r8),intent(in) :: inp_pres,elev,tv
    real(kind=r8) :: denom
    character(len=10) :: modnam='PRESSURE'
    
    denom=r*tv/g
    
    if (iopt .eq. 1) then
        pressure=inp_pres*dexp(elev/denom)
    else
        pressure=inp_pres*dexp(-elev/denom)
    endif
    
    return
    end function pressure
    
!*********************************************************************************************************      
    end module main1