    module reports
!===========================================================================================================
!   MODULE REPORTS
!   THIS MODULE CONTAINS COMMON VARIABLES AND SUBROUTINES NEEDED THROUGOUT AERMET PROCESSING 
!   TO WRITE TO THE REPORT FILE OR SCREEN
!
!   MODIFIED MAY 20, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   USED BY:    AERMET
!=========================================================================================================   
    
    use file_units, only: rpt_unit
    implicit none
    
    contains
!*********************************************************************************************************
      
    subroutine input_summ
!=========================================================================================================
!   SUBROUTINE INPUT_SUMM
!   THIS SUBROUTINE SUMMARIZES THE INPUTS FROM THE AERMET RUNSTREAM FILE
!
!   MODIFIED APRIL 22, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      AERMET
!
!   Variable definitions
!      
!   Integer variables
!   i:              loop counter
!   j:              loop counter
!
!   Logical variables
!   lopened:        logical variable denoting open status of file
!
!   Character variables
!   asterisks:      string of asterisks for headers
!   file_status:    character string to describe status of a file, OPENED or UN-OPENED
!                   UN-OPENED implies bad filename
!   success_str:    character string to describe the status of the AERMET run
!                   UN-SUCCESSFUL or SUCCESSFUL
!   stage_str:      character string to describe which stage(s) being processed
!   modnam:         Subroutine name
!=========================================================================================================
    use file_units, only: inpfile,msgfile,rptfile,up_inpfile,up_qafile,up_extfile,sf_inpfile,sf_qafile,sf_extfile,one_minfile,&
        os_inpfile,os_qafile,sfc_outfile,pfl_outfile,dbgfile
    use main1, only: keywrd,pathid,lpath,npath,lstage,versn,start_stop,items,actions
    use upperair, only: lupkeywords,uplat,uplon,upgmt2lst,upid,upstart,upend,updates,upelev,lmodify,uptop
    use surface, only:  lsfkeywords,sflat,sflon,sfgmt2lst,sfid,sfstart,sfend,sfdates,sfelev,lasos,iasos,asoscommdates,asos_thresh
    use onsite, only:  loskeywords,oslat,oslon,osgmt2lst,osid,osstart,osend,osdates,oselev,nobs_hr,osdt_hts,threshspeed,&
        os_heights,noshts,overland,osvars,mix_var
    use pbl, only: lpblkeywords,pbllat,pbllon,pblgmt2lst,pblstart,pblend,pbldates,pblelev,pblid,litems,lactions,snding_win,&
        subcloud,subtemp
    use read_input, only: lbad1,ljbkeywords
    
    implicit none
    integer(kind=4) :: i,j
    logical :: lopened
    character(len=10) :: modnam='INPUT_SUMM'
    character(len=8) :: file_status
    character(len=13) :: success_str
    character(len=29) :: asterisks='****************************'
    character(len=7) :: stage_str
      
!   initialize
    i=0
    j=0
    lopened=.false.
      
    if (lbad1) then
        success_str='UN-SUCCESSFUL'
    else
        success_str='SUCCESSFUL'
    endif
    
!   post 21DRF, move the message that AERMET setup was sucessful to top of file
    write(rpt_unit,'(2(1x,a/))')'AERMET_SETUP',trim(adjustl(success_str))
      
!   write the AERMET version 
    write(rpt_unit,'(1x,a,1x,a5/)')'AERMET VERSION ',versn
!   processing start time
    call start_stop(1,rpt_unit)
      
!   input runstream filename
    write(rpt_unit,'(/2(1x,a))')'RUNSTREAM CONTROL FILE:',trim(adjustl(inpfile))
      
!   if only checking the syntax and not performing any calculations, issue message
!   that only the runstream file is being checked
    if (ljbkeywords(3))write(rpt_unit,'(/,1x,a)')'THIS RUN ONLY CHECKS THE RUNSTREAM INPUT'
    
!   processing stages summary
    if (lstage(1) .and. lstage(2)) then
        stage_str='1 AND 2'
    elseif (lstage(1)) then
        stage_str='1'
    else
        stage_str='2'
    endif
    
    !write(rpt_unit,'(//1x,a29,1x,a,1x,a29,2(//2(1x,a)))')asterisks,'INPUT SUMMARY',asterisks,'AERMET_SETUP',&
    !    trim(adjustl(success_str)),'PROCESSING STAGES',trim(adjustl(stage_str))
!   post 21DRF, move AERMET setup message to top of report
    write(rpt_unit,'(//1x,a29,1x,a,1x,a29,//2(1x,a))')asterisks,'INPUT SUMMARY',asterisks,&
        'PROCESSING STAGES',trim(adjustl(stage_str))
    
!   job pathid summary
    i=1
    write(rpt_unit,'(///1x,i1,a/)')i,'.  JOB FILE NAMES'
!   message file
    if (ljbkeywords(2)) then
        inquire(file=msgfile,opened=lopened) 
        if (lopened) then
            file_status='OPEN'
        else
            file_status='NOT OPEN'
        endif
        write(rpt_unit,'(1x,a10,1x,a8,1x,a)')adjustl(keywrd(2)),adjustl(file_status),trim(adjustl(msgfile))
    else
        write(rpt_unit,'(1x,a10,1x,a)')adjustl(keywrd(2)),'NO FILE'
    endif
      
!   report file
    if (ljbkeywords(1)) then
        inquire(file=rptfile,opened=lopened) 
        if (lopened) then
            file_status='OPEN'
        else
            file_status='NOT OPEN'
        endif
        write(rpt_unit,'(1x,a10,1x,a8,1x,a)')adjustl(keywrd(1)),adjustl(file_status),trim(adjustl(rptfile))
    else
        write(rpt_unit,'(1x,a10,1x,a)')adjustl(keywrd(1)),'NO FILE'
    endif
      
!   debug file
    if (ljbkeywords(5)) then
        inquire(file=dbgfile,opened=lopened) 
        if (lopened) then
            file_status='OPEN'
        else
            file_status='NOT OPEN'
        endif
        write(rpt_unit,'(1x,a10,1x,a8,1x,a)')adjustl(keywrd(25)),adjustl(file_status),trim(adjustl(dbgfile))
    else
        write(rpt_unit,'(1x,a10,1x,a)')adjustl(keywrd(25)),'NO FILE'
    endif
    
!   now for the various input paths of upperair, surface, onsite, or MMIF
!   do not include MERGE which is legacy and equal to npath
    l1: do i=2,npath-1
        write(rpt_unit,'(///1x,i1,a1,2(1x,a))')i,'.',trim(adjustl(pathid(i))),'DATA'
        if (.not. lpath(i)) then
            write(rpt_unit,'(/1x,a/)')'NO PROCESSING REQUESTED'
        else
!           processing dates and locations
            if (i==2) then
                call dates_loc(i,upstart,upend,updates,upid,uplat,uplon,upgmt2lst,upelev)
            else if (i==3) then
                call dates_loc(i,sfstart,sfend,sfdates,sfid,sflat,sflon,sfgmt2lst,sfelev)
            else if (i== 6) then
                call dates_loc(i,pblstart,pblend,pbldates,pblid,pbllat,pbllon,pblgmt2lst,pblelev)
            else
                call dates_loc(i,osstart,osend,osdates,osid,oslat,oslon,osgmt2lst,oselev)
            endif
!           write input/output file summary
!           1st argument is ipath, 2nd argument is file type, i.e. DATA, EXTRACT, etc.
!           see subroutine sum_files for file type indicators
            if (i==2) then !UPPERAIR
                call sum_files(1,lupkeywords(1),up_inpfile) !DATA
                call sum_files(2,lupkeywords(3),up_extfile) !EXTRACT
                call sum_files(3,lupkeywords(4),up_qafile) !QAOUT
                write(rpt_unit,'(/1x,a,1x,i4,1x,a)')'UPPER AIR DATA ABOVE',uptop,'NOT EXTRACTED' !upper level extracted
                  
!               if any variables are to be audited, list them
!               put in list of audit variables
                  
                if (lupkeywords(7)) then !status of MODIFY keyword
                    write(rpt_unit,'(/1x,a)')'UPPER AIR AUTOMATIC DATA CHECKS ARE: ON'
!                   list individual checks
                    if (lmodify(1)) write(rpt_unit,'(/1x,a)')'DELETE MANDATORY LEVELS'
                    if (lmodify(2)) write(rpt_unit,'(/1x,a)')'RESET WIND DIRECTION FOR UAWS=0'
                    if (lmodify(3)) write(rpt_unit,'(/1x,a)')'INTERPOLATE MISSING UATT AND UATD'
                else
                    write(rpt_unit,'(/1x,a)')'UPPER AIR AUTOMATIC DATA CHECKS ARE: OFF'
                endif
            elseif (i==3) then !SURFACE
!               write ASOS commission date if applicable
                if (lasos .and.iasos > 0)write(rpt_unit,'(/1x,a,1x,i8)')'ASOS COMMISSION DATE:',asoscommdates(iasos)%commisdate 
                call sum_files(1,lsfkeywords(1),sf_inpfile) !DATA
                if (lstage(2)) call sum_files(4,lsfkeywords(9),one_minfile) !ASOS1MIN
                call sum_files(2,lsfkeywords(3),sf_extfile) !EXTRACT
                call sum_files(3,lsfkeywords(4),sf_qafile) !QAOUT
            elseif (i== 6) then !METPREP
                call sum_files(5,lpblkeywords(6),sfc_outfile) !OUTPUT i.e. SURFFILE in AERMOD
                call sum_files(6,lpblkeywords(10),pfl_outfile) !PROFILE i.e. PROFFILE in AERMOD
!               summarize options
                write(rpt_unit,'(2(/1x,a)/)')'METPREP PROCESSING OPTIONS','PROCESS                     OPTION  DESCRIPTION'
!               wind direction randomnization
                if (litems(1)) then
                    if (lactions(1)) then
                        write(rpt_unit,'(1x,a,t30,a7,1x,a)')'WIND DIRECTION',actions(1),'WIND DIRECTIONS ARE RAMDOMNIZED'
                    else
                        write(rpt_unit,'(1x,a,t30,a7,1x,a)')'WIND DIRECTION',actions(2),'WIND DIRECTIONS ARE NOT RAMDOMNIZED'
                    endif
                else !if wind direction keyword not specified, default is to randomnize
                    write(rpt_unit,'(1x,a,t30,a7,1x,a)')'WIND DIRECTION ','DEFAULT','WIND DIRECTIONS ARE RAMDOMNIZED'
                endif
!               substitute NWS
                if (litems(2)) then
                    if (lactions(3)) then
                        write(rpt_unit,'(1x,a,t30,a7,1x,a)')trim(adjustl(items(2))),actions(3),&
                            'NWS WINDS AND TEMPS SUBSTITUED FOR MISSING ONSITE DATA'
                    else
                        write(rpt_unit,'(1x,a,t30,a7,1x,a)')trim(adjustl(items(2))),actions(3),&
                            'NWS WINDS AND TEMPS ARE NOT SUBSTITUED FOR MISSING ONSITE DATA'
                    endif
                endif
!               stable BL options
                if (litems(3)) then
                    if (lactions(4)) then !bulk-richarson number used
                        write(rpt_unit,'(1x,a,t30,a7,1x,a)')'STABLE BOUNDARY LAYER',actions(4),'BULK-RICHARSON METHOD USED'
                        write(rpt_unit,'(t38,a,2(1x,f6.1))')'DELTA-T HEIGHTS (M):',osdt_hts(1,1),osdt_hts(1,2)
                    endif
                    
                    if (lactions(7))write(rpt_unit,'(1x,a,t30,a7,1x,a)')'STABLE BOUNDARY LAYER',actions(4),'ADJUSTED U* USED' !adjust u* used
                endif
!               adjustment of ASOS winds
                if (litems(4)) then
                    if (lactions(5)) then
                        write(rpt_unit,'(1x,a,t30,a7,1x,a)')'AOS WIND ADJUSTMENT',actions(5),&
                            'ASOS WIND ARE NOT ADJUSTED FOR TRUNCATION'
                    else
                        write(rpt_unit,'(1x,a,t30,a7,1x,a)')'AOS WIND ADJUSTMENT','DEFAULT',&
                            'ASOS WIND ARE ADJUSTED FOR TRUNCATION'
                    endif    
                endif
!               cloud cover substitution
                if (subcloud) then
                    write(rpt_unit,'(1x,a,t30,a7,1x,a)')'CLOUD COVER',actions(8),'CLOUD COVER SUBSTITUTED'
                else
                    write(rpt_unit,'(1x,a,t30,a7,1x,a)')'CLOUD COVER',actions(9),'CLOUD COVER NOT SUBSTITUTED'
                endif
!               temperature substitution
                if (subtemp) then
                    write(rpt_unit,'(1x,a,t30,a7,1x,a)')'TEMPERATURE',actions(10),'TEMPERATURE SUBSTITUTED'
                else
                    write(rpt_unit,'(1x,a,t30,a7,1x,a)')'TEMPERATURE',actions(11),'TEMPERATURE NOT SUBSTITUTED'
                endif
!               post 21DRF, only output upper air selections when upper air data read
                if (lpath(2)) then
                    if (litems(5)) then !UASELECT
                        !code modified by GMM
						!original code:-
						!if (lactions(6)) write(rpt_unit,'(1x,a,t30,a7,1x,a)'),'UPPER AIR SELECTION',actions(6),&
						if (lactions(6)) write(rpt_unit,'(1x,a,t30,a7,1x,a)') 'UPPER AIR SELECTION',actions(6),&
                            'SOUNDING SELECTION BASED ON SUNRISE TIME'
                    endif 
                    if (lpblkeywords(9)) write(rpt_unit,'(1x,a,2(i3,1x))')'SOUNDING TIME WINDOW:',snding_win(1),snding_win(2)
                else
                    if (osvars(mix_var)%lread) write(rpt_unit,'(1x,a,t30,a)')'UPPER AIR SELECTION',&
                        'NONE    USE ONSITE MIXING HEIGHTS'
                endif
!               post 21DRF, only output ASOS threshold when processing SURFACE data
                
                if (lpblkeywords(17) .and. lpath(4)) write(rpt_unit,'(1x,a,1x,f4.2)')'ASOS 1-MINUTE THRESHOLD SPEED (M/S):',&
                    asos_thresh
            else !ONSITE or MMIF&
                    
                call sum_files(1,loskeywords(1),os_inpfile) !DATA
                call sum_files(3,loskeywords(4),os_qafile) !QAOUT
    
!               indicate if overland or overwater if prognostic data
                if (lpath(5)) then
                    if (overland) then 
                        write(rpt_unit,'(/1x,a)')'GRID CELL TYPE: OVERLAND'
                    else
                        write(rpt_unit,'(/1x,a)')'GRID CELL TYPE: OVERWATER'
                    endif
                endif
                
!               summarize the options for ONSITE or MMIF data
                
!               number of observations/hour
                write(rpt_unit,'(/1x,a,1x,i2)')'NUMBER OF OBSERVATIONS/HOUR:',nobs_hr  
                  
!               delta temp heights
                if (loskeywords(9)) write(rpt_unit,'(/1x,a,2(1x,f7.2))')'DELTA T HEIGHTS:',osdt_hts(1,1),osdt_hts(1,2)
                  
!               wind speed threshold
                if (loskeywords(11)) write(rpt_unit,'(/1x,a,1x,f6.2)')'THRESHOLD WIND SPEED (M/S): ',threshspeed
!               write tower heights defined by OSHEIGHTS
!               write from highest to lowest
                if (loskeywords(10) .and. allocated(os_heights)) then
                    write(rpt_unit,'(/1x,a/)')'HEIGHTS FOR TOWER DATA (M) BASED ON THE OSHEIGHTS KEYWORD:'
    l4:             do j=noshts,1,-1
                        write(rpt_unit,'(1x,f7.2)')os_heights(j)
                    enddo l4
                endif
                  
            endif
        endif
    enddo l1

    return
    end subroutine input_summ
!*********************************************************************************************************

    subroutine dates_loc(i,startdate,enddate,dates1,statid,lat,lon,gmt2lst,elev)
!=========================================================================================================
!   SUBROUTINE DATES_LOC
!   THIS SUBROUTINE WRITES THE PROCESSING DATES AND STATION LOCATION TO THE REPORT SUMMARY
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE REPORTS (INPUT_SUMM)
!
!   INPUT ARGUMENTS
!
!   I:              PATH ID
!   STARTDATE:      START DATE (YYYYMMDD) OF PERIOD BEING PROCESSED
!   ENDDATE:        END DATE (YYYYMMDD) OF PERIOD BEING PROCESSED     
!   DATES1:         2X3 ARRAY OF START MONTH, DAY, YEAR AND END MONTH,
!                   DAY AND YEAR.  1ST DIM IS THE START (1) OR END (2)
!                   2ND DIM IS YEAR (1), MONTH (2), AND DAY (3)
!   ENDDATE:        END DATE (YYYYMMDD) OF PERIOD BEING PROCESSED      
!   GMT2LST:        CONVERSION FROM GREENWICH MEAN TIME (GMT) TO LOCAL STANDARD TIME (LST)
!   LAT:            LATITUDE OF STATION
!   LON:            LONGITUDE OF STATION
!   ELEV:           STATION ELEVATION
!   STATID:         STATION ID
!
!   Variable definitions
!      
!   Integer variables
!   i:              path id
!   j:              loop counter
!   gmt2lst:        conversion from Greenwich Mean Time (GMT) to local standard time (LST)
!   startdate:      start date (YYYYMMDD) of period being processed
!   enddate:        end date (YYYYMMDD) of period being processed 
!   dates1:         2x3 array of start month, day, year and end month,
!                   day and year.  1st dim is the start (1) or end (2)
!                   2nd dim is year (1), month (2), and day (3)
!   procdates(2):   2-element array of start date (1) and end date (2) (YYYMMDD) of period being processed
!
!   Real variables
!   lat:            latitude of station
!   lon:            longitude of station
!   elev:           station elevation
!                   note for upper air station, elevation not used
!
!   Logical variables
!   valdate:        processing dates are valid; not missing and procdates(1) <= procdates(2)
!                   i.e. start date is before end date
!
!   Character variables
!   statid:         station ID
!   formstr:        formats
!   dashes:         dash text strings for report
!   locstr:         location header strings
!   dates:          character string of the start or end date (YYYYMMDD)
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: r8
    use pbl, only: lpblkeywords
    implicit none
    integer(kind=8), intent(in) :: startdate,enddate
    integer(kind=4), intent(in) :: i,dates1(2,3),gmt2lst
    integer(kind=4) :: j,iloc 
    integer(kind=8) :: procdates(2)
    real(kind=r8), intent(in) :: lat,lon,elev
    logical :: valdate
!      character(len=8):: dates
    character(len=8), intent(in) :: statid
    character(len=60) :: formstr(3)
    character(len=70) :: dashes(2)
    character(len=70) :: locstr(2)
    character(len=10) :: modnam='DATES_LOC'
    
    locstr(1)='SITE ID     LATITUDE    LONGITUDE     TIME ADJUSTMENT'
    locstr(2)='SITE ID     LATITUDE    LONGITUDE     TIME ADJUSTMENT    ELEVATION'
    dashes(1)='--------     --------    ---------     ---------------'
    dashes(2)='--------     --------    ---------     ---------------    ---------'
    
!   1.  processing dates
    write(formstr(1),'(a)')'(/1x,a,1x,2(i2.2,a),i4,1x,a,1x,2(i2.2,a),i4)'
    
!   2. station and location information with no elevation
    write(formstr(2),'(a)')'(/20x,a/19x,a/1x,a,9x,a8,2(6x,f7.2),12x,i2/)'
    
!   3. station and location information with elevation
    write(formstr(3),'(a)')'(/20x,a/19x,a/1x,a,9x,a8,2(6x,f7.2),12x,i2,10x,f9.2/)'	

!   initialize
    j=0
!      imonth=0
!      iday=0
!      iyear=0
    valdate=.false.
      
!   set values for procdates
    procdates(1)=startdate
    procdates(2)=enddate
!     if dates are not missing and the start date <= end date then dates are valid
    if (procdates(1) > 0 .and. procdates(2) > 0 .and. procdates(1) <= procdates(2)) then
        valdate=.true.
    else
        valdate=.false.
    endif
          
!   write the processing dates
    if (valdate) then
        write(rpt_unit,formstr(1))'PROCESSING DATES:',dates1(1,2),'/',dates1(1,3),'/',dates1(1,1),'-',dates1(2,2),'/',&
            dates1(2,3),'/',dates1(2,1)
    else
        write(rpt_unit,'(/1x,a)')'ONE OR MORE DATES ARE INVALID'
    endif
      
!   write locations
!   don't write elevation for upper air station
!   for METPREP, if no LOCATION specified, then just write
!   dates
    if (i == 2 .or. (i == 6 .and. lpblkeywords(2))) then
        write(rpt_unit,formstr(2))trim(adjustl(locstr(1))),trim(adjustl(dashes(1))),'LOCATION:',trim(adjustr(statid)),lat,lon,&
            gmt2lst
    else
        iloc=2
        write(rpt_unit,formstr(3))trim(adjustl(locstr(2))),trim(adjustl(dashes(2))),'LOCATION:',trim(adjustr(statid)),lat,lon,&
            gmt2lst,elev
    endif
      

    return
    end subroutine dates_loc
!*********************************************************************************************************
      
    subroutine sum_files(itype,lwrite,fname)
!=========================================================================================================
!   SUBROUTINE SUM_FILES
!   THIS SUBROUTINE WRITES THE VARIOUS INPUT/OUTPUT FILES AND STATUS TO THE REPORT SUMMARY
!
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      MODULE REPORTS (INPUT_SUMM)
!
!   INPUT ARGUMENTS
!   ITYPE:          INTEGER INDICATOR OF FILE TYPE (DATA, EXTRACT, QAOUT, ASOS1MIN)
!   LWRITE:         LOGICAL VARIABLE DENOTING TO WRITE THE FILENAME
!                   (TRUE=A FILE WAS SPECIFIED IN RUNSTREAM FILE)
!                   (FALSE=NO FILE SPECFIED IN RUNSTREAM FILE)
!   FNAME:          FILENAME TO WRITE
!
!   Variable definitions
!      
!   Integer variables
!   itype:          integer indicator of file type (DATA, EXTRACT, QAOUT, ASOS1MIN)
!   ostatus:        2-element integer array indicating file open status (1=open, 2=not opened)
!                   used to specify which element of array openstatus to write (see below for
!                   description of openstatus
!
!   Logical variables
!   lwrite:         logical variable denoting to write the filename
!                   (true=a file was specified in runstream file)
!                   (false=no file specfied in runstream file)
!   lopened         logical variable denoting open status of
!                   file. used to set ostatus
!
!   Character variables
!   ftypes:         array of file types to which the file belongs
!   openstatus:     2-element array to describe open status of file
!   fname:          filename
!   modnam:         Subroutine name
!=========================================================================================================
    use file_units, only: flength  
    implicit none
    integer(kind=4), intent(in) :: itype
    integer(kind=4) :: ostatus
    logical, intent(in) :: lwrite
    logical :: lopened
    character(len=flength),intent(in) :: fname
    character(len=8) :: ftypes(6),openstatus(2)
    character(len=10) :: modnam='SUM_FILES'
      
    data ftypes /'DATA','EXTRACT','QAOUT','ASOS1MIN','OUTPUT','PROFILE'/
    data openstatus /'OPEN','NOT OPEN'/
 
!   initialize
    lopened=.false.
      
!   if lwrite is true then write out the data file and if it is opened or not
!   if lwrite is false then file was not specified and write message indicating status
    if (lwrite) then
        inquire(file=fname,opened=lopened)
        if (lopened) then
            ostatus=1
        else
            ostatus=2
        endif
        write(rpt_unit,'(2(1x,a),t18,a8,2x,a)')trim(adjustl(ftypes(itype))),'FILE:',adjustl(openstatus(ostatus)),&
            trim(adjustl(fname))
    else
        write(rpt_unit,'(2(1x,a))')trim(adjustl(ftypes(itype))),'FILE: NO FILENAME INPUT'
    endif
      
    return
    end subroutine sum_files
!*********************************************************************************************************
      
    subroutine audit_summ
!=========================================================================================================
!   SUBROUTINE AUDIT_SUMM
!   THIS SUBROUTINE SUMMARIZES AUDIT RESULTS FOR UPPERAIR, SURFACE, ONSITE (OR MMIF)
!   TO THE REPORT SUMMARY
!
!   MODIFIED  APRIL 22, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      AERMET
!
!   Variable definitions
!      
!   Integer variables
!   iv:             audited variable counter
!   iv1:			location of variable in upvars, sfvars or osvars
!   ilayer:         layer number
!   tot_good:       total number of good observations
!   i:              loop counter
!   isplit:         split variable counter
!   nswitch:        indicates to use reports sub-hourly values or just hourly for site-specific data
!   iswitch:        switch between sub-hourly and hourly counter
!   ipath1:         path indicator
!   firstday:       first day of non-missing heights
!   firshr:         first hour of non-missing heights
!
!   Logical variables
!   first_multi:	write first occurence of a level for multi-level 
!   audit_report:   indicates to write audited variables
!   lfound:         logical variable to find firstday and firsthr
!
!   Character variables
!   up_layers:      layer names
!   lt:             less than text string (< or <=)
!   gt:             greater than text string (> or >=)
!   low_str1:       lower bound as text string
!   up_str1:	    upper bound as text string
!   low_str:        concatenation of low_str1 and lt
!   up_str:         concatenation of up_str1 and gt
!   apath:          concatenation of path ID and 'DATA'
!   qa_header:      header string for QA reports
!   formstr:        formats for messages
!   upsum_str:      upper air summary messages
!   sfsum_str:      surface data summary messages
!   ossum_str:      onsite data summary messages
!   asterisks:      string of asterisks for headers
!   modnam:         Subroutine name
!=========================================================================================================
    use main1, only: pathid,lpath,lstage,r8
    use upperair, only: lupkeywords,upvars,up_inc,nup_audit_vars,up_audit_index,up_audit_counts,lbadup,wind_stats,tempstat,&
        nsnd_5k,upstage1,no_sound_days,no_am_days,have_soundings
    use surface, only: sfvars,nsf_audit_vars,sf_audit_index,sf_audit_counts,lowbound,upbound,nsplit,sf_windstats,&
    sf_tempstat,precip_wx,lbadsfc,sfstage1,nsfc,sfformats !post 21DRF add nsfc and sfformats
    use onsite, only: osvars,nos_audit_vars,os_audit_index,os_audit_counts,osdata1,nlevel,os_windstats,os_tempstat,lbados,nosdays,&
        os_info,wind_vars,temp_var,dewpt_var,osstage1,height_var
    implicit none
    integer iv,iv1,ilayer,tot_good,i,isplit,nswitch,iswitch,ipath1,firstday,firsthr
    logical first_multi,audit_report,lfound
    character(len=15) :: up_layers !(10)
    character(len=2) :: lt,gt
    character(len=4) :: vname
    character(len=10) :: low_str1,up_str1
    character(len=20) :: low_str,up_str
    character(len=12) :: apath
    character(len=59) :: qa_header_str='VARIABLE   MISSING VALUE   LOWER BOUND          UPPER BOUND'
    character(len=100) :: formstr(12)
    character(len=75) :: upsum_str(4)
    character(len=75) :: sfsum_str(7) !post 21DRF, increase from 6 to 7 to add variable wind obs
    character(len=75) :: ossum_str(4)
    character(len=29) :: asterisks='****************************'
    character(len=10) :: modnam='AUDIT_SUMM'
    
    data upsum_str /'CALM WIND CONDITIONS (WS=0, WD=0):','ZERO WIND SPEED; NON-ZERO WIND DIRECTION:',&
        'DEW POINT GREATER THAN DRY BULB TEMPERATURE:','NUMBER OF SOUNDINGS THAT DO NOT EXTEND TO 5000 M:'/
    
    data sfsum_str /'CALM WINDS (WDIR = 0, WSPD = 0)','WDIR = 0, WSPD > 0','WDIR > 0, WSPD = 0','WINDS FLAGGED AS VARIABLE',&
        'TEMPERATURE < DEWPOINT','PRECIP WITHOUT WEATHER CODE','WEATHER CODE WITHOUT PRECIP'/
    
    data ossum_str /'CALM WINDS (< THRESHOLD)','NO INPUT WS WITH WD','NO INPUT WD WITH WS','TEMPERATURE < DEWPOINT'/
    
!   formats
!   1.  missing value and bounds 
    write(formstr(1),'(a)')'(1x,a4,6x,f9.2,7x,2(1x,a20))'

!   2.  variable with missing/bound header for upper air or onsite/prognostic data
    write(formstr(2),'(a)')'(/1x,a,t23,a,t38,a1,t49,a1,t58,a,t70,a,t83,a1/t37,a,t46,a,t58,a,t70,a,t80,a)'
    
!   3. audit counts by layer for upper air
    write(formstr(3),'(a)')'(1x,a15,t25,a,4(3x,i8),8x,f6.2)'
    
!   4.  audit counts by layer but no layer name	
    write(formstr(4),'(a)')'(t25,a4,4(3x,i8),8x,f6.2)'
    
!   5.  audit counts for last layer
    write(formstr(5),'(a)')'(/1x,a15,t25,a,4(3x,i8),8x,f6.2)'	

!   6.  summary of winds/temps and soundings < 5 km for upperair
    write(formstr(6),'(a)')'(/1x,a,/4(/1x,a,1x,i8))'
    
!   7.  variable with missing/bound header for upper air
    write(formstr(7),'(a)')'(/t2,a,t17,a1,t28,a1,t37,a,t49,a,t62,a1/t16,a,t25,a,t37,a,t49,a,t59,a)'
    
!   8.  audit counts for surface
    write(formstr(8),'(a)')'(t2,a4,2x,4(3x,i8),8x,f6.2)'
    
!   9.  summary of winds/temps and precip/wx for surface
    write(formstr(9),'(a)')'(2(/t2,a),7(/i7,1x,a))' !post 21DRF added variable winds
    !write(formstr(9),'(a)')'(2(/t2,a),6(/i7,1x,a))' 
    
!   10. onsite/prognostic stats 1st layer
    write(formstr(10),'(a)')'(1x,a10,t25,a4,4(3x,i8),8x,f6.2)'
    
!   11. onsite prognostic stats no layer name
    write(formstr(11),'(a)')'(t25,a4,4(3x,i8),8x,f6.2)'
    
!   12. onsite prognostic stats for layers > 1
    write(formstr(12),'(a)')'(/1x,a10,t25,a4,4(3x,i8),8x,f6.2)'
    
    if (lstage(1)) write(rpt_unit,'(//1x,a29,1x,a,1x,a29)')asterisks,'QA SUMMARY',asterisks
    
!   *********************** UPPERAIR *******************
    if ((lupkeywords(9) .or. lupkeywords(4)) .and. upstage1 .and. .not. lbadup) write(rpt_unit,'(//1x,a)')'UPPERAIR DATA'
    if (lupkeywords(9) .and. upstage1 .and. .not. lbadup .and. have_soundings) then
        write(rpt_unit,'(//1x,a)')qa_header_str
!       write variables being audited and the missing values
    up: do iv=1,nup_audit_vars
            iv1=up_audit_index(iv)
!           set values for boundary check
            if (upvars(iv)%lincbound) then
                lt='<'
                gt='>'
            else
                lt='<='
                gt='>='
            endif
!           convert numerical value to string
            write(low_str1,'(f9.2)')real(upvars(iv1)%lowbound,r8)*upvars(iv1)%conv
            write(low_str,'(a,1x,a)')trim(adjustl(lt)),trim(adjustl(low_str1))
            write(up_str1,'(f9.2)')real(upvars(iv1)%upbound,r8)*upvars(iv1)%conv
            write(up_str,'(a,1x,a)')trim(adjustl(gt)),trim(adjustl(up_str1))
            write(rpt_unit,formstr(1))trim(adjustl(upvars(iv1)%varname)),real(upvars(iv1)%missval,r8),low_str,up_str
    enddo up
        
!       write header for counts
        write(rpt_unit,formstr(2))'LEVEL','VARIABLE','#','#','LOWER','UPPER','%','OBS','MISSING','BOUND','BOUND','ACCEPTED'
        
   layr:do ilayer=1,10
            if (ilayer == 1) then 
                up_layers='SURFACE'
            elseif (ilayer== 10) then
                write(up_layers,'(a,i4,a)')'     > ',8*up_inc,' M'
            else
                write(up_layers,'(i4,a,i4,a)') (ilayer-2)*up_inc,' - ',(ilayer-1)*up_inc,' M'
            endif
    l1:     do iv=1,nup_audit_vars
                iv1=up_audit_index(iv)
                if (ilayer == 1 .and. iv1 >= 7) cycle l1
!               get number of obs that are within bounds
!               initially set to total obs, then subtract # of missing and 
!               # of obs exceeding boundaries
                tot_good=up_audit_counts(iv,ilayer,1)
    l2:         do i=2,4
                    tot_good=tot_good-up_audit_counts(iv,ilayer,i)
                enddo l2
                
                if (iv==1) then !write the layer name
                    if (ilayer == 1) then 
                        write(rpt_unit,formstr(3))trim(adjustl(up_layers)),trim(adjustl(upvars(iv1)%varname)),&
                            (up_audit_counts(iv,ilayer,i),i=1,4),(real(tot_good,r8)/real(up_audit_counts(iv,ilayer,1),r8))*100._r8
                    else !put blank line between last variable of previous level and this level
                        write(rpt_unit,formstr(5))trim(adjustl(up_layers)),trim(adjustl(upvars(iv1)%varname)),&
                            (up_audit_counts(iv,ilayer,i),i=1,4),(real(tot_good,r8)/real(up_audit_counts(iv,ilayer,1),r8))*100._r8
                    endif
                else !don't write layer name
                    write(rpt_unit,formstr(4))trim(adjustl(upvars(iv1)%varname)),&
                        (up_audit_counts(iv,ilayer,i),i=1,4),(real(tot_good,r8)/real(up_audit_counts(iv,ilayer,1),r8))*100._r8
                endif
            enddo l1
            
        enddo layr
    
    endif
!   write wind, temp check, and sounding check summaries
    if (lupkeywords(4) .and. upstage1 .and. .not. lbadup) &
        write(rpt_unit,formstr(6))'NON-QA SUMMARY CHECKS',trim(adjustl(upsum_str(1))),wind_stats(1),trim(adjustl(upsum_str(2))),&
        wind_stats(2),trim(adjustl(upsum_str(3))),tempstat,trim(adjustl(upsum_str(4))),nsnd_5k
    
!   post 21DRF, number of days without soundings or no AM soundings
    if (.not. lbadup .and. upstage1) then
        write(rpt_unit,'(/1x,a,i5)')'NUMBER OF DAYS WITH NO SOUNDINGS:',no_sound_days
        if (no_sound_days > 0) write(rpt_unit,'(1x,a)')'SEE MESSAGE CODE I28 IN MESSAGE FILE FOR DATES'
        write(rpt_unit,'(/1x,a,i5)')'NUMBER OF DAYS WITH SOUNDINGS BUT NO AM SOUNDINGS:',no_am_days
        if (no_am_days > 0) write(rpt_unit,'(1x,a)')'SEE MESSAGE CODE I29 IN MESSAGE FILE FOR DATES'
    endif
        
!   *********************** UPPERAIR *******************    
!   *********************** SURFACE ********************
!   there is always AUDIT for surface (TMPD, WDIR, and WSPD)
    if (lpath(3) .and. sfstage1 .and. .not. lbadsfc) then
        write(rpt_unit,'(2(//1x,a))')'SURFACE DATA',qa_header_str
!       write variables being audited and the missing values
    sf1:do iv=1,nsf_audit_vars
            iv1=sf_audit_index(iv)
!           set values for boundary check
            if (sfvars(iv1)%lincbound) then
                lt='<'
                gt='>'
            else
                lt='<='
                gt='>='
            endif
!           split for concatenated variables
    split1: do isplit=1,nsplit
!               convert numerical value to string
                write(low_str1,'(f9.2)')lowbound(iv,isplit)
                write(low_str,'(a,1x,a)')trim(adjustl(lt)),trim(adjustl(low_str1))
                write(up_str1,'(f9.2)')upbound(iv,isplit)
                write(up_str,'(a,1x,a)')trim(adjustl(gt)),trim(adjustl(up_str1))  
                if (iv1 == 4 .or. iv1 == 5 .or. iv1 == 19 .or. (iv1 >= 13 .and. iv1 <= 18)) then
!                   split the variables into the first 2-characters then last 2-characters
                    if (isplit == 1) then
                        write(rpt_unit,formstr(1))trim(adjustl(sfvars(iv1)%varname(1:2))),real(sfvars(iv1)%missval,r8),&
                            low_str,up_str
                    else
                        write(rpt_unit,formstr(1))trim(adjustl(sfvars(iv1)%varname(3:4))),real(sfvars(iv1)%missval,r8),&
                            low_str,up_str
                    endif
                else
                    if (isplit == 1) write(rpt_unit,formstr(1))trim(adjustl(sfvars(iv1)%varname)),real(sfvars(iv1)%missval,r8),&
                        low_str,up_str
                endif
            
            enddo split1
        
        enddo sf1

!       surface header
        write(rpt_unit,formstr(7))'VARIABLE','#','#','LOWER','UPPER','%','OBS','MISSING','BOUND','BOUND','ACCEPTED'
    
!   now write summaries
    sf2: do iv=1,nsf_audit_vars
            iv1=sf_audit_index(iv)
!           get number of obs that are within bounds
!           initially set to total obs, then subtract # of missing and 
!           # of obs exceeding boundaries
    s1:     do isplit=1,nsplit
                tot_good=sf_audit_counts(iv,isplit,1)
    l3:         do i=2,4
                    tot_good=tot_good-sf_audit_counts(iv,isplit,i)
                enddo l3
                if (iv1 == 4 .or. iv1 == 5 .or. iv1 == 19 .or. (iv1 >= 13 .and. iv1 <= 18)) then
                    if (isplit == 1) then
                        write(rpt_unit,formstr(8))trim(adjustl(sfvars(iv1)%varname(1:2))),(sf_audit_counts(iv,isplit,i),i=1,4),&
                        (real(tot_good,r8)/real(sf_audit_counts(iv,isplit,1),r8))*100._r8
                    else
                        write(rpt_unit,formstr(8))trim(adjustl(sfvars(iv1)%varname(3:4))),(sf_audit_counts(iv,isplit,i),i=1,4),&
                        (real(tot_good,r8)/real(sf_audit_counts(iv,isplit,1),r8))*100._r8
                    endif
                else
                    if (isplit==1)write(rpt_unit,formstr(8))trim(adjustl(sfvars(iv1)%varname)),&
                    (sf_audit_counts(iv,isplit,i),i=1,4),(real(tot_good,r8)/real(sf_audit_counts(iv,isplit,1),r8))*100._r8
                endif
            enddo s1
    enddo sf2
!   write summaries of winds, temperature, and precip checks
!   windstats(1)=calm
!   windstats(2)=direction=0,speed > 0
!   windstats(3)=direction > 0, speed=0
!   tempstat= dewpoint > temperature
!   precip_wx(1)=precip without weather
!   precip_wx(2)=weather without precip

        !write(rpt_unit,formstr(9))'WIND, TEMPERATURE, AND PRECIPITATION CHECKS','NUMBER OF HOURS:',sf_windstats(1),&
        !    trim(adjustl(sfsum_str(1))),sf_windstats(2),trim(adjustl(sfsum_str(2))),sf_windstats(3),&
        !    trim(adjustl(sfsum_str(3))),sf_tempstat,trim(adjustl(sfsum_str(4))),precip_wx(1),&
        !    trim(adjustl(sfsum_str(5))),precip_wx(2),trim(adjustl(sfsum_str(6)))
!       post 21DRF, add variable wind obs
        write(rpt_unit,formstr(9))'WIND, TEMPERATURE, AND PRECIPITATION CHECKS','NUMBER OF HOURS:',sf_windstats(1),&
            trim(adjustl(sfsum_str(1))),sf_windstats(2),trim(adjustl(sfsum_str(2))),sf_windstats(3),&
            trim(adjustl(sfsum_str(3))),nsfc(12),trim(adjustl(sfsum_str(4))),&
            sf_tempstat,trim(adjustl(sfsum_str(5))),precip_wx(1),&
            trim(adjustl(sfsum_str(6))),precip_wx(2),trim(adjustl(sfsum_str(7)))
        
        if (sfformats(7)%lsfform) write(rpt_unit,'(2(/1x,a,1x,i8))')'NUMBER OF DICARDED ISHD RECORDS (see ISHD_discard.txt):',&
            nsfc(6)+nsfc(7)+nsfc(9)+nsfc(10),'NUMBER OF OVERWRITTEN ISHD RECORDS (see ISHD_replace.txt):',nsfc(8)
    endif 
!   *********************** SURFACE ********************
!   *********************** ONSITE OR MMIF ********************
!   see if any variables are being audited.
    audit_report=.false.
    iv=1
    if (osstage1) then
        do while (iv .le. nos_audit_vars .and. .not. audit_report)
            iv1=os_audit_index(iv,2)
            if (osvars(iv1)%laudit) audit_report=.true.
            iv=iv+1
        enddo
    endif
    if ((lpath(4) .or. lpath(5)) .and. osstage1 .and. audit_report .and. .not. lbados) then
        if (lpath(4)) then
            ipath1=4
        else
            ipath1=5
        endif
        write(apath,'(a,1x,a)')trim(adjustl(pathid(ipath1))),'DATA'
        write(rpt_unit,'(2(//1x,a))')trim(adjustl(apath)),qa_header_str
        
!       find first instance of non-missing data for heights
        firstday=1
        firsthr=1
        lfound=.false.
        do while(firstday .le. nosdays .and. .not. lfound)
            firsthr=1
            do while (firsthr .le. 24 .and. .not. lfound)
                if (os_info(firstday)%have_obs(firsthr)) then
                    lfound=.true.
                else
                    firsthr=firsthr+1
                endif
            enddo
            if (.not. lfound)firstday=firstday+1
        enddo
        
!       write variables being audited and the missing values and bounds
    o1: do iv=1,nos_audit_vars
            iv1=os_audit_index(iv,2)
!           set values for boundary check
            if (osvars(iv1)%lincbound) then
                lt='<'
                gt='>'
            else
                lt='<='
                gt='>='
            endif
!           convert numerical value to string
            write(low_str1,'(f9.2)')real(osvars(iv1)%lowbound,r8)*osvars(iv1)%conv
            write(low_str,'(a,1x,a)')trim(adjustl(lt)),trim(adjustl(low_str1))
            write(up_str1,'(f9.2)')real(osvars(iv1)%upbound,r8)*osvars(iv1)%conv
            write(up_str,'(a,1x,a)')trim(adjustl(gt)),trim(adjustl(up_str1))  
            if (iv1 < height_var) then !post 21DRF changed from 26 to ht_var
!               write full variable name for scalars
                vname=osvars(iv1)%varname
            else
!               write first 2 variables for multi-level data
                write(vname,'(a2)')trim(adjustl(osvars(iv1)%varname(1:2)))
            endif
                write(rpt_unit,formstr(1))trim(adjustl(vname)),real(osvars(iv1)%missval,r8),low_str,up_str
        enddo o1
!       now write summaries, scalars first (single level variables) then multi-level variables
!       scalars will have similar format to surface data
!       report counts for sub-hourly data as well if available
        if (osvars(5)%lread) then
            nswitch=2
        else
            nswitch=1
        endif
    m1: do iswitch=nswitch,1,-1
            if (iswitch == 1) then
                write(rpt_unit,'(//a/)')'HOURLY VALUES'
            else
                write(rpt_unit,'(//a/)')'SUB-HOURLY VALUES'
            endif
!           write header for counts
            write(rpt_unit,formstr(2))'LEVEL','VARIABLE','#','#','LOWER','UPPER','%','OBS','MISSING','BOUND','BOUND','ACCEPTED'
    l4:     do ilayer=1,nlevel
!               write level name based on osdata1.  use osdata1 because OSHEIGHTS have
!               already been applied to data so don't have to worry about checking
!               if osheights are being used or not, just use osdata1
!               note up_layers may be reset below if a scalar variable is being read
                
    o2:         do iv=1,nos_audit_vars
                    iv1=os_audit_index(iv,2)
!                   set logical variable to write first occurence of a level
                    if (iv > 1) then
                        if (os_audit_index(iv,2) >= height_var .and. os_audit_index(iv-1,2) < height_var) then
                            first_multi=.true.
                        else
                            first_multi=.false.
                        endif
                    else
                        if (iv1 >=26)first_multi=.true.
                    endif
                    
!                   cycle if above a variables lastlevel or below its first level
                    if ((ilayer > osvars(iv1)%nlevels .and. osvars(iv1)%nlevels > 0) .or. &
                    (ilayer > 1 .and. osvars(iv1)%nlevels == 0) .or. ilayer < osvars(iv1)%firstlev) cycle o2
                    if (iv1 < height_var) then 
!                       write full variable name for scalars
                        vname=osvars(iv1)%varname
                    else
!                       write first 2 variables for multi-level data
                        write(vname,'(a2)')trim(adjustl(osvars(iv1)%varname(1:2)))
                    endif
                   
                    if (ilayer == 1 .and. iv1 < height_var) then
                        up_layers='SCALAR'
                    else
                        write(up_layers,'(f8.3,1x,a1)')osdata1(1,ilayer,firsthr,firstday),'M'
                    endif
                    tot_good=os_audit_counts(iv,ilayer,iswitch,1)
    i1:             do i=2,4
                        tot_good=tot_good-os_audit_counts(iv,ilayer,iswitch,i)
                    enddo i1
                    if (iv==1 .or. first_multi) then !write the layer name
                        if (ilayer == 1) then 
                            write(rpt_unit,formstr(10))trim(adjustl(up_layers)),trim(adjustl(vname)),&
                            (os_audit_counts(iv,ilayer,iswitch,i),i=1,4),&
                            (real(tot_good,r8)/real(os_audit_counts(iv,ilayer,iswitch,1),r8))*100._r8
                        else
                            write(rpt_unit,formstr(12))trim(adjustl(up_layers)),trim(adjustl(vname)),&
                            (os_audit_counts(iv,ilayer,iswitch,i),i=1,4),&
                            (real(tot_good,r8)/real(os_audit_counts(iv,ilayer,iswitch,1),r8))*100._r8
                        endif
                    else !don't write layer name
                        write(rpt_unit,formstr(11))trim(adjustl(vname)),(os_audit_counts(iv,ilayer,iswitch,i),i=1,4),&
                        (real(tot_good,r8)/real(os_audit_counts(iv,ilayer,iswitch,1),r8))*100._r8
                    endif
                enddo o2
            enddo l4
!           wind and temperature stats if applicable
            if ((osvars(wind_vars(1))%lread .and. osvars(wind_vars(2))%lread) .or. (osvars(temp_var)%lread .and. &
                osvars(dewpt_var)%lread)) &
                write(rpt_unit,'(2(/t2,a))')'WIND AND TEMPERATURE CHECKS (IF APPLICABLE)','NUMBER OF OBS:'
            
            if (osvars(wind_vars(1))%lread .and. osvars(wind_vars(2))%lread) &
                write(rpt_unit,'(3(/i7,1x,a))')os_windstats(iswitch,1),trim(adjustl(ossum_str(1))),os_windstats(iswitch,2),&
                trim(adjustl(ossum_str(3))),os_windstats(iswitch,3),trim(adjustl(ossum_str(3)))
            
            if (osvars(temp_var)%lread .and. osvars(dewpt_var)%lread) write(rpt_unit,'(/i7,1x,a)') os_tempstat(iswitch),&
                trim(adjustl(ossum_str(4)))  
            
        enddo m1
    endif
    
!   *********************** ONSITE OR MMIF ********************

    return
    end subroutine audit_summ
!*********************************************************************************************************
      
    subroutine sfchar_sum
!=========================================================================================================
!   SUBROUTINE SFCHAR_SUM
!   THIS SUBROUTINE SUMMARIZES THE SURFACE CHARACTERISTICS
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
!   nsite:          number of surface characteristics sites
!   iyr:            year loop
!   iyr1:           year relative to start year
!   ifreq:          frequency counter
!   j:              counter for seasonal characteristics
!   isec:           sector counter
!   month2seas:     month to season default assignments. 2x3x4 array where first dimension is the
!                   hemisphere (1=north,2=south) and second dimension is months, and 3rd dimension is season  
!
!   Logical variables
!   writeseasons:   logical variable denoting that seasonal surface characteristics used and AERMET should
!                   write month-season assignments
!
!   Character variables
!   asterisks:      string of asterisks for headers
!   formstr:        formats for messages
!   freqname:       frequency name for surface characteristics
!   freqname1:      frequency name for header
!   months:         month names
!   seasons:        season names
!   atemp:          month or season name when writing to report file
!   modnam:         Subroutine name
!=========================================================================================================
    use pbl, only: pbldates,sfc_char1,sfc_char2,sites,sectors1,sectors2,nsectors1,nfreq1,nsectors2,nfreq2,sec_sc,ihem
    implicit none
    integer nsite,iyr,iyr1,ifreq,isec,j,month2seas(2,3,4)
    logical :: writeseason(2)
    character(len=60) :: formstr(3)
    character(len=8) :: hemisphere(2)
    character(len=10) :: modnam='SFCHAR_SUM'
    character(len=10) :: freqname,freqname1
    character(len=29) :: asterisks='****************************'
    character(len=10) :: months(12),seasons(4),atemp
    
    data months /'JANUARY','FEBRUARY','MARCH','APRIL','MAY','JUNE','JULY','AUGUST','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER'/
    data seasons /'WINTER','SPRING','SUMMER','AUTUMN'/
    data hemisphere /'NORTHERN','SOUTHERN'/
!   assign default seasons to months
!   northern hemisphere
!   3rd dimension
!   1=winter (January, February, December)
!   2=spring (March, April, May)
!   3=summer (June, July, August)
!   4=autumn (September, October, November)

    data (month2seas(1,ifreq,1),ifreq=1,3)/1,2,12/
    data (month2seas(1,ifreq,2),ifreq=1,3)/3,4,5/
    data (month2seas(1,ifreq,3),ifreq=1,3)/6,7,8/
    data (month2seas(1,ifreq,4),ifreq=1,3)/9,10,11/
    
    
!   southern hemisphere
!   3rd dimension
!   1=winter (June, July, August)
!   2=spring (September, October, November)
!   3=summer (January, February, December)
!   4=autumn (June, July, August)
    data (month2seas(2,ifreq,1),ifreq=1,3)/6,7,8/
    data (month2seas(2,ifreq,2),ifreq=1,3)/9,10,11/
    data (month2seas(2,ifreq,3),ifreq=1,3)/1,2,12/
    data (month2seas(2,ifreq,4),ifreq=1,3)/3,4,5/
    
!   1.  year, frequency and # of sectors
    write(formstr(1),'(a)')'(/a,1x,i4,2(1x,a)/t12,a,1x,i3)'
    
!   2.  header
    write(formstr(2),'(a)')'(/1x,a4,t9,a,t22,a,t38,a,t53,a,t65,a,t80,a)'
    
!   3.  surface characteristics
    write(formstr(3),'(a)')'(1x,i4,t9,a10,t22,f4.0,t38,f4.0,t53,f4.2,t65,f5.2,t80,f6.4)'
    
    if (sec_sc) then
        nsite=2
    else
        nsite=1
    endif
    
    write(rpt_unit,'(//1x,a29,1x,a,1x,a29)')asterisks,'SFC CHARACTERISTICS SUMMARY',asterisks
        
!   check to see if any of the surface characteristics are seasonal. if so, then list the months
!   associated with the season based on the hemisphere

    y1: do iyr=pbldates(1,1),pbldates(2,1)
            iyr1=iyr-pbldates(1,1)+1
            if (nfreq1(iyr1) == 4) writeseason(1)=.true.
            if (sec_sc) then
                if (nfreq2(iyr1) == 4) writeseason(2)=.true.
            endif
    enddo y1
!   write frequency and # of sectors
    write(rpt_unit,'(//2(1x,a))')trim(adjustl(sites(1))),'SITE CHARACTERISTICS'
    
!   write season-month assignments if seasonal; primary site
    if (writeseason(1)) then
        write(rpt_unit,'(/3(1x,a)//,1x,a)')'MONTH-SEASON ASSIGNMENTS:',trim(adjustl(hemisphere(ihem(1)))),'HEMISPHERE',&
                'SEASON     MONTHS'  
    seas1:  do ifreq=1,4
            write(rpt_unit,'(1x,a,t13,2(a,1x),a)')trim(adjustl(seasons(ifreq))),(months(month2seas(ihem(1),j,ifreq)),j=1,3)
        enddo seas1
    endif
    y2: do iyr=pbldates(1,1),pbldates(2,1)
            iyr1=iyr-pbldates(1,1)+1
            if (nfreq1(iyr1) == 1) then
                freqname='ANNUAL'
                freqname1=''
            elseif (nfreq1(iyr1) == 4) then
                freqname='SEASONAL'
                freqname1='SEASON'
            else
                freqname='MONTHLY'
                freqname1='MONTH'
            endif 
            write(rpt_unit,formstr(1))'YEAR:',iyr,'FREQUENCY',trim(adjustl(freqname)),'NUMBER OF SECTORS:',nsectors1(iyr1)
    enddo y2
    
!   write header
   
    write(rpt_unit,formstr(2))'YEAR',trim(adjustl(freqname1)),'BEGIN SECTOR','END SECTOR','ALBEDO','BOWEN RATIO',&
        'ROUGHNESS LENGTH (M)'
    
!   write data
    y3: do iyr=pbldates(1,1),pbldates(2,1)
        iyr1=iyr-pbldates(1,1)+1
!       primary site
        if (nfreq1(iyr1) == 1) then
            freqname='ANNUAL'
            freqname1=''
        elseif (nfreq1(iyr1) == 4) then
            freqname='SEASONAL'
            freqname1='SEASON'
        else
            freqname='MONTHLY'
            freqname1='MONTH'
        endif        
        
    m1: do ifreq=1,nfreq1(iyr1)
            if (nfreq1(iyr1) ==1) then
                atemp=''
            elseif (nfreq1(iyr1)==4) then
                atemp=seasons(ifreq)
            else
                atemp=months(ifreq)
            endif
    s1:     do isec=1,nsectors1(iyr1)
                write(rpt_unit,formstr(3))iyr,adjustl(atemp),sectors1(1,isec,iyr1),sectors1(2,isec,iyr1),&
                    sfc_char1(isec,ifreq,iyr1)%albedo,sfc_char1(isec,ifreq,iyr1)%bowen,sfc_char1(isec,ifreq,iyr1)%zo
            enddo s1
        enddo m1
    enddo y3
    
!   secondary site
    if (sec_sc) then   
!       write frequency and # of sectors
        write(rpt_unit,'(//2(1x,a))')trim(adjustl(sites(2))),'SITE CHARACTERISTICS'
       
!       write season-month assignments if seasonal; secondary site
        if (writeseason(2)) then
            write(rpt_unit,'(/3(1x,a)//,1x,a)')'MONTH-SEASON ASSIGNMENTS:',trim(adjustl(hemisphere(ihem(2)))),'HEMISPHERE',&
                'SEASON     MONTHS'  
    seas2:  do ifreq=1,4
                write(rpt_unit,'(1x,a,t13,2(a,1x),a)')trim(adjustl(seasons(ifreq))),(months(month2seas(ihem(2),j,ifreq)),j=1,3)
            enddo seas2
        endif
    y4: do iyr=pbldates(1,1),pbldates(2,1)
            iyr1=iyr-pbldates(1,1)+1
            if (nfreq2(iyr1) == 1) then
                freqname='ANNUAL'
                freqname1=''
            elseif (nfreq2(iyr1) == 4) then
                freqname='SEASONAL'
                freqname1='SEASON'
            else
                freqname='MONTHLY'
                freqname1='MONTH'
            endif 
            
            write(rpt_unit,formstr(1))'YEAR:',iyr,'FREQUENCY:',trim(adjustl(freqname)),'NUMBER OF SECTORS:',nsectors2(iyr1)
        enddo y4
!       write header
    
        write(rpt_unit,formstr(2))'YEAR',trim(adjustl(freqname1)),'BEGIN SECTOR','END SECTOR','ALBEDO','BOWEN RATIO',&
            'ROUGHNESS LENGTH (M)'
    
!       write data
        y5: do iyr=pbldates(1,1),pbldates(2,1)
            iyr1=iyr-pbldates(1,1)+1
            if (nfreq2(iyr1) == 1) then
                freqname='ANNUAL'
                freqname1=''
            elseif (nfreq2(iyr1) == 4) then
                freqname='SEASONAL'
                freqname1='SEASON'
            else
                freqname='MONTHLY'
                freqname1='MONTH'
            endif
        m2: do ifreq=1,nfreq2(iyr1)
                if (nfreq2(iyr1) ==1) then
                    atemp=''
                elseif (nfreq2(iyr1)==4) then
                    atemp=seasons(ifreq)
                else
                    atemp=months(ifreq)
                endif
        s2:     do isec=1,nsectors2(iyr1)
                    write(rpt_unit,formstr(3))iyr,adjustl(atemp),sectors2(1,isec,iyr1),sectors2(2,isec,iyr1),&
                        sfc_char2(isec,ifreq,iyr1)%albedo,sfc_char2(isec,ifreq,iyr1)%bowen,sfc_char2(isec,ifreq,iyr1)%zo 
                enddo s2
            enddo m2
    
        enddo y5
    endif
    
    return
    end subroutine sfchar_sum
!*********************************************************************************************************

    subroutine obs_sum
!=========================================================================================================
!   SUBROUTINE OBS_SUM
!   THIS SUBROUTINE SUMMARIZES THE NUMBER OF OBSERVATIONS PER DAY FOR
!   UPPER AIR SOUNDINGS, SURFACE DATA, 1-MINUTE ASOS WINDS AND ONSITE OR PROG
!   COVERED BY THE RANGE OF DATES SPECIFIED BY METPREP.
!
!   MODIFIED MAY 20, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      AERMET
!
!   Variable definitions
!      
!   Integer variables
  
!
!   Logical variables

!
!   Character variables

!   modnam:         Subroutine name
!=========================================================================================================		
    use pbl, only: pbldates,npbldays,pbl_obs,sfc_data
    use main1, only: lpath
    integer(kind=4) :: nlines,iline,d,o,d1,d2,yr1,yr2,ndays,totals(4)
    
    character(len=5), allocatable, dimension(:) :: dates
    character(len=19)  :: yrstr='                   '
    character(len=29) :: asterisks='****************************'
    character(len=3) :: months(12)
    character(len=8) :: adate
    character(len=18) :: obs_lines(4)
    character(len=30) :: formstr
    character(len=10) :: modnam='OBS_SUM'
    
    data months /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
    
    
    obs_lines(1)='NWS UPPER AIR  OBS'
    obs_lines(2)='NWS SURFACE    OBS'
    if (lpath(4)) obs_lines(3)='ONSITE         OBS'
    if (lpath(5)) obs_lines(3)='PROG           OBS' 
    if (.not. lpath(4) .and. .not. lpath(5)) obs_lines(3)='ONSITE/PROG    OBS'
    obs_lines(4)='1-MIN ASOS HR  OBS'
        
!   number of lines that will be written based on 10 day blocks
!   set to 1 at first, at least 1 line
    if (npbldays < 10) then
        nlines=1
    else
        if (mod(npbldays,10) == 0) then
            nlines=npbldays/10
        else
            nlines=(npbldays/10)+1
        endif
    endif
    
!   write header
    write(rpt_unit,'(//1x,a29,1x,a,1x,a29)')asterisks,'OBSERVATIONS PER DAY SUMMARY',asterisks
    write(rpt_unit,'(//1x,a,1x,i2.2,a1,a3,a1,i4)')'STARTING:',pbldates(1,3),'-',months(pbldates(1,2)),'-',pbldates(1,1)
    write(rpt_unit,'(1x,a,1x,i2.2,a1,a3,a1,i4)')'ENDING:',pbldates(2,3),'-',months(pbldates(2,2)),'-',pbldates(2,1)
    
!   total number of observations for each type
    totals=0
    dd: do d=1,npbldays
    o1: do o=1,4
            totals(o)=totals(o)+pbl_obs(o,d)
        enddo o1
    enddo dd
    write(rpt_unit,'(/t20,a/)')'***** TOTAL OBSERVATION COUNTS *****'
    o2: do o=1,4
        write(rpt_unit,'(1x,a18,1x,i6)')obs_lines(o),totals(o)
    enddo o2
    
    write(rpt_unit,'(/t20,a)')'***** DAILY OUTPUT STATISTICS *****'
    
    d1=0
    d2=0
    i1: do iline=1,nlines
        yrstr='                   '
        d1=d2+1
        if (iline < nlines .and. nlines > 1) then
!           more than 10 days
            d2=d2+10
        else
!           more than 10 and last line of data or < 10 days total
            d2=npbldays
        endif
        ndays=d2-d1+1 !number of days
        allocate(dates(d1:d2))
    l1: do d=d1,d2
            write(adate,'(i8.8)')sfc_data(d)%sfcdate
            write(dates(d),'(a2,a1,a2)')adate(5:6),'/',adate(7:8)
            if (d == d1) read(adate(1:4),'(i4)')yr1
            if (d == d2) read(adate(1:4),'(i4)')yr2
        enddo l1			
!		year string
        if (yr2-yr1 == 0) then
!           only 1 year
            write(yrstr(9:19),'(i4,1x,a6)')yr1,'MO/DY:'
        else
            write(yrstr(6:19),'(i4,a1,i2.2,1x,a6)')yr1,'-',yr2-(yr2/100)*100,'MO/DY:'
        endif
        write(formstr,'(a,i6,a)')'(/a19,',ndays,'(3x,a5))'
        write(rpt_unit,formstr)yrstr,(dates(d),d=d1,d2)
        if (ndays .eq. 1) then
            write(formstr,'(a,i1,a)')'(2x,a18,3x,i2,(6x,i2))'
        else
            write(formstr,'(a,i1,a)')'(2x,a18,3x,i2,',ndays-1,'(6x,i2))'
        endif
    o3: do o=1,4
            write(rpt_unit,formstr)obs_lines(o),(pbl_obs(o,d),d=d1,d2)
        enddo o3
        deallocate(dates)
        
    enddo i1
    !endif
    
    
        
    return
    end subroutine obs_sum
!*********************************************************************************************************
    subroutine pbl_sum
!=========================================================================================================
!   SUBROUTINE PBL_SUM
!   THIS SUBROUTINE SUMMARIZES THE PBL PROCESSING
!
!   MODIFIED APRIL 22, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:      AERMET
!
!   Variable definitions
!      
!   Integer variables
!   nsite:          number of surface characteristics sites
!   iyr:            year loop
!   iyr1:           year relative to start year
!   ifreq:          frequency counter
!   j:              counter for seasonal characteristics
!   isec:           sector counter
!   month2seas:     month to season default assignments. 2x3x4 array where first dimension is the
!                   hemisphere (1=north,2=south) and second dimension is months, and 3rd dimension is season  
!
!   Logical variables
!   writeseasons:   logical variable denoting that seasonal surface characteristics used and AERMET should
!                   write month-season assignments
!
!   Character variables
!   asterisks:      string of asterisks for headers
!   modnam:         Subroutine name
!=========================================================================================================	
    use main1,only: lpath
    use pbl, only: pbl_windstats,lpblkeywords,n_cloud_sub,n_temp_sub,subtemp,subcloud,n_conv_miss
    use surface, only: asos_thresh,lasos,lsfkeywords
    use onsite, only: osvars,mix_var
    
    character(len=29) :: asterisks='****************************'
    character(len=10) :: modnam='PBL_SUM'
    
    write(rpt_unit,'(//1x,a29,1x,a,1x,a29)')asterisks,'PBL PROCESSING SUMMARY',asterisks
    
!   if soundings used and mixing heights not read in, report number of days with no convective conditions
    if (lpath(2) .and. .not. osvars(mix_var)%lread) write(rpt_unit,'(/1x,a,1x,i6)')'NUMBER OF DAYS WITH NO CONVECTIVE CONDITIONS:',&
        n_conv_miss
!   number of calms
    write(rpt_unit,'(/1x,a,1x,i6)')'NUMBER OF TOTAL CALMS:',pbl_windstats(1)
    
!   if reading SURFACE data, report 1-minute calms and variable winds if applicable
    if (lpath(3)) then
!       calms due to ASOS threshold, if ASOS site, 1-minute data read in and the threshold is set.
        if (lasos .and. lsfkeywords(9) .and. lpblkeywords(17)) write(rpt_unit,'(1x,a,1x,f3.1,1x,a5,1x,i6/1x,a)')&
            'NUMBER OF 1-MINUTE CALMS: (<',asos_thresh,'M/S):',pbl_windstats(2),&
            'THESE HOURS ARE A SUBSET OF THE TOTAL CALMS'
!       variable winds
        write(rpt_unit,'(1x,a,1x,i6//)')'NUMBER OF VARIABLE WINDS:',pbl_windstats(3)
    endif
    
!   cloud substitution (interpolation)
    if (subcloud) then
        write(rpt_unit,'(/1x,a,1x,i6)')'NUMBER OF SUBSTITUTIONS (INTERPOLATION) FOR MISSING CLOUD COVER:',n_cloud_sub
    endif
    
!   cloud substitution (interpolation)
    if (subtemp) then
        write(rpt_unit,'(/1x,a,1x,i6)')'NUMBER OF SUBSTITUTIONS (INTERPOLATION) FOR MISSING TEMPERATURE:',n_temp_sub
    endif
    
    return
    end subroutine pbl_sum
!*********************************************************************************************************    
    subroutine write_msg
!=========================================================================================================
!   SUBROUTINE WRITE_MSG
!   THIS SUBROUTINE WRITES THE SUMMARY VARIOUS MESSAGES FROM THE MESSAGE FILE TO THE REPORT SUMMARY
!   IF NO MESSAGE FILE EXISTS, SUBROUTINE INDICATES THAT IN REPORT SUMMARY
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
!   itype:			message type and loop counter
!   icode:          message code indicator and used for code loop
!   eof:            integer end-of-file indicator
!   nmessages:      4-element array of count of message types (1=ERROR messages, 2=WARNING messages,
!                   3=INFORMATION MESSAGES,4=QA)
!   nmessages:      100 x 4-element array of count of message types 
!                   1st dimension is code, 01, 02, etc and 2nd dimension is message type.
!                   (1=ERROR messages, 2=WARNING messages,
!                   3=INFORMATION MESSAGES,4=QA)
!   msglim:         maximum number of message codes for each type
!
!   Logical variables
!   lmessage:       logical variable denoting that message file is opened
!                   and available to read from
!
!   Character variables
!   msgtype:        3-element array of message types (1=ERROR messages, 2=WARNING messages,
!                   3=INFORMATION MESSAGES,4=QA)
!   mtype:          3-element array of 1-character variable to search for in message file
!                   to indicate message type (1=E for ERROR, 2=W for WARNING, 3=I for
!                   INFORMATION, and 4=Q fro QA)
!   str:            1-character string read from line to determine which element of 
!                   nmessage to increment
!   asterisks:      string of asterisks for headers
!   modnam:         Subroutine name
!=========================================================================================================
!     nmessages:          Number of error (1), warning (2), information (3), and QA (4) messages
    use file_units, only: msg_unit,msgfile
    implicit none
    integer(kind=4) :: eof,icode,itype,nmessages(4)
    integer(kind=4), parameter :: msglim=100
    integer(kind=4) :: nmessages1(msglim,4)
    logical :: lmessage
    character(len=11) :: msgtype(4)
    character(len=1) :: mtype(4),str
    character(len=29) :: asterisks='****************************'
    character(len=10) :: modnam='WRITE_MSG'
    
    data msgtype /'ERROR','WARNING','INFORMATION','QA'/
    data mtype /'E','W','I','Q'/
      
!   initialize
    itype=0
    eof=0
    nmessages=0
    nmessages1=0
    lmessage=.false.
    
    write(rpt_unit,'(//1x,a29,1x,a,1x,a29/)')asterisks,'MESSAGE SUMMARY',asterisks  

      
    inquire(file=msgfile,opened=lmessage)
      
!   if message file is opened, get # of messages for each
!    type and write out the messages to the REPORT summary
    if (lmessage) then
!   get number of messages
        rewind(msg_unit)
        eof=0
        do while (eof == 0)
            read(msg_unit,'(12x,a1,i2)',iostat=eof)str,icode
            if (eof == 0) then
                if (str == 'E') then !error
                    itype=1
                elseif (str == 'W') then !warning
                    itype=2
                elseif (str == 'I') then !information
                    itype=3
                else !QA
                    itype=4
                endif
                nmessages(itype)=nmessages(itype)+1
                nmessages1(icode,itype)=nmessages1(icode,itype)+1
            endif
        enddo
!       now write the messages for each type
!       temporarily delete; do not write messages to report file
    l1:	do itype=1,4
            write(rpt_unit,'(//2(1x,a),1x,i8,1x,a/)')trim(adjustl(msgtype(itype))),'MESSAGES',nmessages(itype),'MESSAGES'
!			summary of individual codes
    l2:     do icode=1,msglim
                if (nmessages1(icode,itype) > 0) write(rpt_unit,'(1x,a1,i2.2,a1,1x,i8)')mtype(itype),icode,':',&
                    nmessages1(icode,itype)
            enddo l2
        enddo l1    
    else
        write(rpt_unit,'(//1x,a//)')'NO MESSAGE FILE TO READ'
    endif
      
    return
    end subroutine write_msg
!*********************************************************************************************************
    end module reports