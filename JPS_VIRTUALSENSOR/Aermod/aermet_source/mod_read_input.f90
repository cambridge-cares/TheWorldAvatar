    module read_input
!=========================================================================================================
!   MODULE READ_INPUT
!   THIS MODULE CONTAINS VARIABLES AND SUBROUTINES NEEDED TO READ
!   THE AERMET INPUT RUNSTREAM FILE
!
!   MODIFIED JANUARY 24, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   USED BY:        AERMET
!
!   Integer variables
!   linenum:            line number of runstream file being read
!                       Note, this includes blank lines
!   njbkeys:            number of JOB keywords
!                     
!   Logical variables
!   ljbkeywords:        logical variable denoting keywords for JOB pathway found
!   1:                  logical variable denoting that an instance of the 
!                       REPORT keyword (ikey=1) has been found.  This is initially
!                       set to FALSE and when REPORT has been found for the
!                       first time, it is set to TRUE and remains TRUE.
!                       If REPORT is detected after lreport is TRUE, then
!                       an error issued.
!   2:                  logical variable denoting that an instance of the 
!                       MESSAGE keyword (ikey=2) has been found.  This is initially
!                       set to FALSE and when MESSAGE has been found for the
!                       first time, it is set to TRUE and remains TRUE.
!                       If MESSAGE is detected after lmsg is TRUE, then
!                       an error issued.
!   3:                  logical variable denoting that an instance of the 
!                       CHK_SYNTAX keyword (ikey=3) has been found.  This is initially
!                       set to FALSE and when CHK_SYNTAX has been found for the
!                       first time, it is set to TRUE and remains TRUE.
!                       If CHK_SYNTAX is detected after lsyntax is TRUE, then
!                       an error issued.
!   lbad1:              logical variable denoting that at least one line of the
!                       runstream input file is bad.
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit 
!   these variables used in all subroutines
    use main1, only: ikey,ipath,ilen,writeunit,inpline1,pathid,keywrd,lbad
   
    implicit none
    integer(kind=4) :: linenum=0
    integer(kind=4), parameter :: njbkeys=5 
    logical :: ljbkeywords(njbkeys)=.false.
    logical :: lbad1=.false.
      
!   list of subroutines in module
    contains

    subroutine readinp
!=========================================================================================================
!   SUBROUTINE READINP
!   THIS IS THE CONTROLLING SUBROUTINE TO READ THE AERMET RUNSTREAM
!   INPUT FILE
!     
!   MODIFIED DECEMBER 3, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   CALLED BY:  AERMET
!
!   Variable definitions
!      
!   Integer variables
!   istatus:    variable denoting the status of finding an input file 
!               from the command line
!               if istatus=-1, then no file found, assume input file
!               is aermet.inp
!               otherwise, the filename is on the command line
!   eof:        integer end of file indicator to read runstream input file
!   i:          value of index function used throughout subroutine
!   i1:         loop counter
!
!   Logical variables
!   lexist:     logical variable denoting that input runstream file exists
!     
!   Character variables
!   lower:      character string of lowercase letters
!   upper:      character string of uppercase letters
!   fname:      input runstream filename read from command line (if any)
!   form1:      format string to read line from runstream file
!   modnam:     Subroutine name
!=========================================================================================================
    use main1, only: npath,linelen,istage,inpline,lpath,lstage,msg_form
    use file_units, only: inp_unit,inpfile,flength,msg_unit,rpt_unit
    use upperair, only: upper_path,upstage1,up_test,up_stage2
    use surface, only: surf_path,sfstage1,sf_test,sf_stage2
    use onsite, only: os_path,osstage1,os_test,os_stage2!,osvars
    use pbl, only: pbl_path,pbl_test
       
    implicit none
    integer(kind=4) ::  istatus,eof,i,i1 !,istage
    logical :: lexist=.false.
    character(len=26) :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=26) :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=flength) :: fname
    character(len=6) :: form1
    character(len=60) :: formstr
    character(len=10) :: modnam='READINP'
     
!   message formats
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   initialize
    istatus=0
    eof=0
    i=0
    i1=0
    lexist=.false.
      
!   check to see if the input file is on the command line
    call get_command_argument(1,fname,istatus)
      
!   use default filename, aermet.inp, if no argument given (istatus=-1)
!   if istatus not equal to 0 assign inpfile, otherwise use default name

    !if (istatus /= -1) inpfile=fname
    if (istatus /= 0) inpfile=fname
       
!   check for file existence
    inquire(file=inpfile,exist=lexist)
    if (.not. lexist) then
        write(*,'(3(1x,a))')'Input file',trim(adjustl(inpfile)),'not found'
        stop
    endif
       
!   open input file and read for correct syntax, stage(s) being processed
    open(unit=inp_unit,file=inpfile,status='old')

    eof=0
    linenum=0          
    write(form1,'(a2,i3,a1)')'(a',linelen,')'
      
!   only check for paths to determine stages since that affects file type assignments
    do while (eof == 0)
        read(inp_unit,form1,iostat=eof)inpline
        if (eof == 0 .and. inpline(1:2) /= '**') then
            ilen=len_trim(inpline)
            inpline1=inpline !get upper case, keep original line for filenames
    l1:     do i1=1,ilen
                i=index(lower,inpline1(i1:i1))
                if (i > 0) inpline1(i1:i1)=upper(i:i)
            enddo l1
        endif
    enddo
    
    rewind(inp_unit)
    eof=0
    do while (eof == 0)
        read(inp_unit,form1,iostat=eof)inpline
        if (eof ==0) then
            linenum=linenum+1
            ilen=len_trim(inpline)
            lbad=.false.
            ikey=0
            if (ilen > 0 .and. inpline(1:2) /= '**') then   !non-comment line
                inpline1=inpline !get upper case, keep original line for filenames
    l2:         do i1=1,ilen
                    i=index(lower,inpline1(i1:i1))
                    if (i > 0) inpline1(i1:i1)=upper(i:i)
                enddo l2
                call check_line  !line has been checked, no need to reset lbad1, check line is being used to get path and keyword
                if (.not. lbad) then
!                   will ignore MERGE pathway
                    if (ipath == 1) then !JOB pathway
                        call job_path
                        if (.not. lbad1 .and. lbad)lbad1=.true.
                    elseif (ipath == 2) then !UPPERAIR pathway
                        call upper_path
                        if (.not. lbad1 .and. lbad)lbad1=.true.
                    elseif (ipath == 3) then !SURFACE
                        call surf_path
                        if (.not. lbad1 .and. lbad)lbad1=.true.
                    elseif (ipath == 4 .or. ipath == 5) then !ONSITE or MMIF
                            call os_path
                            if (.not. lbad1 .and. lbad)lbad1=.true.
                    !else !PBL
                    elseif (ipath == 6) then !PBL
                       call pbl_path
                       if (.not. lbad1 .and. lbad)lbad1=.true.
                    endif
                else
                    if (.not. lbad1)lbad1=.true.
                endif
            endif
        endif
    enddo
    close(inp_unit) 
    if (lbad1) return
 
!   if lstage(1) is true then the following are permitted with METPREP:
!   UPPERAIR
!   SURFACE
!   ONSITE
!   MMIF
!   UPPERAIR, SURFACE, ONSITE
!   UPPERAIR, SURFACE
!   UPPERAIR, ONSITE
!   UPPERAIR, MMIF

!   if lstage(2) is true, regardless of lstage(1) the following are permitted
!   UPPERAIR, SURFACE, ONSITE, METPREP
!   UPPERAIR, SURFACE, METPREP
!   UPPERAIR, ONSITE, METPREP
!   ONSITE, METPREP (IF ONSITE MIXING HEIGHTS AVAILABLE)
!   MMIF, METPREP (IF ONSITE MIXING HEIGHTS AVAILABLE)

    if (lstage(1) .or. lstage(2)) then
        if (lpath(5) .and. (lpath(3) .or. lpath(4))) then
            if (lstage(1) .and. lstage(2)) then
                write(writeunit,'(12x,a3,5x,a10,6(1x,a))')'E07',modnam,'INVALID PATHS CHOSEN FOR STAGES 1 AND 2;',&
                    trim(adjustl(pathid(5))),'WITH',trim(adjustl(pathid(3))),'OR',trim(adjustl(pathid(4)))
            else
                if (lstage(1)) then
                    istage=1
                else
                    istage=2
                endif
                write(writeunit,'(12x,a3,5x,a10,1x,a,1x,i2,5(1x,a))')'E07',modnam,'INVALID PATHS CHOSEN FOR STAGE',istage,&
                    trim(adjustl(pathid(5))),'WITH',trim(adjustl(pathid(3))),'OR',trim(adjustl(pathid(4)))
            endif
            lbad1=.true.
        elseif (.not. lpath(2) .and. .not. lpath(3) .and. .not. lpath(4) .and. .not. lpath(5)) then
            if (lstage(1) .and. lstage(2)) then
                write(writeunit,'(12x,a3,5x,a10,1x,a)')'E07',modnam,'NO PATHS LISTED FOR STAGES 1 AND 2'
            else
                if (lstage(1)) then
                    istage=1
                else
                    istage=2
                endif
                write(writeunit,'(12x,a3,5x,a10,1x,a,1x,i1)')'E07',modnam,istage
            endif
            lbad1=.true.
        else
            if (lstage(1) .and. lstage(2)) then
                write(writeunit,'(12x,a3,5x,a10,1x,a)')'I01',modnam,'PROCESSING STAGES 1 AND 2'
            else
                if (lstage(1)) then
                    istage=1
                else
                    istage=2
                endif
                write(writeunit,'(12x,a3,5x,a10,1x,a,1x,i1)')'I01',modnam,'PROCESSING STAGE',istage
            endif
        endif
    endif
       
!   if JOB path not found or REPORT and MESSAGE not found, file is bad 
!   if JOB path found but none of the others are found, file is bad
!   or if MMIF and ONSITE or MMIF and SURFACE are present then abort

!   set REPORT file unit to the standard output if REPORT keyword not found
!   note, if REPORT was found and the filename was invalid, rpt_unit
!   was reset in job_path
    if (.not. ljbkeywords(1)) rpt_unit=output_unit
    if (.not. ljbkeywords(2)) msg_unit=output_unit
!   if JOB pathway missing, then issue error
!   otherwise check to make sure MESSAGE keyword was listed
    if (.not. lpath(1)) then 
        write(msg_unit,'(12x,a3,5x,a10,2(1x,a))')'E06',modnam,trim(adjustl(pathid(1))),'PATHWAY IS MISSING'
        lbad1=.true.
    else
        if (.not. ljbkeywords(2)) then
            write(msg_unit,formstr)adjustl(pathid(ipath)),'E08',modnam,'MISSING MANDATORY KEYWORD',trim(adjustl(keywrd(2)))
            lbad1=.true.
        endif
    endif
      

    if (.not. lbad1) then
!       check to see all other paths are missing, regardless of JOB pathway
        if (.not. lpath(2) .and. .not. lpath(3) .and. .not. lpath(4) .and. .not. lpath(5) .and. .not. lpath(6)) then !no paths
    l3:     do i=2,npath
                if (.not. lpath(i)) write(writeunit,'(12x,a3,5x,a10,2(1x,a))')'E06',modnam,trim(adjustl(pathid(i))),&
                    'PATHWAY IS MISSING'
            enddo l3
            lbad1=.true.
        else !check for mandatory keywords and valid combinations of keywords
            if (lpath(2)) then
                ipath=2
                call up_test
                if (.not. upstage1) call up_stage2
                if (lbad .and. .not. lbad1) lbad1=.true.
            endif
            if (lpath(3)) then
                ipath=3
                call sf_test
                if (.not. sfstage1) call sf_stage2
                if (lbad .and. .not. lbad1) lbad1=.true.
            endif
            if (lpath(4) .or.lpath(5)) then
                if (lpath(4)) then
                    ipath=4
                else
                    ipath=5
                endif
                call os_test
                if (.not. osstage1) call os_stage2
                if (lbad .and. .not. lbad1) lbad1=.true.
            endif
            if (lpath(6)) then 
                ipath=6
                call pbl_test 
                if (lbad .and. .not. lbad1) lbad1=.true.
            endif   
        endif
     endif
    
    return
    end subroutine readinp
      
!*********************************************************************************************************

    subroutine check_line
!=========================================================================================================
!   SUBROUTINE CHECK_LINE
!   THIS SUBROUTINE READS THE LINE FROM THE RUNSTREAM FILE AND
!   DETERMINES THE PATH AND/OR KEYWORD BEING PROCESSED AND
!   CHECKS THE SYNTAX AND NUMBER OF FIELDS
!     
!   MODIFIED JANUARY 24, 2022
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
!   istart:         first non-blank character in the input line
!   firstblank:     location in the input line of the first blank after istart
!   curr_path:      current pathway being processed (saved variable)
!
!    Logical variables
!   lfound:         logical variable denoting a path ID or keyword found
!   qindx:          2-element logical variable denoting that quotes found.
!   singlquote:     2-element array denoting if both quotes surrounding
!                   field are single quotation marks.  If singlquote(1)
!                   and singlquote(2) are not the same, issue error
!   parenth:        2-element array checking for left and right parenthesis
!
!   Character variables
!   formstr:        format for messages
!   modnam:         Subroutine name
!========================================================================================================= 
    use main1, only: npath,nkeys,nfield,lpath,lkey,lstage,getunit,msg_form
    implicit none
    integer(kind=4) :: i,firstblank,istart
    integer(kind=4), save :: curr_path=0
    logical :: lfound,qindx(2),singlquote(2),parenth(2)
    character(len=60) :: formstr
    character(len=10) :: modnam='CHECK_LINE'
      
!   message formats
    write(formstr,'(2(a))')trim(adjustl(msg_form)),'3(a,1x),i3)'
    
!   initialize lbad to false (line is initially thought to be okay)
    lbad=.false.
    istart=0
    firstblank=0
    lfound=.false.
    qindx=.false.
    singlquote=.false.
    parenth=.false.
    i=1
!   initialize nfield to 1 because there will be at least one field 
!   because this subroutine only triggered when ilen > 0
    nfield=1
!   find first non-blank character to account for indented text
!   read through line until non-blank found
    do while(i <= ilen .and. istart == 0)
        if (ichar(inpline1(i:i)) /= 32)istart=i
        i=i+1
    enddo
      
!   get # of fields in line by looking for blank or comma characters between non-blank characters
!   check for fields enclosed by quotation marks (single or double).  Only count
!   quotations that are preceded by a blank or comma or proceeded by a blank.  Only
!   increment nfield when a blank or comma is not enclosed by quotations.
!   for FORMAT statements, do not increment nfield when blank or comma is enclosed by parentheses
    
 
    l1: do i=istart,ilen-1
!       look for quote with a preceding blank, starting with column 2 of inpline1
!       if there is a quote in the first column, the code in the remainder of the 
!       subroutine will catch the error.

        if (i > 1) then
!           quotation marks (single or double)
            if ((ichar(inpline1(i:i)) == 34 .or. ichar(inpline1(i:i)) == 39) .and. (ichar(inpline1(i-1:i-1))== 32 .or. &
                ichar(inpline1(i+1:i+1)) == 32 .or. ichar(inpline1(i-1:i-1))== 44 .or. ichar(inpline1(i+1:i+1)) == 44)) then
                if (.not. qindx(1)) then
                    qindx(1)=.true.
                    if(ichar(inpline1(i:i)) == 34) then
                        singlquote(1)=.false.
                    else
                        singlquote(1)=.true.
                    endif
                else
                    qindx(2)=.true.
                    if(ichar(inpline1(i:i)) == 34) then
                        singlquote(2)=.false.
                    else
                        singlquote(2)=.true.
                    endif
                endif
            endif
!           left parentheses for format statements
            if (index(inpline1,'FORMAT') > 0 .and. ichar(inpline1(i:i)) == 40 .and. (ichar(inpline1(i-1:i-1))== 32 .or. &
                ichar(inpline1(i-1:i-1))== 44) .and. .not. parenth(1)) parenth(1)=.true.              
            
        endif
        
        if (((ichar(inpline1(i+1:i+1)) == 32 .and. ichar(inpline1(i:i)) /= 32 .and. ichar(inpline1(i:i)) /= 44) .or. &
            (ichar(inpline1(i+1:i+1)) == 44 .and. ichar(inpline1(i:i)) /= 44 .and. ichar(inpline1(i:i)) /= 32)) .and. &
            ((qindx(1) .and. qindx(2)) .or. (.not. qindx(1) .and. .not. qindx(2))) .and. &
            ((parenth(1) .and. parenth(2)) .or. (.not. parenth(1) .and. .not. parenth(2)))) then 
            nfield=nfield+1
            qindx=.false.
        endif
    enddo l1    
    
!   if a mix of single and double quotes used, issue error
    if ((singlquote(1) .and. .not. singlquote(2)) .or. (.not. singlquote(1) .and. singlquote(2))) then    
        write(writeunit,'(12x,a3,5x,a10,3(1x,a),i3)')'E01',adjustl(modnam),'MIX OF SINGLE AND DOUBLE QUOTES',&
            trim(adjustl(pathid(ipath))),' FOUND LINE NUMBER:',linenum
        lbad=.true.
    endif

!   if nfield=1 then could be a pathway but could also be one of several 
!   keywords that don't have an option listed such as chk_syntax or modify
!   or debug (which could also have more than 1 field)
!   if the string doesn't match a path, then check for a keyword
    if (nfield == 1) then !path (JOB, UPPERAIR, SURFACE, ONSITE, METPREP)
        lfound=.false.
        ipath=1
!       find path in path ID array
        do while (.not. lfound .and. ipath <= npath)
            if (trim(adjustl(inpline1(istart:ilen))) == trim(adjustl(pathid(ipath)))) then
                lfound=.true.
            else
                ipath=ipath+1
            endif
        enddo
          
        if (lfound) then
            if (.not. lpath(ipath)) then
                lpath(ipath)=.true.
!               if MERGE pathway, warn user not needed and ignore processing for MERGE
                if (ipath == 7)write(writeunit,'(12x,a3,5x,a10,2(1x,a))')'W01',adjustl(modnam),trim(adjustl(pathid(ipath))),&
                    'PATH IS OBSOLETE; ASSOCIATED KEYWORDS WILL BE IGNORED' 
                if (ipath == 6) lstage(2)=.true. !METPREP indicates stage 2
            else
                lbad=.true.
            
                write(writeunit,'(12x,a3,5x,a10,3(1x,a),i3)')'E01',adjustl(modnam),'DUPLICATE ENTRY FOR PATH',&
                    trim(adjustl(pathid(ipath))),'FOUND LINE NUMBER:',linenum
            endif
!           determine which stage(s) is being processed
!           if UPPERAIR, SURFACE, ONSITE, or MMIF found with DATA keyword then stage 1
!           otherwise it is stage 2 (METPREP)
              
        else !possible invalid string for path
!           check to see if the CHK_SYNTAX keyword ,MODIFY keyword, or NOPRINT
            if (trim(adjustl(inpline1(istart:ilen))) == trim(adjustl(keywrd(3))) .or. trim(adjustl(inpline1(istart:ilen))) == &
                trim(adjustl(keywrd(25))) .or. trim(adjustl(inpline1(istart:ilen))) == trim(adjustl(keywrd(36)))) then !chk_syntax, debug, or noprint keyword
!                 reset ipath to 1 if JOB already found or no paths found
!                 because JOB doesn't have to be listed
!			      post 21DRF
!                  if ((lpath(1) .and. .not. lpath(2) .and. .not. lpath(3) .and. .not. lpath(4) .and. .not. lpath(5) .and. .not. &
!                      lpath(6)) .or. (.not. lpath(1)  .and. .not. lpath(2) .and. .not. lpath(3) .and. .not. lpath(4) .and. .not. &
!                      lpath(5) .and. .not. lpath(6))) then
                  !code modified by GMM
				  !original code:-
				  !if (curr_path==1 .or. (.not. lpath(1)  .and. .not. lpath(2) .and. .not. lpath(3) .and. .not. lpath(4) .and. .not. &
				  if (curr_path==1 .or. (.not. lpath(1)  .and. .not. lpath(2) .and. .not. lpath(3) .and. .not. lpath(4) .and. &
                      .not.  lpath(5) .and. .not. lpath(6))) then
                    ipath=1
                    if (trim(adjustl(inpline1(istart:ilen))) == trim(adjustl(keywrd(3)))) then
                        ikey=3
                    elseif (trim(adjustl(inpline1(istart:ilen))) == trim(adjustl(keywrd(25)))) then
                        ikey=25
                    else
                        ikey=36
                    endif
                    
                    lfound=.true.
                  else
                    call getunit
                    write(writeunit,'(12x,a3,5x,a10,3(1x,a),i3)')'E01',adjustl(modnam),'INVALID PLACEMENT FOR KEYWORD',&
                        trim(adjustl(inpline1)),'LINE NUMBER:',linenum
                    lbad=.true.
                endif
            elseif (trim(adjustl(inpline1(istart:ilen))) == trim(adjustl(keywrd(11)))) then !modify keyword
                lfound=.true.
                ikey=11
!               post 21DRF
!                if (lpath(2) .and. .not. lpath(3) .and. .not. lpath(4).and. .not. lpath(5) .and. .not. lpath(6)) then !reset ipath      
!                    ipath=2
                if (curr_path == 2) then 
                    ipath=2
                else
                    call getunit
                    write(writeunit,'(12x,a3,5x,a10,3(1x,a),i3)')'E01',adjustl(modnam),'INVALID PLACEMENT FOR KEYWORD',&
                        trim(adjustl(inpline1)),'LINE NUMBER:',linenum
                    lbad=.true.
                endif
            else
!               determine which file to write error message
                call getunit
                write(writeunit,'(12x,a3,5x,a10,3(1x,a),i3)')'E01',adjustl(modnam),'INVALID PATH',trim(adjustl(inpline1)),&
                    'LINE NUMBER:',linenum        
                lbad=.true.
            endif
        endif
    else !possible keyword
!       read 1st "word" of line
        firstblank=0 !location of 1st blank after first non-blank character
        i=istart
        do while (firstblank == 0)
            if (ichar(inpline1(i:i)) == 32 .or. ichar(inpline1(i:i)) == 44)firstblank=i
            i=i+1
        enddo
        ikey=1
        lfound=.false.
!       determine if string is possible keyword
        do while (.not. lfound .and. ikey <= nkeys)
            if (trim(adjustl(inpline1(istart:firstblank-1))) == trim(adjustl(keywrd(ikey)))) then
                lfound=.true.
            else
                ikey=ikey+1
            endif
        enddo
        if (lfound) then
            lkey(ikey)=.true.

!           DATA or EXTRACT found, indicates stage 1; however ignore DATA if associated with METPREP as this is obsolete
            if ((ikey == 4 .and. ipath /= 6) .or. ikey == 7) lstage(1)=.true.  
            
!           if MESSAGE or REPORT keyword found and no other path found yet
!           set lpath(1) to true since these are allowed without the JOB
!           pathway if no other paths have been indicated beforehand.
              if (ikey <= 2 .and. .not. lpath(1) .and. .not. lpath(2) .and. .not. lpath(3) .and. .not. lpath(4) .and. .not. &
                  lpath(5) .and. .not. lpath(6)) then
                lpath(1)=.true.
                ipath=1
            endif
        else !invalid keyword
            call getunit
            if (ipath <= npath) then  !in case there is no valid path listed before keyword
                write(writeunit,formstr)adjustl(pathid(ipath)),'E01',modnam,'INVALID KEYWORD:',&
                    trim(adjustl(inpline1(istart:firstblank-1))),'LINE NUMBER:',linenum
            else
                write(writeunit,formstr)adjustl('UNKNOWN   '),'E01',modnam,'INVALID KEYWORD:',&
                    trim(adjustl(inpline1(istart:firstblank-1))),'LINE NUMBER:',linenum
            endif
            lbad=.true.
        endif
    endif
!	post 21DRF
    curr_path=ipath

    return
    end subroutine check_line
!*********************************************************************************************************

    subroutine job_path
!=========================================================================================================
!   SUBROUTINE JOB_PATH
!   THIS SUBROUTINE PROCESSES LINES FROM THE INPUT RUNSTREAM FILE ASSOCIATED WITH
!   THE JOB PATHWAY.  THIS INCLUDES: MESSAGE, REPORT, CHK_SYNTAX, AND NOPRINT KEYWORDS
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
!
!   Logical variables
!   lgood:          variable denoting if a filename is okay
!
!   Character variables
!   form1:          format to read filename
!   formstr:		format for messages
!   modnam:			Subroutine name
!=========================================================================================================
    use main1, only: nfield,inpline,noprint,debug,getunit,checkfile,msg_form
    use file_units, only: flength,msg_unit,rpt_unit,debug_unit,msgfile,rptfile,dbgfile
    implicit none
      
    integer(kind=4) :: i,i1      
    logical :: lgood
    character(len=6) :: form1
    character(len=60) :: formstr(2)
    character(len=10) :: modnam='JOB_PATH'
      
!   formats for messages
!   1.  duplicate or invalid keyword
    write(formstr(1),'(2(a))')trim(adjustl(msg_form)),'a,1x,a)'
    
!   2.  invalid # of fields
    write(formstr(2),'(2(a))')trim(adjustl(msg_form)),'i3,2(1x,a))'
    
!   initialize
    i=0
    i1=0
    lgood=.true.
      
!   get unit number to write messages
!   may have an error before the message file has been listed
    call getunit
      
!   form1 is the format to use to read the message or report file
!   format string is (a300)
    write(form1,'(a2,i3,a1)')'(a',flength,')'
      
!   ikey = 0 means text string JOB found on input line
!   set the logical variables lreport, lsmg, and lsyntax to false
!   meaning keywords have not been detected yet

    if (ikey == 0) then
        ljbkeywords=.false.
    else
        i=index(inpline1,trim(adjustl(keywrd(ikey))))
        i1=len_trim(keywrd(ikey))
        if (ikey <= 3 .or. ikey==25 .or. ikey == 36) then !MESSAGE, REPORT, NOPRINT, OR DEBUG
!           temporarily reset ikey to 4 if ikey =36 or 5 if 25
            if (ikey==36) ikey=4
            if (ikey==25) ikey=5
            if (.not. ljbkeywords(ikey)) then
                ljbkeywords(ikey)=.true.
                if (ikey==4) then
                    noprint=.true. !set noprint
                    ikey=36
                endif
                if (ikey==5) then
                    debug=.true.
                    ikey=25
                endif
            else
                write(writeunit,formstr(1))adjustl(pathid(ipath)),'E03',modnam,'DUPLICATE ENTRY OF KEYWORD:',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
                if (ikey==4)ikey=36 !reset NOPRINT ikey back to 36
                if (ikey==5) ikey=25 !reset DEBUG ikey back to 25
                
                return
            endif
        else !invalid keyword
            write(writeunit,formstr(1))adjustl(pathid(ipath)),'E01',modnam,'INVALID KEYWORD:',trim(adjustl(keywrd(ikey)))
            lbad=.true.
            return
        endif
!       if ikey = 1 (REPORT) or ikey = 2 (MESSAGE) and more than 2 fields
!       ikey= 3 (CHK_SYNTAX) ikey=36 (NOPRINT) and more than 1 field, then line is bad
!       there should only be one field for CHK_SYNTAX or NOPRINT, the keyword itself.
!       there should only be two fields with the REPORT or MESSAGE file,
!       the keyword and the filename.
        if (ikey == 1) then
            if (nfield /=2) then
                write(*,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                 read(inpline(i+i1+1:ilen),form1)rptfile
                call checkfile(rpt_unit,rptfile,2,lgood)
!               set rpt_unit to output_unit if problem with the filename
                if (.not. lgood) rpt_unit=output_unit
            endif
        elseif(ikey == 2) then !MESSAGE
            if (nfield /=2) then
                write(*,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            else
                read(inpline(i+i1+1:ilen),form1)msgfile
                
                call checkfile(msg_unit,msgfile,2,lgood)
!               set msg_unit to output_unit if problem with the filename
                if (.not. lgood) then
                    msg_unit=output_unit
                else
                    call getunit !reset writeunit
                endif
            endif
        elseif(ikey == 25) then !DEBUG; may have optional filename
            if (nfield == 1) then
                dbgfile='aermet_debug.txt'
                call checkfile(debug_unit,dbgfile,2,lgood)
            elseif (nfield == 2) then !get filename
                read(inpline(i+i1+1:ilen),form1)dbgfile
                call checkfile(debug_unit,dbgfile,2,lgood)
                if (.not. lgood) dbgfile='aermet_debug.txt' !reset to default
            elseif (nfield > 2) then
                write(*,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            endif
        else !CHK_SYNTAX, NOPRINT
            if (nfield /= 1) then
                write(*,formstr(2))adjustl(pathid(ipath)),'E03',modnam,nfield,'IS AN INVALID NUMBER OF FIELDS FOR KEYWORD',&
                    trim(adjustl(keywrd(ikey)))
                lbad=.true.
            endif
        endif
    endif
      

    return
    end subroutine job_path
!*********************************************************************************************************
    end module read_input

      
      
      