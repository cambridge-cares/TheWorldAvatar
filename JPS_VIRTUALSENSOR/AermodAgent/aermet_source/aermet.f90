    program aermet
!=========================================================================================================
!   PROGRAM AERMET
!
!   THIS PROGRAM PROCESSES METEOROLOGICAL DATA FOR INPUT TO AERMOD
!
!   MODIFIED MARCH 31, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!   thurman.james@epa.gov
!=========================================================================================================
    use, intrinsic :: iso_fortran_env,only : output_unit
    use file_units, only: rpt_unit
    use main1,only: datetime,start_stop,lpath,ipath,istage,lstage
    use upperair, only: up_proc,lbadup
    use surface, only: sf_proc,lbadsfc,lsfkeywords,read_1min,lasos
    use onsite,only: os_proc,lbados
    use pbl,only: pbl_proc,lbadpbl
    use read_input, only: readinp,lbad1,ljbkeywords
    use reports, only: input_summ,audit_summ,write_msg,sfchar_sum,pbl_sum,obs_sum
    use misc, only: cleanup
    implicit none
!   logical :: lopened=.false.
    call datetime(1)
    call start_stop(1,output_unit)
    call readinp
!   summarize the inputs before further processing if stage 1 being
!   processed
    if (lstage(1)) call input_summ
      
!   if there was a problem with the runstream file or CHK_SYNTAX
!   was used then stop processing, otherwise continue with processing
    if (lbad1 .or. ljbkeywords(3)) then
!       write messages from message file to report file or screen
        call write_msg
!       close files, except for REPORT file and deallocate arrays
        call cleanup
        call datetime(2)
!        inquire(file=rptfile,opened=lopened)
        if (lbad1) then
            write(rpt_unit,'(//1x,a/)')'AERMET FINISHED UN-SUCCESSFULLY'
            if (rpt_unit /= output_unit) write(output_unit,'(//1x,a/)')'AERMET FINISHED UN-SUCCESSFULLY' 
        else
            write(rpt_unit,'(//1x,a/)')'AERMET FINISHED SUCCESSFULLY'
            if (rpt_unit /= output_unit) write(output_unit,'(//1x,a/)')'AERMET FINISHED SUCCESSFULLY'  
        endif
        call start_stop(2,rpt_unit)
        if (rpt_unit /= output_unit) call start_stop(2,output_unit)
    else
!       continue 
!       set istage for up_proc, sf_proc, and os_proc
        if (.not. lstage(1)) then
            istage=2
        else
            istage=1
        endif
!       process upper air for stage 1
        if (lpath(2)) then
            ipath=2
            call up_proc
            if (lbadup) lbad1=.true.
        endif
!       process NWS for stage 1
        if (lpath(3) .and. .not. lbad1) then
            ipath=3
            call sf_proc
            !if (istage == 2 .and. lasos .and. lsfkeywords(9)) call read_1min
            if (lstage(2) .and. lasos .and. lsfkeywords(9)) call read_1min
            if (lbadsfc) lbad1=.true.
        endif
!       process onsite or MMIF for stage 1
        if ((lpath(4) .or. lpath(5))  .and. .not. lbad1) then
            if (lpath(4)) then
                ipath=4
            else
                ipath=5
            endif
            call os_proc
            if (lbados) lbad1=.true.
        endif
!       calculate PBL parameters
        if (lpath(6) .and. .not. lbad1) then
            ipath=6
            call pbl_proc
            if (lbadpbl) lbad1=.true.
        endif

        if (.not. lstage(1)) call input_summ
        
!       if auditing performed, then summarize the audits
        if (.not. lbad1) call audit_summ
        
!       write surface characteristics and pbl processing information
        if (lpath(6)  .and. .not. lbad1) then
            call sfchar_sum
!           post 21DRF, summarize # of days with each data type and PBL processing
            call obs_sum
            call pbl_sum
        endif
        
        
!       
        
!       write messages from message file to report file or screen
        call write_msg
        call cleanup
        call datetime(2)
        if (lbadup .or. lbadsfc .or. lbados .or. lbadpbl) then
            write(rpt_unit,'(//1x,a/)')'AERMET FINISHED UN-SUCCESSFULLY'
            if (rpt_unit /= output_unit) write(output_unit,'(//1x,a/)')'AERMET FINISHED UN-SUCCESSFULLY'   
        else
            write(rpt_unit,'(//1x,a/)')'AERMET FINISHED SUCCESSFULLY'
            if (rpt_unit /= output_unit) write(output_unit,'(//1x,a/)')'AERMET FINISHED SUCCESSFULLY'      
        endif
        call start_stop(2,rpt_unit)
        if (rpt_unit /= output_unit) call start_stop(2,output_unit)
    endif
      
    end