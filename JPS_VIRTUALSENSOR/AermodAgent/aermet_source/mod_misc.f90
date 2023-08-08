    module misc
!=========================================================================================================
!   MODULE MAIN1
!   THIS MODULE CONTAINS MISCELLANEOUS SUBROUTINES USED IN AN AERMET RUN
!
!   MODIFIED MAY 31, 2022
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   USED BY:    AERMET
!=========================================================================================================   
    
    implicit none
      
    contains 
!*********************************************************************************************************    
  
    subroutine cleanup
!=========================================================================================================
!   SUBROUTINE CLEANUP
!   THIS SUBROUTINE CLOSES FILES and DEALLOCATES ARRAYS
!
!   MODIFIED MAY 31, 2022
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
!
!   Logical variables
!   lopened         logical variable denoting open status of file
!
!   Character variables
!   modnam:         Subroutine name
!=========================================================================================================
    use file_units, only: inp_unit,rpt_unit,debug_unit
    use upperair, only: updata,up_audit_index,up_audit_counts,sound_info
    use surface, only: sfdata,sfc_info,sf_audit_index,sf_audit_counts,one_mindat,lowbound,upbound
    use onsite, only: osdt_hts,os_heights,osdata1,os_info,os_audit_index,os_audit_counts,lcalm_obs_hr
    use pbl, only: sfc_char1,sectors1,nfreq1,nsectors1,sfc_char2,sectors2,nfreq2,nsectors2,sfc_data,pfl_data,pbl_obs
    implicit none
    integer :: i
    logical :: lopened
    character(len=10) :: modnam='CLEANUP'
      
!   initialize
    i=0
    lopened=.false.
      
!   file units are sequential even though
!   have unique names, close all files
!   except report file
    do i=inp_unit,debug_unit !sfc_char_unit(2)
        if (i /= rpt_unit) then
            inquire(unit=i,exist=lopened)
            if (lopened) close(i)
        endif
    enddo
      
!   deallocate arrays
    
!   UPPERAIR
    if (allocated(updata)) deallocate(updata)
    if (allocated(up_audit_index)) deallocate(up_audit_index)
    if (allocated(up_audit_counts)) deallocate(up_audit_counts)
    if (allocated(sound_info)) deallocate(sound_info)
    
!   SURFACE
    if (allocated(sfdata)) deallocate(sfdata)
    if (allocated(sfc_info)) deallocate(sfc_info)
    if (allocated(sf_audit_index)) deallocate(sf_audit_index)
    if (allocated(sf_audit_counts)) deallocate(sf_audit_counts)
    if (allocated(one_mindat)) deallocate(one_mindat)
    if (allocated(lowbound)) deallocate(lowbound)
    if (allocated(upbound)) deallocate(upbound)
!   ONSITE OR PROG
    if (allocated(osdata1)) deallocate(osdata1)
    if (allocated(osdt_hts)) deallocate(osdt_hts)
    if (allocated(os_heights)) deallocate(os_heights)
    if (allocated(os_info)) deallocate(os_info)
    if (allocated(os_audit_index)) deallocate(os_audit_index)
    if (allocated(os_audit_counts)) deallocate(os_audit_counts)
    if (allocated(lcalm_obs_hr)) deallocate(lcalm_obs_hr)
    
!   METPREP
    if (allocated(sfc_char1)) deallocate(sfc_char1)
    if (allocated(sectors1)) deallocate(sectors1)
    if (allocated(nfreq1)) deallocate(nfreq1)
    if (allocated(nsectors1)) deallocate(nsectors1)
    if (allocated(sfc_char2)) deallocate(sfc_char2)
    if (allocated(sectors2)) deallocate(sectors2)
    if (allocated(nfreq2)) deallocate(nfreq2)
    if (allocated(nsectors2)) deallocate(nsectors2)
    if (allocated(sfc_data)) deallocate(sfc_data)
    if (allocated(pfl_data)) deallocate(pfl_data)
    if (allocated(pbl_obs)) deallocate(pbl_obs)
	
    return
    end subroutine cleanup
!*********************************************************************************************************  
    end module misc