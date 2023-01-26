    module file_units
!=========================================================================================================
!   MODULE FILE UNITS
!     
!   THIS MODULE CONTAINS THE FILE UNIT NUMBERS AND FILENAME VARIABLES
!   FOR ALL PATHS (JOB, UPPERAIR, SURFACE, ONSITE, MMIF, METPREP, AND AERCOARE)
!
!   MODIFIED DECEMBER 2, 2021
!     
!   JAMES THURMAN
!   US EPA/AQMG
!
!   USED BY:    AERMET, MODULES MAIN1, MISC, ONSITE, READ_INPUT, REPORTS, SURFACE, UPPERAIR
!
!   Variable definitions
!
!   Integer variables
!   flength:            filename length; set to 300
!   inp_unit:           input control file (AERMET.INP) unit (11)
!   msg_unit:           MESSAGE file unit (12)
!   rpt_unit:           REPORT file unit (13)
!   up_data_unit:       upper air DATA file unit (14)
!   up_extract_unit:    upper air EXTRACT file unit (15)
!   up_qaout_unit:      upper air QAOUT file unit (16)
!   sf_data_unit:       surface DATA file unit (17)
!   sf_extract_unit:    surface EXTRACT file unit (18)
!   sf_qaout_unit:      surface QAOUT file unit (19)     
!   one_min_unit:       AERMINUTE output file (20)
!   ishd_units:         ISHD discard records (21) and replaced records (22) file units
!   os_data_unit:       onsite or prognostic DATA file unit (23)
!   os_qaout_unit:      onsite or prognostic QAOUT file unit (24)
!   sfc_out_unit:       AERMET surface file for AERMOD unit (25)
!   pfl_out_unit:       AERMET profile file for AERMOD unit (26)
!   sfc_char_unit:      AERSURFACE output file units (27, 28)
!   debug_unit:         debug file unit (29)
!
!   Character variables
!   inpfile:            input runstream filename; default to aermet.inp
!   msgfile:            MESSAGE filename
!   rptfile:            REPORT filename
!   dbgfile:            debug filename; default to aermet_debug.txt
!   up_inpfile:         upper air DATA filename
!   up_qafile:          upper air QAOUT filename
!   up_extfile:         upper air EXTRACT filename
!   sf_inpfile:         surface DATA filename
!   sf_qafile:          surface QAOUT filename
!   sf_extfile:         surface EXTRACT filename
!   one_minfile:        AERMINUTE output filename
!   ishd_files(2):      ISHD discarded records file (1) and replaced records file (2)
!   os_inpfile:         onsite or prognostic DATA filename
!   os_qafile:          onsite or prognostic QAOUT filename
!   sfc_outfile:        AERMET surface filename for AERMOD 
!   pfl_outfile:        AERMET profile filename for AERMOD
!   sfc_charfile1:      array of AERSURFACE output filenames for the primary site; associated with sfc_char_unit(1)
!   sfc_charfile2:      array of AERSURFACE output filenames for the secondary site; associated with sfc_char_unit(2)
!=========================================================================================================

    implicit none

    integer (kind=4), parameter :: flength = 300
      
!   job outputs
    integer (kind=4) :: inp_unit    = 11  
    integer (kind=4) :: msg_unit    = 12  
    integer (kind=4) :: rpt_unit    = 13
    integer(kind=4) :: debug_unit=29
    
    character(len=flength) :: inpfile='aermet.inp'
    character(len=flength) :: msgfile=''
    character(len=flength) :: rptfile=''
    character(len=flength) :: dbgfile='aermet_debug.txt'
    
!   upper air inputs/outputs
    integer (kind=4) :: up_data_unit    = 14  
    integer (kind=4) :: up_extract_unit = 15  
    integer (kind=4) :: up_qaout_unit   = 16  
      
!   initialize the filenames to blanks
    character(len=flength) :: up_inpfile= ''
    character(len=flength) :: up_qafile = ''
    character(len=flength) :: up_extfile = ''
      
!   surface inputs/outputs
    integer (kind=4) :: sf_data_unit    = 17  
    integer (kind=4) :: sf_extract_unit = 18  
    integer (kind=4) :: sf_qaout_unit   = 19 
    integer (kind=4) :: one_min_unit     = 20 
    integer (kind=4) :: ishd_units(2)
    data ishd_units /21,22/
    
!   initialize the filenames to blanks
    character(len=flength) :: sf_inpfile = ''
    character(len=flength) :: sf_qafile = ''
    character(len=flength) :: sf_extfile = ''
    character(len=flength) :: one_minfile = ''
    character(len=flength) :: ishd_files(2)
    
    data ishd_files /'ISHD_discard.txt','ISHD_replace.txt'/
    
!   onsite or mmif inputs/outputs
    integer (kind=4) :: os_data_unit    = 23   
    integer (kind=4) :: os_qaout_unit    = 24   

!   initialize the filenames to blanks
    character(len=flength) :: os_inpfile = ''
    character(len=flength) :: os_qafile = ''
      
!   surface characteristics files
!   1=primary site,2=secondary site
    integer (kind=4) :: sfc_char_unit(2)
    data sfc_char_unit /27,28/
    
!   initialize the filenames to blanks
    character(len=flength),allocatable, dimension(:) :: sfc_charfile1 
    character(len=flength),allocatable, dimension(:) :: sfc_charfile2 
      
!   final output files
    integer (kind=4) :: sfc_out_unit    = 25   
    integer (kind=4) :: pfl_out_unit    = 26  

!   initialize the filenames to blanks
    character(len=flength) :: sfc_outfile = ''
    character(len=flength) :: pfl_outfile = ''

    end module file_units