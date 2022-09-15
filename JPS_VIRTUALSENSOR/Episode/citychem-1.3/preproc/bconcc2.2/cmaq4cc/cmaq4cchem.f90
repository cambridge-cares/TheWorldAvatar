! <cmaq4cchem.f90 - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*
!*  Unless explicitly acquired and licensed from Licensor under another license,
!*  the contents of this file are subject to the Reciprocal Public License ("RPL")
!*  Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!*  allowed by the RPL, and You may not copy or use this file in either source code
!*  or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!*  All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!*  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!*  DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!*  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!*  See the RPL for specific language governing rights and limitations under the RPL.
!*
!*****************************************************************************!
!*****************************************************************************! 
!  PROGRAM: CMAQ4CCHEM
!
!  VERSION: 1.1
!
!  PURPOSE:    This program creates 3-D BCON files for use with CityChem.
!              Assumes CMAQ has been run with the chemical mechanism
!              cb05tucl_ae5_aq. If a different chemical mechanism has been used,
!              the species indices in lines 468-503 must be adjusted to the
!              the corresponding variable list of the CONC* output file.
!
! Based on:
!
!
! Contact:
!              See License header
!
!*****************************************************************************!

      program cmaq4cchem

! ----------------------------------------------------------------------------------
!
! Program BCONCC creates 3-D BCON files for use with CityChem
! 
! ----------------------------------------------------------------------------------
!
! Changes compared to V1.0:
!    March 2019  M. Karl: Added ISOP (gas 19) and SULF (gas 20) from CMAQ output
!                          and write to new files
!   02 Apr 2019  M. Karl: Correction of vertical interpolation of CMAQ conc.
!   03 Apr 2019  M. Karl: Define PM indices as starting from ncg
!   03 Apr 2019  M. Karl: Corrections of ground-level O3 and PM with 7th layer
!   30 Apr 2019  M. karl: Split TERP 50:50 in Apinene and Limonene (ncg=21,22)
!   30 Apr 2019  M. Karl: Rename bcon output file _ch3cocho_ to _ch3coc2h5_
!   19 Sep 2019  M. Karl: Added 'USE MODATTS3' for compatibility with IOAPI 3.2
!   19 Sep 2019  M. Karl: New output file coarse mode dust and seasalt
!   24 Oct 2019  M. Karl: IOLE is used for bcons of c3h6
!   24 Oct 2019  M. Karl: New output file for c3h6 and for hono
!   24 Oct 2019  M. Karl: NO3 (=ANO3I+ANO3J) as separate PM compound
!   24 Oct 2019  M. Karl: New output file for fine mode NO3  
!   
! ----------------------------------------------------------------------------------

          use module_cc_input

! ***  Compatibility to ioapi v3.2
          USE MODATTS3

        implicit none

! **** IOAPI includes

        INCLUDE 'PARMS3.EXT'         !  I/O API parameters (in modfileset)
        INCLUDE 'IODECL3.EXT'        !  I/O API function declarations 
        INCLUDE 'FDESC3.EXT'         !  I/O API file description data structures (in modfileset)

      integer, parameter :: hhd  = 24                !! read one day from CMAQ input file
      
      integer, parameter :: ncp = 31                !! number of chemical species for xbcon output

      integer, parameter :: nz = 24                 !! cells in Z-dir

      integer, parameter :: nbcx = 1                !! border cells in X-dir

      integer, parameter :: nbcy = 1                !! border cells in Y-dir

      integer, parameter :: nbcz = 1                !! border cells in Z-dir

      integer, parameter :: nzcmaq = 24             !! cmaq vertical layers

      integer, parameter :: ncg = 24                !! number of gaseous compounds in xbcon output

! *** CMAQ INPUT
      integer, parameter :: latmin = 1

      integer, parameter :: lonmin = 1

      integer            :: ncols
      integer            :: nrows

! *** ASCII or BINARY: 
!      integer :: EP_fm = 1    ! Ascii  - The extensions below are automatically set to ".asc" (Could also be changed to ".txt" 
!      integer :: EP_fm = 2   ! Binary - The extensions below are automatically set to ".fld"

! *** CITYCHEM layer thickness dsigma_ep  (24+1 extra layer for upper bcon layer)
      real, dimension(nz+nbcz), parameter :: dzf_ep  = (/ 17.50, 20.00, 25.00, 25.00, 37.50, 50.00, 50.00,         &
                                                     50.00, 50.00, 50.00, 50.00, 50.00, 75.00, 125.00,             &
                                                     200.00, 250.00, 250.00, 250.00, 250.00, 250.00, 250.00,       &
                                                     375.00, 500.00, 500.00, 500.00  /)

! ***                                                o3   no  no2 h2o2  so2 form  co   n2o5 hno3  hono 
      real, dimension(ncg), parameter     :: mw = (/ 48,  30,  46,  34,  64,  30,  28,  108,  63,  47,       &
! ***                                                pan  xyl etha ald2 etoh par  eth  iole  mgly  nh3    
                                                     121, 106, 30,  44,  17,  58,  28,   48,  72,  47,       &
! ***                                                isop h2so4 apin limo
                                                     68,  96,   136, 136 /)
      real, parameter                     :: rgas = 8.314

! *** Filename of the 3D time-dependent background concentrations:
      character(len=256) :: BC_input_file( ncp )
      character(len=256) :: CMAQ_bcon_file
      character(len=256) :: CMAQ_bcon_path
      character(len=40)  :: ci
      integer            :: inday

! *** Chemical/physical parameters
! *** CMAQ metcro3d
      real, allocatable  :: ctmp( :,:,:,: )
      real, allocatable  ::   ta( :,:,: )
      real, allocatable  ::   pa( :,:,: )
! *** Background concentration, unit of choice (recommend ug/m3)
      double precision,allocatable :: bconc( :,:,:,: )
      
      real,dimension(nzcmaq) :: z_cq
      real,dimension(nz) :: z_ep, z_mid, z_top
      real               :: mod_h_ep
      real               :: w_hi, w_lo
      integer            :: i, p, n, x, y, k, nh
      logical            :: layer_found = .false.
      logical            :: file_exist  = .false.

! *** IOAPI i/o declarations
      real, allocatable  :: tmp1( :,:,:,: )
      real, allocatable  :: tmp2( :,:,:,: )
      integer            :: col,row,lay
      integer            :: idev       ! tmp unit number if ENAME is map file
      integer            :: ldev       ! log-device
      integer            :: ios1
      integer            :: ios2
      character(len=namlen3) :: iname1               ! CONC
      character(len=namlen3) :: iname2               ! METCRO3D
      character(len=256) :: mesg                     ! message buffer 
      character(len=16)  :: progname = 'CMAQ4CCHEM'  ! program name
      character(len=16)  :: promptmfile
      integer            :: ftime
      integer            :: fdate
      integer            :: cdate
      integer            :: ctime
      integer            :: nlays
      integer            :: nvars1
      integer            :: nvars2
      integer            :: tstep
      integer            :: nsteps

      EXTERNAL   PROMPTMFILE

!	EXTERNAL  CRLF, ENVYN, DSCM3GRD, GETCFDSC, INDEX1, INGRID, 
!     &            PROMPTFFILE, PROMPTMFILE, VERCHAR, FIND1, ENVINT, GETFLINE,
!     &            BLKORCMT, SETENVVAR, GETEFILE

! *** File units
      integer            :: unbc
      integer            :: un( ncp )


! *** Date declarations:
      !character(len=60) :: startdate
      integer, dimension(12), parameter :: monlen  = (/ 31, 28, 31, 30, 31, 30,  &
                                                        31, 31, 30, 31, 30, 31   /)

      integer            :: day
      integer            :: month


! *** File formatting
      character(len=256) :: headerin,dateline
      character(len=10)  :: txt1
      character(len=10)  :: txt2
      character(len=18)  :: filedate

      integer :: ii, jj, kk, c

! ******** USER INTERFACE ******************************************************************************
!
! *** Start date YY MM DD HH MIN SEC
! *** EDIT HERE
!
!      startdate = '12        12         1         0         0         0         '
!      year      = 12
!      month     = 11
!
!
! **********END USER ***********************************************************************************

        call get_user_input

        ncols  = nx+2
        nrows  = ny+2

! ALLOCATE VARIABLES
        if (.not. allocated(ctmp))   allocate(  ctmp(ncols,nrows,nzcmaq,ncp) )
        if (.not. allocated(  ta))   allocate(    ta(ncols,nrows,nzcmaq)     )
        if (.not. allocated(  pa))   allocate(    pa(ncols,nrows,nzcmaq)     )                 
        if (.not. allocated(bconc))  allocate( bconc(ncp,nx+2*nbcx,ny+2*nbcy,nz+nbcz) )  

      
        filedate = '_'//trim(startdate)//'_'//trim(enddate)

      day       = 1
      hour      = 1
      minu      = 0


! *** Header
      txt1  = 'cmaq.bckg_'
      txt2  = 'conc.ug/m3'


! *******************************************************************************************************

! *** CALCULATE CITYCHEM LAYER HEIGHTS
! ***  initialize
        z_cq(:)  = 0.0
        z_ep(:)  = 0.0
        z_top(:) = 0.0
! ***  calculate mid-point layer heights
        mod_h_ep = 3750.0 + 500.00
! ***  different height of terrain (topography) not considered   (zs(i,j))
        z_ep(1)  = dzf_ep(1)*0.5
        z_top(1) = dzf_ep(1)
        do k = 2, nz
          z_ep(k)  = z_top(k-1) + dzf_ep(k)*0.5
          z_top(k) = z_top(k-1) + dzf_ep(k)
        end do

! *******************************************************************************************************

! ***   Giving filename extension indicating ASCII-files (".txt" or ".asc"):

      if (EP_fm .eq. 1) then
         BC_input_file( 1 )  = trim(fname_outpath)//'/cbg_o3_'//trim(filedate)//'.txt'
         BC_input_file( 2 )  = trim(fname_outpath)//'/cbg_no_'//trim(filedate)//'.txt'
         BC_input_file( 3 )  = trim(fname_outpath)//'/cbg_no2_'//trim(filedate)//'.txt'
         BC_input_file( 4 )  = trim(fname_outpath)//'/cbg_h2o2_'//trim(filedate)//'.txt'
         BC_input_file( 5 )  = trim(fname_outpath)//'/cbg_so2_'//trim(filedate)//'.txt'
         BC_input_file( 6 )  = trim(fname_outpath)//'/cbg_hcho_'//trim(filedate)//'.txt'
         BC_input_file( 7 )  = trim(fname_outpath)//'/cbg_co_'//trim(filedate)//'.txt'
         BC_input_file( 8 )  = trim(fname_outpath)//'/cbg_n2o5_'//trim(filedate)//'.txt'
         BC_input_file( 9 )  = trim(fname_outpath)//'/cbg_hno3_'//trim(filedate)//'.txt'
         BC_input_file( 10)  = trim(fname_outpath)//'/cbg_hono_'//trim(filedate)//'.txt'
         BC_input_file( 11 ) = trim(fname_outpath)//'/cbg_pan_'//trim(filedate)//'.txt'
         BC_input_file( 12 ) = trim(fname_outpath)//'/cbg_oxylen_'//trim(filedate)//'.txt'
         BC_input_file( 13 ) = trim(fname_outpath)//'/cbg_c2h6_'//trim(filedate)//'.txt'
         BC_input_file( 14 ) = trim(fname_outpath)//'/cbg_ch3cho_'//trim(filedate)//'.txt'
         BC_input_file( 15 ) = trim(fname_outpath)//'/cbg_c2h5oh_'//trim(filedate)//'.txt'
         BC_input_file( 16 ) = trim(fname_outpath)//'/cbg_nc4h10_'//trim(filedate)//'.txt'
         BC_input_file( 17 ) = trim(fname_outpath)//'/cbg_c2h4_'//trim(filedate)//'.txt'
         BC_input_file( 18 ) = trim(fname_outpath)//'/cbg_c3h6_'//trim(filedate)//'.txt'
! rename CH3COCHO to CH3COC2H5 for use in CITYCHEM 
         BC_input_file( 19 ) = trim(fname_outpath)//'/cbg_ch3coc2h5_'//trim(filedate)//'.txt'
         BC_input_file( 20 ) = trim(fname_outpath)//'/cbg_nh3_'//trim(filedate)//'.txt'
         BC_input_file( 21 ) = trim(fname_outpath)//'/cbg_isoprene_'//trim(filedate)//'.txt'
         BC_input_file( 22 ) = trim(fname_outpath)//'/cbg_sulphate_'//trim(filedate)//'.txt'
         BC_input_file( 23 ) = trim(fname_outpath)//'/cbg_apinene_'//trim(filedate)//'.txt'
         BC_input_file( 24 ) = trim(fname_outpath)//'/cbg_limonene_'//trim(filedate)//'.txt'
         BC_input_file( 25 ) = trim(fname_outpath)//'/cbg_pm25_'//trim(filedate)//'.txt'
         BC_input_file( 26 ) = trim(fname_outpath)//'/cbg_so4_'//trim(filedate)//'.txt'
         BC_input_file( 27 ) = trim(fname_outpath)//'/cbg_soa_'//trim(filedate)//'.txt'
         BC_input_file( 28 ) = trim(fname_outpath)//'/cbg_pm10_'//trim(filedate)//'.txt'
         BC_input_file( 29 ) = trim(fname_outpath)//'/cbg_dust_c_'//trim(filedate)//'.txt'
         BC_input_file( 30 ) = trim(fname_outpath)//'/cbg_salt_c_'//trim(filedate)//'.txt'
         BC_input_file( 31 ) = trim(fname_outpath)//'/cbg_no3_'//trim(filedate)//'.txt'
      else
         BC_input_file( 1 )  = trim(fname_outpath)//'/cbg_o3_'//trim(filedate)//'.fld'
         BC_input_file( 2 )  = trim(fname_outpath)//'/cbg_no_'//trim(filedate)//'.fld'
         BC_input_file( 3 )  = trim(fname_outpath)//'/cbg_no2_'//trim(filedate)//'.fld'
         BC_input_file( 4 )  = trim(fname_outpath)//'/cbg_h2o2_'//trim(filedate)//'.fld'
         BC_input_file( 5 )  = trim(fname_outpath)//'/cbg_so2_'//trim(filedate)//'.fld'
         BC_input_file( 6 )  = trim(fname_outpath)//'/cbg_hcho_'//trim(filedate)//'.fld'
         BC_input_file( 7 )  = trim(fname_outpath)//'/cbg_co_'//trim(filedate)//'.fld'
         BC_input_file( 8 )  = trim(fname_outpath)//'/cbg_n2o5_'//trim(filedate)//'.fld'
         BC_input_file( 9 )  = trim(fname_outpath)//'/cbg_hno3_'//trim(filedate)//'.fld'
         BC_input_file( 10)  = trim(fname_outpath)//'/cbg_hono_'//trim(filedate)//'.fld'
         BC_input_file( 11 ) = trim(fname_outpath)//'/cbg_pan_'//trim(filedate)//'.fld'
         BC_input_file( 12 ) = trim(fname_outpath)//'/cbg_oxylen_'//trim(filedate)//'.fld'
         BC_input_file( 13 ) = trim(fname_outpath)//'/cbg_c2h6_'//trim(filedate)//'.fld'
         BC_input_file( 14 ) = trim(fname_outpath)//'/cbg_ch3cho_'//trim(filedate)//'.fld'
         BC_input_file( 15 ) = trim(fname_outpath)//'/cbg_c2h5oh_'//trim(filedate)//'.fld'
         BC_input_file( 16 ) = trim(fname_outpath)//'/cbg_nc4h10_'//trim(filedate)//'.fld'
         BC_input_file( 17 ) = trim(fname_outpath)//'/cbg_c2h4_'//trim(filedate)//'.fld'
         BC_input_file( 18 ) = trim(fname_outpath)//'/cbg_c3h6_'//trim(filedate)//'.fld'
! rename CH3COCHO to CH3COC2H5 for use in CITYCHEM 
         BC_input_file( 19 ) = trim(fname_outpath)//'/cbg_ch3coc2h5_'//trim(filedate)//'.fld'
         BC_input_file( 20 ) = trim(fname_outpath)//'/cbg_nh3_'//trim(filedate)//'.fld'
         BC_input_file( 21 ) = trim(fname_outpath)//'/cbg_isoprene_'//trim(filedate)//'.fld'
         BC_input_file( 22 ) = trim(fname_outpath)//'/cbg_sulphate_'//trim(filedate)//'.fld'
         BC_input_file( 23 ) = trim(fname_outpath)//'/cbg_apinene_'//trim(filedate)//'.fld'
         BC_input_file( 24 ) = trim(fname_outpath)//'/cbg_limonene_'//trim(filedate)//'.fld'
         BC_input_file( 25 ) = trim(fname_outpath)//'/cbg_pm25_'//trim(filedate)//'.fld'
         BC_input_file( 26 ) = trim(fname_outpath)//'/cbg_so4_'//trim(filedate)//'.fld'
         BC_input_file( 27 ) = trim(fname_outpath)//'/cbg_soa_'//trim(filedate)//'.fld'
         BC_input_file( 28 ) = trim(fname_outpath)//'/cbg_pm10_'//trim(filedate)//'.fld'
         BC_input_file( 29 ) = trim(fname_outpath)//'/cbg_dust_c_'//trim(filedate)//'.fld'
         BC_input_file( 30 ) = trim(fname_outpath)//'/cbg_salt_c_'//trim(filedate)//'.fld'
         BC_input_file( 31 ) = trim(fname_outpath)//'/cbg_no3_'//trim(filedate)//'.fld'
      endif

! ***   File units
        unbc    = 20
        un( 1)  = 21
        un( 2)  = 22
        un( 3)  = 23
        un( 4)  = 24
        un( 5)  = 25
        un( 6)  = 26
        un( 7)  = 27
        un( 8)  = 28
        un( 9)  = 29
        un(10)  = 30
        un(11)  = 31
        un(12)  = 32
        un(13)  = 33
        un(14)  = 34
        un(15)  = 35
        un(16)  = 36
        un(17)  = 37
        un(18)  = 38
        un(19)  = 39
        un(20)  = 40
        un(21)  = 41
        un(22)  = 42
        un(23)  = 43
        un(24)  = 44
        un(25)  = 45
        un(26)  = 46
        un(27)  = 47
        un(28)  = 48
        un(29)  = 49
        un(30)  = 50
        un(31)  = 51

! ***   Open the 3D time-independent xbcon_2012put files:
! ***   If files exist, append the new day

        do i=1,ncp

          inquire(file = BC_input_file(i), exist=file_exist)
          if (file_exist) then
             if (EP_fm .eq. 1) then
               open (un(i), file = BC_input_file(i), status = 'old', form  = 'formatted',   position="append", action = 'write')
             else
              open (un(i), file = BC_input_file(i),  status = 'old', form  = 'unformatted', position="append", action = 'write')
             endif
          else
             if (EP_fm .eq. 1) then
               open (un(i), file = BC_input_file(i), status = 'unknown', form  = 'formatted', action = 'write')
             else
              open (un(i), file = BC_input_file(i),  status = 'unknown', form  = 'unformatted', action = 'write')
             endif
          endif

        enddo


! *** Header Line                ********************************

! wanted format:
!        READ (UN,2103) TEXT1,TEXT2,NX_D,NY_D,NZ_D,NBCX_D,NBCY_D,NBCZ_D
! 2103 FORMAT (2A10,6I3)

        ! in all compound bc files
        do i=1,ncp
          if (.not.file_exist) then
            if (EP_fm .eq. 1) then
              write (un(i),'(2A10,6I3)'  )    txt1,txt2,nx,ny,nz,nbcx,nbcy,nbcz
            !!else
            !!   write (un(i) )   txt1,txt2,nx,ny,nz,nbcx,nbcy,nbcz
            endif
          endif
        enddo


!***********************************************************************
!.........  Begin of program CMAQ4CCHEM

	LDEV = INIT3()

! *** OPEN INPUT FILES (IOAPI)

! *** CMAQ Concentrations
        MESG   = 'Enter logical name for the INPUT FILE1 (CMAQ CONC)'
        INAME1 = 'INPUT_FILE1'
        INAME1 = PROMPTMFILE( MESG, FSREAD3, INAME1, PROGNAME )

! *** METCRO3D Variables
        MESG   = 'Enter logical name for the INPUT FILE2 (METCRO3D)'
        INAME2 = 'INPUT_FILE2'
        INAME2 = PROMPTMFILE( MESG, FSREAD3, INAME2, PROGNAME )

! *** Get header from file and store necessary info
        IF ( .NOT. DESC3( INAME1 ) ) THEN
            MESG = 'Could not get description of file ' // INAME1
            CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
        END IF
	CDATE   = SDATE3D
	CTIME   = STIME3D
	FTIME   = CTIME
	NLAYS   = NLAYS3D
	NSTEPS  = MXREC3D
	TSTEP   = TSTEP3D
	SDATE3D = FDATE
	NVARS1  = NVARS3D

        IF ( .NOT. DESC3( INAME2 ) ) THEN
            MESG = 'Could not get description of file ' // INAME2
            CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
        END IF
	NVARS2  = NVARS3D

 
! *** CMAQ FORMAT    TMP1( NCOLS,NROWS,NLAYS,NVARS )
!     Dimensions: 32 rows, 32 cols, 30 lays, 135 vbles
! *** METCRO3 FORMAT TMP2( NCOLS,NROWS,NLAYS,NVARS )
!     Dimensions: 32 rows, 32 cols, 30 lays, 16 vbles
! *** Allocate temporary CMAQ fields
	ALLOCATE( TMP1( NCOLS,NROWS,NLAYS,NVARS1 ), STAT=IOS1 )
	ALLOCATE( TMP2( NCOLS,NROWS,NLAYS,NVARS2 ), STAT=IOS2 )


! *** Going through the time-loop:

!      START AT 00:00 AND END AT 23:00
!	DO T = 1, NSTEPS-1

        do n = 1, hhd

! ***   READ the CMAQ BCON FILE as ASCII-File

! *** Get TMP with all variables from CMAQ INPUT FILE
	  IF ( .NOT. READ3( INAME1, ALLVAR3, ALLAYS3, CDATE, CTIME, TMP1 ) ) THEN
	     MESG = 'ERROR: Reading timestep from file ' // INAME1
             CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
	  END IF
!:VAR-LIST = "
!1-8	 NO2             NO              O               O3              NO3             O1D             OH       HO2            
!9-16	 N2O5            HNO3            HONO            PNA             H2O2            XO2             XO2N     NTR
!17-24	 ROOH            FORM            ALD2            ALDX            PAR             CO              MEO2     MEPX
!25-32	 MEOH            HCO3            FACD            C2O3            PAN             PACD            AACD     CXO3
!33-40	 PANX            ROR             OLE             ETH             IOLE            TOL             CRES     TO2
!41-48	 TOLRO2          OPEN            MGLY            CRO             CAT1            CRON            CRNO     CRN2
!49-56	 CRPX            OPO3            CAO2            OPAN            XYL             XYLRO2          ISOP     ISPD
!57-64	 TERP            SO2             SULF            ETOH            ETHA            CL2             CL       HOCL
!65-72	 CLO             FMCL            HCL             BENZENE         BENZRO2         SESQ            ASO4J    ASO4I 
!73-80	 ANH4J           ANH4I           ANO3J           ANO3I           AALKJ           AXYL1J          AXYL2J   AXYL3J  
!81-88	 ATOL1J          ATOL2J          ATOL3J          ABNZ1J          ABNZ2J          ABNZ3J          ATRP1J   ATRP2J      
!89-96	 AISO1J          AISO2J          ASQTJ           AORGCJ          AORGPAJ         AORGPAI         AECJ     AECI        
!97-104	 A25J            ACORS           ASOIL           NUMATKN         NUMACC          NUMCOR          SRFATKN  SRFACC     
!105-112 SRFCOR          AH2OJ           AH2OI           ANAJ            ACLJ            ACLI            ANAK     ACLK      
!113-120 ASO4K           ANH4K           ANO3K           AH2OK           AISO3J          AOLGAJ          AOLGBJ   NH3            
! SV_ALK          SV_XYL1         SV_XYL2         SV_TOL1         SV_TOL2         SV_BNZ1         SV_BNZ2         SV_TRP1     
! SV_TRP2         SV_ISO1         SV_ISO2         SV_SQT          W_VEL           LAT             LON             " ;

	  IF ( .NOT. READ3( INAME2, ALLVAR3, ALLAYS3, CDATE, CTIME, TMP2 ) ) THEN
	     MESG = 'ERROR: Reading timestep from file ' // INAME2
             CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )
	  END IF

!:VAR-LIST = "JACOBF          JACOBM          DENSA_J         WHAT_JD         TA              QV              PRES
!             DENS            ZH              ZF              QC              QR              QI              QS
!             PV              WWIND           " ;


! *** EXTRACT ZH, TA, PA FROM METCRO3D INPUT
            do lay = 1, nzcmaq
              do col=1, ncols
                do row = 1, nrows
                  ta(col,row,lay)      = TMP2(COL,ROW,LAY,5)     ! TA
                  pa(col,row,lay)      = TMP2(COL,ROW,LAY,7)     ! PRES
                  ctmp(col,row,lay,1)  = TMP1(COL,ROW,LAY,4)     ! O3 
                  ctmp(col,row,lay,2)  = TMP1(COL,ROW,LAY,2)     ! NO
                  ctmp(col,row,lay,3)  = TMP1(COL,ROW,LAY,1)     ! NO2
                  ctmp(col,row,lay,4)  = TMP1(COL,ROW,LAY,13)    ! H2O2
                  ctmp(col,row,lay,5)  = TMP1(COL,ROW,LAY,58)    ! SO2
                  ctmp(col,row,lay,6)  = TMP1(COL,ROW,LAY,18)    ! HCHO (=FORM)
                  ctmp(col,row,lay,7)  = TMP1(COL,ROW,LAY,22)    ! CO
                  ctmp(col,row,lay,8)  = TMP1(COL,ROW,LAY,9)     ! N2O5
                  ctmp(col,row,lay,9)  = TMP1(COL,ROW,LAY,10)    ! HNO3
                  ctmp(col,row,lay,10) = TMP1(COL,ROW,LAY,11)    ! HONO
                  ctmp(col,row,lay,11) = TMP1(COL,ROW,LAY,29)    ! PAN
                  ctmp(col,row,lay,12) = TMP1(COL,ROW,LAY,53)    ! XYL
                  ctmp(col,row,lay,13) = TMP1(COL,ROW,LAY,61)    ! ETHA (=Ethane)
                  ctmp(col,row,lay,14) = TMP1(COL,ROW,LAY,19)    ! ALD2 (=Acetaldehyde)
                  ctmp(col,row,lay,15) = TMP1(COL,ROW,LAY,60)    ! ETOH (=Ethanol)
                  ctmp(col,row,lay,16) = TMP1(COL,ROW,LAY,21)    ! PAR
                  ctmp(col,row,lay,17) = TMP1(COL,ROW,LAY,36)    ! ETH  (=Ethene)
                  ctmp(col,row,lay,18) = TMP1(COL,ROW,LAY,37)    ! IOLE (=internal alkenes)
                  ctmp(col,row,lay,19) = TMP1(COL,ROW,LAY,43)    ! MGLY
                  ctmp(col,row,lay,20) = TMP1(COL,ROW,LAY,120)   ! NH3
                  ctmp(col,row,lay,21) = TMP1(COL,ROW,LAY,55)    ! ISOP
                  ctmp(col,row,lay,22) = TMP1(COL,ROW,LAY,59)    ! SULF
                  ctmp(col,row,lay,23) = TMP1(COL,ROW,LAY,57)*0.5    ! TERP (=a-pinene group)
                  ctmp(col,row,lay,24) = TMP1(COL,ROW,LAY,57)*0.5    ! TERP (=limonene group)
! ***
! *** Define PM indices as starting from ncg
! *** SO4
                  ctmp(col,row,lay,ncg+2) = TMP1(COL,ROW,LAY,71)+TMP1(COL,ROW,LAY,72)
! *** SOA
                  ctmp(col,row,lay,ncg+3) = TMP1(COL,ROW,LAY,77)+TMP1(COL,ROW,LAY,78)+TMP1(COL,ROW,LAY,79)+ &
                                            TMP1(COL,ROW,LAY,80)+TMP1(COL,ROW,LAY,81)+TMP1(COL,ROW,LAY,82)+ &
                                            TMP1(COL,ROW,LAY,83)+TMP1(COL,ROW,LAY,84)+TMP1(COL,ROW,LAY,85)+ &
                                            TMP1(COL,ROW,LAY,86)+TMP1(COL,ROW,LAY,87)+TMP1(COL,ROW,LAY,88)+ &
                                            TMP1(COL,ROW,LAY,89)+TMP1(COL,ROW,LAY,90)+TMP1(COL,ROW,LAY,91)+ &
                                            TMP1(COL,ROW,LAY,117)+TMP1(COL,ROW,LAY,118)+TMP1(COL,ROW,LAY,119)
! *** PM-rest (nh4, ec, orgc, orgpa, na, cl, a25j)
                  ctmp(col,row,lay,ncg+4) = TMP1(COL,ROW,LAY,73)+TMP1(COL,ROW,LAY,74)+ &
                                            TMP1(COL,ROW,LAY,92)+TMP1(COL,ROW,LAY,93)+ &
                                            TMP1(COL,ROW,LAY,94)+TMP1(COL,ROW,LAY,95)+TMP1(COL,ROW,LAY,96)+ &
                                            TMP1(COL,ROW,LAY,108)+TMP1(COL,ROW,LAY,109)+TMP1(COL,ROW,LAY,110)+ &
                                            TMP1(COL,ROW,LAY,97)
! *** Coarse mode dust
                  ctmp(col,row,lay,ncg+5) = TMP1(col,row,lay,98)+TMP1(col,row,lay,99)
! *** Coarse mode sea-salt
                  ctmp(col,row,lay,ncg+6) = TMP1(col,row,lay,111)+TMP1(col,row,lay,112)
! *** NO3
                  ctmp(col,row,lay,ncg+7) = TMP1(col,row,lay,75)+TMP1(col,row,lay,76)

! *** PM2.5 (=ncg+1)
                  ctmp(col,row,lay,ncg+1) = ctmp(col,row,lay,ncg+2)+ctmp(col,row,lay,ncg+3)+  &
                                            ctmp(col,row,lay,ncg+4)+ctmp(col,row,lay,ncg+7)

                end do
              end do
              z_cq(lay) = TMP2(1,1,LAY,9)     ! ZH 
            end do

            !print *,pa(1,1,:)

! *** CONVERT CMAQ CONCENTRATION UNITS
! ***   CMAQ uses ppmV for gases and ug/m3 for PM
! ***   CityChem uses ug/m3 for gases and PM

! *** Gases
            do lay = 1, nzcmaq
              do col=1, ncols
                do row = 1, nrows
                  do i=1, ncg
                    ctmp(col,row,lay,i) = (pa(col,row,lay) * mw(i)  / (rgas*ta(col,row,lay)) ) * ctmp(col,row,lay,i)
                  end do
                end do
              end do
            end do

            !print *,ctmp(1,1,:,19)

! *** INTERPOLATING VERTICAL LAYERS --> CITYCHEM LAYERS
            do y = 1, ny+ 2*nbcy      !y-direction =row
              do x = 1, nx+ 2*nbcx    !x-direction =col

! *** First layer value assumed identical:

                do i=1,ncp
                  bconc(i,x,y,1) =  dble( ctmp(x,y,1,i) )
                end do

! *** Loop through CITYCHEM layers nz_ep-1
                do k = 2,nz

                  !if((y==1).and.(x==1)) print *,'k ',k

                  layer_found = .false.

                  do nh = 1,nzcmaq-1   ! cmaq layers

                    if ( ( z_ep(k) .le. z_cq(nh) ) .and. (z_ep(k) .gt. z_cq(nh-1)) .and. .not.layer_found ) then
                      w_hi = (z_ep(k) - z_cq(nh-1) )/(z_cq(nh) - z_cq(nh-1))
                      w_lo = 1.0 - w_hi
                      layer_found = .true.

!MSK START CHANGE Apr 2019

                      do i=1,ncp
                        bconc(i,x,y,k)  = w_hi * dble(ctmp(x,y,nh,i))  + w_lo * dble(ctmp(x,y,nh-1,i))
                      end do

                  end if
                end do  ! loop over cmaq layers

                end do     ! vertical loop

! *** Correction of Ozone and NOx and PM2.5, use 7th EPISODE layer in ground layer
! *** O3, NO, NO2
                bconc(1,x,y,1) =  bconc(1,x,y,7)
                bconc(2,x,y,1) =  bconc(2,x,y,7)
                bconc(3,x,y,1) =  bconc(3,x,y,7)
! *** PM2.5
                bconc(ncg+1,x,y,1) =  bconc(ncg+1,x,y,7)
! *** SO4
                bconc(ncg+2,x,y,1) =  bconc(ncg+2,x,y,7)
! *** SOA
                bconc(ncg+3,x,y,1) =  bconc(ncg+3,x,y,7)
! *** PM2.5-rest
                bconc(ncg+4,x,y,1) =  bconc(ncg+4,x,y,7)
! *** DUST_c
                bconc(ncg+5,x,y,1) =  bconc(ncg+5,x,y,7)
! *** SEAS_c
                bconc(ncg+6,x,y,1) =  bconc(ncg+6,x,y,7)
! *** NO3
                bconc(ncg+7,x,y,1) =  bconc(ncg+7,x,y,7)

!MSK END CHANGE Apr 2019


! *** Last layer value (nz+nbcz) assumed identical:
                do i=1,ncp
                  bconc(i,x,y,nz+nbcz) = dble( ctmp(x,y,nzcmaq,i) )
                end do

              end do   !x
            end do      !y


! *** CONVERTING BCON COMPOUNDS --> CITYCHEM COMPOUNDS
! *** 1    2    3    4     5    6     7   8     9     10     
! *** o3   no   no2  h2o2  so2  form  co  n2o5  hno3  hono  
! *** 11   12   13   14    15    16   17   18   19     
! *** pan  xyl  etha ald2  etoh  par  eth  iole mgly
! *** 20   21   22   23    24 
! *** nh3  isop sulf apin  limo
! *** 25    26  27   28     29     30     31
! *** pm2.5 so4 soa  pmrest dust_c salt_c no3
! *** --------------------------------------------
! *** pmrest = PM2.5_prim+PM2.5_nh4

! *** PM10 (index: ncp, overwrites pmrest)
! *** PM10 = 1.5*PM2.5
            bconc(ncp,:,:,:) = 1.5 * bconc(ncg+1,:,:,:) 


! *** xbcon_2012PUT SECTION --> CITYCHEM BCON INPUT FILES

! *** Write 3D background concentration datalines

! wanted format
!      READ(UN,*) IX,JY,FLD(IC,I,J,K)

             write(txt1,'(A6,I4)')    '    H:',n

  
             do i=1,ncp
               do k = 1, nz+nbcz

                  write(txt2,'(A7,I3.3)') '_Lay_k=',k

                  call write2dfield(nx+2*nbcx,ny+2*nbcy,txt1,txt2,un( i ),EP_fm, bconc(i,:,:,k) )

              enddo
             enddo

! *** Write Date line

!           write (un,'(I4,4X,I4,4X,I4,4X,I4,4X,I4,4X,I4,4X)' )      year,month,day,hour,minu,secu

! *** Change date by one hour

             hour = hour + 1
             if ( hour == 24 ) then
              hour = 0
              day = day + 1
            endif
            if ( day > monlen(month) ) then
              day = 1
              month = month + 1
            endif

! END HOUR LOOP
        enddo  


! *** Close CMAQ bcon file to open next one 
!        close (unbc )
!
!        enddo  !input file loop

! *** Close BC files
        do i=1,ncp

            close(un(i) )
        enddo

! *** DEALLOCATE
      if (allocated( ctmp) )      deallocate( ctmp)
      if (allocated(   ta) )      deallocate(   ta)
      if (allocated(   pa) )      deallocate(   pa)
      if (allocated(bconc) )      deallocate(bconc)


        write(6,      '(A18)')  'Finished writing! '
!

! *** End write Dynamic 2d Fields ********************************

!      contains

     end program cmaq4cchem
