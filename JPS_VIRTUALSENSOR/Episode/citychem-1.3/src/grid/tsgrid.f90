! <tsgrid.f90 - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************! 
!* 
!* EPISODE - An urban-scale air quality model
!* ========================================== 
!* Copyright (C) 2018  NILU - Norwegian Institute for Air Research
!*                     Instituttveien 18
!*                     PO Box 100
!*                     NO-2027 Kjeller
!*                     Norway
!*
!*                     Contact persons: Gabriela Sousa Santos - gss@nilu.no
!*                                      Paul Hamer - pdh@nilu.no
!*
!* Unless explicitly acquired and licensed from Licensor under another license,
!* the contents of this file are subject to the Reciprocal Public License ("RPL")
!* Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!* allowed by the RPL, and You may not copy or use this file in either source code
!* or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!* All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!* WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!* DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!* See the RPL for specific language governing rights and limitations under the RPL.
!*
!* ========================================== 
!* The dispersion model EPISODE (Grønskei et. al., 1993; Larssen et al., 1994;
!* Walker et al., 1992, 1999; Slørdal et al., 2003, 2008) is an Eulerian grid model 
!* with embedded subgrid models for calculations of pollutant concentrations resulting 
!* from different types of sources (area-, line- and point sources). EPISODE solves the 
!* time dependent advection/-diffusion equation on a 3 dimensional grid. 
!* Finite difference numerical methods are applied to integrate the solution forward in time. 
!* It also includes extensions as the implementation of a simplified EMEP photochemistry 
!* scheme for urban areas (Walker et al. 2004) and a scheme for Secondary Organic Aerosol 
!* implemented by Håvard Slørdal
!*
!* Grønskei, K.E., Walker, S.E., Gram, F. (1993) Evaluation of a model for hourly spatial
!*    concentrations distributions. Atmos. Environ., 27B, 105-120.
!* Larssen, S., Grønskei, K.E., Gram, F., Hagen, L.O., Walker, S.E. (1994) Verification of 
!*    urban scale time-dependent dispersion model with sub-grid elements in Oslo, Norway. 
!*    In: Air poll. modelling and its appl. X. New York, Plenum Press.
!* Slørdal, L.H., McInnes, H., Krognes, T. (2008): The Air Quality Information System AirQUIS. 
!*    Info. Techn. Environ. Eng., 1, 40-47, 2008.
!* Slørdal, L.H., Walker, S.-E., Solberg, S. (2003) The Urban Air Dispersion Model EPISODE 
!*    applied in AirQUIS. Technical Description. NILU TR 12/2003. ISBN 82-425-1522-0.
!* Walker, S.E., Grønskei, K.E. (1992) Spredningsberegninger for on-line overvåking i Grenland. 
!*    Programbeskrivelse og brukerveiledning. Lillestrøm, 
!*    Norwegian Institute for Air Research (NILU OR 55/92).
!* Walker, S.E., Slørdal, L.H., Guerreiro, C., Gram, F., Grønskei, K.E. (1999) Air pollution 
!*    exposure monitoring and estimation. Part II. Model evaluation and population exposure. 
!*    J. Environ. Monit, 1, 321-326.
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a 
!*    simplified EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*****************************************************************************! 

      subroutine TSGRID(I)

! *** ThIS subroutine updates, calculates and writes grid model concentrations for one timestep.
! ----------------------------------------------------------------------------------
! Based on:
! Version Episode 5.5 (May29, 2012) prepared for BB-Stand-Alone
! Original source code of EPISODE by Sam-Erik Walker (NILU)
!
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
! ----------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!           2016  M. Karl: commented all emep69 SOA preprocessor definitions
!           2016  M. Karl: commented sub grid cell and nested subgrid treatment
!           2016  M. Karl: EPISODE newbc, newc and oldc files are not supported.
!           2016  M. Karl: new way to open and update BCONs
!           2017  M. Karl: restart with IC files possible
!           2017  M. Karl: c,dcdt,d2cdx2,d2cdy2,excz,bc defined as double precision
!
!           2019 Dec. Kang @ CARES, write out 3D emission data (instantanous) to .dat file
!!          here, two height (Z) are given: 
!                 Z(IZ) - vertical layer height, 
!                 Z_act - height after considering topograhpy
!           2020 Mar. Kang @ CARES, write out 3D emission data (including BC, instantanous)
!                Changes include:
!                1. C(NC,NX+2,NY+2,NZ+1)
!!               2. location of main grid point is changed from sw and top to central point 
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_grid
!MSK start
      use mod_writenc
!MSK end

!!DEC$ IF DEFINED (emep68)
!
! *** Additional EMEP1_SOA modules:
!
!      use OrganicAerosol_ml, only : OrganicAerosol,temp_soa, itemp_soa,
!     &        xn,
!     &        Grid_SOA_Fgas,Grid_avg_mw, 
!     &        Grid_M0,
!     &        aer_ug_OC,    aer_ugC_OC,    aer_ug_POM,  aer_ugC_POM,
!     &        aer_ug_WOOD,  aer_ugC_WOOD,  aer_ug_FFUEL,aer_ugC_FFUEL,
!     &        aer_ug_BGNDOC,aer_ugC_BGNDOC,
!     &        aer_ug_ASOA,  aer_ugC_ASOA,  aer_ug_BSOA, aer_ugC_BSOA,
!     &        my_last_call
!
!      use My_SOA_ml,         only : FIRST_NONVOL, LAST_NONVOL,
!     &                              FIRST_SOA, LAST_SOA, BGND_SPEC
!
!!DEC$ ENDIF

      implicit none
      
! *** Start subroutine declarations: **************************************************************

! *** Global variables:

      INTEGER :: I

! *** I - Indicator of pre (0) or post (1) processing

! *** Local variables:

      INTEGER :: IC
      INTEGER :: IX
      INTEGER :: IY
      INTEGER :: IZ
!MSK      LOGICAL :: ATDATE
!MSK      LOGICAL :: ATTIME

! *** IC     - Compound index
! *** IX     - X-direction index
! *** IY     - Y-direction index
! *** IZ     - Vertical layer index
! *** ATDATE - If at given date          then true else false
! *** ATTIME - If at given date and time then true else false

      real             :: CMINV
      real             :: molec2ug
      double precision :: HOR_VFLUX,HOR_MFLUX,VERT_VFLUX,VERT_MFLUX,  &
                        TOT_VFLUX,TOT_MFLUX

      integer :: AV_GRIDAI
      real :: rsum_DCDT  

!MSK start
!     Variables for netCDF output
      character(len=10) :: namefield
      character(len=10) :: unitname
      character(len=23) :: validity

      integer :: Nhh_in

      integer, dimension(4,1) :: mdate

      logical :: dopacking  = .false.
      logical :: dofloat    = .false.
      logical :: domirror   = .false.

      double precision        :: field3D(NX,NY,NZ)
      !double precision        :: field3D(NY,NX,NZ)
!!=====================================================================================
!! write out main grid emission data at last timestep of the simulation to .dat file
!! added by Kang @ CARES, Dec. 23, 2019
!!=====================================================================================
      double precision, allocatable :: field3D_output(:,:,:,:)
      integer :: II
      real,   allocatable :: mxm_i(:,:)
      real,   allocatable :: mxm_j(:,:)
      
!$$$$$$       real,   allocatable :: mxm_i_new(:,:)
!$$$$$$       real,   allocatable :: mxm_j_new(:,:)
!$$$$$$       real,   allocatable :: Znew(:)
!$$$$$$       real,   allocatable :: Znew0(:)      
! Allocate
        if (.not. allocated(mxm_i) )    allocate(mxm_i(NY,NX))
        if (.not. allocated(mxm_j) )    allocate(mxm_j(NY,NX))     
      


! Allocate
!$$$$$$         if (.not. allocated(mxm_i_new) )    allocate(mxm_i_new(NY+2*NBCY,NX+2*NBCX))
!$$$$$$         if (.not. allocated(mxm_j_new) )    allocate(mxm_j_new(NY+2*NBCY,NX+2*NBCX))    
!$$$$$$         if (.not. allocated(Znew) )    allocate(Znew(NZ+NBCZ))   
!$$$$$$         if (.not. allocated(Znew0) )    allocate(Znew0(NZ))                                      
!!=====================================================================================      
!MSK end

!!DEC$ IF DEFINED (emep68)
! ***  Local EMEP1_SOA-variables
!
!      logical, parameter :: DEBUG = .true.
!      real               :: ppb   = 2.5e10  ! ????
!
!!DEC$ ENDIF


! *** End subroutine declarations. ****************************************************************

      if (averaged_output) then
        AV_GRIDAI = GRIDAI
      else
        AV_GRIDAI = 0
      endif

      if (messfe .AND. ATTIME(BDAT) .AND. ISH == 1 .AND. ITS == 1 .AND. I == 1) then
        write (messun,*)
        if (averaged_output) then
           write (messun,'(A38)') &
            'TSGRID: The CM-output is NTS-averaged.'
        else
           write (messun,'(A39)') &
            'TSGRID: The CM-output is Instantaneous.'
        endif
        write (messun,*)
      endif


! *************************************************************************************************
!_NEST_Start:
!
! *** IMETHOD: Horizontal interpolation method when calculating concentration values for the NEST 
! ***          domain (Declared in: mod_grid).
!
! ***   IMETHOD equal to 1 means: BILINEAR INTERPOLATION
! ***   IMETHOD equal to 2 means: BESSEL   INTERPOLATION
! ***   IMETHOD equal to 3 means: NEAREST GRIDPOINT VALUE
!
        IMETHOD  = 1

! *** IZMETHOD: Vertical interpolation method when calculating concentration values for the NEST
! ***           domian (Declared in: mod_grid).
!
! ***   IZMETHOD equal to 0 means: NO INTERPOLATION (i.e. is used when
! ***                              we have identical vertical resolution.)
! ***   IZMETHOD equal to 1 means: LINEAR INTERPOLATION

        IZMETHOD = 1

!_NEST_End.

! *************************************************************************************************

! *** Perform pre- or postprocessing calculations:
      IF (I .EQ. 0) GOTO 100
      IF (I .EQ. 1) GOTO 200

  100 CONTINUE

! *** Preprocessing calculations:
! *** ***************************

!!DEC$ IF DEFINED (emep68)
!
!! ***   The SOA-module is only applied if NC >= 68:
!
!!_TEST      if (NC >= 68 ) then
!
!        if (ATTIME(BDAT) .AND. ISH == 1 .AND. ITS == 1) then
!
!! ***     Allocate the temperature and concentration profile arrays used
!! ***     in OrganicAerosol at the start of the simulation:
!
!! ***     The three arrays below was originally declared in: module SOA_Inputs_ml.
!          if (.NOT. allocated(temp_soa))  allocate(temp_soa(1:nz))
!          if (.NOT. allocated(itemp_soa)) allocate(itemp_soa(1:nz))
!          if (.NOT. allocated(xn))        allocate(xn(1:LAST_SOA,1:nz))
!
!! ***     The three arrays below should be available outside of 
!! ***     the SOA-module.
!!          if (.NOT. allocated(Grid_SOA_Fgas)) 
!!     &      allocate(Grid_SOA_Fgas(FIRST_SOA:LAST_SOA,nx,ny,1:nz))
!!          if (.NOT. allocated(Grid_avg_mw))   
!!     &      allocate(Grid_avg_mw(nx,ny,1:nz))
!
!          if (.NOT. allocated(Grid_M0)) allocate(Grid_M0(nx,ny,1:nz))
!
!! ***     The compound numbers below are declared in 'mod_conc.for':
!! ***     See: subroutine SendConcCompoundsData(pinc,piacmpnd,piacunit,piacout,pxacmolw,pxathalf)
!!
!! ***     The numbers below are not equal to "icmpnd(ic)", but instead: "icmpnd(ic) - 4"
!          ic_org_PM2p5  = LAST_SOA      + 1
!          ic_aerOM      = ic_org_PM2p5  + n_advect
!          ic_aerOM_C    = ic_aerOM      + 1
!          ic_aerPOM     = ic_aerOM_C    + 1
!          ic_aerPOM_C   = ic_aerPOM     + 1
!          ic_aerWOOD    = ic_aerPOM_C   + 1
!          ic_aerWOOD_C  = ic_aerWOOD    + 1
!          ic_aerFFUEL   = ic_aerWOOD_C  + 1
!          ic_aerFFUEL_C = ic_aerFFUEL   + 1
!          ic_aerBGND    = ic_aerFFUEL_C + 1
!          ic_aerBGND_C  = ic_aerBGND    + 1				 
!          ic_aerASOA    = ic_aerBGND_C  + 1
!          ic_aerASOA_C  = ic_aerASOA    + 1
!          ic_aerBSOA    = ic_aerASOA_C  + 1	  
!          ic_aerBSOA_C  = ic_aerBSOA    + 1	    
!   
!        endif  ! if (ATTIME(BDAT) .AND. ISH == 1 .AND. ITS == 1).
!        
!_TEST      endif  ! if (NC >= 68 ).
!
!!DEC$ ENDIF (emep68)

! *** UPDATE BOUNDARY CONDITIONS:
! *** ***************************
! 
! *** Standard practice: Updating the BC array once every hour
! *** or whenever ITS = 1 (for instance each half-hour):
!
!MSK: new BC values
!MSK:  Note: rbcon opens the netcdf files for main grid C output

      IF (ITS .EQ. 1)  call rbcon

!MSK start debug
!debug          do IZ=1,NZ+NBCZ
!debug            print *,'tsgrid BC(Z) ', IZ, BC(1,2,2,IZ)
!debug          enddo
!MSK end debug

! *** TEST POSSIBILITIES:
!

!MKS start
!MSK: FIRST TIME STEP: USE BC(:,:,:,:) AS INITIAL CONDITION FOR C(:,:,:,:)

      IF (ATTIME(BDAT) .AND. ITS .EQ. 1) THEN

         DO IC = 1,NC
           DO IZ=1,NZ+NBCZ
             DO IY=1,NY+2*NBCY
               DO IX=1,NX+2*NBCX
                 C(IC,IX,IY,IZ) = BC(IC,IX,IY,IZ)
               ENDDO
             ENDDO
           ENDDO
         ENDDO

      ENDIF

!MSK start debug
!debug           do IZ=1,NZ+NBCZ
!debug             print *,'tsgrid C(Z) 1', IZ, C(1,2,2,IZ)
!debug           enddo
!MSK end debug
!MSK end

!MSK:
! *** Only the main grid extent border is filled with BC values
!     BC(NC, NX+2*NBCX, NY+2*NBCY, NZ+NBCZ)
!     C(NC, NX+2*NBCX, NY+2*NBCY, NZ+NBCZ)
!     C(1,1) is same as BC(1,1)

!Fill the NZ+NBCZ layer
!        DO IX=1,NX+2*NBCX
!          DO IY=1,NY+2*NBCY
!            C(IC, IX, IY, NZ+NBCZ)     = BC(IC,IX,IY,NZ+NBCZ)
!          ENDDO
!        ENDDO
!Fill the East-West borders
!        DO K=1,NZ
!          DO IY=1,NY+2*NBCY
!            C(IC,  1, IY, K)      = BC(IC,  1, IY,K)
!            C(IC, NX+2*NBCX,IY,K) = BC(IC,NX+2*NBCX,IY,K)
!          ENDDO
!        ENDDO
!Fill the North-South borders
!        DO K=1,NZ
!          DO IX=1,NX+2*NBCX
!            C(IC, IX,  1, K)      = BC(IC,IX,  1,K)
!            C(IC, IX,NY+2*NBCY,K) = BC(IC,IX, NY+2*NBCY,K)
!          ENDDO
!        ENDDO
!
!MSK end


! *** Updating the boundary conditione every timestep:
!     CALL RBCON
!
! *** The IF-test below is activated if an analytical boundary solution is
! *** specified at the beginning of the simulation, and is zero thereafter: 
!     IF (ATTIME(BDAT) .AND. ITS .EQ. 1)THEN
! ***   The three lines below can be activated alone if the analytic boundary 
!       solution is to be updated every timestep.
!       DO IC = 1,NC
!         CALL AN_BCON(IC,0)
!       ENDDO
!     ELSE
!       DO IC = 1,NC
!         DO IZ=1,NZ+NBCZ
!           DO IY=1-NBCY,NY+NBCY
!             DO IX=1-NBCX,NX+NBCX
!               BC(IC,IX,IY,IZ) = 0.0
!             ENDDO
!           ENDDO
!         ENDDO
!       ENDDO
!     ENDIF
! *************************************************************************************************
!
! *** SPECIFY INITIAL CONDITIONS:
! *** ***************************
! *** Specify old grid model concentrations (from a previous simultion) at the beginning 
! *** of the simuation:

      IF (ATTIME(BDAT) .AND. ISH .EQ. 1 .AND. ITS .EQ. 1) THEN

!MSK start
! ***   For RESTART: get initial concentrations from IC restart file
! ***   Replaces roldc from EPISODE

               call ricon




!!!!!!!!!!!!!!!!!!! for testing 3D concentration output file by Kang @ Dec.2, 2019
!             if(restart .eq. 1) then
!                open(unit=1113,file="../OUTPUT/3D_field_new.txt")  !!
!                open(unit=1112,file="../OUTPUT/3D_field_para.txt")  !!   
!              do 350 IC=1,NC    
!               if (IC .eq. 3) then        
!                do 351 IZ=1,NZ
!                  do 352 IX = 1,NX
!                    do 353 IY = 1,NY

                     !!  field3D(IX,IY,IZ) = C(3,IX+1,IY+1,IZ)
                  !     field3D(IY,IX,IZ) = C(IC,IX+1,IY+1,IZ)
!                      write(1113,*) C(3,IX+1,IY+1,IZ)
!  351               continue
!  352             continue
!  353           continue
!               endif  !!! end if of IC=3
!  350          continue

!                 write(1112,*)"dump out 3D concentration file, NX=",NX,", NY=", NY, "NZ=",NZ, "Nhh_in=",Nhh_in, "mdate=",mdate, ", validity=",validity, ", dopacking=",dopacking,", dofloat=", dofloat, ", domirror=",domirror
               
!                close(1113)
!                close(1112)
!            endif
!!!!!!!!!!!!!!!!!!! end for testing 3D concentration output file by Kang @ Dec.2, 2019



!MSK start debug
!debug           do IZ=1,NZ+NBCZ
!debug             print *,'tsgrid C(Z) 2', IZ, C(1,2,2,IZ)
!debug           enddo
!MSK end debug
!MSK end

! ***  Calculate the concentrations for the NEST grid domain. These values are calculated
! ***  for the entire NEST grid, and are stored in the array:
!
! ***       BC_nest(NC,1-nbcx_nest:nx_nest+nbcx_nest,
! ***      &           1-nbcy_nest:ny_nest+nbcy_nest,
! ***      &                       nz_nest+nbcz_nest))

!MSK start  New subgrid BC values, only wanted if there are NEWBC files
!MSK     Now commented subrid BC and IC files
!MSK        if (NEWBCFE( 1 )) then
!MSK
!MSK          CALL WNEWBC
!MSK
!MSK       endif
!MSK end

!MSK start 
! *** Subgrid nests are currently not used, commented below
! ***   Store the initial BC_nest variables in the array. Needed since the values 
! ***   of BC_nest will be overwritten at the end of the present hour in the post-
! ***   processing treatment within this routine. 
!
! ***       IC_BC_nest(nc,1-nbcx_nest:nx_nest+nbcx_nest,
! ***      &              1-nbcy_nest:ny_nest+nbcy_nest,
! ***      &                          nz_nest+nbcy_nest)
        
!MSK        IC_BC_nest = BC_nest

! ***   Store the internal values of IC_BC_nest in the array:
!
! ***       IC_C_nest(nc,nx_nest,ny_nest,nz_nest)
!MSK
!MSK        do ic = 1,nc
!MSK          do iz = 1,nz_nest
!MSK            do iy = 1,ny_nest
!MSK              do ix = 1,nx_nest
!MSK                IC_C_nest(ic,ix,iy,iz) = IC_BC_nest(ic,ix,iy,iz)
!MSK              enddo
!MSK            enddo
!MSK          enddo
!MSK        enddo

!MSK ***   Zero initialisation of the not used subgrid nest fields
        BC_nest(:,:,:,:) = 0.0
        IC_C_nest(:,:,:,:)  = 0.0
        IC_BC_nest(:,:,:,:) = 0.0
!MSK end


      ENDIF



!     Run model OK?

      IF (.NOT. RUNOK) THEN

!       Set grid model concentrations to missing data:

        print *,'tsgrid not runok'

        DO IC = 1,NC

          IF (C(IC,1,1,1) .EQ. MISS) THEN

!_NEST_Start:

            DO IZ=1,NZ+NBCZ
!MSK              DO IY=1-NBCY,NY+NBCY
!MSK                DO IX=1-NBCX,NX+NBCX
              DO IY=1,NY+2*NBCY
                DO IX=1,NX+2*NBCX
                  C(IC,IX,IY,IZ) = MISS
                ENDDO
              ENDDO
            ENDDO 
!_NEST_End.
				  
          ENDIF

        END DO

!MSK !       Write new grid model concentrations
!MSK
!MSK        IF (ATDATE(EDAT) .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN
!MSK
!MSK          print *,'new conc file: ', NEWCFN(1)
!MSK          if ( NEWCFE( 1 ) ) then
!MSK
!MSK              call wnewc
!MSK
!MSK          endif
!MSK
!MSK          IF (MESSFE) WRITE (MESSUN,'(A)')   &
!MSK            'TSGRID: Finished call to: WNEWC'
!MSK        ENDIF
!MSK

        RETURN

      ENDIF  ! IF (.NOT. RUNOK) 



!     If missing data then grid model concentrations is set equal to
!     background concentrations or if these are also missing set equal
!     to zero.

      DO IC = 1,NC

!       If missing data then set equal to background concentration:

        IF (C(IC,1,1,1) .EQ. MISS) THEN

!_NEST_Start:
          IF (MESSFE) WRITE (MESSUN,'(A67)')   &
            'TSGRID WARNING: UNDEF values calculated, background values applied.'

          DO IZ=1,NZ+NBCZ
!MSK index null not allowed
!MSK            DO IY=1-NBCY,NY+NBCY
!MSK              DO IX=1-NBCX,NX+NBCX
            DO IY=1, NY+2*NBCY
              DO IX=1, NX+2*NBCX
                C(IC,IX,IY,IZ) = BC(IC,IX,IY,IZ)
              ENDDO
            ENDDO
          ENDDO

!_NEST_End.

        ENDIF

!       If missing data then set equal to zero:

        IF (C(IC,1,1,1) .EQ. MISS) THEN

!_NEST_Start.
          IF (MESSFE) WRITE (MESSUN,'(A73)')  &
            'TSGRID WARNING: UNDEF values found in background, minimum values applied.'
     
          CMINV = 0.
          IF (CUNIT(IC) .EQ. 'mol/cm3') CMINV = 1.E-20

          DO IZ=1,NZ+NBCZ
!MSK index null not allowed
!MSK            DO IY=1-NBCY,NY+NBCY
!MSK              DO IX=1-NBCX,NX+NBCX
            DO IY=1, NY+2*NBCY
              DO IX=1, NX+2*NBCX
                C(IC,IX,IY,IZ) = CMINV
              ENDDO
            ENDDO
          ENDDO
!_NEST_End.

        ENDIF

      END DO


!MSK start debug
!debug          do IZ=1,NZ+NBCZ
!debug             print *,'tsgrid C(Z) 3', IZ, C(1,2,2,IZ)
!debug          enddo
!debug          print *,'tsgrid C before adv/dif ',C(1,2,2,1)
!MSK end debug

! *** Alternate direction of advection-diffusion operators every timestep:

      IF (MOD(ITS,2) .EQ. 1) THEN

! ***   Calculate vertical advection and diffusion:
!
! ***   At the moment:  
! ***   NC       = 84 Total number of species in the model.
! ***   n_nochem = 16 Number of species not included in the chemistry.
! ***                 (Thus: 84 - 16 = 68 species included in the chemistry).
! ***   n_advect =  2 Number of n_nochem species that are advected.
! ***                 (these 2 are: 'ORGPM2p5' and  'ECfi'.) 


        DO IC = 1,NC - n_nochem + n_advect


          if (messfe .AND. (ITS == 1 .OR. ITS == NTS) ) then
            write(messun,'(A,I4)') 'TSGRID: Preprocessing timestep ITS = ',ITS
              rsum_DCDT     = SUM(DCDT(IC,:,:,:))
           write(messun,'(A,I2,A,E12.5)')   &
              'TSGRID: Domain sum DCDT(IC=',IC,',:,:,:) = ',rsum_DCDT
          endif


          IF (GRIDVS(IC) .EQ. 0) THEN

! ***       No vertical advection or diffusion applied.
! ***       Then the emissions are introduced directly into 
! ***       the appropriate grid cells.

            DO IZ = 1,NZ
              DO IY = 1,NY
                DO IX = 1,NX
!MSK                  C(IC,IX,IY,IZ) = C(IC,IX,IY,IZ) + DCDT(IC,IX,IY,IZ)*DT
                  C(IC,IX+1,IY+1,IZ) = C(IC,IX+1,IY+1,IZ) +  &
                                       DCDT(IC,IX,IY,IZ)*DT
                END DO
              END DO
            END DO


          ELSEIF (GRIDVS(IC) .EQ. 1) THEN

! ***       Applying the original combined vertical (upstream) advection
! ***       and explicit diffusion scheme:

              call adiv(ic)

          ELSEIF (GRIDVS(IC) .EQ. 2) THEN

! ***       Vertical diffusion by the explicit diffusion scheme:
              call v_dif(ic)

! ***       Vertical advection by the upstream scheme:
              call v_adv(ic)
            
          ELSEIF (GRIDVS(IC) .EQ. 3) THEN

! ***       Vertical diffusion by the implicit C-N diffusion scheme:
!MSK debug start pnc check
!debug       if((ic.eq.21).or.(ic.eq.22).or.(ic.eq.23) )then
!debug         print *,'tsgrid DCDT before vert diff', IC, DCDT(IC,12,10,1),C(IC,13,11,1)
!debug       endif
!MSK debug

              call difv_cn(ic)

!MSK debug start pnc check
!debug       if((ic.eq.21).or.(ic.eq.22).or.(ic.eq.23) )then
!debug         print *,'tsgrid DCDT before vert adv', IC, DCDT(IC,12,10,1),C(IC,13,11,1)
!debug       endif
!MSK debug

! ***       Vertical advection by the upstream scheme:
              call v_adv(ic)

!MSK debug start pnc check
!debug       if((ic.eq.21).or.(ic.eq.22).or.(ic.eq.23) )then
!debug         print *,'tsgrid DCDT after vert adv', IC, DCDT(IC,12,10,1),C(IC,13,11,1)
!debug       endif
!MSK debug

          ELSEIF (GRIDVS(IC) .EQ. 4) THEN

! ***       Only applying vertical diffusion by the implicit 
! ***       C-N diffusion scheme:
              call difv_cn(ic)

          ELSEIF (GRIDVS(IC) .EQ. 5) THEN

! ***       Only applying vertical advection by the upstream scheme:

! ***       This should only be applied for test purposes
! ***       when no grid emissions are included.
              call v_adv(ic)

          ENDIF


         END DO  ! IC = 1,NC - n_nochem + n_advect

      ! print *,'tsgrid C after vertical ',C(1,2,2,1)

!_LHS_SOA_May_2007_End.

! ***   Calculate horisontal 2D advection:

          call advh

      ! print *,'tsgrid C after horizontal adv ',C(1,2,2,1)

! ***   Calculate horisontal 2D diffusion:

          call difh


       ! print *,'tsgrid C after horizontal dif ',C(1,2,2,1)


! ***   Calculate grid model output concentration and deposition

!MSK *** This is the connection between Eulerian grid concentration C
!MSK *** and the local main grid, receptor point, line receptor
!MSK *** and subgrid concentrations CM, CR, CLR, CS

! ***   ... in the main grid cells:
! ***       (At present: zero deposition calculated in CSUBGM.)

        call csubgm(AV_GRIDAI,GRIDAI,GRIDAI)

! ***   ... in the sub  grid cells
! ***       (At present: zero deposition calculated in CSUBGS.)

!MSK        CALL CSUBGS(AV_GRIDAI,GRIDAI,GRIDAI)

! ***   ... in the irregular receptor points
! ***       (At present: zero deposition calculated in CSUBGR.)

        call csubgr(AV_GRIDAI,GRIDAI,GRIDAI)

! ***   ... in the line source associated receptor points
! ***       (At present: zero deposition calculated in CSUBGL.)

        call csubgl(AV_GRIDAI,GRIDAI,GRIDAI)

! ***   ... in the grid model

        !print *,'tsgrid C after csub ',C(1,2,2,1)

! ***    The routine below facilitate the inclusion of dry- and
! ***    wet-deposition as a separate process instead of being 
! ***    introduced in the vertical diffusion routines or in
! ***    the chemistry scheme:

        call gmdepo

        !print *,'gmphot: after gmdepo'
        !print *,'tsgrid C after gmdepo ',C(1,2,2,1)

!       Calculate grid model radioactive decay:

        call gmradi

        !print *,'tsgrid: after gmradi'
        !print *,'tsgrid C after gmradi ',C(1,2,2,1)

! ***   Calculate grid model photochemical reactions:

!_EMEP03	  IF (MESSFE) WRITE (MESSUN,*) 'TSGRID: Next action CALL GMPHOT.'

         call gmphot

        !print *,'tsgrid: after gmphot',C(1,2,2,1)


! ***   The SOA-module is applied if NC >= 68:
           
!!DEC$ IF DEFINED (emep68)
!
!!_TEST        IF (NC >= 68 ) THEN
!
!! ***   SOA-MODULE is to be called here. The Chemistry calculations
!! ***   have been made in SUBROUTINE GMPHOT and immediately after
!! ***   the partitioning calculations should be performed.
!
!          DO IX = 1,NX
!            DO IY = 1,NY
!
!! ***         For the present TEST setup the variable setting below 
!! ***         could have been moved outside of the IX,IY - DO-LOOP.
!! ***         However, for real applications these variables will be
!! ***         updated from their 3-D model variables.
!
!              temp_soa(1:nz)  = tair_soa(IX,IY,1:nz)      ! 298.0
!              itemp_soa(1:nz) = INT(temp_soa(1:nz)+0.5)
!
!!_SOA        [xn] is concentrations in: molecules/cm3.
!              xn(FIRST_NONVOL:LAST_SOA,1:nz) = 
!     &           C(FIRST_NONVOL:LAST_SOA,IX,IY,1:nz)   ! 0.1 * ppb ?
!
!!             Update the total Organic_PM_mass_conc (ug/m3) after
!!             having performed the advection and diffusion processes:
!!             If the line below is commented out we do not transport the
!!             total organic PM mass concentration.
!
!              Grid_M0(IX,IY,1:nz) = C(ic_org_PM2p5,IX,IY,1:nz)
!
!              call OrganicAerosol(DEBUG,IX,IY)
!
!!              IF (MESSFE) WRITE (MESSUN,'(A46,I3,A,I3,A)') 
!!     &          ' OrganicAerosol has been completed for I,J = ('
!!     &         ,IX,',',IY,')'
!
!              C(ic_org_PM2p5,IX,IY,1:nz)  = Grid_M0(IX,IY,1:nz)
!              C(ic_aerOM,IX,IY,1:nz)      = aer_ug_OC(1:nz)
!              C(ic_aerPOM,IX,IY,1:nz)     = aer_ug_POM(1:nz)
!              C(ic_aerWOOD,IX,IY,1:nz)    = aer_ug_WOOD(1:nz)
!              C(ic_aerFFUEL,IX,IY,1:nz)   = aer_ug_FFUEL(1:nz)
!              C(ic_aerBGND,IX,IY,1:nz)    = aer_ug_BGNDOC(1:nz)
!              C(ic_aerASOA,IX,IY,1:nz)    = aer_ug_ASOA(1:nz)
!              C(ic_aerBSOA,IX,IY,1:nz)    = aer_ug_BSOA(1:nz)
!
!              C(ic_aerOM_C,IX,IY,1:nz)    = aer_ugC_OC(1:nz)
!              C(ic_aerPOM_C,IX,IY,1:nz)   = aer_ugC_POM(1:nz)
!              C(ic_aerWOOD_C,IX,IY,1:nz)  = aer_ugC_WOOD(1:nz)
!              C(ic_aerFFUEL_C,IX,IY,1:nz) = aer_ugC_FFUEL(1:nz)
!              C(ic_aerBGND_C,IX,IY,1:nz)  = aer_ugC_BGNDOC(1:nz)
!              C(ic_aerASOA_C,IX,IY,1:nz)  = aer_ugC_ASOA(1:nz)
!              C(ic_aerBSOA_C,IX,IY,1:nz)  = aer_ugC_BSOA(1:nz)
!
!            END DO
!          END DO
!
!!_TEST        ENDIF !(NC >= 68)
!
!! ***   SOA-MODULE is ended here.
!
!!DEC$ ENDIF

!_LHS_SOA_June_2007_End.

      ELSE

! ***   Calculate grid model photochemical reactions

!_LHS_SOA_May_2007_Start:
!
! ***   Commented out since the timestep is set to 2 * DT within 
! ***   SUBROUTINE GMPHOT at the end of the previous timestep.
!
! ***       CALL GMPHOT
!
!_LHS_SOA_May_2007_End.
!
!
!       Calculate grid model radioactive decay

        call gmradi

!debug      print *,'tsgrid C after gmradi alt  ',C(1,2,2,1)


!MSK *** This is the connection between Eulerian grid concentration C
!MSK *** and the local main grid, receptor point, line receptor
!MSK *** and subgrid concentrations CM, CR, CLR, CS

!       Calculate grid model output concentration and deposition

! ***   ... in the main grid cells

        call csubgm(AV_GRIDAI,GRIDAI,GRIDAI)


! ***   ... in the sub  grid cells

!MSK        CALL CSUBGS(AV_GRIDAI,GRIDAI,GRIDAI)

! ***   ... in the irregular receptor points

        call csubgr(AV_GRIDAI,GRIDAI,GRIDAI)


! ***   ... in the line source associated receptor points

        call csubgl(AV_GRIDAI,GRIDAI,GRIDAI)

!_LHS_Hourly_Averaged_Output_December_2007_End.

!       ... in the grid model

!       PRINT *,'GMDEPO ',ITS

      !print *,'tsgrid C after csub alt  ',C(1,2,2,1)


        call gmdepo


      !print *,'tsgrid C after gmdepo alt  ',C(1,2,2,1)

!       Calculate vertical advection and diffusion


        DO IC = 1,NC - n_nochem + n_advect

          if (messfe .AND. (ITS == 1 .OR. ITS == NTS) ) then
            write(messun,'(A,I4)') 'TSGRID: Preprocessing timestep ITS = ',ITS
              rsum_DCDT     = SUM(DCDT(IC,:,:,:))
            write(messun,'(A,I2,A,E12.5)')  &
              'TSGRID: Domain sum DCDT(IC=',IC,',:,:,:) = ',rsum_DCDT
          endif


!MSK start   avoid tiny concentration values
          do IZ=1,NZ+NBCZ
            do IY=1, NY+2*NBCY
              do IX=1, NX+2*NBCX
                if ( C(IC,IX,IY,IZ) .lt. 1.e-20)  C(IC,IX,IY,IZ) = 0.0
              enddo
            enddo
          enddo
!MSK end


          IF (GRIDVS(IC) .EQ. 0) THEN

!           Introduce emissions directly into the grid when 
!           there is no vertical operator

            DO IZ = 1,NZ
              DO IY = 1,NY
                DO IX = 1,NX
!MSK                  C(IC,IX,IY,IZ) = C(IC,IX,IY,IZ) + DCDT(IC,IX,IY,IZ)*DT
                  C(IC,IX+1,IY+1,IZ) = C(IC,IX+1,IY+1,IZ) +    &
                                       DCDT(IC,IX,IY,IZ)*DT
                END DO
              END DO
            END DO

          ELSEIF (GRIDVS(IC) .EQ. 1) THEN
              call adiv(ic)

          ELSEIF (GRIDVS(IC) .EQ. 2) THEN
              call v_dif(ic)
              call v_adv(ic)

          ELSEIF (GRIDVS(IC) .EQ. 3) THEN

              call difv_cn(ic)
              call v_adv(ic)


          ELSEIF (GRIDVS(IC) .EQ. 4) THEN
              call difv_cn(ic)

          ELSEIF (GRIDVS(IC) .EQ. 5) THEN
! ***       This should only be applied for test purposes
! ***       when no grid emissions are included.
              call v_adv(ic)
          ENDIF

        END DO

      !print *,'tsgrid C after vertical alt  ',C(1,2,2,1)

!_LHS_SOA_May_2007_End.

!       Calculate horisontal 2D diffusion

!       PRINT *,'DIVH ',ITS
          call difh

      !print *,'tsgrid C after hor dif alt  ',C(1,2,2,1)

!       Calculate horisontal 2D advection

!       PRINT *,'*ADVH ',ITS,MOD(ITS,2)
          call advh
!       PRINT *,'*END ',ITS

      !print *,'tsgrid C after hor adv alt  ',C(1,2,2,1)


      ENDIF


!     If NOT NTS-Averaging, calculate grid model concentrations.

      IF (ITS == NTS .AND. AV_GRIDAI == 0) THEN

!       ... in the main grid cells

        call csubgm(GRIDAI,0,0)

!MSK debug
!MSK debug        print *,'tsgrid CR(NO) after csubgm alt', CR(2,10000)

!       ... in the sub  grid cells

!MSK        CALL CSUBGS(GRIDAI,0,0)
          
!       ... in the irregular receptor points

        call csubgr(GRIDAI,0,0)

!MSK debug
!MSK debug        print *,'tsgrid CR(NO) after csubgr alt', CR(2,10000)

!       ... in the line source associated receptor points

        call csubgl(GRIDAI,0,0)

!debug      print *,'tsgrid C after csub alt  ',C(1,2,2,1)


      ENDIF

!     Finished with preprocessing

          !print *,'tsgrid: finished preprocessing'

      RETURN

  200 CONTINUE

!     Postprocessing calculations:
!     ****************************

!     If run model not ok then return

      IF (.NOT. RUNOK) RETURN


!_NEST_SIGMA_Start: *********  START FLUX-CALCULATIONS   *****************
!
      IF (INFOFE) THEN
!       Calculate total volume, total mass and volume averaged conc.:
!!MSK             CALL V_ADV(IC)  !buggy routine
!       The variables TOTVOL(MC), TOTMASS(MC) and VOLAVG(MC) are declared
!       in: /grid/mod_grid.for

        IF (ITS .EQ. NTS) THEN

          DO IC = 1,NC
 
            TOTVOL(IC)  = 0.0
            TOTMASS(IC) = 0.0
            VOLAVG(IC)  = 0.0

            WESTB_VF(IC) = 0.0
            WESTB_MF(IC) = 0.0

            EASTB_VF(IC) = 0.0
            EASTB_MF(IC) = 0.0

            NORTHB_VF(IC) = 0.0
            NORTHB_MF(IC) = 0.0

            SOUTHB_VF(IC) = 0.0
            SOUTHB_MF(IC) = 0.0

            TOPB_VF(IC) = 0.0
            TOPB_MF(IC) = 0.0

            BOTB_VF(IC) = 0.0
            BOTB_MF(IC) = 0.0

!           When multiplying C(IC,IX,IY,IZ) with molec2ug we
!           converts from [molecules/cm3] to [ug/m3].

            IF (CUNIT(IC)(1:3) .EQ. 'mol')THEN
!            IF(IC <= NC - n_nochem)THEN
                molec2ug = CMOLW(IC)*1.0E+12/avogad
            ELSE
                molec2ug = 1.0
            ENDIF


!MSK *** C in the FLUX calculation has been shifted by 1 in x- and y-direction

!	      PRINT *,' molec2ug = ',molec2ug
!           Calculates the total model volum and the total model mass:
            DO IZ = 1,NZ
              DO IY = 1,NY
               DO IX = 1,NX
!_Fine_Model domain	    DO IY = 8,16
!_Fine_Model domain          DO IX = 6,16

              TOTVOL(IC)  = TOTVOL(IC) + (VOL(IZ)*DEPTHM(IX,IY)/MOD_H)


              TOTMASS(IC) = TOTMASS(IC) +  &
               (C(IC,IX+1,IY+1,IZ)*molec2ug*VOL(IZ)*DEPTHM(IX,IY)/MOD_H )

                ENDDO
               ENDDO
              ENDDO

!           Calculates the Volume fluxes and the Mass fluxes out of the 
!           model boundaries:

!           The EAST/WEST-Boundaries:
               DO IZ=1,NZ
                  DO IY=1,NY
!_Fine_Model domain		  DO IY=8,16

               WESTB_VF(IC) = WESTB_VF(IC) +   &
                             U(1,IY,IZ)*DY*DZ(IZ)*DEPTHM(1,IY)/MOD_H
               WESTB_MF(IC) = WESTB_MF(IC) +    &
                             C(IC,2,IY+1,IZ)*molec2ug*  &
                             U(1,IY,IZ)*DY*DZ(IZ)*DEPTHM(1,IY)/MOD_H

!_NEST_NOTE: The sign for the turbulent flux is chosen so that this
!            transport is positive if the transport is directed in the 
!            positive x-direction (eastwards):
!             WESTB_MF(IC) = WESTB_MF(IC) + 
!     &          ( (D(IZ)*molec2ug*(BC(IC,0,IY,IZ) - C(IC,1,IY,IZ))/DX)*
!     &                      (DY*DZ(IZ)*DEPTHM(1,IY)/MOD_H) )


               EASTB_VF(IC) = EASTB_VF(IC) +   &
                             U(NX,IY,IZ)*DY*DZ(IZ)*DEPTHM(NX,IY)/MOD_H
               EASTB_MF(IC) = EASTB_MF(IC) +    &
                             C(IC,NX+1,IY+1,IZ)*molec2ug*   &
                             U(NX,IY,IZ)*DY*DZ(IZ)*DEPTHM(NX,IY)/MOD_H

!_NEST_NOTE: The sign for the turbulent flux is chosen so that this
!            transport is positive if the transport is directed in the 
!            positive x-direction (eastwards): 
!             EASTB_MF(IC) = EASTB_MF(IC) -
!     &        ( (D(IZ)*molec2ug*(BC(IC,NX+1,IY,IZ) - C(IC,NX,IY,IZ))/DX)*
!     &                   (DY*DZ(IZ)*DEPTHM(NX,IY)/MOD_H) )
              ENDDO
             ENDDO
	   
!            The NORTH/SOUTH-Boundaries:
             DO IZ=1,NZ
               DO IX=1,NX
!_Fine_Model domain		  DO IX=6,16
               NORTHB_VF(IC) = NORTHB_VF(IC) +    &
                              V(IX,NY,IZ)*DY*DZ(IZ)*DEPTHM(IX,NY)/MOD_H
               NORTHB_MF(IC) = NORTHB_MF(IC) +    &
                              C(IC,IX+1,NY+1,IZ)*molec2ug*   &
                              V(IX,NY,IZ)*DY*DZ(IZ)*DEPTHM(IX,NY)/MOD_H

!_NEST_NOTE: The sign for the turbulent flux is chosen so that this 
!            transport is positive if the transport is directed in the 
!            positive y-direction (northwards):
!             NORTHB_MF(IC) = NORTHB_MF(IC) -
!     &        ( (D(IZ)*molec2ug*(BC(IC,IX,NY+1,IZ) - C(IC,IX,NY,IZ))/DY)*
!     &                (DY*DZ(IZ)*DEPTHM(IX,NY)/MOD_H) )


               SOUTHB_VF(IC) = SOUTHB_VF(IC) +    &
                              V(IX,1,IZ)*DY*DZ(IZ)*DEPTHM(IX,1)/MOD_H
               SOUTHB_MF(IC) = SOUTHB_MF(IC) +    &
                              C(IC,IX+1,2,IZ)*molec2ug*   &
                              V(IX,1,IZ)*DY*DZ(IZ)*DEPTHM(IX,1)/MOD_H


!_NEST_NOTE: The sign for the turbulent flux is chosen so that this 
!            transport is positive if the transport is directed in the 
!            positive y-direction (northwards):
!             SOUTHB_MF(IC) = SOUTHB_MF(IC) +
!     &        ( (D(IZ)*molec2ug*(BC(IC,IX,0,IZ) - C(IC,IX,1,IZ))/DY) *
!     &                       (DY*DZ(IZ)*DEPTHM(IX,1)/MOD_H) ) 
               ENDDO
             ENDDO


!           The TOP/(Bottom)-Boundaries:
            DO IX=1,NX
              DO IY=1,NY
!_Fine_Model domain		DO IX=6,16
!_Fine_Model domain		  DO IY=8,16


!_SIGMA_NOTE: W(IX,IY,NZ) must be multiplied by DEPTHM(IX,IY)/MOD_H
!             in order to get the cartesian vertical velocity. None
!             of the other terms are necessary since at the top 
!             boundary sigma is equal to H_0 and the additional terms 
!             therefore vanish.
               TOPB_VF(IC) = TOPB_VF(IC) +    &
                            (W(IX,IY,NZ)*DX*DY*DEPTHM(IX,IY)/MOD_H)
               TOPB_MF(IC) = TOPB_MF(IC) +    &
                            C(IC,IX+1,IY+1,NZ)*molec2ug*   &
                            (W(IX,IY,NZ)*DX*DY*DEPTHM(IX,IY)/MOD_H)


!_SIGMA_NOTE:  In /mete/cdzdt.for DZDT(I,J,NZ) is multiplied with
!              square of the stretch factor, so we do not need to 
!              include this here. The sign below is chosen so that
!              upward (outward) flux is positive.
! 	       TOPB_MF(IC) = TOPB_MF(IC) +  DZDT(IX,IY,NZ) * 
!     &         ( molec2ug*(C(IC,IX,IY,NZ) - BC(IC,IX,IY,NZ+1)) *
!     &                     (DX*DY) )
!             BOTB_VF(IC) = BOTB_VF(IC) + W(IX,IY,1)*DX*DY
!             BOTB_MF(IC) = BOTB_MF(IC) + C(IC,IX,IY,1)*BOTB_VF(IC)

              ENDDO
             ENDDO

!            Horizontal, vertical and total Volume and Mass Flux 
!            INTO the model domain, i.e. THE INFLUX:

             HOR_VFLUX  = WESTB_VF(IC)  - EASTB_VF(IC)    &
                        + SOUTHB_VF(IC) - NORTHB_VF(IC) 

             HOR_MFLUX  = WESTB_MF(IC)  - EASTB_MF(IC)    &
                        + SOUTHB_MF(IC) - NORTHB_MF(IC) 



!	       VERT_VFLUX = BOTB_VF(IC) - TOPB_VF(IC)
!	       VERT_MFLUX = BOTB_MF(IC) - TOPB_MF(IC) 

             VERT_VFLUX = - TOPB_VF(IC)
             VERT_MFLUX = - TOPB_MF(IC)


!           TOTAL Volume and Mass transport INTO the model domain
!           i.e. The total INFLUX:

             TOT_VFLUX = HOR_VFLUX + VERT_VFLUX
             TOT_MFLUX = HOR_MFLUX + VERT_MFLUX

!           Giving the quantities as (km3/s) and as (kg/s) if
!           both are multipled by 1.0E-09: 
             EASTB_VF(IC) = EASTB_VF(IC) * 1.0E-09 
!            EASTB_MF(IC) = EASTB_MF(IC) * 1.0E-06
             EASTB_MF(IC) = EASTB_MF(IC) * 1.0E-09
             HOR_VFLUX = HOR_VFLUX  * 1.0E-09 
!            HOR_MFLUX = HOR_MFLUX  * 1.0E-06
             HOR_MFLUX = HOR_MFLUX  * 1.0E-09
             VERT_VFLUX = VERT_VFLUX * 1.0E-09
!            VERT_MFLUX = VERT_MFLUX * 1.0E-06
             VERT_MFLUX = VERT_MFLUX * 1.0E-09
             TOT_VFLUX = TOT_VFLUX * 1.0E-09
!            TOT_MFLUX = TOT_MFLUX * 1.0E-06
             TOT_MFLUX = TOT_MFLUX * 1.0E-09
		
             VOLAVG(IC) = TOTMASS(IC)/TOTVOL(IC)


!_Include the output below to the log file if necessary:		
!	        IF (MESSFE) WRITE (MESSUN,2002) IC,TOTMASS(IC)
!	        IF (MESSFE) WRITE (MESSUN,2003) IC,VOLAVG(IC)
!	        IF (MESSFE) WRITE (MESSUN,2004) IC,TOTVOL(IC)

!             Convert Volume from m**3 to Km**3:
             TOTVOL(IC) = TOTVOL(IC) * 1.0E-9
!             Convert total NOx Mass from ug to Kg:
             TOTMASS(IC) = TOTMASS(IC) * 1.0E-9

             WRITE (INFOUN,2005) CMPND(IC),IC,THOUR,   &
                    TOTVOL(IC),TOTMASS(IC),VOLAVG(IC),        &     
                    EASTB_MF(IC),HOR_MFLUX,VERT_MFLUX,TOT_MFLUX
!     &              EASTB_VF(IC),HOR_VFLUX,VERT_VFLUX,TOT_VFLUX


!
!			IF (MESSFE) WRITE (MESSUN,2067) D(1)
!			IF (MESSFE) WRITE (MESSUN,2068) D(2)
!			IF (MESSFE) WRITE (MESSUN,2069) D(3)

           ENDDO   ! DO IC = 1,NC 

         ENDIF  ! IF (ITS .EQ. NTS)

      ENDIF  !  IF (INFOFE)

!_NEST_SIGMA_End *************  END FLUX-CALCULATIONS   *******************************************

      IF (ISH == NSH .AND. ITS == NTS) THEN

! ***  Calculate the concentrations for the NEST grid domain at the end of each hour. 
! ***  These values are calculated for the entire NEST grid, and are stored in the array:
!
! ***       BC_nest(NC,1-nbcx_nest:nx_nest+nbcx_nest,
! ***      &           1-nbcy_nest:ny_nest+nbcy_nest,
! ***      &                       nz_nest+nbcz_nest))

!MSK start  New subgrid BC values, only wanted if there are NEWBC files
!MSK     Now commented subrid BC and IC files
!MSK        if (NEWBCFE( 1 )) then
!MSK
!MSK          CALL WNEWBC
!MSK
!MSK        endif
!MSK end

! ***   The export of the "BC_nest" data is moved to the "ReceiveTimeDependentOutputData" 
! ***   routine which is called after the present call of "CallRunEpi(picallcnt)"
        
      ENDIF

!MSK start
! *** Writing out RESTART concentration values:
      if (ATDATE(EDAT) .AND. ISH == NSH .AND. ITS == NTS) then
      
!MSK        CALL WNEWC
!!==========================================================================
!! write out main grid emission data at last timestep of the simulation to .dat file
!! added by Kang @ CARES, Dec. 23, 2019
!!==========================================================================
        if (.not. allocated(field3D_output) )  allocate(field3D_output(NX,NY,NZ,NC))
!!==========================================================================
          
! Write the netCDF output file of main grid C() for RESTART
          if (IC1GRIDFE) then

              ! one time step 
              Nhh_in = 1

              ! current simulation date
              mdate(1,Nhh_in) = YEAR
              mdate(2,Nhh_in) = MNTH
              mdate(3,Nhh_in) = DAYM
              mdate(4,Nhh_in) = HOUR

              unitname = "ug/m^3"

              if (averaged_output) then
                 validity = 'averaged'
              else
                 validity = 'instantaneous'
              endif

              dopacking = .false.
              dofloat   = .true.
              domirror  = .false.

              do 300 IC = 1,NC

                namefield = CMPND(IC)
                if ( trim(CMPND(IC)).eq."PM2.5") then
                  namefield = "PM25      "
                endif

                ! Do neglect the outer boundaries
                ! Read C() from IX=2, IY=2, until NZ
                do 310 IZ=1,NZ
                  do 320 IX = 1,NX
                    do 330 IY = 1,NY

                       field3D(IX,IY,IZ) = C(IC,IX+1,IY+1,IZ)
                  !     field3D(IY,IX,IZ) = C(IC,IX+1,IY+1,IZ)
!!==========================================================================
!! write out main grid emission data at last timestep of the simulation to .dat file
!! added by Kang @ CARES, Dec. 23, 2019
!!==========================================================================
                 field3D_output(IX,IY,IZ,IC) = C(IC,IX+1,IY+1,IZ)
!!==========================================================================
  330               continue
  320             continue
  310           continue

                call writeconcfield(IC1GRIDFN, namefield,unitname,field3D(1:NX,1:NY,1:NZ), NX, NY, NZ, &
                                 Nhh_in, mdate, validity, dopacking, dofloat, domirror)

!!!!!!!!!!!!!!!!!!! for testing 3D concentration output file by Kang @ Dec.2, 2019
!               if (IC .eq. 3) then
!                open(unit=1111,file="../OUTPUT/3D_field_write.txt")  !!
!                open(unit=1112,file="../OUTPUT/3D_field_para_write.txt")  !!                
!                do 311 IZ=1,NZ
!                    do 331 IY = 1,NY

!                      write(1111,*) (field3D(IX,IY,IZ), IX=1,NX)
                    !  write(1111,*) (C(3,IX+1,IY+1,IZ), IX=1,NX)
!  331               continue

!  311           continue

!                write(1112,*)"write out 3D concentration file, namefield=",namefield,"unitname=",unitname, & 
!                              ", NX=",NX,", NY=",NY, "NZ=",NZ, "Nhh_in=",Nhh_in, "mdate=",mdate,  & 
!                              ", validity=",validity,", dopacking=",dopacking,", dofloat=", dofloat,  &
!                              ", domirror=",domirror, ", IC1GRIDFN=", IC1GRIDFN
               
!                close(1111)
!                close(1112)
!                endif
!!!!!!!!!!!!!!!!!!! end for testing 3D concentration output file by Kang @ Dec.2, 2019


  300          continue


          endif
!MSK end
!!===================================================================================
!! write out main grid emission data at last timestep of the simulation to .dat file
!! added by Kang @ CARES, Dec. 23, 2019
!!===================================================================================

      open(unit=1116,file="../OUTPUT/3D_instantanous_mainconc_center.dat")  !! 3D data at center point of main cell

      write(1116,2140)(CMPND(IC),IC=1,NC)
      
       do  IZ=1,NZ
            do  IX = 1,NX
                do  IY = 1,NY
                    mxm_i(IY,IX) = SITEX0 + (IX-1)*DX + 0.5*DX 
                    mxm_j(IY,IX) = SITEY0 + (IY-1)*DY + 0.5*DY         
             write(1116,2150) (mdate(II,Nhh_in),II=1,4), mxm_i(IY,IX),mxm_j(IY,IX),Z(IZ)-0.5*DZ(IZ),& 
                      (field3D_output(IX,IY,IZ,IC), IC=1,NC)  
                enddo
          enddo
      enddo 

      close(1116)

!$$$$$$ 
!$$$$$$       open(unit=1113,file="../OUTPUT/3D_instantanous_mainconc.dat")  !! 3D data at SW+top point of main cell
!$$$$$$ 
!$$$$$$       write(1113,2140)(CMPND(IC),IC=1,NC)
!$$$$$$       
!$$$$$$        do  IZ=1,NZ
!$$$$$$             do  IX = 1,NX
!$$$$$$                 do  IY = 1,NY
!$$$$$$                     mxm_i(IY,IX) = SITEX0 + (IX-1)*DX 
!$$$$$$                     mxm_j(IY,IX) = SITEY0 + (IY-1)*DY          
!$$$$$$              write(1113,2150) (mdate(II,Nhh_in),II=1,4), mxm_i(IY,IX),mxm_j(IY,IX),Z(IZ),& 
!$$$$$$                                Z(IZ)*DEPTHM(IX,IY)/MOD_H,(field3D_output(IX,IY,IZ,IC), IC=1,NC)  
!$$$$$$                 enddo
!$$$$$$           enddo
!$$$$$$       enddo 
 

        if (allocated(field3D_output))  deallocate(field3D_output)

! Deallocate
        if (allocated(mxm_i))     deallocate(mxm_i)
        if (allocated(mxm_j))     deallocate(mxm_j)
          
!$$$$$$        close(1113)



   !   open(unit=1114,file="../OUTPUT/3D_instantanous_mainconc_centerBCZ.dat") !!!! 3D data at center point of main cell including (BC @ x,y,z direction)
      
   !   write(1114,2140)(CMPND(IC),IC=1,NC)
      
   !    do  IZ=1,NZ+NBCZ
   !             if (IZ .lt. NZ+NBCZ) then    
   !             Znew(IZ)=Z(IZ)-0.5*DZ(IZ)
   !             else
   !             Znew(IZ)=Znew(IZ-1)+DZ(IZ-1)
   !             endif  
                    
   !         do  IX = 1,NX+2*NBCX
   !             do  IY = 1,NY+2*NBCY
   !                 mxm_i_new(IY,IX) = SITEX0 + (IX-1)*DX-0.5*DX 
   !                 mxm_j_new(IY,IX) = SITEY0 + (IY-1)*DY-0.5*DY   
             
   !          if (IX .ne. 1 .and. IX .ne. NX+2*NBCX .and. IY .ne. 1 .and. IY .ne. NY+2*NBCY ) then
               
   !          write(1114,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew(IZ),& 
!$$$$$$                                Znew(IZ)*DEPTHM(IX-1,IY-1)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)  
   !          endif
               
   !          if (IX .eq. 1) then
   !            if (IY .eq. NY+2*NBCY) then                 
   !          write(1114,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew(IZ),& 
   !                            Znew(IZ)*DEPTHM(IX,IY-2)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC) 
   !            endif 
   !            if (IY .eq. 1) then
   !         write(1114,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew(IZ),& 
   !                            Znew(IZ)*DEPTHM(IX,IY)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)
   !            endif
   !            if (IY .ne.1 .and. IY .ne. NY+2*NBCY) then   
   !         write(1114,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew(IZ),& 
   !                            Znew(IZ)*DEPTHM(IX,IY-1)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)  
   !            endif
   !          endif
   !          if (IX .eq. NX+2*NBCX) then
!$$$$$$                if (IY .eq. NY+2*NBCY) then               
!$$$$$$              write(1114,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew(IZ),& 
!$$$$$$                                Znew(IZ)*DEPTHM(IX-2,IY-2)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC) 
!$$$$$$                endif 
!$$$$$$               if (IY .eq. 1) then
!$$$$$$             write(1114,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew(IZ),& 
!$$$$$$                                Znew(IZ)*DEPTHM(IX-2,IY)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)
!$$$$$$                endif
!$$$$$$               if (IY .ne.1 .and. IY .ne. NY+2*NBCY) then 
!$$$$$$             write(1114,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew(IZ),& 
!$$$$$$                                Znew(IZ)*DEPTHM(IX-2,IY-1)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)  
!$$$$$$                endif
!$$$$$$              endif
!$$$$$$  
!$$$$$$              if ( IX .ne. 1 .and. IX .ne. NX+2*NBCX) then  
!$$$$$$                if (IY .eq. 1) then                
!$$$$$$              write(1114,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew(IZ),& 
!$$$$$$                                Znew(IZ)*DEPTHM(IX-1,IY)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC) 
!$$$$$$                endif
!$$$$$$                if (IY .eq.  NY+2*NBCY) then
!$$$$$$              write(1114,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew(IZ),& 
!$$$$$$                                Znew(IZ)*DEPTHM(IX-1,IY-2)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)
!$$$$$$                endif
!$$$$$$              endif
!$$$$$$            
!$$$$$$                 enddo
!$$$$$$           enddo
!$$$$$$       enddo 
!$$$$$$  
! Deallocate
!$$$$$$         if (allocated(Znew))     deallocate(Znew)          
!$$$$$$        close(1114)


!$$$$$$       open(unit=1115,file="../OUTPUT/3D_instantanous_mainconc_centerBC.dat") !!!! 3D data at center point of main cell including (BC @ x,y direction)
!$$$$$$       
!$$$$$$       write(1115,2140)(CMPND(IC),IC=1,NC)
!$$$$$$       
!$$$$$$        do  IZ=1,NZ
!$$$$$$ 
!$$$$$$                 Znew0(IZ)=Z(IZ)-0.5*DZ(IZ)
!$$$$$$                     
!$$$$$$             do  IX = 1,NX+2*NBCX
!$$$$$$                 do  IY = 1,NY+2*NBCY
!$$$$$$                     mxm_i_new(IY,IX) = SITEX0 + (IX-1)*DX-0.5*DX 
!$$$$$$                     mxm_j_new(IY,IX) = SITEY0 + (IY-1)*DY-0.5*DY   
!$$$$$$        
!$$$$$$              if (IX .ne. 1 .and. IX .ne. NX+2*NBCX .and. IY .ne. 1 .and. IY .ne. NY+2*NBCY ) then             
!$$$$$$              write(1115,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew0(IZ),& 
!$$$$$$                                Znew0(IZ)*DEPTHM(IX-1,IY-1)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)  
!$$$$$$              endif
!$$$$$$                
!$$$$$$              if (IX .eq. 1) then
!$$$$$$                if (IY .eq. NY+2*NBCY) then               
!$$$$$$              write(1115,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew0(IZ),& 
!$$$$$$                                Znew0(IZ)*DEPTHM(IX,IY-2)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC) 
!$$$$$$                endif 
!$$$$$$                if (IY .eq. 1) then
!$$$$$$             write(1115,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew0(IZ),& 
!$$$$$$                                Znew0(IZ)*DEPTHM(IX,IY)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)
!$$$$$$                endif
!$$$$$$                if (IY .ne.1 .and. IY .ne. NY+2*NBCY) then   
!$$$$$$             write(1115,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew0(IZ),& 
!$$$$$$                                Znew0(IZ)*DEPTHM(IX,IY-1)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)  
!$$$$$$                endif
!$$$$$$              endif
!$$$$$$ 
!$$$$$$              if (IX .eq. NX+2*NBCX) then
!$$$$$$                if (IY .eq. NY+2*NBCY) then               
!$$$$$$              write(1115,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew0(IZ),& 
!$$$$$$                                Znew0(IZ)*DEPTHM(IX-2,IY-2)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC) 
!$$$$$$                endif 
!$$$$$$               if (IY .eq. 1) then
!$$$$$$             write(1115,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew0(IZ),& 
!$$$$$$                                Znew0(IZ)*DEPTHM(IX-2,IY)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)
!$$$$$$                endif
!$$$$$$               if (IY .ne.1 .and. IY .ne. NY+2*NBCY) then 
!$$$$$$             write(1115,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew0(IZ),& 
!$$$$$$                                Znew0(IZ)*DEPTHM(IX-2,IY-1)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)  
!$$$$$$                endif
!$$$$$$              endif
!$$$$$$  
!$$$$$$              if ( IX .ne. 1 .and. IX .ne. NX+2*NBCX) then  
!$$$$$$                if (IY .eq. 1) then                
!$$$$$$              write(1115,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew0(IZ),& 
!$$$$$$                                Znew0(IZ)*DEPTHM(IX-1,IY)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC) 
!$$$$$$                endif
!$$$$$$               if (IY .eq.  NY+2*NBCY) then
!$$$$$$              write(1115,2150) (mdate(II,Nhh_in),II=1,4), mxm_i_new(IY,IX),mxm_j_new(IY,IX),Znew0(IZ),& 
!$$$$$$                                Znew0(IZ)*DEPTHM(IX-1,IY-2)/MOD_H,(C(IC,IX,IY,IZ), IC=1,NC)
!$$$$$$                endif
!$$$$$$              endif
!$$$$$$              
!$$$$$$                 enddo
!$$$$$$           enddo
!$$$$$$       enddo 
 

! Deallocate
!$$$$$$         if (allocated(mxm_i_new))     deallocate(mxm_i_new)
!$$$$$$         if (allocated(mxm_j_new))     deallocate(mxm_j_new)
!$$$$$$         if (allocated(Znew0))     deallocate(Znew0)          
!$$$$$$        close(1115)

      
!!==========================================================================  
      endif
      
!!DEC$ IF DEFINED (emep68)
!      if (ATDATE(EDAT) .AND. ISH == NSH .AND. ITS == NTS) then
!!_TEST	  IF (NC >= 68 ) THEN
!
!! ***     Deallocate the allocatable arrays that were declared as
!! ***     private in OrganicAerosol:
!          my_last_call = .true.
!	    call OrganicAerosol(DEBUG,1,1)
!
!!_TEST        ENDIF
!      endif
!!DEC$ ENDIF (emep68)

!     Finished with postprocessing

      RETURN

 2000 FORMAT ('TSGRID: IC,BC(IC,15,15,1) = ',I3,E16.7)
 2002 FORMAT ('TSGRID: IC,Total Model Mass    = ',I3,E16.7)
 2003 FORMAT ('TSGRID: IC,Volme Average Conc. = ',I3,E16.7)
 2004 FORMAT ('TSGRID: IC,Total Model Volume  = ',I3,E16.7)
 2005 FORMAT ('TSGRID:',2X,A12,2X,I4,1X,F7.0,1X,F10.1,1X,E12.4,1X,  &
                       E12.5,1X,E17.6,1X,E17.6,1X,E17.6,1X,E17.6)

! 2067 FORMAT ('TSGRID: H_DIFFU(1) = ',F14.7)
! 2068 FORMAT ('TSGRID: H_DIFFU(2) = ',F14.7)
! 2069 FORMAT ('TSGRID: H_DIFFU(3) = ',F14.7)
!!==============================================================================

!!! added by Kang for outputing data to .dat file @ 2020 
 2140 format ("Year      Month     Day      Hour      X(m)      Y(m)      Z(m)      ",22A10)
 2150 format (I4,3I3,3F12.3,22E16.7)
!!==============================================================================

!     End of subroutine TSGRID

      end subroutine tsgrid

